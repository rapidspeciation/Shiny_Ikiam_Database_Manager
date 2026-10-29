<script setup lang="ts">
import { computed, ref } from 'vue'
import { AlertTriangle, CloudUpload, Download, ExternalLink, Loader2 } from 'lucide-vue-next'
import { api } from '../../lib/api'
import { errorText } from '../../lib/notice'
import { notReady, type RackDeposit } from '../../lib/rackDeposit'
import type { Proposal } from '../../lib/proposals'
import { t } from '../../lib/i18n'

/**
 * The head of a rack deposit's proposal (lib/rackDeposit): the deposit sheet's own fields (responsable: the
 * person signed in until someone says otherwise; the freezer, LBI062), a jump to each rack when there are
 * several, «Descargar .xlsx» (the LBI's Anexo1, one sheet per rack) and «Guardar en Google Drive» (a copy in
 * the project's folder «Depósitos LBI (racks)», only on click), with the links once made. Saved at once.
 */
const props = defineProps<{ proposalId: string; deposit: RackDeposit; editable: boolean }>()
const emit = defineEmits<{ replace: [proposal: Proposal]; notice: [message: string]; jump: [reading: string] }>()

const busy = ref<'' | 'fields' | 'xlsx' | 'drive'>('')
const why = computed(() => notReady(props.deposit))
const problems = ref<string[]>([])

async function saveField(field: 'responsable' | 'ultracongelador', event: Event) {
  const value = (event.target as HTMLInputElement).value.trim()
  if (!props.editable || value === (props.deposit[field] ?? '')) return
  busy.value = 'fields'
  try {
    const out = await api<{ proposal: Proposal }>(`chat/proposals/${props.proposalId}/rack`, { method: 'POST', body: { [field]: value } })
    emit('replace', out.proposal)
  } catch (e) {
    emit('notice', errorText(e))
  } finally {
    busy.value = ''
  }
}
/** The kept file's address on this site (the download needs the person signed in). */
const local = (url: string) => {
  const m = /\/api\/(rack-reports\/[a-f0-9]{24}\/[\w.-]+\.xlsx)$/.exec(url)
  return m ? `api/${m[1]}` : url
}
function download(url: string, file: string) {
  const a = document.createElement('a')
  a.href = local(url)
  a.download = file
  document.body.appendChild(a)
  a.click()
  a.remove()
}
async function make(drive: boolean) {
  if (busy.value) return
  busy.value = drive ? 'drive' : 'xlsx'
  problems.value = []
  try {
    const out = await api<{
      file: string
      download: string
      drive?: { link?: string; error?: string }
      proposal: Proposal
    }>(`chat/proposals/${props.proposalId}/rack-files`, { method: 'POST', body: { drive } })
    emit('replace', out.proposal)
    if (!drive) download(out.download, out.file)
    else if (out.drive?.error) emit('notice', t('No se pudo guardar en Drive: {why}', { why: out.drive.error }))
  } catch (e) {
    const found = (e as { details?: { problems?: string[] } }).details?.problems
    if (Array.isArray(found)) problems.value = found
    emit('notice', errorText(e))
  } finally {
    busy.value = ''
  }
}
const when = (at?: string) => (at ? new Date(at).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' }) : '')
</script>

<template>
  <div class="deposit-head">
    <div class="flex flex-wrap items-center gap-x-3 gap-y-1">
      <span class="text-xs font-semibold text-stone-800">{{ $t('Depósito LBI · hoja Anexo1') }}</span>
      <label class="dep-field">
        <span>{{ $t('Responsable') }}</span>
        <input
          :value="deposit.responsable"
          :class="{ 'is-assumed': deposit.responsable_default, 'is-unset': !deposit.responsable }"
          :placeholder="$t('falta')"
          :disabled="!editable || !!busy"
          :title="deposit.responsable_default ? $t('Supuesto: quien tiene la sesión abierta. Cámbialo si deposita otra persona') : ''"
          @change="saveField('responsable', $event)"
          @keydown.enter="($event.target as HTMLInputElement).blur()"
        />
        <em v-if="deposit.responsable_default" class="text-[10px] text-amber-800">{{ $t('supuesto') }}</em>
      </label>
      <label class="dep-field">
        <span>{{ $t('Ultracongelador') }}</span>
        <input
          :value="deposit.ultracongelador"
          class="w-20 font-mono"
          :class="{ 'is-unset': !deposit.ultracongelador }"
          :disabled="!editable || !!busy"
          @change="saveField('ultracongelador', $event)"
          @keydown.enter="($event.target as HTMLInputElement).blur()"
        />
      </label>
    </div>
    <!-- Several racks: a jump to each section. -->
    <nav v-if="deposit.racks.length > 1" class="flex flex-wrap items-center gap-1" :aria-label="$t('Racks del depósito')">
      <span class="text-[11px] text-stone-500">{{ $t('Racks') }}:</span>
      <button
        v-for="(r, i) in deposit.racks"
        :key="r.reading"
        type="button"
        class="jump"
        :class="{ 'has-open': r.missing.length || r.unset.length || r.assumed }"
        :title="r.missing.length || r.unset.length || r.assumed ? $t('Le falta algo por decir') : ''"
        @click="emit('jump', r.reading)"
      >
        {{ i + 1 }} · <span class="font-mono">{{ r.rack_id ?? '?' }}</span>
      </button>
    </nav>
    <!-- The deposit sheet: downloaded, and copied to Drive on click. -->
    <div class="flex flex-wrap items-center gap-2">
      <button
        type="button"
        class="btn h-8 px-2.5 text-xs"
        :disabled="!!busy || why.length > 0"
        :title="why.length ? $t('Aún no: {why}', { why: why.join('; ') }) : $t('La hoja de depósito del LBI (Anexo1), una hoja por rack')"
        @click="make(false)"
      >
        <Loader2 v-if="busy === 'xlsx'" :size="14" class="animate-spin" /><Download v-else :size="14" /> {{ $t('Descargar .xlsx') }}
      </button>
      <button
        type="button"
        class="btn h-8 px-2.5 text-xs"
        :disabled="!editable || !!busy || why.length > 0"
        :title="
          why.length
            ? $t('Aún no: {why}', { why: why.join('; ') })
            : $t('Sube una copia a la carpeta «Depósitos LBI (racks)» del Drive del proyecto')
        "
        @click="make(true)"
      >
        <Loader2 v-if="busy === 'drive'" :size="14" class="animate-spin" /><CloudUpload v-else :size="14" />
        {{ $t('Guardar en Google Drive') }}
      </button>
      <a v-if="deposit.drive?.link" class="file-link" :href="deposit.drive.link" target="_blank" rel="noopener">
        <ExternalLink :size="12" /> {{ $t('En Drive') }} ({{ when(deposit.drive.at) }})
      </a>
      <a v-if="deposit.xlsx?.download" class="file-link" :href="local(deposit.xlsx.download)" :download="deposit.xlsx.file">
        {{ deposit.xlsx.file }}
      </a>
      <span v-if="why.length" class="text-[11px] text-stone-500">{{ $t('Aún no: {why}', { why: why[0] }) }}{{ why.length > 1 ? ' …' : '' }}</span>
    </div>
    <ul v-if="problems.length" class="text-[11px] text-red-800">
      <li v-for="(p, i) in problems" :key="i">{{ p }}</li>
    </ul>
    <p v-if="deposit.protected" class="dep-warn" role="status">
      <AlertTriangle :size="12" class="shrink-0" />
      {{
        $t(
          'La hoja {sheet} está protegida para la cuenta de la app: «Aplicar» lo rechazará Google hasta que su dueño le dé permiso. La hoja de depósito (.xlsx) sí funciona.',
          { sheet: deposit.sheet ?? '' },
        )
      }}
    </p>
  </div>
</template>

<style scoped>
.deposit-head {
  display: flex;
  flex-direction: column;
  gap: 0.4rem;
  border-bottom: 1px solid var(--color-stone-200);
  background: var(--color-stone-50);
  padding: 0.45rem 0.5rem;
}
.dep-field {
  display: inline-flex;
  align-items: center;
  gap: 0.3rem;
  font-size: 11px;
  color: var(--color-stone-600);
}
.dep-field input {
  border: 1px solid var(--color-stone-300);
  border-radius: 0.25rem;
  background: white;
  padding: 0.1rem 0.35rem;
  font-size: 12px;
  color: var(--color-stone-900);
}
.dep-field input.is-assumed,
.dep-field input.is-unset {
  background: var(--color-amber-50);
  border-color: var(--color-amber-400);
}
.jump {
  border: 1px solid var(--color-stone-300);
  border-radius: 9999px;
  background: white;
  padding: 0 0.5rem;
  font-size: 11px;
}
.jump:hover {
  border-color: var(--color-emerald-600);
}
.jump.has-open {
  border-color: var(--color-amber-400);
  background: var(--color-amber-50);
}
.file-link {
  display: inline-flex;
  align-items: center;
  gap: 0.2rem;
  font-size: 11px;
  color: var(--color-emerald-800);
  text-decoration: underline;
}
.dep-warn {
  display: flex;
  align-items: flex-start;
  gap: 0.3rem;
  border-radius: 0.25rem;
  background: var(--color-amber-100);
  padding: 0.3rem 0.5rem;
  font-size: 11px;
  color: var(--color-amber-950);
}
</style>
