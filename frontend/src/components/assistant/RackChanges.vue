<script setup lang="ts">
import { computed, ref } from 'vue'
import { CheckCheck, FileSpreadsheet, Loader2 } from 'lucide-vue-next'
import { api } from '../../lib/api'
import { errorText } from '../../lib/notice'
import { changesNotReady, type RackDeposit } from '../../lib/rackDeposit'
import type { Proposal, ProposalChange } from '../../lib/proposals'
import { t } from '../../lib/i18n'

/**
 * A rack deposit's tube-locations changes as an .xlsx to paste by hand (POST …/rack-changes: «Filas nuevas» in the
 * sheet's column order, «Filas cambiadas», «Léeme»), made only once every well is settled (lib/rackDeposit
 * changesNotReady, the deposit sheet's rule). `main`: the app cannot edit the sheet, so this is the proposal's main
 * action in place of «Aplicar», with «Aplicado a mano» (POST …/rack-by-hand) once the file was downloaded: the
 * proposal leaves the pending list with who and when, and nothing is written to the sheet.
 */
const props = defineProps<{ proposalId: string; deposit: RackDeposit; changes: ProposalChange[]; editable: boolean; main?: boolean }>()
const emit = defineEmits<{ replace: [proposal: Proposal]; notice: [message: string] }>()

const busy = ref<'' | 'file' | 'hand'>('')
const asking = ref(false)
const problems = ref<string[]>([])
const why = computed(() => changesNotReady(props.deposit, props.changes))

/** The kept file's address on this site (the download needs the person signed in). */
const local = (url: string) => {
  const m = /\/api\/(rack-reports\/[a-f0-9]{24}\/[\w.-]+\.xlsx)$/.exec(url)
  return m ? `api/${m[1]}` : url
}
async function download() {
  if (busy.value) return
  busy.value = 'file'
  problems.value = []
  try {
    const out = await api<{ file: string; download: string; proposal: Proposal }>(`chat/proposals/${props.proposalId}/rack-changes`, { method: 'POST', body: {} })
    emit('replace', out.proposal)
    const a = document.createElement('a')
    a.href = local(out.download)
    a.download = out.file
    document.body.appendChild(a)
    a.click()
    a.remove()
  } catch (e) {
    const found = (e as { details?: { problems?: string[] } }).details?.problems
    if (Array.isArray(found)) problems.value = found
    emit('notice', errorText(e))
  } finally {
    busy.value = ''
  }
}
async function byHand() {
  if (busy.value) return
  busy.value = 'hand'
  try {
    const out = await api<{ proposal: Proposal }>(`chat/proposals/${props.proposalId}/rack-by-hand`, { method: 'POST', body: {} })
    asking.value = false
    emit('replace', out.proposal)
  } catch (e) {
    emit('notice', errorText(e))
  } finally {
    busy.value = ''
  }
}
</script>

<template>
  <span class="contents">
    <button
      type="button"
      :class="main ? 'btn-primary bg-emerald-700 hover:bg-emerald-800' : 'btn h-8 px-2.5 text-xs'"
      :disabled="!!busy || why.length > 0"
      :title="
        why.length
          ? $t('Aún no: {why}', { why: why.join('; ') })
          : $t('Las filas nuevas y las cambiadas de {sheet}, para pegarlas a mano, con instrucciones', { sheet: deposit.sheet ?? '' })
      "
      data-rack-changes
      @click="download"
    >
      <Loader2 v-if="busy === 'file'" :size="main ? 15 : 14" class="animate-spin" /><FileSpreadsheet v-else :size="main ? 15 : 14" />
      {{ $t('Descargar cambios (.xlsx)') }}
    </button>
    <template v-if="main && editable">
      <button
        v-if="!asking"
        type="button"
        class="btn"
        :disabled="!!busy || !deposit.changes_xlsx"
        :title="
          deposit.changes_xlsx
            ? $t('Cuando alguien con acceso haya pegado los cambios en la hoja: la propuesta sale de pendientes. La app no escribe nada')
            : $t('Primero descarga los cambios y pégalos en la hoja')
        "
        data-rack-by-hand
        @click="asking = true"
      >
        <CheckCheck :size="15" /> {{ $t('Aplicado a mano') }}
      </button>
      <span v-else class="by-hand-ask" role="group" :aria-label="$t('Aplicado a mano')">
        {{ $t('¿Ya están pegados en {sheet}?', { sheet: deposit.sheet ?? '' }) }}
        <button type="button" class="btn h-7 px-2 text-xs" :disabled="!!busy" data-rack-by-hand-yes @click="byHand">
          <Loader2 v-if="busy === 'hand'" :size="13" class="animate-spin" /> {{ $t('Sí, marcar aplicado a mano') }}
        </button>
        <button type="button" class="btn h-7 px-2 text-xs" :disabled="!!busy" @click="asking = false">{{ $t('Cancelar') }}</button>
      </span>
    </template>
    <span v-if="why.length" class="text-[11px] text-stone-500">{{ $t('Aún no: {why}', { why: why[0] }) }}{{ why.length > 1 ? ' …' : '' }}</span>
    <span v-if="problems.length" class="basis-full text-[11px] text-red-800">{{ problems.join(' · ') }}</span>
  </span>
</template>

<style scoped>
.by-hand-ask {
  display: inline-flex;
  flex-wrap: wrap;
  align-items: center;
  gap: 0.35rem;
  border-radius: 0.25rem;
  background: var(--color-amber-50);
  padding: 0.15rem 0.4rem;
  font-size: 12px;
  color: var(--color-amber-950);
}
</style>
