<script setup lang="ts">
// Descargas → «Fotos para comparar»: which photo pack to keep on the phone (lib/photoPacks.ts, lib/usePacks.ts):
// the taxa (Ithomiini, Nymphalidae, butterflies, butterflies and moths), the region (Ecuador, chosen countries, the
// Neotropics) and whether to add the subspecies, each option with its photos and MB; the installed pack with its
// size, an update when the server's list is newer, and its removal.
import { computed, onMounted, ref, watch } from 'vue'
import { Check, ChevronDown, Images } from 'lucide-vue-next'
import { locale, t, tn } from '../../lib/i18n'
import { countrySizes, packCountries, mbText, sameRegion, sameScope, scopeSize, TAXA_SCOPES, updateAvailable, type PackScope, type Region } from '../../lib/photoPacks'
import { fieldSet } from '../../lib/identifyRefs'
import { taxaLabel, usePacks } from '../../lib/usePacks'

const props = defineProps<{ online: boolean }>()
const { index, indexFailed, pack, progress, note, failedSave, scope, load, install, remove, removeLegacy: dropLegacy } = usePacks()
onMounted(() => load())
watch(
  () => props.online,
  on => on && !index.value && load(true),
)

const regionNames = computed(() => {
  try {
    return new Intl.DisplayNames([locale.value === 'es' ? 'es' : 'en'], { type: 'region' })
  } catch {
    return null
  }
})
const countryName = (iso: string) => regionNames.value?.of(iso) ?? iso
const countries = computed(() => (index.value ? packCountries(index.value, countryName) : []))

const size = (s: PackScope) => (index.value ? scopeSize(index.value, s) : null)
// A number stays with its unit when the line wraps («439 fotos ·» / «12 MB», never «12» / «MB»).
const zText = (z: { photos: number; bytes: number } | null | undefined) =>
  z ? tn(z.photos, '{n} foto · {mb} MB', '{n} fotos · {mb} MB', { n: z.photos.toLocaleString(), mb: mbText(z.bytes) }).replace(/(\d) (?=\S)/g, '$1\u00a0') : ''
const sizeText = (s: PackScope) => zText(size(s))
const EC: Region = ['EC']
const isEc = computed(() => sameRegion(scope.value.region, EC))
const isSome = computed(() => scope.value.region !== 'neotropics' && !isEc.value)
const picking = ref(isSome.value)
const set = (patch: Partial<PackScope>) => (scope.value = { ...scope.value, ...patch })
function chooseRegion(r: 'ec' | 'some' | 'neotropics') {
  if (r === 'ec') {
    set({ region: [...EC] })
    picking.value = false
  } else if (r === 'neotropics') {
    set({ region: 'neotropics' })
    picking.value = false
  } else picking.value = !picking.value || !isSome.value
}
const chosenCountries = computed(() => new Set(scope.value.region === 'neotropics' ? [] : scope.value.region))
function toggleCountry(iso: string) {
  const now = new Set(chosenCountries.value)
  if (now.has(iso)) now.delete(iso)
  else now.add(iso)
  set({ region: now.size ? [...now] : [...EC] })
}
/** Every country's size at once (one pass), again only when the groups or the subspecies choice change. */
const byCountry = computed(() => (index.value ? countrySizes(index.value, { taxa: scope.value.taxa, ssp: scope.value.ssp }) : new Map()))
const countrySize = (iso: string) => zText(byCountry.value.get(iso))
const regionLabel = (r: Region) => (r === 'neotropics' ? t('Neotrópico') : r.length === 1 ? countryName(r[0]) : t('{n} países', { n: r.length }))
const regionOn = (r: 'ec' | 'some' | 'neotropics') => (r === 'ec' ? isEc.value && !picking.value : r === 'some' ? isSome.value || picking.value : scope.value.region === 'neotropics')

const total = computed(() => size(scope.value))
const installed = computed(() => pack.value?.installed ?? null)
const current = computed(() => !!installed.value && sameScope(installed.value.scope, scope.value))
const newer = computed(() => updateAvailable(installed.value, index.value))
const legacy = ref(fieldSet())
const when = (iso: string) => new Date(iso).toLocaleDateString(locale.value === 'es' ? 'es' : 'en', { day: 'numeric', month: 'short' })
/** The list is a sample (the lab's): said plainly. */
const sampleNote = computed(() => {
  const v = index.value?.sample
  if (v === 'ithomiini-only') return t('Por ahora la lista tiene fotos solo de Ithomiini.')
  if (v?.startsWith('regions:')) return t('Por ahora hay fotos solo de {places}.', { places: v.slice(8).split(',').map(countryName).join(', ') })
  return null
})

async function onRemove() {
  if (!confirm(t('¿Borrar las fotos de este teléfono?'))) return
  await remove()
}
async function removeLegacy() {
  if (!confirm(t('¿Borrar las fotos de este teléfono?'))) return
  await dropLegacy()
  legacy.value = null
}
const pct = computed(() => (progress.value ? Math.round((100 * progress.value.done) / Math.max(1, progress.value.total)) : 0))
</script>

<template>
  <section class="rounded-xl border-2 border-border-strong border-l-[6px] border-l-fill-emerald bg-surface p-3 sm:p-4" data-testid="pack-panel">
    <div class="flex items-start gap-3">
      <Images :size="22" class="mt-0.5 shrink-0 text-fill-emerald" />
      <div class="min-w-0 flex-1">
        <h2 class="text-lg font-semibold">{{ $t('Fotos para comparar') }}</h2>
        <p class="text-sm text-fg-muted">{{ $t('Una foto por taxón, para comparar y buscar sin internet.') }}</p>
      </div>
    </div>

    <!-- Installed -->
    <div v-if="installed" class="mt-3 rounded-lg border border-success-text/40 bg-brand-soft p-3" data-testid="pack-installed">
      <p class="flex items-start gap-2 font-medium">
        <Check :size="18" class="mt-0.5 shrink-0 text-success-text" />
        <span>
          {{ taxaLabel(installed.scope.taxa) }} · {{ regionLabel(installed.scope.region) }} · {{ installed.scope.ssp ? $t('con subespecies') : $t('solo especies') }}
          <span class="block text-sm font-normal text-fg-muted tabular-nums">{{ $t('{n} fotos · {mb} MB · {date}', { n: installed.photos.toLocaleString(), mb: mbText(installed.bytes), date: when(installed.at) }) }}</span>
        </span>
      </p>
      <p v-if="newer" class="mt-2 text-sm font-medium text-pending-text">{{ $t('Hay fotos nuevas en el servidor.') }}</p>
      <div class="mt-2 flex flex-wrap gap-2">
        <button v-if="newer || installed.failed" class="btn-primary min-h-11 px-4" :disabled="!!progress || !online" data-testid="pack-update" @click="install(installed.scope)">{{ $t('Actualizar fotos') }}</button>
        <button class="btn-danger min-h-11 px-4" :disabled="!!progress" @click="onRemove">{{ $t('Borrar del teléfono') }}</button>
      </div>
    </div>
    <div v-else-if="legacy" class="mt-3 rounded-lg border border-border p-3 text-sm">
      <p>{{ $t('Fotos guardadas antes (Ithomiini): {n} fotos · {mb} MB', { n: legacy.count, mb: mbText(legacy.bytes) }) }}</p>
      <button class="btn-danger mt-2 min-h-11 px-4" @click="removeLegacy">{{ $t('Borrar del teléfono') }}</button>
    </div>

    <p v-if="!index && indexFailed" class="mt-3 text-sm text-fg-muted">
      {{ online ? $t('No se pudo leer la lista de fotos.') : $t('Sin señal: conéctese para elegir y descargar fotos.') }}
      <button v-if="online" class="ml-1 min-h-11 font-medium text-brand-text underline" @click="load(true)">{{ $t('Reintentar') }}</button>
    </p>
    <p v-else-if="!index" class="mt-3 text-sm text-fg-muted">{{ $t('Calculando el tamaño…') }}</p>

    <template v-if="index">
      <!-- Taxa -->
      <fieldset class="mt-4">
        <legend class="mb-1.5 text-sm font-semibold">{{ $t('Qué grupos') }}</legend>
        <div class="grid grid-cols-2 gap-2">
          <label
            v-for="k in TAXA_SCOPES"
            :key="k"
            class="opt flex min-h-14 cursor-pointer flex-col justify-center rounded-lg border-2 px-3 py-1.5"
            :class="scope.taxa === k ? 'border-brand bg-brand-soft' : 'border-border bg-surface'"
          >
            <input type="radio" name="pack-taxa" class="sr-only" :value="k" :checked="scope.taxa === k" @change="set({ taxa: k })" />
            <span class="flex items-center gap-1.5 font-medium"><Check v-if="scope.taxa === k" :size="16" class="shrink-0 text-brand-text" />{{ taxaLabel(k) }}</span>
            <span class="text-xs text-fg-muted tabular-nums">{{ sizeText({ ...scope, taxa: k }) }}</span>
          </label>
        </div>
        <p v-if="sampleNote" class="mt-1.5 text-xs text-pending-text">{{ sampleNote }}</p>
      </fieldset>

      <!-- Region -->
      <fieldset class="mt-4">
        <legend class="mb-1.5 text-sm font-semibold">{{ $t('Dónde') }}</legend>
        <div class="grid grid-cols-3 gap-2" role="radiogroup" :aria-label="$t('Dónde')">
          <button
            v-for="r in ['ec', 'some', 'neotropics'] as const"
            :key="r"
            type="button"
            :data-testid="`region-${r}`"
            role="radio"
            :aria-checked="regionOn(r)"
            class="opt flex min-h-14 flex-col justify-center rounded-lg border-2 px-2 py-1.5 text-left"
            :class="regionOn(r) ? 'border-brand bg-brand-soft' : 'border-border bg-surface'"
            @click="chooseRegion(r)"
          >
            <span class="flex items-center gap-1 text-sm font-medium">
              <Check v-if="regionOn(r)" :size="16" class="shrink-0 text-brand-text" />
              {{ r === 'ec' ? countryName('EC') : r === 'some' ? (isSome ? regionLabel(scope.region) : $t('Otros países')) : $t('Neotrópico') }}
              <ChevronDown v-if="r === 'some'" :size="14" class="shrink-0 transition-transform" :class="picking ? 'rotate-180' : ''" />
            </span>
            <span v-if="r !== 'some' || isSome" class="text-xs text-fg-muted tabular-nums">{{ sizeText({ ...scope, region: r === 'ec' ? EC : r === 'some' ? scope.region : 'neotropics' }) }}</span>
          </button>
        </div>
        <ul v-if="picking" class="mt-2 max-h-72 divide-y divide-border overflow-y-auto rounded-lg border border-border" data-testid="countries">
          <li v-for="c in countries" :key="c">
            <label class="flex min-h-11 cursor-pointer items-center gap-3 px-3 py-1.5" :class="chosenCountries.has(c) ? 'bg-brand-soft' : ''">
              <input type="checkbox" class="size-5 shrink-0 accent-[var(--focus)]" :checked="chosenCountries.has(c)" @change="toggleCountry(c)" />
              <span class="min-w-0 flex-1">{{ countryName(c) }}</span>
              <span class="shrink-0 text-xs text-fg-muted tabular-nums">{{ countrySize(c) }}</span>
            </label>
          </li>
        </ul>
      </fieldset>

      <!-- Subspecies -->
      <fieldset class="mt-4">
        <legend class="mb-1.5 text-sm font-semibold">{{ $t('Subespecies') }}</legend>
        <div class="grid grid-cols-2 gap-2">
          <label
            v-for="v in [false, true]"
            :key="String(v)"
            class="opt flex min-h-14 cursor-pointer flex-col justify-center rounded-lg border-2 px-3 py-1.5"
            :class="scope.ssp === v ? 'border-brand bg-brand-soft' : 'border-border bg-surface'"
          >
            <input type="radio" name="pack-ssp" class="sr-only" :checked="scope.ssp === v" @change="set({ ssp: v })" />
            <span class="flex items-center gap-1.5 text-sm font-medium"><Check v-if="scope.ssp === v" :size="16" class="shrink-0 text-brand-text" />{{ v ? $t('Especies y subespecies') : $t('Solo especies') }}</span>
            <span class="text-xs text-fg-muted tabular-nums">{{ sizeText({ ...scope, ssp: v }) }}</span>
          </label>
        </div>
      </fieldset>

      <div v-if="progress" class="mt-4" role="status" data-testid="pack-progress">
        <div class="h-2.5 overflow-hidden rounded-full bg-surface-sunken" role="progressbar" :aria-valuenow="pct" aria-valuemin="0" aria-valuemax="100">
          <div class="h-full bg-brand transition-[width]" :style="{ width: `${pct}%` }" />
        </div>
        <p class="mt-1 text-sm text-fg-muted tabular-nums">{{ $t('{done} de {total} fotos', progress) }}</p>
      </div>
      <button
        v-else-if="!current && total"
        class="btn-primary mt-4 min-h-12 w-full px-4 text-base"
        :disabled="!online || !total.photos"
        data-testid="pack-install"
        @click="install()"
      >
        {{ installed ? $t('Cambiar a esta elección') : $t('Descargar fotos') }} · {{ mbText(total.bytes) }} MB
      </button>
      <p v-if="!online && !current" class="mt-1 text-xs text-fg-muted">{{ $t('Hace falta conexión a internet para descargar.') }}</p>
      <p v-if="installed && !current && !progress" class="mt-1 text-xs text-fg-muted">{{ $t('Reemplaza las fotos de este teléfono.') }}</p>
      <p v-if="note" class="mt-2 text-sm text-fg-muted" role="status" data-testid="pack-note">
        {{ note }}
        <button v-if="failedSave && online && !progress" class="ml-1 min-h-11 font-medium text-brand-text underline underline-offset-2" data-testid="pack-retry" @click="install(failedSave.scope)">
          {{ $t('Reintentar') }}
        </button>
      </p>
    </template>
  </section>
</template>

<style scoped>
.opt {
  transition:
    transform 0.12s ease,
    border-color 0.12s ease;
}
.opt:active {
  transform: scale(0.98);
}
.opt:has(input:focus-visible) {
  outline: 2px solid var(--focus);
  outline-offset: 2px;
}
</style>
