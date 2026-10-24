<script setup lang="ts">
import { computed, nextTick, ref, watch } from 'vue'
import { ArrowRight, Check, ChevronLeft, ChevronRight, ExternalLink, Loader2, ScanSearch, X } from 'lucide-vue-next'
import { api } from '../../lib/api'
import { errorText, notify } from '../../lib/notice'
import { t } from '../../lib/i18n'
import { photoUrl } from '../../lib/review'
import type { Proposal } from '../../lib/proposals'
import {
  TIERS,
  cardChoices,
  changedByNote,
  curationChange,
  curationChips,
  kindLabel,
  sectionLabel,
  tierHint,
  tierLabel,
  undecided,
  type CurationEntry,
  type CurationTier,
  type PhotoCuration,
} from '../../lib/photoCuration'
import CropImage from '../review/CropImage.vue'
import PhotoViewer from './PhotoViewer.vue'
import CurationRecord from './CurationRecord.vue'

/**
 * A section of the photo archive's disagreements with the database (show_photo_fixes, or «Revisar» in
 * the Fotos tab), to decide case by case beside the photos. Each case is one row, always open: the
 * crops that matter (the line the OCR read the ID from, the envelope, the wings, from the 1600 px copy),
 * the change in bold, and the evidence as chips — green where a source supports the change, amber where
 * it argues against it. The selected case's photos are in the viewer beside the list (wheel zoom, drag,
 * the ID line outlined). The sure ones come ticked; «Lectura mala» keeps the database's value and records
 * the reading as wrong (it is not raised again). One «Aplicar» writes the database fixes ticked and starts
 * the Drive renames and turns at once (their own card follows them). Keys: ↑/↓ a case, Space tick,
 * X «Lectura mala», F the next photo.
 */
const props = defineProps<{ proposal: Proposal }>()
const emit = defineEmits<{ discard: []; started: []; open: [id: string] }>()

const card = computed(() => props.proposal.sync as unknown as PhotoCuration)
const entries = computed(() => card.value?.entries ?? [])
const open = computed(() => props.proposal.status === 'pending' || props.proposal.status === 'partial')
const showDecided = ref(false)
const shown = computed(() => entries.value.filter(e => showDecided.value || undecided(e)))
const decidedCount = computed(() => entries.value.length - entries.value.filter(undecided).length)
/** In the list's order: by group, then as the server ranked them. */
const ordered = computed(() => TIERS.flatMap(tier => shown.value.filter(e => e.tier === tier)))
/** Cases of more than one kind: each says its own. */
const mixed = computed(() => new Set(entries.value.map(e => e.kind)).size > 1)
const groups = computed(() => TIERS.map(tier => ({ tier, rows: ordered.value.filter(e => e.tier === tier) })).filter(g => g.rows.length))

// ------------------------------------------------------------ the decisions on this card
/** Ticked: make the change. Wrong: keep the database's value, the reading recorded as wrong. */
const ticked = ref(new Set<string>())
const wrong = ref(new Set<string>())
const values = ref<Record<string, string>>({})
function reset() {
  const kept = cardChoices.get(props.proposal.id)
  ticked.value = kept?.ticked ?? new Set(entries.value.filter(e => undecided(e) && e.fix && e.checked).map(e => e.id))
  wrong.value = kept?.wrong ?? new Set()
  values.value = kept?.values ?? {}
}
watch(() => props.proposal.id, reset, { immediate: true })
watch([ticked, wrong, values], () => cardChoices.set(props.proposal.id, { ticked: ticked.value, wrong: wrong.value, values: values.value }), { deep: true })
const can = (e: CurationEntry) => open.value && undecided(e)
function tick(e: CurationEntry, on = !ticked.value.has(e.id)) {
  if (!can(e) || !e.fix) return
  const next = new Set(ticked.value)
  if (on) next.add(e.id)
  else next.delete(e.id)
  ticked.value = next
  if (on && wrong.value.has(e.id)) markWrong(e, false)
}
function markWrong(e: CurationEntry, on = !wrong.value.has(e.id)) {
  if (!can(e)) return
  const next = new Set(wrong.value)
  if (on) next.add(e.id)
  else next.delete(e.id)
  wrong.value = next
  if (on && ticked.value.has(e.id)) tick(e, false)
}
function tickAll(tier: CurationTier, on: boolean) {
  const next = new Set(ticked.value)
  for (const e of entries.value) if (e.tier === tier && can(e) && e.fix && !wrong.value.has(e.id)) on ? next.add(e.id) : next.delete(e.id)
  ticked.value = next
}
const toDrive = computed(() => [...ticked.value].filter(id => entries.value.find(e => e.id === id)?.fix?.drive?.length).length)
const toDatabase = computed(() => ticked.value.size - toDrive.value)
const total = computed(() => ticked.value.size + wrong.value.size)

// ------------------------------------------------------------ the case selected, and its photos in the viewer
const current = ref<string | null>(null)
const selected = computed(() => entries.value.find(e => e.id === current.value) ?? ordered.value[0] ?? null)
const photo = ref(0)
type ViewPhoto = { id: string; name: string; mark?: readonly [number, number, number, number] | null }
const viewPhotos = computed<ViewPhoto[]>(() => {
  const e = selected.value
  if (!e) return []
  const list: ViewPhoto[] = []
  const add = (id: string, name: string, mark?: ViewPhoto['mark']) => {
    const had = list.find(p => p.id === id)
    if (had) had.mark ??= mark
    else list.push({ id, name, mark })
  }
  // A CAMID: first the photo whose ID line was read, outlined. A species or a sex: first the envelope,
  // outlined. A side or a turn: the photos whole, the one to turn first.
  const env = e.photos?.envelope
  if (e.kind === 'camid') for (const l of e.idLines ?? []) if (l.box) add(l.fileId, l.name, l.box)
  if (env && ['species', 'gallery', 'sex', 'aisex'].includes(e.kind)) add(env.fileId, env.name, env.bbox)
  const files = e.photos?.files ?? []
  const first = e.kind === 'turn' ? files.filter(f => f.name.toUpperCase() === String(e.evidence.photo ?? '').toUpperCase()) : []
  for (const f of [...first, ...files]) add(f.id, f.name)
  if (env) add(env.fileId, env.name)
  return list
})
watch(selected, () => (photo.value = 0))
const listEl = ref<HTMLElement>()
function select(e: CurationEntry, scroll = false) {
  current.value = e.id
  if (scroll) void nextTick(() => listEl.value?.querySelector(`[data-entry="${CSS.escape(e.id)}"]`)?.scrollIntoView({ block: 'nearest' }))
}
function move(by: 1 | -1) {
  const list = ordered.value
  if (!list.length) return
  const at = list.findIndex(e => e.id === selected.value?.id)
  select(list[Math.max(0, Math.min(list.length - 1, at + by))], true)
}
function onKey(event: KeyboardEvent) {
  const tag = (event.target as HTMLElement).tagName
  if (['INPUT', 'SELECT', 'TEXTAREA'].includes(tag) || event.ctrlKey || event.metaKey || event.altKey) return
  const e = selected.value
  if (event.key === 'ArrowDown' || event.key === 'j') move(1)
  else if (event.key === 'ArrowUp' || event.key === 'k') move(-1)
  else if (event.key === ' ' && e) tick(e)
  else if ((event.key === 'x' || event.key === 'X') && e) markWrong(e)
  else if ((event.key === 'f' || event.key === 'F') && viewPhotos.value.length) photo.value = (photo.value + 1) % viewPhotos.value.length
  else return
  event.preventDefault()
}

// ------------------------------------------------------------ what each row shows
/** The crops of a row: what tells its case at a glance. */
function crops(e: CurationEntry) {
  const out: { key: string; src: string; alt: string; box?: [number, number, number, number]; aspect?: number; turned?: number; cls: string; rotate?: number }[] = []
  const line = e.idLines?.find(l => l.box)
  if (e.kind === 'camid' && line?.box)
    out.push({ key: 'id', src: photoUrl(line.fileId, 1600), alt: t('Línea del ID en {name}', { name: line.name }), box: line.box, aspect: line.aspect, turned: line.turn ?? 0, cls: 'h-9 w-auto max-w-40' })
  const env = e.photos?.envelope
  if (env && e.kind !== 'turn' && e.kind !== 'side')
    out.push({ key: 'env', src: photoUrl(env.fileId, 1600), alt: t('Sobre en {name}', { name: env.name }), box: env.bbox, aspect: env.aspect, turned: env.turned ?? 0, cls: e.kind === 'camid' ? 'h-16 w-auto' : 'h-20 w-auto' })
  const files = (e.photos?.files ?? []).filter(f => e.kind !== 'turn' || f.name.toUpperCase() === String(e.evidence.photo ?? '').toUpperCase())
  for (const f of files.slice(0, e.kind === 'camid' ? 1 : 2))
    out.push({ key: f.id, src: photoUrl(f.id, 1600), alt: f.name, box: f.wings, cls: 'h-16 w-auto' })
  // A turn: the photo as it is, then as it will be.
  if (e.kind === 'turn' && files[0]) out.push({ key: 'after', src: photoUrl(files[0].id, 1600), alt: t('{name} girada', { name: files[0].name }), box: files[0].wings, cls: 'h-16 w-auto', rotate: e.fix?.drive?.[0]?.degrees ?? 0 })
  return out
}
const status = (e: CurationEntry) =>
  ({ accepted: t('Aplicado'), kept: t('Lectura mala'), planned: t('En Drive…'), queued: t('En cola'), deferred: '' })[e.decision ?? 'deferred'] ?? ''

// ------------------------------------------------------------ the section's pages, and «Aplicar»
const from = computed(() => (card.value.offset ?? 0) + 1)
const to = computed(() => (card.value.offset ?? 0) + entries.value.length)
const sectionTotal = computed(() => card.value.totalInSection ?? entries.value.length)
const paging = ref(false)
async function page(by: 1 | -1) {
  paging.value = true
  try {
    const size = Math.max(entries.value.length, 40)
    const offset = Math.max(0, (card.value.offset ?? 0) + by * size)
    const out = await api<{ proposalId: string }>('photo-curation/show', { method: 'POST', body: { ...(card.value.filters ?? {}), offset, limit: size } })
    emit('open', out.proposalId)
    window.dispatchEvent(new CustomEvent('curation:open', { detail: out.proposalId }))
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    paging.value = false
  }
}
const busy = ref(false)
const error = ref('')
async function apply() {
  if (!total.value || busy.value) return
  busy.value = true
  error.value = ''
  try {
    const out = await api<{ status: Proposal['status']; entries?: CurationEntry[]; result?: PhotoCuration['result']; written?: number; kept?: number; planned?: number; queued?: number; skipped?: string[]; failed?: string[] }>(
      `chat/proposals/${props.proposal.id}/curation/apply`,
      { method: 'POST', body: { accept: [...ticked.value], keep: [...wrong.value], values: values.value } },
    )
    props.proposal.status = out.status
    if (out.entries) card.value.entries = out.entries
    card.value.result = out.result ?? { written: out.written, kept: out.kept, planned: out.planned, queued: out.queued, skipped: out.skipped, failed: out.failed }
    // The ones decided leave the selection; the others keep what the person chose.
    const settled = new Set(entries.value.filter(e => !undecided(e)).map(e => e.id))
    ticked.value = new Set([...ticked.value].filter(id => !settled.has(id)))
    wrong.value = new Set([...wrong.value].filter(id => !settled.has(id)))
    emit('started')
    const r = card.value.result ?? {}
    notify(
      [
        r.written ? t('{n} en la base de datos', { n: r.written }) : '',
        r.planned ? t('{n} en Drive, en marcha', { n: r.planned }) : '',
        r.kept ? t('{n} lecturas malas', { n: r.kept }) : '',
      ]
        .filter(Boolean)
        .join(' · ') || t('Nada cambió'),
    )
  } catch (e) {
    error.value = errorText(e)
  } finally {
    busy.value = false
  }
}
const drive = computed(() => card.value.result?.driveProgress ?? null)
const plans = computed(() => [...new Map((card.value.result?.drive ?? []).filter(d => d.link).map(d => [d.plan ?? d.link, d])).values()])
const problems = computed(() => [...(card.value.result?.skipped ?? []), ...(card.value.result?.failed ?? [])])
</script>

<template>
  <section
    class="curation mt-2 flex h-[calc(100dvh-12rem)] min-h-[34rem] flex-col overflow-hidden rounded-md border border-stone-300 bg-white text-sm"
    data-photo-curation
  >
    <!-- The section: what it holds and where in it this page is. -->
    <header class="flex flex-wrap items-center gap-2 border-b border-stone-200 px-3 py-2">
      <ScanSearch :size="16" class="shrink-0 text-emerald-700" />
      <h3 class="font-medium text-stone-900">{{ card.title || sectionLabel(card)[0] }}</h3>
      <span v-for="s in card.title ? sectionLabel(card) : sectionLabel(card).slice(1)" :key="s" class="rounded-full border border-stone-200 bg-stone-50 px-2 py-0.5 text-[11px] text-stone-600">{{ s }}</span>
      <span class="text-xs text-stone-500 tabular-nums">{{ $t('{from}–{to} de {total}', { from, to, total: sectionTotal }) }}</span>
      <span class="ml-auto flex items-center gap-1">
        <button v-if="decidedCount" type="button" class="btn-ghost px-1.5 py-0.5 text-xs" :aria-pressed="showDecided" @click="showDecided = !showDecided">
          {{ showDecided ? $t('Ocultar resueltos') : $tn(decidedCount, 'Ver {n} resuelto', 'Ver {n} resueltos') }}
        </button>
        <button type="button" class="btn-ghost p-1" :disabled="paging || from <= 1" :title="$t('Casos anteriores')" :aria-label="$t('Casos anteriores')" @click="page(-1)"><ChevronLeft :size="15" /></button>
        <button type="button" class="btn-ghost p-1" :disabled="paging || to >= sectionTotal" :title="$t('Casos siguientes')" :aria-label="$t('Casos siguientes')" @click="page(1)"><ChevronRight :size="15" /></button>
        <button v-if="open" type="button" class="btn-ghost p-1" :title="$t('Cerrar la tarjeta')" :aria-label="$t('Cerrar la tarjeta')" @click="emit('discard')"><X :size="15" /></button>
      </span>
    </header>

    <div class="curation-body grid min-h-0 flex-1">
      <!-- The cases, by group; the selected one outlined. -->
      <div ref="listEl" class="min-h-0 overflow-y-auto outline-none" tabindex="0" data-rows @keydown="onKey">
        <template v-for="g in groups" :key="g.tier">
          <div class="sticky top-0 z-10 flex flex-wrap items-baseline gap-x-2 border-b border-stone-200 bg-stone-100 px-3 py-1.5 text-xs" :data-group="g.tier">
            <strong class="font-medium text-stone-800">{{ tierLabel(g.tier) }} · {{ g.rows.length }}</strong>
            <span class="text-stone-500">{{ tierHint(g.tier) }}</span>
            <span v-if="open && g.rows.some(e => e.fix && undecided(e))" class="ml-auto flex gap-2">
              <button type="button" class="text-emerald-800 hover:underline" @click="tickAll(g.tier, true)">{{ $t('Marcar todos') }}</button>
              <button type="button" class="text-emerald-800 hover:underline" @click="tickAll(g.tier, false)">{{ $t('Ninguno') }}</button>
            </span>
          </div>
          <div
            v-for="e in g.rows"
            :key="e.id"
            class="row grid cursor-pointer items-center gap-x-3 border-b border-stone-100 px-3 py-2"
            :class="[e.id === selected?.id ? 'bg-emerald-50/70 shadow-[inset_3px_0_0_var(--color-emerald-700)]' : 'hover:bg-stone-50', !undecided(e) && 'opacity-60']"
            :data-entry="e.id"
            @click="select(e)"
          >
            <input
              type="checkbox"
              class="size-4 rounded border-stone-400 text-emerald-700"
              :checked="ticked.has(e.id) || e.decision === 'accepted' || e.decision === 'planned' || e.decision === 'queued'"
              :disabled="!can(e) || !e.fix"
              :title="e.fix ? $t('Hacer este cambio') : $t('Sin cambio propuesto')"
              :aria-label="$t('Hacer el cambio de {cam}', { cam: e.cam })"
              @click.stop
              @change="tick(e, ($event.target as HTMLInputElement).checked)"
            />
            <div class="flex min-w-0 items-center gap-1">
              <figure v-for="c in crops(e)" :key="c.key" class="relative shrink-0 overflow-hidden rounded" :class="c.rotate ? 'flex items-center gap-1' : ''">
                <ArrowRight v-if="c.rotate" :size="12" class="text-stone-400" />
                <CropImage :src="c.src" :alt="c.alt" :box="c.box" :aspect="c.aspect" :turned="c.turned ?? 0" :class="c.cls" :style="c.rotate ? { transform: `rotate(${c.rotate}deg)` } : undefined" />
              </figure>
            </div>
            <div class="min-w-0">
              <p class="flex flex-wrap items-baseline gap-x-1.5 text-[13px]">
                <span v-if="e.kind !== 'camid' && e.kind !== 'turn'" class="font-mono text-xs text-stone-500">{{ e.cam }}</span>
                <template v-if="curationChange(e, values[e.id]).field && e.kind !== 'camid'"><span class="text-stone-500">{{ curationChange(e, values[e.id]).field }}:</span></template>
                <s v-if="curationChange(e, values[e.id]).from && curationChange(e, values[e.id]).to" class="text-stone-400">{{ curationChange(e, values[e.id]).from }}</s>
                <template v-if="curationChange(e, values[e.id]).to">→ <strong class="font-semibold text-stone-900">{{ curationChange(e, values[e.id]).to }}</strong></template>
                <span v-else class="text-stone-600">{{ curationChange(e, values[e.id]).from ? $t('{value} (sin cambio propuesto)', { value: curationChange(e, values[e.id]).from }) : $t('Sin cambio propuesto') }}</span>
                <span v-if="mixed" class="text-[11px] text-stone-400">{{ kindLabel(e.kind) }}</span>
              </p>
              <p class="mt-1 flex flex-wrap gap-1">
                <span
                  v-for="c in curationChips(e)"
                  :key="c.text"
                  class="rounded border px-1.5 py-px text-[11px]"
                  :class="c.tone === 'ok' ? 'border-emerald-200 bg-emerald-50 text-emerald-900' : c.tone === 'against' ? 'border-amber-300 bg-amber-50 text-amber-900' : 'border-stone-200 text-stone-600'"
                >{{ c.text }}</span>
              </p>
              <p v-if="e.note" class="mt-1 text-xs text-sky-900 italic">{{ e.note }}</p>
              <p v-if="changedByNote(e.records).length" class="mt-1 text-xs text-amber-900" :title="changedByNote(e.records).map(n => n.text).join('\n')">
                {{ $t('Una nota cambió {fields} después: el sobre puede estar desactualizado.', { fields: [...new Set(changedByNote(e.records).map(n => n.field === 'species' ? $t('la especie') : n.field === 'sex' ? $t('el sexo') : 'CAM_ID'))].join(', ') }) }}
              </p>
              <p v-if="e.problem && undecided(e)" class="mt-1 text-xs text-red-800">{{ $t('No se hizo en Drive: {why}', { why: e.problem }) }}</p>
              <label v-if="e.choices?.length && can(e)" class="mt-1 flex items-center gap-1.5 text-xs text-stone-600" @click.stop>
                {{ $t('Valor:') }}
                <select v-model="values[e.id]" class="rounded border border-stone-300 bg-white py-0.5 pr-6 pl-1 text-xs" @change="tick(e, true)">
                  <option value="">{{ e.fix?.to }}</option>
                  <option v-for="v in e.choices.filter(c => c !== e.fix?.to)" :key="v" :value="v">{{ v }}</option>
                </select>
              </label>
            </div>
            <div class="flex flex-col items-end gap-1 text-xs">
              <span v-if="!undecided(e)" class="font-medium" :class="e.decision === 'kept' ? 'text-stone-600' : 'text-emerald-800'">{{ status(e) }}</span>
              <button
                v-else-if="open"
                type="button"
                class="rounded border px-2 py-1 whitespace-nowrap"
                :class="wrong.has(e.id) ? 'border-red-300 bg-red-50 text-red-800' : 'border-stone-300 text-stone-600 hover:bg-stone-100'"
                :aria-pressed="wrong.has(e.id)"
                :title="$t('La base de datos está bien: la lectura no se vuelve a proponer')"
                @click.stop="markWrong(e)"
              >
                ✗ {{ $t('Lectura mala') }}
              </button>
            </div>
          </div>
        </template>
        <p v-if="!groups.length" class="p-6 text-center text-xs text-stone-500">
          {{ decidedCount ? $t('Todos los casos de esta página están resueltos.') : $t('Nada que revisar en esta sección.') }}
        </p>
      </div>

      <!-- The selected case's photos: wheel to zoom, drag to move; the ID line outlined. -->
      <div class="viewer flex min-h-0 flex-col border-stone-300">
        <div class="min-h-0 flex-1">
        <PhotoViewer
          v-if="viewPhotos.length"
          v-model:photo="photo"
          :count="viewPhotos.length"
          :url="(n, size) => photoUrl(viewPhotos[n].id, size === 'thumb' ? 400 : 1600)"
          :alt="n => viewPhotos[n].name"
          :notes="viewPhotos.map(p => (p.mark ? (selected?.kind === 'camid' ? $t('ID leído') : $t('Sobre')) : ''))"
          :links="viewPhotos.map(p => `https://drive.google.com/file/d/${p.id}/view`)"
          :marks="viewPhotos.map(p => p.mark)"
          whole
        />
        <p v-else class="grid h-full place-items-center bg-stone-800 p-6 text-center text-xs text-stone-300">{{ $t('Este caso no tiene fotos en Photo_links.') }}</p>
        </div>
        <!-- The envelope beside the database, and the notes, of the butterflies the case involves. -->
        <div v-if="selected?.records?.length" class="record max-h-[42%] shrink-0 space-y-3 overflow-y-auto border-t border-stone-300 bg-white p-2.5" data-records>
          <CurationRecord v-for="(r, i) in selected.records" :key="r.cam" :record="r" :second="i > 0" />
        </div>
      </div>
    </div>

    <!-- What was done, and what could not be. -->
    <div v-if="plans.length || problems.length" class="space-y-0.5 border-t border-stone-200 bg-stone-50 px-3 py-1.5 text-xs text-stone-700" data-result>
      <p v-for="p in plans" :key="p.link" :class="drive?.failed ? 'text-amber-900' : ''">
        <Loader2 v-if="!drive || drive.state === 'writing'" :size="12" class="inline animate-spin" />
        <template v-if="!drive || drive.state === 'writing'">{{ $t('Cambios en Drive en marcha.') }}</template>
        <template v-else-if="drive.failed">{{ $t('Cambios en Drive: {done} hechos, {failed} no se pudieron hacer.', { done: (drive.total ?? 0) - drive.failed, failed: drive.failed }) }}</template>
        <template v-else>{{ $t('Cambios en Drive hechos.') }}</template>
        {{ ' ' }}<a :href="p.link" class="inline-flex items-center gap-0.5 text-emerald-800 hover:underline">{{ $t('Ver el plan') }} <ExternalLink :size="11" /></a>
      </p>
      <p v-if="problems.length" class="text-amber-900">{{ problems.join('; ') }}</p>
    </div>

    <footer v-if="open" class="flex flex-wrap items-center gap-x-4 gap-y-1 border-t border-stone-300 bg-white px-3 py-2 text-xs">
      <span class="text-stone-700">
        <template v-if="total">
          {{ [toDrive ? $tn(toDrive, '{n} en Drive (empieza al aplicar)', '{n} en Drive (empiezan al aplicar)') : '', toDatabase ? $tn(toDatabase, '{n} en la base de datos', '{n} en la base de datos') : '', wrong.size ? $tn(wrong.size, '{n} lectura mala', '{n} lecturas malas') : ''].filter(Boolean).join(' · ') }}
        </template>
        <template v-else>{{ $t('Marca los cambios a hacer') }}</template>
      </span>
      <span class="hidden text-stone-500 lg:inline">
        <kbd>↑</kbd><kbd>↓</kbd> {{ $t('caso') }} · <kbd>{{ $t('Espacio') }}</kbd> {{ $t('marcar') }} · <kbd>X</kbd> {{ $t('lectura mala') }} · <kbd>F</kbd> {{ $t('otra foto') }}
      </span>
      <p v-if="error" role="alert" class="text-red-800">{{ error }}</p>
      <button type="button" class="btn-primary ml-auto bg-emerald-700 hover:bg-emerald-800" :disabled="!total || busy" data-apply-curation @click="apply">
        <Loader2 v-if="busy" :size="14" class="animate-spin" /><Check v-else :size="14" />
        {{ $tn(total, 'Aplicar {n}', 'Aplicar {n}') }}
      </button>
    </footer>
  </section>
</template>

<style scoped>
.curation {
  container-type: inline-size;
}
.curation-body {
  grid-template-columns: minmax(0, 1fr);
  grid-template-rows: 16rem minmax(0, 1fr);
}
.curation-body .viewer {
  order: -1;
  border-bottom-width: 1px;
}
.row {
  grid-template-columns: auto auto minmax(0, 1fr) auto;
}
@container (min-width: 960px) {
  .curation-body {
    grid-template-columns: minmax(0, 1fr) minmax(22rem, 38%);
    grid-template-rows: minmax(0, 1fr);
  }
  .curation-body .viewer {
    order: 0;
    border-bottom-width: 0;
    border-left-width: 1px;
  }
}
kbd {
  font: 11px ui-monospace, monospace;
  border: 1px solid var(--color-stone-300);
  border-bottom-width: 2px;
  border-radius: 3px;
  padding: 0 4px;
  margin: 0 1px;
  background: white;
}
</style>
