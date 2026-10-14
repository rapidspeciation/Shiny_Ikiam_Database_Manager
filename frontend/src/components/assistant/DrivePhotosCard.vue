<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import { Check, ExternalLink, Images, RotateCw, Sparkles, Trash2, Undo2, X } from 'lucide-vue-next'
import { api } from '../../lib/api'
import { errorText, notify } from '../../lib/notice'
import type { Proposal } from '../../lib/proposals'
import { newName, reasonText, shownRows, type DrivePhotoRow, type DrivePhotosSync } from '../../lib/drivePhotos'
import { t } from '../../lib/i18n'

/**
 * Photos uploaded to Drive without being renamed, read on the server (server/drive-photos.mjs), as a
 * card of Cambios propuestos: how the reading goes, then a tile per photo (the photo or its envelope,
 * turned as it will be) with the ID read, the name it takes, and what to look at. The ID, the view, a
 * turn and «quitar» are changed here; «Aplicar en Drive» makes the changes (a plan of the files' card).
 */
const props = defineProps<{ proposal: Proposal }>()
const emit = defineEmits<{ discard: [] }>()

const sync = computed(() => (props.proposal.sync ?? {}) as unknown as DrivePhotosSync)
const base = computed(() => `drive-photos/${sync.value.batch}`)
const rows = ref<DrivePhotoRow[]>([])
const which = ref<'look' | 'all'>('look')
const picture = ref<'photo' | 'envelope'>('photo')
const busy = ref(false)

async function load() {
  if (sync.value.state === 'reading' || !sync.value.batch) return
  try {
    rows.value = (await api<{ rows: DrivePhotoRow[] }>(base.value)).rows
  } catch (e) {
    notify(errorText(e), 'error')
  }
}
watch(() => [props.proposal.revision, sync.value.state], load, { immediate: true })

const shown = computed(() => shownRows(rows.value, which.value))
const toLook = computed(() => shownRows(rows.value, 'look').length)
const changes = computed(() => rows.value.filter(r => r.read && (r.remove || r.rotation || (newName(r) && newName(r) !== r.name))).length)
const percent = computed(() => (sync.value.photos ? Math.round(((sync.value.read ?? 0) / sync.value.photos) * 100) : 0))

async function edit(row: DrivePhotoRow, change: Record<string, unknown>) {
  try {
    await api(`${base.value}/edit`, { method: 'POST', body: { changes: [{ file: row.file, ...change }] } })
    await load()
  } catch (e) {
    notify(errorText(e), 'error')
  }
}
const setId = (row: DrivePhotoRow, value: string) => {
  const id = value.trim().toUpperCase()
  if (id !== (row.camid ?? '')) void edit(row, { id })
}
async function apply() {
  busy.value = true
  try {
    const out = await api<{ plan: string; leftOut?: string[] }>(`${base.value}/apply`, { method: 'POST', body: {} })
    notify(out.leftOut?.length ? t('Cambios en marcha; {n} fotos sin ID quedan como están', { n: out.leftOut.length }) : t('Cambios en marcha en Drive'))
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    busy.value = false
  }
}
const src = (row: DrivePhotoRow) => `api/${base.value}/${row.file}/${picture.value}`
</script>

<template>
  <div class="mt-2 rounded-md border border-stone-300 bg-white text-sm" data-drive-photos>
    <p class="flex flex-wrap items-center gap-x-2 gap-y-1 border-b border-stone-200 px-3 py-2">
      <Images :size="15" class="shrink-0 text-emerald-700" />
      <strong class="font-medium">{{ proposal.reason }}</strong>
      <span v-if="sync.state !== 'reading' && sync.photos" class="text-xs text-stone-500">
        {{ $t('{photos} fotos, {ids} IDs', { photos: sync.photos, ids: sync.ids ?? 0 }) }}
      </span>
    </p>

    <!-- Being read. -->
    <div v-if="sync.state === 'reading'" class="px-3 py-3" data-reading>
      <p class="mb-1.5 text-xs text-stone-700">
        {{ $t('Leyendo los sobres en el servidor: {read} de {photos} fotos (cada una se borra del servidor al leerla)', { read: sync.read ?? 0, photos: sync.photos ?? 0 }) }}
      </p>
      <div class="h-2.5 overflow-hidden rounded-full bg-stone-200" role="progressbar" aria-valuemin="0" aria-valuemax="100" :aria-valuenow="percent">
        <div class="h-full rounded-full bg-emerald-600 transition-[width] duration-500" :style="{ width: `${percent}%` }" />
      </div>
    </div>
    <p v-else-if="sync.state === 'failed'" class="px-3 py-2 text-xs text-red-800">{{ $t('No se pudo leer: {error}', { error: sync.error ?? '' }) }}</p>
    <p v-else-if="sync.state === 'applied'" class="flex items-center gap-1.5 px-3 py-2 text-xs text-emerald-800" data-applied>
      <Check :size="14" /> {{ $t('Aplicado: los cambios en Drive siguen en su propia tarjeta.') }}
    </p>
    <p v-else-if="sync.state === 'discarded'" class="px-3 py-2 text-xs text-stone-500">{{ $t('Descartada') }}</p>

    <template v-if="sync.state === 'ready'">
      <div class="flex flex-wrap items-center gap-2 border-b border-stone-200 px-3 py-1.5 text-xs">
        <div class="flex rounded border border-stone-300 p-0.5">
          <button
            v-for="[key, label] in [['look', $t('Para revisar ({n})', { n: toLook })], ['all', $t('Todas ({n})', { n: rows.length })]] as const"
            :key="key"
            class="rounded px-1.5 py-0.5"
            :class="which === key ? 'bg-emerald-700 text-white' : 'text-stone-600'"
            @click="which = key"
          >
            {{ label }}
          </button>
        </div>
        <div class="flex rounded border border-stone-300 p-0.5">
          <button
            v-for="[key, label] in [['photo', $t('Fotos')], ['envelope', $t('Sobres')]] as const"
            :key="key"
            class="rounded px-1.5 py-0.5"
            :class="picture === key ? 'bg-stone-700 text-white' : 'text-stone-600'"
            @click="picture = key"
          >
            {{ label }}
          </button>
        </div>
        <span v-if="sync.order" class="min-w-0 flex-1 truncate text-stone-500" :title="sync.order">{{ sync.order }}</span>
      </div>
      <p v-if="!shown.length" class="px-3 py-3 text-xs text-stone-600">{{ $t('Nada por revisar: cada ID tiene sus dos fotos, en orden.') }}</p>
      <ul class="grid max-h-[32rem] grid-cols-[repeat(auto-fill,minmax(9.5rem,1fr))] gap-2 overflow-y-auto p-2" data-tiles>
        <li v-for="r in shown" :key="r.file" class="rounded border text-xs" :class="r.remove ? 'border-red-200 bg-red-50 opacity-70' : r.marked ? 'border-amber-300' : 'border-stone-200'" :data-file="r.file">
          <div class="flex h-28 items-center justify-center overflow-hidden bg-stone-100">
            <img :src="src(r)" :alt="r.name" loading="lazy" class="max-h-full max-w-full transition-transform" :style="{ transform: `rotate(${picture === 'photo' ? r.rotation ?? 0 : 0}deg)` }" />
          </div>
          <div class="space-y-1 p-1.5">
            <div class="flex items-center gap-1">
              <input
                :value="r.camid ?? ''"
                class="w-full min-w-0 rounded border border-stone-300 px-1 py-0.5 font-mono text-[11px] uppercase"
                :aria-label="$t('ID de {name}', { name: r.name })"
                :disabled="r.remove"
                @change="setId(r, ($event.target as HTMLInputElement).value)"
                @keydown.enter="($event.target as HTMLInputElement).blur()"
              />
              <select
                :value="r.view"
                class="rounded border border-stone-300 py-0.5 pr-4 pl-1 text-[11px]"
                :aria-label="$t('Vista')"
                :disabled="r.remove || !r.camid"
                @change="edit(r, { view: ($event.target as HTMLSelectElement).value })"
              >
                <option v-for="v in ['d', 'v', 'd2', 'v2']" :key="v" :value="v">{{ v }}</option>
              </select>
            </div>
            <p class="truncate text-stone-500" :title="r.name">
              {{ r.name }}<template v-if="newName(r)"> → <span class="text-stone-800">{{ newName(r) }}</span></template>
            </p>
            <!-- The reader's ID beside the one set after, and by whom. -->
            <p v-if="r.readAs && r.readAs !== r.camid" class="text-[11px] text-stone-500">
              <Sparkles v-if="r.setBy === 'assistant'" :size="10" class="inline text-sky-600" />
              {{ $t('leído {read}', { read: r.readAs }) }}
            </p>
            <p v-for="why in r.review ?? []" :key="why" class="text-[11px] text-amber-800">{{ reasonText(why) }}</p>
            <p v-if="r.note" class="text-[11px] text-stone-600 italic">{{ r.note }}</p>
            <p v-if="r.error" class="text-[11px] text-red-700">{{ r.error }}</p>
            <div class="flex items-center gap-1">
              <button class="btn-ghost p-0.5" :title="$t('Girar 90° a la derecha')" :disabled="r.remove" @click="edit(r, { rotation: ((r.rotation ?? 0) + 90) % 360 })">
                <RotateCw :size="13" />
              </button>
              <span v-if="r.rotation" class="text-[11px] text-stone-500">{{ r.rotation }}°</span>
              <a v-if="r.url" class="btn-ghost p-0.5" :href="r.url" target="_blank" rel="noopener" :title="$t('Abrir en Drive')"><ExternalLink :size="13" /></a>
              <button
                class="btn-ghost ml-auto p-0.5"
                :class="r.remove ? 'text-stone-600' : 'text-red-700'"
                :title="r.remove ? $t('No quitar') : $t('Quitar esta foto (al aplicar)')"
                @click="edit(r, { remove: !r.remove })"
              >
                <Undo2 v-if="r.remove" :size="13" /><Trash2 v-else :size="13" />
              </button>
            </div>
          </div>
        </li>
      </ul>
      <div class="flex flex-wrap items-center gap-2 border-t border-stone-200 px-3 py-2">
        <button class="btn-primary bg-emerald-700 hover:bg-emerald-800" :disabled="busy || !changes" data-apply @click="apply">
          <Check :size="15" /> {{ $tn(changes, 'Aplicar en Drive ({n} foto)', 'Aplicar en Drive ({n} fotos)') }}
        </button>
        <span class="text-xs text-stone-500">{{ $t('Renombra cada foto y su RAW, las gira y quita las marcadas. Después, «Leer Drive» las lista en Photo_links.') }}</span>
        <button class="btn ml-auto" :disabled="busy" @click="emit('discard')"><X :size="14" /> {{ $t('Descartar') }}</button>
      </div>
    </template>
  </div>
</template>
