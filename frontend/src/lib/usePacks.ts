/**
 * The photo packs' state for the page (lib/photoPacks.ts), shared by the Descargas place (PackPanel.vue) and the
 * Identificar view (which saves the chosen pack by itself after a model's download): the server's list, the
 * installed pack, a save in progress and its outcome.
 */
import { ref, shallowRef, watch } from 'vue'
import { t } from './i18n'
import { installPack, installedPack, onlineIndex, packScope, removePack, resetOnlineIndex, setPackScope, type InstalledPack, type PackIndex, type PackScope } from './photoPacks'
import { retireFieldSet } from './identifyRefs'

const index = shallowRef<PackIndex | null>(null)
const indexFailed = ref(false)
const pack = shallowRef<InstalledPack | null>(null)
const progress = ref<{ done: number; total: number } | null>(null)
const note = ref<string | null>(null)
const scope = ref<PackScope>(packScope())
watch(scope, v => setPackScope(v), { deep: true })
/** Bumped after a save or a removal: the photos shown are asked again. */
const changed = ref(0)
let loading: Promise<void> | null = null

async function load(force = false) {
  if (force) resetOnlineIndex()
  loading ??= (async () => {
    pack.value = await installedPack()
    try {
      index.value = await onlineIndex()
      indexFailed.value = false
    } catch {
      indexFailed.value = !index.value
    }
  })().finally(() => (loading = null))
  return loading
}

async function install(s: PackScope = scope.value) {
  if (progress.value) return
  note.value = null
  if (!index.value) await load(true)
  if (!index.value) {
    note.value = t('Hace falta conexión a internet.')
    return
  }
  // Asked inside the tap (Chrome weighs the gesture): the phone then does not clear the photos when short of space.
  navigator.storage?.persist?.().catch(() => null)
  progress.value = { done: 0, total: 1 }
  try {
    const out = await installPack(index.value, s, (done, total) => (progress.value = { done, total }))
    pack.value = out
    await retireFieldSet(new Set(out.index.taxa.flatMap(x => (x.photo ? [new URL(x.photo.file, out.index.base).href] : []))))
    note.value = out.installed.failed ? t('{n} fotos no se guardaron: toque Actualizar fotos para completarlas.', { n: out.installed.failed }) : null
  } catch (err) {
    note.value = /quota/i.test(String((err as Error)?.message)) ? t('No hay espacio suficiente en el teléfono.') : t('Hace falta conexión a internet.')
  } finally {
    progress.value = null
    changed.value++
  }
}

async function remove() {
  if (progress.value) return
  await removePack()
  pack.value = null
  note.value = null
  changed.value++
}

export function usePacks() {
  return { index, indexFailed, pack, progress, note, scope, changed, load, install, remove }
}
