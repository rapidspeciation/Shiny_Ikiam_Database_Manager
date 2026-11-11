/**
 * The photo packs' state for the page (lib/photoPacks.ts), shared by the Descargas place (PackPanel.vue), the search
 * and the Identificar view: the installed pack (read from the phone, cheap), the server's list (read only where it is
 * needed: Descargas, the search, a photo the pack lacks), a save in progress and its outcome.
 *
 * The one-time save after the first model (AUTO): only the first screen's download button, which says it, asks for
 * it ('pending'). It becomes 'done' once a save returns a pack, and also when the person saves, deletes or removes the
 * older set by hand, so a Delete never brings a pack back. A first save cut off (page closed, signal lost) stays
 * 'pending' and is tried again on the next open with a model and signal.
 */
import { ref, shallowRef, watch } from 'vue'
import { locale, t } from './i18n'
import { installPack, installedPack, numText, onlineIndex, packScope, removePack, resetOnlineIndex, revalidateIndex, setPackScope, type InstalledPack, type PackIndex, type PackScope, type Region, type TaxaScope } from './photoPacks'
import { retireFieldSet } from './identifyRefs'

/** A taxa choice's name (the scientific ones are not translated). */
export function taxaLabel(k: TaxaScope): string {
  return k === 'ithomiini' ? 'Ithomiini' : k === 'nymphalidae' ? 'Nymphalidae' : k === 'butterflies' ? t('Mariposas') : t('Mariposas y polillas')
}

/** A country's name in the app's language (ISO2 when the browser cannot say). */
export function countryName(iso: string): string {
  try {
    return new Intl.DisplayNames([locale.value === 'es' ? 'es' : 'en'], { type: 'region' }).of(iso) ?? iso
  } catch {
    return iso
  }
}
/** A region choice's name: «Neotrópico», a country, or «3 países». */
export const regionLabel = (r: Region) => (r === 'neotropics' ? t('Neotrópico') : r.length === 1 ? countryName(r[0]) : t('{n} países', { n: r.length }))

const AUTO = 'identify:pack-auto'
const getAuto = () => {
  try {
    return localStorage.getItem(AUTO)
  } catch {
    return null
  }
}
const setAuto = (v: 'pending' | 'done') => {
  try {
    localStorage.setItem(AUTO, v)
  } catch {
    /* private mode */
  }
}

const index = shallowRef<PackIndex | null>(null)
const indexFailed = ref(false)
const pack = shallowRef<InstalledPack | null>(null)
const progress = ref<{ done: number; total: number } | null>(null)
/** A Delete under way (its photos go a while after the pack is off the screen). */
const removing = ref(false)
const note = ref<string | null>(null)
/** The last save did not complete: what it was, for «Reintentar». */
const failedSave = ref<{ scope: PackScope; failed: number; total: number } | null>(null)
const scope = ref<PackScope>(packScope())
watch(scope, v => setPackScope(v), { deep: true })
/** Bumped after a save or a removal: the photos shown are asked again. */
const changed = ref(0)
let loadingIndex: Promise<void> | null = null

/** The installed pack, from the phone (no network). */
async function loadInstalled() {
  pack.value = await installedPack()
}
/** When the list was last checked against the server. */
let checkedAt = 0
/**
 * The server's list checked again (Descargas opened, the page back in front, a save about to start): a new list
 * replaces the one shown, so its sizes and «Actualizar fotos» follow the server. At most every `minMs`.
 */
async function refreshIndex(minMs = 15_000) {
  if (!index.value) return loadIndex()
  if (Date.now() - checkedAt < minMs || !navigator.onLine) return
  checkedAt = Date.now()
  const fresh = await revalidateIndex().catch(() => null)
  if (fresh && fresh !== index.value) index.value = fresh
}
if (typeof document !== 'undefined')
  document.addEventListener('visibilitychange', () => {
    // A phone keeps the page for days: back in front after a while, the list is checked again.
    if (document.visibilityState === 'visible' && index.value) refreshIndex(10 * 60_000)
  })
/** The server's list (for sizes, the search beyond the pack, updates). */
async function loadIndex(force = false) {
  if (index.value && !force) return
  if (force) resetOnlineIndex()
  loadingIndex ??= (async () => {
    try {
      index.value = await onlineIndex()
      checkedAt = Date.now()
      indexFailed.value = false
    } catch {
      indexFailed.value = !index.value
    }
  })().finally(() => (loadingIndex = null))
  return loadingIndex
}
async function load(force = false) {
  await loadInstalled()
  await loadIndex(force)
}
/** Descargas opened: the installed pack, and the server's list checked again. */
async function open() {
  await loadInstalled()
  await refreshIndex()
}

async function install(s: PackScope = scope.value) {
  if (progress.value || removing.value) return null
  note.value = null
  failedSave.value = null
  // Never a save from a list the server has replaced since.
  await refreshIndex(0)
  if (!index.value) {
    note.value = t('Hace falta conexión a internet.')
    return null
  }
  // Asked inside the tap (Chrome weighs the gesture): the phone then does not clear the photos when short of space.
  navigator.storage?.persist?.().catch(() => null)
  progress.value = { done: 0, total: 1 }
  try {
    const out = await installPack(index.value, s, (done, total) => (progress.value = { done, total }))
    pack.value = out.pack
    if (out.pack) setAuto('done')
    if (out.kept) {
      failedSave.value = { scope: s, failed: out.failed, total: out.total }
      note.value = out.pack
        ? t('{n} de {total} fotos no se descargaron: siguen las fotos de antes.', { n: numText(out.failed), total: numText(out.total) })
        : t('No se descargaron las fotos.')
    } else if (out.failed) {
      note.value = t('{n} fotos no se guardaron: toque Actualizar fotos para completarlas.', { n: numText(out.failed) })
    } else {
      // Replaced in full: the older «Fotos para el campo» set goes (its files this pack uses stay).
      await retireFieldSet(new Set(out.pack!.index.taxa.flatMap(x => x.photos.map(ph => new URL(ph.file, out.pack!.index.base).href))))
    }
    return out
  } catch (err) {
    failedSave.value = { scope: s, failed: 0, total: 0 }
    note.value = /quota/i.test(String((err as Error)?.message)) ? t('No hay espacio suficiente en el teléfono.') : t('Hace falta conexión a internet.')
    return null
  } finally {
    progress.value = null
    changed.value++
  }
}

async function remove() {
  if (progress.value || removing.value) return
  setAuto('done')
  // Off the screen at once; the photos go after (lib/photoPacks.ts removePack: the record first).
  pack.value = null
  note.value = null
  failedSave.value = null
  removing.value = true
  try {
    await removePack()
  } finally {
    removing.value = false
    changed.value++
  }
}
/** The older «Fotos para el campo» set off the phone (Descargas' Delete on it). */
async function removeLegacy() {
  setAuto('done')
  await retireFieldSet()
  changed.value++
}

export function usePacks() {
  return {
    index,
    indexFailed,
    pack,
    progress,
    removing,
    note,
    failedSave,
    scope,
    changed,
    load,
    open,
    loadInstalled,
    loadIndex,
    refreshIndex,
    install,
    remove,
    removeLegacy,
    /** The first screen's button: a pack is to follow the model. */
    askAuto: () => getAuto() !== 'done' && setAuto('pending'),
    autoPending: () => getAuto() === 'pending',
    autoDone: () => getAuto() === 'done',
  }
}
