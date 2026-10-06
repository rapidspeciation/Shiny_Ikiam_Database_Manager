import { ref, watch } from 'vue'
import { api, ApiError, requestId } from '../lib/api'
import type { CensusDetail, CensusMark, CensusOverview, DeathEdit, Doubt, MarkKind } from '../lib/census'
import { errorText, notify } from '../lib/notice'
import { persistentRef } from '../lib/persist'
import { useLive } from '../stores/live'
import { useSession } from '../stores/session'

/** What a tap on a butterfly got: marked now, or someone marked it first. */
export interface MarkResult {
  mark: CensusMark
  already: boolean
}

/**
 * The Censo tab's state, shared by its screens (server/census.mjs): the
 * censuses (start, open ones, history), the census open on this device (kept
 * in this browser: a reload or a lost signal comes back to it) and everyone's
 * marks, which follow the others' phones through /api/pulse. A tap shows its
 * mark at once and the server's answer replaces it.
 */
function create() {
  const live = useLive()
  const session = useSession()
  const overview = ref<CensusOverview | null>(null)
  const detail = ref<CensusDetail | null>(null)
  const currentId = persistentRef<string | null>('census:current', null, { lasting: true })
  const loading = ref(false)
  /** This device's marks the server has not answered yet (kept over a reload of the census meanwhile). */
  const sending = new Map<string, CensusMark>()

  async function loadOverview() {
    try {
      overview.value = await api<CensusOverview>('census')
    } catch (e) {
      notify(errorText(e), 'error')
    }
  }
  async function loadDetail() {
    const id = currentId.value
    if (!id) return (detail.value = null)
    loading.value = !detail.value || detail.value.census.id !== id
    try {
      const out = await api<CensusDetail>(`census/${encodeURIComponent(id)}`)
      if (currentId.value !== id) return
      out.marks.push(...[...sending.values()].filter(m => !out.marks.some(o => o.recordId && o.recordId === m.recordId)))
      detail.value = out
    } catch (e) {
      // Gone (another database, a wrong link): back to the list.
      if (e instanceof ApiError && e.status === 404) {
        currentId.value = null
        detail.value = null
      } else notify(errorText(e), 'error')
    } finally {
      loading.value = false
    }
  }
  function open(id: string | null) {
    currentId.value = id
    detail.value = null
    if (id) void loadDetail()
    else void loadOverview()
  }

  async function start(species: string, day: string) {
    const out = await api<CensusDetail & { joined: boolean }>('census', {
      method: 'POST',
      body: { requestId: requestId(), species, day },
    })
    currentId.value = out.census.id
    detail.value = out
    void loadOverview()
    return out
  }

  /** Marks a butterfly (seen, or left out with why) or an ID in no row; shown at once, the server's answer replaces it. */
  async function mark(body: {
    recordId?: string
    insectaryId: string
    species?: string | null
    kind?: MarkKind
    text?: string
    note?: string
  }): Promise<MarkResult | null> {
    const census = detail.value?.census
    if (!census) return null
    const rid = requestId()
    const now = new Date().toISOString()
    const temp: CensusMark = {
      id: `sending:${rid}`,
      recordId: body.recordId ?? null,
      insectaryId: body.insectaryId,
      kind: body.kind ?? 'seen',
      species: body.species ?? null,
      doubt: null,
      note: body.note ?? null,
      actor: session.user?.id ?? '',
      actorName: session.user?.displayName ?? '',
      createdAt: now,
      updatedAt: now,
      sending: true,
    }
    sending.set(temp.id, temp)
    detail.value!.marks.push(temp)
    const drop = () => {
      sending.delete(temp.id)
      if (detail.value) detail.value.marks = detail.value.marks.filter(m => m.id !== temp.id)
    }
    try {
      const out = await api<{ mark: CensusMark; already?: boolean }>(`census/${encodeURIComponent(census.id)}/marks`, {
        method: 'POST',
        body: { requestId: rid, recordId: body.recordId, kind: body.kind ?? 'seen', text: body.text, note: body.note },
      })
      drop()
      if (detail.value?.census.id === census.id) {
        const marks = detail.value.marks.filter(
          m => m.id !== out.mark.id && !(out.mark.recordId && m.recordId === out.mark.recordId),
        )
        detail.value.marks = [...marks, out.mark]
      }
      return { mark: out.mark, already: !!out.already }
    } catch (e) {
      drop()
      notify(errorText(e), 'error')
      return null
    }
  }
  async function unmark(markId: string) {
    const census = detail.value?.census
    if (!census || markId.startsWith('sending:')) return
    const before = detail.value!.marks
    detail.value!.marks = before.filter(m => m.id !== markId)
    try {
      await api(`census/${encodeURIComponent(census.id)}/marks/${encodeURIComponent(markId)}`, { method: 'DELETE', body: {} })
    } catch (e) {
      if (detail.value) detail.value.marks = before
      notify(errorText(e), 'error')
    }
  }
  async function setDoubt(markId: string, doubt: Doubt | null, note: string | null) {
    const census = detail.value?.census
    if (!census || markId.startsWith('sending:')) return
    try {
      const out = await api<{ mark: CensusMark }>(`census/${encodeURIComponent(census.id)}/marks/${encodeURIComponent(markId)}`, {
        method: 'PATCH',
        body: { doubt, note },
      })
      if (detail.value) detail.value.marks = detail.value.marks.map(m => (m.id === out.mark.id ? out.mark : m))
    } catch (e) {
      notify(errorText(e), 'error')
    }
  }
  /** Finishes the census with the disappearances' cells (lib/census.ts disappearanceEdits); kept in the app until saved. */
  let finishId: string | null = null
  async function finish(edits: DeathEdit[]) {
    const census = detail.value?.census
    if (!census) return null
    finishId ??= requestId()
    try {
      const out = await api<CensusDetail>(`census/${encodeURIComponent(census.id)}/finish`, {
        method: 'POST',
        body: { requestId: finishId, edits },
      })
      finishId = null
      detail.value = out
      await live.refresh()
      void loadOverview()
      return out
    } catch (e) {
      // Only an unclear outcome keeps the request for a safe retry.
      if (!(e instanceof ApiError) || !['OFFLINE', 'SERVER_ERROR'].includes(e.code)) finishId = null
      throw e
    }
  }
  async function action(path: 'reopen' | 'cancel') {
    const census = detail.value?.census
    if (!census) return
    try {
      detail.value = await api<CensusDetail>(`census/${encodeURIComponent(census.id)}/${path}`, { method: 'POST', body: {} })
      await live.refresh()
      void loadOverview()
    } catch (e) {
      notify(errorText(e), 'error')
    }
  }
  async function notebook(done: boolean) {
    const census = detail.value?.census
    if (!census) return
    try {
      detail.value = await api<CensusDetail>(`census/${encodeURIComponent(census.id)}/notebook`, {
        method: 'PUT',
        body: { done },
      })
    } catch (e) {
      notify(errorText(e), 'error')
    }
  }

  // Someone else marked, started or finished a census: the open one and the lists follow.
  watch(
    () => live.census,
    (now, before) => {
      if (now === before) return
      if (currentId.value) void loadDetail()
      void loadOverview()
    },
  )

  return {
    overview,
    detail,
    currentId,
    loading,
    loadOverview,
    loadDetail,
    open,
    start,
    mark,
    unmark,
    setDoubt,
    finish,
    reopen: () => action('reopen'),
    cancel: () => action('cancel'),
    notebook,
  }
}

let shared: ReturnType<typeof create> | null = null
export function useCensus() {
  return (shared ??= create())
}
