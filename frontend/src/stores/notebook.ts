import { defineStore } from 'pinia'
import { api, requestId } from '../lib/api'
import { errorText, notify } from '../lib/notice'
import { fitSize, mergeEdits, type Job, type JobDetail, type Kind } from '../lib/notebook'
import { useTables } from './tables'

/**
 * The pages being digitized. Photos are shrunk and uploaded one after another
 * while the person keeps photographing; the server reads each one in the
 * background, and this store follows the list (long polling) while the screen
 * is open or a page is still being read. Everything lives on the server, so a
 * reload finds the pages again.
 */
export interface Upload {
  localId: string
  name: string
  url: string
  status: 'subiendo' | 'error'
  error?: string
}

/** Shrinks a photo (orientation from the camera kept) to a JPEG legible up to 2400 px. */
async function shrink(file: File): Promise<string> {
  const bitmap = await createImageBitmap(file, { imageOrientation: 'from-image' })
  const size = fitSize(bitmap.width, bitmap.height, 2400)
  const canvas = document.createElement('canvas')
  canvas.width = size.width
  canvas.height = size.height
  canvas.getContext('2d')!.drawImage(bitmap, 0, 0, size.width, size.height)
  bitmap.close()
  const blob = await new Promise<Blob>((resolve, reject) =>
    canvas.toBlob(b => (b ? resolve(b) : reject(new Error('No se pudo preparar la foto'))), 'image/jpeg', 0.85),
  )
  const bytes = new Uint8Array(await blob.arrayBuffer())
  let binary = ''
  for (let i = 0; i < bytes.length; i += 0x8000) binary += String.fromCharCode(...bytes.subarray(i, i + 0x8000))
  return btoa(binary)
}
/** The original file's hash: the same photo sent twice is recognised even if shrunk differently. */
async function hashOf(file: File) {
  const digest = new Uint8Array(await crypto.subtle.digest('SHA-256', await file.arrayBuffer()))
  return [...digest].map(b => b.toString(16).padStart(2, '0')).join('')
}

const sleep = (ms: number) => new Promise(resolve => setTimeout(resolve, ms))

export const useNotebook = defineStore('notebook', {
  state: () => ({
    jobs: [] as Job[],
    uploads: [] as Upload[],
    details: {} as Record<string, JobDetail>,
    revision: '',
    loaded: false,
    watchers: 0,
    following: false,
    /** Corrections waiting to be sent, per page. */
    waiting: {} as Record<string, { edits: Record<number, Record<string, string | null>>; picks: Record<number, boolean> }>,
    saving: {} as Record<string, boolean>,
  }),
  getters: {
    open: s => s.jobs.filter(j => ['queued', 'reading', 'ready', 'error'].includes(j.status)),
    busy: s => s.uploads.some(u => u.status === 'subiendo') || s.jobs.some(j => j.status === 'queued' || j.status === 'reading'),
  },
  actions: {
    /** Follows the list while a screen watches it or pages are still being read. */
    async follow() {
      if (this.following) return
      this.following = true
      try {
        while (this.watchers > 0 || this.busy || !this.loaded) {
          if (document.visibilityState !== 'visible') {
            await sleep(1500)
            continue
          }
          try {
            const out = await api<{ revision: string; jobs: Job[] }>(
              `notebook/jobs?${this.loaded ? `wait=1&revision=${encodeURIComponent(this.revision)}` : ''}`,
            )
            this.receive(out.jobs)
            this.revision = out.revision
            this.loaded = true
          } catch {
            await sleep(5000)
          }
        }
      } finally {
        this.following = false
      }
    },
    watch() {
      this.watchers++
      void this.follow()
      return () => {
        this.watchers = Math.max(0, this.watchers - 1)
      }
    },
    receive(jobs: Job[]) {
      const before = new Map(this.jobs.map(j => [j.id, j]))
      this.jobs = jobs
      for (const job of jobs) {
        const old = before.get(job.id)
        // A page just read, or changed elsewhere (applied in Cambios propuestos): its detail is fetched again.
        if (this.details[job.id] && old && (old.updatedAt !== job.updatedAt || old.proposalStatus !== job.proposalStatus))
          void this.load(job.id)
        if (old && old.status !== job.status && job.status === 'ready' && old.status !== 'ready')
          notify(`Página lista para revisar (${job.label})`, 'success')
      }
    },

    /** Uploads the photos in order; each becomes a page read in the background. */
    async addPhotos(files: File[], kind: Kind | 'auto', year: number | null) {
      const items = files.map(file => {
        const upload: Upload = { localId: crypto.randomUUID(), name: file.name || 'foto.jpg', url: URL.createObjectURL(file), status: 'subiendo' }
        this.uploads.push(upload)
        return { file, upload }
      })
      void this.follow()
      const created: string[] = []
      for (const { file, upload } of items) {
        try {
          const [dataBase64, sourceHash] = await Promise.all([shrink(file), hashOf(file)])
          const { attachment } = await api<{ attachment: { id: string } }>('attachments', {
            method: 'POST',
            body: { name: upload.name, mimeType: 'image/jpeg', dataBase64, requestId: requestId() },
          })
          const { job } = await api<{ job: Job }>('notebook/jobs', {
            method: 'POST',
            body: { attachmentId: attachment.id, kind, year, sourceHash, name: upload.name },
          })
          this.jobs = [job, ...this.jobs.filter(j => j.id !== job.id)]
          this.uploads = this.uploads.filter(u => u.localId !== upload.localId)
          URL.revokeObjectURL(upload.url)
          created.push(job.id)
        } catch (e) {
          const found = this.uploads.find(u => u.localId === upload.localId)
          if (found) Object.assign(found, { status: 'error', error: errorText(e) })
        }
      }
      void this.follow()
      return created
    },
    dropUpload(localId: string) {
      const upload = this.uploads.find(u => u.localId === localId)
      if (upload) URL.revokeObjectURL(upload.url)
      this.uploads = this.uploads.filter(u => u.localId !== localId)
    },

    async load(id: string) {
      try {
        const { job } = await api<{ job: JobDetail }>(`notebook/jobs/${id}`)
        // Corrections still waiting to be sent win over what the server had.
        if (!this.waiting[id]) this.details[id] = job
        return job
      } catch (e) {
        notify(errorText(e), 'error')
        return null
      }
    },

    /** A typed or picked cell: shown at once, sent together with the others a moment later. */
    edit(id: string, line: number, field: string, value: string | null) {
      const w = (this.waiting[id] ??= { edits: {}, picks: {} })
      mergeEdits(w.edits, line, field, value)
      const cell = this.details[id]?.reviewLines.find(l => l.n === line)?.cells[field]
      if (cell) Object.assign(cell, { value, edited: true, doubt: false })
      this.schedule(id)
    },
    pick(id: string, line: number, on: boolean) {
      const w = (this.waiting[id] ??= { edits: {}, picks: {} })
      w.picks[line] = on
      const found = this.details[id]?.reviewLines.find(l => l.n === line)
      if (found) found.picked = on
      this.schedule(id)
    },
    schedule(id: string) {
      clearTimeout(timers.get(id))
      timers.set(id, window.setTimeout(() => void this.flush(id), 350))
    },
    async flush(id: string) {
      clearTimeout(timers.get(id))
      const w = this.waiting[id]
      if (!w) return
      if (this.saving[id]) return this.schedule(id)
      delete this.waiting[id]
      this.saving[id] = true
      try {
        const { job } = await api<{ job: JobDetail }>(`notebook/jobs/${id}`, { method: 'PATCH', body: w })
        if (!this.waiting[id]) this.details[id] = job
      } catch (e) {
        notify(errorText(e), 'error')
        void this.load(id)
      } finally {
        this.saving[id] = false
      }
    },
    async setYear(id: string, year: number | null) {
      await this.flush(id)
      const { job } = await api<{ job: JobDetail }>(`notebook/jobs/${id}`, { method: 'PATCH', body: { year } })
      this.details[id] = job
    },

    async apply(id: string, lines: number[]) {
      await this.flush(id)
      try {
        const out = await api<{ job: JobDetail; result: { status: string; applied: number } }>(`notebook/jobs/${id}/apply`, {
          method: 'POST',
          body: { lines, requestId: requestId() },
        })
        this.details[id] = out.job
        this.jobs = this.jobs.map(j => (j.id === id ? { ...j, ...out.job } : j))
        notify(
          `${out.result.applied} ${out.result.applied === 1 ? 'fila aplicada' : 'filas aplicadas'} en Google Sheets`,
          out.result.status === 'applied' ? 'success' : 'error',
        )
        const sheet = out.job.sheet
        const tables = useTables()
        if (sheet && tables.tables[sheet]) void tables.load(sheet, true)
        return true
      } catch (e) {
        notify(errorText(e), 'error')
        void this.load(id)
        return false
      }
    },
    async discard(id: string) {
      delete this.waiting[id]
      const { job } = await api<{ job: Job }>(`notebook/jobs/${id}/discard`, { method: 'POST', body: {} })
      this.jobs = this.jobs.map(j => (j.id === id ? job : j))
      delete this.details[id]
    },
    async retry(id: string, kind: Kind | 'auto') {
      delete this.waiting[id]
      const { job } = await api<{ job: Job }>(`notebook/jobs/${id}/retry`, { method: 'POST', body: { kind } })
      this.jobs = this.jobs.map(j => (j.id === id ? job : j))
      delete this.details[id]
      void this.follow()
    },
  },
})

const timers = new Map<string, number>()
