import { afterEach, describe, expect, it, vi } from 'vitest'
import { createApp, h } from 'vue'
import { locale, t } from '../../lib/i18n'
import DownloadsPanel, { type ModelRow, type SexRow } from '../identify/DownloadsPanel.vue'

// Identificar → Descargas with model versions: «Nueva versión disponible» and «Actualizar» when the server lists a
// newer version than the phone's (or the phone recorded none), nothing when they are the same, the progress and the
// check during an update, in Spanish and in English. The view's state is given directly; no server, no AI.

vi.mock('../identify/PackPanel.vue', () => ({ default: { render: () => null } }))

let host: HTMLElement | null = null
const unmounts: (() => void)[] = []
function show(props: { sex?: SexRow | null; models?: ModelRow[]; download?: { kind: 'small' | 'full'; done: number; total: number; waiting?: boolean } | null }, events: Record<string, () => void> = {}) {
  host = document.createElement('div')
  document.body.append(host)
  const app = createApp({
    render: () =>
      h(DownloadsPanel, {
        models: props.models ?? [],
        active: 'small',
        download: props.download ?? null,
        error: null,
        lacksSpace: null,
        iosNote: false,
        fromHf: false,
        sex: props.sex ?? null,
        online: true,
        ...events,
      }),
  })
  app.config.globalProperties.$t = t
  app.mount(host)
  unmounts.push(() => app.unmount())
  return host
}
afterEach(() => {
  unmounts.splice(0).forEach(u => u())
  host?.remove()
  host = null
  locale.value = 'en'
})
const text = (el: Element | null | undefined) => el?.textContent?.replace(/\s+/g, ' ').trim() ?? ''
const C = 'field-sex-C-20261111'
const NOTE = 'Más lento que el modelo B: 0,92 s por foto en una computadora portátil (B: 0,15 s). En teléfonos aún no se ha medido.'
const sexRow = (o: Partial<SexRow> = {}): SexRow => ({ genera: 13, ready: true, outdated: false, downloadMb: 172, takesMb: 43, progress: null, error: null, versions: { from: 'B 2026-11-08', to: 'B 2026-11-08', fromMb: 43, toMb: 43 }, ...o })
/** Model C listed, model B on the phone (the numbers Descargas gets from the view: 10^6-byte MB). */
const update = (o: Partial<SexRow> = {}) => sexRow({ outdated: true, note: NOTE, versions: { from: 'B 2026-11-08', to: C, fromMb: 43, toMb: 172 }, ...o })
const modelRow = (o: Partial<ModelRow> = {}): ModelRow => ({ kind: 'small', label: 'Rápido y liviano', ready: true, outdated: false, downloadMb: 25, takesMb: 65, recommended: true, ...o })

describe('Descargas: model versions', () => {
  it('the phone has no stored version, the server lists C: «Nueva versión disponible», 172 MB and «Actualizar»', async () => {
    locale.value = 'es'
    let asked = 0
    const el = show({ sex: update({ versions: { from: null, to: C, fromMb: 43, toMb: 172 } }) }, { onSexDownload: () => asked++ })
    const sex = el.querySelector('[data-testid=dl-sex]')!
    const notice = text(sex.querySelector('[data-testid=sex-update]'))
    expect(notice).toMatch(/^Nueva versión disponible ?Nueva: field-sex-C-20261111, 172 MB\. ?Instalada: 43 MB, sin versión registrada\./)
    // The cost, as the model's manifest states it (measured on a laptop; nothing claimed for phones).
    expect(text(sex.querySelector('[data-testid=update-note]'))).toBe(NOTE)
    const btn = sex.querySelector<HTMLButtonElement>('[data-testid=sex-refresh]')!
    expect(text(btn)).toBe('Actualizar · 172 MB')
    expect(btn.disabled).toBe(false)
    btn.click()
    expect(asked).toBe(1)
    // The installed one stays usable: still on the phone, removable; no first-download button.
    expect(text(sex)).toContain('43 MB en el teléfono')
    expect(sex.querySelector('[data-testid=sex-download]')).toBeNull()
  })

  it('model B recorded: both versions and sizes; in English too', () => {
    locale.value = 'en'
    const el = show({ sex: update() })
    expect(text(el.querySelector('[data-testid=sex-update]'))).toMatch(/^New version available ?New: field-sex-C-20261111, 172 MB\. ?Installed: B 2026-11-08, 43 MB\./)
    expect(text(el.querySelector('[data-testid=sex-refresh]'))).toBe('Update · 172 MB')
  })

  it('the same version: no update notice, no update button, the version shown', () => {
    locale.value = 'es'
    const el = show({ sex: sexRow() })
    expect(el.querySelector('[data-testid=sex-update]')).toBeNull()
    expect(el.querySelector('[data-testid=sex-refresh]')).toBeNull()
    expect(el.querySelector('[data-testid=sex-note]')).toBeNull()
    expect(text(el.querySelector('[data-testid=sex-version]'))).toBe('Versión B 2026-11-08')
  })

  it('nothing on the phone: the download button, no note (it compares with a version the phone never had)', () => {
    locale.value = 'es'
    const el = show({ sex: sexRow({ ready: false, note: NOTE, versions: { from: null, to: C, fromMb: null, toMb: 172 } }), models: [modelRow({ ready: false, note: NOTE })] })
    expect(text(el.querySelector('[data-testid=sex-download]'))).toBe('Descargar · 172 MB')
    expect(text(el)).not.toContain(NOTE)
    expect(el.querySelector('[data-testid=sex-update]')).toBeNull()
  })

  it('during the update: the progress, then «Comprobando la descarga…»; the installed version stays in use', () => {
    locale.value = 'es'
    let el = show({ sex: update({ progress: { done: 50e6, total: 171606829 } }) })
    expect(text(el.querySelector('[data-testid=dl-sex] [role=status]'))).toMatch(/^50 de 172 MB ?Mientras tanto se sigue usando la versión instalada\.$/)
    expect(el.querySelector('[data-testid=sex-refresh]')).toBeNull()
    el = show({ sex: update({ progress: { done: 171606829, total: 171606829 } }) })
    expect(text(el.querySelector('[data-testid=dl-sex] [role=status]'))).toContain('Comprobando la descarga…')
  })

  it('the identifier: the same notice and button; a content hash is not shown as a version', () => {
    locale.value = 'es'
    const el = show({ models: [modelRow({ outdated: true, downloadMb: 25, versions: { from: 'sha256-aaaaaaaaaaaa', to: 'sha256-bbbbbbbbbbbb' } }), modelRow({ kind: 'full', label: 'Más preciso', ready: false, recommended: false })] })
    expect(text(el.querySelector('[data-testid=dl-update-small]'))).toBe('Nueva versión disponible')
    expect(text(el.querySelector('[data-testid=dl-refresh-small]'))).toBe('Actualizar · 25 MB')
    expect(el.querySelector('[data-testid=dl-update-full]')).toBeNull()
    expect(el.querySelector('[data-testid=dl-get-full]')).not.toBeNull()
    // Still usable meanwhile: «Usar este» is not offered for the one in use, removal is.
    expect(text(el.querySelector('[data-testid=dl-model-small]'))).toContain('Borrar del teléfono')
  })

  it('«Rápido y liviano», a newer named version listed: the versions with their sizes, its note, «Actualizar · 23 MB»', () => {
    locale.value = 'es'
    const NEW_NOTE = 'Una nota del manifiesto del modelo.'
    const el = show({ models: [modelRow({ outdated: true, downloadMb: 23, versions: { from: 'small-v1-test', to: 'small-v2-test', fromMb: 24, toMb: 23 }, note: NEW_NOTE })] })
    expect(text(el.querySelector('[data-testid=dl-update-small]'))).toMatch(/^Nueva versión disponible ?Nueva: small-v2-test, 23 MB\. ?Instalada: small-v1-test, 24 MB\. ?Una nota del manifiesto/)
    expect(text(el.querySelector('[data-testid=dl-refresh-small]'))).toBe('Actualizar · 23 MB')
    // After the update: its version, no notice.
    const after = show({ models: [modelRow({ versions: { from: 'small-v2-test', to: 'small-v2-test' } })] })
    expect(after.querySelector('[data-testid=dl-update-small]')).toBeNull()
    expect(text(after.querySelector('[data-testid=dl-version-small]'))).toBe('Versión small-v2-test')
  })

  it('the progress: «Preparando la descarga…» before the first byte, then 0 of the total (not «1 de 1 MB»)', () => {
    locale.value = 'es'
    let el = show({ sex: update({ progress: { done: 0, total: 1 } }) })
    expect(text(el.querySelector('[data-testid=dl-sex] [role=status]'))).toMatch(/^Preparando la descarga…/)
    el = show({ sex: update({ progress: { done: 0, total: 171606829 } }) })
    expect(text(el.querySelector('[data-testid=dl-sex] [role=status]'))).toMatch(/^0 de 172 MB/)
    el = show({ models: [modelRow({ outdated: true })], download: { kind: 'small', done: 0, total: 1 } })
    expect(text(el.querySelector('[data-testid=dl-model-small] [role=status]'))).toMatch(/^Preparando la descarga…/)
  })
})

// 12 Nov 2026: a download cut by a network switch waits and goes on by itself; after a reload it offers «Continuar».
describe('Descargas: a download that outlasts a dropped connection', () => {
  it('no signal: «Esperando conexión…» with the bar where it was, no error, no button', () => {
    locale.value = 'es'
    const el = show({ models: [modelRow({ ready: true, outdated: true })], download: { kind: 'small', done: 12e6, total: 23e6, waiting: true } })
    const row = el.querySelector('[data-testid=dl-model-small]')!
    expect(text(row.querySelector('[data-testid=dl-waiting-small]'))).toMatch(/^Esperando conexión…/)
    expect(text(row.querySelector('[role=status]'))).toContain('12 de 23 MB')
    expect(row.querySelector('[role=progressbar]')!.getAttribute('aria-valuenow')).toBe('52')
    expect(el.querySelector('[role=alert]')).toBeNull()
    expect(row.querySelector('button.btn-primary')).toBeNull()
    locale.value = 'en'
    const en = show({ models: [modelRow()], download: { kind: 'small', done: 12e6, total: 23e6, waiting: true } })
    expect(text(en.querySelector('[data-testid=dl-waiting-small]'))).toMatch(/^Waiting for connection…/)
  })

  it('after a reload: «Continuar» with what is left, and how much is on the phone', () => {
    locale.value = 'es'
    let asked = 0
    const el = show({ models: [modelRow({ ready: false, partial: { done: 9e6, total: 23e6 } })] }, { onDownload: () => asked++ })
    const row = el.querySelector('[data-testid=dl-model-small]')!
    expect(text(row.querySelector('[data-testid=dl-partial-small]'))).toBe('Descarga a medias: 9 de 23 MB en el teléfono.')
    const go = row.querySelector<HTMLButtonElement>('[data-testid=dl-continue-small]')!
    expect(text(go)).toBe('Continuar · 14 MB')
    expect(row.querySelector('[data-testid=dl-get-small]')).toBeNull()
    go.click()
    expect(asked).toBe(1)
    // The sex model too.
    const sex = show({ sex: sexRow({ ready: false, partial: { done: 20e6, total: 43e6 } }) }).querySelector('[data-testid=dl-sex]')!
    expect(text(sex.querySelector('[data-testid=sex-continue]'))).toBe('Continuar · 23 MB')
    expect(text(show({ sex: sexRow({ progress: { done: 20e6, total: 43e6, waiting: true } }) }).querySelector('[data-testid=sex-waiting]'))).toMatch(/^Esperando conexión…/)
  })
})
