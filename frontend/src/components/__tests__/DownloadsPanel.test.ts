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
function show(props: { sex?: SexRow | null; models?: ModelRow[]; download?: { kind: 'small' | 'full'; done: number; total: number } | null }, events: Record<string, () => void> = {}) {
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
const sexRow = (o: Partial<SexRow> = {}): SexRow => ({ genera: 13, ready: true, outdated: false, downloadMb: 164, takesMb: 41, progress: null, error: null, versions: { from: 'B 2026-11-08', to: 'B 2026-11-08' }, ...o })
const modelRow = (o: Partial<ModelRow> = {}): ModelRow => ({ kind: 'small', label: 'Rápido y liviano', ready: true, outdated: false, downloadMb: 25, takesMb: 65, recommended: true, ...o })

describe('Descargas: model versions', () => {
  it('the phone has no stored version, the server lists C: «Nueva versión disponible» with the new size and «Actualizar»', async () => {
    locale.value = 'es'
    let asked = 0
    const el = show({ sex: sexRow({ outdated: true, versions: { from: null, to: 'C 2026-11-20' } }) }, { onSexDownload: () => asked++ })
    const sex = el.querySelector('[data-testid=dl-sex]')!
    expect(text(sex.querySelector('[data-testid=sex-update]'))).toMatch(/^Nueva versión disponible ?Nueva: C 2026-11-20\.$/)
    const btn = sex.querySelector<HTMLButtonElement>('[data-testid=sex-refresh]')!
    expect(text(btn)).toBe('Actualizar · 164 MB')
    expect(btn.disabled).toBe(false)
    btn.click()
    expect(asked).toBe(1)
    // The installed one stays usable: still on the phone, removable; no first-download button.
    expect(text(sex)).toContain('41 MB en el teléfono')
    expect(sex.querySelector('[data-testid=sex-download]')).toBeNull()
  })

  it('an older recorded version: both versions shown; in English too', () => {
    locale.value = 'en'
    const el = show({ sex: sexRow({ outdated: true, versions: { from: 'B 2026-11-08', to: 'C 2026-11-20' } }) })
    expect(text(el.querySelector('[data-testid=sex-update]'))).toMatch(/^New version available ?Installed: B 2026-11-08\. New: C 2026-11-20\.$/)
    expect(text(el.querySelector('[data-testid=sex-refresh]'))).toBe('Update · 164 MB')
  })

  it('the same version: no update notice, no update button, the version shown', () => {
    locale.value = 'es'
    const el = show({ sex: sexRow() })
    expect(el.querySelector('[data-testid=sex-update]')).toBeNull()
    expect(el.querySelector('[data-testid=sex-refresh]')).toBeNull()
    expect(text(el.querySelector('[data-testid=sex-version]'))).toBe('Versión B 2026-11-08')
  })

  it('during the update: the progress, then «Comprobando la descarga…»; the installed version stays in use', () => {
    locale.value = 'es'
    let el = show({ sex: sexRow({ outdated: true, progress: { done: 50 * 1048576, total: 164 * 1048576 } }) })
    expect(text(el.querySelector('[data-testid=dl-sex] [role=status]'))).toMatch(/^50 de 164 MB ?Mientras tanto se sigue usando la versión instalada\.$/)
    expect(el.querySelector('[data-testid=sex-refresh]')).toBeNull()
    el = show({ sex: sexRow({ outdated: true, progress: { done: 164 * 1048576, total: 164 * 1048576 } }) })
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
})
