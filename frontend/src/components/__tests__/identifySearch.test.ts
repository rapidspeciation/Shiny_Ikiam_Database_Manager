import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { createApp, nextTick, type Component } from 'vue'
import { createMemoryHistory, createRouter, type Router } from 'vue-router'
import { t, tn } from '../../lib/i18n'
import type { PackIndex, PackTaxon } from '../../lib/photoPacks'

// Identificar's search and Descargas (review r1): a touch that starts a scroll does not pick a suggestion (fable-F4),
// the phone's Back closes the full-screen photo and returns from a species to its genus (fable-F5), and Update stays
// reachable when the server's list is newer, whatever option is being looked at (fable-F6).

const tx = (name: string, file: string, bytes = 100): PackTaxon => {
  const p = name.split(' ')
  return { name, rank: p.length > 2 ? 'subspecies' : 'species', species: p.slice(0, 2).join(' '), genus: p[0], family: 'Nymphalidae', group: 'ithomiini', countries: ['EC'], photos: [{ file, attribution: '(c) A', license: 'CC-BY', taxonUrl: null, source: null, bytes }] }
}
const index = (version: string): PackIndex => ({
  schema: 'ithomiini-photo-packs/2',
  version,
  built: '2026-11-11',
  taxa: [tx('Mechanitis polymnia', '1.webp'), tx('Mechanitis lysimnia', '2.webp'), tx('Mechanitis polymnia casabranca', '3.webp'), tx('Melinaea menophilus', '4.webp')],
})
let served = index('v1')

function fakeCaches() {
  const store = new Map<string, Response>()
  const cache = {
    match: async (u: string) => store.get(String(u))?.clone(),
    put: async (u: string, r: Response) => void store.set(String(u), r),
    keys: async () => [...store.keys()].map(url => ({ url })),
    delete: async (r: string | { url: string }) => store.delete(typeof r === 'string' ? r : r.url),
  }
  vi.stubGlobal('caches', { open: async () => cache })
}
let unmount = () => {}
beforeEach(() => {
  vi.resetModules()
  localStorage.clear()
  served = index('v1')
  fakeCaches()
  vi.stubGlobal(
    'fetch',
    vi.fn(async (u: string) =>
      String(u).endsWith('/packs/index.json')
        ? new Response(JSON.stringify(served))
        : new Response(new Uint8Array(100), { headers: { 'content-type': 'image/webp' } }),
    ),
  )
  vi.stubGlobal('confirm', () => true)
})
afterEach(() => {
  unmount()
  vi.unstubAllGlobals()
  document.body.innerHTML = ''
})

async function mount(component: Component, props: Record<string, unknown>, path = '/identificar?vista=buscar'): Promise<{ host: HTMLElement; router: Router }> {
  const router = createRouter({ history: createMemoryHistory(), routes: [{ path: '/identificar', component: { template: '<div />' } }] })
  await router.push(path)
  const host = document.createElement('div')
  document.body.append(host)
  const app = createApp(component, props)
  app.use(router)
  app.config.globalProperties.$t = t
  app.config.globalProperties.$tn = tn
  app.mount(host)
  unmount = () => app.unmount()
  await settle()
  return { host, router }
}
const settle = async () => {
  for (let i = 0; i < 8; i++) {
    await new Promise(r => setTimeout(r, 0))
    await nextTick()
  }
}
async function type(host: HTMLElement, text: string) {
  const input = host.querySelector<HTMLInputElement>('#taxon-q')!
  input.value = text
  input.dispatchEvent(new Event('input'))
  input.dispatchEvent(new Event('focus'))
  await settle()
}
const heading = (host: HTMLElement) => host.querySelector('h2')?.textContent?.trim() ?? null

describe('taxon search', () => {
  it('fable-F4: a touch that starts a scroll does not pick; a tap does', async () => {
    const { default: TaxonSearch } = await import('../identify/TaxonSearch.vue')
    const { host } = await mount(TaxonSearch, { signedIn: false, online: true })
    await type(host, 'mech')
    const options = host.querySelectorAll('[role=option]')
    expect(options.length).toBeGreaterThan(1)
    expect(options.length).toBeLessThanOrEqual(5)
    options[1].dispatchEvent(new Event('pointerdown', { bubbles: true }))
    options[1].dispatchEvent(new Event('touchstart', { bubbles: true }))
    await settle()
    expect(heading(host)).toBeNull()
    ;(options[1] as HTMLElement).click()
    await settle()
    expect(heading(host)).toBe(options[1].querySelector('i')!.textContent)
  })

  it('fable-F5: Back closes the full-screen photo, and from a species returns to its genus', async () => {
    const { default: TaxonSearch } = await import('../identify/TaxonSearch.vue')
    const { host, router } = await mount(TaxonSearch, { signedIn: false, online: true })
    await type(host, 'Mechanitis')
    host.querySelector<HTMLElement>('[role=option]')!.click()
    await settle()
    expect(heading(host)).toBe('Mechanitis')
    // A species from the genus page.
    await type(host, 'Mechanitis polymnia')
    host.querySelector<HTMLElement>('[role=option]')!.click()
    await settle()
    expect(heading(host)).toBe('Mechanitis polymnia')
    host.querySelector<HTMLElement>('[data-testid=lead-photo]')!.click()
    await settle()
    expect(document.querySelector('[data-testid=photo-viewer]')).not.toBeNull()
    router.back()
    await settle()
    expect(document.querySelector('[data-testid=photo-viewer]')).toBeNull()
    expect(heading(host)).toBe('Mechanitis polymnia')
    router.back()
    await settle()
    expect(heading(host)).toBe('Mechanitis')
  })
})

describe('Descargas', () => {
  it('fable-F6: Update stays when the server’s list is newer, also while another option is chosen', async () => {
    const { installPack } = await import('../../lib/photoPacks')
    await installPack(index('v1'), { taxa: 'ithomiini', region: ['EC'], ssp: false }, () => {})
    served = index('v2')
    localStorage.setItem('identify:pack-scope', JSON.stringify({ taxa: 'all', region: 'neotropics', ssp: true }))
    const { default: PackPanel } = await import('../identify/PackPanel.vue')
    const { host } = await mount(PackPanel, { online: true }, '/identificar?vista=descargas')
    const update = host.querySelector<HTMLButtonElement>('[data-testid=pack-update]')
    expect(update).not.toBeNull()
    update!.click()
    await settle()
    const { installedPack } = await import('../../lib/photoPacks')
    const now = await installedPack()
    // Updated in its own scope (not the option being looked at), to the new list.
    expect(now?.installed).toMatchObject({ version: 'v2', scope: { taxa: 'ithomiini', region: ['EC'], ssp: false } })
    expect(host.querySelector('[data-testid=pack-update]')).toBeNull()
  })
})
