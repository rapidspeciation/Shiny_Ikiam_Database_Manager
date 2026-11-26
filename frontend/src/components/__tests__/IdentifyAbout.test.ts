import { readFileSync } from 'node:fs'
import { resolve } from 'node:path'
import { afterEach, describe, expect, it } from 'vitest'
import { createApp, h } from 'vue'
import { locale, t } from '../../lib/i18n'
import IdentifyAbout from '../identify/IdentifyAbout.vue'

let host: HTMLElement | null = null
const prevLocale = locale.value

function show(props: { team?: boolean; serverOpen?: boolean }) {
  host = document.createElement('div')
  document.body.append(host)
  const app = createApp({ render: () => h(IdentifyAbout, props) })
  app.config.globalProperties.$t = t
  app.mount(host)
  return host
}

afterEach(() => {
  host?.remove()
  host = null
  locale.value = prevLocale
})

describe('IdentifyAbout', () => {
  it('shows required Meta DINOv3 server and downloadable offline attribution and license link in ES and EN for visitors and team', () => {
    for (const lang of ['es', 'en'] as const) {
      locale.value = lang
      for (const props of [{ team: false, serverOpen: false }, { team: false, serverOpen: true }, { team: true, serverOpen: true }]) {
        const el = show(props)
        const content = el.textContent ?? ''
        expect(content).toContain('Built with DINOv3.')
        expect(content).toMatch(lang === 'es' ? /modelo descargable «Más preciso»/i : /downloadable “More accurate” model/i)
        expect(content).not.toMatch(lang === 'es' ? /no se descargan ni se redistribuyen/i : /not downloaded or redistributed/i)
        const localLink = el.querySelector<HTMLAnchorElement>('a[href="DINOv3-LICENSE.txt"]')
        expect(localLink).not.toBeNull()
        expect(localLink!.textContent).toBe(lang === 'es' ? 'Licencia de Meta DINOv3 (copia incluida)' : 'Meta DINOv3 License (included copy)')
        const link = el.querySelector<HTMLAnchorElement>('a[href="https://github.com/facebookresearch/dinov3/blob/main/LICENSE.md"]')
        expect(link).not.toBeNull()
        expect(link!.textContent).toBe(lang === 'es' ? 'Licencia de Meta DINOv3' : 'Meta DINOv3 License')
        host!.remove()
        host = null
      }
    }
  })

  it('ships byte-identical Meta DINOv3 License copies in licenses/ and frontend/public/identificar/', () => {
    const root = resolve(import.meta.dirname, '../../../..')
    const canonical = readFileSync(resolve(root, 'licenses/DINOv3-LICENSE.txt'), 'utf8')
    const publicCopy = readFileSync(resolve(root, 'frontend/public/identificar/DINOv3-LICENSE.txt'), 'utf8')
    expect(publicCopy).toBe(canonical)
    expect(canonical).toContain('DINOv3 License')
  })

  it('describes photo handling accurately by mode when visitor server identification is open vs offline-only', () => {
    for (const lang of ['es', 'en'] as const) {
      locale.value = lang
      // Signed-out visitor with serverOpen=true must NOT be told photos never go to a server
      const visitorServer = show({ team: false, serverOpen: true })
      const pServer = visitorServer.querySelector('p')!.textContent ?? ''
      expect(pServer).toMatch(lang === 'es' ? /En línea \(servidor\)/ : /Online \(server\)/)
      expect(pServer).not.toMatch(lang === 'es' ? /no se envían a ningún servidor/i : /not sent to any server/i)
      host!.remove()
      host = null

      // Strictly offline visitor (serverOpen=false) keeps on-device-only notice
      const visitorOffline = show({ team: false, serverOpen: false })
      const pOffline = visitorOffline.querySelector('p')!.textContent ?? ''
      expect(pOffline).toMatch(lang === 'es' ? /no se envían a ningún servidor/i : /not sent to any server/i)
      host!.remove()
      host = null
    }
  })
})
