import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { createApp, nextTick } from 'vue'
import CameraCapture from '../CameraCapture.vue'
import { CAMERA_KEY } from '../../lib/camera'
import { t } from '../../lib/i18n'

const saved = Object.getOwnPropertyDescriptor(navigator, 'mediaDevices')
let unmount = () => {}
beforeEach(() => {
  localStorage.clear()
  vi.spyOn(HTMLMediaElement.prototype, 'play').mockResolvedValue(undefined)
})
afterEach(() => {
  unmount()
  document.body.innerHTML = ''
  vi.restoreAllMocks()
  if (saved) Object.defineProperty(navigator, 'mediaDevices', saved)
  else delete (navigator as unknown as Record<string, unknown>).mediaDevices
})

function fakeStream(deviceId: string) {
  const track = { stop: vi.fn(), addEventListener: vi.fn(), getSettings: () => ({ deviceId, width: 1280 }), getCapabilities: () => ({ width: { max: 1280 }, height: { max: 720 } }) }
  const stream = Object.assign(Object.create(MediaStream.prototype), { getTracks: () => [track], getVideoTracks: () => [track] }) as MediaStream
  return { track, stream }
}
function cameras(getUserMedia: (c: MediaStreamConstraints) => Promise<MediaStream>, ids = ['front', 'back']) {
  const media = {
    getUserMedia: vi.fn(getUserMedia),
    enumerateDevices: vi.fn(async () => ids.map(deviceId => ({ deviceId, kind: 'videoinput', label: `Camera ${deviceId}` }))),
  }
  Object.defineProperty(navigator, 'mediaDevices', { value: media, configurable: true })
  return media
}
function mount() {
  const host = document.createElement('div')
  document.body.append(host)
  const events: Record<string, unknown[]> = { photo: [], file: [], close: [] }
  const app = createApp(CameraCapture, {
    onPhoto: (f: File) => events.photo.push(f),
    onFile: () => events.file.push(1),
    onClose: () => events.close.push(1),
  })
  app.config.globalProperties.$t = t
  app.mount(host)
  unmount = () => app.unmount()
  return { host, events, close: () => (app.unmount(), (unmount = () => {})) }
}
const settle = async () => {
  for (let i = 0; i < 5; i++) await new Promise(r => setTimeout(r, 0))
  await nextTick()
}
const button = (host: HTMLElement, text: string) => [...host.querySelectorAll('button')].find(b => b.textContent?.includes(text))!

describe('CameraCapture', () => {
  it('shows the live camera, offers a switch between cameras, remembers it, and stops the camera when it closes', async () => {
    const opened: ReturnType<typeof fakeStream>[] = []
    const media = cameras(async c => {
      const id = (c.video as MediaTrackConstraints).deviceId as { exact: string } | undefined
      const s = fakeStream(id?.exact ?? 'front')
      opened.push(s)
      return s.stream
    })
    const { host, close } = mount()
    await settle()
    expect(media.getUserMedia).toHaveBeenCalledTimes(1)
    expect(host.querySelector('video')!.srcObject).toBe(opened[0].stream)
    expect(button(host, 'Tomar foto').disabled).toBe(false)
    button(host, 'Camera front').click()
    await settle()
    expect(opened[0].track.stop).toHaveBeenCalled()
    expect(localStorage.getItem(CAMERA_KEY)).toBe('back')
    expect((media.getUserMedia.mock.calls[1][0].video as MediaTrackConstraints).deviceId).toEqual({ exact: 'back' })
    close()
    expect(opened[1].track.stop).toHaveBeenCalled()
  })

  it('a camera remembered and since unplugged: opens another one', async () => {
    localStorage.setItem(CAMERA_KEY, 'gone')
    const media = cameras(async c => {
      if ((c.video as MediaTrackConstraints).deviceId) throw Object.assign(new Error('x'), { name: 'OverconstrainedError' })
      return fakeStream('front').stream
    })
    const { host } = mount()
    await settle()
    expect(media.getUserMedia).toHaveBeenCalledTimes(2)
    expect(host.querySelector('[role="alert"]')).toBeNull()
  })

  it('permission denied: says so, and a file can be chosen instead', async () => {
    cameras(async () => {
      throw Object.assign(new Error('denied'), { name: 'NotAllowedError' })
    })
    const { host, events } = mount()
    await settle()
    expect(host.querySelector('[role="alert"]')?.textContent).toContain('Sin permiso para usar la cámara')
    expect(button(host, 'Tomar foto').disabled).toBe(true)
    button(host, 'Elegir un archivo').click()
    expect(events.file).toHaveLength(1)
  })

  it('no camera, and a page that is not secure', async () => {
    cameras(async () => {
      throw Object.assign(new Error('none'), { name: 'NotFoundError' })
    })
    let view = mount()
    await settle()
    expect(view.host.querySelector('[role="alert"]')?.textContent).toContain('No se encontró ninguna cámara')
    view.close()
    document.body.innerHTML = ''
    vi.stubGlobal('isSecureContext', false)
    view = mount()
    await settle()
    expect(view.host.querySelector('[role="alert"]')?.textContent).toContain('dirección segura')
    vi.unstubAllGlobals()
  })

  it('Escape closes the camera only', async () => {
    cameras(async () => fakeStream('front').stream)
    const { events } = mount()
    await settle()
    const below = vi.fn()
    window.addEventListener('keydown', below)
    window.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape' }))
    window.removeEventListener('keydown', below)
    expect(events.close).toHaveLength(1)
    expect(below).not.toHaveBeenCalled()
  })
})
