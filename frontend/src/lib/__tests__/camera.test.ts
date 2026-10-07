import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import {
  CAMERA_KEY,
  cameraEnv,
  cameraMode,
  cameraModeFor,
  cameraProblem,
  nextCamera,
  photoName,
  saveCamera,
  savedCamera,
  videoConstraints,
  type CameraEnv,
} from '../camera'

const desktop: CameraEnv = { coarsePointer: false, mobile: false, getUserMedia: true, secure: true }

describe('what a «Cámara» button opens', () => {
  it('phones and tablets: their camera app (input with capture)', () => {
    expect(cameraModeFor({ ...desktop, coarsePointer: true })).toBe('native')
    expect(cameraModeFor({ ...desktop, mobile: true })).toBe('native')
    expect(cameraModeFor({ ...desktop, mobile: true, getUserMedia: false })).toBe('native')
    expect(cameraModeFor({ ...desktop, coarsePointer: true, secure: false, getUserMedia: false })).toBe('native')
  })
  it('a computer with getUserMedia: the camera inside the app', () => {
    expect(cameraModeFor(desktop)).toBe('app')
  })
  it('a computer without getUserMedia: the file picker', () => {
    expect(cameraModeFor({ ...desktop, getUserMedia: false })).toBe('file')
  })
  it('a page that is not secure: the app says why the camera does not open', () => {
    expect(cameraModeFor({ ...desktop, getUserMedia: false, secure: false })).toBe('app')
  })
})

describe('the browser read for it', () => {
  const nav = navigator as unknown as Record<string, unknown>
  const saved = { mediaDevices: Object.getOwnPropertyDescriptor(navigator, 'mediaDevices'), userAgent: navigator.userAgent }
  afterEach(() => {
    vi.unstubAllGlobals()
    if (saved.mediaDevices) Object.defineProperty(navigator, 'mediaDevices', saved.mediaDevices)
    else delete nav.mediaDevices
    Object.defineProperty(navigator, 'userAgent', { value: saved.userAgent, configurable: true })
  })
  const media = (coarse: boolean) => vi.fn((q: string) => ({ matches: coarse && q === '(pointer: coarse)' }) as MediaQueryList)

  it('a desktop Chrome with a camera API opens the app camera', () => {
    vi.stubGlobal('matchMedia', media(false))
    Object.defineProperty(navigator, 'userAgent', { value: 'Mozilla/5.0 (X11; Linux x86_64) Chrome/140.0', configurable: true })
    Object.defineProperty(navigator, 'mediaDevices', { value: { getUserMedia: () => Promise.resolve() }, configurable: true })
    expect(cameraEnv()).toMatchObject({ coarsePointer: false, mobile: false, getUserMedia: true })
    expect(cameraMode()).toBe('app')
  })
  it('an Android phone keeps its camera app', () => {
    vi.stubGlobal('matchMedia', media(true))
    Object.defineProperty(navigator, 'userAgent', { value: 'Mozilla/5.0 (Linux; Android 15; Pixel 9) Mobile Chrome/140.0', configurable: true })
    Object.defineProperty(navigator, 'mediaDevices', { value: { getUserMedia: () => Promise.resolve() }, configurable: true })
    expect(cameraMode()).toBe('native')
  })
  it('a desktop browser with no camera API opens the file picker', () => {
    vi.stubGlobal('matchMedia', media(false))
    Object.defineProperty(navigator, 'userAgent', { value: 'Mozilla/5.0 (X11; Linux x86_64) Firefox/140.0', configurable: true })
    Object.defineProperty(navigator, 'mediaDevices', { value: undefined, configurable: true })
    expect(cameraEnv().getUserMedia).toBe(false)
    expect(cameraMode()).toBe('file')
  })
})

describe('why the camera did not open', () => {
  const named = (name: string) => Object.assign(new Error(name), { name })
  it('reads getUserMedia errors', () => {
    expect(cameraProblem(named('NotAllowedError'))).toBe('denied')
    expect(cameraProblem(named('SecurityError'))).toBe('denied')
    expect(cameraProblem(named('NotFoundError'))).toBe('none')
    expect(cameraProblem(named('OverconstrainedError'))).toBe('none')
    expect(cameraProblem(named('NotReadableError'))).toBe('busy')
    expect(cameraProblem(named('TypeError'))).toBe('other')
    expect(cameraProblem('odd')).toBe('other')
    expect(cameraProblem(named('NotAllowedError'), false)).toBe('insecure')
  })
})

describe('cameras', () => {
  beforeEach(() => localStorage.clear())
  it('asks for the chosen camera, or the one facing away, at its largest size', () => {
    expect(videoConstraints('abc')).toEqual({ deviceId: { exact: 'abc' }, width: { ideal: 4096 }, height: { ideal: 3072 } })
    expect(videoConstraints(null)).toEqual({ facingMode: { ideal: 'environment' }, width: { ideal: 4096 }, height: { ideal: 3072 } })
  })
  it('switches round the list', () => {
    const list = [{ deviceId: 'a' }, { deviceId: 'b' }, { deviceId: 'c' }]
    expect(nextCamera(list, 'a')).toBe('b')
    expect(nextCamera(list, 'c')).toBe('a')
    expect(nextCamera(list, null)).toBe('a')
    expect(nextCamera(list, 'gone')).toBe('a')
    expect(nextCamera([], 'a')).toBeNull()
  })
  it('remembers the choice in this browser', () => {
    expect(savedCamera()).toBeNull()
    saveCamera('b')
    expect(localStorage.getItem(CAMERA_KEY)).toBe('b')
    expect(savedCamera()).toBe('b')
    saveCamera(null)
    expect(savedCamera()).toBeNull()
  })
  it('names a photo by when it was taken', () => {
    expect(photoName(new Date(2026, 9, 6, 15, 3, 9))).toBe('camera-2026-10-06-150309.jpg')
  })
})
