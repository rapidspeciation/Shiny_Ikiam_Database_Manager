/**
 * What a «Cámara» button opens. Phones and tablets open their camera app from
 * a file input with `capture`; desktop browsers ignore `capture` and show the
 * file picker, so there the app opens the computer's camera itself
 * (components/CameraCapture.vue). Without getUserMedia the file picker is all
 * there is, except on a page that is not secure (http), where the in-app camera
 * says why the camera cannot open.
 */
export type CameraMode = 'native' | 'app' | 'file'

export interface CameraEnv {
  /** The main pointer is a finger (phones, tablets). */
  coarsePointer: boolean
  /** The browser says it is a phone or tablet (user agent, iPadOS as a Mac with touch). */
  mobile: boolean
  getUserMedia: boolean
  secure: boolean
}

export function cameraModeFor(env: CameraEnv): CameraMode {
  if (env.coarsePointer || env.mobile) return 'native'
  if (env.getUserMedia) return 'app'
  return env.secure ? 'file' : 'app'
}

/** This browser's answers to cameraModeFor. */
export function cameraEnv(): CameraEnv {
  if (typeof window === 'undefined') return { coarsePointer: false, mobile: false, getUserMedia: false, secure: true }
  const nav = navigator as Navigator & { userAgentData?: { mobile?: boolean } }
  const ua = nav.userAgent || ''
  const iPadAsMac = /Macintosh/.test(ua) && (nav.maxTouchPoints ?? 0) > 1
  return {
    coarsePointer: !!window.matchMedia?.('(pointer: coarse)').matches,
    mobile: nav.userAgentData?.mobile === true || /Android|iPhone|iPad|iPod|Mobile/i.test(ua) || iPadAsMac,
    getUserMedia: typeof nav.mediaDevices?.getUserMedia === 'function',
    secure: window.isSecureContext !== false,
  }
}

export const cameraMode = () => cameraModeFor(cameraEnv())

/** Why the camera did not open, from getUserMedia's error. */
export type CameraProblem = 'denied' | 'none' | 'busy' | 'insecure' | 'other'

export function cameraProblem(error: unknown, secure = true): CameraProblem {
  if (!secure) return 'insecure'
  const name = error && typeof error === 'object' && 'name' in error ? String((error as { name: unknown }).name) : ''
  if (name === 'NotAllowedError' || name === 'PermissionDeniedError' || name === 'SecurityError') return 'denied'
  if (name === 'NotFoundError' || name === 'DevicesNotFoundError' || name === 'OverconstrainedError') return 'none'
  if (name === 'NotReadableError' || name === 'TrackStartError' || name === 'AbortError') return 'busy'
  return 'other'
}

/** The video constraints: the chosen camera (or the one facing away), at the largest size it gives. */
export function videoConstraints(deviceId: string | null): MediaTrackConstraints {
  return {
    ...(deviceId ? { deviceId: { exact: deviceId } } : { facingMode: { ideal: 'environment' } }),
    width: { ideal: 4096 },
    height: { ideal: 3072 },
  }
}

/** The camera after `current` in the list (round), for the switch button. */
export function nextCamera(devices: { deviceId: string }[], current: string | null): string | null {
  if (!devices.length) return null
  const i = devices.findIndex(d => d.deviceId === current)
  return devices[(i + 1) % devices.length].deviceId
}

/** The camera chosen last on this computer. */
export const CAMERA_KEY = 'ithomiini:camera'
export function savedCamera(): string | null {
  try {
    return localStorage.getItem(CAMERA_KEY) || null
  } catch {
    return null
  }
}
export function saveCamera(deviceId: string | null) {
  try {
    if (deviceId) localStorage.setItem(CAMERA_KEY, deviceId)
    else localStorage.removeItem(CAMERA_KEY)
  } catch {
    /* private mode: not remembered */
  }
}

/** A name for a photo taken in the app: camera-2026-10-06-153012.jpg. */
export function photoName(now = new Date()): string {
  const p = (n: number) => String(n).padStart(2, '0')
  return `camera-${now.getFullYear()}-${p(now.getMonth() + 1)}-${p(now.getDate())}-${p(now.getHours())}${p(now.getMinutes())}${p(now.getSeconds())}.jpg`
}
