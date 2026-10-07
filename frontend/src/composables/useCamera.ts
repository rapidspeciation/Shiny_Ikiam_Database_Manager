import { onBeforeUnmount, ref, shallowRef } from 'vue'
import { cameraProblem, saveCamera, savedCamera, videoConstraints, type CameraProblem } from '../lib/camera'

/** The frame is kept nearly as the camera gives it; the upload makes it small (preparePhoto). */
const FRAME_QUALITY = 0.95

/**
 * The computer's camera, live: `start` opens the camera chosen last (or the one
 * facing away), at the largest size it gives; `use` switches to another and
 * remembers it; `frame` takes the video's current frame at full size as a JPEG.
 * The stream stops on `stop` and when the component goes away.
 */
export function useCamera() {
  const stream = shallowRef<MediaStream | null>(null)
  const devices = ref<MediaDeviceInfo[]>([])
  const deviceId = ref<string | null>(null)
  const starting = ref(false)
  const problem = ref<CameraProblem | null>(null)
  /** Bumped by every start and stop: a camera that opens after it was no longer wanted is closed at once. */
  let generation = 0

  function release() {
    stream.value?.getTracks().forEach(track => track.stop())
    stream.value = null
  }

  async function open(id: string | null): Promise<MediaStream> {
    return navigator.mediaDevices.getUserMedia({ video: videoConstraints(id), audio: false })
  }

  async function start(id: string | null = savedCamera()) {
    const mine = ++generation
    release()
    problem.value = null
    if (typeof window !== 'undefined' && window.isSecureContext === false) return void (problem.value = 'insecure')
    if (typeof navigator.mediaDevices?.getUserMedia !== 'function') return void (problem.value = 'none')
    starting.value = true
    try {
      let s: MediaStream
      try {
        s = await open(id)
      } catch (e) {
        // The camera remembered is gone (unplugged): any other one.
        if (!id || cameraProblem(e) !== 'none') throw e
        s = await open(null)
      }
      if (mine !== generation) return void s.getTracks().forEach(track => track.stop())
      const track = s.getVideoTracks()[0]
      await largest(track)
      track?.addEventListener('ended', () => {
        if (stream.value === s) ((stream.value = null), (problem.value = 'other'))
      })
      stream.value = s
      deviceId.value = track?.getSettings().deviceId ?? id
      // Names and the list of cameras are known once one is allowed.
      const all = await navigator.mediaDevices.enumerateDevices().catch(() => [] as MediaDeviceInfo[])
      if (mine === generation) devices.value = all.filter(d => d.kind === 'videoinput' && d.deviceId)
    } catch (e) {
      if (mine === generation) problem.value = cameraProblem(e, typeof window === 'undefined' || window.isSecureContext !== false)
    } finally {
      if (mine === generation) starting.value = false
    }
  }

  /** Another camera, remembered for next time. */
  async function use(id: string | null) {
    saveCamera(id)
    await start(id)
  }

  function stop() {
    generation++
    starting.value = false
    release()
  }

  onBeforeUnmount(stop)
  return { stream, devices, deviceId, starting, problem, start, use, stop }
}

/** Asks the track for the most pixels the camera has, when it opened smaller. */
async function largest(track: MediaStreamTrack | undefined) {
  const caps = track?.getCapabilities?.()
  const settings = track?.getSettings()
  const width = caps?.width?.max
  const height = caps?.height?.max
  if (!track || !width || !height || !settings?.width || settings.width >= width) return
  await track.applyConstraints({ width: { ideal: width }, height: { ideal: height } }).catch(() => {})
}

/** The video's current frame, at the size the camera gives, as a JPEG file. */
export function frameOf(video: HTMLVideoElement, name: string): Promise<File> {
  const canvas = document.createElement('canvas')
  canvas.width = video.videoWidth
  canvas.height = video.videoHeight
  const context = canvas.getContext('2d')
  if (!context || !canvas.width || !canvas.height) return Promise.reject(new Error('no frame'))
  context.drawImage(video, 0, 0, canvas.width, canvas.height)
  return new Promise((resolve, reject) =>
    canvas.toBlob(blob => (blob ? resolve(new File([blob], name, { type: 'image/jpeg', lastModified: Date.now() })) : reject(new Error('no frame'))), 'image/jpeg', FRAME_QUALITY),
  )
}
