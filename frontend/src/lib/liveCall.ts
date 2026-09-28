// A file, not an inlined data: URL, which the app's CSP (script-src 'self') would block.
import workletUrl from './micWorklet.js?url&no-inline'
import type { Proposal } from '../components/ProposalGrid.vue'

/**
 * A live voice call with the assistant (Gemini Live). The browser streams the
 * microphone straight to the model with a one-use token from the server; the
 * model's tool calls go to the server (api/ai/voice/tool), which runs them with
 * this person's permissions, and their whole result goes back to the model.
 *
 * The call goes on with the tab hidden or the screen off: it ends only when the
 * person hangs up. Dropped connections come back on their own, resuming the
 * same conversation.
 */

export type CallState = 'connecting' | 'listening' | 'speaking' | 'working' | 'reconnecting' | 'ended'
export interface Line {
  id: number
  role: 'user' | 'assistant'
  text: string
  final: boolean
  saved: boolean
}
export interface CallView {
  state: CallState
  /** What the assistant is doing (the tool running), shown under the state. */
  detail: string
  muted: boolean
  level: number
  lines: Line[]
  proposals: Proposal[]
  threadId: string | null
  title: string
  startedAt: number
  error: string
}
export interface VoiceSession {
  provider: string
  model: string
  token: string
  url: string
  setup: Record<string, unknown>
  threadId: string
  title: string
  proposals: Proposal[]
}
export interface ToolAnswer {
  result: unknown
  proposals: Proposal[]
  applied: string[]
}
type Said = { role: 'user' | 'assistant'; text: string }
export interface CallServer {
  /** Throws an error with `fatal: true` when retrying cannot help (not configured, no permission). */
  session(threadId: string | null): Promise<VoiceSession>
  tool(body: { threadId: string; name: string; args: unknown; heard: string; lines: Said[] }): Promise<ToolAnswer>
  transcript(body: { threadId: string; lines: Said[] }): Promise<unknown>
  applied?(proposalIds: string[]): void
}
export interface CallControls {
  hangUp(): void
  setMuted(muted: boolean): void
  sendText(text: string): void
}

/** Both sides' words as they arrive in pieces; a line closes when the other side speaks. */
export class Transcript {
  private next = 1
  constructor(private lines: Line[]) {}

  private open(role: Line['role']) {
    const last = this.lines[this.lines.length - 1]
    if (last && !last.final && last.role === role) return last
    this.finish()
    this.lines.push({ id: this.next++, role, text: '', final: false, saved: false })
    // Read back through the array so a reactive array tracks the change.
    return this.lines[this.lines.length - 1]
  }
  hear(text: string) {
    this.open('user').text += text
  }
  say(text: string) {
    this.open('assistant').text += text
  }
  typed(text: string) {
    this.finish()
    this.lines.push({ id: this.next++, role: 'user', text, final: true, saved: false })
  }
  finish(role?: Line['role'], suffix = '') {
    for (const line of this.lines)
      if (!line.final && (!role || line.role === role)) {
        if (line.text.trim()) line.text = line.text.trim() + suffix
        line.final = true
      }
  }
  /** What the person said last: the server checks it before saving anything. */
  lastHeard() {
    for (let i = this.lines.length - 1; i >= 0; i--) if (this.lines[i].role === 'user') return this.lines[i].text.trim()
    return ''
  }
  /** Closed lines not yet stored in the conversation; marked stored until giveBack says otherwise. */
  take(): (Said & { id: number })[] {
    const out = this.lines.filter(l => l.final && !l.saved && l.text.trim())
    for (const line of out) line.saved = true
    return out.map(l => ({ id: l.id, role: l.role, text: l.text.trim() }))
  }
  giveBack(taken: { id: number }[]) {
    const ids = new Set(taken.map(t => t.id))
    for (const line of this.lines) if (ids.has(line.id)) line.saved = false
  }
}

const TOOL_LABELS: Record<string, string> = {
  search_records: 'Buscando en la hoja…',
  find_records: 'Buscando en la hoja…',
  get_record: 'Buscando en la hoja…',
  describe_sheet: 'Revisando las columnas…',
  search_knowledge: 'Buscando en los protocolos…',
  run_report: 'Preparando el informe…',
  propose_changes: 'Preparando los cambios…',
  apply_proposal: 'Guardando en la hoja…',
}

export function toBase64(buffer: ArrayBuffer) {
  const bytes = new Uint8Array(buffer)
  let text = ''
  for (let i = 0; i < bytes.length; i += 0x8000) text += String.fromCharCode(...bytes.subarray(i, i + 0x8000))
  return btoa(text)
}
export function fromBase64(data: string) {
  const text = atob(data)
  const bytes = new Uint8Array(text.length)
  for (let i = 0; i < text.length; i++) bytes[i] = text.charCodeAt(i)
  return bytes
}

function micError(e: unknown) {
  const name = (e as { name?: string }).name
  if (name === 'NotAllowedError' || name === 'SecurityError')
    return 'Permite el micrófono en el navegador para hablar con el asistente.'
  if (name === 'NotFoundError') return 'No se encontró un micrófono.'
  return 'No se pudo abrir el micrófono.'
}

interface Message {
  setupComplete?: unknown
  sessionResumptionUpdate?: { newHandle?: string; resumable?: boolean }
  goAway?: unknown
  toolCall?: { functionCalls?: { id: string; name: string; args?: unknown }[] }
  toolCallCancellation?: { ids?: string[] }
  serverContent?: {
    modelTurn?: { parts?: { inlineData?: { data?: string; mimeType?: string } }[] }
    inputTranscription?: { text?: string }
    outputTranscription?: { text?: string }
    interrupted?: boolean
    turnComplete?: boolean
  }
}

/** Call from a click: the audio context must be created within the gesture. */
export function startCall(view: CallView, server: CallServer): CallControls {
  const audio = new AudioContext()
  void audio.resume()
  const transcript = new Transcript(view.lines)
  let stream: MediaStream | null = null
  let mic: AudioWorkletNode | null = null
  let socket: WebSocket | null = null
  let ready = false
  let handle: string | null = null
  let attempts = 0
  let ended = false
  let retryTimer = 0
  let saveTimer = 0
  let playhead = 0
  let toolsRunning = 0
  let moveWhenIdle = false
  let connectedOnce = false
  let waiting = false
  let lock: WakeLockSentinel | null = null
  const playing = new Set<AudioBufferSourceNode>()
  const cancelled = new Set<string>()
  Object.assign(view, { state: 'connecting', detail: '', muted: false, level: 0, error: '', startedAt: Date.now() })

  const send = (message: unknown) => {
    if (socket?.readyState === WebSocket.OPEN) socket.send(JSON.stringify(message))
  }

  async function openMic() {
    stream = await navigator.mediaDevices.getUserMedia({
      audio: { echoCancellation: true, noiseSuppression: true, autoGainControl: true, channelCount: 1 },
    })
    // Hung up while the browser was asking for the microphone.
    if (ended) return stream.getTracks().forEach(track => track.stop())
    await audio.audioWorklet.addModule(workletUrl)
    mic = new AudioWorkletNode(audio, 'mic-capture')
    mic.port.onmessage = ({ data }: MessageEvent<{ pcm: ArrayBuffer; level: number }>) => {
      view.level = view.muted ? 0 : Math.min(1, data.level * 5)
      if (!view.muted && ready) send({ realtimeInput: { audio: { data: toBase64(data.pcm), mimeType: 'audio/pcm;rate=16000' } } })
    }
    audio.createMediaStreamSource(stream).connect(mic)
    // The worklet writes nothing to its output; connecting it keeps it running.
    mic.connect(audio.destination)
    // Another app taking the microphone (a phone call) ends the track: take it back when possible.
    stream.getAudioTracks()[0]?.addEventListener('ended', () => {
      if (ended) return
      mic?.disconnect()
      openMic().catch(e => (view.error = micError(e)))
    })
  }

  async function connect() {
    waiting = false
    if (ended) return
    let session: VoiceSession
    try {
      session = await server.session(view.threadId)
    } catch (e) {
      const err = e as Error & { fatal?: boolean }
      return err.fatal ? finish(err.message) : retry(err.message)
    }
    if (ended) return
    Object.assign(view, { threadId: session.threadId, title: session.title, proposals: session.proposals })
    ready = false
    const resuming = handle
    const ws = new WebSocket(`${session.url}?access_token=${encodeURIComponent(session.token)}`)
    ws.binaryType = 'arraybuffer'
    socket = ws
    ws.onopen = () =>
      ws.send(JSON.stringify({ setup: { ...session.setup, sessionResumption: resuming ? { handle: resuming } : {} } }))
    ws.onmessage = event => {
      if (ws !== socket) return
      try {
        receive(JSON.parse(typeof event.data === 'string' ? event.data : new TextDecoder().decode(event.data)))
      } catch (e) {
        console.error('Voice message', e)
      }
    }
    ws.onclose = event => {
      if (ws !== socket || ended) return
      socket = null
      const wasReady = ready
      ready = false
      // A resumption handle that no longer works must not block the call: start afresh.
      if (!wasReady && resuming) handle = null
      retry(event.reason)
    }
  }

  function retry(reason = '') {
    if (ended) return
    stopPlayback()
    clearTimeout(retryTimer)
    waiting = true
    Object.assign(view, {
      state: connectedOnce ? 'reconnecting' : 'connecting',
      detail: navigator.onLine ? '' : 'Sin internet; la llamada vuelve sola al reconectar.',
    })
    // Offline waits for the 'online' event without spending attempts.
    if (!navigator.onLine) return
    // A call that never connected (a wrong key, the model down) stops sooner than one that dropped.
    if (++attempts > (connectedOnce ? 8 : 2))
      return finish(
        `${connectedOnce ? 'Se perdió la conexión con el asistente' : 'No se pudo conectar con el asistente'}${reason ? `: ${reason}` : '.'}`,
      )
    retryTimer = window.setTimeout(connect, Math.min(15000, 500 * 2 ** attempts))
  }
  function onOnline() {
    if (ended || !waiting) return
    clearTimeout(retryTimer)
    void connect()
  }
  /** The server announced this connection ends soon: continue on a new one with the same conversation. */
  function move() {
    moveWhenIdle = false
    const old = socket
    socket = null
    ready = false
    old?.close(1000)
    view.state = 'reconnecting'
    void connect()
  }

  function receive(m: Message) {
    if (m.setupComplete) {
      ready = true
      connectedOnce = true
      attempts = 0
      Object.assign(view, { state: playing.size ? 'speaking' : 'listening', detail: '', error: '' })
      return
    }
    if (m.sessionResumptionUpdate?.resumable && m.sessionResumptionUpdate.newHandle) handle = m.sessionResumptionUpdate.newHandle
    if (m.goAway) {
      if (toolsRunning) moveWhenIdle = true
      else move()
      return
    }
    if (m.toolCall) {
      void runTools(m.toolCall.functionCalls ?? [])
      return
    }
    for (const id of m.toolCallCancellation?.ids ?? []) cancelled.add(id)
    const content = m.serverContent
    if (!content) return
    if (content.inputTranscription?.text) transcript.hear(content.inputTranscription.text)
    if (content.outputTranscription?.text) transcript.say(content.outputTranscription.text)
    for (const part of content.modelTurn?.parts ?? []) {
      const data = part.inlineData?.data
      if (data && part.inlineData?.mimeType?.startsWith('audio/pcm'))
        play(data, Number(/rate=(\d+)/.exec(part.inlineData.mimeType)?.[1] ?? 24000))
    }
    if (content.interrupted) {
      // The person spoke over the assistant: stop talking at once.
      stopPlayback()
      transcript.finish('assistant', ' …')
      if (view.state === 'speaking') view.state = 'listening'
    }
    if (content.turnComplete) {
      transcript.finish('assistant')
      scheduleSave()
      if (!playing.size && view.state === 'speaking') view.state = 'listening'
    }
  }

  function play(data: string, rate: number) {
    const bytes = fromBase64(data)
    const samples = new Int16Array(bytes.buffer, 0, bytes.length >> 1)
    if (!samples.length) return
    const buffer = audio.createBuffer(1, samples.length, rate)
    const channel = buffer.getChannelData(0)
    for (let i = 0; i < samples.length; i++) channel[i] = samples[i] / 32768
    const node = audio.createBufferSource()
    node.buffer = buffer
    node.connect(audio.destination)
    const at = Math.max(audio.currentTime + 0.03, playhead)
    node.start(at)
    playhead = at + buffer.duration
    playing.add(node)
    if (view.state !== 'working') view.state = 'speaking'
    node.onended = () => {
      playing.delete(node)
      if (!playing.size && view.state === 'speaking') view.state = 'listening'
    }
  }
  function stopPlayback() {
    for (const node of playing) {
      node.onended = null
      try {
        node.stop()
      } catch {
        /* Not started yet. */
      }
    }
    playing.clear()
    playhead = 0
  }

  async function runTools(calls: { id: string; name: string; args?: unknown }[]) {
    toolsRunning++
    transcript.finish('user')
    const responses: { id: string; name: string; response: unknown }[] = []
    for (const call of calls) {
      Object.assign(view, { state: 'working', detail: TOOL_LABELS[call.name] ?? 'Consultando…' })
      const lines = transcript.take()
      let result: unknown
      try {
        const answer = await server.tool({
          threadId: view.threadId!,
          name: call.name,
          args: call.args ?? {},
          heard: transcript.lastHeard(),
          lines,
        })
        result = answer.result
        view.proposals = answer.proposals
        if (answer.applied.length) server.applied?.(answer.applied)
      } catch (e) {
        transcript.giveBack(lines)
        result = { error: `The app could not run ${call.name}: ${(e as Error).message}` }
      }
      // Sent whole: the model needs every row it asked for.
      if (!cancelled.has(call.id))
        responses.push({
          id: call.id,
          name: call.name,
          response: result && typeof result === 'object' && !Array.isArray(result) ? result : { output: result },
        })
    }
    toolsRunning--
    if (responses.length) send({ toolResponse: { functionResponses: responses } })
    if (view.state === 'working') Object.assign(view, { state: 'listening', detail: '' })
    if (moveWhenIdle && !toolsRunning) move()
  }

  function scheduleSave() {
    clearTimeout(saveTimer)
    saveTimer = window.setTimeout(save, 1500)
  }
  async function save() {
    const lines = transcript.take()
    if (!lines.length || !view.threadId) return
    try {
      await server.transcript({ threadId: view.threadId, lines })
    } catch {
      transcript.giveBack(lines)
    }
  }

  async function keepAwake() {
    try {
      lock = (await navigator.wakeLock?.request('screen')) ?? null
    } catch {
      /* Not supported or refused: the call still works, the screen may dim. */
    }
  }
  // Coming back to the tab only restores things; hiding it never ends the call.
  function onVisible() {
    if (ended || document.visibilityState !== 'visible') return
    if (!lock || lock.released) void keepAwake()
    if (audio.state === 'suspended') void audio.resume()
  }

  function finish(reason = '') {
    if (ended) return
    ended = true
    clearTimeout(retryTimer)
    clearTimeout(saveTimer)
    transcript.finish()
    void save()
    const ws = socket
    socket = null
    ws?.close(1000)
    stopPlayback()
    stream?.getTracks().forEach(track => track.stop())
    mic?.disconnect()
    void audio.close()
    void lock?.release().catch(() => {})
    lock = null
    window.removeEventListener('online', onOnline)
    document.removeEventListener('visibilitychange', onVisible)
    Object.assign(view, { state: 'ended', detail: '', level: 0, error: reason })
  }

  void (async () => {
    try {
      await openMic()
    } catch (e) {
      return finish(micError(e))
    }
    window.addEventListener('online', onOnline)
    document.addEventListener('visibilitychange', onVisible)
    void keepAwake()
    await connect()
  })()

  return {
    hangUp: () => finish(),
    setMuted(muted) {
      view.muted = muted
      view.level = 0
      // Tells the model's turn detection the person stopped talking.
      if (muted) send({ realtimeInput: { audioStreamEnd: true } })
    },
    sendText(text) {
      transcript.typed(text)
      send({ realtimeInput: { text } })
    },
  }
}
