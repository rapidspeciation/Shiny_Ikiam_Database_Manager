// Runs on the audio thread: turns the microphone (usually 48 kHz float) into the
// 16 kHz 16-bit PCM the voice model takes, in 40 ms pieces, with their loudness
// for the level meter. Kept as plain JS: it is loaded with audioWorklet.addModule.

const RATE = 16000
const PIECE = 640 // 40 ms

class MicCapture extends AudioWorkletProcessor {
  constructor() {
    super()
    this.step = sampleRate / RATE
    this.position = 0
    this.out = new Int16Array(PIECE)
    this.filled = 0
    this.energy = 0
  }

  process(inputs) {
    const input = inputs[0] && inputs[0][0]
    if (!input) return true
    for (; this.position < input.length; this.position += this.step) {
      const i = Math.floor(this.position)
      const fraction = this.position - i
      const a = input[i]
      const b = i + 1 < input.length ? input[i + 1] : a
      const sample = Math.max(-1, Math.min(1, a + (b - a) * fraction))
      this.energy += sample * sample
      this.out[this.filled++] = sample < 0 ? sample * 0x8000 : sample * 0x7fff
      if (this.filled === PIECE) {
        this.port.postMessage({ pcm: this.out.buffer, level: Math.sqrt(this.energy / PIECE) }, [this.out.buffer])
        this.out = new Int16Array(PIECE)
        this.filled = 0
        this.energy = 0
      }
    }
    this.position -= input.length
    return true
  }
}

registerProcessor('mic-capture', MicCapture)
