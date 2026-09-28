import { describe, expect, it } from 'vitest'
import { reactive } from 'vue'
import { Transcript, fromBase64, toBase64, type Line } from '../liveCall'

describe('call transcript', () => {
  it('joins pieces into lines and closes a line when the other side speaks', () => {
    const lines = reactive<Line[]>([])
    const t = new Transcript(lines)
    t.hear('La 5VB')
    t.hear(' es hembra')
    t.say('Propuse 5VB')
    t.say(' hembra. ¿Lo guardo?')
    t.finish('assistant')
    t.hear('Sí, guárdalo')
    expect(lines.map(l => [l.role, l.text, l.final])).toEqual([
      ['user', 'La 5VB es hembra', true],
      ['assistant', 'Propuse 5VB hembra. ¿Lo guardo?', true],
      ['user', 'Sí, guárdalo', false],
    ])
    expect(t.lastHeard()).toBe('Sí, guárdalo')
  })

  it('marks an interrupted answer and keeps typed text apart from speech', () => {
    const lines = reactive<Line[]>([])
    const t = new Transcript(lines)
    t.say('Encontré doce filas y')
    t.finish('assistant', ' …')
    t.hear('Espera')
    t.typed('CAM078038')
    expect(lines.map(l => l.text)).toEqual(['Encontré doce filas y …', 'Espera', 'CAM078038'])
    expect(t.lastHeard()).toBe('CAM078038')
  })

  it('hands each closed line over once, and again only if saving failed', () => {
    const lines = reactive<Line[]>([])
    const t = new Transcript(lines)
    t.hear('Busca la H79')
    t.say('Un momento')
    const first = t.take()
    expect(first.map(l => l.text)).toEqual(['Busca la H79'])
    expect(t.take()).toEqual([])
    t.giveBack(first)
    expect(t.take().map(l => l.text)).toEqual(['Busca la H79'])
  })
})

describe('audio encoding', () => {
  it('round-trips PCM through base64', () => {
    const pcm = new Int16Array([0, 1, -1, 32767, -32768])
    const back = fromBase64(toBase64(pcm.buffer))
    expect([...new Int16Array(back.buffer)]).toEqual([...pcm])
  })
})
