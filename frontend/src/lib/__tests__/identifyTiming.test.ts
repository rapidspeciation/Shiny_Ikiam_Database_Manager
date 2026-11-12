import { describe, expect, it, vi } from 'vitest'
import { phoneTiming, seconds, timed, timingLine } from '../identifyTiming'
import en from '../../locales/en/identify'

// Identificar's timing line: both modes and the formatting. No model, no AI.

const fill = (s: string, v?: Record<string, string | number>) => s.replace(/\{(\w+)\}/g, (_, k) => String(v?.[k] ?? `{${k}}`))
const es = (s: string, v?: Record<string, string | number>) => fill(s, v)
const english = (s: string, v?: Record<string, string | number>) => fill((en as Record<string, string>)[s] ?? s, v)

describe('timingLine', () => {
  it('online: the time on the server, comma decimal in Spanish', () => {
    expect(timingLine({ source: 'online', ms: 2100 }, es, 'es')).toBe('Tiempo: 2,1 s en el servidor')
    expect(timingLine({ source: 'online', ms: 2100 }, english, 'en')).toBe('Time: 2.1 s on the server')
  })
  it('phone: the time on this phone; loads this photo needed are added and said', () => {
    expect(timingLine({ source: 'phone', ms: 1400, timing: { loadMs: 0 } }, es, 'es')).toBe('Tiempo: 1,4 s en este teléfono')
    expect(timingLine({ source: 'phone', ms: 1400, timing: { loadMs: 3150 } }, es, 'es')).toBe('Tiempo: 4,6 s en este teléfono (primera vez, cargando modelos)')
  })
  it('phone: the sex model when it ran, its load counted too', () => {
    expect(timingLine({ source: 'phone', ms: 1400, timing: { loadMs: 0, sexMs: 400 } }, es, 'es')).toBe('Tiempo: 1,4 s en este teléfono, sexo 0,4 s')
    expect(timingLine({ source: 'phone', ms: 1400, timing: { loadMs: 0, sexMs: 400, sexLoadMs: 900 } }, es, 'es')).toBe('Tiempo: 1,4 s en este teléfono, sexo 1,3 s (primera vez, cargando modelos)')
    expect(timingLine({ source: 'phone', ms: 1400, timing: { loadMs: 0, sexMs: 400 } }, english, 'en')).toBe('Time: 1.4 s on this phone, sex 0.4 s')
    expect(timingLine({ source: 'phone', ms: 1400, timing: { loadMs: 2000, sexMs: 400, sexLoadMs: 900 } }, english, 'en')).toBe('Time: 3.4 s on this phone, sex 1.3 s (first time, loading models)')
  })
  it('a result without timing (restored from before) still shows its time; no usable time shows nothing', () => {
    expect(timingLine({ source: 'phone', ms: 950 }, es, 'es')).toBe('Tiempo: 1,0 s en este teléfono')
    expect(timingLine({ source: 'online', ms: 0 }, es, 'es')).toBeNull()
    expect(timingLine({ source: 'phone', ms: null }, es, 'es')).toBeNull()
    expect(timingLine({ source: 'online', ms: Number.NaN }, es, 'es')).toBeNull()
  })
  it('no em dash in any variant, either language', () => {
    for (const [tr, lang] of [[es, 'es'], [english, 'en']] as const)
      for (const timing of [{ loadMs: 0 }, { loadMs: 10, sexMs: 5, sexLoadMs: 5 }])
        for (const source of ['online', 'phone'] as const) expect(timingLine({ source, ms: 1234, timing }, tr, lang)).not.toMatch(/—/)
  })
})

describe('seconds', () => {
  it('one decimal, rounded', () => {
    expect(seconds(1449, 'es')).toBe('1,4')
    expect(seconds(1450, 'es')).toBe('1,5')
    expect(seconds(12000, 'es')).toBe('12,0')
    expect(seconds(-5, 'en')).toBe('0.0')
  })
})

describe('timed and phoneTiming: the load phase measured once', () => {
  it('a phase that loads: its whole wall time, what loading added, and the flag', async () => {
    let gen = 4
    const now = vi.spyOn(performance, 'now')
    now.mockReturnValueOnce(1000).mockReturnValueOnce(5200)
    const phase = await timed(() => gen, async () => { gen += 3; return 'run' })
    now.mockRestore()
    expect(phase).toEqual({ value: 'run', ms: 4200, fresh: true })
    // the photo itself took 1,4 s: loading added 2,8 s, not counted twice
    expect(phoneTiming(phase, 1400)).toEqual({ loadMs: 2800, fresh: true })
    expect(timingLine({ source: 'phone', ms: 1400, timing: phoneTiming(phase, 1400) }, es, 'es')).toBe('Tiempo: 4,2 s en este teléfono (primera vez, cargando modelos)')
  })
  it('a phase with nothing to load: the photo time alone, no flag', async () => {
    const phase = await timed(() => 7, async () => 1)
    expect(phase.fresh).toBe(false)
    expect(phoneTiming(phase, 1400)).toEqual({ loadMs: 0, fresh: false })
    expect(timingLine({ source: 'phone', ms: 1400, timing: phoneTiming({ ms: 1500, fresh: false }, 1400) }, es, 'es')).toBe('Tiempo: 1,4 s en este teléfono')
  })
  it('regression: a reload that takes exactly as long as the first load still counts as a load', async () => {
    // the old rule compared rounded durations (small_load 3000 before and after: «no load»); the generation moves
    let gen = 0
    const load = async () => { gen++ }
    const first = await timed(() => gen, load)
    const again = await timed(() => gen, load)
    expect(first.fresh && again.fresh).toBe(true)
    expect(timingLine({ source: 'phone', ms: 900, timing: phoneTiming({ ms: 900, fresh: true }, 900) }, es, 'es')).toBe('Tiempo: 0,9 s en este teléfono (primera vez, cargando modelos)')
  })
  it('the sex model\'s load flag alone also says «primera vez»', () => {
    expect(timingLine({ source: 'phone', ms: 1400, timing: { loadMs: 0, fresh: false, sexMs: 400, sexLoadMs: 0, sexFresh: true } }, es, 'es')).toBe('Tiempo: 1,4 s en este teléfono, sexo 0,4 s (primera vez, cargando modelos)')
  })
})
