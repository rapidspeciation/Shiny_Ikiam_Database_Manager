import { describe, expect, it } from 'vitest'
import { loadsSince, seconds, timingLine } from '../identifyTiming'
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

describe('seconds and loadsSince', () => {
  it('one decimal, rounded', () => {
    expect(seconds(1449, 'es')).toBe('1,4')
    expect(seconds(1450, 'es')).toBe('1,5')
    expect(seconds(12000, 'es')).toBe('12,0')
    expect(seconds(-5, 'en')).toBe('0.0')
  })
  it('only the loads this photo needed: new or changed *_load keys, the sex model apart', () => {
    const before = { small_load: 3000, detector_load: 400, photo: 1200, sex_load: 800 }
    expect(loadsSince(before, { ...before, photo: 1300 })).toBe(0)
    expect(loadsSince({}, { small_load: 3000, detector_load: 400, photo: 1200 })).toBe(3400)
    expect(loadsSince(before, { ...before, full_load: 9000 })).toBe(9000)
    expect(loadsSince(before, { ...before, sex_load: 700 })).toBe(0)
    expect(loadsSince(before, { ...before, sex_load: 700 }, 'sex_load')).toBe(700)
    expect(loadsSince(before, before, 'sex_load')).toBe(0)
  })
})
