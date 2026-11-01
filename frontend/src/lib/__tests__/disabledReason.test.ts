import { describe, expect, it } from 'vitest'
import { disabledReason } from '../disabledReason'

describe('disabledReason', () => {
  it('gives the title of a disabled button, also from its icon', () => {
    document.body.innerHTML = '<button disabled data-why=" Elige primero la especie "><svg><path/></svg></button>'
    expect(disabledReason(document.querySelector('path'))).toBe('Elige primero la especie')
  })
  it('says nothing for an enabled button or one without a title', () => {
    document.body.innerHTML = '<button id="on" data-why="Copiar"></button><button id="plain" disabled title="Copiar"></button><button id="off" disabled></button>'
    expect(disabledReason(document.getElementById('on'))).toBe('')
    expect(disabledReason(document.getElementById('off'))).toBe('')
    expect(disabledReason(document.getElementById('plain'))).toBe('')
  })
  it('reads aria-disabled controls (links drawn as buttons)', () => {
    document.body.innerHTML = '<a aria-disabled="true" data-why="Primero trae un recorrido">CSV</a>'
    expect(disabledReason(document.querySelector('a'))).toBe('Primero trae un recorrido')
  })
})
