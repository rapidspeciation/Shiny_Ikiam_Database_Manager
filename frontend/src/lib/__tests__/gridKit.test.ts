import { describe, expect, it } from 'vitest'
import { tileToSelection } from '../gridKit'

describe('pasting over a selection', () => {
  it('repeats the copied block to fill a larger selection, as Google Sheets does', () => {
    const pair = [['Ithomia salapia', 'salapia']]
    // Copied SPECIES + Subspecies_Form of one row, pasted over three rows of SPECIES.
    expect(tileToSelection(pair, 3, 1)).toEqual([pair[0], pair[0], pair[0]])
    // One value over a 2 × 2 selection fills the four cells.
    expect(tileToSelection([['male']], 2, 2)).toEqual([
      ['male', 'male'],
      ['male', 'male'],
    ])
    // Two rows over five: the pattern repeats.
    expect(tileToSelection([['a'], ['b']], 5, 1).map(r => r[0])).toEqual(['a', 'b', 'a', 'b', 'a'])
  })
  it('pastes the block as it is into a single selected cell (it can add rows)', () => {
    const block = [['x', 'y'], ['z', 'w']]
    expect(tileToSelection(block, 1, 1)).toEqual(block)
  })
})
