import { describe, expect, it } from 'vitest'
import { cardHeight, chatOptions, elsewhere, hasChats, keepChoice, listQuery, type ChatEntry, type ChatScope } from '../proposalChats'

const open = (chat: string, title = 'Cuaderno'): ChatScope => ({ chat, how: 'open', title })
const chats: ChatEntry[] = [
  { id: 'b', title: 'Posturas', pending: 2 },
  { id: 'a', title: null, pending: 1 },
  { id: 'app', title: null, pending: 3 },
]

describe('proposals by T3 chat', () => {
  it('a chat picked by hand stays until another chat is opened in T3', () => {
    expect(keepChoice('auto', open('a'), open('b'))).toBe('auto')
    expect(keepChoice('b', open('a'), open('a'))).toBe('b')
    expect(keepChoice('all', open('a'), open('c'))).toBe('auto')
    // Only a chat opened in T3 counts: a guess (the latest active) changing does not take the choice away.
    expect(keepChoice('b', { chat: 'a', how: 'recent', title: null }, { chat: 'c', how: 'recent', title: null })).toBe('b')
    expect(keepChoice('b', null, open('c'))).toBe('b')
  })

  it('asks for the chat chosen, says which one it follows, waits unless told not to', () => {
    const q = new URLSearchParams(listQuery({ chosen: 'auto', follow: open('a'), revision: 'x.3' }).split('?')[1])
    expect(Object.fromEntries(q)).toEqual({ all: '1', chat: 'auto', follow: 'a', wait: '1', revision: 'x.3' })
    const one = new URLSearchParams(listQuery({ chosen: 'auto', follow: null, only: 'p1', revision: '', wait: false }).split('?')[1])
    expect(Object.fromEntries(one)).toEqual({ all: '1', chat: 'auto', only: 'p1', revision: '' })
    // The chat the T3 frame shows (its bridge).
    const seen = new URLSearchParams(listQuery({ chosen: 'auto', follow: null, seen: 'draft', revision: '' }).split('?')[1])
    expect(seen.get('seen')).toBe('draft')
    // The stamp goes with a revision only (a first request has neither).
    expect(listQuery({ chosen: 'auto', follow: null, revision: 'x.3', stamp: 's1' })).toContain('stamp=s1')
    expect(listQuery({ chosen: 'auto', follow: null, revision: '', stamp: 's1' })).not.toContain('stamp')
  })

  it('the selector: the chat T3 shows, each chat with its count, those outside T3, all', () => {
    expect(chatOptions(open('a', 'Emergidos'), chats)).toEqual([
      { value: 'auto', label: 'Este chat: Emergidos' },
      { value: 'b', label: 'Posturas (2)' },
      { value: 'a', label: 'chat sin título (1)' },
      { value: 'app', label: 'Fuera de los chats de T3 (3)' },
      { value: 'all', label: 'Todos los chats (6)' },
    ])
    expect(chatOptions({ chat: 'b', how: 'recent', title: 'Posturas' }, [])[0].label).toBe('Último chat: Posturas')
    expect(chatOptions({ chat: 'draft', how: 'open', title: null }, [])[0].label).toBe('Este chat: chat nuevo')
    expect(chatOptions({ chat: 'all', how: 'all', title: null }, [chats[2]]).map(o => o.value)).toEqual(['app', 'all'])
  })

  it('shows the selector only with T3 chats; counts what waits elsewhere', () => {
    expect(hasChats({ chat: 'all', how: 'all', title: null }, [chats[2]])).toBe(false)
    expect(hasChats({ chat: 'all', how: 'all', title: null }, chats)).toBe(true)
    expect(hasChats(open('a'), [])).toBe(true)
    expect(elsewhere(open('a'), chats)).toBe(5)
    expect(elsewhere({ chat: 'all', how: 'chosen', title: null }, chats)).toBe(0)
    expect(elsewhere(null, chats)).toBe(0)
  })

  it('keeps room for a table not built yet, up to a screenful', () => {
    expect(cardHeight(1)).toBe(148)
    expect(cardHeight(500)).toBe(cardHeight(60))
  })
})
