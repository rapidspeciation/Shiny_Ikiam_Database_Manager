import { describe, expect, it } from 'vitest'
import { afterMove, assistantLink, bridgeMessage, chatPath, originOf, seenChat, type T3View } from '../t3Bridge'

const T3 = 'https://t3.example.org'
const ENV = '4e6c4765-8cfa-4adc-b761-3c3bae2ae7e0'
const A = '5c8de89d-bbaf-4328-b354-74733e099781'
const frame = {} as Window
const data = (over: Record<string, unknown> = {}) => ({
  type: 'ithomiini-t3',
  v: 1,
  path: `/${ENV}/${A}`,
  environmentId: ENV,
  threadId: A,
  draftId: null,
  visible: true,
  focused: false,
  ...over,
})
const view = (over: Partial<T3View> = {}): T3View => ({
  path: '/',
  environmentId: null,
  threadId: null,
  draftId: null,
  visible: true,
  focused: true,
  ...over,
})

describe('the T3 bridge', () => {
  it("hears only this frame's bridge, from T3's origin", () => {
    expect(bridgeMessage({ source: frame, origin: T3, data: data() }, frame, T3)).toEqual({
      path: `/${ENV}/${A}`,
      environmentId: ENV,
      threadId: A,
      draftId: null,
      visible: true,
      focused: false,
    })
    // Another window (a second T3 tab can't post here, but another frame could), another site, no frame yet.
    expect(bridgeMessage({ source: {}, origin: T3, data: data() }, frame, T3)).toBeNull()
    expect(bridgeMessage({ source: frame, origin: 'https://evil.example', data: data() }, frame, T3)).toBeNull()
    expect(bridgeMessage({ source: frame, origin: T3, data: data() }, null, T3)).toBeNull()
    expect(bridgeMessage({ source: frame, origin: T3, data: data() }, frame, '')).toBeNull()
    // Not the bridge's shape: T3's own messages, a later version.
    expect(bridgeMessage({ source: frame, origin: T3, data: { type: 'other' } }, frame, T3)).toBeNull()
    expect(bridgeMessage({ source: frame, origin: T3, data: data({ v: 2 }) }, frame, T3)).toBeNull()
    expect(bridgeMessage({ source: frame, origin: T3, data: 'text' }, frame, T3)).toBeNull()
    // A thread id that is not one is no chat.
    expect(bridgeMessage({ source: frame, origin: T3, data: data({ threadId: "x' OR 1" }) }, frame, T3)?.threadId).toBeNull()
    expect(originOf(`${T3}/pair#token=x`)).toBe(T3)
    expect(originOf('nonsense')).toBe('')
  })

  it('the chat to ask for: the thread, a new chat, none; nothing while the bridge is not speaking', () => {
    expect(seenChat({ bridge: 'on', view: view({ threadId: A, environmentId: ENV }) })).toBe(A)
    expect(seenChat({ bridge: 'on', view: view({ draftId: 'd1' }) })).toBe('draft')
    expect(seenChat({ bridge: 'on', view: view({ path: '/settings/general' }) })).toBe('none')
    expect(seenChat({ bridge: 'waiting', view: null })).toBeUndefined()
    expect(seenChat({ bridge: 'off', view: null })).toBeUndefined()
    expect(seenChat(null)).toBeUndefined()
  })

  it('a chat picked by hand stays until the frame moves to another chat', () => {
    expect(afterMove('b', undefined, A)).toBe('b')
    expect(afterMove('b', A, A)).toBe('b')
    expect(afterMove('b', A, 'draft')).toBe('auto')
    expect(afterMove('all', 'draft', A)).toBe('auto')
    expect(afterMove('auto', A, 'none')).toBe('auto')
  })

  it('links: to a chat, from the address', () => {
    expect(chatPath(ENV, A)).toBe(`/${ENV}/${A}`)
    expect(chatPath(null, A)).toBeNull()
    expect(chatPath(ENV, '../x')).toBeNull()
    expect(assistantLink({ propuesta: A, chat: A.toUpperCase(), fila: 'CAM0123' })).toEqual({
      proposal: A,
      chat: A,
      row: 'CAM0123',
    })
    expect(assistantLink({ chat: A })).toEqual({ proposal: null, chat: A, row: null })
    expect(assistantLink({ propuesta: 'nope' })).toBeNull()
    expect(assistantLink({ grupo: 'x' })).toBeNull()
  })
})
