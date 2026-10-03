import { t } from './i18n'

/**
 * Cambios propuestos by T3 chat: the panel shows the proposals of the chat
 * open in T3 (the T3 frame beside it says which, lib/t3Bridge; without it the
 * server guesses from T3, see server/t3chats.mjs), or of a chat picked in its
 * selector. Pure helpers, kept apart so they can be tested.
 */
export interface ChatScope {
  /** A T3 thread id, 'draft' (a new chat in T3), 'app' (proposals made outside T3 chats) or 'all'. */
  chat: string
  /** open: the chat open in T3; recent: the chat active last (T3 does not say which is open); all: no T3 chats. */
  how: 'open' | 'recent' | 'all' | 'chosen' | 'only'
  title: string | null
}
export interface ChatEntry {
  id: string
  title: string | null
  pending: number
}

/** 'auto' follows T3; otherwise the chat picked in the selector. */
export type ChatChoice = 'auto' | string

/**
 * The choice after a new answer: a chat picked by hand gives way when another
 * chat is opened in T3 (the person moved on), and picking the chat T3 shows is
 * following T3.
 */
export function keepChoice(chosen: ChatChoice, before: ChatScope | null, follow: ChatScope): ChatChoice {
  if (chosen === 'auto') return chosen
  if (follow.how === 'open' && before && before.chat !== follow.chat) return 'auto'
  return chosen
}

/**
 * The list's address: the chat asked for, the one the page follows, the one
 * its T3 frame shows (seen, lib/t3Bridge), the revision it holds.
 */
export function listQuery(o: {
  chosen: ChatChoice
  follow: ChatScope | null
  seen?: string
  only?: string
  revision: string
  /** The chats and titles the page holds (the server's stamp): with the revision, an unchanged list is not sent again. */
  stamp?: string
  wait?: boolean
}) {
  const q = new URLSearchParams({ all: '1', chat: o.chosen })
  if (o.only) q.set('only', o.only)
  if (o.follow) q.set('follow', o.follow.chat)
  if (o.seen) q.set('seen', o.seen)
  if (o.wait !== false) q.set('wait', '1')
  q.set('revision', o.revision)
  if (o.revision && o.stamp) q.set('stamp', o.stamp)
  return `chat/proposals?${q}`
}

const titled = (title: string | null) => title || t('chat sin título')

/** The selector's options: following T3, each chat with pending proposals, those outside T3 chats, all. */
export function chatOptions(follow: ChatScope | null, chats: ChatEntry[]): { value: string; label: string }[] {
  const out: { value: string; label: string }[] = []
  if (follow && follow.how !== 'all')
    out.push({
      value: 'auto',
      label:
        follow.how === 'open'
          ? t('Este chat: {title}', { title: follow.chat === 'draft' ? t('chat nuevo') : titled(follow.title) })
          : t('Último chat: {title}', { title: titled(follow.title) }),
    })
  for (const c of chats) {
    if (c.id === 'app') continue
    out.push({ value: c.id, label: `${titled(c.title)} (${c.pending})` })
  }
  const outside = chats.find(c => c.id === 'app')
  if (outside) out.push({ value: 'app', label: t('Fuera de los chats de T3 ({n})', { n: outside.pending }) })
  out.push({ value: 'all', label: t('Todos los chats ({n})', { n: chats.reduce((n, c) => n + c.pending, 0) }) })
  return out
}

/** Whether the selector is worth showing: there are T3 chats to choose from. */
export const hasChats = (follow: ChatScope | null, chats: ChatEntry[]) =>
  (!!follow && follow.how !== 'all') || chats.some(c => c.id !== 'app')

/** Pending proposals in other chats than the one shown (0 when showing all). */
export function elsewhere(scope: ChatScope | null, chats: ChatEntry[]) {
  if (!scope || scope.chat === 'all') return 0
  return chats.filter(c => c.id !== scope.chat).reduce((n, c) => n + c.pending, 0)
}

/** A proposal's room before its table is built (title, table of `rows` rows, buttons), in px. */
export const cardHeight = (rows: number) => 120 + Math.min(rows, 60) * 28
