<script setup lang="ts">
import { onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { useRouter } from 'vue-router'
import { api } from '../lib/api'
import { errorText } from '../lib/notice'
import {
  BRIDGE_WAIT_MS,
  HELLO,
  LINK_OK,
  bridgeLink,
  bridgeMessage,
  chatPath,
  openChat,
  originOf,
  type T3Seen,
  type T3View,
} from '../lib/t3Bridge'
import { useSession } from '../stores/session'

/**
 * T3 Code (a stock install on its own address) inside the Asistente tab. The
 * first time on a device the app asks T3 for a one-time sign-in link; T3 then
 * keeps its own session for 30 days, so later visits open it directly.
 * T3's page, served with the app's bridge (server/t3bridge.mjs), says which
 * chat it shows on each navigation: passed on as `seen` (Cambios propuestos
 * follows it). Only this frame's messages count, not another T3 tab's.
 * A link to a chat (`open`, from #/asistente?chat=…) moves the frame to it: in
 * T3's own router when the bridge is there, else by loading the chat's address.
 * Links to the Asistente tab clicked inside T3 open here (the bridge passes them on).
 */
const props = defineProps<{
  url: string
  /** T3's environment (chats are /<environmentId>/<threadId>); null: links to chats are not followed. */
  environmentId?: string | null
  /** A chat to show (a new object each time a link asks for it). */
  open?: { thread: string } | null
}>()
const emit = defineEmits<{ seen: [value: T3Seen] }>()
const session = useSession()
const router = useRouter()
const src = ref('')
const problem = ref('')
const frame = ref<HTMLIFrameElement>()
const t3Origin = originOf(props.url)
let silent: ReturnType<typeof setTimeout> | undefined
/** What the bridge said last (null: not heard since the frame loaded); silent: no bridge in this page. */
let view: T3View | null = null
let silentPage = false
/** A chat to move to once the frame can (signing in first, or loading). */
let pending: string | null = null
let fallback: ReturnType<typeof setTimeout> | undefined

const addressOf = (thread: string) => `${props.url.replace(/\/+$/, '')}${chatPath(props.environmentId, thread)}`

/** Moves the frame to a chat: in T3's router via the bridge, by its address if that does not get there. */
function go(thread: string) {
  clearTimeout(fallback)
  if (view?.threadId === thread) return
  if (!view) return void (src.value = addressOf(thread))
  frame.value?.contentWindow?.postMessage(openChat(chatPath(props.environmentId, thread)!), t3Origin)
  fallback = setTimeout(() => {
    if (view?.threadId !== thread) src.value = addressOf(thread)
  }, 3000)
}
function show(thread: string) {
  if (!chatPath(props.environmentId, thread) || view?.threadId === thread) return
  // Not loaded yet, or loading (signing in goes to T3's home first): moved once the bridge speaks.
  if (src.value && silentPage) src.value = addressOf(thread)
  else if (!src.value || !view) pending = thread
  else go(thread)
}
watch(
  () => props.open,
  value => value && show(value.thread),
  { immediate: true },
)

function hear(event: MessageEvent) {
  const link = bridgeLink(event, frame.value?.contentWindow, t3Origin)
  if (link) {
    frame.value?.contentWindow?.postMessage(LINK_OK, t3Origin)
    return void router.push(link)
  }
  const now = bridgeMessage(event, frame.value?.contentWindow, t3Origin)
  if (!now) return
  clearTimeout(silent)
  view = now
  // Still on T3's sign-in page: moved once it has gone to T3 itself.
  if (pending && now.path !== '/pair') {
    const thread = pending
    pending = null
    go(thread)
  }
  emit('seen', { bridge: 'on', view: now })
}
/** A page loaded in the frame: the bridge is asked again; silent for a few seconds = no bridge. */
function loaded() {
  view = null
  silentPage = false
  frame.value?.contentWindow?.postMessage(HELLO, t3Origin)
  clearTimeout(silent)
  silent = setTimeout(() => {
    silentPage = true
    emit('seen', { bridge: 'off', view: null })
    // Without the bridge a chat asked for is opened by its address.
    if (pending && !src.value.includes('/pair')) {
      src.value = addressOf(pending)
      pending = null
    }
  }, BRIDGE_WAIT_MS)
}
onMounted(() => {
  emit('seen', { bridge: 'waiting', view: null })
  addEventListener('message', hear)
})
onBeforeUnmount(() => {
  removeEventListener('message', hear)
  clearTimeout(silent)
  clearTimeout(fallback)
})
const key = `t3:paired:${session.user?.username}`
const DAYS_29 = 29 * 24 * 60 * 60 * 1000

async function connect(force = false) {
  problem.value = ''
  const paired = Number(localStorage.getItem(key) || 0)
  if (!force && Date.now() - paired < DAYS_29) {
    // A chat asked for before the frame first loaded: straight to it.
    src.value = pending ? addressOf(pending) : props.url
    pending = null
    return
  }
  try {
    const { url } = await api<{ url: string }>('t3/pair', { method: 'POST', body: {} })
    localStorage.setItem(key, String(Date.now()))
    src.value = url
  } catch (e) {
    problem.value = errorText(e)
  }
}
onMounted(() => connect())
defineExpose({ connect })
</script>

<template>
  <div class="flex h-full flex-col">
    <p v-if="problem" class="bg-amber-50 px-4 py-2 text-sm text-amber-900">{{ problem }}</p>
    <iframe
      v-if="src"
      ref="frame"
      :src="src"
      class="min-h-0 w-full flex-1 border-0"
      allow="clipboard-read; clipboard-write; microphone; camera"
      title="T3 Code"
      @load="loaded"
    />
  </div>
</template>
