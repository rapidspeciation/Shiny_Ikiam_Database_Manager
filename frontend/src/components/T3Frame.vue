<script setup lang="ts">
import { onBeforeUnmount, onMounted, ref } from 'vue'
import { api } from '../lib/api'
import { errorText } from '../lib/notice'
import { BRIDGE_WAIT_MS, HELLO, bridgeMessage, originOf, type T3Seen } from '../lib/t3Bridge'
import { useSession } from '../stores/session'

/**
 * T3 Code (a stock install on its own address) inside the Asistente tab. The
 * first time on a device the app asks T3 for a one-time sign-in link; T3 then
 * keeps its own session for 30 days, so later visits open it directly.
 * T3's page, served with the app's bridge (server/t3bridge.mjs), says which
 * chat it shows on each navigation: passed on as `seen` (Cambios propuestos
 * follows it). Only this frame's messages count, not another T3 tab's.
 */
const props = defineProps<{ url: string }>()
const emit = defineEmits<{ seen: [value: T3Seen] }>()
const session = useSession()
const src = ref('')
const problem = ref('')
const frame = ref<HTMLIFrameElement>()
const t3Origin = originOf(props.url)
let silent: ReturnType<typeof setTimeout> | undefined

function hear(event: MessageEvent) {
  const view = bridgeMessage(event, frame.value?.contentWindow, t3Origin)
  if (!view) return
  clearTimeout(silent)
  emit('seen', { bridge: 'on', view })
}
/** A page loaded in the frame: the bridge is asked again; silent for a few seconds = no bridge. */
function loaded() {
  frame.value?.contentWindow?.postMessage(HELLO, t3Origin)
  clearTimeout(silent)
  silent = setTimeout(() => emit('seen', { bridge: 'off', view: null }), BRIDGE_WAIT_MS)
}
onMounted(() => {
  emit('seen', { bridge: 'waiting', view: null })
  addEventListener('message', hear)
})
onBeforeUnmount(() => {
  removeEventListener('message', hear)
  clearTimeout(silent)
})
const key = `t3:paired:${session.user?.username}`
const DAYS_29 = 29 * 24 * 60 * 60 * 1000

async function connect(force = false) {
  problem.value = ''
  const paired = Number(localStorage.getItem(key) || 0)
  if (!force && Date.now() - paired < DAYS_29) return void (src.value = props.url)
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
