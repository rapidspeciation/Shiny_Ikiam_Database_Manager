<script setup lang="ts">
import { onMounted, ref } from 'vue'
import { ExternalLink, RefreshCw } from 'lucide-vue-next'
import { api } from '../lib/api'
import { errorText } from '../lib/notice'
import { useSession } from '../stores/session'

/**
 * T3 Code (a stock install on its own address) inside the Asistente tab. The
 * first time on a device the app asks T3 for a one-time sign-in link; T3 then
 * keeps its own session for 30 days, so later visits open it directly.
 */
const props = defineProps<{ url: string }>()
const session = useSession()
const src = ref('')
const problem = ref('')
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
</script>

<template>
  <div class="flex h-full flex-col">
    <div class="flex items-center gap-2 border-b border-stone-200 bg-white px-3 py-1 text-xs text-stone-600">
      <span>T3 Code · cambia de modelo o proveedor, adjunta fotos y archivos, y revisa tu uso en su menú.</span>
      <button class="btn-ghost ml-auto" title="Volver a conectar" @click="connect(true)"><RefreshCw :size="13" /></button>
      <a class="btn-ghost" :href="url" target="_blank" rel="noopener" title="Abrir en otra pestaña"
        ><ExternalLink :size="13"
      /></a>
    </div>
    <p v-if="problem" class="bg-amber-50 px-4 py-2 text-sm text-amber-900">{{ problem }}</p>
    <iframe
      v-if="src"
      :src="src"
      class="min-h-0 w-full flex-1 border-0"
      allow="clipboard-read; clipboard-write; microphone; camera"
      title="T3 Code"
    />
  </div>
</template>
