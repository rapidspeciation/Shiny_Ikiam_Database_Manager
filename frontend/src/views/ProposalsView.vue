<script setup lang="ts">
import { computed, onActivated, onMounted, watch } from 'vue'
import { RouterLink, useRoute, useRouter } from 'vue-router'
import { ArrowLeft } from 'lucide-vue-next'
import ProposalsLive from '../components/assistant/ProposalsLive.vue'
import { locale, t } from '../lib/i18n'

/**
 * Cambios propuestos on their own browser tab, on any device: the same live
 * tables as beside T3, full screen. Its address says what it shows, fixed
 * whatever chat is opened elsewhere: #/propuestas/<id> one proposal,
 * #/propuestas?chat=<thread> one chat's (or app / all); #/propuestas follows
 * the chat open in T3.
 */
const route = useRoute()
const router = useRouter()
const only = computed(() => String(route.params.id ?? ''))
const chat = computed(() => (only.value ? '' : String(route.query.chat ?? '')))
/** A chat picked in the page's selector goes into its address ('' = follow T3 again). */
const pick = (value: string) => router.replace({ query: value ? { chat: value } : {} })
const back = computed(() => (only.value ? { path: '/asistente', query: { propuesta: only.value } } : '/asistente'))
const title = () => (document.title = `${t('Cambios propuestos')} · Ikiam Insectary DB`)
onMounted(title)
onActivated(title)
watch(locale, title)
</script>

<template>
  <div class="flex h-full flex-col">
    <ProposalsLive layout="page" :only="only" :chat="chat" class="min-h-0 flex-1" @chat="pick" />
    <RouterLink
      :to="back"
      class="flex items-center gap-1 border-t border-stone-200 bg-white px-3 py-1 text-xs text-stone-500 hover:text-stone-800"
    >
      <ArrowLeft :size="12" /> {{ $t('Volver al Asistente') }}
    </RouterLink>
  </div>
</template>
