<script setup lang="ts">
import { computed, onActivated, onMounted, watch } from 'vue'
import { RouterLink, useRoute } from 'vue-router'
import { ArrowLeft } from 'lucide-vue-next'
import ProposalsLive from '../components/assistant/ProposalsLive.vue'
import { locale, t } from '../lib/i18n'

/**
 * Cambios propuestos on their own browser tab (#/propuestas, or
 * #/propuestas/<id> for one proposal): the same live tables as beside T3, full
 * screen, e.g. on a second monitor while the chat stays in the first tab.
 */
const route = useRoute()
const only = computed(() => String(route.params.id ?? ''))
const title = () => (document.title = `${t('Cambios propuestos')} · Ikiam Insectary DB`)
onMounted(title)
onActivated(title)
watch(locale, title)
</script>

<template>
  <div class="flex h-full flex-col">
    <ProposalsLive layout="page" :only="only" class="min-h-0 flex-1" />
    <RouterLink
      to="/asistente"
      class="flex items-center gap-1 border-t border-stone-200 bg-white px-3 py-1 text-xs text-stone-500 hover:text-stone-800"
    >
      <ArrowLeft :size="12" /> {{ $t('Volver al Asistente') }}
    </RouterLink>
  </div>
</template>
