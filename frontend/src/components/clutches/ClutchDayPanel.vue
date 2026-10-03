<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted, ref } from 'vue'
import { BookOpen, History, X } from 'lucide-vue-next'
import NotebookChanges from './NotebookChanges.vue'
import TodayChanges from './TodayChanges.vue'
import { useClutchDay } from '../../composables/useClutchDay'
import { initialsOf } from '../../lib/rows'
import { useSession } from '../../stores/session'

/**
 * The Clutches history as a panel over the table, for the table mode: the
 * day's changes (TodayChanges: to copy into the notebook and to undo) and the
 * notebook's list (NotebookChanges: everything since the notebook was brought
 * up to date); the cards show them as their «Hoy» and «Cuaderno» lists.
 */
const props = defineProps<{ collectors: string[] }>()
const emit = defineEmits<{ close: [] }>()
const session = useSession()
const day = useClutchDay()
const tab = ref<'today' | 'notebook'>('today')
const initials = computed(() => initialsOf(session.user?.displayName || '', props.collectors, session.user?.username || ''))
const initialsFor = (name: string) => initialsOf(name, props.collectors)
const onKey = (e: KeyboardEvent) => e.key === 'Escape' && emit('close')
onMounted(() => window.addEventListener('keydown', onKey))
onBeforeUnmount(() => window.removeEventListener('keydown', onKey))
</script>

<template>
  <div class="fixed inset-0 z-50 flex justify-end bg-black/30" @click.self="emit('close')">
    <section class="flex h-full w-full flex-col bg-stone-50 shadow-xl sm:max-w-xl" role="dialog" :aria-label="$t('Historial de Clutches')">
      <header class="flex items-center gap-2 border-b border-stone-200 bg-white px-3 py-1.5">
        <History :size="20" class="shrink-0 text-stone-600" />
        <h2 class="min-w-0 flex-1 truncate text-lg font-semibold">{{ $t('Historial de Clutches') }}</h2>
        <div class="inline-flex overflow-hidden rounded-lg border border-stone-300 text-sm" role="tablist">
          <button role="tab" class="h-10 px-3" :class="tab === 'today' ? 'bg-brand-700 text-white' : 'bg-white'" :aria-selected="tab === 'today'" @click="tab = 'today'">
            {{ $t('Hoy') }}
          </button>
          <button
            role="tab"
            class="h-10 border-l border-stone-300 px-3"
            :class="tab === 'notebook' ? 'bg-brand-700 text-white' : 'bg-white'"
            :aria-selected="tab === 'notebook'"
            @click="tab = 'notebook'"
          >
            <BookOpen :size="14" class="-mt-0.5 inline" /> {{ $t('Cuaderno') }}
          </button>
        </div>
        <button class="btn-ghost h-11 w-11 justify-center" :aria-label="$t('Cerrar')" @click="emit('close')"><X :size="22" /></button>
      </header>
      <div class="min-h-0 flex-1 overflow-y-auto">
        <TodayChanges v-if="tab === 'today'" :day="day" :initials="initials" />
        <NotebookChanges v-else :initials-for="initialsFor" />
      </div>
    </section>
  </div>
</template>
