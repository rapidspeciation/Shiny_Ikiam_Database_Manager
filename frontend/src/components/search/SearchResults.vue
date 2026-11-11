<script setup lang="ts">
import { computed, onBeforeUnmount, ref, shallowRef, watch } from 'vue'
import { RefreshCw } from 'lucide-vue-next'
import SearchSection from './SearchSection.vue'
import { errorText } from '../../lib/notice'
import { MIN_QUERY, searchSheets, type SearchReply } from '../../lib/search'
import type { HistoryTarget } from '../../lib/history'
import { useTables } from '../../stores/tables'

/**
 * The Buscador's results: every sheet where the text is, best first, each as
 * a grid of the rows around its match (SearchSection). `pin` (the sheet picked
 * above, or named by a link) comes first.
 */
const props = defineProps<{ query: string; pin: string }>()
const emit = defineEmits<{ open: [module: string, row: number]; history: [target: HistoryTarget] }>()

const reply = shallowRef<SearchReply | null>(null)
const searching = ref(false)
const error = ref('')
/** Bumped by the refresh button: the same search again, with what changed since. */
const round = ref(0)
/** Each answer's grids are new ones. */
const answers = ref(0)
let controller: AbortController | null = null
/**
 * Sheets whose row form is open. A newer answer (the search box's delayed search, or the refresh button) would
 * rebuild the grids and close that form while the person types in it: it waits until the form is closed.
 */
const formsOpen = new Set<string>()
let held: SearchReply | null = null
function show(answer: SearchReply) {
  held = null
  formsOpen.clear()
  reply.value = answer
  answers.value++
}
function onForm(module: string, open: boolean) {
  if (open) formsOpen.add(module)
  else formsOpen.delete(module)
  if (!formsOpen.size && held) show(held)
}

async function run() {
  controller?.abort()
  const q = props.query.trim()
  error.value = ''
  if (q.length < MIN_QUERY) {
    held = null
    formsOpen.clear()
    reply.value = null
    searching.value = false
    return
  }
  const mine = (controller = new AbortController())
  searching.value = true
  try {
    const answer = await searchSheets(q, props.pin, mine.signal)
    if (controller === mine) {
      if (formsOpen.size) held = answer
      else show(answer)
    }
  } catch (e) {
    // A search replaced by a newer one (typing on) is dropped quietly.
    if (controller !== mine || mine.signal.aborted) return
    error.value = errorText(e)
  } finally {
    if (controller === mine) searching.value = false
  }
}
// Only the text and the refresh button search again; a sheet picked above is moved up here.
watch([() => props.query.trim(), round], run, { immediate: true })
onBeforeUnmount(() => controller?.abort())

// The grids' dropdowns come from the Lists sheet (read once, shared with the other tabs).
const tables = useTables()
watch(reply, r => r?.sheets.length && tables.load('Lists').catch(() => {}))

const sheets = computed(() => {
  const list = reply.value?.sheets || []
  const pinned = list.filter(s => s.module === props.pin)
  return [...pinned, ...list.filter(s => s.module !== props.pin)]
})

function jump(module: string) {
  document.getElementById(`buscar-${module}`)?.scrollIntoView({ behavior: 'smooth', block: 'start' })
}
</script>

<template>
  <div class="flex flex-col gap-3 p-3 sm:p-4">
    <p v-if="query.trim().length < MIN_QUERY" class="text-stone-500">
      {{ $t('Escribe al menos 2 letras o números.') }}
    </p>
    <p v-else-if="error" class="text-red-700">{{ error }}</p>
    <p v-else-if="!reply" class="text-stone-500">{{ $t('Buscando «{text}» en todas las hojas…', { text: query.trim() }) }}</p>
    <template v-else>
      <div class="flex flex-wrap items-center gap-2 text-sm">
        <span v-if="!sheets.length" class="text-stone-600">
          {{ $t('Ninguna celda de ninguna hoja contiene «{text}».', { text: reply.query }) }}
        </span>
        <template v-else>
          <span class="text-stone-600">{{ $tn(sheets.length, 'En {n} hoja:', 'En {n} hojas:') }}</span>
          <button
            v-for="s in sheets"
            :key="s.module"
            type="button"
            class="rounded-full border border-stone-300 bg-surface px-2.5 py-0.5 hover:border-emerald-700 hover:text-emerald-900"
            :class="{ 'border-emerald-700 font-semibold': s.idExact || s.exact }"
            @click="jump(s.module)"
          >
            {{ s.module }} <span class="text-stone-500 tabular-nums">{{ s.total }}</span>
          </button>
        </template>
        <button
          type="button"
          class="btn ml-auto"
          :disabled="searching"
          :title="$t('Buscar otra vez (con los cambios de otras personas)')"
          @click="round++"
        >
          <RefreshCw :size="15" :class="{ 'animate-spin': searching }" />
        </button>
      </div>
      <SearchSection
        v-for="s in sheets"
        :id="`buscar-${s.module}`"
        :key="`${answers}\u0000${s.module}`"
        class="scroll-mt-2"
        :result="s"
        :query="reply.query"
        :pinned="s.module === pin"
        @open="(module, row) => emit('open', module, row)"
        @history="target => emit('history', target)"
        @form="open => onForm(s.module, open)"
      />
    </template>
  </div>
</template>
