<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import { RouterLink } from 'vue-router'
import { ChevronLeft, ChevronRight, RefreshCw, Wand2 } from 'lucide-vue-next'
import { api } from '../lib/api'
import { errorText, notify } from '../lib/notice'
import { useSession } from '../stores/session'

/**
 * "Revisión de datos": the inconsistencies the server finds across the workbook
 * (the assistant's check_data tool), by kind and sheet. A row opens in the grid;
 * obvious fixes can be sent as one proposal to confirm in Asistente.
 */
interface Ref {
  sheet: string
  row: number
  recordId: string
  label: string
  field?: string
  value?: unknown
}
interface Issue extends Ref {
  id: string
  kind: string
  field: string
  problem: string
  fix?: { recordId: string; values: Record<string, unknown> }
  fixNote?: string
  related?: Ref[]
}
interface Page {
  checkedAt: string
  total: number
  offset: number
  limit: number
  counts: Record<string, number>
  sheets: Record<string, number>
  kinds: Record<string, string>
  issues: Issue[]
}

const props = defineProps<{ sheet: string }>()
const emit = defineEmits<{ open: [sheet: string, search: string] }>()
const session = useSession()
const PAGE = 100
const kind = ref('')
const onlySheet = ref(true)
const offset = ref(0)
const page = ref<Page | null>(null)
const loading = ref(false)
const chosen = ref(new Set<string>())

async function load() {
  loading.value = true
  try {
    const query = new URLSearchParams({ limit: String(PAGE), offset: String(offset.value) })
    if (kind.value) query.set('kind', kind.value)
    if (onlySheet.value) query.set('sheet', props.sheet)
    page.value = await api<Page>(`checks?${query}`)
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    loading.value = false
  }
}
watch(
  [kind, onlySheet, () => props.sheet],
  () => {
    chosen.value = new Set()
    // Back to the first page (which loads it), or reload the first page.
    if (offset.value) offset.value = 0
    else load()
  },
  { immediate: true },
)
watch(offset, load)

const withFix = computed(() => page.value?.issues.filter(i => i.fix) ?? [])
function toggle(id: string) {
  const next = new Set(chosen.value)
  if (!next.delete(id)) next.add(id)
  chosen.value = next
}
async function propose() {
  try {
    const out = await api<{ rows: number }>('chat/proposals/from-checks', {
      method: 'POST',
      body: { ids: [...chosen.value] },
    })
    chosen.value = new Set()
    notify(`${out.rows} ${out.rows === 1 ? 'fila propuesta' : 'filas propuestas'}: revísalas en Asistente → Cambios propuestos`, 'success')
  } catch (e) {
    notify(errorText(e), 'error')
  }
}
const show = (v: unknown) => (v === null || v === undefined || v === '' ? '—' : String(v))
const fixText = (i: Issue) =>
  i.fix
    ? Object.entries(i.fix.values)
        .map(([f, v]) => `${f} → ${show(v)}`)
        .join(', ') + (i.fixNote ? ` (${i.fixNote})` : '')
    : ''
</script>

<template>
  <div class="flex h-full flex-col">
    <!-- One scrolling row of kinds on phones, so the list keeps the screen. -->
    <div class="flex items-center gap-1.5 overflow-x-auto border-b border-stone-200 bg-white px-4 py-2 text-xs md:flex-wrap">
      <button
        class="shrink-0 rounded-full px-2.5 py-1 whitespace-nowrap"
        :class="!kind ? 'bg-brand-700 text-white' : 'bg-stone-100 text-stone-700 hover:bg-stone-200'"
        @click="kind = ''"
      >
        Todo ({{ page ? Object.values(page.counts).reduce((a, b) => a + b, 0) : '…' }})
      </button>
      <template v-for="(label, key) in page?.kinds" :key="key">
        <button
          v-if="page?.counts[key]"
          class="shrink-0 rounded-full px-2.5 py-1 whitespace-nowrap"
          :class="kind === key ? 'bg-brand-700 text-white' : 'bg-stone-100 text-stone-700 hover:bg-stone-200'"
          @click="kind = String(key)"
        >
          {{ label }} ({{ page.counts[key] }})
        </button>
      </template>
      <label class="ml-auto flex shrink-0 items-center gap-1.5 whitespace-nowrap">
        <input v-model="onlySheet" type="checkbox" /> Solo {{ sheet }}
      </label>
      <button class="btn-ghost" :disabled="loading" title="Volver a revisar" @click="load">
        <RefreshCw :size="14" :class="{ 'animate-spin': loading }" />
      </button>
    </div>
    <p v-if="!onlySheet && page" class="hint px-4 py-1">
      Por hoja:
      <span v-for="(n, s) in page.sheets" :key="s" class="mr-2">{{ s }} {{ n }}</span>
    </p>
    <div class="min-h-0 flex-1 overflow-auto">
      <p v-if="page && !page.issues.length" class="p-6 text-sm text-stone-500">
        No se encontró nada {{ kind ? `de «${page.kinds[kind]}» ` : '' }}{{ onlySheet ? `en ${sheet}` : 'en el libro' }}.
      </p>
      <table v-else-if="page" class="w-full border-collapse text-xs">
        <thead class="sticky top-0 bg-stone-100 text-left">
          <tr>
            <th class="w-7 border-b border-stone-200 px-1.5 py-1" />
            <th class="border-b border-stone-200 px-1.5 py-1">Hoja</th>
            <th class="border-b border-stone-200 px-1.5 py-1">Fila</th>
            <th class="border-b border-stone-200 px-1.5 py-1">ID</th>
            <th class="border-b border-stone-200 px-1.5 py-1">Columna</th>
            <th class="border-b border-stone-200 px-1.5 py-1">Valor</th>
            <th class="border-b border-stone-200 px-1.5 py-1">Problema</th>
            <th class="border-b border-stone-200 px-1.5 py-1">Arreglo</th>
          </tr>
        </thead>
        <tbody>
          <tr v-for="i in page.issues" :key="i.id" class="align-top hover:bg-stone-50">
            <td class="border-b border-stone-100 px-1.5 py-1">
              <input
                v-if="i.fix && session.canEdit"
                type="checkbox"
                :checked="chosen.has(i.id)"
                :aria-label="`Proponer el arreglo de ${i.label}`"
                @change="toggle(i.id)"
              />
            </td>
            <td class="border-b border-stone-100 px-1.5 py-1 text-stone-600">{{ i.sheet }}</td>
            <td class="border-b border-stone-100 px-1.5 py-1 tabular-nums">
              <button class="text-brand-700 hover:underline" title="Abrir en la tabla" @click="emit('open', i.sheet, i.label)">
                {{ i.row }}
              </button>
            </td>
            <td class="border-b border-stone-100 px-1.5 py-1 font-medium whitespace-nowrap">{{ i.label }}</td>
            <td class="border-b border-stone-100 px-1.5 py-1">{{ i.field }}</td>
            <td class="border-b border-stone-100 bg-red-50 px-1.5 py-1">{{ show(i.value) }}</td>
            <td class="min-w-72 border-b border-stone-100 px-1.5 py-1">
              {{ i.problem }}
              <span v-for="r in i.related?.slice(0, 3)" :key="`${r.sheet}${r.row}`" class="ml-1">
                <button class="text-brand-700 hover:underline" @click="emit('open', r.sheet, r.label)">
                  → {{ r.sheet === i.sheet ? '' : `${r.sheet} ` }}fila {{ r.row }}
                </button>
              </span>
            </td>
            <td class="border-b border-stone-100 px-1.5 py-1 text-emerald-800">{{ fixText(i) }}</td>
          </tr>
        </tbody>
      </table>
    </div>
    <div v-if="page" class="flex flex-wrap items-center gap-2 border-t border-stone-200 bg-white px-4 py-2 text-xs">
      <span>
        {{ page.total ? `${page.offset + 1}–${Math.min(page.offset + page.limit, page.total)} de ${page.total}` : '0' }}
      </span>
      <button class="btn-ghost" :disabled="!page.offset" title="Anteriores" @click="offset = Math.max(0, offset - PAGE)">
        <ChevronLeft :size="15" />
      </button>
      <button
        class="btn-ghost"
        :disabled="page.offset + page.limit >= page.total"
        title="Siguientes"
        @click="offset += PAGE"
      >
        <ChevronRight :size="15" />
      </button>
      <template v-if="session.canEdit && withFix.length">
        <button class="btn" @click="chosen = new Set(withFix.map(i => i.id))">Elegir los {{ withFix.length }} con arreglo</button>
        <button class="btn-primary" :disabled="!chosen.size" @click="propose">
          <Wand2 :size="15" /> Proponer {{ chosen.size }} {{ chosen.size === 1 ? 'arreglo' : 'arreglos' }}
        </button>
      </template>
      <span class="hint ml-auto hidden md:inline">
        Revisado {{ new Date(page.checkedAt).toLocaleTimeString('es-EC', { hour: '2-digit', minute: '2-digit' }) }} · el
        asistente ve la misma lista (check_data) y puede proponer los arreglos;
        <RouterLink to="/asistente" class="text-brand-700 hover:underline">Asistente</RouterLink>
      </span>
    </div>
  </div>
</template>
