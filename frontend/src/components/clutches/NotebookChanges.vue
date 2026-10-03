<script setup lang="ts">
import { computed, onMounted, ref, watch } from 'vue'
import { BookCheck, Check, Copy, Info, Printer, RefreshCw } from 'lucide-vue-next'
import DateField from '../DateField.vue'
import { api } from '../../lib/api'
import { changeText, notebookText, type NotebookClutch } from '../../lib/clutches'
import { dayLabel, formatSerial, todayIso } from '../../lib/dates'
import { errorText, notify } from '../../lib/notice'
import { persistentRef } from '../../lib/persist'
import { useSession } from '../../stores/session'
import { t } from '../../lib/i18n'
import { eventLine } from './eventWords'

/**
 * For the paper notebook: what everyone changed in clutches through the app
 * since the notebook was last brought up to date (or from a chosen moment),
 * clutch by clutch in the notebook's order (number ascending, batches N(k)
 * under N), each field as before → after (a count as the terms added) with
 * who and when, and the app-only events (died, disappeared, preserved…).
 * Left out: what came from the notebook itself (the assistant's proposals read
 * from its photos) and, unless asked for, what was typed in Google Sheets.
 * "Already in the notebook" remembers, for everyone, the moment it is up to date.
 */
const props = defineProps<{ initialsFor: (name: string) => string }>()
const emit = defineEmits<{ open: [recordId: string] }>()
const session = useSession()

interface UpTo {
  at: string
  by: string
  name: string
  setAt: string
}
interface List {
  from: string
  to: string
  upTo: UpTo | null
  sheets: boolean
  excluded: { notebook: number; sheets: number }
  clutches: NotebookClutch[]
}
const withSheets = persistentRef('clutches:notebook-sheets', false)
const list = ref<List | null>(null)
const loading = ref(false)
/** The start chosen by the person (date and time, local); empty: since the notebook was brought up to date. */
const fromDate = ref('')
const fromTime = ref('')

const pad = (n: number) => String(n).padStart(2, '0')
const localDate = (d: Date) => `${d.getFullYear()}-${pad(d.getMonth() + 1)}-${pad(d.getDate())}`
const localTime = (d: Date) => `${pad(d.getHours())}:${pad(d.getMinutes())}`
/** "2/10 17:30": when, as the notebook dates things. */
const when = (iso: string) => {
  const d = new Date(iso)
  return `${d.getDate()}/${d.getMonth() + 1} ${localTime(d)}`
}
const fromIso = computed(() => {
  if (!fromDate.value) return ''
  const d = new Date(`${fromDate.value}T${fromTime.value || '00:00'}:00`)
  return Number.isNaN(d.getTime()) ? '' : d.toISOString()
})

async function load() {
  loading.value = true
  try {
    const params = new URLSearchParams()
    if (fromIso.value) params.set('from', fromIso.value)
    if (withSheets.value) params.set('sheets', '1')
    list.value = await api<List>(`clutches/notebook?${params}`)
    // The start shown is the one used (since the notebook was brought up to date, by default).
    if (!fromDate.value) {
      const d = new Date(list.value.from)
      fromDate.value = localDate(d)
      fromTime.value = localTime(d)
    }
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    loading.value = false
  }
}
onMounted(load)
watch(withSheets, load)
watch([fromDate, fromTime], ([d, tm], [od, ot]) => {
  if (od && (d !== od || tm !== ot) && fromIso.value) void load()
})
/** Back to "since the notebook was brought up to date". */
function sinceUpTo() {
  fromDate.value = ''
  fromTime.value = ''
  void load()
}

// --- Already in the notebook: up to date until the end of this list (or the start chosen)
const marking = ref(false)
async function markUpTo(at: string) {
  marking.value = true
  try {
    await api('clutches/notebook/up-to', { method: 'PUT', body: { at } })
    notify(t('Cuaderno al día hasta {when}', { when: when(at) }), 'success')
    fromDate.value = ''
    fromTime.value = ''
    await load()
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    marking.value = false
  }
}

const clutches = computed(() => list.value?.clutches ?? [])
const who = (names: string[]) => names.map(n => (n === 'Google Sheets' ? 'Sheets' : props.initialsFor(n))).join(', ')
const title = computed(() => (list.value ? t('Clutches: cambios del {from} al {to}', { from: when(list.value.from), to: when(list.value.to) }) : ''))
const text = computed(() =>
  notebookText(title.value, clutches.value, {
    formatDate: formatSerial,
    when,
    who: n => who([n]),
    event: eventLine,
    removedWord: t('quitado'),
    newWord: t('nuevo'),
  }),
)
const copied = ref(false)
const showText = ref(false)
async function copy() {
  if (!navigator.clipboard) {
    showText.value = true
    return
  }
  try {
    await navigator.clipboard.writeText(text.value)
    copied.value = true
    setTimeout(() => (copied.value = false), 2500)
    notify(t('Copiado: pégalo donde quieras'), 'success')
  } catch (e) {
    notify(errorText(e), 'error')
  }
}
/** Printed as plain text, in a page of its own (the app's pages do not print well). */
function print() {
  const w = window.open('', '_blank')
  if (!w) {
    showText.value = true
    return
  }
  const doc = w.document
  doc.title = title.value
  const pre = doc.createElement('pre')
  pre.style.cssText = 'font: 13px/1.45 ui-monospace, monospace; white-space: pre-wrap; margin: 16px'
  pre.textContent = text.value
  doc.body.appendChild(pre)
  w.focus()
  w.print()
}
const upToText = computed(() => {
  const u = list.value?.upTo
  if (!u) return t('Todavía nadie marcó hasta cuándo está al día el cuaderno.')
  return t('El cuaderno está al día hasta {when} (lo marcó {who} el {set}).', { when: when(u.at), who: props.initialsFor(u.name), set: when(u.setAt) })
})
const isDefault = computed(() => !list.value?.upTo || !fromIso.value || Math.abs(Date.parse(list.value.upTo.at) - Date.parse(fromIso.value)) < 60_000)
</script>

<template>
  <div class="px-3 pt-3 pb-8">
    <p class="flex items-start gap-1.5 rounded-lg bg-brand-50 px-3 py-2 text-sm text-brand-900">
      <BookCheck :size="16" class="mt-0.5 shrink-0" />
      <span>{{ upToText }}</span>
    </p>
    <div class="mt-2 flex flex-wrap items-end gap-2">
      <label class="min-w-0">
        <span class="field-label">{{ $t('Desde') }}</span>
        <span class="flex gap-1">
          <DateField v-model="fromDate" class="field-input h-11 w-36 text-base" />
          <input v-model="fromTime" type="time" class="field-input h-11 w-36 text-base" :aria-label="$t('Hora')" />
        </span>
      </label>
      <button class="btn h-11 px-3" :aria-label="$t('Actualizar')" :disabled="loading" @click="load"><RefreshCw :size="16" :class="{ 'animate-spin': loading }" /></button>
      <button v-if="!isDefault" class="btn h-11 px-3 text-sm" @click="sinceUpTo">{{ $t('Desde que está al día') }}</button>
    </div>
    <p v-if="fromDate" class="mt-1 text-xs text-stone-600">{{ dayLabel(fromDate, todayIso()) }} {{ fromTime }} → {{ $t('ahora') }}</p>
    <div class="mt-2 flex flex-wrap items-center gap-2">
      <button class="btn-primary h-11 px-4" :disabled="!clutches.length" @click="copy">
        <Check v-if="copied" :size="16" /><Copy v-else :size="16" /> {{ $t('Copiar como texto') }}
      </button>
      <button class="btn h-11 px-3" :disabled="!clutches.length" @click="print"><Printer :size="16" /> {{ $t('Imprimir') }}</button>
      <label class="ml-auto flex min-h-11 items-center gap-1.5 text-sm text-stone-700">
        <input v-model="withSheets" type="checkbox" class="h-4 w-4" /> {{ $t('Incluir lo escrito en Google Sheets') }}
      </label>
    </div>
    <p v-if="list && (list.excluded.notebook || (!list.sheets && list.excluded.sheets))" class="mt-1 flex items-start gap-1 text-xs text-stone-500">
      <Info :size="14" class="mt-px shrink-0" />
      <span>
        <template v-if="list.excluded.notebook">
          {{ $tn(list.excluded.notebook, 'No se muestra {n} cambio que vino del cuaderno (fotos leídas por el asistente): ya está en él.', 'No se muestran {n} cambios que vinieron del cuaderno (fotos leídas por el asistente): ya están en él.') }}
        </template>
        <template v-if="!list.sheets && list.excluded.sheets">
          {{ $tn(list.excluded.sheets, 'Tampoco {n} cambio escrito en Google Sheets (suele copiar el cuaderno).', 'Tampoco {n} cambios escritos en Google Sheets (suelen copiar el cuaderno).') }}
        </template>
      </span>
    </p>
    <textarea
      v-if="showText"
      class="field-input mt-2 min-h-40 font-mono text-sm"
      readonly
      :value="text"
      :aria-label="$t('Copiar como texto')"
      @focus="($event.target as HTMLTextAreaElement).select()"
    />
    <p v-if="list && !clutches.length" class="py-6 text-center text-sm text-stone-500">{{ $t('Nada que copiar al cuaderno en este periodo.') }}</p>
    <ul class="mt-2 space-y-2">
      <li v-for="c in clutches" :key="c.recordId" class="rounded-xl border border-stone-200 bg-white shadow-sm" :data-clutch="c.clutch">
        <button class="flex min-h-12 w-full items-baseline gap-2 px-3 py-2 text-left" :aria-label="$t('Abrir el clutch {clutch}', { clutch: c.clutch })" @click="emit('open', c.recordId)">
          <span class="text-lg font-semibold">{{ c.clutch }}</span>
          <span v-if="c.isNew" class="rounded bg-brand-100 px-1.5 text-xs font-medium text-brand-800">{{ $t('nuevo') }}</span>
          <span class="min-w-0 flex-1 truncate text-sm text-stone-600">{{ c.species }}</span>
        </button>
        <ul class="divide-y divide-stone-100 border-t border-stone-100">
          <li v-for="(l, i) in c.lines" :key="`${l.field}:${i}`" class="px-3 py-1 leading-snug">
            <span class="flex items-baseline gap-2">
              <span class="min-w-0 flex-1 text-[11px] font-medium tracking-wide text-stone-500">{{ l.field }}</span>
              <span class="shrink-0 text-[11px] text-stone-500">{{ who(l.actors) }} · {{ when(l.at) }}</span>
            </span>
            <span class="block text-[15px] break-words tabular-nums">{{ changeText(l, formatSerial, $t('quitado')) }}</span>
            <span v-if="l.sources.includes('assistant')" class="text-[11px] text-violet-700">{{ $t('por el asistente') }}</span>
          </li>
          <li v-for="e in c.events" :key="e.id" class="flex items-baseline gap-2 bg-sky-50/60 px-3 py-1 text-sm">
            <span class="min-w-0 flex-1 break-words tabular-nums">{{ eventLine(e) }}</span>
            <span class="shrink-0 text-[11px] text-stone-500">{{ who([e.name || e.username || '']) }} · {{ when(e.createdAt) }}</span>
          </li>
        </ul>
      </li>
    </ul>
    <div v-if="session.canEdit && list" class="mt-4 rounded-xl border border-stone-200 bg-white p-3">
      <p class="text-sm text-stone-700">{{ $t('¿Ya lo copiaste al cuaderno? Márcalo para que la próxima lista empiece aquí (para todos).') }}</p>
      <div class="mt-2 flex flex-wrap gap-2">
        <button class="btn-primary h-11 px-4" :disabled="marking" @click="markUpTo(list.to)">
          <BookCheck :size="16" /> {{ $t('Cuaderno al día hasta ahora') }}
        </button>
        <button v-if="fromIso && !isDefault" class="btn h-11 px-3 text-sm" :disabled="marking" @click="markUpTo(fromIso)">
          {{ $t('Al día hasta «Desde»') }}
        </button>
      </div>
    </div>
  </div>
</template>
