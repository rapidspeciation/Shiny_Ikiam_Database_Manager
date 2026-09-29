<script setup lang="ts">
import { computed, ref } from 'vue'
import { Check, History, Layers, PenLine, RotateCw, Undo2, X } from 'lucide-vue-next'
import ChoiceField from '../ChoiceField.vue'
import CropImage from './CropImage.vue'
import { api } from '../../lib/api'
import { errorText, notify } from '../../lib/notice'
import {
  STRENGTH_HINT,
  VERDICT_WORD,
  differingCells,
  fixText,
  otherField,
  percent,
  photoList,
  photoUrl,
  shown,
  type Issue,
  type Verdict,
} from '../../lib/review'

/**
 * One inconsistency: the problem, the rows side by side (differing cells in
 * red), the specimen's photos with its envelope cut out, what was read and
 * what the sheet says, the gallery's prediction, the proposed fix, and the
 * verdict buttons with who decided.
 */
const props = defineProps<{ issue: Issue; kindLabel: string; canEdit: boolean; busy: boolean }>()
const emit = defineEmits<{
  verdict: [issue: Issue, verdict: string, value?: string, comment?: string]
  batch: [issue: Issue, verdict: string]
  group: [key: string]
  photos: [list: { id: string; name: string }[], index: number]
  open: [sheet: string, label: string]
}>()

const comment = ref('')
const other = ref(false)
const otherValue = ref('')
const history = ref<Verdict[] | null>(null)
/** The reader sometimes turns an envelope that was upright: a person can turn it back. */
const flipped = ref(false)
const i = computed(() => props.issue)
const cells = computed(() => (i.value.table ? differingCells(i.value.table, i.value.field) : new Set<string>()))
const photos = computed(() => photoList(i.value.photos))
const envelope = computed(() => i.value.photos?.envelope)
/** The envelope opens full size with the card's other photos. */
const viewerList = computed(() => {
  const list = photos.value.map(p => ({ id: p.id, name: p.name }))
  const env = envelope.value
  return env && !list.some(p => p.id === env.fileId) ? [{ id: env.fileId, name: env.name }, ...list] : list
})
const openPhoto = (id: string) =>
  emit(
    'photos',
    viewerList.value,
    Math.max(
      0,
      viewerList.value.findIndex(p => p.id === id),
    ),
  )
const relatedList = computed(() => photoList(i.value.relatedPhotos))
const acceptLabel = computed(() => (i.value.fix ? 'Aceptar arreglo' : i.value.task ? 'Aceptar tarea' : 'Es un problema'))
const done = computed(() => i.value.verdict?.verdict === 'applied' || i.value.resolved)
const strengthClass: Record<string, string> = {
  fuerte: 'bg-emerald-100 text-emerald-900',
  media: 'bg-amber-100 text-amber-900',
  baja: 'bg-stone-200 text-stone-700',
  dudosa: 'bg-stone-200 text-stone-700',
}
const verdictClass: Record<string, string> = {
  accepted: 'bg-emerald-100 text-emerald-900',
  other: 'bg-sky-100 text-sky-900',
  rejected: 'bg-stone-200 text-stone-700',
  applied: 'bg-brand-100 text-brand-800',
  pending: 'bg-stone-100 text-stone-600',
}
const when = (at: string) =>
  new Date(at).toLocaleString('es-EC', { day: '2-digit', month: '2-digit', hour: '2-digit', minute: '2-digit' })

function send(verdict: string) {
  if (verdict === 'other' && !otherValue.value.trim()) return notify('Escribe el valor correcto', 'error')
  emit('verdict', i.value, verdict, verdict === 'other' ? otherValue.value.trim() : undefined, comment.value.trim() || undefined)
  comment.value = ''
  other.value = false
}
async function toggleHistory() {
  if (history.value) return (history.value = null)
  try {
    history.value = (await api<{ history: Verdict[] }>(`review/verdicts?issueId=${encodeURIComponent(i.value.id)}`)).history
  } catch (e) {
    notify(errorText(e), 'error')
  }
}
</script>

<template>
  <article class="rounded-lg border border-stone-200 bg-white p-3 shadow-sm md:p-4">
    <header class="flex flex-wrap items-center gap-x-2 gap-y-1 text-sm">
      <span class="rounded bg-stone-800 px-1.5 py-0.5 text-xs font-medium text-white">{{ kindLabel }}</span>
      <span
        v-if="i.strength"
        class="rounded px-1.5 py-0.5 text-xs font-medium"
        :class="strengthClass[i.strength]"
        :title="STRENGTH_HINT[i.strength]"
      >
        lectura {{ i.strength }}
      </span>
      <strong class="font-semibold">{{ i.cam || i.label }}</strong>
      <button
        v-if="i.row && !i.resolved"
        class="text-brand-700 hover:underline"
        title="Abrir la fila en Tablas"
        @click="emit('open', i.sheet, i.label)"
      >
        {{ i.sheet }} fila {{ i.row }}
      </button>
      <span v-else class="text-stone-500">{{ i.sheet }}</span>
      <span v-if="i.date" class="text-xs text-stone-500">{{ i.date }}</span>
      <span v-if="i.who?.length" class="truncate text-xs text-stone-500">{{ i.who.join(' · ') }}</span>
      <span
        v-if="i.verdict"
        class="ml-auto rounded px-1.5 py-0.5 text-xs font-medium"
        :class="verdictClass[i.verdict.verdict]"
        :title="i.verdict.comment ?? undefined"
      >
        {{ VERDICT_WORD[i.verdict.verdict] }}{{ i.verdict.verdict === 'other' ? `: ${i.verdict.value}` : '' }} ·
        {{ i.verdict.user }}
      </span>
    </header>
    <p class="mt-1.5 text-sm text-stone-900">{{ i.problem }}</p>

    <div class="mt-3 grid gap-4 md:grid-cols-[minmax(0,1fr)_minmax(16rem,24rem)]">
      <!-- Photos: first on phones, full width; beside the details on a computer. -->
      <div v-if="envelope || photos.length || relatedList.length" class="space-y-2 md:order-last">
        <figure v-if="envelope">
          <button class="block w-full cursor-zoom-in" title="Ver la foto completa" @click="openPhoto(envelope.fileId)">
            <CropImage
              :src="photoUrl(envelope.fileId, 1600)"
              :alt="`Sobre en ${envelope.name}`"
              :box="envelope.bbox"
              :aspect="envelope.aspect"
              :turned="(envelope.turned + (flipped ? 180 : 0)) % 360"
              class="mx-auto max-h-96 w-full"
            />
          </button>
          <figcaption class="mt-0.5 flex items-center gap-2 text-xs text-stone-500">
            Sobre (recorte de {{ envelope.name }}{{ envelope.turned ? ', girado por el lector' : '' }})
            <button
              class="ml-auto inline-flex items-center gap-1 text-brand-700 hover:underline"
              title="Girar el recorte 180°"
              @click="flipped = !flipped"
            >
              <RotateCw :size="12" /> girar
            </button>
          </figcaption>
        </figure>
        <div v-if="photos.length" class="grid grid-cols-2 gap-2">
          <figure v-for="p in photos" :key="p.id">
            <button class="block w-full cursor-zoom-in" :title="`${p.name}: ver completa`" @click="openPhoto(p.id)">
              <CropImage :src="photoUrl(p.id)" :alt="p.name" :box="p.wings" />
            </button>
            <figcaption class="mt-0.5 truncate text-xs text-stone-500">{{ p.name }}</figcaption>
          </figure>
        </div>
        <div v-if="relatedList.length" class="rounded border border-dashed border-stone-300 p-2">
          <p class="mb-1 text-xs text-stone-600">Fotos de {{ i.relatedPhotos?.cam }}, para comparar</p>
          <div class="grid grid-cols-3 gap-1.5">
            <button
              v-for="(p, n) in relatedList"
              :key="p.id"
              class="block cursor-zoom-in"
              :title="p.name"
              @click="emit('photos', relatedList, n)"
            >
              <CropImage :src="photoUrl(p.id)" :alt="p.name" :box="p.wings" />
            </button>
          </div>
        </div>
      </div>

      <div class="min-w-0 space-y-3">
        <!-- The rows involved side by side, the cells that disagree in red. -->
        <div v-if="i.table?.rows.length" class="overflow-x-auto">
          <table class="w-full border-collapse text-xs">
            <thead class="bg-stone-50 text-left text-stone-600">
              <tr>
                <th class="border-b border-stone-200 px-1.5 py-1 font-medium">Fila</th>
                <th
                  v-for="f in i.table.fields"
                  :key="f"
                  class="border-b border-stone-200 px-1.5 py-1 font-medium whitespace-nowrap"
                >
                  {{ f }}
                </th>
              </tr>
            </thead>
            <tbody>
              <tr v-for="(r, n) in i.table.rows" :key="r.recordId">
                <td class="border-b border-stone-100 px-1.5 py-1 whitespace-nowrap">
                  <button class="text-brand-700 hover:underline" @click="emit('open', r.sheet, r.label)">
                    {{ r.sheet === i.sheet ? '' : `${r.sheet} ` }}{{ r.row }}
                  </button>
                </td>
                <td
                  v-for="f in i.table.fields"
                  :key="f"
                  class="border-b border-stone-100 px-1.5 py-1"
                  :class="cells.has(`${n}:${f}`) ? 'bg-red-50 font-medium text-red-900' : ''"
                >
                  {{ shown(r.values[f]) }}
                </td>
              </tr>
            </tbody>
          </table>
        </div>

        <!-- What the envelope (or the model) says against the sheet. -->
        <dl
          v-if="i.ocr || i.envelopeCamid || i.envelopeText"
          class="grid grid-cols-[auto_minmax(0,1fr)] gap-x-3 gap-y-1 rounded bg-stone-50 p-2 text-xs"
        >
          <template v-if="i.ocr">
            <dt class="text-stone-500">Sobre dice</dt>
            <dd class="font-medium text-sky-900">{{ i.ocr.read }}</dd>
            <dt class="text-stone-500">Hoja dice</dt>
            <dd class="font-medium text-red-900">{{ i.ocr.sheet }}</dd>
          </template>
          <template v-if="i.envelopeCamid">
            <dt class="text-stone-500">CAM leído</dt>
            <dd :class="i.envelopeCamid !== i.cam ? 'font-medium text-red-900' : ''">
              {{ i.envelopeCamid
              }}<span v-if="i.cam && i.envelopeCamid !== i.cam" class="text-stone-500"> (archivo {{ i.cam }})</span>
            </dd>
          </template>
          <template v-if="i.envelopeText">
            <dt class="text-stone-500">Texto del sobre</dt>
            <dd class="break-words text-stone-700">{{ i.envelopeText }}</dd>
          </template>
          <template v-if="i.curation?.decision || i.curation?.decidedBy">
            <dt class="text-stone-500">Curaduría</dt>
            <dd class="text-stone-700">
              {{ [i.curation.decision, i.curation.note, i.curation.decidedBy].filter(Boolean).join(' · ') }}
            </dd>
          </template>
        </dl>

        <!-- The Wings Gallery's model, for species issues. -->
        <div
          v-if="
            i.prediction?.species.length &&
            (i.kind === 'ai_species' || i.kind === 'envelope_species' || i.kind === 'link_mismatch')
          "
          class="rounded border border-stone-200 p-2 text-xs"
        >
          <p class="mb-1 flex items-center gap-2 font-medium text-stone-700">
            IA de la galería (fotos)
            <span v-if="i.ai" class="rounded bg-amber-100 px-1.5 py-0.5 text-amber-900"
              >distinta de la hoja: {{ i.ai.recorded }}</span
            >
          </p>
          <div v-for="[name, conf] in i.prediction.species" :key="name" class="flex items-center gap-2">
            <span class="w-44 truncate">{{ name }}</span>
            <span class="h-2 flex-1 rounded bg-stone-100">
              <span class="block h-2 rounded bg-brand-600" :style="{ width: percent(conf).replace(' ', '') }" />
            </span>
            <span class="w-10 text-right tabular-nums text-stone-600">{{ percent(conf) }}</span>
          </div>
          <p v-if="i.prediction.sex" class="mt-1 text-stone-600">
            Sexo: {{ i.prediction.sex.sex === 'male' ? 'macho' : 'hembra' }} · {{ percent(i.prediction.sex.confidence) }}
            {{ i.prediction.sex.supported ? '· respaldado' : '· incierto' }}
          </p>
        </div>

        <p v-if="fixText(i)" class="rounded bg-emerald-50 px-2 py-1.5 text-sm text-emerald-900">
          <span class="font-medium">{{ i.task ? 'Tarea (en Drive, no en la hoja)' : 'Arreglo propuesto' }}:</span>
          {{ fixText(i) }}
        </p>
        <p v-else-if="!i.resolved" class="text-xs text-stone-500">
          Sin arreglo obvio: si es un problema, da el valor correcto con «Otro valor».
        </p>

        <!-- A batch (e.g. one day's envelopes with the same species): judged together after looking. -->
        <div
          v-if="i.group && i.group.size > 1 && !i.resolved"
          class="flex flex-wrap items-center gap-2 rounded bg-stone-50 px-2 py-1.5 text-xs"
        >
          <Layers :size="14" class="text-stone-500" />
          <span>Lote «{{ i.group.label }}»: {{ i.group.size }}</span>
          <button class="text-brand-700 hover:underline" @click="emit('group', i.group.key)">ver solo el lote</button>
          <template v-if="canEdit">
            <button class="btn px-2 py-0.5 text-xs" :disabled="busy" @click="emit('batch', i, 'accepted')">
              Aceptar los {{ i.group.size }}
            </button>
            <button class="btn px-2 py-0.5 text-xs" :disabled="busy" @click="emit('batch', i, 'rejected')">
              Rechazar los {{ i.group.size }}
            </button>
          </template>
        </div>

        <p v-if="i.verdict" class="text-xs text-stone-600">
          {{ VERDICT_WORD[i.verdict.verdict] }}{{ i.verdict.verdict === 'other' ? ` (${i.verdict.value})` : '' }} por
          <strong>{{ i.verdict.user }}</strong> · {{ when(i.verdict.at) }}{{ i.verdict.comment ? ` — ${i.verdict.comment}` : '' }}
          <button class="ml-1 inline-flex items-center gap-0.5 text-brand-700 hover:underline" @click="toggleHistory">
            <History :size="12" /> historial
          </button>
        </p>
        <ul v-if="history" class="space-y-0.5 border-l-2 border-stone-200 pl-2 text-xs text-stone-600">
          <li v-for="(h, n) in history" :key="n">
            {{ when(h.at) }} · {{ h.user }}: {{ VERDICT_WORD[h.verdict] }}{{ h.value ? ` (${h.value})` : ''
            }}{{ h.comment ? ` — ${h.comment}` : '' }}
          </li>
        </ul>

        <div v-if="canEdit && !done" class="flex flex-wrap items-center gap-2">
          <button class="btn-primary" :disabled="busy" @click="send('accepted')"><Check :size="15" /> {{ acceptLabel }}</button>
          <button class="btn" :disabled="busy" @click="send('rejected')"><X :size="15" /> Rechazar</button>
          <button class="btn" :class="{ 'bg-stone-100': other }" :disabled="busy" @click="other = !other">
            <PenLine :size="15" /> Otro valor
          </button>
          <button
            v-if="i.task && i.verdict && i.verdict.verdict !== 'rejected'"
            class="btn"
            :disabled="busy"
            @click="send('applied')"
          >
            Marcar hecho
          </button>
          <button v-if="i.verdict" class="btn-ghost" title="Volver a pendiente" :disabled="busy" @click="send('pending')">
            <Undo2 :size="15" />
          </button>
          <input
            v-model="comment"
            class="field-input min-w-40 flex-1 py-1 text-xs"
            placeholder="Comentario (opcional)"
            maxlength="500"
          />
        </div>
        <div v-if="other && canEdit" class="flex flex-wrap items-end gap-2 rounded bg-sky-50 p-2">
          <label class="min-w-48 flex-1">
            <span class="field-label">{{ otherField(i) }} correcto</span>
            <ChoiceField v-model="otherValue" class="field-input" :options="i.choices ?? []" placeholder="Escribe el valor" />
          </label>
          <button class="btn-primary" :disabled="busy" @click="send('other')">Guardar</button>
        </div>
      </div>
    </div>
  </article>
</template>
