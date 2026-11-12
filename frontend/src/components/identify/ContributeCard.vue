<script setup lang="ts">
/**
 * «Ayude a entrenar la IA» on Identificar's result (DESIGN.md §2): the person confirms the species or names another,
 * marks Hembra / Macho / No sé and the kind of photo, and sends it to the team. The first time (and once more after
 * the consent's version changed), the consent sheet (ConsentSheet.vue). No location is offered or sent. The view keeps
 * the item (lib/contribOutbox.ts) and says whether it went or waits for signal; «Deshacer» for UNDO_MS after. Team
 * only, behind VITE_CONTRIB (lib/contribConfig.ts).
 * The card never shows the sex model's call next to the buttons, so the person is not nudged.
 */
import { computed, onBeforeUnmount, ref, watch } from 'vue'
import { Check, CloudOff, HeartHandshake } from 'lucide-vue-next'
import TaxonPick from './TaxonPick.vue'
import ConsentSheet from './ConsentSheet.vue'
import { acceptConsent, consentAccepted, UNDO_MS } from '../../lib/contribConfig'
import type { Payload } from '../../lib/contribOutbox'
import type { TaxonHit, TaxonSource } from '../../lib/taxonPick'

export type Labels = Payload['labels']
export type Outcome = 'sent' | 'queued' | 'already' | 'refused' | 'full' | 'error'

const props = defineProps<{
  /** The name the result shows: a species, or a genus when the species is not sure. */
  shown: { name: string; rank: 'genus' | 'species' }
  /** The person's «Se parece a esta», when it is not the shown one: the card opens on «Es otra especie» with it. */
  pick: string | null
  source: TaxonSource
  /** The address the consent gives for deletions and questions (the server's config), or null. */
  contact?: string | null
  send: (labels: Labels) => Promise<Outcome>
  undo: () => Promise<void>
  /** The server's reason when it refused the item (`refused`). */
  refusal?: string | null
}>()
const emit = defineEmits<{ consented: [] }>()

const speciesAnswer = ref<'yes' | 'other' | null>(props.pick && props.pick !== props.shown.name ? 'other' : null)
const other = ref(props.pick && props.pick !== props.shown.name ? props.pick : '')
const otherHit = ref<TaxonHit | null>(props.pick ? (props.source.search(props.pick, 1).find(h => h.name === props.pick) ?? null) : null)
// «Se parece a esta» chosen after the card opened: the species line follows it while the person has not answered it.
watch(
  () => props.pick,
  (now, before) => {
    if (step.value !== 'edit') return
    const untouched = speciesAnswer.value === null || (speciesAnswer.value === 'other' && other.value === (before ?? ''))
    if (!untouched) return
    if (now && now !== props.shown.name) {
      speciesAnswer.value = 'other'
      other.value = now
      otherHit.value = props.source.search(now, 1).find(h => h.name === now) ?? null
    } else if (speciesAnswer.value === 'other') {
      speciesAnswer.value = null
      other.value = ''
    }
  },
)
const sex = ref<'female' | 'male' | 'unknown' | null>(null)
const kind = ref<'hand' | 'live' | 'collection' | null>(null)
const step = ref<'edit' | 'consent' | 'sending' | 'done'>('edit')
const outcome = ref<Outcome | null>(null)
const canUndo = ref(false)
let undoTimer: ReturnType<typeof setTimeout> | null = null
onBeforeUnmount(() => undoTimer && clearTimeout(undoTimer))

const species = computed<Labels['species']>(() => {
  if (speciesAnswer.value === 'yes') return { name: props.shown.name, rank: props.shown.rank, confirmed: true, freeText: false }
  const name = other.value.trim().replace(/\s+/g, ' ')
  if (speciesAnswer.value !== 'other' || name.length < 3) return null
  const hit = otherHit.value?.name === name ? otherHit.value : props.source.search(name, 1).find(h => h.name.toLowerCase() === name.toLowerCase())
  const words = name.split(' ').length
  return { name: hit?.name ?? name, rank: hit?.rank ?? (words >= 3 ? 'subspecies' : words === 2 ? 'species' : 'genus'), confirmed: false, freeText: !hit }
})
const sexLabel = computed(() => (sex.value === 'female' || sex.value === 'male' ? sex.value : null))
/** What keeps Enviar off, said under it. */
const missing = computed(() => {
  if (speciesAnswer.value === 'other' && !species.value) return 'Escriba el nombre de la especie.'
  if (!species.value && !sexLabel.value) return 'Indique la especie o el sexo.'
  if (sexLabel.value && !kind.value) return 'Indique qué tipo de foto es.'
  return null
})

async function submit() {
  if (missing.value) return
  if (!consentAccepted()) {
    step.value = 'consent'
    return
  }
  step.value = 'sending'
  outcome.value = await props.send({ species: species.value, sex: sexLabel.value, photoKind: kind.value })
  if (outcome.value === 'full' || outcome.value === 'error' || outcome.value === 'refused' || outcome.value === 'already') {
    step.value = 'edit'
    return
  }
  step.value = 'done'
  canUndo.value = true
  undoTimer = setTimeout(() => (canUndo.value = false), UNDO_MS)
}
function accept() {
  acceptConsent()
  // Items this phone queued under an older version go now too (the view lets them out of the outbox).
  emit('consented')
  step.value = 'edit'
  submit()
}
async function undoNow() {
  canUndo.value = false
  await props.undo()
  step.value = 'edit'
  outcome.value = null
}

const SEXES = [
  { value: 'female', label: '♀ Hembra' },
  { value: 'male', label: '♂ Macho' },
  { value: 'unknown', label: 'No sé' },
] as const
const KINDS = [
  { value: 'hand', label: 'En la mano' },
  { value: 'live', label: 'Viva' },
  { value: 'collection', label: 'Colección' },
] as const
const seg = (on: boolean) => (on ? 'border-brand bg-brand text-on-brand' : 'border-control bg-surface text-fg hover:bg-stone-100')
</script>

<template>
  <section class="mt-4 rounded-md border border-border bg-surface p-3" data-testid="contribute" :aria-label="$t('Ayude a entrenar la IA')">
    <template v-if="step === 'done'">
      <p class="flex flex-wrap items-center gap-x-2 gap-y-1 text-sm" role="status" data-testid="contrib-done">
        <template v-if="outcome === 'sent'"><Check :size="16" class="text-success-text" />{{ $t('Enviada. Gracias: la revisa el equipo.') }}</template>
        <template v-else><CloudOff :size="16" class="text-pending-text" />{{ $t('Guardada en el teléfono: se enviará cuando haya señal.') }}</template>
        <button v-if="canUndo" class="text-brand-text underline-offset-2 hover:underline" data-testid="contrib-undo" @click="undoNow">{{ $t('Deshacer') }}</button>
      </p>
    </template>

    <ConsentSheet v-else-if="step === 'consent'" :contact="contact ?? null" @accept="accept" @later="step = 'edit'" />

    <template v-else>
      <h2 class="flex items-center gap-2 font-semibold"><HeartHandshake :size="18" class="text-brand-text" />{{ $t('Ayude a entrenar la IA') }}</h2>

      <p class="mt-2 text-sm">
        <template v-if="shown.rank === 'genus'">{{ $t('¿Es del género') }} <i>{{ shown.name }}</i>?</template>
        <template v-else>{{ $t('¿Es') }} <i>{{ shown.name }}</i>?</template>
      </p>
      <div class="mt-1 flex flex-wrap gap-2" role="radiogroup" :aria-label="$t('Especie')">
        <button role="radio" :aria-checked="speciesAnswer === 'yes'" class="min-h-11 rounded-md border px-4 text-sm" :class="seg(speciesAnswer === 'yes')" data-testid="species-yes" @click="speciesAnswer = speciesAnswer === 'yes' ? null : 'yes'">
          {{ $t('Sí, es esta') }}
        </button>
        <button role="radio" :aria-checked="speciesAnswer === 'other'" class="min-h-11 rounded-md border px-4 text-sm" :class="seg(speciesAnswer === 'other')" data-testid="species-other" @click="speciesAnswer = speciesAnswer === 'other' ? null : 'other'">
          {{ $t('Es otra especie') }}
        </button>
      </div>
      <TaxonPick v-if="speciesAnswer === 'other'" v-model="other" :source="source" class="mt-2" @pick="h => (otherHit = h)" />

      <p class="mt-3 text-sm">{{ $t('Sexo') }}</p>
      <div class="mt-1 flex flex-wrap gap-2" role="radiogroup" :aria-label="$t('Sexo')">
        <button v-for="s in SEXES" :key="s.value" role="radio" :aria-checked="sex === s.value" class="min-h-11 rounded-md border px-4 text-sm" :class="seg(sex === s.value)" :data-testid="`sex-${s.value}`" @click="sex = sex === s.value ? null : s.value">
          {{ $t(s.label) }}
        </button>
      </div>

      <p class="mt-3 text-sm">{{ $t('Tipo de foto') }}</p>
      <div class="mt-1 flex flex-wrap gap-2" role="radiogroup" :aria-label="$t('Tipo de foto')">
        <button v-for="k in KINDS" :key="k.value" role="radio" :aria-checked="kind === k.value" class="min-h-11 rounded-md border px-4 text-sm" :class="seg(kind === k.value)" :data-testid="`kind-${k.value}`" @click="kind = kind === k.value ? null : k.value">
          {{ $t(k.label) }}
        </button>
      </div>

      <button class="btn-primary mt-3 min-h-11 w-full text-base" :disabled="!!missing || step === 'sending'" data-testid="contrib-send" @click="submit">
        {{ step === 'sending' ? $t('Enviando…') : $t('Enviar al equipo') }}
      </button>
      <p v-if="missing" class="mt-1 text-xs text-fg-subtle">{{ $t(missing) }}</p>
      <p v-if="outcome === 'full'" class="mt-1 text-sm text-pending-text" role="alert">{{ $t('El teléfono tiene 50 envíos esperando señal; se enviarán antes de guardar más.') }}</p>
      <p v-else-if="outcome === 'error'" class="mt-1 text-sm text-error-text" role="alert">{{ $t('No se pudo preparar la foto para enviarla.') }}</p>
      <p v-else-if="outcome === 'refused'" class="mt-1 text-sm text-error-text" role="alert" data-testid="contrib-refused">{{ $t('El servidor no aceptó la foto: {reason}', { reason: refusal ?? '' }) }}</p>
      <p v-else-if="outcome === 'already'" class="mt-1 text-sm text-pending-text" role="status" data-testid="contrib-already">
        {{ $t('Esta foto ya se había enviado, con las etiquetas de entonces. Para cambiarlas, bórrela en «Mis envíos» y envíela de nuevo.') }}
      </p>
      <p class="mt-1 text-xs text-fg-subtle">{{ $t('Se revisa antes de usarse. Puede borrarla después en «Mis envíos».') }}</p>
    </template>
  </section>
</template>
