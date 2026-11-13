<script setup lang="ts">
/**
 * Identificar's sex, after the species (team only): one line with the AI's call and its % (the in-hand line,
 * SexEstimate, or the general line, FieldSexEstimate), whose details (the scale, the cut-offs, what it was tested on)
 * open on a tap; and under it the person's own answer: «Sexo real» ♀ / ♂ and «Enviar», which sends the photo with that
 * sex (and the species the person chose above with «Es esta», if any) to the team's review through the contributions
 * outbox (lib/contribOutbox.ts, consent v2 the first time, «Deshacer» for a few seconds). No kind of photo is asked.
 * The person's buttons never show the AI's call, so the answer is not nudged.
 */
import { computed, onBeforeUnmount, ref, watch } from 'vue'
import { Check, ChevronDown, CloudOff, Send } from 'lucide-vue-next'
import SexEstimate, { type SexState } from './SexEstimate.vue'
import FieldSexEstimate from './FieldSexEstimate.vue'
import ConsentSheet from './ConsentSheet.vue'
import { locale } from '../../lib/i18n'
import { SEX_DEFAULT, sexShown } from '../../lib/sexModel'
import type { FieldSexState } from '../../lib/fieldSex'
import { acceptConsent, consentAccepted, UNDO_MS } from '../../lib/contribConfig'
import type { Payload } from '../../lib/contribOutbox'

export type Labels = Payload['labels']
export type Outcome = 'sent' | 'queued' | 'already' | 'refused' | 'full' | 'error'

const props = defineProps<{
  /** The in-hand line's state (SexEstimate), or null. */
  state: SexState | null
  /** The general line's state (FieldSexEstimate), shown when the in-hand line does not apply. */
  field: FieldSexState | null
  /** The person can send for training (team, VITE_CONTRIB, a photo on this phone). */
  canSend: boolean
  /** The species the person chose above (or null): sent with the sex. */
  species: Labels['species']
  userId: string | null
  contact?: string | null
  send?: (labels: Labels) => Promise<Outcome>
  undo?: () => Promise<void>
  refusal?: string | null
}>()
const emit = defineEmits<{ download: []; consented: [] }>()

// ---------------------------------------------------------------- the AI's line
const fmt = (n: number) => new Intl.NumberFormat(locale.value === 'es' ? 'es' : 'en', { maximumFractionDigits: 0 }).format(n)
const pct = (p: number) => (p >= 0.995 ? `> ${fmt(99)} %` : `${fmt(p * 100)} %`)
/** The in-hand estimate's call (as SexEstimate shows it). */
const inHand = computed(() => {
  const s = props.state
  if (s?.kind !== 'ready') return null
  return sexShown(s.z, s.calibration ?? SEX_DEFAULT)
})
/** The general line shown when the in-hand line does not apply (as the view chose before: genus «No aplica»). */
const useField = computed(() => !!props.field && (!props.state || props.state.kind === 'genus'))
const fieldNamed = computed(() => !!props.field && props.field.status !== 'cell_under_90' && props.field.sure)
/** female, male or null (not sure / none): the line's colour. */
const colour = computed<'female' | 'male' | null>(() => {
  if (useField.value) return fieldNamed.value ? props.field!.sex : null
  const c = inHand.value?.call
  return c === 'hembra' ? 'female' : c === 'macho' ? 'male' : null
})
const hasLine = computed(() => useField.value || !!props.state)
/** Details open on a tap (the scale, the notes): only for an answer or a «No aplica». */
const hasDetails = computed(() => useField.value || props.state?.kind === 'ready' || props.state?.kind === 'genus' || props.state?.kind === 'nowings')

// ---------------------------------------------------------------- the person's answer
const sex = ref<'female' | 'male' | null>(null)
const step = ref<'edit' | 'consent' | 'sending' | 'done'>('edit')
const outcome = ref<Outcome | null>(null)
const canUndo = ref(false)
let undoTimer: ReturnType<typeof setTimeout> | null = null
onBeforeUnmount(() => undoTimer && clearTimeout(undoTimer))
// A species chosen after a send: the person may send again with it (the outbox says if the photo went already).
watch(
  () => props.species?.name,
  () => step.value === 'done' && !canUndo.value && ((step.value = 'edit'), (outcome.value = null)),
)
const ready = computed(() => !!sex.value || !!props.species)

async function submit() {
  if (!ready.value || !props.send) return
  if (!consentAccepted(props.userId)) {
    step.value = 'consent'
    return
  }
  step.value = 'sending'
  outcome.value = await props.send({ species: props.species, sex: sex.value, photoKind: null })
  if (outcome.value === 'full' || outcome.value === 'error' || outcome.value === 'refused' || outcome.value === 'already') {
    step.value = 'edit'
    return
  }
  step.value = 'done'
  canUndo.value = true
  undoTimer = setTimeout(() => (canUndo.value = false), UNDO_MS)
}
function accept() {
  acceptConsent(props.userId)
  emit('consented')
  step.value = 'edit'
  submit()
}
async function undoNow() {
  canUndo.value = false
  await props.undo?.()
  step.value = 'edit'
  outcome.value = null
}
const seg = (on: boolean) => (on ? 'border-brand bg-brand text-on-brand' : 'border-control bg-surface text-fg hover:bg-surface-sunken')
</script>

<template>
  <section
    v-if="hasLine || canSend"
    class="rounded-md border border-border border-l-4 bg-surface"
    :class="colour === 'female' ? 'border-l-fill-pink' : colour === 'male' ? 'border-l-fill-sky' : 'border-l-border-strong'"
    data-testid="sex-panel"
    :aria-label="$t('Sexo')"
  >
    <details v-if="hasLine && hasDetails" class="sex-details group">
      <summary class="flex min-h-10 cursor-pointer list-none items-center gap-2 px-3 py-0.5" data-testid="sex-summary">
        <span class="text-sm text-fg-muted">{{ $t('Sexo') }}</span>
        <span class="text-lg font-semibold" :class="colour === 'female' ? 'text-fill-pink' : colour === 'male' ? 'text-fill-sky' : 'text-fg-muted'" data-testid="sex-line">
          <template v-if="useField">
            <template v-if="field!.status === 'cell_under_90'">{{ $t('Sin respuesta') }}</template>
            <template v-else-if="fieldNamed">{{ field!.sex === 'female' ? `♀ ${$t('Hembra')}` : `♂ ${$t('Macho')}` }}<template v-if="field!.pct != null"> {{ fmt(field!.pct) }} %</template></template>
            <template v-else>{{ $t('No seguro') }}</template>
          </template>
          <template v-else-if="inHand">
            <template v-if="inHand.call === 'hembra'">♀ {{ $t('Hembra') }} {{ pct(inHand.p) }}</template>
            <template v-else-if="inHand.call === 'macho'">♂ {{ $t('Macho') }} {{ pct(inHand.p) }}</template>
            <template v-else>{{ $t('No seguro') }}</template>
          </template>
          <span v-else class="text-sm font-normal">{{ state?.kind === 'genus' ? $t('No aplica a {genus}', { genus: (state as { genus: string }).genus }) : $t('No aplica: sin alas en la foto') }}</span>
        </span>
        <span class="ml-auto flex items-center gap-0.5 text-sm text-brand-text">{{ $t('Detalles') }}<ChevronDown :size="16" class="transition-transform group-open:rotate-180" /></span>
      </summary>
      <div class="px-3 pb-3">
        <FieldSexEstimate v-if="useField" :state="field!" bare />
        <SexEstimate v-else-if="state" :state="state" bare @download="emit('download')" />
      </div>
    </details>
    <div v-else-if="hasLine && state" class="px-3 py-1">
      <SexEstimate :state="state" bare class="sex-plain" @download="emit('download')" />
    </div>

    <div v-if="canSend" class="px-3 py-1.5" :class="hasLine ? 'border-t border-border' : ''" data-testid="sex-send">
      <template v-if="step === 'done'">
        <p class="flex min-h-11 flex-wrap items-center gap-x-2 gap-y-1 text-sm" role="status" data-testid="contrib-done">
          <template v-if="outcome === 'sent'"><Check :size="16" class="text-success-text" />{{ $t('Enviada. Gracias: la revisa el equipo.') }}</template>
          <template v-else><CloudOff :size="16" class="text-pending-text" />{{ $t('Guardada en el teléfono: se enviará cuando haya señal.') }}</template>
          <button v-if="canUndo" class="min-h-11 px-1 text-brand-text underline-offset-2 hover:underline" data-testid="contrib-undo" @click="undoNow">{{ $t('Deshacer') }}</button>
        </p>
      </template>
      <ConsentSheet v-else-if="step === 'consent'" class="py-1" :contact="contact ?? null" @accept="accept" @later="step = 'edit'" />
      <template v-else>
        <div class="flex flex-wrap items-center gap-2">
          <span class="text-sm">{{ $t('Sexo real') }}</span>
          <div class="flex gap-1.5" role="radiogroup" :aria-label="$t('Sexo real')">
            <button role="radio" :aria-checked="sex === 'female'" :aria-label="$t('Hembra')" :title="$t('Hembra')" class="grid size-11 place-items-center rounded-md border text-xl" :class="seg(sex === 'female')" data-testid="sex-female" @click="sex = sex === 'female' ? null : 'female'">♀</button>
            <button role="radio" :aria-checked="sex === 'male'" :aria-label="$t('Macho')" :title="$t('Macho')" class="grid size-11 place-items-center rounded-md border text-xl" :class="seg(sex === 'male')" data-testid="sex-male" @click="sex = sex === 'male' ? null : 'male'">♂</button>
          </div>
          <button class="btn-primary ml-auto min-h-11 px-4" :disabled="!ready || step === 'sending'" data-testid="contrib-send" @click="submit">
            <Send :size="16" />{{ step === 'sending' ? $t('Enviando…') : $t('Enviar') }}
          </button>
        </div>
        <p v-if="species" class="mt-1 text-xs text-fg-muted" data-testid="send-species">{{ $t('Con la especie') }} <i>{{ species.name }}</i></p>
        <p v-if="outcome === 'full'" class="mt-1 text-sm text-pending-text" role="alert">{{ $t('El teléfono tiene 50 envíos esperando señal; se enviarán antes de guardar más.') }}</p>
        <p v-else-if="outcome === 'error'" class="mt-1 text-sm text-error-text" role="alert">{{ $t('No se pudo preparar la foto para enviarla.') }}</p>
        <p v-else-if="outcome === 'refused'" class="mt-1 text-sm text-error-text" role="alert" data-testid="contrib-refused">{{ $t('El servidor no aceptó la foto: {reason}', { reason: refusal ?? '' }) }}</p>
        <p v-else-if="outcome === 'already'" class="mt-1 text-sm text-pending-text" role="status" data-testid="contrib-already">
          {{ $t('Esta foto ya se había enviado, con las etiquetas de entonces. Para cambiarlas, bórrela en «Mis envíos» y envíela de nuevo.') }}
        </p>
      </template>
    </div>
  </section>
</template>

<style scoped>
.sex-details summary::-webkit-details-marker {
  display: none;
}
</style>
