<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import DiffView from './DiffView.vue'
import { act, type DocChange, type DocCheck } from '../../lib/docReview'
import { commitTime } from '../../lib/instructions'
import { renderMarkdown } from '../../lib/markdown'
import { errorText, notify } from '../../lib/notice'
import { t } from '../../lib/i18n'
import { ApiError } from '../../lib/api'

/**
 * A change Claude proposes to one section («cambio de documento»): its word diff or the text after it,
 * the versions (v1, v2…), and for admins Aplicar, Descartar, Deshacer; anyone comments on it.
 */
const props = defineProps<{ change: DocChange; admin: boolean; active?: boolean }>()
const emit = defineEmits<{ activate: []; section: []; asked: [chat: string | null] }>()

const view = ref<'diff' | 'after'>('diff')
const picked = ref<number | null>(null)
watch(
  () => props.change.version,
  () => (picked.value = null),
)
const shownVersion = computed(() => props.change.versions.find(v => v.version === (picked.value ?? props.change.version)) ?? props.change.versions.at(-1)!)
const older = computed(() => props.change.versions.length > 1)
const diff = computed(() => (shownVersion.value.diff ?? '').split('\n').filter(l => !l.startsWith('@@')).join('\n'))
const after = computed(() => renderMarkdown(shownVersion.value.after).html)
const pending = computed(() => props.change.status === 'pending')
const draft = ref('')
const busy = ref(false)
const refused = ref<DocCheck[]>([])
const file = computed(() => props.change.path.replace(/^assistant\//, ''))

const checkText = (c: DocCheck) =>
  c.code === 'front_matter'
    ? t('El encabezado de la habilidad ya no es válido ({file})', { file: c.file ?? '' })
    : c.code === 'placeholder'
      ? t('Marcador desconocido: {text}', { text: c.text ?? '' })
      : c.code === 'guide_link'
        ? t('SKILL.md ya no enlaza {file}', { file: c.file ?? '' })
        : c.code === 'route'
          ? t('La ruta #/{route} no existe en la app', { route: c.route ?? '' })
          : c.code === 'stale'
            ? t('La sección cambió')
            : t('La comprobación falló: {text}', { text: c.text ?? '' })
const checks = computed(() => [...props.change.checks, ...refused.value])
const grows = computed(() => props.change.budget)
const gitState = computed(() => props.change.revision?.gitState ?? null)

async function run(fn: () => Promise<unknown>) {
  busy.value = true
  try {
    await fn()
  } catch (e) {
    const details = e instanceof ApiError ? (e.details as { checks?: DocCheck[] } | null) : null
    if (details?.checks) refused.value = details.checks
    else notify(errorText(e), 'error')
  } finally {
    busy.value = false
  }
}
const apply = (anyway = false) => run(() => act(`changes/${props.change.seriesId}/apply`, 'POST', { anyway }))
const discard = () => run(() => act(`changes/${props.change.seriesId}/discard`))
const undo = () =>
  run(async () => {
    const out = await act<{ proposed?: unknown }>(`changes/${props.change.seriesId}/undo`)
    if (out.proposed) notify(t('La sección cambió desde entonces: el deshacer queda como cambio propuesto.'))
  })
const retry = () => run(() => act(`revisions/${props.change.revision!.id}/retry`))
const repropose = () => run(() => act(`changes/${props.change.seriesId}/repropose`))
const comment = () =>
  run(async () => {
    if (!draft.value.trim()) return
    await act(`changes/${props.change.seriesId}/comments`, 'POST', { body: draft.value })
    draft.value = ''
  })
/** «Pedir versión nueva» (with the comment) and «Pedir a Claude que la rehaga» (a stale change): to its chat. */
const askAgain = (why: 'comment' | 'stale') =>
  run(async () => {
    const out = await act<{ chat: string | null; sent: boolean; prompt?: string | null }>(`changes/${props.change.seriesId}/revise`, 'POST', {
      body: why === 'comment' ? draft.value : '',
      why,
    })
    draft.value = ''
    if (out.sent) notify(t('Enviado a Claude'), 'success')
    else if (out.prompt) {
      await navigator.clipboard?.writeText(out.prompt).catch(() => {})
      notify(t('T3 no está disponible: el pedido está copiado para pegarlo en un chat.'))
    }
    emit('asked', out.chat)
  })
</script>

<template>
  <article
    class="doc-card doc-change-edge rounded-lg border bg-surface p-3 text-sm"
    :class="active ? 'doc-card-active border-brand-300' : 'border-stone-200'"
    :data-card="change.seriesId"
    @click="emit('activate')"
  >
    <header class="flex flex-wrap items-baseline gap-x-2 gap-y-1">
      <span class="doc-claude-tag">Claude</span>
      <button type="button" class="min-w-0 truncate text-left font-medium text-stone-800 hover:underline" @click.stop="emit('section')">
        § {{ change.heading || $t('Inicio') }}
      </button>
      <span v-if="change.path.includes('/voice/')" class="font-mono text-xs text-stone-500">{{ file }}</span>
      <span class="ml-auto flex items-center gap-1 text-xs">
        <template v-if="older">
          <button
            v-for="v in change.versions"
            :key="v.id"
            type="button"
            class="rounded px-1"
            :class="v.version === shownVersion.version ? 'bg-stone-200 text-stone-800' : 'text-stone-500 hover:text-stone-800'"
            @click.stop="picked = v.version === change.version ? null : v.version"
          >
            v{{ v.version }}
          </button>
        </template>
      </span>
    </header>

    <p v-if="change.stale" class="mt-1 text-xs font-medium text-amber-800">{{ $t('Desactualizada') }}</p>
    <p v-else-if="change.lost" class="mt-1 text-xs font-medium text-amber-800">{{ $t('No está en la versión desplegada') }}</p>
    <p v-else-if="change.status === 'applied'" class="mt-1 text-xs text-stone-500">
      {{ $t('Aplicado por {name} · {date}', { name: change.decidedBy ?? '', date: change.decidedAt ? commitTime(change.decidedAt) : '' }) }}
    </p>
    <p v-else-if="change.status === 'undone'" class="mt-1 text-xs text-stone-500">{{ $t('Deshecho') }}</p>

    <div class="mt-2 inline-flex rounded-md border border-stone-300 p-0.5 text-xs" role="tablist" @click.stop>
      <button role="tab" type="button" class="rounded px-2 py-0.5" :class="view === 'diff' ? 'bg-stone-200 text-stone-900' : 'text-stone-600'" :aria-selected="view === 'diff'" @click="view = 'diff'">
        {{ $t('Cambios') }}
      </button>
      <button role="tab" type="button" class="rounded px-2 py-0.5" :class="view === 'after' ? 'bg-stone-200 text-stone-900' : 'text-stone-600'" :aria-selected="view === 'after'" @click="view = 'after'">
        {{ $t('Después') }}
      </button>
    </div>
    <div class="mt-2 max-h-80 overflow-y-auto" @click.stop>
      <DiffView v-if="view === 'diff' && diff" :diff="diff" />
      <!-- renderMarkdown escapes every text. -->
      <div v-else class="md md-small rounded-md border border-stone-200 px-3 py-2" v-html="after" />
    </div>

    <ul v-if="checks.length" class="mt-2 space-y-0.5 text-xs text-error-text">
      <li v-for="(c, i) in checks" :key="i">{{ checkText(c) }}</li>
    </ul>
    <p v-for="g in grows" :key="g.part" class="mt-2 text-xs text-stone-600">+{{ g.bytes }} bytes · {{ g.part }}</p>

    <ol v-if="change.comments.length" class="mt-3 space-y-2 border-t border-stone-100 pt-2">
      <li v-for="c in change.comments" :key="c.id">
        <div class="flex items-baseline gap-2 text-xs text-stone-500">
          <span v-if="c.kind === 'claude'" class="doc-claude-tag">Claude</span>
          <span v-else class="font-medium text-stone-800">{{ c.author }}</span>
          <span>{{ commitTime(c.createdAt) }}</span>
        </div>
        <p class="whitespace-pre-wrap text-stone-800">{{ c.deleted ? $t('Comentario borrado') : c.body }}</p>
      </li>
    </ol>

    <form v-if="active && pending" class="mt-2" @submit.prevent="comment" @click.stop>
      <textarea v-model="draft" rows="2" class="doc-box" :placeholder="$t('Comentar el cambio')" />
      <div class="mt-1 flex flex-wrap justify-end gap-2">
        <button class="btn px-2 py-0.5 text-xs" :disabled="busy || !draft.trim()">{{ $t('Comentar') }}</button>
        <button v-if="admin" type="button" class="btn px-2 py-0.5 text-xs" :disabled="busy || !draft.trim()" @click="askAgain('comment')">
          {{ $t('Pedir versión nueva') }}
        </button>
      </div>
    </form>

    <div v-if="admin" class="mt-2 flex flex-wrap items-center justify-end gap-2" @click.stop>
      <template v-if="pending && change.stale">
        <button type="button" class="btn px-2 py-0.5 text-xs" :disabled="busy" @click="discard">{{ $t('Descartar') }}</button>
        <button type="button" class="btn-primary px-2 py-0.5 text-xs" :disabled="busy" @click="askAgain('stale')">{{ $t('Pedir a Claude que la rehaga') }}</button>
      </template>
      <template v-else-if="pending">
        <button type="button" class="btn px-2 py-0.5 text-xs" :disabled="busy" @click="discard">{{ $t('Descartar') }}</button>
        <button v-if="grows.length" type="button" class="btn-primary px-2 py-0.5 text-xs" :disabled="busy || checks.length > 0" @click="apply(true)">
          {{ $t('Aplicar igualmente') }}
        </button>
        <button v-else type="button" class="btn-primary px-2 py-0.5 text-xs" :disabled="busy || checks.length > 0" @click="apply()">{{ $t('Aplicar') }}</button>
      </template>
      <template v-else-if="change.lost">
        <button type="button" class="btn-primary px-2 py-0.5 text-xs" :disabled="busy" @click="repropose">{{ $t('Volver a proponer') }}</button>
      </template>
      <template v-else-if="change.status === 'applied'">
        <span v-if="gitState === 'failed'" class="mr-auto text-xs text-amber-800">
          {{ $t('Sin subir a git') }} ·
          <button type="button" class="underline" :disabled="busy" @click="retry">{{ $t('Reintentar') }}</button>
        </span>
        <span v-else-if="gitState === 'pending'" class="mr-auto text-xs text-stone-500">{{ $t('Guardando en git…') }}</span>
        <span v-else-if="gitState === 'committed' || gitState === 'pushed'" class="mr-auto font-mono text-xs text-stone-500">
          {{ gitState === 'pushed' ? $t('En git') : $t('En git (local)') }} {{ change.revision?.gitCommit?.slice(0, 7) }}
        </span>
        <button type="button" class="btn px-2 py-0.5 text-xs" :disabled="busy" @click="undo">{{ $t('Deshacer') }}</button>
      </template>
      <template v-else-if="change.status === 'undone'">
        <button type="button" class="btn px-2 py-0.5 text-xs" :disabled="busy" @click="apply()">{{ $t('Aplicar de nuevo') }}</button>
      </template>
    </div>
  </article>
</template>
