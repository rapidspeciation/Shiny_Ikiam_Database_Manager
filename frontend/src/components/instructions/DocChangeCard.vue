<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import ProseDiff from './ProseDiff.vue'
import { act, changeTitle, type DocChange, type DocCheck } from '../../lib/docReview'
import { commitTime } from '../../lib/instructions'
import { renderMarkdown } from '../../lib/markdown'
import { errorText, notify } from '../../lib/notice'
import { t } from '../../lib/i18n'
import { ApiError } from '../../lib/api'

/**
 * A change Claude proposes to one section («cambio de documento»): its word diff or the text after it,
 * the versions (v1, v2…), and for admins Aplicar, Descartar, Deshacer; anyone comments on it.
 */
const props = defineProps<{ change: DocChange; admin: boolean; active?: boolean; quotes?: { id: string; quote: string }[] }>()
const emit = defineEmits<{ activate: []; section: []; asked: [chat: string | null]; thread: [id: string] }>()

const view = ref<'diff' | 'after'>('diff')
const picked = ref<number | null>(null)
watch(
  () => props.change.version,
  () => (picked.value = null),
)
const shownVersion = computed(() => props.change.versions.find(v => v.version === (picked.value ?? props.change.version)) ?? props.change.versions.at(-1)!)
const older = computed(() => props.change.versions.length > 1)
/** An earlier version on screen: read only, no status or decisions of the current one. */
const viewingOld = computed(() => shownVersion.value.version !== props.change.version)
const diff = computed(() => shownVersion.value.diff ?? '')
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
/** A part of the chats' fixed context in words (scripts/context-budget.mjs names them skill:<x>, brief, mcp …). */
const partName = (part: string) =>
  part.startsWith('skill:')
    ? t('la habilidad {name}', { name: part.slice(6) })
    : part === 'brief'
      ? t('las instrucciones generales')
      : part.startsWith('mcp')
        ? t('las herramientas')
        : part
/** A new version is on its way (Claude's chat, or an outside proposer): nothing is applied meanwhile. */
const waiting = computed(() => props.change.revising || !!props.change.awaiting)
const gitFailed = computed(() => props.change.revision?.gitState === 'failed')
/** The commit, for admins, in the «Aplicado por…» line's tooltip only. */
const commitTitle = computed(() => (props.admin && props.change.revision?.gitCommit ? props.change.revision.gitCommit.slice(0, 7) : undefined))

/** A system line of the series: why a version exists. */
function systemText(body: string) {
  const [kind, n] = body.split(':')
  return kind === 'version_after_stale' ? t('v{n}: el texto había cambiado', { n }) : kind === 'version_reproposed' ? t('v{n}: propuesta de nuevo', { n }) : body
}
/**
 * The decision button changes in place (Aplicar → Deshacer → Aplicar de nuevo): taps in the first 500 ms after it
 * changed are ignored, so a double tap never applies and undoes.
 */
let flippedAt = 0
watch(
  () => [props.change.status, props.change.version],
  () => (flippedAt = Date.now()),
)
const settled = () => Date.now() - flippedAt > 500
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
/** Applies the version on screen, which is always the current one (older versions are read only). */
const apply = (anyway = false) => settled() && run(() => act(`changes/${props.change.seriesId}/apply`, 'POST', { anyway, version: props.change.version }))
const discard = () => settled() && run(() => act(`changes/${props.change.seriesId}/discard`))
const undo = () =>
  settled() &&
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
    // Sent: the card says «Claude está respondiendo…» (no toast over the comment box).
    if (!out.sent && out.prompt) {
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
      <button type="button" class="min-w-0 truncate text-left font-medium text-stone-800 hover:underline" @click="emit('section')">
        {{ changeTitle(change, t) }}
      </button>
      <span v-if="change.path.includes('/voice/')" class="font-mono text-xs text-stone-500">{{ file }}</span>
      <span class="ml-auto hidden items-center gap-1 text-xs lg:flex">
        <template v-if="older">
          <button
            v-for="v in change.versions"
            :key="v.id"
            type="button"
            class="doc-act doc-ver rounded px-1"
            :class="v.version === shownVersion.version ? 'bg-stone-200 text-stone-800' : 'text-stone-500 hover:text-stone-800'"
            @click.stop="picked = v.version === change.version ? null : v.version"
          >
            v{{ v.version }}
          </button>
        </template>
      </span>
    </header>
    <!-- The comment it answers: a click shows that thread. -->
    <button
      v-if="quotes?.length"
      type="button"
      class="doc-quote doc-quote-btn mt-2 flex w-full items-baseline gap-1 border-l-2 pl-2 text-left text-xs text-stone-600"
      :title="$t('Ver el comentario')"
      @click.stop="emit('thread', quotes[0].id)"
    >
      <span class="min-w-0 truncate">{{ quotes[0].quote }}</span>
      <span v-if="quotes.length > 1" class="shrink-0 text-stone-500">+{{ quotes.length - 1 }}</span>
    </button>

    <p v-if="viewingOld" class="mt-1 flex items-center gap-2 text-xs text-stone-600">
      <span class="font-medium">{{ $t('Versión anterior') }}</span>
      <button type="button" class="doc-act font-medium text-brand-text hover:underline" @click.stop="picked = null">{{ $t('Ver v{n}', { n: change.version }) }}</button>
    </p>
    <p v-else-if="change.stale" class="mt-1 text-xs font-medium text-amber-800">{{ $t('Desactualizada') }}</p>
    <p v-else-if="change.lost" class="mt-1 text-xs font-medium text-amber-800">{{ $t('No está en la versión desplegada') }}</p>
    <p v-else-if="change.status === 'applied'" class="mt-1 text-xs text-stone-500" :title="commitTitle">
      {{ $t('Aplicado por {name} · {date}', { name: change.decidedBy ?? '', date: change.decidedAt ? commitTime(change.decidedAt) : '' }) }}
    </p>
    <p v-else-if="change.status === 'undone'" class="mt-1 text-xs text-stone-500">
      {{ $t('Deshecho por {name} · {date}', { name: change.decidedBy ?? '', date: change.decidedAt ? commitTime(change.decidedAt) : '' }) }}
    </p>

    <div class="mt-2 flex items-center gap-2">
    <div class="inline-flex rounded-md border border-stone-300 p-0.5 text-xs" role="tablist">
      <button role="tab" type="button" class="doc-act rounded px-2 py-0.5" :class="view === 'diff' ? 'bg-stone-200 text-stone-900' : 'text-stone-600'" :aria-selected="view === 'diff'" @click="view = 'diff'">
        {{ $t('Cambios') }}
      </button>
      <button role="tab" type="button" class="doc-act rounded px-2 py-0.5" :class="view === 'after' ? 'bg-stone-200 text-stone-900' : 'text-stone-600'" :aria-selected="view === 'after'" @click="view = 'after'">
        {{ $t('Después') }}
      </button>
    </div>
      <span v-if="older" class="ml-auto flex items-center gap-1 text-xs lg:hidden">
        <button
          v-for="v in change.versions"
          :key="v.id"
          type="button"
          class="doc-act doc-ver rounded px-1"
          :class="v.version === shownVersion.version ? 'bg-stone-200 text-stone-800' : 'text-stone-500 hover:text-stone-800'"
          @click.stop="picked = v.version === change.version ? null : v.version"
        >
          v{{ v.version }}
        </button>
      </span>
    </div>
    <div class="mt-2 max-h-80 overflow-y-auto" :class="{ 'opacity-70': change.status === 'undone' && !viewingOld }" data-diff>
      <ProseDiff v-if="view === 'diff' && diff" :diff="diff" />
      <!-- renderMarkdown escapes every text. -->
      <div v-else class="md md-small rounded-md border border-stone-200 px-3 py-2" v-html="after" />
    </div>

    <template v-if="!viewingOld">
      <ul v-if="checks.length" class="mt-2 space-y-0.5 text-xs text-error-text">
        <li v-for="(c, i) in checks" :key="i">{{ checkText(c) }}</li>
      </ul>
      <p v-for="g in grows" :key="g.part" class="mt-2 text-xs text-stone-600">{{ $t('Crece {n} bytes: {part}', { n: g.bytes, part: partName(g.part) }) }}</p>
    </template>

    <ol v-if="change.comments.length" class="mt-3 space-y-2 border-t border-stone-100 pt-2" data-comments>
      <li v-for="c in change.comments" :key="c.id">
        <p v-if="c.kind === 'system'" class="text-xs text-stone-500">{{ systemText(c.body) }}</p>
        <template v-else>
        <div class="flex items-baseline gap-2 text-xs text-stone-500">
          <span v-if="c.kind === 'claude'" class="doc-claude-tag">Claude</span>
          <span v-else class="font-medium text-stone-800">{{ c.author }}</span>
          <span>{{ commitTime(c.createdAt) }}</span>
        </div>
        <p class="whitespace-pre-wrap text-stone-800">{{ c.deleted ? $t('Comentario borrado') : c.body }}</p>
        </template>
      </li>
    </ol>
    <p v-if="change.revising && !viewingOld" class="mt-2 text-xs text-stone-600" role="status">{{ $t('Claude está respondiendo…') }}</p>
    <p v-else-if="change.awaiting && !viewingOld" class="mt-2 text-xs text-stone-600" role="status">{{ $t('Esperando nueva versión') }}</p>

    <form v-if="active && pending && !viewingOld" class="mt-2" @submit.prevent="comment">
      <textarea v-model="draft" rows="2" class="doc-box" :placeholder="$t('Comentar el cambio')" />
      <div class="mt-1 flex flex-wrap justify-end gap-2">
        <button class="doc-act btn px-2 py-0.5 text-xs" :disabled="busy || !draft.trim()">{{ $t('Comentar') }}</button>
        <button v-if="admin" type="button" class="doc-act btn px-2 py-0.5 text-xs" :disabled="busy || !draft.trim()" @click="askAgain('comment')">
          {{ $t('Pedir versión nueva') }}
        </button>
      </div>
    </form>

    <!-- Decisions: the destructive one on the left, the main one on the right. -->
    <div v-if="admin && !viewingOld" class="doc-decide mt-3 flex flex-wrap items-center justify-between gap-x-6 gap-y-2">
      <template v-if="pending && change.stale">
        <button type="button" class="doc-act btn px-2 py-0.5 text-xs" :disabled="busy" @click="discard">{{ $t('Descartar') }}</button>
        <button type="button" class="doc-act btn-primary px-2 py-0.5 text-xs" :disabled="busy" @click="askAgain('stale')">{{ $t('Pedir a Claude que la rehaga') }}</button>
      </template>
      <template v-else-if="pending">
        <button type="button" class="doc-act btn px-2 py-0.5 text-xs" :disabled="busy" @click="discard">{{ $t('Descartar') }}</button>
        <button v-if="grows.length" type="button" class="doc-act btn-primary px-2 py-0.5 text-xs" :disabled="busy || checks.length > 0 || waiting" @click="apply(true)">
          {{ $t('Aplicar igualmente') }}
        </button>
        <button v-else type="button" class="doc-act btn-primary px-2 py-0.5 text-xs" :disabled="busy || checks.length > 0 || waiting" @click="apply()">{{ $t('Aplicar') }}</button>
      </template>
      <template v-else-if="change.lost">
        <span />
        <button type="button" class="doc-act btn-primary px-2 py-0.5 text-xs" :disabled="busy" @click="repropose">{{ $t('Volver a proponer') }}</button>
      </template>
      <template v-else-if="change.status === 'applied'">
        <span v-if="gitFailed" class="text-xs text-amber-800">
          {{ $t('Sin subir a git') }} ·
          <button type="button" class="doc-act underline" :disabled="busy" @click="retry">{{ $t('Reintentar') }}</button>
        </span>
        <span v-else />
        <button type="button" class="doc-act btn px-2 py-0.5 text-xs" :disabled="busy" @click="undo">{{ $t('Deshacer') }}</button>
      </template>
      <template v-else-if="change.status === 'undone'">
        <span />
        <button type="button" class="doc-act btn px-2 py-0.5 text-xs" :disabled="busy" @click="apply()">{{ $t('Aplicar de nuevo') }}</button>
      </template>
    </div>
  </article>
</template>
