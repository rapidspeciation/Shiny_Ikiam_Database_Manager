<script setup lang="ts">
import { computed, nextTick, ref } from 'vue'
import { act, type DocComment, type DocThread } from '../../lib/docReview'
import { commitTime } from '../../lib/instructions'
import { errorText, notify } from '../../lib/notice'
import { t } from '../../lib/i18n'

/** One comment thread: its quote, the comments and replies, a reply box, resolve and reopen. */
const props = defineProps<{ thread: DocThread; active?: boolean; detached?: boolean }>()
const emit = defineEmits<{ activate: []; quote: [] }>()

const draft = ref('')
const busy = ref(false)
const editing = ref<string | null>(null)
const editText = ref('')
const box = ref<HTMLTextAreaElement>()
const shown = computed(() => props.thread.comments)
const resolved = computed(() => props.thread.status === 'resolved')

async function run(fn: () => Promise<unknown>) {
  busy.value = true
  try {
    await fn()
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    busy.value = false
  }
}
const reply = () =>
  run(async () => {
    if (!draft.value.trim()) return
    await act(`threads/${props.thread.id}/comments`, 'POST', { body: draft.value })
    draft.value = ''
  })
const setStatus = (to: 'resolve' | 'reopen') => run(() => act(`threads/${props.thread.id}/${to}`))
function startEdit(c: DocComment) {
  editing.value = c.id
  editText.value = c.body
}
const saveEdit = (c: DocComment) =>
  run(async () => {
    await act(`comments/${c.id}`, 'PATCH', { body: editText.value })
    editing.value = null
  })
const remove = (c: DocComment) =>
  run(async () => {
    if (!window.confirm(t('¿Borrar este comentario?'))) return
    await act(`comments/${c.id}`, 'DELETE')
  })
/** The reply box gets the focus (a card opened from its highlight). */
defineExpose({ focus: () => nextTick(() => box.value?.focus()) })
</script>

<template>
  <article
    class="doc-card rounded-lg border bg-surface p-3 text-sm"
    :class="[active ? 'doc-card-active border-brand-300' : 'border-stone-200', resolved ? 'opacity-80' : '']"
    :data-card="thread.id"
    @click="emit('activate')"
  >
    <button
      type="button"
      class="mb-2 block w-full border-l-2 pl-2 text-left text-xs leading-5"
      :class="detached ? 'border-stone-300 text-stone-400' : 'doc-quote text-stone-600'"
      :title="detached ? $t('El texto cambió') : $t('Ver en el texto')"
      @click.stop="emit('quote')"
    >
      <span class="line-clamp-3">{{ thread.quote }}</span>
    </button>
    <ol class="space-y-2">
      <li v-for="c in shown" :key="c.id">
        <p v-if="c.kind === 'system'" class="text-xs text-stone-500">{{ $t('Resuelto con el cambio') }}</p>
        <template v-else>
          <div class="flex items-baseline gap-2 text-xs text-stone-500">
            <span v-if="c.kind === 'claude'" class="doc-claude-tag">Claude</span>
            <span v-else class="font-medium text-stone-800">{{ c.author }}</span>
            <span>{{ commitTime(c.createdAt) }}</span>
            <span v-if="c.editedAt && !c.deleted">· {{ $t('editado') }}</span>
            <span v-if="c.own && !c.deleted && editing !== c.id" class="ml-auto flex gap-2">
              <button type="button" class="hover:text-stone-800" @click.stop="startEdit(c)">{{ $t('Editar') }}</button>
              <button type="button" class="hover:text-stone-800" @click.stop="remove(c)">{{ $t('Borrar') }}</button>
            </span>
          </div>
          <p v-if="c.deleted" class="text-stone-400 italic">{{ $t('Comentario borrado') }}</p>
          <form v-else-if="editing === c.id" class="mt-1" @submit.prevent="saveEdit(c)" @click.stop>
            <textarea v-model="editText" rows="3" class="doc-box" />
            <div class="mt-1 flex justify-end gap-2">
              <button type="button" class="btn-ghost px-2 py-0.5 text-xs" @click="editing = null">{{ $t('Cancelar') }}</button>
              <button class="btn-primary px-2 py-0.5 text-xs" :disabled="busy || !editText.trim()">{{ $t('Guardar') }}</button>
            </div>
          </form>
          <p v-else class="whitespace-pre-wrap text-stone-800">{{ c.body }}</p>
        </template>
      </li>
    </ol>
    <p v-if="resolved" class="mt-2 text-xs text-stone-500">
      {{ thread.resolvedBy ? $t('Resuelto por {name}', { name: thread.resolvedBy }) : $t('Resuelto') }}
    </p>
    <form v-if="active && !resolved" class="mt-2" @submit.prevent="reply" @click.stop>
      <textarea
        ref="box"
        v-model="draft"
        rows="2"
        class="doc-box"
        :placeholder="$t('Responder')"
        @keydown.ctrl.enter.prevent="reply"
        @keydown.meta.enter.prevent="reply"
      />
      <div class="mt-1 flex items-center justify-end gap-2">
        <button type="button" class="btn-ghost px-2 py-0.5 text-xs" :disabled="busy" @click="setStatus('resolve')">{{ $t('Resolver') }}</button>
        <button class="btn-primary px-2 py-0.5 text-xs" :disabled="busy || !draft.trim()">{{ $t('Responder') }}</button>
      </div>
    </form>
    <div v-else-if="active && resolved" class="mt-2 flex justify-end" @click.stop>
      <button type="button" class="btn-ghost px-2 py-0.5 text-xs" :disabled="busy" @click="setStatus('reopen')">{{ $t('Reabrir') }}</button>
    </div>
  </article>
</template>
