<script setup lang="ts">
/**
 * «Mis envíos»: the photos this person sent from this phone (lib/contribOutbox.ts), with their status, and «Borrar»,
 * which deletes one on the phone and on the server (once there is signal).
 */
import { Trash2 } from 'lucide-vue-next'
import { t } from '../../lib/i18n'
import type { Item } from '../../lib/contribOutbox'

defineProps<{ items: Item[]; paused: boolean }>()
const emit = defineEmits<{ remove: [string] }>()

const SEX = { female: '♀', male: '♂' } as const
function status(i: Item) {
  if (i.state === 'pending' || i.state === 'sending') return t('pendiente de envío')
  if (i.state === 'failed') return t('no se pudo enviar: {reason}', { reason: i.lastError ?? '' })
  if (i.serverStatus === 'accepted') return t('aceptada')
  if (i.serverStatus === 'rejected') return t('no usada')
  return t('enviada, en revisión')
}
const when = (iso: string) => new Date(iso).toLocaleString(undefined, { day: 'numeric', month: 'short', hour: '2-digit', minute: '2-digit' })
function remove(i: Item) {
  if (confirm(t('¿Borrar este envío del teléfono y del servidor?'))) emit('remove', i.id)
}
</script>

<template>
  <details class="rounded-lg border border-border bg-surface p-3 text-sm" data-testid="my-contributions">
    <summary class="cursor-pointer font-medium">{{ $t('Mis envíos') }} <span class="font-normal text-fg-muted">({{ items.length }})</span></summary>
    <p v-if="paused" class="mt-2 text-pending-text">{{ $t('Inicie sesión para enviar.') }}</p>
    <ul class="mt-2 space-y-2">
      <li v-for="i in items" :key="i.id" class="flex items-center gap-3" data-testid="contribution">
        <img v-if="i.mini" :src="i.mini" alt="" class="size-10 rounded object-cover" />
        <span class="min-w-0 flex-1">
          <i v-if="i.payload.labels.species">{{ i.payload.labels.species.name }}</i>
          <span v-if="i.payload.labels.sex" class="ml-1">{{ SEX[i.payload.labels.sex] }}</span>
          <span class="block text-xs text-fg-muted">{{ when(i.createdAt) }} · <span data-testid="contribution-status">{{ status(i) }}</span></span>
        </span>
        <button class="btn-ghost size-11" :aria-label="$t('Borrar')" data-testid="contribution-delete" @click="remove(i)"><Trash2 :size="16" /></button>
      </li>
    </ul>
  </details>
</template>
