<script setup lang="ts">
/**
 * The top of Identificar (as the Wings Gallery's AI Identifier): the photos of this visit as thumbnails (tap one to
 * see its result again), «Añadir fotos» (the phone's chooser, several at once), the camera, and «Borrar fotos». With
 * no photo yet, the same box is the start: take or choose a photo, paste or drop one. The view keeps the photos and
 * the file inputs; this only shows them and says what was tapped.
 */
import { Camera, Check, ImagePlus, LoaderCircle, X } from 'lucide-vue-next'
import PhotoThumb from './PhotoThumb.vue'

defineProps<{
  shots: { at: string; thumb: string }[]
  current: string | null
  /** What is happening now (preparing, sending, analysing), or null. */
  busy: string | null
  disabled: boolean
  /** Paste and drop work here (a mouse and keyboard): the start says so. */
  pasteKeys: string | null
}>()
defineEmits<{ select: [at: string]; remove: [at: string]; files: []; camera: []; clear: [] }>()
</script>

<template>
  <section class="rounded-lg border border-border bg-surface p-2" data-testid="photo-bar" :aria-label="$t('Sus fotos')">
    <template v-if="shots.length">
    <div class="flex items-center gap-1.5 sm:gap-2">
      <!-- the thumbnails scroll by themselves; the buttons stay in view -->
      <div class="flex min-w-12 items-center gap-2 overflow-x-auto py-1 pr-1" data-testid="photo-bar-thumbs">
        <span v-for="(s, i) in shots" :key="s.at" class="relative shrink-0">
          <PhotoThumb :src="s.thumb" :size="48" :selected="s.at === current" :label="$t('Foto {n}', { n: i + 1 })" @select="$emit('select', s.at)" />
          <!-- the photo shown: × takes just it off the strip (Mis identificaciones keeps it) -->
          <button
            v-if="s.at === current"
            type="button"
            class="hit-target absolute -top-1 -right-1 grid size-5 place-items-center rounded-full border border-border-strong bg-surface text-fg shadow-sm hover:bg-surface-sunken"
            :aria-label="$t('Quitar esta foto')"
            data-testid="remove-photo"
            @click="$emit('remove', s.at)"
          >
            <X :size="12" />
          </button>
        </span>
      </div>
      <button class="btn min-h-12 shrink-0 border-brand px-2.5 text-brand-text" :disabled="disabled" data-testid="add-photos" @click="$emit('files')">
        <ImagePlus :size="18" />{{ $t('Añadir fotos') }}
      </button>
      <button class="btn size-12 shrink-0 px-0" :disabled="disabled" :aria-label="$t('Tomar foto')" data-testid="take-photo" @click="$emit('camera')">
        <Camera :size="20" />
      </button>
      <span v-if="busy" class="hidden shrink-0 items-center gap-1 text-sm text-fg-muted sm:flex" role="status"><LoaderCircle :size="16" class="animate-spin" />{{ busy }}</span>
      <span v-else-if="current" class="hidden shrink-0 items-center gap-1 text-sm text-success-text sm:flex"><Check :size="16" />{{ $t('Identificada') }}</span>
      <button class="ml-auto flex min-h-12 shrink-0 items-center gap-1 rounded-md px-1 text-sm font-medium text-brand-text underline-offset-2 hover:underline" data-testid="clear-photos" @click="$emit('clear')">
        <X :size="16" /><span class="max-[379px]:hidden">{{ $t('Borrar fotos') }}</span><span class="hidden max-[379px]:inline">{{ $t('Borrar') }}</span>
      </button>
    </div>
    <p v-if="busy" class="mt-1 flex items-center gap-1 px-1 text-sm text-fg-muted sm:hidden" role="status"><LoaderCircle :size="16" class="animate-spin" />{{ busy }}</p>
    </template>

    <div v-else class="p-2" data-testid="photo-start">
      <h1 class="text-xl font-semibold">{{ $t('Identificar una mariposa') }}</h1>
      <p class="mt-1 text-sm text-fg-muted">{{ $t('Alas abiertas, de cerca, que la mariposa llene la foto.') }}</p>
      <div class="mt-3 flex flex-wrap gap-2">
        <button class="btn-primary min-h-12 px-5 text-base" :disabled="disabled" data-testid="take-photo" @click="$emit('camera')"><Camera :size="20" />{{ $t('Tomar foto') }}</button>
        <button class="btn min-h-12 border-brand px-5 text-base text-brand-text" :disabled="disabled" data-testid="add-photos" @click="$emit('files')"><ImagePlus :size="20" />{{ $t('Añadir fotos') }}</button>
      </div>
      <p v-if="pasteKeys" class="mt-2 text-sm text-fg-subtle">{{ $t('También puede pegar una imagen ({keys}) o arrastrarla a la página.', { keys: pasteKeys }) }}</p>
      <p v-if="busy" class="mt-2 flex items-center gap-1 text-sm" role="status"><LoaderCircle :size="16" class="animate-spin" />{{ busy }}</p>
    </div>
  </section>
</template>
