<script setup lang="ts">
// The Identificar tab is a page of its own (identificar/, cross-origin isolated for its models): this route goes there,
// replacing its own history entry so the browser's back button returns to the tab before.
import { identifyPage } from '../tabs'

location.replace(identifyPage())
</script>

<template>
  <div class="grid h-full place-items-center text-stone-500">{{ $t('Cargando…') }}</div>
</template>
