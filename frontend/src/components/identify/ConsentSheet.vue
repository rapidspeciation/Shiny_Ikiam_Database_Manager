<script setup lang="ts">
/**
 * The consent for photos sent to train the AI (lib/contribConfig.ts CONSENT, version CONSENT_VERSION), with «Acepto» /
 * «Ahora no». The contact address is the server's (its config), passed in at runtime; without it, a line that says how
 * to ask the team. Shown by ContributeCard on Enviar and by «Mis envíos» when items wait for a newer version.
 */
import { CONSENT, CONTROLLER } from '../../lib/contribConfig'

defineProps<{ contact: string | null }>()
const emit = defineEmits<{ accept: []; later: [] }>()
</script>

<template>
  <div data-testid="consent-sheet">
    <h2 class="font-semibold">{{ $t(CONSENT.title) }}</h2>
    <p v-for="(p, i) in CONSENT.body" :key="i" class="mt-2 text-sm text-fg-muted">{{ $t(p) }}</p>
    <p class="mt-2 text-sm text-fg-muted" data-testid="consent-contact">
      <template v-if="contact">{{ $t(CONSENT.contact, { name: CONTROLLER, email: contact }) }}</template>
      <template v-else>{{ $t(CONSENT.noContact, { name: CONTROLLER }) }}</template>
    </p>
    <div class="mt-3 flex flex-wrap gap-2">
      <button class="btn-primary min-h-11 px-5" data-testid="consent-accept" @click="emit('accept')">{{ $t('Acepto') }}</button>
      <button class="btn min-h-11 px-4" data-testid="consent-later" @click="emit('later')">{{ $t('Ahora no') }}</button>
    </div>
  </div>
</template>
