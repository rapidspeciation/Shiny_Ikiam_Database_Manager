<script setup lang="ts">
import { ref } from 'vue'
import { useRoute } from 'vue-router'
import { api } from '../lib/api'
import { errorText } from '../lib/notice'

/** From "¿Olvidaste tu contraseña?": asks for a reset link by username or email. */
const route = useRoute()
const identifier = ref(String(route.query.usuario ?? ''))
const busy = ref(false)
const done = ref(false)
const problem = ref('')

async function submit() {
  problem.value = ''
  busy.value = true
  try {
    await api('auth/reset/request', { method: 'POST', body: { identifier: identifier.value.trim() } })
    done.value = true
  } catch (e) {
    problem.value = errorText(e)
  } finally {
    busy.value = false
  }
}
</script>

<template>
  <div class="grid h-full place-items-center overflow-y-auto bg-brand-700 p-4">
    <form class="w-full max-w-sm space-y-3 rounded-lg bg-white p-6 shadow-xl" @submit.prevent="submit">
      <h1 class="text-lg font-semibold">{{ $t('Restablecer la contraseña') }}</h1>
      <template v-if="!done">
        <p class="text-sm text-stone-600">
          {{ $t('Escribe tu usuario o tu correo. Te enviaremos un enlace para elegir una contraseña nueva.') }}
        </p>
        <label class="block">
          <span class="field-label">{{ $t('Usuario o correo') }}</span>
          <input
            v-model="identifier"
            class="field-input w-full"
            name="username"
            autocomplete="username"
            autocapitalize="none"
            spellcheck="false"
            required
          />
        </label>
        <button class="btn-primary w-full justify-center py-2" :disabled="busy">
          {{ busy ? $t('Enviando…') : $t('Enviar enlace') }}
        </button>
      </template>
      <template v-else>
        <p class="rounded bg-brand-50 px-3 py-2 text-sm text-stone-800">
          {{
            $t(
              'Si hay una cuenta con ese usuario o correo y tiene un correo registrado, te llegará un enlace en unos minutos. Vale 24 horas.',
            )
          }}
        </p>
        <p class="text-sm text-stone-600">
          {{ $t('¿No llega? Revisa la carpeta de spam, o pide a un administrador un enlace para restablecer.') }}
        </p>
      </template>
      <p v-if="problem" class="text-sm text-red-700">{{ problem }}</p>
      <RouterLink to="/entrar" class="block text-center text-sm text-stone-600 underline">
        {{ $t('Volver a iniciar sesión') }}
      </RouterLink>
    </form>
  </div>
</template>
