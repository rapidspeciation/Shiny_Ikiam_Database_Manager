<script setup lang="ts">
import { computed, onMounted, ref } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { useSession } from '../stores/session'
import { errorText } from '../lib/notice'

const session = useSession()
const route = useRoute()
const router = useRouter()
// Already signed in (e.g. an old "Iniciar sesión" link).
onMounted(() => session.user && router.replace('/inicio'))
const hashToken = new URLSearchParams(location.hash.split('?')[1] || '').get('token') || ''
const setupMode = computed(() => session.setupRequired || !!hashToken)

const username = ref('')
const displayName = ref('')
const password = ref('')
const token = ref(hashToken)
const error = ref('')
const busy = ref(false)

async function submit() {
  error.value = ''
  busy.value = true
  try {
    if (setupMode.value) {
      await session.setup(
        token.value.trim(),
        username.value.trim(),
        displayName.value.trim() || username.value.trim(),
        password.value,
      )
      history.replaceState(null, '', location.pathname + '#/tablas')
    } else {
      await session.login(username.value.trim(), password.value)
      // From the "Iniciar sesión" button: back to where the person was.
      if (route.path === '/entrar') await router.replace(String(route.query.volver || '/inicio'))
    }
  } catch (e) {
    error.value = errorText(e)
  } finally {
    busy.value = false
  }
}
</script>

<template>
  <div class="grid min-h-full place-items-center bg-brand-700 p-4">
    <form class="w-full max-w-sm rounded-lg bg-white p-6 shadow-xl" @submit.prevent="submit">
      <div class="mb-5 flex items-center gap-2">
        <img src="/mark.svg" alt="" class="h-8 w-8" />
        <div>
          <h1 class="text-lg font-semibold">Ikiam Insectary DB</h1>
          <p class="text-sm text-stone-500">{{ setupMode ? 'Crear la cuenta de administrador' : 'Iniciar sesión' }}</p>
        </div>
      </div>
      <label v-if="setupMode" class="mb-3 block">
        <span class="field-label">Código de configuración</span>
        <input v-model="token" class="field-input" type="text" autocomplete="off" spellcheck="false" required />
      </label>
      <label class="mb-3 block">
        <span class="field-label">Usuario</span>
        <input v-model="username" class="field-input" name="username" autocomplete="username" autocapitalize="none" required />
      </label>
      <label v-if="setupMode" class="mb-3 block">
        <span class="field-label">Nombre visible</span>
        <input v-model="displayName" class="field-input" autocomplete="name" />
      </label>
      <label class="mb-4 block">
        <span class="field-label">Contraseña</span>
        <input
          v-model="password"
          class="field-input"
          type="password"
          name="password"
          :autocomplete="setupMode ? 'new-password' : 'current-password'"
          minlength="6"
          maxlength="16"
          required
        />
        <span v-if="setupMode" class="hint">De 6 a 16 caracteres.</span>
      </label>
      <p v-if="error" class="mb-3 rounded bg-red-50 px-3 py-2 text-sm text-red-800">{{ error }}</p>
      <button class="btn-primary w-full py-2" :disabled="busy">
        {{ busy ? 'Un momento…' : setupMode ? 'Crear cuenta' : 'Entrar' }}
      </button>
      <RouterLink v-if="!setupMode" to="/inicio" class="mt-4 block text-center text-sm text-stone-600 underline">
        Ver los resúmenes del proyecto sin cuenta
      </RouterLink>
    </form>
  </div>
</template>
