<script setup lang="ts">
import { onMounted, reactive, ref } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { api, setCsrf, type ApiError } from '../lib/api'
import { errorText } from '../lib/notice'
import { useSession } from '../stores/session'
import { t } from '../lib/i18n'

/** Opened from a reset link (email, or shared by an administrator): choose a new password. */
const route = useRoute()
const router = useRouter()
const session = useSession()
const token = String(route.query.t ?? '')
type Status = 'valid' | 'used' | 'expired' | 'invalid'
const reset = ref<{ username: string; displayName: string; status: Status } | null>(null)
const problem = ref('')
const busy = ref(false)
const form = reactive({ password: '', repeat: '' })
const GONE: Record<string, Status> = { RESET_USED: 'used', RESET_EXPIRED: 'expired', RESET_INVALID: 'invalid' }

onMounted(async () => {
  try {
    reset.value = (
      await api<{ reset: NonNullable<typeof reset.value> }>(`auth/reset/lookup?t=${encodeURIComponent(token)}`)
    ).reset
  } catch (e) {
    if ((e as ApiError).code === 'RESET_INVALID') reset.value = { username: '', displayName: '', status: 'invalid' }
    else problem.value = errorText(e)
  }
})

async function submit() {
  problem.value = ''
  if (form.password !== form.repeat) return void (problem.value = t('Las contraseñas no coinciden.'))
  busy.value = true
  try {
    const out = await api<{ csrf: string }>('auth/reset', { method: 'POST', body: { token, password: form.password } })
    setCsrf(out.csrf)
    await session.load()
    await router.replace('/tablas')
  } catch (e) {
    const gone = GONE[(e as ApiError).code]
    if (gone && reset.value) reset.value.status = gone
    else problem.value = errorText(e)
  } finally {
    busy.value = false
  }
}
</script>

<template>
  <div class="grid h-full place-items-center overflow-y-auto bg-stone-50 p-4">
    <form class="w-full max-w-sm space-y-3 rounded-lg border border-stone-200 bg-white p-6 shadow-sm" @submit.prevent="submit">
      <h1 class="text-lg font-semibold">{{ $t('Nueva contraseña') }}</h1>
      <template v-if="reset?.status === 'valid'">
        <p class="text-sm text-stone-600">
          {{ $t('Cuenta') }}: <strong class="text-stone-900">{{ reset.username }}</strong>
          <span v-if="reset.displayName && reset.displayName !== reset.username"> ({{ reset.displayName }})</span>
        </p>
        <!-- For the browser's password manager. -->
        <input type="text" class="hidden" name="username" autocomplete="username" :value="reset.username" readonly />
        <label class="block">
          <span class="field-label">{{ $t('Contraseña nueva') }}</span>
          <input
            v-model="form.password"
            class="field-input w-full"
            type="password"
            autocomplete="new-password"
            minlength="6"
            maxlength="16"
            required
          />
          <span class="hint">{{ $t('6 a 16 caracteres.') }}</span>
        </label>
        <label class="block">
          <span class="field-label">{{ $t('Repite la contraseña') }}</span>
          <input v-model="form.repeat" class="field-input w-full" type="password" autocomplete="new-password" required />
        </label>
        <button class="btn-primary w-full justify-center" :disabled="busy">
          {{ busy ? $t('Un momento…') : $t('Guardar la contraseña y entrar') }}
        </button>
      </template>
      <template v-else-if="reset">
        <p class="text-sm text-stone-700">
          {{
            reset.status === 'used'
              ? $t('Este enlace ya se usó.')
              : reset.status === 'expired'
                ? $t('Este enlace venció (vale 24 horas) o fue reemplazado por uno más nuevo.')
                : $t('Este enlace no es válido.')
          }}
        </p>
        <RouterLink
          :to="{ path: '/recuperar', query: reset.username ? { usuario: reset.username } : {} }"
          class="btn-primary w-full justify-center"
        >
          {{ $t('Pedir un enlace nuevo') }}
        </RouterLink>
        <RouterLink to="/entrar" class="block text-center text-sm text-stone-600 underline">
          {{ $t('Volver a iniciar sesión') }}
        </RouterLink>
      </template>
      <p v-if="problem" class="text-sm text-red-700">{{ problem }}</p>
    </form>
  </div>
</template>
