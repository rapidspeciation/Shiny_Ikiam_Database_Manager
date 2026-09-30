<script setup lang="ts">
import { onMounted, reactive, ref } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { api, setCsrf } from '../lib/api'
import { errorText } from '../lib/notice'
import { useSession } from '../stores/session'
import { t } from '../lib/i18n'

/** Opened from the invitation email: the person chooses a username and password. */
const route = useRoute()
const router = useRouter()
const session = useSession()
const token = String(route.query.t ?? '')
const invitation = ref<{ email: string; displayName: string; role: string; status: string } | null>(null)
const problem = ref('')
const busy = ref(false)
const form = reactive({ username: '', displayName: '', password: '', repeat: '' })
// After activation: the username and email the person signs in with.
const created = ref<{ username: string; email: string | null } | null>(null)

onMounted(async () => {
  try {
    invitation.value = (
      await api<{ invitation: NonNullable<typeof invitation.value> }>(`invitations/lookup?t=${encodeURIComponent(token)}`)
    ).invitation
    form.displayName = invitation.value.displayName
    form.username = invitation.value.email
      .split('@')[0]
      .toLowerCase()
      .replace(/[^a-z0-9._-]/g, '')
      .slice(0, 30)
  } catch (e) {
    problem.value = errorText(e)
  }
})

async function submit() {
  problem.value = ''
  if (form.password !== form.repeat) return void (problem.value = t('Las contraseñas no coinciden.'))
  busy.value = true
  try {
    const out = await api<{ csrf: string; user: { username: string; email: string | null } }>('invitations/accept', {
      method: 'POST',
      body: { token, username: form.username, displayName: form.displayName, password: form.password },
    })
    setCsrf(out.csrf)
    await session.load()
    created.value = { username: out.user.username, email: out.user.email }
  } catch (e) {
    problem.value = errorText(e)
  } finally {
    busy.value = false
  }
}
</script>

<template>
  <div class="grid h-full place-items-center overflow-y-auto bg-stone-50 p-4">
    <form class="w-full max-w-sm space-y-3 rounded-lg border border-stone-200 bg-white p-6 shadow-sm" @submit.prevent="submit">
      <h1 class="text-lg font-semibold">Ithomiini database</h1>
      <template v-if="created">
        <p class="rounded bg-brand-50 px-3 py-2 text-sm text-stone-800">
          {{ $t('Tu cuenta está lista.') }}
          {{ $t('Tu usuario es') }} <strong class="text-stone-900">{{ created.username }}</strong>{{
            created.email ? $t('; también puedes entrar con tu correo ({email}).', { email: created.email }) : '.'
          }}
        </p>
        <button type="button" class="btn-primary w-full justify-center" @click="router.replace('/tablas')">
          {{ $t('Continuar') }}
        </button>
      </template>
      <template v-else-if="invitation?.status === 'pending'">
        <p class="text-sm text-stone-600">
          {{
            $t('Hola {name}. Elige cómo entrarás a la app ({email}).', {
              name: invitation.displayName,
              email: invitation.email,
            })
          }}
        </p>
        <label class="block">
          <span class="field-label">{{ $t('Usuario') }}</span>
          <input v-model="form.username" class="field-input w-full" autocomplete="username" autocapitalize="none" required />
          <span class="hint">{{ $t('3 a 64 letras, números, puntos, guiones.') }}</span>
        </label>
        <label class="block">
          <span class="field-label">{{ $t('Nombre') }}</span>
          <input v-model="form.displayName" class="field-input w-full" autocomplete="name" required />
        </label>
        <label class="block">
          <span class="field-label">{{ $t('Contraseña') }}</span>
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
          {{ busy ? $t('Creando…') : $t('Crear mi cuenta') }}
        </button>
      </template>
      <p v-else-if="invitation?.status === 'used'" class="text-sm text-stone-700">
        {{ $t('Esta invitación ya se usó.') }}
        <RouterLink to="/tablas" class="underline">{{ $t('Inicia sesión con tu usuario.') }}</RouterLink>
      </p>
      <p v-else-if="invitation" class="text-sm text-stone-700">
        {{ $t('Esta invitación venció. Pide a un administrador que te envíe una nueva.') }}
      </p>
      <p v-if="problem" class="text-sm text-red-700">{{ problem }}</p>
    </form>
  </div>
</template>
