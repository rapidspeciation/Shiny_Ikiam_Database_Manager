<script setup lang="ts">
import { onMounted, reactive, ref } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { api, setCsrf } from '../lib/api'
import { errorText } from '../lib/notice'
import { useSession } from '../stores/session'

/** Opened from the invitation email: the person chooses a username and password. */
const route = useRoute()
const router = useRouter()
const session = useSession()
const token = String(route.query.t ?? '')
const invitation = ref<{ email: string; displayName: string; role: string; status: string } | null>(null)
const problem = ref('')
const busy = ref(false)
const form = reactive({ username: '', displayName: '', password: '', repeat: '' })

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
  if (form.password !== form.repeat) return void (problem.value = 'Las contraseñas no coinciden.')
  busy.value = true
  try {
    const out = await api<{ csrf: string }>('invitations/accept', {
      method: 'POST',
      body: { token, username: form.username, displayName: form.displayName, password: form.password },
    })
    setCsrf(out.csrf)
    await session.load()
    await router.replace('/tablas')
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
      <template v-if="invitation?.status === 'pending'">
        <p class="text-sm text-stone-600">
          Hola {{ invitation.displayName }}. Elige cómo entrarás a la app ({{ invitation.email }}).
        </p>
        <label class="block">
          <span class="field-label">Usuario</span>
          <input v-model="form.username" class="field-input w-full" autocomplete="username" autocapitalize="none" required />
          <span class="hint">3 a 64 letras, números, puntos, guiones.</span>
        </label>
        <label class="block">
          <span class="field-label">Nombre</span>
          <input v-model="form.displayName" class="field-input w-full" autocomplete="name" required />
        </label>
        <label class="block">
          <span class="field-label">Contraseña</span>
          <input
            v-model="form.password"
            class="field-input w-full"
            type="password"
            autocomplete="new-password"
            minlength="6"
            maxlength="16"
            required
          />
          <span class="hint">6 a 16 caracteres.</span>
        </label>
        <label class="block">
          <span class="field-label">Repite la contraseña</span>
          <input v-model="form.repeat" class="field-input w-full" type="password" autocomplete="new-password" required />
        </label>
        <button class="btn-primary w-full justify-center" :disabled="busy">{{ busy ? 'Creando…' : 'Crear mi cuenta' }}</button>
      </template>
      <p v-else-if="invitation?.status === 'used'" class="text-sm text-stone-700">
        Esta invitación ya se usó. <RouterLink to="/tablas" class="underline">Inicia sesión</RouterLink> con tu usuario.
      </p>
      <p v-else-if="invitation" class="text-sm text-stone-700">
        Esta invitación venció. Pide a un administrador que te envíe una nueva.
      </p>
      <p v-if="problem" class="text-sm text-red-700">{{ problem }}</p>
    </form>
  </div>
</template>
