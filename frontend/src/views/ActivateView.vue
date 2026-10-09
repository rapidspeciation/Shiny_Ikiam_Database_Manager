<script setup lang="ts">
import { computed, nextTick, onMounted, reactive, ref } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { api, setCsrf, type ApiError } from '../lib/api'
import { errorText } from '../lib/notice'
import { useSession } from '../stores/session'
import { t } from '../lib/i18n'
import { activationError, activationStep, usernameFromEmail, type InvitationLookup } from '../lib/invitations'

/**
 * Opened from the invitation email: the person chooses a username and password.
 * After the link's first 7 days a code sent to the invitation's email comes first.
 */
const route = useRoute()
const router = useRouter()
const session = useSession()
const token = String(route.query.t ?? '')
const invitation = ref<InvitationLookup | null>(null)
const invalid = ref(false)
const problem = ref('')
const busy = ref(false)
const form = reactive({ username: '', displayName: '', password: '', repeat: '' })
// After the 7 days: the code typed, whether one was asked for here, and whether it was right.
const code = ref('')
const codeAsked = ref(false)
const codeOk = ref(false)
const codeNote = ref('')
const codeInput = ref<HTMLInputElement | null>(null)
// After activation: the username and email the person signs in with.
const created = ref<{ username: string; email: string | null } | null>(null)
const step = computed(() =>
  activationStep(invitation.value, { invalid: invalid.value, codeAsked: codeAsked.value, codeOk: codeOk.value }),
)

/** The page's own words for the errors it expects; the usual text otherwise. */
function show(e: unknown) {
  const err = e as ApiError
  if (err.code === 'INVITATION_INVALID') invalid.value = true
  else if (err.code === 'INVITATION_USED' && invitation.value) invitation.value.status = 'used'
  else if (err.code === 'INVITATION_REVOKED' && invitation.value) invitation.value.status = 'revoked'
  else {
    const own = activationError(err.code)
    problem.value = own ? t(own) : errorText(e)
    if (['CODE_EXPIRED', 'CODE_NEEDED'].includes(err.code)) {
      codeOk.value = false
      code.value = ''
    }
  }
}

function startForm(email: string) {
  form.displayName = invitation.value?.displayName ?? ''
  form.username = usernameFromEmail(email)
}

onMounted(async () => {
  try {
    invitation.value = (
      await api<{ invitation: InvitationLookup }>(`invitations/lookup?t=${encodeURIComponent(token)}`)
    ).invitation
    if (invitation.value.status === 'pending') startForm(invitation.value.email)
  } catch (e) {
    show(e)
  }
})

async function sendCode() {
  problem.value = ''
  busy.value = true
  try {
    const out = await api<{ email: string; minutes: number }>('invitations/code', { method: 'POST', body: { token } })
    codeAsked.value = true
    code.value = ''
    codeNote.value = t('Te enviamos un código a {email}. Vale {minutes} minutos.', out)
    await nextTick()
    codeInput.value?.focus()
  } catch (e) {
    show(e)
  } finally {
    busy.value = false
  }
}

async function checkCode() {
  problem.value = ''
  busy.value = true
  try {
    const out = await api<{ email: string }>('invitations/code/check', {
      method: 'POST',
      body: { token, code: code.value.trim() },
    })
    codeOk.value = true
    startForm(out.email)
    if (invitation.value) invitation.value.email = out.email
  } catch (e) {
    show(e)
  } finally {
    busy.value = false
  }
}

async function createAccount() {
  problem.value = ''
  if (form.password !== form.repeat) return void (problem.value = t('Las contraseñas no coinciden.'))
  busy.value = true
  try {
    const out = await api<{ csrf: string; user: { username: string; email: string | null } }>('invitations/accept', {
      method: 'POST',
      body: {
        token,
        username: form.username,
        displayName: form.displayName,
        password: form.password,
        ...(codeOk.value ? { code: code.value.trim() } : {}),
      },
    })
    setCsrf(out.csrf)
    await session.load()
    created.value = { username: out.user.username, email: out.user.email }
  } catch (e) {
    show(e)
  } finally {
    busy.value = false
  }
}

function submit() {
  if (created.value) return
  if (step.value === 'ask-code') return sendCode()
  if (step.value === 'enter-code') return checkCode()
  if (step.value === 'form') return createAccount()
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
      <template v-else-if="step === 'ask-code' || step === 'enter-code'">
        <p class="text-sm text-stone-600">
          {{
            $t(
              'Hola {name}. Esta invitación pasó sus primeros 7 días: para comprobar que eres tú, enviaremos un código de 6 cifras a {email}.',
              { name: invitation!.displayName, email: invitation!.email },
            )
          }}
        </p>
        <button v-if="step === 'ask-code'" class="btn-primary w-full justify-center" :disabled="busy">
          {{ busy ? $t('Enviando…') : $t('Enviarme un código') }}
        </button>
        <template v-else>
          <p v-if="codeNote" class="rounded bg-brand-50 px-3 py-2 text-sm text-stone-800">{{ codeNote }}</p>
          <label class="block">
            <span class="field-label">{{ $t('Código del correo') }}</span>
            <input
              ref="codeInput"
              v-model="code"
              class="field-input w-full text-center text-lg tracking-[0.3em]"
              inputmode="numeric"
              autocomplete="one-time-code"
              pattern="\s*\d{6}\s*"
              maxlength="8"
              required
            />
          </label>
          <button class="btn-primary w-full justify-center" :disabled="busy">
            {{ busy ? $t('Un momento…') : $t('Continuar') }}
          </button>
          <button type="button" class="btn-ghost w-full justify-center text-sm" :disabled="busy" @click="sendCode">
            {{ $t('Enviarme otro código') }}
          </button>
        </template>
      </template>
      <template v-else-if="step === 'form'">
        <p class="text-sm text-stone-600">
          {{
            $t('Hola {name}. Elige cómo entrarás a la app ({email}).', {
              name: invitation!.displayName,
              email: invitation!.email,
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
      <p v-else-if="step === 'used'" class="text-sm text-stone-700">
        {{ $t('Esta invitación ya se usó.') }}
        <RouterLink to="/tablas" class="underline">{{ $t('Inicia sesión con tu usuario.') }}</RouterLink>
      </p>
      <p v-else-if="step === 'revoked'" class="text-sm text-stone-700">
        {{ $t('Esta invitación fue anulada. Si necesitas una cuenta, pide una invitación nueva a un administrador del equipo.') }}
      </p>
      <p v-else-if="step === 'invalid'" class="text-sm text-stone-700">
        {{ $t('Este enlace no es válido. Revisa que esté completo, o pide una invitación nueva a un administrador del equipo.') }}
      </p>
      <p v-if="problem" class="text-sm text-red-700">{{ problem }}</p>
    </form>
  </div>
</template>
