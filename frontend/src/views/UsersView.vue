<script setup lang="ts">
import ChoiceField from '../components/ChoiceField.vue'
import { computed, onMounted, reactive, ref } from 'vue'
import { Copy, Mail, RefreshCw, UserPlus, X } from 'lucide-vue-next'
import { api, requestId } from '../lib/api'
import { errorText, notify } from '../lib/notice'
import type { User } from '../lib/types'
import { useSession } from '../stores/session'
import { intlLocale, t } from '../lib/i18n'

/**
 * Administrators invite people by email (they choose their own username and
 * password from the link) and set what each person may do.
 */
interface Invitation {
  id: string
  email: string
  displayName: string
  role: string
  createdAt: string
  expiresAt: string
  sentAt: string | null
  sendError: string | null
  usedAt: string | null
  status: 'pending' | 'used' | 'expired'
}

const session = useSession()
const users = ref<User[]>([])
const invitations = ref<Invitation[]>([])
const links = ref<Record<string, string>>({})
const sending = ref(false)
const invite = reactive({ email: '', displayName: '', role: 'editor' })
const form = reactive({ username: '', displayName: '', password: '', role: 'editor' })
const ROLES: Record<string, string> = {
  observer: 'Solo lectura',
  editor: 'Editor',
  reviewer: 'Revisor',
  admin: 'Administrador',
}
const roleChoices = computed(() => Object.entries(ROLES).map(([value, label]) => ({ value, label: t(label) })))
const STATUS: Record<Invitation['status'], string> = { pending: 'Esperando', used: 'Cuenta creada', expired: 'Vencida' }
const day = (iso: string | null) =>
  iso ? new Date(iso).toLocaleDateString(intlLocale(), { day: 'numeric', month: 'short' }) : ''

async function load() {
  try {
    const [u, i] = await Promise.all([
      api<{ users: User[] }>('admin/users'),
      api<{ invitations: Invitation[] }>('admin/invitations'),
    ])
    users.value = u.users
    invitations.value = i.invitations
  } catch (e) {
    notify(errorText(e), 'error')
  }
}
onMounted(load)

function after(out: { invitation: Invitation; link: string }) {
  links.value[out.invitation.id] = out.link
  if (out.invitation.sentAt && !out.invitation.sendError)
    notify(t('Invitación enviada a {email}', { email: out.invitation.email }), 'success')
  else
    notify(
      t('No se pudo enviar el correo ({error}). Copia el enlace y compártelo.', { error: out.invitation.sendError }),
      'error',
    )
}
async function sendInvite() {
  sending.value = true
  try {
    after(await api('admin/invitations', { method: 'POST', body: { ...invite } }))
    Object.assign(invite, { email: '', displayName: '', role: 'editor' })
    await load()
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    sending.value = false
  }
}
async function resend(i: Invitation) {
  try {
    after(await api(`admin/invitations/${i.id}/resend`, { method: 'POST', body: {} }))
    await load()
  } catch (e) {
    notify(errorText(e), 'error')
  }
}
async function revoke(i: Invitation) {
  if (!confirm(t('¿Anular la invitación a {email}?', { email: i.email }))) return
  await api(`admin/invitations/${i.id}/revoke`, { method: 'POST', body: {} }).catch(e => notify(errorText(e), 'error'))
  await load()
}
async function copy(id: string) {
  await navigator.clipboard.writeText(links.value[id])
  notify(t('Enlace copiado'))
}

async function create() {
  try {
    await api('admin/users', { method: 'POST', body: { ...form, requestId: requestId() } })
    notify(t('Cuenta {username} creada', { username: form.username }), 'success')
    Object.assign(form, { username: '', displayName: '', password: '', role: 'editor' })
    await load()
  } catch (e) {
    notify(errorText(e), 'error')
  }
}
async function update(user: User, changes: Record<string, unknown>) {
  try {
    await api(`admin/users/${user.id}`, { method: 'PATCH', body: { ...changes, requestId: requestId() } })
    await load()
  } catch (e) {
    notify(errorText(e), 'error')
    await load()
  }
}
async function resetPassword(user: User) {
  const password = prompt(t('Nueva contraseña para {username} (6 a 16 caracteres)', { username: user.username }))
  if (password) {
    await update(user, { password })
    notify(t('Contraseña cambiada; la persona debe volver a iniciar sesión'), 'success')
  }
}
</script>

<template>
  <div class="h-full overflow-y-auto p-4">
    <p v-if="!session.isAdmin" class="text-stone-500">{{ $t('Solo un administrador puede gestionar cuentas.') }}</p>
    <template v-else>
      <h1 class="mb-3 text-lg font-semibold">{{ $t('Usuarios') }}</h1>
      <form
        class="mb-3 flex flex-wrap items-end gap-3 rounded-lg border border-stone-200 bg-white p-4"
        @submit.prevent="sendInvite"
      >
        <label>
          <span class="field-label">{{ $t('Correo') }}</span>
          <input v-model="invite.email" class="field-input w-64" type="email" autocomplete="off" required />
        </label>
        <label>
          <span class="field-label">{{ $t('Nombre') }}</span>
          <input v-model="invite.displayName" class="field-input" required />
        </label>
        <label>
          <span class="field-label">{{ $t('Permiso') }}</span>
          <ChoiceField v-model="invite.role" class="field-input" :options="roleChoices" :freetext="false" />
        </label>
        <button class="btn-primary" :disabled="sending">
          <Mail :size="15" /> {{ sending ? $t('Enviando…') : $t('Enviar invitación') }}
        </button>
        <p class="hint w-full">
          {{
            $t(
              'Llega un correo desde {from} con un enlace para que la persona elija su usuario y contraseña. El enlace vale 7 días.',
              { from: 'jmithominii@gmail.com' },
            )
          }}
        </p>
      </form>

      <table v-if="invitations.length" class="mb-6 w-full max-w-4xl text-sm">
        <thead class="text-left text-xs text-stone-600">
          <tr>
            <th class="py-2">{{ $t('Invitación') }}</th>
            <th>{{ $t('Permiso') }}</th>
            <th>{{ $t('Estado') }}</th>
            <th></th>
          </tr>
        </thead>
        <tbody>
          <tr v-for="i in invitations" :key="i.id" class="border-t border-stone-200">
            <td class="py-2">
              {{ i.displayName }} <span class="text-stone-500">&lt;{{ i.email }}&gt;</span>
            </td>
            <td>{{ ROLES[i.role] ? $t(ROLES[i.role]) : i.role }}</td>
            <td>
              {{ $t(STATUS[i.status]) }}
              <span class="hint">
                <template v-if="i.status === 'pending'"
                  >· {{ $t('enviada {sent}, vence {expires}', { sent: day(i.sentAt), expires: day(i.expiresAt) }) }}</template
                >
                <template v-else-if="i.status === 'used'">· {{ day(i.usedAt) }}</template>
              </span>
              <span v-if="i.sendError && i.status === 'pending'" class="block text-xs text-red-700">
                {{ $t('Correo no enviado: {error}', { error: i.sendError }) }}
              </span>
            </td>
            <td class="whitespace-nowrap text-right">
              <template v-if="i.status !== 'used'">
                <button v-if="links[i.id]" class="btn-ghost" :title="$t('Copiar enlace')" @click="copy(i.id)">
                  <Copy :size="14" />
                </button>
                <button
                  class="btn-ghost"
                  :title="$t('Enviar de nuevo (el enlace anterior deja de funcionar)')"
                  @click="resend(i)"
                >
                  <RefreshCw :size="14" />
                </button>
                <button v-if="i.status === 'pending'" class="btn-ghost" :title="$t('Anular')" @click="revoke(i)">
                  <X :size="14" />
                </button>
              </template>
            </td>
          </tr>
        </tbody>
      </table>

      <table class="w-full max-w-4xl text-sm">
        <thead class="text-left text-xs text-stone-600">
          <tr>
            <th class="py-2">{{ $t('Usuario') }}</th>
            <th>{{ $t('Nombre') }}</th>
            <th>{{ $t('Correo') }}</th>
            <th>{{ $t('Permiso') }}</th>
            <th>{{ $t('Activa') }}</th>
            <th></th>
          </tr>
        </thead>
        <tbody>
          <tr v-for="u in users" :key="u.id" class="border-t border-stone-200">
            <td class="py-2">{{ u.username }}</td>
            <td>{{ u.displayName }}</td>
            <td class="text-stone-600">{{ u.email }}</td>
            <td>
              <ChoiceField
                :model-value="u.role"
                class="field-input w-40"
                :options="roleChoices"
                :freetext="false"
                @update:model-value="update(u, { role: $event })"
              />
            </td>
            <td><input type="checkbox" :checked="u.active" @change="update(u, { active: !u.active })" /></td>
            <td>
              <button class="text-xs underline" @click="resetPassword(u)">{{ $t('Cambiar contraseña') }}</button>
            </td>
          </tr>
        </tbody>
      </table>

      <details class="mt-6 max-w-4xl">
        <summary class="cursor-pointer text-sm text-stone-600">
          {{ $t('Crear una cuenta sin correo (con contraseña inicial)') }}
        </summary>
        <form
          class="mt-2 flex flex-wrap items-end gap-3 rounded-lg border border-stone-200 bg-white p-4"
          @submit.prevent="create"
        >
          <label>
            <span class="field-label">{{ $t('Usuario') }}</span>
            <input v-model="form.username" class="field-input" autocomplete="off" autocapitalize="none" required />
          </label>
          <label>
            <span class="field-label">{{ $t('Nombre visible') }}</span>
            <input v-model="form.displayName" class="field-input" />
          </label>
          <label>
            <span class="field-label">{{ $t('Contraseña inicial') }}</span>
            <input
              v-model="form.password"
              class="field-input"
              type="text"
              autocomplete="off"
              minlength="6"
              maxlength="16"
              required
            />
          </label>
          <label>
            <span class="field-label">{{ $t('Permiso') }}</span>
            <ChoiceField v-model="form.role" class="field-input" :options="roleChoices" :freetext="false" />
          </label>
          <button class="btn"><UserPlus :size="15" /> {{ $t('Crear cuenta') }}</button>
        </form>
      </details>
    </template>
  </div>
</template>
