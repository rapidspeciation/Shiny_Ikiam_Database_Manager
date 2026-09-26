<script setup lang="ts">
import { onMounted, reactive, ref } from 'vue'
import { UserPlus } from 'lucide-vue-next'
import { api, requestId } from '../lib/api'
import { errorText, notify } from '../lib/notice'
import type { User } from '../lib/types'
import { useSession } from '../stores/session'

/** Administrators create accounts for the team and set what each person may do. */
const session = useSession()
const users = ref<User[]>([])
const form = reactive({ username: '', displayName: '', password: '', role: 'editor' })
const ROLES: Record<string, string> = {
  observer: 'Solo lectura',
  editor: 'Editor',
  reviewer: 'Revisor',
  admin: 'Administrador',
}

async function load() {
  try {
    users.value = (await api<{ users: User[] }>('admin/users')).users
  } catch (e) {
    notify(errorText(e), 'error')
  }
}
onMounted(load)

async function create() {
  try {
    await api('admin/users', { method: 'POST', body: { ...form, requestId: requestId() } })
    notify(`Cuenta ${form.username} creada`, 'success')
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
  const password = prompt(`Nueva contraseña para ${user.username} (6 a 16 caracteres)`)
  if (password) {
    await update(user, { password })
    notify('Contraseña cambiada; la persona debe volver a iniciar sesión', 'success')
  }
}
</script>

<template>
  <div class="h-full overflow-y-auto p-4">
    <p v-if="!session.isAdmin" class="text-stone-500">Solo un administrador puede gestionar cuentas.</p>
    <template v-else>
      <h1 class="mb-3 text-lg font-semibold">Usuarios</h1>
      <form class="mb-6 flex flex-wrap items-end gap-3 rounded-lg border border-stone-200 bg-white p-4" @submit.prevent="create">
        <label>
          <span class="field-label">Usuario</span>
          <input v-model="form.username" class="field-input" autocomplete="off" autocapitalize="none" required />
        </label>
        <label>
          <span class="field-label">Nombre visible</span>
          <input v-model="form.displayName" class="field-input" />
        </label>
        <label>
          <span class="field-label">Contraseña inicial</span>
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
          <span class="field-label">Permiso</span>
          <select v-model="form.role" class="field-input">
            <option v-for="(label, key) in ROLES" :key="key" :value="key">{{ label }}</option>
          </select>
        </label>
        <button class="btn-primary"><UserPlus :size="15" /> Crear cuenta</button>
      </form>
      <table class="w-full max-w-3xl text-sm">
        <thead class="text-left text-xs text-stone-600">
          <tr>
            <th class="py-2">Usuario</th>
            <th>Nombre</th>
            <th>Permiso</th>
            <th>Activa</th>
            <th></th>
          </tr>
        </thead>
        <tbody>
          <tr v-for="u in users" :key="u.id" class="border-t border-stone-200">
            <td class="py-2">{{ u.username }}</td>
            <td>{{ u.displayName }}</td>
            <td>
              <select
                :value="u.role"
                class="field-input w-40"
                @change="update(u, { role: ($event.target as HTMLSelectElement).value })"
              >
                <option v-for="(label, key) in ROLES" :key="key" :value="key">{{ label }}</option>
              </select>
            </td>
            <td><input type="checkbox" :checked="u.active" @change="update(u, { active: !u.active })" /></td>
            <td><button class="text-xs underline" @click="resetPassword(u)">Cambiar contraseña</button></td>
          </tr>
        </tbody>
      </table>
    </template>
  </div>
</template>
