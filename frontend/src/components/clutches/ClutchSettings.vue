<script setup lang="ts">
import { onBeforeUnmount, onMounted, ref } from 'vue'
import { Settings, X } from 'lucide-vue-next'
import { api } from '../../lib/api'
import { applyClutchSettings, clutchSettings } from '../../lib/clutchSettings'
import { errorText, notify } from '../../lib/notice'
import { useSession } from '../../stores/session'
import { t } from '../../lib/i18n'

/**
 * The Clutches tab's settings: one team convention, whether preserved larvae
 * (and eggs, pupae) are taken off NUMBER OF LARVAE or stay counted in it.
 * Clutches (−N → preserved) and Emergidos (larvae preserved from a clutch)
 * follow it. Everyone sees it explained; only an administrator changes it.
 */
const emit = defineEmits<{ close: [] }>()
const session = useSession()
const saving = ref(false)
async function choose(subtractPreserved: boolean) {
  if (!session.isAdmin || subtractPreserved === clutchSettings.subtractPreserved) return
  saving.value = true
  try {
    applyClutchSettings(await api<{ subtractPreserved: boolean }>('clutches/settings', { method: 'PUT', body: { subtractPreserved } }))
    notify(t('Ajuste guardado: vale para todo el equipo'), 'success')
  } catch (e) {
    notify(errorText(e), 'error')
  } finally {
    saving.value = false
  }
}
const onKey = (e: KeyboardEvent) => e.key === 'Escape' && emit('close')
onMounted(() => window.addEventListener('keydown', onKey))
onBeforeUnmount(() => window.removeEventListener('keydown', onKey))
const OPTIONS = [
  {
    value: true,
    title: () => t('Sí: restarlas (como hasta ahora)'),
    text: () => t('NUMBER OF LARVAE guarda las larvas vivas en la jaula. Una larva preservada se resta, como una muerta (−1), y queda registrada aparte como preservada.'),
  },
  {
    value: false,
    title: () => t('No: dejarlas contadas'),
    text: () => t('NUMBER OF LARVAE guarda las larvas que sobrevivieron (vivas + preservadas). Solo las que murieron o desaparecieron se restan; las preservadas quedan registradas aparte.'),
  },
]
</script>

<template>
  <div class="fixed inset-0 z-50 flex items-end justify-center bg-black/30 sm:items-center" @click.self="emit('close')">
    <section class="max-h-full w-full overflow-y-auto rounded-t-2xl bg-white p-4 shadow-xl sm:max-w-lg sm:rounded-2xl" role="dialog" :aria-label="$t('Ajustes de Clutches')">
      <header class="flex items-center gap-2">
        <Settings :size="20" class="text-stone-600" />
        <h2 class="flex-1 text-lg font-semibold">{{ $t('Ajustes de Clutches') }}</h2>
        <button class="btn-ghost h-11 w-11 justify-center" :aria-label="$t('Cerrar')" @click="emit('close')"><X :size="22" /></button>
      </header>
      <h3 class="mt-3 font-medium">{{ $t('Larvas preservadas: ¿restarlas de NUMBER OF LARVAE?') }}</h3>
      <p class="mt-1 text-sm text-stone-600">
        {{ $t('Una sola regla para todo el equipo, en Clutches y en Emergidos. Vale igual para huevos y pupas preservados.') }}
      </p>
      <div class="mt-3 space-y-2" role="radiogroup">
        <button
          v-for="o in OPTIONS"
          :key="String(o.value)"
          type="button"
          role="radio"
          class="block w-full rounded-xl border px-3 py-2 text-left"
          :class="clutchSettings.subtractPreserved === o.value ? 'border-brand-700 bg-brand-50 ring-2 ring-brand-100' : 'border-stone-300 bg-white'"
          :aria-checked="clutchSettings.subtractPreserved === o.value"
          :disabled="!session.isAdmin || saving"
          @click="choose(o.value)"
        >
          <span class="block font-medium">{{ o.title() }}</span>
          <span class="mt-0.5 block text-sm text-stone-600">{{ o.text() }}</span>
        </button>
      </div>
      <p class="mt-3 text-sm text-stone-600">
        {{ $t('Con los eventos registrados aparte, la app muestra los dos números en cada clutch: vivas en la jaula y sobrevivieron (vivas + preservadas).') }}
      </p>
      <p v-if="!session.isAdmin" class="mt-2 rounded-md bg-stone-100 px-3 py-2 text-sm text-stone-700">{{ $t('Solo un administrador puede cambiarlo.') }}</p>
    </section>
  </div>
</template>
