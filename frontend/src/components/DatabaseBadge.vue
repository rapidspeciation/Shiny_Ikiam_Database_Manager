<script setup lang="ts">
import { ExternalLink, FileSpreadsheet } from 'lucide-vue-next'
import { useSession } from '../stores/session'

/**
 * Which database is open, at every width: the lab's offline copy («Copia de prueba», shortened on phones) or a link
 * to the team's Google Sheet. `surface` for a light bar (Cambios propuestos on its own page); the header is green.
 */
withDefaults(defineProps<{ tone?: 'brand' | 'surface' }>(), { tone: 'brand' })
const session = useSession()
</script>

<template>
  <span
    v-if="session.settings?.localMode"
    class="inline-flex shrink-0 items-center gap-1 rounded bg-amber-300 px-2 py-0.5 text-xs font-semibold whitespace-nowrap text-amber-950"
    :title="$t('Copia sin conexión: los cambios se guardan solo aquí, nunca en la hoja del equipo')"
    data-database="local"
  >
    <span class="sm:hidden">{{ $t('Prueba') }}</span><span class="max-sm:hidden">{{ $t('Copia de prueba') }}</span>
  </span>
  <a
    v-else-if="session.settings?.sheetUrl"
    :href="session.settings.sheetUrl"
    target="_blank"
    rel="noopener"
    class="hit-target inline-flex shrink-0 items-center gap-1 rounded px-2 py-0.5 text-xs font-semibold whitespace-nowrap ring-1"
    :class="
      tone === 'brand'
        ? 'text-on-brand-muted ring-on-brand-muted/50 hover:bg-brand-hover hover:text-white'
        : 'text-brand-text ring-control hover:bg-surface-sunken'
    "
    :title="$t('Abrir Google Sheet')"
    :aria-label="$t('Abrir Google Sheet')"
    data-database="sheet"
  >
    <FileSpreadsheet :size="13" class="sm:hidden" aria-hidden="true" />
    <span class="sm:hidden">Sheet</span><span class="max-sm:hidden">Google Sheet</span>
    <ExternalLink :size="12" aria-hidden="true" />
  </a>
</template>
