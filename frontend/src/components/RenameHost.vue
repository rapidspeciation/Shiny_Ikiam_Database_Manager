<script setup lang="ts">
import { onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { api } from '../lib/api'
import { blobBase64, renameHost, type RenameState } from '../lib/renameHost'

/**
 * The Rename Photos app's frame for the whole session (lib/renameHost.ts): fixed over the
 * «Fotos» tab's slot, hidden (not unloaded) while another tab is open, so the folder
 * and its readings stay.
 * It is also the link between that app and this one's server (server/rename-photos.mjs):
 *  - the workbook's IDs go to the frame (its reader's "is this ID in the database" rule);
 *  - the batch the frame tells (each photo's file, ID, flags, rotation) goes to the server;
 *  - what the assistant asks of the batch (pictures of photos, corrections to rows) is taken
 *    from the server, done by the frame, and answered. The photos themselves stay on the
 *    computer: only the small pictures the assistant asks for are sent.
 * The frame's messages are the app's embedded mode (rename_photos_AI web-app/src/lib/embed/host.ts).
 */
const SRC = 'rename-app/index.html?embed'
const frame = ref<HTMLIFrameElement>()
const box = ref({ left: 0, top: 0, width: 0, height: 0 })
const shown = ref(false)
const tab = `${Date.now().toString(36)}-${Math.random().toString(36).slice(2, 10)}`
renameHost.tab = tab
let observer: ResizeObserver | undefined
let alive = true
let looping = false
const stop = new AbortController()

function measure() {
  const r = renameHost.slot?.getBoundingClientRect()
  shown.value = !!r && r.width > 0 && r.height > 0
  if (r && shown.value) box.value = { left: r.left, top: r.top, width: r.width, height: r.height }
}
watch(
  () => renameHost.slot,
  slot => {
    observer?.disconnect()
    if (slot) {
      observer = new ResizeObserver(measure)
      observer.observe(slot)
    }
    measure()
  },
  { immediate: true, flush: 'post' },
)

const say = (message: Record<string, unknown>) =>
  frame.value?.contentWindow?.postMessage({ source: 'rename-host', ...message }, location.origin)

// ------------------------------------------------------------ the batch, told to the server
let latest: RenameState | null = null
let sending = false
async function tell() {
  if (sending || !latest) return
  sending = true
  try {
    while (latest && alive) {
      const state = latest
      latest = null
      await api('rename/state', { method: 'POST', body: { tab, state } })
    }
  } catch {
    // Told again with the next change, or when the server asks for work.
  } finally {
    sending = false
  }
}

// ------------------------------------------------------------ what the assistant asks
const answers = new Map<string, (out: { ok: boolean; data?: unknown; error?: string }) => void>()
function ask(id: string, name: string, args: unknown) {
  return new Promise<{ ok: boolean; data?: unknown; error?: string }>(resolve => {
    const timer = setTimeout(() => {
      answers.delete(id)
      resolve({ ok: false, error: 'The Rename Photos app did not answer.' })
    }, 80_000)
    answers.set(id, out => {
      clearTimeout(timer)
      answers.delete(id)
      resolve(out)
    })
    say({ type: 'command', id, name, args })
  })
}
async function work() {
  if (looping) return
  looping = true
  while (alive) {
    try {
      const { commands } = await api<{ commands: { id: string; name: string; args: unknown }[] }>(`rename/commands?tab=${tab}`, { signal: stop.signal })
      for (const command of commands) {
        const out = await ask(command.id, command.name, command.args)
        // Pictures go as text in the answer.
        const data = out.data as { images?: { files: string[]; blob: Blob }[] } | undefined
        const images = data?.images ? await Promise.all(data.images.map(async i => ({ files: i.files, jpeg: await blobBase64(i.blob) }))) : undefined
        await api('rename/result', {
          method: 'POST',
          body: { tab, id: command.id, ok: out.ok, error: out.error, data: images ? { ...data, images } : out.data },
        })
      }
    } catch {
      if (!alive) break
      await new Promise(resolve => setTimeout(resolve, 4000))
    }
  }
}

// The page asking the frame's app to show a part of it (the review, when a chat about the batch starts).
watch(
  () => renameHost.show?.at,
  () => renameHost.show && say({ type: 'show', view: renameHost.show.view }),
)

async function hear(event: MessageEvent) {
  if (event.origin !== location.origin || event.source !== frame.value?.contentWindow) return
  const m = event.data as { source?: string; type?: string; id?: string; ok?: boolean; data?: unknown; error?: string; state?: RenameState } | null
  if (!m || m.source !== 'rename-photos') return
  if (m.type === 'ready') {
    renameHost.ready = true
    void work()
    try {
      say({ type: 'camids', ids: (await api<{ ids: string[] }>('rename/ids')).ids })
    } catch {
      // Without them the frame's app reads the IDs from the workbook's public copy, as on its own site.
    }
  } else if (m.type === 'state' && m.state) {
    renameHost.state = m.state
    latest = m.state
    void tell()
  } else if (m.type === 'result' && m.id) {
    answers.get(m.id)?.({ ok: m.ok !== false, data: m.data, error: m.error })
  }
}

onMounted(async () => {
  addEventListener('message', hear)
  addEventListener('resize', measure)
  // Not built on this server: the tab says so instead of showing a "not found" page.
  try {
    const found = await fetch(SRC, { method: 'HEAD', credentials: 'same-origin' })
    renameHost.missing = !found.ok
  } catch {
    renameHost.missing = true
  }
})
onBeforeUnmount(() => {
  alive = false
  stop.abort()
  observer?.disconnect()
  removeEventListener('message', hear)
  removeEventListener('resize', measure)
  Object.assign(renameHost, { ready: false, state: null })
})
</script>

<template>
  <div
    v-if="!renameHost.missing"
    class="fixed"
    :class="{ invisible: !shown, 'pointer-events-none': !shown || renameHost.passThrough }"
    :style="{ left: `${box.left}px`, top: `${box.top}px`, width: `${box.width}px`, height: `${box.height}px` }"
    :aria-hidden="!shown"
  >
    <iframe ref="frame" :src="SRC" class="h-full w-full border-0" :title="$t('Renombrar fotos')" @load="say({ type: 'hello' })" />
  </div>
</template>
