import { ref, watch, type Ref } from 'vue'

/** A ref kept in sessionStorage (or localStorage), so a tab's selections survive switching tabs or reloading. */
export function persistentRef<T>(key: string, initial: T, { lasting = false } = {}): Ref<T> {
  // Most screen state lives as long as the tab (sessionStorage); `lasting` keeps
  // it in this browser even after the tab is closed (localStorage).
  const storage = lasting ? localStorage : sessionStorage
  let start = initial
  try {
    const saved = storage.getItem(`ithomiini:${key}`)
    if (saved !== null) start = JSON.parse(saved)
  } catch {
    /* ignore */
  }
  const value = ref(start) as Ref<T>
  watch(value, v => storage.setItem(`ithomiini:${key}`, JSON.stringify(v)), { deep: true })
  return value
}
