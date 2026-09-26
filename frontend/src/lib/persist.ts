import { ref, watch, type Ref } from 'vue'

/** A ref kept in sessionStorage, so a tab's selections survive switching tabs or reloading. */
export function persistentRef<T>(key: string, initial: T): Ref<T> {
  let start = initial
  try {
    const saved = sessionStorage.getItem(`ithomiini:${key}`)
    if (saved !== null) start = JSON.parse(saved)
  } catch {
    /* ignore */
  }
  const value = ref(start) as Ref<T>
  watch(value, v => sessionStorage.setItem(`ithomiini:${key}`, JSON.stringify(v)), { deep: true })
  return value
}
