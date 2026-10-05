import { computed, ref } from 'vue'
import { api } from '../lib/api'
import { lookAlikeTable } from '../lib/idMatch'

const learned = ref<[string, string][]>([])
let asked = false

/**
 * The look-alike table (lib/idMatch.ts) with the pairs the team corrected in
 * Insectary IDs (from the history, GET /api/census/lookalikes), asked once per page.
 */
export function useLookAlikes() {
  if (!asked) {
    asked = true
    api<{ pairs: { a: string; b: string }[] }>('census/lookalikes')
      .then(r => (learned.value = r.pairs.map(p => [p.a, p.b] as [string, string])))
      .catch(() => (asked = false))
  }
  return computed(() => lookAlikeTable(learned.value))
}
