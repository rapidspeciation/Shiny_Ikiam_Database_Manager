import { computed, ref } from 'vue'
import { isBlank } from '../lib/cells'
import { errorText, notify } from '../lib/notice'
import { useTables } from '../stores/tables'
import { t } from '../lib/i18n'

/** A butterfly that can be a parent: its species and sex (Insectary_data). */
export interface Parent {
  id: string
  species: string
  sex: string
}

/**
 * The insectary's butterflies, to choose a clutch's mother and father by
 * Insectary ID (newest first, females first for ♀ and males for ♂). The sheet
 * (13k rows) is asked for only when the parents are being written.
 */
export function useParents() {
  const tables = useTables()
  const loading = computed(() => !!tables.loading.Insectary_data)
  const wanted = ref(false)
  async function load() {
    wanted.value = true
    try {
      await tables.load('Insectary_data')
    } catch (e) {
      notify(errorText(e), 'error')
    }
  }
  const byId = computed(() => {
    void tables.versions.Insectary_data
    const out = new Map<string, Parent>()
    const table = wanted.value ? tables.tables.Insectary_data : undefined
    if (!table) return out
    for (let i = table.rows.length - 1; i >= 0; i--) {
      const v = table.rows[i].values
      if (!table.rows[i].observed || isBlank(v.Insectary_ID)) continue
      const id = String(v.Insectary_ID).trim().toUpperCase()
      if (!out.has(id)) out.set(id, { id, species: isBlank(v.SPECIES) ? '' : String(v.SPECIES), sex: isBlank(v.Sex) ? '' : String(v.Sex) })
    }
    return out
  })
  const ids = (sex: 'female' | 'male') => {
    const all = [...byId.value.values()]
    return [...all.filter(p => p.sex.toLowerCase() === sex), ...all.filter(p => p.sex.toLowerCase() !== sex)].map(p => p.id)
  }
  const females = computed(() => ids('female'))
  const males = computed(() => ids('male'))
  const find = (id: string) => byId.value.get(id.trim().toUpperCase())
  /** Why a parent looks wrong: unknown ID, or the wrong sex (the female is written first). */
  function warning(id: string, sex: 'female' | 'male'): string {
    if (!id.trim() || !byId.value.size) return ''
    const p = find(id)
    if (!p) return t('{id} no está en Insectary_data', { id: id.trim().toUpperCase() })
    const s = p.sex.toLowerCase()
    if (s && s !== sex) return sex === 'female' ? t('{id} es {sex}: la hembra va primero', { id: p.id, sex: p.sex }) : t('{id} es {sex}', { id: p.id, sex: p.sex })
    return ''
  }
  /**
   * The clutch's species from its parents: the mother's, or the hybrid's list
   * name ("mother x father's subspecies") when they differ and the list has it.
   */
  function speciesFrom(female: string, male: string, list: string[]): string {
    const mother = find(female)?.species || ''
    const father = find(male)?.species || ''
    if (!mother || !father || mother === father) return mother
    const hybrid = `${mother} x ${father.split(' ').at(-1)}`
    return list.includes(hybrid) ? hybrid : mother
  }
  return { load, loading, loaded: computed(() => byId.value.size > 0), females, males, find, warning, speciesFrom }
}
