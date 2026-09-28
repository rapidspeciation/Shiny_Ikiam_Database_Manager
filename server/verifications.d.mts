export type ListRule = { values?: string[]; from?: [string, string]; strict?: boolean }
export const UNIQUE: Record<string, string[]>
export const TUBE_FIELD: RegExp
export const LISTS: Record<string, Record<string, ListRule>>
export function isUnique(sheet: string, field: string): boolean
export function blankOrNA(value: unknown): boolean
export function isIdValue(value: unknown): boolean
