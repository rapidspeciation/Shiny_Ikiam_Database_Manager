// Censo (views/CensusView.vue, components/census) and the Insectary ID matcher
// shared with Muertes (lib/idMatch.ts, components/IdSuggestion.vue, IdFilters.vue).
export default {
  // The ID matcher
  'Insectary ID, CAM o tubo (A?B, A[16]B)': 'Insectary ID, CAM or tube (A?B, A[16]B)',
  'Los caracteres en ámbar se leyeron como parecidos': 'The characters in amber were read as look-alikes',
  'Cualquier sexo': 'Any sex',
  'Sexo desconocido': 'Sex unknown',
  'Sexo visto': 'Sex seen',
  'Especie vista': 'Species seen',
  'Cualquier especie': 'Any species',
} as Record<string, string>
