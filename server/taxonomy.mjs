// The Wings Gallery's rule for "the prediction differs from the recorded species"
// (Shiny_Ikiam_Wings_Gallery src/utils/taxonomy.js: recordedTaxonomy,
// predictionRank, rankComparison, predictionDiffers), for one sheet row.
// Recorded truth comes from the row; the prediction is model output only.

export const TAXON_ALIASES = Object.freeze({
  'vanessa brasiliensis': 'Vanessa braziliensis',
  'oleria onega jaranilla': 'Oleria onega janarilla',
});

function clean(value) {
  if (value === null || value === undefined) return '';
  const text = String(value).trim().replace(/\s+/g, ' ');
  return ['', 'na', 'none', 'null'].includes(text.toLowerCase()) ? '' : text;
}
export function canonicalTaxon(value) {
  const text = clean(value);
  return TAXON_ALIASES[text.toLowerCase()] || text;
}
const first = (item, ...keys) => keys.map(k => clean(item?.[k])).find(Boolean) || '';

/** Genus, species and subspecies of a Collection_data / Insectary_data row. */
export function recordedTaxonomy(item = {}) {
  const full = first(item, 'SPECIES', 'Stock_of_origin');
  const parts = full.split(/\s+/);
  let species = first(item, 'Species');
  let subspecies = first(item, 'Subspecies_Form');
  if (!species && parts.length >= 2) species = parts.slice(0, 2).join(' ');
  if (!subspecies && parts.length >= 3) subspecies = parts.slice(2).join(' ');
  const genus = canonicalTaxon(first(item, 'Genus') || species.split(/\s+/)[0] || parts[0] || '');
  species = canonicalTaxon(species);
  subspecies = canonicalTaxon(subspecies);
  if (species && genus && species.toLowerCase().startsWith(`${genus.toLowerCase()} `))
    species = `${genus} ${species.slice(genus.length + 1).trim()}`;
  if (subspecies && species && !subspecies.toLowerCase().startsWith(`${species.toLowerCase()} `))
    subspecies = `${species} ${subspecies}`;
  return { genus, species, subspecies: canonicalTaxon(subspecies) };
}

/** The model's top label of a rank: [[label, confidence, …], …] as stored per CAM. */
export function predictionRank(pred, rank) {
  const row = Array.isArray(pred?.[rank]) ? pred[rank][0] : null;
  if (!Array.isArray(row) || !row[0]) return null;
  return {
    label: canonicalTaxon(row[0]),
    rawLabel: clean(row[0]),
    confidence: typeof row[1] === 'number' ? row[1] : null,
  };
}

export function rankComparison(item, pred, rank = 'species') {
  const recorded = recordedTaxonomy(item)[rank];
  const model = predictionRank(pred, rank);
  if (!model || !recorded)
    return { rank, recorded, predicted: model?.label || '', confidence: model?.confidence ?? null, status: 'missing' };
  const equal = model.label.toLowerCase() === recorded.toLowerCase();
  return {
    rank,
    recorded,
    predicted: model.label,
    confidence: model.confidence,
    status: equal
      ? model.rawLabel.toLowerCase() !== recorded.toLowerCase()
        ? 'synonym-only'
        : 'agreement'
      : 'disagreement',
  };
}

export const predictionDiffers = (item, pred, rank = 'species') =>
  rankComparison(item, pred, rank).status === 'disagreement';
