// The two rows of one wild butterfly (Collection_data and Insectary_data, same
// Insectary_ID) that disagree on species or sex: the checks' link_mismatch.
// Which side is right needs the envelope, so most suggestions are to check. The
// suggestion is likely when one side was corrected later (the history has an
// edit that replaced a value there after the other side was last written) or
// when the envelope read from the photos says the same as one side. Otherwise
// the Insectary row is suggested to follow the Collection row, the field record
// it was copied from. Left out: pairs whose dates are more than 3 days apart or
// that disagree on both species and sex (the old letter cycle gave the same ID
// to two butterflies: the "twin" is probably another butterfly and neither value
// is wrong), and species the other sheet's list does not have.

import { msg, tpl } from '../messages.mjs';
import { listOptions } from '../verify.mjs';

const text = value => (value === null || value === undefined ? '' : String(value).trim());
const isDate = value => typeof value === 'number' && Number.isFinite(value) && value > 0;
const words = value => text(value).split(/\s+/).filter(Boolean);
const binomial = value => words(value).slice(0, 2).join(' ').toLowerCase();
const blank = value => /^(|NA|N\/A)$/i.test(text(value));
const sexOf = value => {
  const s = text(value)
    .toLowerCase()
    .replace(/[_\s]*\?$/, '')
    .trim();
  return s === 'female' || s === 'male' ? s : null;
};

/** A real correction: a value replaced by another (not the row's first writing). */
const corrected = edit => !!edit && !blank(edit.before) && text(edit.before) !== text(edit.after);

export default {
  id: 'twins',
  title: tpl('Colecta e insectario no coinciden'),
  describe: tpl(
    'Especie o sexo distintos entre la fila de Collection_data y la de Insectary_data de la misma mariposa silvestre. Probable cuando un lado se corrigió después (historial) o el sobre de las fotos dice lo mismo que un lado; si no, revisar: se sugiere que Insectary_data siga a Collection_data. Se omiten los pares con fechas a más de 3 días (un ID viejo repetido: otra mariposa).',
  ),
  suggest(ctx) {
    const issues = ctx.issues();
    // What the envelopes say, per row (the photo checks: envelope_sex, envelope_species).
    const envelope = new Map();
    for (const i of issues)
      if ((i.kind === 'envelope_sex' || i.kind === 'envelope_species') && i.recordId && i.ocr?.read)
        envelope.set(`${i.recordId}:${i.kind === 'envelope_sex' ? 'Sex' : 'SPECIES'}`, i.ocr.read);
    const mismatches = issues.filter(i => i.kind === 'link_mismatch' && (i.field === 'SPECIES' || i.field === 'Sex'));
    const pairKey = i => `${i.recordId}:${i.related?.[0]?.recordId}`;
    const fields = new Map();
    for (const i of mismatches) fields.set(pairKey(i), (fields.get(pairKey(i)) ?? 0) + 1);
    // The species each sheet accepts (Insectary_data: Lists Insectary_species; Collection_data: the Taxonomy).
    const accepted = sheet => new Set([...(listOptions(ctx.store, sheet).SPECIES?.values ?? [])].map(v => text(v).toLowerCase()));
    const speciesLists = { Insectary_data: accepted('Insectary_data'), Collection_data: accepted('Collection_data') };
    const out = [];
    for (const issue of mismatches) {
      if (fields.get(pairKey(issue)) > 1) continue;
      const ins = ctx.byId.get(issue.recordId);
      const col = ctx.byId.get(issue.related?.[0]?.recordId);
      if (!ins || !col) continue;
      const [caught, intro] = [col.values.Collection_date, ins.values.Intro2Insectary_date];
      if (isDate(caught) && isDate(intro) && Math.abs(intro - caught) > 3) continue;
      const field = issue.field;
      const same = field === 'Sex' ? (a, b) => sexOf(a) === sexOf(b) : (a, b) => binomial(a) === binomial(b);
      const species = row => (row.sheet === 'Collection_data' ? [row.values.SPECIES, row.values.Subspecies_Form] : [row.values.SPECIES]);
      const shown = row =>
        field === 'Sex'
          ? text(row.values.Sex)
          : species(row)
              .map(text)
              .filter(v => v && !blank(v))
              .join(' ');

      // Evidence: a later correction on one side, then the envelope.
      const [ce, ie] = [ctx.lastEdit(col.id, field), ctx.lastEdit(ins.id, field)];
      let winner = null;
      let why = null;
      if (corrected(ce) && (!ie || ce.at > ie.at)) {
        winner = col;
        why = msg('Collection_data se corrigió el {date} ({who}): {before} → {after}', {
          date: ce.at.slice(0, 10),
          who: ce.user ?? 'Google Sheets',
          before: text(ce.before),
          after: text(ce.after),
        });
      } else if (corrected(ie) && (!ce || ie.at > ce.at)) {
        winner = ins;
        why = msg('Insectary_data se corrigió el {date} ({who}): {before} → {after}', {
          date: ie.at.slice(0, 10),
          who: ie.user ?? 'Google Sheets',
          before: text(ie.before),
          after: text(ie.after),
        });
      } else {
        const read = envelope.get(`${col.id}:${field}`) ?? envelope.get(`${ins.id}:${field}`);
        const agrees = read ? [col, ins].filter(r => same(read, shown(r))) : [];
        if (agrees.length === 1) {
          winner = agrees[0];
          why = msg('el sobre de las fotos dice «{read}», como {sheet}', { read, sheet: winner.sheet });
        }
      }
      const target = winner === ins ? col : ins;
      const source = target === ins ? col : ins;
      const certainty = winner ? 'likely' : 'check';
      const reason = winner
        ? msg('{id}: {why}', { id: text(ins.values.Insectary_ID), why })
        : msg('{id}: {sheet} fila {row} dice {value}; nada en el libro dice qué lado es el correcto (mirar el sobre)', {
            id: text(ins.values.Insectary_ID),
            sheet: source.sheet,
            row: source.row,
            value: shown(source) || '—',
          });
      const base = { recordId: target.id, sheet: target.sheet, row: target.row, label: target.label, certainty, reason };
      const related = [ctx.ref(source, field)];
      if (field === 'Sex') {
        // The insectary's list has no "?": a doubtful field sex is copied as the sex alone.
        const value = target === ins ? (sexOf(col.values.Sex) ?? text(col.values.Sex)) : text(ins.values.Sex);
        out.push({ ...base, field, current: text(target.values.Sex), suggested: value, related });
        continue;
      }
      if (target === ins) {
        if (speciesLists.Insectary_data.size && !speciesLists.Insectary_data.has(shown(col).toLowerCase())) continue;
        out.push({ ...base, field: 'SPECIES', current: text(ins.values.SPECIES), suggested: shown(col), related });
        continue;
      }
      // Collection_data keeps genus + epithet in SPECIES and the subspecies apart.
      const [genus, epithet, ...rest] = words(ins.values.SPECIES);
      if (speciesLists.Collection_data.size && !speciesLists.Collection_data.has(`${genus} ${epithet}`.toLowerCase())) continue;
      const group = `twins:${col.id}`;
      out.push({ ...base, field: 'SPECIES', current: text(col.values.SPECIES), suggested: `${genus} ${epithet}`, related, group });
      if (rest.length && rest.join(' ') !== text(col.values.Subspecies_Form))
        out.push({
          ...base,
          field: 'Subspecies_Form',
          current: text(col.values.Subspecies_Form),
          suggested: rest.join(' '),
          related,
          group,
        });
    }
    return out;
  },
};
