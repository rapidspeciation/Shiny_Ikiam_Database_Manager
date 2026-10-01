// The fixes the checks already compute (server/checks.mjs), as suggestions with
// their certainty: a list value written another way (only case, spaces, "_" or
// accents differ) is certain; the other sheet's CAM in a copied CAM column, a
// year typed one off and a date written as text are likely; a date taken from
// the start of a note needs a look. Photo readings are judged in Revisión with
// their own strength, so they are not repeated here.

import { msg, tpl } from '../messages.mjs';

const PHOTO_KIND = /^(photo_|envelope_|ai_)/;
const days = (a, b) => Math.abs(Date.parse(a) - Date.parse(b)) / 864e5;
/**
 * The certainty of an issue's fix, by its kind and the note that says where the
 * value comes from. A year moved to put a death after the collection or entry is
 * likely only when the butterfly then lived a plausible time (120 days at most).
 */
function certaintyOf(issue) {
  if (issue.kind === 'list') return 'certain';
  if (issue.kind === 'link_mismatch') return 'likely';
  const note = issue.fixNoteMsg?.key ?? issue.fixNote;
  if (note === 'año mal escrito' && issue.kind === 'date_order') {
    const fixed = Object.values(issue.fix.values)[0];
    const earlier = issue.related?.[0]?.value;
    return typeof earlier === 'string' && days(fixed, earlier) <= 120 ? 'likely' : 'check';
  }
  if (note === 'año mal escrito' || note === 'fecha escrita como texto') return 'likely';
  return 'check';
}
const asVar = (text, descriptor) => (descriptor ? { text, msg: descriptor } : text);

export default {
  id: 'check_fixes',
  title: tpl('Arreglos de los chequeos'),
  describe: tpl(
    'Los arreglos que la Revisión de datos ya calcula: un valor de lista escrito de otra forma (seguro), el CAM de la otra hoja en una columna copiada, un año escrito con uno de diferencia y una fecha escrita como texto (probables), y una fecha sacada del inicio de una nota (revisar).',
  ),
  suggest(ctx) {
    const out = [];
    for (const issue of ctx.issues()) {
      if (!issue.fix || issue.task || PHOTO_KIND.test(issue.kind)) continue;
      const row = ctx.byId.get(issue.fix.recordId);
      if (!row) continue;
      const problem = asVar(issue.problem, issue.problemMsg);
      for (const [field, suggested] of Object.entries(issue.fix.values))
        out.push({
          sheet: row.sheet,
          row: row.row,
          recordId: row.id,
          label: row.label,
          field,
          current: ctx.shown(row.sheet, field, row.values[field]),
          suggested,
          certainty: certaintyOf(issue),
          reason: issue.fixNote
            ? msg('{problem} ({note})', { problem, note: asVar(issue.fixNote, issue.fixNoteMsg) })
            : problem,
          ...(issue.related?.length ? { related: issue.related } : {}),
        });
    }
    return out;
  },
};
