import { createHash, randomUUID } from 'node:crypto';
import { readFile } from 'node:fs/promises';
import { createReports } from './reports.mjs';
import { TYPED_OVER_FORMULA, renamesWithSuffix, sameAsFormula, uniqueIdIndex } from './batch.mjs';
import { allIssues, checkData } from './checks.mjs';
import { agreedFixes, markApplied } from './review.mjs';
import { CERTAINTIES, suggestionPage } from './suggestions/index.mjs';
import { alerts } from './alerts.mjs';
import { msg, tpl, withoutMsgs } from './messages.mjs';
import { columnKeys, columnOf, comparable, isSumField, labelFor, moduleMap, simpleSum, validateValues, withColumnNames } from './schema.mjs';
import { TUBE_FIELD, isIdValue, isUnique } from './verifications.mjs';
import { listOptions, listProblem } from './verify.mjs';
import { queueWalk, walkDraft } from './walks.mjs';
import { KINDS, isNone, noteText, reviewColumns } from './notebook.mjs';
import { HIDDEN_COLUMNS, isNotWritten, notWrittenWhy } from './proposal-columns.mjs';
import { createFormulaReader, sameResult } from './formula-gives.mjs';
import { FILTERS_DOC, FIND_BUDGET, RECORD_TOOLS, compactRecord, countRecords, findRecords, pickRows, resolveRows, selectRecords } from './records-tool.mjs';
import { RESULT_BUDGET, fitList, fitResult } from './tool-budget.mjs';
import { MATCH_NOTEBOOK_TOOL, createNotebookMatcher, matchSummary } from './notebook-tool.mjs';
import { duplicateIdRow, insectaryIdPlaces, insectaryIdRow, newRowFormulaFields } from './premade.mjs';
import { BETWEEN_ROWS, VIEW_PARAM, VIEW_UPDATE, readView, showBetween, viewColumns } from './proposal-view.mjs';
import { proposalSampleWarnings } from './preserved.mjs';
import { compareWithSheet, currentRecord } from './needs-review.mjs';
import { carryChecks, dropDoubt, setChecked, uncheckedDoubts, unfilledUnreadable, withoutUnchecked } from './doubts.mjs';
import { decide, editedInSheet, forget, lastEdit, resolveSheetEdits, sheetChangesOf, shownValue, takenRow, takenRows } from './sheet-edits.mjs';
import { tellChat } from './t3tell.mjs';
import { KNOWLEDGE_TOOLS, createKnowledge, runKnowledgeTool } from './knowledge.mjs';
import { HISTORY_TOOLS, HISTORY_TOOL_NAMES, runHistoryTool } from './history.mjs';
import { QUERY_HINT, QUERY_TOOL, createQueryRunner, formatRows, sqlProblem } from './query-tool.mjs';
import { createSheetsCopy } from './replica.mjs';
import { createT3Chats } from './t3chats.mjs';
import { photoCacheDir } from './photos.mjs';
import { PHOTO_SIZES, attachmentFile, attachmentsDir, createPhotoCopies, photosOf } from './proposal-photos.mjs';

const bad = (status, code, message) => ({ status, body: { error: { code, message } } });
const now = () => new Date().toISOString();
const clip = (value, length = 1200) => String(value ?? '').slice(0, length);
const owner = user => String(user?.id ?? user?.username ?? '');
const json = value => JSON.stringify(value);
const EDITORS = ['editor', 'reviewer', 'admin'];
/**
 * Rows of one proposal at most (as one save takes at most, server/batch.mjs MAX_BATCH): its
 * table is reviewed and applied as a whole, and get_proposal lists them all. A `bulk` call
 * picks up to as many.
 */
const PROPOSAL_ROWS = 500;
const tooManyRows = n => `At most ${PROPOSAL_ROWS} rows per proposal (here ${n}): put the rest in another proposal.`;
/** Rows a `bulk` call may pick before those already holding its values are left out. */
const BULK_PICKED = 4 * PROPOSAL_ROWS;
const narrower = () =>
  `One proposal takes at most ${PROPOSAL_ROWS} rows: narrow the filters (count_records says how many match, e.g. per month), or make another proposal for the rest.`;
const isoDate = serial => new Date(Date.UTC(1899, 11, 30) + serial * 864e5).toISOString().slice(0, 10);
const TIME_FIELD = /(^|_)time$/i;
/** "9:20" in a time column becomes the day fraction Sheets stores (the grids show it as 9:20). */
const withSheetTimes = values =>
  !values || typeof values !== 'object' || Array.isArray(values)
    ? values
    : Object.fromEntries(
        Object.entries(values).map(([key, value]) => {
          const m = TIME_FIELD.test(key) && typeof value === 'string' && /^\s*([01]?\d|2[0-3]):([0-5]\d)\s*$/.exec(value);
          return [key, m ? (Number(m[1]) * 60 + Number(m[2])) / 1440 : value];
        }),
      );
const parse = value => {
  try {
    return JSON.parse(value);
  } catch {
    return null;
  }
};

/*
 * What the assistant's values mean in a proposal. Emptying a cell is never
 * implicit: null (or leaving the column out) is "no change there", and only
 * { clear: true } empties a cell. A note the assistant adds keeps the team's
 * "d/m/yy INI: " form after what the cell holds; { replace } rewrites it.
 */
const VALUES_DOC =
  'Column → value, also in newRows and bulk set: dates YYYY-MM-DD, times H:MM; null or leaving the column out = no change; {"clear": true} empties the cell (in a new row both leave it empty).';
const VALUES_RULES = [
  '- null = no change, never an empty cell; {"clear": true} only when the person wants it emptied.',
  '- Notes (NOTES, Notes_…): only the new text; it goes after the existing note as "d/m/yy INI: text". {"replace": "…"} rewrites it, only when asked.',
].join('\n');
/**
 * A value the assistant dropped (null in update_proposal), or a cell the person
 * set back to the sheet's value ("Valor de la hoja"): the cell goes back to no change.
 */
const DROP = Symbol('drop');
/** The person took the assistant's value again ("Valor de la IA"): the one kept with their mark. */
const AI_VALUE = Symbol('ai value');
const NOTE_FIELD = /^notes?(?:_|$)/i;
/** A note already in the team's form: "29/9/26 FCH:", "16/06/2023 AA:", "23Ago26 PAS". */
const NOTE_PREFIX = /^\s*(?:\d{1,2}\s*[/.-]\s*\d{1,2}\s*[/.-]\s*\d{2,4}|\d{1,2}\s*[A-Za-z]{3}\s*\d{2,4})\s+[A-ZÑ]{2,4}\b/;
const ecuadorDay = () => new Intl.DateTimeFormat('en-CA', { timeZone: 'America/Guayaquil' }).format(new Date());
/** Rows a table shown with show_rows holds at most, and the columns it shows when the assistant names none. */
const TABLE_ROWS = 500;
const TABLE_COLUMNS = 20;
/** A sheet's ID columns when the app names none for it (Sperm_dissections: Father_CAMid, Mother_CAMid). */
const ID_LIKE = /(?:^|_)ID$|CAM_?id$/i;

const TOOLS = [
  {
    type: 'function',
    function: {
      name: 'search_records',
      description:
        'Search workbook rows by free text: up to 12 rows with sheet, row and app ID.\nExact identifiers or column conditions: `find_records`. Counts: `count_records`.',
      parameters: {
        type: 'object',
        properties: { query: { type: 'string' }, module: { type: 'string' } },
        required: ['query'],
      },
    },
  },
  ...RECORD_TOOLS,
  QUERY_TOOL,
  {
    type: 'function',
    function: {
      name: 'get_record',
      description:
        'One row: its sheet and row, every non-empty value (formula cells computed) and `formulas` (the text of each formula cell).',
      parameters: {
        type: 'object',
        properties: {
          id: { type: 'string', description: 'recordId, or the ID in the sheet (W2B, CAM079891, a clutch)' },
          sheet: { type: 'string' },
        },
        required: ['id'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'describe_sheet',
      description:
        "A sheet's columns: type, `formula: true`, and for dropdown columns `allowed`: the sheet's list (from Lists; strict = other values refused), long ID lists (CAM pools, tubes) as ranges of consecutive IDs, newest first. Other columns with few values give the values in use; free text a few `examples` and the `distinct` count.",
      parameters: {
        type: 'object',
        properties: {
          module: { type: 'string', description: 'The sheet, e.g. Insectary_data' },
          columns: { type: 'array', items: { type: 'string' }, description: 'Only these columns' },
          latestRows: { type: 'integer', description: 'Also the latest n filled rows (up to 10)' },
        },
        required: ['module'],
      },
    },
  },
  ...KNOWLEDGE_TOOLS,
  {
    type: 'function',
    function: {
      name: 'run_report',
      description: 'Produce a sourced count, stage, cross, sample, quality or weekly report.',
      parameters: {
        type: 'object',
        properties: {
          kind: { type: 'string', enum: ['overview', 'counts', 'stages', 'crosses', 'samples', 'quality', 'weekly'] },
          module: { type: 'string' },
          field: { type: 'string' },
        },
        required: ['kind'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'check_data',
      description:
        [
          "Scan the workbook (the app's copy: fast) for inconsistencies: the issues people judge in the Revisión tab («Problemas»).",
          '- Without `kind`: how many issues there are of each kind. Then ask for one kind or sheet, paging with `offset`.',
          "- Each issue: kind, sheet, row, recordId, label, field, value, problem (Spanish), related (the other rows involved), and `fix` = {recordId, values} when the checks compute the right value (`list_suggested_edits` source `check_fixes` gives each fix its certainty).",
          '- Without a fix, the rows disagree and the data alone do not say which is right: show the person the rows and the evidence.',
          '- Photo issues add cam, strength (fuerte/media/baja/dudosa: how often such a reading was right), curation (an earlier decision), photos, envelopeText (what was read on the envelope in the photo), envelopeCamid, prediction.',
          '- An issue with `task.text` is work on the photos in Drive, not a sheet change.',
        ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          sheet: { type: 'string', description: 'Only this sheet, e.g. Collection_data' },
          kind: {
            type: 'string',
            description:
              'Comma-separated: repeat (a unique ID or a tube in two rows), cam_cross (one CAM on two butterflies across Collection_data and Insectary_data / Wing_tissue), list (outside a strict list), insectary_link (Collected_Sent2Insectary without its Insectary_data row, or the reverse), link_mismatch (the two rows of one butterfly disagree), date_order, future_date, bad_date (no date in a date column), missing_sample (preserved without CAM_ID or Tube_1_id), preserved_na (Death_cause Killed_Preserved but CAM_ID NA and no tube: the cause and the preservation cells disagree), mark_reuse (a FieldMark_ID on two species), walk_doubt (a Wikiloc point stored without a row, its pairing doubtful: row = the likeliest or null, related = the candidates; a person pairs it in Monitoreo → Dudas, not with propose_changes), photo_camid (envelope CAM ≠ photo file name: Drive task), photo_extra (another butterfly\'s photos in a CAM folder: Drive task), envelope_sex, envelope_species (ocr = {read, sheet}; group = the batch), photo_missing (preserved over 30 days, no photos), ai_species (the Wings Gallery model sees another species; a person decides)',
          },
          recordId: { type: 'string', description: 'Only the issues of this row' },
          limit: { type: 'integer', description: '1 to 200, default 50' },
          offset: { type: 'integer' },
        },
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'queue_wikiloc',
      description:
        "Queue a Wikiloc monitoring walk (trail URL) for the app server's Wikiloc importer, which reads the public trail page, usually within a minute or two. A walk already read returns its walkId at once. Then call `get_walk`.",
      parameters: {
        type: 'object',
        properties: {
          url: { type: 'string', description: 'Wikiloc trail link, e.g. https://es.wikiloc.com/rutas-senderismo/…-123456789' },
          refresh: { type: 'boolean', description: 'Read it again even if already read (notes corrected in Wikiloc)' },
        },
        required: ['url'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'get_walk',
      description:
        [
          'A Wikiloc walk read from its public page (no GPS times). While it is being read: its queue status; call again a minute later.',
          '- points: each with its parsed note (species, subspecies, sex, time, height, weather, mark; transect section from the GPS position), inSheet (already in Collection_data) and the Monitoreo checks (recapture, mark on another species, 30-preserved rule, missing parts).',
          "- newRows: the points not in the sheet as Collection_data rows in the app's import template, for ONE `propose_changes`. Preserved points come without CAM_ID and Tube_1_id: ask for them (envelope, tube label) or leave them out and say so.",
          '- problems: the day or collector is unknown: ask, then pass `date` / `collector`.',
          'Once applied, the person puts the walk on the map in Monitoreo → Importar («Pasar al mapa … ya registrados en la hoja»).',
        ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          url: { type: 'string' },
          walkId: { type: 'string' },
          date: { type: 'string', description: 'YYYY-MM-DD, when the title has no day' },
          collector: { type: 'string', description: 'As in Collection_data, e.g. "FCH - Franz Chandi"' },
        },
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'propose_changes',
      description:
        [
          'Draft edits to existing rows (`changes`) and/or new rows (`newRows`), shown at once as a table beside the chat; nothing is written until the person confirms.',
          '- One proposal per task (a walk, a kind of fix), with a short note per row on where its values come from.',
          '- A row: its `recordId`, or `sheet` + `id`, its ID in the sheet (W2B, CAM079891, a clutch number).',
          "- Formula cells cannot be changed, except Insectary_data's SPECIES when what emerged differs from the formula, and an Insectary_ID given to two butterflies: a suffix on the row's own ID (W2B → W2B.1).",
          "- A new Insectary_data row names its Insectary_ID and fills the pre-made row of that ID; a second butterfly of a used ID takes a suffix (W2B.2), its row inserted below that ID's rows.",
          `- Up to ${PROPOSAL_ROWS} rows per proposal; \`bulk\` gives the same values to many existing rows.`,
          VALUES_RULES,
        ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          changes: {
            type: 'array',
            items: {
              type: 'object',
              properties: {
                recordId: { type: 'string' },
                sheet: { type: 'string' },
                id: { type: 'string' },
                values: { type: 'object', description: VALUES_DOC },
                note: { type: 'string' },
              },
              required: ['values'],
            },
          },
          newRows: {
            type: 'array',
            items: {
              type: 'object',
              properties: { sheet: { type: 'string' }, values: { type: 'object' }, note: { type: 'string' } },
              required: ['sheet', 'values'],
            },
          },
          bulk: {
            type: 'array',
            description:
              'Rows of one sheet by `filters` (as in find_records) and/or `recordIds`, each given `set`, e.g. {"sheet": "Insectary_data", "filters": {"Sex": {"empty": true}}, "set": {"Sex": "NOT_COLLECTED"}}. Rows already holding the values are left out.',
            items: {
              type: 'object',
              properties: {
                sheet: { type: 'string' },
                filters: { type: 'object' },
                recordIds: { type: 'array', items: { type: 'string' } },
                set: { type: 'object' },
                note: { type: 'string' },
              },
              required: ['sheet', 'set'],
            },
          },
          reason: { type: 'string' },
          issueIds: {
            type: 'array',
            items: { type: 'string' },
            description: 'Of the list_agreed_fixes fixes in it (marked applied in Revisión with it)',
          },
          view: VIEW_PARAM,
        },
        required: ['reason'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'list_agreed_fixes',
      description:
        [
          'The corrections people agreed on in the Revisión tab (accepted, or another value given):',
          '- fixes: {issueId, recordId, sheet, row, label, values, note, decidedBy}.',
          '- tasks: Drive work on the specimen photos (renames, merges), not sheet changes.',
          '- needsValue: accepted without a value: ask for it.',
          '- stale: the row changed since the verdict.',
          'To make them: one `propose_changes` with the fixes (values merged per recordId, notes kept) and their issueIds.',
        ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          kind: { type: 'string', description: 'Only these kinds (comma-separated), e.g. envelope_sex' },
          limit: { type: 'integer', description: '1 to 100, default 100' },
        },
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'list_suggested_edits',
      description:
        [
          'Read-only: the corrections the app computes from the workbook (Revisión → Sugerencias).',
          '- Without filters, the answer also lists the sources with their description and counts.',
          '- Each suggestion: sheet, row, recordId, label, field, current, suggested (null = a person must decide), certainty, reason (the evidence, Spanish).',
          '- certainty: certain = only the spelling changes; likely = strong evidence; check = a lead for someone who knows.',
          '- manual: true = a formula cell, fixed by hand in Google Sheets (`propose_changes` cannot write it).',
          "To make some of them: one `propose_changes` with those (the reason as each row's note). `check` suggestions and those without a value are for the person to decide first.",
        ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          source: { type: 'string', description: 'Only these sources (comma-separated), e.g. tubes,dates' },
          certainty: { type: 'string', description: 'certain, likely, check (comma-separated)' },
          sheet: { type: 'string', description: 'Only this sheet' },
          recordId: { type: 'string', description: 'Only the suggestions for this row' },
          q: { type: 'string', description: 'Text in the row label, the values or the reason' },
          limit: { type: 'integer', description: '1 to 200, default 50' },
          offset: { type: 'integer' },
        },
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'get_alerts',
      description:
        [
          'Read-only alerts (`alerts`: the texts the app shows, in Spanish).',
          '- camPools: the CAM pools of the Lists sheet; per range its size, used, highest, next free, left above the highest used, gaps and last use. A range in use with fewer than 50 or 15 % left is an alert (PAS or AA hand out new ranges).',
          '- preserveRule: the 30-preserved rule: Ithomiini species with 30 or more Collected_Preserved from Ikiam, Casa de Lin or Mariposario Ikiam, the day each reached 30 and those preserved after it; close = species at 25–29.',
          '- missingSamples: insectary butterflies preserved (by their preservation cells) without CAM_ID or Tube_1_id, or Killed_Preserved with NA in them (kind preserved_na). Those that died in the last 180 days are alerts.',
        ].join('\n'),
      parameters: { type: 'object', properties: {} },
    },
  },
  MATCH_NOTEBOOK_TOOL,
  // Historial: find a save, link to it, preview and undo (server/history.mjs).
  ...HISTORY_TOOLS,
  {
    type: 'function',
    function: {
      name: 'show_rows',
      description:
        [
          "Show the person rows of one sheet as a read-only table beside the chat (current values). When an answer is about many rows (a clutch's butterflies, an issue's rows), show them there and keep the text to what they mean.",
          `- Rows: \`recordIds\` (or IDs) and/or \`filters\` or \`field\` + \`values\` as in find_records; up to ${TABLE_ROWS}.`,
          '- `columns`: the ones that matter, in order (default: ID columns, then filled ones). `notes`: comments on a row or a cell (`field`); `highlight` marks it.',
          '- `tableId`: change a shown table in place (what you leave out stays).',
          'Returns `link` (the table alone) and `assistantLink` (beside its chat).',
        ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          title: { type: 'string', description: 'e.g. "Dissections of M. lysimnia"' },
          sheet: { type: 'string' },
          recordIds: { type: 'array', items: { type: 'string' } },
          field: { type: 'string' },
          values: { type: 'array', items: { type: 'string' } },
          filters: { type: 'object' },
          columns: { type: 'array', items: { type: 'string' } },
          notes: {
            type: 'array',
            items: {
              type: 'object',
              properties: { recordId: { type: 'string' }, field: { type: 'string' }, text: { type: 'string' }, highlight: { type: 'boolean' } },
              required: ['recordId'],
            },
          },
          tableId: { type: 'string' },
        },
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'list_proposals',
      description:
        "This chat's proposals and tables (show_rows), newest first (pending or shown, then the last reviewed or closed), each with its status, reason, rows and `link`; `chatLink` shows them all on one page. allChats: every chat's pending proposals and tables shown.",
      parameters: {
        type: 'object',
        properties: { allChats: { type: 'boolean' } },
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'apply_proposal',
      description:
        [
          "Write a pending proposal to Google Sheets. Only when the person's latest message explicitly approves it ('sí, aplícalo', 'está correcto').",
          "- It writes what the table shows: your values and the cells the person typed, not the cells they set back to the sheet's value (a row left with nothing to write is skipped). `indexes`: only those rows.",
          "- Doubtful cells (match_notebook's, amber in the table) not yet checked: it writes nothing and returns them (doubtful: index, label, field, value, alternatives, reason); ask the person about each. Then `confirmDoubtful: true` writes them as they are (only when the person said so after seeing them), or `skipDoubtful: true` writes only the sure cells.",
          '- Unreadable cells still empty are never written (the sheet keeps its value); the answer lists them (unreadable, unreadableNote): ask the person for those values.',
        ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          proposalId: { type: 'string' },
          indexes: { type: 'array', items: { type: 'integer' } },
          confirmDoubtful: { type: 'boolean', description: 'The person saw the unchecked doubtful cells and wants them written as they are' },
          skipDoubtful: { type: 'boolean', description: 'Write only the sure cells; the unchecked doubtful ones are left out' },
        },
        required: ['proposalId'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'update_proposal',
      description:
        [
          'Revise a pending proposal in place (the person sees it change): when the person corrects something, update the same proposal.',
          '- rows: cells of its rows, by `index` or `id` (e.g. "W2B"). A value replaces yours; null drops your proposed change to that cell, never empties it; {"clear": true} empties it. `checked`: doubtful cells the person confirmed.',
          '- changes / newRows: more rows, as in propose_changes. removeRows: indexes or IDs. If a value fails its checks, nothing is saved.',
          '- Cells the person edited are theirs: kept, and returned as conflicts; `overridePersonEdits` only when they ask.',
          "- photo / rotate: the page's photos, as in match_notebook.",
          'Returns `changed` (those rows as get_proposal full shows them), `removed`, the row count; `full: true`: every row.',
        ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          proposalId: { type: 'string' },
          photo: { description: 'As in match_notebook' },
          rotate: { description: 'As in match_notebook' },
          rows: {
            type: 'array',
            items: {
              type: 'object',
              properties: {
                index: { type: 'integer' },
                id: { type: 'string' },
                values: { type: 'object' },
                note: { type: 'string' },
                checked: { type: 'array', items: { type: 'string' } },
              },
            },
          },
          changes: { type: 'array', items: { type: 'object' } },
          newRows: { type: 'array', items: { type: 'object' } },
          removeRows: { type: 'array', items: { anyOf: [{ type: 'integer' }, { type: 'string' }] } },
          reason: { type: 'string', description: 'A new title, only if the subject changed' },
          overridePersonEdits: { type: 'boolean' },
          view: VIEW_UPDATE,
          full: { type: 'boolean' },
        },
        required: ['proposalId'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'get_proposal',
      description:
        [
          "A proposal as the person sees it now: status, revision, each row's label by index (`labels`) and `attention`, the rows that need a look:",
          "- personEdits: cells the person corrected in the table, or set back to the sheet's value («Valor de la hoja», not written), each with what you had proposed;",
          "- doubtful: match_notebook's doubtful cells not checked yet (value, alternatives, reason);",
          '- unreadable: cells nobody could read, still empty (never written empty);',
          "- sheetChanged: cells edited in the sheet after you read them (read, now, by, applying). Applying keeps the sheet's value unless the person chose yours; re-check them, then update_proposal: your value goes over the sheet's, null keeps it. rowTaken: a new row's pre-made row is in use now.",
          "A notebook page's lines that go another way in the sheet than on the page come as `orderDiffers`.",
          '`full: true`: every row with its index, values (dates YYYY-MM-DD), note and these marks (`offset` continues a long one).',
          'Read it when the person says they changed the table, before update_proposal on a proposal you did not just make, and before apply_proposal if they edited it.',
        ].join('\n'),
      parameters: {
        type: 'object',
        properties: { proposalId: { type: 'string' }, full: { type: 'boolean' }, offset: { type: 'integer' } },
        required: ['proposalId'],
      },
    },
  },
];

/**
 * The tools most chats use (reading rows, the documents, drafting and revising a proposal):
 * Claude Code loads them with the chat instead of behind its tool search, a round trip saved.
 */
const ALWAYS_LOADED = new Set([
  'search_records',
  'find_records',
  'count_records',
  'query',
  'get_record',
  'describe_sheet',
  'propose_changes',
  'update_proposal',
  'show_rows',
]);

/** The tools as MCP's tools/list gives them to T3 Code's chats (and the app's AI instructions page shows them). */
export const mcpTools = () =>
  TOOLS.map(t => ({
    name: t.function.name,
    description: t.function.description,
    inputSchema: t.function.parameters,
    ...(ALWAYS_LOADED.has(t.function.name) ? { _meta: { 'anthropic/alwaysLoad': true } } : {}),
  }));

function init(db) {
  db.exec(`CREATE TABLE IF NOT EXISTS ai_threads (
    id TEXT PRIMARY KEY, owner_id TEXT NOT NULL, title TEXT NOT NULL, created_at TEXT NOT NULL, updated_at TEXT NOT NULL
  );
  CREATE INDEX IF NOT EXISTS ai_threads_owner ON ai_threads(owner_id, updated_at);
  CREATE TABLE IF NOT EXISTS ai_messages (
    id TEXT PRIMARY KEY, thread_id TEXT NOT NULL REFERENCES ai_threads(id) ON DELETE CASCADE,
    role TEXT NOT NULL, content TEXT NOT NULL, sources_json TEXT NOT NULL DEFAULT '[]',
    results_json TEXT NOT NULL DEFAULT '[]', proposals_json TEXT NOT NULL DEFAULT '[]', created_at TEXT NOT NULL
  );
  CREATE INDEX IF NOT EXISTS ai_messages_thread ON ai_messages(thread_id, created_at);
  CREATE TABLE IF NOT EXISTS ai_proposals (
    id TEXT PRIMARY KEY, thread_id TEXT NOT NULL REFERENCES ai_threads(id) ON DELETE CASCADE,
    owner_id TEXT NOT NULL, changes_json TEXT NOT NULL, reason TEXT, status TEXT NOT NULL,
    created_at TEXT NOT NULL, applied_at TEXT
  );`);
  const has = (table, column) =>
    db
      .prepare(`PRAGMA table_info(${table})`)
      .all()
      .some(c => c.name === column);
  if (!has('ai_messages', 'attachments_json'))
    db.exec("ALTER TABLE ai_messages ADD COLUMN attachments_json TEXT NOT NULL DEFAULT '[]'");
  if (!has('ai_proposals', 'applied_json')) db.exec('ALTER TABLE ai_proposals ADD COLUMN applied_json TEXT');
  // New rows of a proposal, once written: their record IDs (to show their sheet rows).
  if (!has('ai_proposals', 'created_json')) db.exec('ALTER TABLE ai_proposals ADD COLUMN created_json TEXT');
  // Issues of the Revisión tab a proposal fixes (list_agreed_fixes): marked applied when it is written.
  if (!has('ai_proposals', 'issues_json')) db.exec('ALTER TABLE ai_proposals ADD COLUMN issues_json TEXT');
  // Proposals are revised in place (update_proposal, the person's edits in the table): a revision per
  // proposal, when and by whom ('ai' or 'person') it last changed.
  if (!has('ai_proposals', 'revision')) db.exec('ALTER TABLE ai_proposals ADD COLUMN revision INTEGER NOT NULL DEFAULT 1');
  if (!has('ai_proposals', 'updated_at')) db.exec('ALTER TABLE ai_proposals ADD COLUMN updated_at TEXT');
  if (!has('ai_proposals', 'last_by')) db.exec('ALTER TABLE ai_proposals ADD COLUMN last_by TEXT');
  // The T3 Code chat a proposal comes from (server/t3chats.mjs): its thread id ('' = looked for, not
  // found), its title then, and the tool-use id of the call that drafted it (to find the chat later).
  if (!has('ai_proposals', 't3_thread')) db.exec('ALTER TABLE ai_proposals ADD COLUMN t3_thread TEXT');
  if (!has('ai_proposals', 't3_title')) db.exec('ALTER TABLE ai_proposals ADD COLUMN t3_title TEXT');
  if (!has('ai_proposals', 't3_tool_use')) db.exec('ALTER TABLE ai_proposals ADD COLUMN t3_tool_use TEXT');
  // A notebook page's proposal (match_notebook): the page's lines, its notebook and its photos, so its
  // table shows every line in the notebook's order beside the photo.
  if (!has('ai_proposals', 'page_json')) db.exec('ALTER TABLE ai_proposals ADD COLUMN page_json TEXT');
  // A table of sheet rows the assistant shows the person (show_rows): its sheet, rows, columns and
  // notes. It is listed with the proposals but never written: status 'shown', then 'closed'.
  if (!has('ai_proposals', 'table_json')) db.exec('ALTER TABLE ai_proposals ADD COLUMN table_json TEXT');
  // How the assistant asked its proposal's table to be shown (the `view` of propose_changes): the
  // columns first or only, and the sheet's rows between its rows or not.
  if (!has('ai_proposals', 'view_json')) db.exec('ALTER TABLE ai_proposals ADD COLUMN view_json TEXT');
  // A proposal in needs_review compared with the sheet after a sync (server/needs-review.mjs).
  if (!has('ai_proposals', 'check_json')) db.exec('ALTER TABLE ai_proposals ADD COLUMN check_json TEXT');
  db.exec('CREATE INDEX IF NOT EXISTS ai_proposals_owner_status ON ai_proposals(owner_id, status, created_at)');
  // Personal tokens for agents outside the app (T3 Code projects) to use the same tools.
  db.exec(`CREATE TABLE IF NOT EXISTS ai_tokens (
    token_hash TEXT PRIMARY KEY, user_id TEXT NOT NULL, label TEXT NOT NULL, created_at TEXT NOT NULL, revoked_at TEXT
  )`);
  // A restarted process cannot know whether an in-flight source write completed.
  db.prepare("UPDATE ai_proposals SET status = 'needs_review' WHERE status = 'applying'").run();
}

async function keyFor(ai) {
  if (ai.apiKeyFile) return (await readFile(ai.apiKeyFile, 'utf8')).trim();
  return ai.apiKey ?? '';
}

function providerConfig(config) {
  const ai = config.ai ?? {};
  return {
    baseUrl: ai.baseUrl ?? process.env.ITHOMIINI_AI_BASE_URL ?? 'https://api.openai.com/v1',
    model: ai.model ?? process.env.ITHOMIINI_AI_MODEL ?? '',
    apiKeyFile: ai.apiKeyFile ?? process.env.ITHOMIINI_AI_API_KEY_FILE,
    apiKey: ai.apiKey ?? process.env.ITHOMIINI_AI_API_KEY,
    transcriptionModel: ai.transcriptionModel ?? process.env.ITHOMIINI_AI_TRANSCRIPTION_MODEL,
    transcriptionMode:
      ai.transcriptionMode ??
      process.env.ITHOMIINI_AI_TRANSCRIPTION_MODE ??
      (String(ai.baseUrl ?? process.env.ITHOMIINI_AI_BASE_URL ?? '').includes('openrouter.ai') ? 'chat' : 'endpoint'),
    visionModel: ai.visionModel ?? process.env.ITHOMIINI_AI_VISION_MODEL,
  };
}

function endpoint(ai, path) {
  const base = new URL(ai.baseUrl.endsWith('/') ? ai.baseUrl : `${ai.baseUrl}/`);
  if (
    base.protocol !== 'https:' &&
    !(base.protocol === 'http:' && ['localhost', '127.0.0.1', '[::1]'].includes(base.hostname))
  )
    throw new Error('AI endpoint must use HTTPS or local HTTP');
  return new URL(path, base).toString();
}

async function providerFetch(ai, path, body, multipart = false, timeoutMs = 45000) {
  const key = await keyFor(ai);
  if (!key || !ai.model) throw new Error('AI provider is not configured');
  const response = await fetch(endpoint(ai, path), {
    method: 'POST',
    headers: { Authorization: `Bearer ${key}`, ...(multipart ? {} : { 'Content-Type': 'application/json' }) },
    body: multipart ? body : json(body),
    signal: AbortSignal.timeout(timeoutMs),
  });
  if (!response.ok) throw new Error(`AI provider returned HTTP ${response.status}`);
  const payload = await response.json();
  return payload;
}

async function complete(ai, messages, tools = TOOLS, model = ai.model, timeoutMs) {
  const payload = await providerFetch(
    { ...ai, model },
    'chat/completions',
    {
      model,
      messages,
      ...(tools?.length ? { tools, tool_choice: 'auto' } : {}),
    },
    false,
    timeoutMs,
  );
  const message = payload?.choices?.[0]?.message;
  if (!message || (typeof message.content !== 'string' && !Array.isArray(message.tool_calls)))
    throw new Error('AI provider returned no message');
  return message;
}

function recordSource(record) {
  return {
    id: record.id,
    type: 'record',
    sheet: record.sheet,
    row: record.row,
    version: record.version,
    label: record.label,
    sourceUrl: record.sourceUrl ?? null,
  };
}

export function createAssistant({ store, config = {} }) {
  if (!store?.db) throw new Error('Assistant requires store.db');
  const db = store.db;
  init(db);
  const ai = providerConfig(config);
  const reports = createReports({ store, config });
  const knowledge = createKnowledge(config);
  // What a proposal's formula cells will give once applied (server/formula-gives.mjs).
  const formulaReader = createFormulaReader(store);
  // The sheets' copy that `query` reads (server/replica.mjs), where the app keeps one.
  const sheetsCopy = config.sheetsCopyPath ? createSheetsCopy({ store, path: config.sheetsCopyPath, ...config.sheetsCopy }) : null;
  const queries = sheetsCopy ? createQueryRunner({ path: sheetsCopy.path, ...config.sheetsQuery }) : null;
  // The chats of T3 Code (its state and trace log, read-only): which one made a proposal, which one is open.
  const t3 = config.t3Chats ?? (config.t3?.home ? createT3Chats({ home: config.t3.home }) : null);
  /** A note in the person's conversation (T3 Code, Revisión de datos) of the proposals made there. */
  const insertMessage = (threadId, role, content, sources = [], results = [], proposals = []) => {
    const at = now();
    db.prepare(
      'INSERT INTO ai_messages (id,thread_id,role,content,sources_json,results_json,proposals_json,created_at) VALUES (?,?,?,?,?,?,?,?)',
    ).run(randomUUID(), threadId, role, content, json(sources), json(results), json(proposals), at);
    db.prepare('UPDATE ai_threads SET updated_at = ? WHERE id = ?').run(at, threadId);
  };

  /** A row as the model sees it: every value (formula cells computed), dates readable, the formulas worth reading. */
  const compact = (record, options) => compactRecord(record, options);

  /** find_records (server/records-tool.mjs): the rows it returns can be cited. */
  function findRows(args, context) {
    const out = findRecords(store, args);
    for (const row of out.found ?? out.rows ?? []) {
      const record = store.getRecord(Array.isArray(row) ? row[0] : row.id);
      if (!record) continue;
      context.records.set(record.id, record);
      context.sources.set(record.id, recordSource(record));
    }
    return out;
  }

  /** describe_sheet: the sheet's columns (only `columns` when given) and, when asked, its `latestRows` filled rows. */
  function describeSheet(args) {
    const mod = moduleMap.get(String(args.module ?? args.sheet ?? ''));
    if (!mod) return { error: `Unknown sheet ${clip(args.module ?? args.sheet, 60)}; the sheets: ${[...moduleMap.keys()].join(', ')}` };
    let only = null;
    if (args.columns !== undefined) {
      const asked = Array.isArray(args.columns) ? args.columns : [args.columns];
      const named = columnKeys(mod, asked.map(String));
      if (named.error) return { error: `columns: ${named.error}` };
      only = new Set(named.keys);
    }
    const latest = Math.min(Math.max(Math.trunc(Number(args.latestRows) || 0), 0), 10);
    const recent = db
      .prepare(
        'SELECT id FROM records WHERE sheet = ? AND missing = 0 AND observed = 1 AND row_num > 0 ORDER BY row_num DESC LIMIT 1500',
      )
      .all(mod.id)
      .map(r => store.getRecord(r.id));
    const formulas = new Map(),
      options = new Map();
    for (const record of recent) {
      for (const key of Object.keys(record.formulas ?? {})) formulas.set(key, (formulas.get(key) ?? 0) + 1);
      for (const [key, value] of Object.entries(record.values ?? {}))
        if (typeof value === 'string' && value && !record.formulas?.[key]) {
          const seen = options.get(key) ?? new Map();
          seen.set(value, (seen.get(value) ?? 0) + 1);
          options.set(key, seen);
        }
    }
    // The sheet's own dropdown lists (from the Lists sheet): what a cell may hold. Long ID lists
    // (CAM pools, tubes) are summarised: how many, the first and the last.
    const lists = listOptions(store, mod.id);
    const allowedOf = key => {
      const list = lists[key];
      if (!list) return null;
      const values = [...list.values];
      const head = { strict: list.strict, source: list.source };
      if (values.length <= 60) return { ...head, values };
      // IDs like CAM079935: their consecutive runs (the pools), newest first; anything else apart.
      const ids = values.map(v => /^([A-Z]{2,4})(\d{4,})$/.exec(String(v).trim())).filter(Boolean);
      if (ids.length < values.length * 0.8)
        return { ...head, count: values.length, examples: values.slice(0, 10) };
      const width = new Map();
      for (const m of ids) width.set(m[1], Math.max(width.get(m[1]) ?? 0, m[2].length));
      const name = (prefix, n) => `${prefix}${String(n).padStart(width.get(prefix), '0')}`;
      const sorted = ids.map(m => [m[1], Number(m[2])]).sort((a, b) => a[0].localeCompare(b[0]) || a[1] - b[1]);
      const runs = [];
      for (const [prefix, n] of sorted) {
        const last = runs.at(-1);
        if (last && last.prefix === prefix && n === last.to + 1) last.to = n;
        else runs.push({ prefix, from: n, to: n });
      }
      const big = runs.filter(r => r.to - r.from >= 19).sort((a, b) => b.to - a.to);
      return {
        ...head,
        count: values.length,
        ranges: big.slice(0, 12).map(r => `${name(r.prefix, r.from)}–${name(r.prefix, r.to)}`),
        ...(big.length > 12 ? { olderRanges: big.length - 12 } : {}),
        ...(runs.length > big.length ? { looseIds: runs.length - big.length } : {}),
        ...(values.length > ids.length ? { otherValues: values.filter(v => !/^[A-Z]{2,4}\d{4,}$/.test(String(v).trim())).slice(0, 5) } : {}),
      };
    };
    // The values in use, or for free text (notes) the most used few and how many there are.
    const inUse = seen => {
      const values = [...seen.keys()];
      if (values.join('').length <= 300) return { values };
      const examples = [...seen].sort((a, b) => b[1] - a[1]).slice(0, 3);
      return { distinct: values.length, examples: examples.map(([v]) => (v.length > 100 ? `${v.slice(0, 100)}…` : v)) };
    };
    // Typed cells: the pre-made rows hold only formulas, in some sheets also one default (Father_Split_tube "No").
    const typed = record =>
      Object.entries(record.values ?? {}).filter(([key, value]) => !record.formulas?.[key] && value !== null && value !== '').length;
    const out = {
      sheet: mod.id,
      columns: mod.fields
        .filter(f => !only || only.has(f.key))
        .map(f => {
          const seen = options.get(f.key);
          const allowed = allowedOf(f.key);
          return {
            key: f.key,
            type: f.type,
            ...((formulas.get(f.key) ?? 0) > recent.length / 2 ? { formula: true } : {}),
            ...(allowed ? { allowed } : seen && seen.size <= 30 ? inUse(seen) : {}),
          };
        }),
      ...(latest
        ? {
            latestRows: recent
              .filter(r => typed(r) >= 2)
              .slice(0, latest)
              .map(r => compact(r, { formulaColumns: false, inSheet: true, fields: only })),
          }
        : {}),
    };
    // Within one answer: the columns' lists cut first (a sheet of long dropdown lists).
    const more = kept =>
      kept < out.columns.length
        ? { truncated: true, next: `Columns from ${out.columns[kept].key} on not shown: ask for them with columns` }
        : {};
    return fitList(out, 'columns', RESULT_BUDGET - 500, more).out;
  }

  /**
   * Columns that are formulas in the next unused (pre-made) row of a sheet: a new row leaves them.
   * Insectary_data's ID is the exception: a new row names its Insectary ID, which picks the
   * pre-made row whose ID formula gives it (made when needed); the formula stays.
   */
  function createFormulaFields(sheet) {
    const fields = newRowFormulaFields(store, sheet);
    if (sheet === 'Insectary_data') fields.delete('Insectary_ID');
    return fields;
  }

  /**
   * What a formula column will give once a row's `values` are written (`record`:
   * the existing row, null for a new one): SPECIES follows the clutch the row
   * takes. Undefined when the column is no formula there or it cannot be told.
   */
  function formulaWillGive(sheet, field, values, record) {
    const formula = record ? !!record.formulas?.[field] : createFormulaFields(sheet).has(field);
    if (!formula || !TYPED_OVER_FORMULA[sheet]?.has(field)) return undefined;
    const clutch = values['CLUTCH NUMBER'];
    const moved = clutch !== undefined && comparable(clutch) !== comparable(record?.values?.['CLUTCH NUMBER'] ?? null);
    if (field === 'SPECIES' && sheet === 'Insectary_data' && (moved || !record))
      return isNone(clutch) ? undefined : (notebooks.speciesOfClutch(clutch) ?? undefined);
    return record && !moved ? (record.values?.[field] ?? null) : undefined;
  }

  /**
   * A new row for a proposal, checked now as the save will check it (strict lists,
   * IDs already used), so the assistant can correct it before the person sees it.
   */
  function proposedRow(candidate, index, ids, clientId = randomUUID()) {
    const at = `newRows[${index}]`;
    const sheet = String(candidate?.sheet ?? '');
    if (!moduleMap.has(sheet)) return { error: `${at}: unknown sheet ${clip(sheet, 60)}` };
    const raw = candidate.values;
    if (!raw || typeof raw !== 'object' || Array.isArray(raw) || !Object.keys(raw).length || Object.keys(raw).length > 80)
      return { error: `${at}: invalid values` };
    let values;
    try {
      values = validateValues(sheet, withSheetTimes(raw));
    } catch (e) {
      return { error: `${at}: ${e.message}` };
    }
    const unwritten = Object.keys(values).find(key => isNotWritten(sheet, key));
    if (unwritten) return { error: `${at}: ${notWrittenWhy(sheet, unwritten)}` };
    // A count kept as a sum (=12+15) goes into the new row over its pre-made formula; other formulas stay.
    for (const [key, value] of Object.entries(values)) if (value?.formula && isSumField(sheet, key)) values[key] = value.formula;
    const formulas = createFormulaFields(sheet);
    const kept = key => isSumField(sheet, key) && values[key] !== null;
    const dropped = Object.keys(values).filter(key => formulas.has(key) && !kept(key));
    for (const key of Object.keys(values)) if ((formulas.has(key) && !kept(key)) || values[key] === null) delete values[key];
    if (!Object.keys(values).length) return { error: `${at}: the new row has no values` };
    const lists = listOptions(store, sheet);
    for (const [field, value] of Object.entries(values)) {
      const problem = lists[field]?.strict && listProblem(lists, field, value);
      if (problem) return { error: `${at}: ${problem}` };
    }
    for (const [field, value, key] of uniqueKeys(sheet, values)) {
      const holder = ids.used().get(key)?.[0];
      if (holder) return { error: `${at}: ${value} is already used in ${holder.sheet} row ${holder.row}` };
      if (ids.proposed.has(key)) return { error: `${at}: ${value} appears twice in this proposal` };
      ids.proposed.add(key);
    }
    if (sheet === 'Insectary_data' && values.Insectary_ID !== undefined) {
      values.Insectary_ID = String(values.Insectary_ID).trim().toUpperCase();
      // A suffixed ID (W2B.2): the second butterfly given an ID, in a row inserted below that ID's rows.
      const duplicate = duplicateIdRow(store, values.Insectary_ID);
      if (duplicate?.problem) return { error: `${at}: ${DUPLICATE_PROBLEMS[duplicate.problem](duplicate)}` };
      if (!duplicate && !insectaryIdRow(store, values.Insectary_ID))
        return {
          error: `${at}: ${values.Insectary_ID} is not a free Insectary ID (an empty pre-made row's, one the ID series reaches next, or a suffixed one like W2B.2 for a second butterfly with an ID already used)`,
        };
    }
    const identity = moduleMap.get(sheet).identityFields.map(key => values[key]).find(isIdValue);
    const time = Object.entries(raw).find(([key]) => TIME_FIELD.test(key))?.[1];
    return {
      change: {
        create: true,
        sheet,
        clientId,
        recordId: null,
        row: null,
        label: clip(identity ?? ([values.SPECIES, time].filter(Boolean).join(' ') || labelFor(sheet, values)), 80),
        before: {},
        values,
        replaceFormula: [],
        note: clip(candidate.note, 300),
        ...(dropped.length ? { dropped } : {}),
      },
    };
  }

  /** The IDs of a new row that must not be used elsewhere: [field, value, key] (tubes across the workbook). */
  const uniqueKeys = (sheet, values) =>
    Object.entries(values)
      .filter(([field, value]) => isUnique(sheet, field) && isIdValue(value))
      .map(([field, value]) => [field, value, `${TUBE_FIELD.test(field) ? 'tube' : `${sheet}:${field}`}\u0000${String(value).trim()}`]);

  /** Why a suffixed Insectary ID cannot get its row (premade.mjs duplicateIdRow). */
  const DUPLICATE_PROBLEMS = {
    used: d => `${d.id} is already used in Insectary_data row ${d.rows.join(', ')}`,
    no_base: d => `${d.base} is not in Insectary_data: a suffixed ID (${d.id}) is for a second butterfly with an ID already used`,
    empty_base: d => `the row of ${d.base} is still empty: the butterfly goes in it, without a suffix`,
    repeated: d => `${d.value} is in more than one row (${d.rows.join(', ')}): fix that before adding ${d.id}`,
  };

  const idsFor = () => {
    let index;
    return { proposed: new Set(), used: () => (index ??= uniqueIdIndex(store)) };
  };

  /**
   * The checked rows of a proposal (as the save will check them), or { error }.
   * `ids` is shared when a page is checked one row at a time (IDs repeated between rows).
   */
  function draftChanges(args, ids = idsFor(), maxRows = PROPOSAL_ROWS) {
    const idsUsed = () => ids.used();
    const edits = Array.isArray(args.changes) ? args.changes : [];
    const creates = Array.isArray(args.newRows) ? args.newRows : [];
    if (!edits.length && !creates.length) return { error: 'Provide changes to existing rows or newRows' };
    if (edits.length + creates.length > maxRows) return { error: tooManyRows(edits.length + creates.length) };
    const changes = [];
    for (const [i, candidate] of creates.entries()) {
      const out = proposedRow(candidate, i, ids);
      if (out.error) return out;
      changes.push(out.change);
    }
    for (const candidate of edits) {
      const old = store.getRecord(String(candidate?.recordId ?? ''));
      if (!old || old.missing) return { error: `Row ${clip(candidate?.recordId, 60)} not found; use find_records` };
      const raw = withSheetTimes(candidate.values);
      if (
        !raw ||
        typeof raw !== 'object' ||
        Array.isArray(raw) ||
        !Object.keys(raw).length ||
        Object.keys(raw).length > 80
      )
        return { error: `Invalid values for ${old.label}` };
      let values;
      try {
        values = validateValues(old.sheet, raw);
      } catch (e) {
        return { error: `${old.label}: ${e.message}` };
      }
      const before = {},
        replaceFormula = [];
      const unwritten = Object.keys(values).find(key => isNotWritten(old.sheet, key));
      if (unwritten) return { error: `${old.label}: ${notWrittenWhy(old.sheet, unwritten)}` };
      for (const key of Object.keys(values)) {
        // A count kept as a sum is shown and written as its formula text (=12+15), over the old sum.
        const sum = isSumField(old.sheet, key) ? simpleSum(old.formulas?.[key]) : null;
        if (values[key]?.formula && isSumField(old.sheet, key)) values[key] = values[key].formula;
        if (sum) {
          before[key] = sum;
          continue;
        }
        if (old.formulas?.[key]) {
          // Two butterflies with one ID: this row's ID gets a suffix (W2B → W2B.1), typed over its formula.
          const rename = old.sheet === 'Insectary_data' && key === 'Insectary_ID';
          if (rename) {
            values[key] = String(values[key] ?? '').trim().toUpperCase();
            if (!renamesWithSuffix(old.values?.[key], values[key]))
              return {
                error: `${old.label}: Insectary_ID is calculated by a formula; it only takes a suffix (${old.values?.[key]}.1) to tell apart two butterflies with that ID`,
              };
            const holder = idsUsed().get(`Insectary_data:Insectary_ID\u0000${values[key]}`)?.[0];
            if (holder) return { error: `${old.label}: ${values[key]} is already used in ${holder.sheet} row ${holder.row}` };
          } else if (!TYPED_OVER_FORMULA[old.sheet]?.has(key))
            return { error: `${old.label}: ${key} is calculated by a formula and cannot be changed` };
          // What the formula gives (from the clutch the row will have) is left to it.
          const gives = formulaWillGive(old.sheet, key, values, old);
          if (gives !== undefined && sameAsFormula(gives, values[key])) {
            delete values[key];
            continue;
          }
          replaceFormula.push(key);
        }
        before[key] = old.values?.[key] ?? null;
      }
      if (Object.keys(values).every(key => comparable(before[key]) === comparable(values[key]))) continue;
      changes.push({
        recordId: old.id,
        sheet: old.sheet,
        row: old.row,
        label: old.label,
        expectedVersion: old.version,
        before,
        values,
        replaceFormula,
        note: clip(candidate.note, 300),
      });
    }
    if (!changes.length) return { error: 'Every proposed value is already in the sheet' };
    return { changes };
  }

  /** A drafted proposal saved for review: it shows at once in Cambios propuestos (and the chat). */
  function saveProposal(changes, reason, context, issueIds = [], view = null) {
    const id = randomUUID();
    const time = now();
    const chat = context.t3 ? chatOfCall(context) : null;
    db.prepare(
      'INSERT INTO ai_proposals (id,thread_id,owner_id,changes_json,reason,status,created_at,issues_json,updated_at,last_by,t3_thread,t3_title,t3_tool_use,view_json) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?)',
    ).run(
      id,
      context.threadId,
      owner(context.user),
      json(changes),
      clip(reason, 500),
      'pending',
      time,
      issueIds.length ? json(issueIds) : null,
      time,
      'ai',
      chat?.id ?? null,
      chat?.title ?? null,
      context.t3?.toolUseId ?? null,
      view ? json(view) : null,
    );
    const proposal = { id, changes, reason: clip(reason, 500), status: 'pending' };
    context.proposals.push(proposal);
    changed(owner(context.user));
    return { ...proposal, chat: chat?.id ?? null };
  }

  /** The app's address of a page (`#/…`): absolute with the app's public address. */
  const appUrl = hash => {
    const base = String(config.publicUrl || '').replace(/\/+$/, '');
    return `${base ? `${base}/` : ''}#/${hash}`;
  };
  /**
   * The links of a proposal: `link`, a page with that proposal alone (#/propuestas/<id>, any
   * device, no T3 needed); `assistantLink`, the Asistente tab with it beside its T3 chat
   * (AssistantView reads propuesta/chat; without chat it finds the chat).
   */
  function proposalLink(id, chat = null) {
    const q = new URLSearchParams({ propuesta: id });
    if (chat) q.set('chat', chat);
    return { link: appUrl(`propuestas/${encodeURIComponent(id)}`), assistantLink: appUrl(`asistente?${q}`) };
  }
  /** The page with every proposal of a T3 chat (#/propuestas?chat=<thread>). */
  const chatProposalsLink = chat => appUrl(`propuestas?${new URLSearchParams({ chat })}`);
  /** The chat a proposal is shown with: its T3 chat, else the chat of this call when T3 knows it. */
  const chatOf = (proposal, context) => proposal?.t3_thread || (context?.t3 ? (chatOfCall(context)?.id ?? null) : null);

  const initialsCache = new Map();
  const initialsOf = user => {
    const key = owner(user) || user?.username || '';
    const hit = initialsCache.get(key);
    if (hit && hit.until > Date.now()) return hit.value;
    const value = initialsFor(user);
    initialsCache.set(key, { value, until: Date.now() + 600000 });
    return value;
  };

  /**
   * A note the assistant writes, in the team's form: "d/m/yy INI: text" after
   * what the cell already holds (" | " between them), never over it. Text that
   * already starts with the old note (the assistant kept it) only gets its new
   * part prefixed; a note already dated keeps its own prefix.
   */
  function addedNote(before, text, user) {
    const note = String(text).trim();
    const dated = s => (NOTE_PREFIX.test(s) ? s : noteText(s, { today: ecuadorDay(), initials: initialsOf(user) }));
    if (isNone(before)) return dated(note);
    const old = String(before).trim();
    if (note === old) return old;
    if (note.startsWith(old)) {
      const tail = note.slice(old.length).replace(/^\s*\|\s*/, '').trim();
      return tail ? `${old} | ${dated(tail)}` : old;
    }
    return `${old} | ${dated(note)}`;
  }

  /**
   * The values the assistant gave for one row, as a proposal keeps them. null,
   * "" or a missing value is no change (listed in `ignored`, or DROP with
   * keepDrops, for update_proposal to take back a change it proposed);
   * { clear: true } empties an existing row's cell (null, the only way to);
   * { replace: text } is taken as it is; a note is added after the existing one.
   */
  function fromAssistant(record, raw, user, { keepDrops = false } = {}) {
    if (!raw || typeof raw !== 'object' || Array.isArray(raw)) return { values: raw, ignored: [] };
    const values = {},
      ignored = [];
    const nothing = field => (keepDrops ? (values[field] = DROP) : ignored.push(field));
    for (const [field, value] of Object.entries(raw)) {
      const object = value && typeof value === 'object' && !Array.isArray(value);
      if (value === null || value === undefined || value === '') nothing(field);
      else if (object && (value.clear === true || ('replace' in value && isNone(value.replace) && value.replace !== 'NA'))) {
        if (record) values[field] = null;
        else if (keepDrops) values[field] = DROP;
      } else if (object && 'replace' in value) values[field] = value.replace;
      else if (NOTE_FIELD.test(field) && typeof value === 'string') values[field] = addedNote(record?.values?.[field], value, user);
      else values[field] = value;
    }
    return { values, ignored };
  }

  /** Values with each column named as the sheet names it ("sex" → Sex, "pupa date" → PUPA DATE): { values } or { error }. */
  function sheetNames(sheet, values, at) {
    if (!moduleMap.has(sheet)) return { values };
    const out = withColumnNames(sheet, values);
    return out.error ? { error: `${at}: ${out.error}` } : out;
  }

  /**
   * Rows the assistant names by what the sheet shows ({sheet, id: "W2B"}, a bare ID, {sheet, key})
   * with their app recordId: { changes } or { error } saying which row and what to give instead.
   */
  function withRecordIds(list, what = 'changes') {
    const changes = Array.isArray(list) ? list : [];
    if (!changes.length) return { changes };
    const refs = changes.map(c =>
      c && typeof c === 'object' && c.recordId === undefined && (c.id !== undefined || c.key !== undefined)
        ? { sheet: c.sheet, id: c.id, key: c.key }
        : { recordId: c?.recordId, sheet: c?.sheet },
    );
    const { ids, problems } = resolveRows(store, refs);
    if (problems.length) return { error: problems.slice(0, 5).map(p => `${what}[${p.index}]: ${p.error}`).join(' ') };
    return {
      changes: changes.map((c, i) => {
        const named = { ...c, recordId: ids[i] };
        delete named.id;
        delete named.key;
        return named;
      }),
    };
  }

  /** propose_changes' arguments with the assistant's values read as fromAssistant says (and the columns as the sheet names them). */
  function assistantArgs(args, user) {
    const ignored = [];
    const changes = [];
    for (const [i, change] of (Array.isArray(args.changes) ? args.changes : []).entries()) {
      const record = store.getRecord(String(change?.recordId ?? ''));
      const live = record && !record.missing ? record : null;
      const given = live ? sheetNames(live.sheet, change?.values, `changes[${i}] (${live.label})`) : { values: change?.values };
      if (given.error) return given;
      const out = fromAssistant(live, given.values, user);
      ignored.push(...out.ignored.map(field => `${record?.label ?? clip(change?.recordId, 60)}: ${field}`));
      // Only nulls: nothing to change in that row.
      if (out.ignored.length && out.values && !Object.keys(out.values).length) continue;
      changes.push({ ...change, values: out.values });
    }
    const newRows = [];
    for (const [i, row] of (Array.isArray(args.newRows) ? args.newRows : []).entries()) {
      const given = sheetNames(String(row?.sheet ?? ''), row?.values, `newRows[${i}]`);
      if (given.error) return given;
      newRows.push({ ...row, values: fromAssistant(null, given.values, user).values });
    }
    return { args: { ...args, changes, newRows }, ignored };
  }

  /**
   * propose_changes' `bulk`: the same values for many existing rows, each group
   * picking the rows of one sheet by recordIds and/or filters (records-tool
   * pickRows), turned into ordinary changes ({ recordId, values, note }) after
   * the rows listed in `changes`. A row already listed (or in an earlier group)
   * takes only the columns it does not give yet. The values are checked once per
   * group (columns, strict lists) before the rows are drafted one by one.
   * Returns { changes, groups: [{ sheet, ids, matched, missing }] } or { error }.
   */
  function bulkChanges(input) {
    const groups = Array.isArray(input.bulk) ? input.bulk : [input.bulk];
    if (groups.length > 10) return { error: 'bulk takes up to 10 groups' };
    const listed = Array.isArray(input.changes) ? input.changes : [];
    const newRows = Array.isArray(input.newRows) ? input.newRows : [];
    if (listed.length + newRows.length > PROPOSAL_ROWS) return { error: tooManyRows(listed.length + newRows.length) };
    const changes = listed.map(c => ({ ...c }));
    const byId = new Map(changes.map(c => [String(c?.recordId ?? ''), c]));
    const out = [];
    let picked = 0;
    for (const [i, group] of groups.entries()) {
      const at = `bulk[${i}]`;
      if (!group || typeof group !== 'object' || Array.isArray(group)) return { error: `${at}: give sheet, filters and/or recordIds, and set` };
      if (!group.set || typeof group.set !== 'object' || Array.isArray(group.set) || !Object.keys(group.set).length || Object.keys(group.set).length > 80)
        return { error: `${at}: set must be column → value` };
      if (Object.values(group.set).every(v => v === null || v === undefined || v === ''))
        return { error: `${at}: every value in set is null, and null means no change. To empty a cell give {"clear": true}.` };
      const rows = pickRows(store, group);
      if (rows.error) return { error: `${at}: ${rows.error}` };
      const named = sheetNames(rows.mod.id, group.set, `${at}: set`);
      if (named.error) return named;
      const set = named.values;
      const unwritten = Object.keys(set).find(f => isNotWritten(rows.mod.id, f));
      if (unwritten) return { error: `${at}: ${notWrittenWhy(rows.mod.id, unwritten)}` };
      picked += rows.rows.length;
      if (picked > BULK_PICKED) return { error: `${at}: ${picked} rows picked. ${narrower()}` };
      // The values as the sheet will take them ({"clear": true} empties, a note is text), checked once.
      const plain = Object.fromEntries(
        Object.entries(set).map(([f, v]) => [f, v && typeof v === 'object' && !Array.isArray(v) ? ('replace' in v ? v.replace : null) : v]),
      );
      try {
        validateValues(rows.mod.id, withSheetTimes(plain));
      } catch (e) {
        return { error: `${at}: ${e.message}` };
      }
      const lists = listOptions(store, rows.mod.id);
      for (const [field, value] of Object.entries(plain)) {
        const problem = lists[field]?.strict && listProblem(lists, field, value);
        if (problem) return { error: `${at}: ${problem}` };
      }
      for (const row of rows.rows) {
        const known = byId.get(row.id);
        if (known) {
          known.values = { ...set, ...(known.values && typeof known.values === 'object' ? known.values : {}) };
          known.note ||= group.note;
          continue;
        }
        const change = { recordId: row.id, values: set, note: group.note };
        changes.push(change);
        byId.set(row.id, change);
      }
      out.push({ sheet: rows.mod.id, ids: new Set(rows.rows.map(r => r.id)), matched: rows.rows.length, missing: rows.missing });
    }
    return { changes, groups: out };
  }

  /**
   * What a bulk call picked, per group: the rows matched, those left out because they
   * already hold the values, ids not in the sheet, and a few rows as they will change.
   */
  function bulkSummary(groups, changes) {
    const readable = (sheet, values) =>
      Object.fromEntries(
        Object.entries(values ?? {}).map(([f, v]) => [
          f,
          moduleMap.get(sheet)?.fields.find(x => x.key === f)?.type === 'date' && typeof v === 'number' ? isoDate(v) : v,
        ]),
      );
    return groups.map(g => {
      const mine = changes.filter(c => g.ids.has(c.recordId));
      return {
        sheet: g.sheet,
        matched: g.matched,
        changed: mine.length,
        ...(g.matched > mine.length ? { alreadySet: g.matched - mine.length } : {}),
        ...(g.missing.length ? { notInSheet: g.missing.slice(0, 20) } : {}),
        preview: mine.slice(0, 5).map(c => ({ row: c.row, label: c.label, before: readable(c.sheet, c.before), after: readable(c.sheet, c.values) })),
      };
    });
  }

  /**
   * propose_changes as the assistant calls it; `literal` for fixes built by the
   * app (the Revisión tab), whose values are taken as they are.
   */
  function proposeChanges(input, context, { literal = false } = {}) {
    if (!EDITORS.includes(context.user.role)) return { error: 'Your role cannot propose edits' };
    if (!literal && Array.isArray(input.changes) && input.changes.length) {
      const named = withRecordIds(input.changes);
      if (named.error) return named;
      input = { ...input, changes: named.changes };
    }
    const bulk = !literal && input.bulk !== undefined && input.bulk !== null ? bulkChanges(input) : null;
    if (bulk?.error) return bulk;
    const given = bulk ? { ...input, changes: bulk.changes } : input;
    const read = literal ? { args: given, ignored: [] } : assistantArgs(given, context.user);
    if (read.error) return read;
    const { args, ignored } = read;
    if (ignored.length && !args.changes.length && !args.newRows.length)
      return { error: 'Every value was null, and null means no change. To empty a cell give {"clear": true}.' };
    // A bulk call's rows are counted once those already holding its values are left out.
    const drafted = draftChanges(args, idsFor(), bulk ? Infinity : PROPOSAL_ROWS);
    if (drafted.error) return drafted;
    const { changes } = drafted;
    if (changes.length > PROPOSAL_ROWS) return { error: `${changes.length} rows to change. ${narrower()}` };
    const view = readView(input.view, [...new Set(changes.map(c => c.sheet))]);
    if (view?.error) return view;
    const issueIds = Array.isArray(args.issueIds) ? args.issueIds.slice(0, 500).map(i => clip(i, 200)) : [];
    const saved = saveProposal(changes, args.reason, context, issueIds, view);
    const dropped = [...new Set(changes.flatMap(c => c.dropped ?? []))];
    const noSample = changes.flatMap((c, index) => {
      const warned = proposalSampleWarnings(c, c.create ? null : store.getRecord(c.recordId));
      return warned ? [{ index, label: c.label, missing: Object.keys(warned) }] : [];
    });
    // Row indexes for update_proposal (new rows first, then edits of existing rows). A long
    // proposal: a bulk call's preview, or where to read them.
    const listed = changes.length <= (bulk ? 30 : 100);
    return {
      proposalId: saved.id,
      ...proposalLink(saved.id, saved.chat),
      rows: changes.length,
      status: 'waiting for the person to confirm',
      ...(bulk ? { bulk: bulkSummary(bulk.groups, changes) } : {}),
      ...(noSample.length
        ? {
            preservedWithoutSample: noSample,
            preservedWithoutSampleNote:
              'These rows leave a preserved butterfly without its CAM_ID or Tube_1_id; the table marks those cells. Ask the person for them.',
          }
        : {}),
      ...(dropped.length ? { leftOut: `Formula columns left out of the new rows: ${dropped.join(', ')}` } : {}),
      ...(ignored.length ? { noChange: ignored.slice(0, 50), noChangeNote: 'null means no change: these cells keep the sheet value. To empty one give {"clear": true}.' } : {}),
      ...(listed
        ? { table: changes.map((c, index) => ({ index, sheet: c.sheet, label: c.label, ...(c.create ? { create: true } : { row: c.row }) })) }
        : bulk
          ? {}
          : { table: 'get_proposal with full: true lists every row with its index' }),
    };
  }

  // ------------------------------------------------------------ proposals revised in place
  /*
   * A pending proposal changes while the person reviews it: the assistant
   * revises it (update_proposal, a notebook page matched again) and the person
   * corrects cells in the table (POST …/edit). Each row keeps its key (the
   * clientId of a new row, the recordId of an edited one) and the cells the
   * person typed: personEdits = field → { ai: what the assistant had proposed
   * (absent: no change to that cell), by, at }. The assistant does not
   * overwrite those unless asked; it gets them back as conflicts. A cell the
   * person set back to the sheet's value (or emptied, in a new row) keeps its
   * mark with the assistant's value and is left out of `values`: the table shows
   * the suggestion aside, and applying writes only `values` (a row left without
   * any is not written).
   */
  const rowKey = change => change.clientId ?? change.recordId;
  const proposedOf = (change, field) => (field in change.values ? change.values[field] : undefined);
  const same = (a, b) => comparable(a) === comparable(b);
  /** A value as the save will store it (dates as serials, times as day fractions), to compare it. */
  function normal(sheet, field, value) {
    try {
      const out = validateValues(sheet, withSheetTimes({ [field]: value }))[field];
      return out?.formula ?? out;
    } catch {
      return value;
    }
  }

  /** A cell of a row set (not yet checked). In an existing row, the sheet's own value means no change there. */
  function setCell(change, field, value) {
    const values = { ...change.values };
    // The assistant dropped its change: the cell goes back to the sheet's value (or empty, in a new row).
    if (value === DROP) {
      delete values[field];
      return { ...change, values };
    }
    if (!change.create && !isSumField(change.sheet, field)) {
      const record = store.getRecord(change.recordId);
      if (same(normal(change.sheet, field, value), record?.values?.[field] ?? null)) {
        delete values[field];
        return { ...change, values };
      }
    }
    if (value === null && change.create) delete values[field];
    else values[field] = value;
    return { ...change, values };
  }

  /**
   * One row checked again as propose_changes checks it: { change, dropped } or { error }.
   * An existing row keeps what the sheet had when each cell was read (`before`): only
   * `fresh`, the cell the assistant sets now, is read again, so a cell edited in the
   * sheet meanwhile stays told apart (server/sheet-edits.mjs).
   */
  function redraftRow(change, index, others, used, fresh = null) {
    const personEdits = change.personEdits && Object.keys(change.personEdits).length ? change.personEdits : undefined;
    const keep = next => ({ ...change, ...next, personEdits });
    const read = drafted => {
      const out = { ...drafted };
      for (const [f, v] of Object.entries(change.before ?? {})) if (f !== fresh) out[f] = v;
      return out;
    };
    if (!Object.keys(change.values).length)
      return { change: keep(change.create ? { values: {} } : { values: {}, before: read({}), replaceFormula: [] }), dropped: [] };
    if (change.create) {
      const proposed = new Set(others.filter(c => c.create).flatMap(c => uniqueKeys(c.sheet, c.values).map(k => k[2])));
      const out = proposedRow({ sheet: change.sheet, values: change.values, note: change.note }, index, { proposed, used }, change.clientId);
      // Only formula columns: they are left out, as an empty row.
      if (/the new row has no values$/.test(out.error ?? ''))
        return { change: keep({ values: {} }), dropped: Object.keys(change.values) };
      if (out.error) return { error: out.error.replace(/^newRows\[\d+\]: /, '') };
      const { dropped = [], ...fresh } = out.change;
      // A row whose ID (and species) was set back or emptied keeps the name it was shown with, not "Insectary".
      const named =
        !!fresh.values.SPECIES || moduleMap.get(change.sheet).identityFields.some(key => isIdValue(fresh.values[key]));
      return { change: { ...keep(fresh), ...(named || !change.label ? {} : { label: change.label }), dropped: undefined }, dropped };
    }
    const out = draftChanges({ changes: [{ recordId: change.recordId, values: change.values, note: change.note }] });
    if (out.error === 'Every proposed value is already in the sheet')
      return { change: keep({ values: {}, before: read({}), replaceFormula: [] }), dropped: [] };
    if (out.error) return { error: out.error };
    return { change: keep({ ...out.changes[0], before: read(out.changes[0].before) }), dropped: [] };
  }

  /**
   * Revises a proposal's rows. `by`: 'ai' or 'person'. ops: set [{ ref (index or
   * key), values, note, before }], remove [ref], add { changes, newRows } (the
   * assistant's new rows), addEmpty [{ sheet }] (an empty new row the person fills),
   * sheet [{ ref, field, use }] (the person's choice on a cell edited in the sheet:
   * 'sheet' or 'proposal'; a value they type there chooses the proposal's, theirs;
   * a value the assistant sets there is read again against the sheet).
   * Each cell is checked as it is set: a refused cell keeps its value (for the
   * assistant the caller refuses the whole revision).
   */
  function reviseChanges(changes, ops, { by, force = false, user } = {}) {
    let rows = changes.map(c => ({ ...c, values: { ...c.values }, ...(c.personEdits ? { personEdits: { ...c.personEdits } } : {}) }));
    const find = ref => (typeof ref === 'number' ? (rows[ref] ? ref : -1) : rows.findIndex(c => rowKey(c) === ref));
    const out = { conflicts: [], rejected: [], overrode: [], leftOut: [] };
    let index;
    const used = () => (index ??= uniqueIdIndex(store));
    const where = i => ({ index: i, key: rowKey(rows[i]), label: rows[i].label, sheet: rows[i].sheet });
    const who = user ? clip(user.displayName || user.username || owner(user), 80) : 'person';

    for (const op of ops.set ?? []) {
      const i = find(op.ref);
      if (i < 0) {
        out.rejected.push({ ref: op.ref, message: `Row ${clip(op.ref, 60)} is not in the proposal` });
        continue;
      }
      if (typeof op.note === 'string' && by === 'ai') rows[i] = { ...rows[i], note: clip(op.note, 300) };
      const values = op.values && typeof op.values === 'object' && !Array.isArray(op.values) ? op.values : {};
      for (const [field, raw] of Object.entries(values)) {
        const row = rows[i];
        const current = proposedOf(row, field);
        const mark = row.personEdits?.[field];
        // "Valor de la IA" where the assistant proposed nothing (or it is already there): nothing to do.
        if (raw === AI_VALUE && !(mark && 'ai' in mark)) continue;
        const value = raw === AI_VALUE ? mark.ai : raw === '' || raw === undefined ? null : raw;
        if (by === 'ai' && mark && !force) {
          if (value === DROP ? current !== undefined : !same(normal(row.sheet, field, value), current))
            out.conflicts.push({ ...where(i), field, person: current ?? null, yours: value === DROP ? 'no change' : value });
          continue;
        }
        const drafted = redraftRow(setCell(row, field, value), i, rows.filter((_, j) => j !== i), used, by === 'ai' ? field : null);
        // The species the new row's formula will give: left to the formula, nothing to say.
        const toFormula =
          !drafted.error && row.create && value !== DROP && sameAsFormula(formulaWillGive(row.sheet, field, drafted.change.values, null) ?? null, value);
        if (drafted.error || (drafted.dropped.includes(field) && !toFormula)) {
          const message = drafted.error ?? `${field} is a formula in the new row; it is left empty`;
          if (drafted.error || by === 'person') out.rejected.push({ ...where(i), field, message });
          else out.leftOut.push(field);
          if (drafted.error) continue;
        }
        let next = drafted.change;
        const after = proposedOf(next, field);
        const marks = { ...next.personEdits };
        // The assistant wrote another value in a doubtful cell (the person told it): no longer a doubt.
        if (by === 'ai' && !same(after, current)) next = dropDoubt(next, field);
        // The person took the assistant's reading again with its button: they looked at it.
        if (by === 'person' && raw === AI_VALUE) next = setChecked(next, field, true, who, 'ai-value');
        // A cell edited in the sheet since it was read: what the person writes there goes over it.
        if (after === undefined || by === 'ai') next = forget(next, field);
        else if (!next.create) {
          const record = store.getRecord(next.recordId);
          next = editedInSheet(next, record, field) ? decide(next, record, field, 'proposal', who) : forget(next, field);
        }
        if (by === 'ai') delete marks[field];
        else {
          const ai = mark ? mark.ai : current;
          // What the person saw when they started typing was replaced by the assistant meanwhile.
          if (op.before && field in op.before && !same(op.before[field], current) && !same(current, after))
            out.overrode.push({ ...where(i), field, ai: current ?? null });
          // Back to what the assistant proposed (or, where it proposed nothing, to no change): no longer theirs.
          // A cell set back to the sheet keeps the assistant's value aside, even one that emptied it (null).
          const back = after === undefined || ai === undefined ? after === ai : same(after, ai);
          if (back) delete marks[field];
          else marks[field] = { ...(ai === undefined ? {} : { ai }), by: who, at: now() };
        }
        rows[i] = { ...next, personEdits: Object.keys(marks).length ? marks : undefined };
        // A notebook line shown only for context becomes a real change once someone gives it a value.
        if (rows[i].context && Object.keys(rows[i].values).length) rows[i] = { ...rows[i], context: undefined };
      }
    }

    // Cells edited in the sheet since they were read: the sheet's value kept, or the proposal's written over it.
    for (const op of ops.sheet ?? []) {
      const i = find(op.ref);
      const row = rows[i];
      if (i < 0 || row.create || !(op.field in row.values) || !['sheet', 'proposal'].includes(op.use)) continue;
      const record = store.getRecord(row.recordId);
      if (editedInSheet(row, record, op.field)) rows[i] = decide(row, record, op.field, op.use, who);
    }

    // Doubtful cells marked checked (or unchecked) in the table, or by the assistant on the person's word.
    for (const op of ops.check ?? []) {
      const i = find(op.ref);
      if (i < 0 || !rows[i].doubts?.[op.field]) continue;
      rows[i] = setChecked(rows[i], op.field, op.checked !== false, who, op.how ?? (by === 'ai' ? 'chat' : 'table'));
    }

    const removing = new Set();
    for (const ref of ops.remove ?? []) {
      const i = find(ref);
      if (i < 0) continue;
      if (by === 'ai' && !force && Object.keys(rows[i].personEdits ?? {}).length) {
        out.conflicts.push({ ...where(i), field: null, message: 'The person edited this row in the table; it was kept' });
        continue;
      }
      removing.add(i);
    }
    rows = rows.filter((_, i) => !removing.has(i));

    const add = ops.add;
    if (add && ((add.changes ?? []).length || (add.newRows ?? []).length)) {
      const proposed = new Set(rows.filter(c => c.create).flatMap(c => uniqueKeys(c.sheet, c.values).map(k => k[2])));
      const drafted = draftChanges(add, { proposed, used });
      if (drafted.error) out.rejected.push({ message: drafted.error });
      else {
        rows.push(...drafted.changes.map(({ dropped, ...c }) => c));
        out.leftOut.push(...drafted.changes.flatMap(c => c.dropped ?? []));
      }
    }
    for (const { sheet } of ops.addEmpty ?? []) {
      if (!moduleMap.has(sheet)) continue;
      rows.push({ create: true, sheet, clientId: randomUUID(), recordId: null, row: null, label: '', before: {}, values: {}, replaceFormula: [], note: '' });
    }
    if (rows.length > PROPOSAL_ROWS) out.rejected.push({ message: tooManyRows(rows.length) });
    out.leftOut = [...new Set(out.leftOut)];
    return { changes: rows, ...out };
  }

  /**
   * Saves a revised proposal (only while pending): its revision goes up and the
   * Asistente tab follows it at once (`page`: the page whose edit it is, which has it already).
   */
  function saveRevision(proposal, changes, by, reason = null, page = null) {
    const row = db
      .prepare(
        "UPDATE ai_proposals SET changes_json = ?, reason = coalesce(?, reason), revision = revision + 1, updated_at = ?, last_by = ? WHERE id = ? AND status = 'pending' RETURNING revision",
      )
      .get(json(changes), reason, now(), by, proposal.id);
    if (row) changed(proposal.owner_id, page);
    return row?.revision ?? null;
  }

  const ownProposal = (id, user) =>
    db.prepare('SELECT * FROM ai_proposals WHERE id = ? AND owner_id = ?').get(String(id ?? ''), owner(user));
  /**
   * Any person's proposal, for the app's table (Cambios propuestos): whoever has its chat on screen
   * can review, edit and apply it (a chat handed to someone else to finish). The assistant's own
   * tools still reach only the proposals of the person they act for.
   */
  const anyProposal = id => db.prepare('SELECT * FROM ai_proposals WHERE id = ?').get(String(id ?? ''));
  /** A proposal the person may see in the table: their own, or anyone's for those who may edit proposals. */
  const teamProposal = (id, user) => {
    const found = anyProposal(id);
    return found && (found.owner_id === owner(user) || EDITORS.includes(user.role)) ? found : undefined;
  };
  /** Whose proposals a T3 chat holds (or will): its proposals' owner, else the owner of the chat's T3 project. */
  function ownerOfChat(threadId) {
    const made = db.prepare('SELECT owner_id FROM ai_proposals WHERE t3_thread = ? LIMIT 1').get(threadId);
    if (made) return made.owner_id;
    const username = t3?.ownerOf?.(threadId);
    return username ? (db.prepare('SELECT id FROM users WHERE username = ?').get(username)?.id ?? null) : null;
  }
  const ownProposalListed = id =>
    db.prepare('SELECT p.*, t.title FROM ai_proposals p JOIN ai_threads t ON t.id = p.thread_id WHERE p.id = ?').get(id);
  /**
   * A proposal as Cambios propuestos lists it (with the conversation it comes from: its T3 chat, if
   * known), and a digest of it all: a page that holds it already gets only { id, digest, same: true }.
   */
  const listedView = (r, titles = new Map()) => {
    const view = {
      ...(isTable(r) ? tableView(r) : proposalView({ id: r.id, changes: [], reason: r.reason, status: r.status }, r)),
      createdAt: r.created_at,
      source: (r.t3_thread && (titles.get(r.t3_thread)?.title ?? r.t3_title)) || r.title,
      chat: r.t3_thread || null,
    };
    return { ...view, digest: createHash('sha1').update(json(view)).digest('base64url').slice(0, 12) };
  };

  /**
   * What the sheet did to a pending proposal's row since it was drafted (`since`):
   * its cells edited there (server/sheet-edits.mjs) with who and when, and for a
   * new row, its pre-made row taken (`inUse`: takenRows of its proposal). Empty
   * for a row the sheet left alone.
   */
  function sheetState(change, since, inUse = null) {
    if (change.create) {
      const taken = takenRow(store, change, inUse);
      return taken ? { rowTaken: { row: taken.row, label: taken.label, ...lastEdit(db, taken.recordId, 'Insectary_ID', since) } } : {};
    }
    const edited = sheetChangesOf(change, store.getRecord(change.recordId));
    if (!Object.keys(edited).length) return {};
    return {
      sheetChanged: Object.fromEntries(
        Object.entries(edited).map(([field, cell]) => [field, { ...cell, ...lastEdit(db, change.recordId, field, since) }]),
      ),
    };
  }

  /** The rows of a proposal as the assistant reads them: index, values with readable dates, the person's edits. */
  function proposalTable(changes, proposal = null) {
    const readable = (sheet, field, value) =>
      moduleMap.get(sheet)?.fields.find(f => f.key === field)?.type === 'date' && typeof value === 'number' ? isoDate(value) : value;
    const open = proposal?.status === 'pending';
    const inUse = open ? takenRows(store, changes) : new Map();
    // Cells edited in the sheet since they were read, as the table shows them: what applying does with each.
    const sheetOf = c => {
      const state = open && !c.context ? sheetState(c, proposal.created_at, inUse) : {};
      return {
        ...(state.sheetChanged
          ? {
              sheetChanged: Object.fromEntries(
                Object.entries(state.sheetChanged).map(([f, e]) => [
                  f,
                  {
                    read: readable(c.sheet, f, e.read),
                    now: readable(c.sheet, f, e.now),
                    ...(e.by ? { by: e.by } : e.source ? { by: e.source === 'app' ? 'app' : 'Google Sheets' } : {}),
                    ...(e.at ? { at: e.at } : {}),
                    applying: e.again
                      ? 'edited again after the person chose: not applied until they look'
                      : e.use === 'proposal'
                        ? 'yours, over the sheet (the person chose)'
                        : "keeps the sheet's",
                  },
                ]),
              ),
            }
          : {}),
        ...(state.rowTaken ? { rowTaken: `row ${state.rowTaken.row} is in use now (${state.rowTaken.label}): this new row is left out` } : {}),
      };
    };
    return changes.map((c, index) => ({
      index,
      sheet: c.sheet,
      label: c.label,
      ...(c.create ? { create: true } : { row: c.row, recordId: c.recordId }),
      ...(c.context ? { context: 'already in the sheet: shown for context, never written' } : {}),
      // An existing row's cell to be emptied reads as it is given: { clear: true } (null means no change).
      values: Object.fromEntries(
        Object.entries(c.values).map(([f, v]) => [f, v === null && !c.create ? { clear: true } : readable(c.sheet, f, v)]),
      ),
      ...(c.note ? { note: c.note } : {}),
      // Doubtful cells (match_notebook): unchecked ones must be checked by the person before applying.
      ...(c.doubts && Object.keys(c.doubts).some(f => f in c.values)
        ? {
            doubtful: Object.fromEntries(
              Object.entries(c.doubts)
                .filter(([f]) => f in c.values)
                .map(([f, d]) => [
                  f,
                  {
                    alternatives: (d.alternatives ?? []).map(a => readable(c.sheet, f, a)),
                    ...(d.reason ? { reason: d.reason } : {}),
                    checked: !!d.checked || !!c.personEdits?.[f],
                  },
                ]),
            ),
          }
        : {}),
      // Cells match_notebook could not read: the person fills them (filled: a value was given since).
      ...(c.unreadable && Object.keys(c.unreadable).length
        ? {
            unreadable: Object.fromEntries(
              Object.entries(c.unreadable).map(([f, u]) => [
                f,
                {
                  ...(u?.reason ? { reason: u.reason } : {}),
                  ...(u?.partial?.length ? { partial: u.partial } : {}),
                  filled: f in c.values,
                },
              ]),
            ),
          }
        : {}),
      ...(c.personEdits
        ? {
            personEdits: Object.fromEntries(
              Object.entries(c.personEdits).map(([f, m]) => [
                f,
                {
                  // A cell the person set back: an existing row keeps the sheet's value, a new row's stays empty.
                  value:
                    f in c.values
                      ? readable(c.sheet, f, c.values[f])
                      : c.create
                        ? 'left empty (not written)'
                        : 'no change (keep the sheet value)',
                  youProposed: 'ai' in m ? readable(c.sheet, f, m.ai) : 'no change',
                },
              ]),
            ),
          }
        : {}),
      ...sheetOf(c),
    }));
  }

  /**
   * The rows of a proposal's table that wait for a look: cells the person edited, doubtful
   * cells not checked (with the value proposed), unreadable cells still empty, cells edited
   * in the sheet since, a new row whose pre-made row was taken.
   */
  function attentionRows(table) {
    return table.flatMap(v => {
      const doubtful = Object.entries(v.doubtful ?? {}).filter(([, d]) => !d.checked);
      const unreadable = Object.entries(v.unreadable ?? {}).filter(([, u]) => !u.filled);
      const need = {
        ...(v.personEdits ? { personEdits: v.personEdits } : {}),
        ...(doubtful.length ? { doubtful: Object.fromEntries(doubtful.map(([f, d]) => [f, { value: v.values[f], ...d }])) } : {}),
        ...(unreadable.length ? { unreadable: Object.fromEntries(unreadable) } : {}),
        ...(v.sheetChanged ? { sheetChanged: v.sheetChanged } : {}),
        ...(v.rowTaken ? { rowTaken: v.rowTaken } : {}),
      };
      return Object.keys(need).length ? [{ index: v.index, label: v.label, ...need }] : [];
    });
  }

  /**
   * What a revision changed, as update_proposal answers it: the rows whose view changed (as
   * get_proposal full shows them), the rows taken out (their old index) and then every row's
   * label by its new index, how many rows there are and how many doubtful cells wait unchecked.
   */
  function revisedRows(old, fresh, table, proposal) {
    const plain = view => {
      const rest = { ...view };
      delete rest.index;
      return json(rest);
    };
    const before = new Map(proposalTable(old, proposal).map((v, i) => [rowKey(old[i]), plain(v)]));
    const kept = new Set(fresh.map(rowKey));
    const changed = table.filter((v, i) => before.get(rowKey(fresh[i])) !== plain(v));
    const removed = old.flatMap((c, i) => (kept.has(rowKey(c)) ? [] : [{ index: i, label: c.label }]));
    const doubtful = uncheckedDoubts(fresh).length;
    return {
      rows: fresh.length,
      changed,
      ...(removed.length ? { removed, labels: fresh.map(c => c.label) } : {}),
      ...(doubtful ? { doubtfulUnchecked: doubtful } : {}),
    };
  }

  /** A needs_review check as get_proposal gives it: few cells one by one, many as their rows and columns. */
  function sheetCheckOf(check) {
    const differ = check?.differ ?? [];
    if (differ.length <= 20) return check;
    const rows = new Map();
    for (const d of differ) {
      const key = `${d.index}`;
      if (!rows.has(key)) rows.set(key, { index: d.index, label: d.label, row: d.row, fields: [], sheetEmpty: true });
      const r = rows.get(key);
      r.fields.push(d.field);
      if (d.sheet !== null && d.sheet !== '') r.sheetEmpty = false;
    }
    return { at: check.at, matched: check.matched, differing: check.count ?? differ.length, rows: [...rows.values()] };
  }

  /**
   * get_proposal: its state and the rows that wait for a look (attentionRows), each row's
   * label by index; `full`: every row as the table shows it, from row `offset` on, as many
   * as fit in one answer.
   */
  function getProposal(args, context) {
    const proposal = ownProposal(args.proposalId, context.user);
    if (!proposal) return { error: 'Proposal not found' };
    if (isTable(proposal)) return { error: NOT_A_PROPOSAL };
    const changes = parse(proposal.changes_json) ?? [];
    const table = proposalTable(changes, proposal);
    const head = {
      proposalId: proposal.id,
      ...proposalLink(proposal.id, chatOf(proposal, context)),
      status: proposal.status,
      revision: proposal.revision,
      reason: proposal.reason,
      lastChangedBy: proposal.last_by ?? 'ai',
      // needs_review: what the sheet holds of it, as compared after the last sync.
      ...(proposal.status === 'needs_review' && proposal.check_json ? { sheetCheck: sheetCheckOf(parse(proposal.check_json)) } : {}),
    };
    if (args.full === true) {
      const from = Math.min(Math.max(Number(args.offset) || 0, 0), table.length);
      const rest = table.slice(from);
      const more = shown => (shown < rest.length ? { truncated: true, next: `Rows ${from + shown}–${table.length - 1} not shown: get_proposal with full: true and offset: ${from + shown}` } : {});
      return fitList({ ...head, total: table.length, rows: rest }, 'rows', RESULT_BUDGET - 500, more).out;
    }
    const contextRows = changes.filter(c => c.context).length;
    const attention = attentionRows(table);
    return {
      ...head,
      rows: table.length,
      labels: table.map(v => v.label),
      ...(contextRows ? { contextRows } : {}),
      attention,
      ...orderDiffers(proposal),
    };
  }

  /** list_proposals: the proposals of the chat calling (all chats' pending ones when it is not known, or asked). */
  function listProposals(args, context) {
    const chat = !args.allChats && context.t3 ? (chatOfCall(context)?.id ?? null) : null;
    const select = `SELECT id, status, reason, changes_json, table_json, created_at, t3_thread FROM ai_proposals WHERE owner_id = ?${chat ? ' AND t3_thread = ?' : ''}`;
    const params = [owner(context.user), ...(chat ? [chat] : [])];
    const order = 'ORDER BY created_at DESC, rowid DESC';
    const rows = [
      ...db.prepare(`${select} AND status IN ('pending', 'applying', 'shown') ${order} LIMIT 50`).all(...params),
      ...(chat ? db.prepare(`${select} AND status NOT IN ('pending', 'applying', 'shown') ${order} LIMIT 10`).all(...params) : []),
    ];
    return {
      ...(chat ? { chat, chatLink: chatProposalsLink(chat) } : { chat: 'all chats (pending only)' }),
      proposals: rows.map(r => ({
        ...(r.table_json ? { tableId: r.id, kind: 'table (show_rows)' } : { proposalId: r.id }),
        status: r.status,
        reason: r.reason,
        rows: r.table_json ? (parse(r.table_json)?.rows ?? []).length : (parse(r.changes_json) ?? []).length,
        createdAt: r.created_at,
        ...proposalLink(r.id, r.t3_thread),
      })),
    };
  }

  // ------------------------------------------------------------ tables of rows (show_rows)
  /*
   * A table the assistant shows the person beside the chat: rows of one sheet,
   * the columns it chose and its notes. Kept as a row of ai_proposals (so it
   * is listed, linked to its T3 chat and followed live like a proposal) with
   * `table_json` and no changes; its status is 'shown', then 'closed', never
   * 'pending', so nothing can apply it. It keeps the rows' IDs, not their
   * values: the panel reads them from the sheet's copy each time.
   */
  const isTable = proposal => !!proposal?.table_json;
  const NOT_A_PROPOSAL = 'That is a table shown with show_rows: it changes nothing. Draft changes with propose_changes.';

  /** The rows a show_rows call names, in sheet order: { ids, missing } or { error }. */
  function tableRows(sheet, args) {
    const ids = new Set();
    const missing = [];
    const recordIds = Array.isArray(args.recordIds) ? args.recordIds.slice(0, 2000) : [];
    // App recordIds, or the rows' IDs in the sheet (W2B).
    const named = resolveRows(store, recordIds, { sheet });
    for (const p of named.problems) {
      if (p.kind === 'missing') missing.push(clip(typeof p.ref === 'object' ? JSON.stringify(p.ref) : p.ref, 120));
      else return { error: / is in \S+, not \S+$/.test(p.error) ? `${p.error}: one sheet per table` : p.error };
    }
    for (const id of named.ids) if (id) ids.add(id);
    const query = args.filters !== undefined || args.field !== undefined || args.values !== undefined;
    if (query) {
      const out = selectRecords(store, { module: sheet, field: args.field, values: args.values, filters: args.filters });
      if (out.error) return { error: out.error };
      for (const { record } of out.rows) ids.add(record.id);
      missing.push(...(out.missing ?? []));
    }
    if (!recordIds.length && !query) return { error: 'Give the rows: recordIds, filters, or field + values' };
    if (ids.size > TABLE_ROWS)
      return {
        error: `${ids.size} rows match; a table holds at most ${TABLE_ROWS}. Narrow the filters (a species, a period), or show it in parts.`,
      };
    if (!ids.size)
      return { error: `No rows found${missing.length ? ` (not found: ${missing.slice(0, 20).join(', ')})` : ''}` };
    const rowOf = id => store.getRecord(id)?.row ?? Infinity;
    return { ids: [...ids].sort((a, b) => rowOf(a) - rowOf(b)), missing };
  }

  /** The columns of a table: the ones asked (checked), else the ID columns and then the filled ones. */
  function tableColumns(mod, ids, args, noted) {
    const known = new Set(mod.fields.map(f => f.key));
    if (args.columns !== undefined) {
      if (!Array.isArray(args.columns) || !args.columns.length) return { error: 'columns must be a list of columns' };
      const named = columnKeys(mod, args.columns);
      if (named.error) return { error: `columns: ${named.error}` };
      return { columns: [...new Set([...named.keys, ...noted])].slice(0, 80) };
    }
    const keys = [...new Set(mod.fields.map(f => f.key))];
    const idColumns = mod.identityFields.length ? mod.identityFields : keys.filter(k => ID_LIKE.test(k));
    const filled = new Set();
    for (const id of ids) {
      const values = store.getRecord(id)?.values ?? {};
      for (const k of keys) if (!filled.has(k) && !isNone(values[k])) filled.add(k);
    }
    const filtered =
      args.filters && typeof args.filters === 'object' ? Object.keys(args.filters).map(k => columnOf(mod, k).key).filter(k => known.has(k)) : [];
    const first = [...idColumns.filter(k => filled.has(k)), ...filtered, ...noted];
    const rest = keys.filter(k => filled.has(k) && !first.includes(k));
    return { columns: [...new Set([...first, ...rest])].slice(0, Math.max(TABLE_COLUMNS, new Set(first).size)) };
  }

  /**
   * The assistant's notes, by row: { note, cells: field → text, highlight, marked: [fields] };
   * `skipped`: notes on rows not in the table or on columns the sheet does not have.
   */
  function tableNotes(list, ids, mod) {
    const rows = new Set(ids);
    const notes = {};
    const skipped = [];
    const given = (Array.isArray(list) ? list : []).slice(0, 2000);
    // A note's row by its recordId or its ID in the sheet (W2B).
    const named = resolveRows(
      store,
      given.map(n => clip(n?.recordId ?? n?.id, 120)),
      { sheet: mod.id },
    ).ids;
    for (const [i, n] of given.entries()) {
      const id = named[i] ?? clip(n?.recordId ?? n?.id, 120);
      const text = typeof n?.text === 'string' ? clip(n.text.trim(), 500) : '';
      const field = typeof n?.field === 'string' && n.field ? (columnOf(mod, clip(n.field, 120)).key ?? null) : null;
      if (!rows.has(id) || (n?.field && !field)) {
        if (id) skipped.push(n?.field && rows.has(id) ? `${id} ${clip(n.field, 120)}` : id);
        continue;
      }
      const at = (notes[id] ??= {});
      if (field) {
        if (text) at.cells = { ...at.cells, [field]: text };
        if (n.highlight === true) at.marked = [...new Set([...(at.marked ?? []), field])];
      } else {
        if (text) at.note = at.note ? `${at.note} · ${text}` : text;
        if (n.highlight === true) at.highlight = true;
      }
      if (!Object.keys(at).length) delete notes[id];
    }
    return { notes, skipped: [...new Set(skipped)] };
  }

  /** show_rows: a read-only table of rows beside the chat, new or (tableId) changed in place. */
  function showRows(args, context) {
    const old = args.tableId ? ownProposal(args.tableId, context.user) : null;
    if (args.tableId && !isTable(old)) return { error: 'Table not found (tableId comes from an earlier show_rows)' };
    const before = old ? (parse(old.table_json) ?? {}) : {};
    const sheet = String(args.sheet ?? before.sheet ?? '');
    const mod = moduleMap.get(sheet);
    if (!mod) return { error: `Unknown sheet ${clip(sheet, 60)}` };
    const title = clip(String(args.title ?? old?.reason ?? '').trim(), 300);
    if (!title) return { error: 'Give the table a title' };
    const rowsAsked = ['recordIds', 'filters', 'field', 'values'].some(k => args[k] !== undefined);
    if (old && sheet !== before.sheet && !rowsAsked) return { error: 'Another sheet: give its rows too' };
    const picked = rowsAsked || !old ? tableRows(sheet, args) : { ids: before.rows ?? [], missing: [] };
    if (picked.error) return picked;
    const { ids } = picked;
    const { notes, skipped } =
      args.notes !== undefined || !old ? tableNotes(args.notes, ids, mod) : { notes: before.notes ?? {}, skipped: [] };
    // A note on a cell of a column the table lacks brings that column in.
    const noted = [...new Set(Object.values(notes).flatMap(n => [...Object.keys(n.cells ?? {}), ...(n.marked ?? [])]))];
    const keep = old && args.columns === undefined && sheet === before.sheet;
    const shown = keep
      ? { columns: [...new Set([...(before.columns ?? []), ...noted])] }
      : tableColumns(mod, ids, args, noted);
    if (shown.error) return shown;
    const labels = Object.fromEntries(ids.map(id => [id, store.getRecord(id)?.label ?? before.labels?.[id] ?? '']));
    const table = { sheet, columns: shown.columns, rows: ids, labels, notes };
    let id, chat;
    if (old) {
      db.prepare(
        `UPDATE ai_proposals SET table_json = ?, reason = ?, status = 'shown', revision = revision + 1, updated_at = ?,
         last_by = 'ai' WHERE id = ?`,
      ).run(json(table), title, now(), old.id);
      id = old.id;
      chat = chatOf(old, context);
    } else {
      id = randomUUID();
      const found = context.t3 ? chatOfCall(context) : null;
      chat = found?.id ?? null;
      const at = now();
      db.prepare(
        `INSERT INTO ai_proposals (id,thread_id,owner_id,changes_json,reason,status,created_at,updated_at,last_by,
         t3_thread,t3_title,t3_tool_use,table_json) VALUES (?,?,?,'[]',?,'shown',?,?,'ai',?,?,?,?)`,
      ).run(id, context.threadId, owner(context.user), title, at, at, chat, found?.title ?? null, context.t3?.toolUseId ?? null, json(table));
    }
    changed(owner(context.user));
    return {
      tableId: id,
      ...proposalLink(id, chat),
      status: 'shown to the person beside the chat',
      sheet,
      rows: ids.length,
      columns: shown.columns,
      ...(picked.missing.length ? { notFound: picked.missing.slice(0, 50) } : {}),
      ...(skipped.length
        ? { notesNotShown: skipped.slice(0, 20), notesNote: 'These rows or columns are not in the table: their notes were left out.' }
        : {}),
    };
  }

  /**
   * A table as the panel shows it: each row with the current values of its
   * columns (formula cells computed), its note and the notes on its cells; a
   * row gone from the sheet stays, empty and marked. `sheetStamp` changes when
   * any of its rows changes, so the panel redraws it.
   */
  function tableView(r) {
    const spec = parse(r.table_json) ?? {};
    const mod = moduleMap.get(spec.sheet);
    const columns = spec.columns ?? [];
    const versions = [];
    const rows = (spec.rows ?? []).map(id => {
      const record = store.getRecord(id);
      const live = !!record && !record.missing;
      versions.push(live ? [record.version, record.row] : null);
      const n = spec.notes?.[id] ?? {};
      return {
        key: id,
        recordId: id,
        row: live ? record.row : null,
        label: (live && record.label) || spec.labels?.[id] || '',
        values: live ? Object.fromEntries(columns.map(f => [f, record.values?.[f] ?? null])) : {},
        ...(live ? {} : { missing: true }),
        ...(n.note ? { note: n.note } : {}),
        ...(n.cells ? { cells: n.cells } : {}),
        ...(n.highlight ? { highlight: true } : {}),
        ...(n.marked?.length ? { marked: n.marked } : {}),
      };
    });
    rows.sort((a, b) => (a.row ?? Infinity) - (b.row ?? Infinity));
    return {
      id: r.id,
      kind: 'table',
      reason: r.reason ?? '',
      status: r.status,
      sheets: [spec.sheet],
      fields: columns,
      types: Object.fromEntries(columns.map(f => [f, mod?.fields.find(x => x.key === f)?.type ?? 'text'])),
      revision: r.revision ?? 1,
      updatedAt: r.updated_at ?? null,
      lastBy: r.last_by ?? null,
      appliedAt: null,
      applied: null,
      sheetStamp: createHash('sha1').update(json(versions)).digest('base64url').slice(0, 12),
      changes: [],
      rows,
    };
  }

  /** update_proposal: the assistant revises a pending proposal the person is looking at. */
  function updateProposal(args, context) {
    if (!EDITORS.includes(context.user.role)) return { error: 'Your role cannot propose edits' };
    const proposal = ownProposal(args.proposalId, context.user);
    if (!proposal) return { error: 'Proposal not found' };
    if (isTable(proposal)) return { error: NOT_A_PROPOSAL };
    if (proposal.status !== 'pending')
      return { error: `The proposal is ${proposal.status}; draft a new one with propose_changes` };
    const changes = parse(proposal.changes_json) ?? [];
    const problems = [];
    // A row of the proposal by its index, or by its recordId, ID or label (W2B).
    const rowIndex = (r, at) => {
      if (Number.isInteger(r?.index)) return r.index;
      const name = r?.recordId ?? r?.id;
      if (name === undefined || name === null || String(name).trim() === '') {
        problems.push(`${at}: give the row's index (or its ID)`);
        return -1;
      }
      const text = String(name).trim().toLowerCase();
      let i = changes.findIndex(c => c.recordId === String(name));
      if (i < 0) {
        const named = changes.flatMap((c, j) => (String(c.label ?? '').trim().toLowerCase() === text ? [j] : []));
        if (named.length > 1) {
          problems.push(`${at}: ${clip(name, 60)} names rows ${named.join(', ')} of the proposal; give the index`);
          return -1;
        }
        i = named[0] ?? -1;
      }
      if (i < 0) {
        const { ids } = resolveRows(store, [r.recordId !== undefined ? { recordId: r.recordId } : { id: r.id, sheet: r.sheet }]);
        if (ids[0]) i = changes.findIndex(c => c.recordId === ids[0]);
      }
      if (i < 0) problems.push(`${at}: ${clip(name, 60)} is not a row of this proposal; give its index, or add it with changes`);
      return i;
    };
    // The assistant's values: null takes back its change to a cell, { clear: true } empties it, notes are added.
    const own = (i, values, at) => {
      const row = changes[i];
      const named = row ? sheetNames(row.sheet, values, at) : { values };
      if (named.error) problems.push(named.error);
      const record = row && !row.create ? store.getRecord(row.recordId) : null;
      return fromAssistant(record, named.values ?? {}, context.user, { keepDrops: true }).values;
    };
    const rowArgs = Array.isArray(args.rows) ? args.rows : [];
    const set = rowArgs.map((r, k) => {
      const ref = rowIndex(r, `rows[${k}]`);
      return { ref, values: own(ref, r?.values, `rows[${k}]`), note: r?.note };
    });
    // Doubtful cells the person confirmed in the chat ("sí, es un 7").
    const check = rowArgs.flatMap((r, k) => {
      const ref = set[k].ref;
      if (ref < 0 || !Array.isArray(r?.checked)) return [];
      return r.checked
        .filter(f => typeof f === 'string')
        .map(field => ({ ref, field: changes[ref] ? (columnOf(changes[ref].sheet, field).key ?? field) : field, checked: true, how: 'chat' }));
    });
    const listed = withRecordIds(args.changes);
    if (listed.error) return listed;
    const extra = assistantArgs({ changes: [], newRows: args.newRows }, context.user);
    if (extra.error) return extra;
    const add = { changes: [], newRows: extra.args.newRows };
    // A row already in the proposal is revised, not added twice.
    for (const [k, c] of listed.changes.entries()) {
      const i = changes.findIndex(r => !r.create && r.recordId === c.recordId);
      if (i >= 0) set.push({ ref: i, values: own(i, c.values, `changes[${k}]`), note: c.note });
      else {
        const more = assistantArgs({ changes: [c] }, context.user);
        if (more.error) return more;
        add.changes.push(...more.args.changes);
      }
    }
    // Rows to take out, by index or ID.
    const remove = (Array.isArray(args.removeRows) ? args.removeRows : [])
      .map((r, k) => (Number.isInteger(r) ? r : typeof r === 'string' ? rowIndex({ id: r }, `removeRows[${k}]`) : -1))
      .filter(i => i >= 0);
    if (problems.length) return { error: 'Nothing was changed', problems: problems.slice(0, 20) };
    // The page's photos (attachments of this chat), for a proposal made without them.
    const chat = proposal.t3_thread || (context.t3 ? chatOfCall(context)?.id : null) || null;
    const given = args.photo ? photosOf(config.t3?.home, args, chat) : null;
    if (given && !given.photos.length)
      return { error: 'No photo of this chat by that name', photoNote: 'Give `photo` as the file name of this chat\'s attachment, from "[Attached image … saved at …]"' };
    const viewGiven = args.view !== undefined && args.view !== null;
    if (!set.length && !check.length && !remove.length && !add.changes.length && !add.newRows.length && !args.reason && !given && !viewGiven)
      return { error: 'Give rows, changes, newRows, removeRows, photo or view' };
    const out = reviseChanges(changes, { set, check, remove, add }, { by: 'ai', force: !!args.overridePersonEdits, user: context.user });
    if (out.rejected.length) return { error: 'Nothing was changed', problems: out.rejected.slice(0, 20) };
    const oldView = parse(proposal.view_json ?? 'null');
    const view = viewGiven ? readView(args.view, [...new Set(out.changes.map(c => c.sheet))], oldView) : oldView;
    if (view?.error) return { error: 'Nothing was changed', problems: [view.error] };
    const reason = args.reason ? clip(args.reason, 500) : null;
    if (given) {
      const page = parse(proposal.page_json ?? 'null') ?? {};
      db.prepare('UPDATE ai_proposals SET page_json = ? WHERE id = ?').run(json({ ...page, photos: given.photos }), proposal.id);
    }
    const viewChanged = json(view) !== json(oldView);
    if (viewChanged) db.prepare('UPDATE ai_proposals SET view_json = ? WHERE id = ?').run(view ? json(view) : null, proposal.id);
    const unchanged = json(out.changes) === json(changes) && !reason && !given && !viewChanged;
    const revision = unchanged ? proposal.revision : saveRevision(proposal, out.changes, 'ai', reason);
    if (revision === null) return { error: 'The proposal is no longer pending' };
    const table = proposalTable(out.changes, proposal);
    return {
      proposalId: proposal.id,
      ...proposalLink(proposal.id, chatOf(proposal, context)),
      revision,
      ...(unchanged ? { unchanged: true } : {}),
      ...(given ? { photos: given.photos.length, ...(given.refused.length ? { photoNotShown: given.refused } : {}) } : {}),
      ...(args.full === true ? { rows: table } : revisedRows(changes, out.changes, table, proposal)),
      ...(out.leftOut.length ? { leftOut: `Formula columns left out of the new rows: ${out.leftOut.join(', ')}` } : {}),
      ...(out.conflicts.length
        ? {
            conflicts: out.conflicts,
            note: 'The person edited these cells by hand; they were kept. Tell the person, and only replace them (overridePersonEdits) if they ask.',
          }
        : {}),
    };
  }

  /**
   * A notebook page matched again (match_notebook with replaceProposalId) takes
   * the place of the page's proposal, keeping its id; the person's cells stay,
   * and are conflicts where the new match reads something else.
   */
  function carryPersonEdits(old, fresh, user) {
    const sameRow = (a, b) =>
      a.create ? b.create && a.sheet === b.sheet && !!a.label && a.label === b.label : !b.create && a.recordId === b.recordId;
    // New rows keep their key, so the table keeps its ticks.
    const keyed = fresh.map(c => {
      const before = c.create && old.find(o => sameRow(o, c));
      return before ? { ...c, clientId: before.clientId } : c;
    });
    // Doubtful cells already checked stay checked while the new reading gives the same value.
    const rows = carryChecks(old, keyed, sameRow, same);
    const conflicts = [];
    const set = [];
    for (const o of old) {
      const edits = Object.entries(o.personEdits ?? {});
      if (!edits.length) continue;
      const i = rows.findIndex(c => sameRow(o, c));
      if (i < 0) {
        conflicts.push({ label: o.label, sheet: o.sheet, field: null, message: 'A row the person edited is no longer in the match; their edits were dropped' });
        continue;
      }
      for (const [field, mark] of edits) {
        const person =
          field in o.values ? o.values[field] : o.create ? null : (store.getRecord(o.recordId)?.values?.[field] ?? null);
        const ai = proposedOf(rows[i], field);
        if (ai !== undefined && !same(ai, person) && !same(ai, mark.ai))
          conflicts.push({ index: i, label: rows[i].label, field, person, yours: ai });
        set.push({ ref: i, values: { [field]: person } });
      }
    }
    const out = reviseChanges(rows, { set }, { by: 'person', user });
    for (const r of out.rejected) conflicts.push({ ...r, message: `The person's value no longer fits: ${r.message}` });
    return { changes: out.changes, conflicts };
  }

  /**
   * Writes the chosen rows of a proposal as one save (undoable in Historial).
   * Cells edited in the sheet since they were read keep the sheet's value unless
   * the person chose the proposal's (server/sheet-edits.mjs): listed in the
   * answer as `keptFromSheet`. One edited again after the person chose stops it.
   */
  async function applyProposal(proposal, user, { requestId, indexes, reason, doubtful = null }) {
    if (isTable(proposal)) throw Object.assign(new Error(NOT_A_PROPOSAL), { status: 409, code: 'read_only_table' });
    if (proposal.status !== 'pending')
      throw Object.assign(new Error('Proposal has already been applied.'), { status: 409, code: 'proposal_used' });
    if (!EDITORS.includes(user.role))
      throw Object.assign(new Error('Your role cannot apply changes.'), { status: 403, code: 'forbidden' });
    // A row whose record a sync replaced (same sheet row and label, a new record): applied to that one.
    const all = (parse(proposal.changes_json) ?? []).map(c => {
      if (c.create || c.context || !c.recordId) return c;
      const record = currentRecord(store, c);
      return record && record.id !== c.recordId ? { ...c, recordId: record.id } : c;
    });
    // A row left without values (the person emptied it in the table) has nothing to write, and a
    // notebook line shown only for context (match_notebook includeUnchanged) is never written.
    const picked = (
      Array.isArray(indexes) && indexes.length ? [...new Set(indexes.map(Number))].filter(i => all[i]) : all.map((_, i) => i)
    ).filter(i => Object.keys(all[i].values ?? {}).length && !all[i].context);
    // The sheet's edits since the proposal was read: kept (left out), or written over as the person chose.
    const sheet = resolveSheetEdits(store, picked.map(i => [i, all[i]]));
    if (sheet.again.length)
      throw Object.assign(new Error(`${sheet.again.length} cells were edited in the sheet again after you chose; look at them first.`), {
        status: 409,
        code: 'sheet_changed_again',
        details: { again: sheet.again },
      });
    const resolved = new Map(sheet.writes);
    // As written: the rows with the sheet's edits resolved (the rest of the proposal as it is).
    const effective = all.map((c, i) => resolved.get(i) ?? c);
    const chosen = picked.filter(i => resolved.has(i) && Object.keys(resolved.get(i).values ?? {}).length);
    // Unreadable cells still empty: never written (they stay as the sheet has them), listed in the answer.
    const unreadable = unfilledUnreadable(all);
    const keptFromSheet = sheet.kept.length ? { keptFromSheet: sheet.kept } : {};
    if (!chosen.length)
      throw Object.assign(
        new Error(
          sheet.kept.length
            ? 'Nothing to write: the sheet was edited in every cell chosen, and its values are kept.'
            : unreadable.length
              ? 'Nothing to write yet: only unreadable cells, still empty.'
              : 'No rows selected.',
        ),
        { status: 400, code: 'nothing_selected', details: { ...(unreadable.length ? { unreadable } : {}), ...keptFromSheet } },
      );
    // Doubtful cells nobody looked at: the person decides first (apply them anyway, or only the sure cells).
    const unchecked = uncheckedDoubts(effective, chosen);
    if (unchecked.length && doubtful !== 'confirm' && doubtful !== 'skip')
      throw Object.assign(new Error(`${unchecked.length} doubtful cells have not been checked.`), {
        status: 409,
        code: 'doubtful_unchecked',
        details: { doubtful: unchecked, unreadable },
      });
    const who = clip(user.displayName || user.username || owner(user), 80);
    // Applied anyway: those cells count as confirmed by the person (kept with the proposal).
    const confirm = rows =>
      doubtful === 'confirm'
        ? rows.map((c, i) =>
            chosen.includes(i) ? unchecked.filter(u => u.index === i).reduce((row, u) => setChecked(row, u.field, true, who, 'apply'), c) : c,
          )
        : rows;
    const kept = confirm(all);
    const ready = confirm(effective);
    const writes = chosen
      .map(i => [i, doubtful === 'skip' ? withoutUnchecked(ready[i]) : ready[i]])
      .filter(([, c]) => Object.keys(c.values ?? {}).length);
    const written = writes.map(([i]) => i);
    if (!writes.length) throw Object.assign(new Error('Only doubtful cells were left to write.'), { status: 400, code: 'nothing_selected' });
    // The app is stopping (a deploy): the proposal stays pending, to apply in a minute.
    if (store.draining)
      throw Object.assign(new Error('The app is restarting; apply it again in a minute.'), { status: 503, code: 'shutting_down' });
    const claimed = db
      .prepare("UPDATE ai_proposals SET status = 'applying' WHERE id = ? AND status = 'pending'")
      .run(proposal.id);
    if (!claimed.changes)
      throw Object.assign(new Error('Proposal is already being applied.'), { status: 409, code: 'proposal_used' });
    changed(proposal.owner_id);
    try {
      const result = await store.applyProposal(
        writes.map(([, c]) => c),
        { user, requestId, reason: clip(reason || proposal.reason, 500) },
      );
      const status = ['verified', 'unchanged'].includes(result?.status) ? 'applied' : 'needs_review';
      const created = Object.fromEntries((result?.created ?? []).map(c => [c.clientId, c.recordId]));
      db.prepare(
        'UPDATE ai_proposals SET status = ?, applied_at = ?, applied_json = ?, created_json = ?, changes_json = ? WHERE id = ?',
      ).run(status, status === 'applied' ? now() : null, json(written), json(created), json(kept), proposal.id);
      // Agreed issues of the Revisión tab whose rows were written: now applied.
      const issueIds = parse(proposal.issues_json ?? 'null');
      if (status === 'applied' && issueIds?.length)
        markApplied(store, issueIds, { recordIds: new Set(written.map(i => all[i].recordId)), proposalId: proposal.id, user });
      return {
        proposalId: proposal.id,
        status,
        applied: written,
        result,
        ...(unchecked.length ? { doubtful: { count: unchecked.length, how: doubtful } } : {}),
        ...(unreadable.length ? { unreadable } : {}),
        ...keptFromSheet,
      };
    } catch (cause) {
      // The save refused it as a whole (nothing was written): still pending, to look at and apply again.
      // A cell someone changed in the sheet the app had not read yet is read now, so the table shows it.
      const refused = cause?.code === 'BATCH_CONFLICT';
      db.prepare('UPDATE ai_proposals SET status = ? WHERE id = ?').run(refused ? 'pending' : 'needs_review', proposal.id);
      if (refused) await readAgain(cause.details?.items ?? []);
      throw cause;
    } finally {
      changed(proposal.owner_id);
    }
  }

  /** The rows of a refused save where someone else changed a cell: read from the sheet again (as the sheet hook does). */
  async function readAgain(items) {
    const bySheet = new Map();
    for (const item of items) {
      if (item.code !== 'EXTERNAL_CONFLICT' || !item.id) continue;
      const record = store.getRecord(item.id);
      if (record && !record.missing && record.row > 0) bySheet.set(record.sheet, [...(bySheet.get(record.sheet) ?? []), record.row]);
    }
    for (const [sheet, rows] of bySheet)
      try {
        const out = await store.refreshRows?.(sheet, rows);
        if (out?.needsSync) await store.sync({ sheets: [sheet] });
      } catch (e) {
        console.error('Proposal rows read again:', e.message);
      }
  }

  /**
   * The sheet row a new row goes into, whose formulas it keeps: its Insectary ID's pre-made row in
   * Insectary_data, else the sheet's next pre-made row. Null when there is none yet.
   */
  function premadeRecordOf(change) {
    if (change.sheet === 'Insectary_data') {
      const place = change.values?.Insectary_ID ? insectaryIdRow(store, change.values.Insectary_ID) : null;
      return place && !place.ahead ? store.getRecordBySheetRow('Insectary_data', place.row) : null;
    }
    const next = db
      .prepare('SELECT id FROM records WHERE sheet=? AND missing=0 AND observed=0 AND row_num>? AND row_num<2000000000 ORDER BY row_num LIMIT 1')
      .get(change.sheet, moduleMap.get(change.sheet)?.headerRow ?? 1);
    return next ? store.getRecord(next.id) : null;
  }

  /** What the formula cells of a proposal row will give ({ gives, fallback }, server/formula-gives.mjs). */
  function rowFormulaGives(change, target, session) {
    if (!target?.formulas || !Object.keys(target.formulas).length) return { gives: {}, fallback: [] };
    // A new row writes into its pre-made row: its ID formula stays, the rest is the proposal's.
    const formulas =
      change.create && change.sheet === 'Insectary_data'
        ? Object.fromEntries(Object.entries(target.formulas).filter(([f]) => f !== 'Insectary_ID'))
        : target.formulas;
    const values = Object.fromEntries(Object.entries(change.values ?? {}).filter(([f]) => !(change.create && f === 'Insectary_ID')));
    try {
      return formulaReader.rowGives({ sheet: change.sheet, record: { ...target, formulas }, values, all: !!change.create, ctx: session });
    } catch (e) {
      console.error('Formula gives:', e.message);
      return { gives: {}, fallback: [] };
    }
  }

  /**
   * A proposal as the review table shows it: each row with the current values of
   * the changed columns, and of the columns its sheet's table always shows
   * (`shownColumns`, see reviewColumns in server/notebook.mjs). New rows have no
   * current values (and, once written, their row).
   * `row` is its ai_proposals row: its rows as last revised, revision, status.
   * A pending one also gives what the table needs to edit it: every current value
   * of the edited rows (columns can be added), their formula columns, and those
   * of the pre-made rows new rows go into.
   *
   * Its rows go in the sheet's order (inSheetOrder). A notebook page's proposal
   * shows the whole page: the lines with nothing to write as grey context rows
   * (never written), lines not found or crossed out as a placeholder with the
   * line as written, a line the save would refuse with its reason; each row
   * with its photo and line. Rows added later take the line of the same ID.
   *
   * Sent lean (slow connections): an edited row's values once (rowValues while
   * pending, `current` after), a sheet's formula columns once (`sheetFormulas`;
   * a row lists its own only when they differ), the cells' hints once
   * (`hintTable`; a row gives their index).
   */
  function proposalView(proposal, row) {
    const changes = (row?.changes_json && parse(row.changes_json)) || proposal.changes;
    const status = row?.status ?? proposal.status;
    const open = status === 'pending';
    // A butterfly the row would leave preserved without CAM or tube: those cells are marked (and shown) until filled.
    const warned = changes.map(c => (open ? proposalSampleWarnings(c, c.create ? null : store.getRecord(c.recordId)) : null));
    const fields = [
      ...new Set(
        changes.flatMap((c, i) => [
          ...Object.keys(c.values),
          ...Object.keys(c.personEdits ?? {}),
          ...Object.keys(c.unreadable ?? {}),
          ...Object.keys(warned[i] ?? {}),
        ]),
      ),
    ];
    const created = parse(row?.created_json ?? 'null') ?? {};
    const locked = (sheet, keys) => keys.filter(f => !TYPED_OVER_FORMULA[sheet]?.has(f) && !isSumField(sheet, f));
    const page = parse(row?.page_json ?? 'null');
    const view = parse(row?.view_json ?? 'null');
    // In the sheet's order, a notebook page's lines too (its photos come in any order); the rows between them for context.
    const paged = page?.lines?.length ? page : null;
    const { rows, outOfOrder } = inSheetOrder(paged ? pageRows(changes, page) : changes.map((change, index) => ({ change, index, line: null })), {
      created,
      open,
      page: paged,
      view,
    });
    // A notebook page's proposal (its page, or a reason "Cuaderno Emergidos (Insectary_data): …"): the table
    // shows the notebook's columns first, in the order it writes them.
    const reason = row?.reason ?? proposal.reason ?? '';
    const kindId = page?.lines?.length
      ? page.kind
      : Object.keys(KINDS).find(k => reason.startsWith(`Cuaderno ${KINDS[k].label} (${KINDS[k].sheet})`));
    // Photos also on a proposal made before pages were kept (given later with update_proposal).
    const photoCount = (page?.photos ?? []).length;
    const notebook =
      (kindId && (KINDS[kindId] || page?.lines?.length)) || photoCount
        ? {
            kind: kindId ?? '',
            sheet: page?.lines?.length ? page.sheet : (KINDS[kindId]?.sheet ?? changes[0]?.sheet ?? null),
            columns: KINDS[kindId]?.fields ?? [],
            keys: KINDS[kindId]?.keys ?? [],
            photos: photoCount,
          }
        : null;
    const sheets = [...new Set(rows.map(r => r.change.sheet))];
    // The columns each sheet's table shows whatever the proposal changes (a notebook's, the rest up to its
    // notes), or those the assistant's view names first (or only).
    const shownColumns = Object.fromEntries(
      sheets.map(s => {
        const mod = moduleMap.get(s);
        const kind = notebook?.sheet === s ? notebook.kind : null;
        const hidden = [...(HIDDEN_COLUMNS[s] ?? [])];
        const shown = viewColumns(s, reviewColumns(s, mod?.fields.map(f => f.key) ?? [], mod?.identityFields ?? [], kind), view);
        return [s, hidden.length ? { ...shown, hidden } : shown];
      }),
    );
    const typeOf = f =>
      sheets.map(s => moduleMap.get(s)?.fields.find(x => x.key === f)?.type).find(Boolean) ?? 'text';
    const newRowFormulas = open
      ? Object.fromEntries(
          sheets.filter(s => changes.some(c => c.create && c.sheet === s)).map(s => [s, locked(s, [...createFormulaFields(s)])]),
        )
      : {};
    // What the row's formula cells will give once its values are written (shown, never written).
    const formulaSession = formulaReader.session();
    const hintTable = [];
    const hintIndex = new Map();
    const hintOf = h => {
      const lean = h?.msg ? { msg: h.msg } : { text: clip(h?.text, 300) };
      const key = json(lean);
      if (!hintIndex.has(key)) hintIndex.set(key, hintTable.push(lean) - 1);
      return hintIndex.get(key);
    };
    // The rows in use holding the Insectary IDs of its new rows (taken meanwhile), read once.
    const inUse = open ? takenRows(store, changes) : new Map();
    /** The version of each row's sheet row as read now (the list's stamp of the sheet). */
    const versions = [];
    const out = rows.map(({ change, index, line, outOfOrder: after }) => {
      const recordId = change.create ? (created[change.clientId] ?? null) : change.recordId;
      const record = recordId ? store.getRecord(recordId) : null;
      versions.push(record?.version ?? null);
      // Not needed by the table: what the sheet had when it was drafted, the version the save checks.
      const { before, expectedVersion, hints, formulaGives, sheetEdits, ...rest } = change;
      const view = {
        ...rest,
        key: change.key ?? rowKey(change),
        recordId,
        index,
        row: record?.row ?? change.row,
        label: change.label || record?.label || '',
        ...(index >= 0 && warned[index] ? { warnings: warned[index] } : {}),
      };
      if (hints && Object.keys(hints).length) view.hints = Object.fromEntries(Object.entries(hints).map(([f, h]) => [f, hintOf(h)]));
      if (line) view.page = pageLine(line, index < 0 || !!change.context);
      // Before the previous line of its photo in the sheet: the notebook goes the other way here.
      if (after) view.outOfOrder = after;
      if (change.placeholder) return view;
      // Cells edited in the sheet since they were read (or the pre-made row taken): told apart in the table.
      if (open && !change.context) Object.assign(view, sheetState(change, row?.created_at, inUse));
      // The rest of the row, for columns the person adds to the table (and its formula columns).
      if (open && !change.create) {
        view.rowValues = Object.fromEntries(
          Object.keys(record?.values ?? {})
            .map(f => [f, shownValue(record, f)])
            .filter(([, v]) => v !== null && v !== ''),
        );
        view.formulas = locked(change.sheet, Object.keys(record?.formulas ?? {}));
      } else if (!change.create) {
        // The changed columns, and the non-empty ones of those the table always shows.
        const shown = (shownColumns[change.sheet]?.fields ?? []).filter(f => !fields.includes(f));
        view.current = Object.fromEntries([
          ...fields.map(f => [f, shownValue(record, f)]),
          ...shown.map(f => [f, shownValue(record, f)]).filter(([, v]) => v !== null && v !== ''),
        ]);
      }
      // The row's formula cells its values reach, with what they will give (a new row: all of its pre-made
      // row's); beside a species typed over its formula too, so the table can tell when the person types the
      // formula's own. Those that cannot be evaluated keep the sheet's value, marked (formulaFallback).
      if (!change.context && !change.gap) {
        const target = change.create ? premadeRecordOf(change) : record;
        const { gives, fallback } = rowFormulaGives(change, target, formulaSession);
        const shown = Object.entries(gives).filter(([f, v]) =>
          f in change.values
            ? TYPED_OVER_FORMULA[change.sheet]?.has(f) && !isNone(v)
            : change.create
              ? v !== null && v !== ''
              : !sameResult(v, shownValue(record, f)),
        );
        if (shown.length) view.formulaGives = Object.fromEntries(shown);
        if (fallback.length) view.formulaFallback = fallback;
      }
      return view;
    });
    // A sheet's formula columns once: a row lists its own only when they differ.
    const sheetFormulas = {};
    for (const sheet of sheets) {
      const counts = new Map();
      for (const c of out) if (c.sheet === sheet && c.formulas) counts.set(json(c.formulas), (counts.get(json(c.formulas)) ?? 0) + 1);
      const common = [...counts].sort((a, b) => b[1] - a[1])[0]?.[0];
      if (common) sheetFormulas[sheet] = parse(common);
    }
    for (const c of out) if (c.formulas && json(c.formulas) === json(sheetFormulas[c.sheet] ?? null)) delete c.formulas;
    // The sheet's rows as the table shows them (the list keeps a proposal whose revision and rows are as they were).
    const sheetStamp = open
      ? createHash('sha1')
          .update(json(out.map((c, i) => [versions[i], c.row ?? null, c.sheetChanged ?? null, c.rowTaken ?? null])))
          .digest('base64url')
          .slice(0, 12)
      : undefined;
    return {
      ...proposal,
      status,
      ...(sheetStamp ? { sheetStamp } : {}),
      revision: row?.revision ?? 1,
      updatedAt: row?.updated_at ?? null,
      lastBy: row?.last_by ?? null,
      appliedAt: row?.applied_at ?? null,
      applied: parse(row?.applied_json ?? 'null'),
      sheets,
      fields,
      types: Object.fromEntries(fields.map(f => [f, typeOf(f)])),
      newRowFormulas,
      shownColumns,
      ...(Object.keys(sheetFormulas).length ? { sheetFormulas } : {}),
      ...(hintTable.length ? { hintTable } : {}),
      ...(notebook ? { page: notebook } : {}),
      ...(outOfOrder.length ? { outOfOrder: outOfOrder.map(({ key, ...o }) => o) } : {}),
      changes: out,
    };
  }

  /**
   * A proposal's rows ({ change, index, line }, from pageRows for a notebook
   * page) as the sheet has them, so the person reads the table beside the sheet
   * and from its oldest rows to the newest, whatever order a page's photos came
   * in: by sheet (a page's first), then by row number; a new Insectary_data row
   * where it will be written (the pre-made row of its ID, or below its ID's rows
   * for a suffixed one), as a page line not found; rows without a place after
   * them (a page's in ID order). While pending, the sheet's rows between two of
   * them that it leaves alone come in as context rows (`gap`: never written, not
   * editable; index < 0, as a page line without a row), as the view says (by
   * default when they are few, and not on a notebook page: see proposal-view.mjs).
   */
  function inSheetOrder(rows, { created, open, page = null, view = null }) {
    // Where each row stands: its sheet row; a new Insectary_data row (or a page line not found), where its ID goes.
    const idOf = c => String((c.create ? c.values?.Insectary_ID : c.label) ?? '').trim().toUpperCase();
    const unwritten = c => c.sheet === 'Insectary_data' && (c.create ? !created[c.clientId] : c.placeholder);
    const places = insectaryIdPlaces(store, rows.filter(r => unwritten(r.change)).map(r => idOf(r.change)));
    const placeOf = c => {
      const recordId = c.create ? (created[c.clientId] ?? null) : c.recordId;
      // A row no longer in the sheet (deleted, or the sheet read again without it) has no place: after the others.
      const record = recordId ? store.getRecord(recordId) : null;
      if (record && (record.missing || !(record.row > 0))) return null;
      const row = record?.row ?? (c.create || c.placeholder ? null : c.row);
      if (row != null) return row;
      const place = unwritten(c) ? places.get(idOf(c)) : null;
      return place ? place.row + (place.below ? 0.5 : 0) : null;
    };
    const placed = rows.map((r, order) => ({ ...r, order, at: placeOf(r.change) }));
    const byPlace = (a, b) =>
      (a.at ?? 0) - (b.at ?? 0) || (a.line?.photo ?? 0) - (b.line?.photo ?? 0) || (a.line?.n ?? 0) - (b.line?.n ?? 0) || a.order - b.order;
    let sorted;
    if (page) {
      // A page: by sheet (its own first), then by row; the rows without a place after them, in ID order; the
      // rows off the page with the same slip as a line (sameErrorAs, a table of their own) last.
      const sheets = [...new Set([page.sheet, ...rows.map(r => r.change.sheet)])];
      const ids = new Intl.Collator('en', { numeric: true, sensitivity: 'base' });
      const near = r => (r.change.sameErrorAs !== undefined ? 1 : 0);
      sorted = [...placed].sort(
        (a, b) =>
          near(a) - near(b) ||
          sheets.indexOf(a.change.sheet) - sheets.indexOf(b.change.sheet) ||
          (a.at == null) - (b.at == null) ||
          (a.at == null ? ids.compare(String(a.change.label ?? ''), String(b.change.label ?? '')) : 0) ||
          byPlace(a, b),
      );
    } else {
      // Any other proposal: its rows with a place take, sheet by sheet, the places such rows have in its list,
      // by row number; the others (new rows without one) stay where they are.
      const queues = new Map();
      for (const r of placed) if (r.at != null) queues.set(r.change.sheet, [...(queues.get(r.change.sheet) ?? []), r]);
      for (const list of queues.values()) list.sort(byPlace);
      sorted = placed.map(r => (r.at == null ? r : queues.get(r.change.sheet).shift()));
    }
    // The sheet's rows between two of them with a place in the same sheet (before the second), counted first.
    const last = new Map();
    const pairs = sorted.flatMap((r, i) => {
      if (r.at == null) return [];
      const prev = last.get(r.change.sheet);
      last.set(r.change.sheet, r);
      return prev ? [[i, Math.floor(prev.at) + 1, Math.ceil(r.at) - 1]] : [];
    });
    const between = pairs.reduce((n, [, from, to]) => n + Math.max(0, to - from + 1), 0);
    const writes = sorted.filter(r => r.index >= 0 && !r.change.context).length;
    const outOfOrder = page ? pageOrder(placed, page) : [];
    const out = sorted.map(({ at, order, ...r }) => r);
    if (!open || !between || !showBetween(view, { paged: !!page, between, rows: writes })) return { rows: out, outOfOrder };
    const shown = new Set(out.map(r => r.change.recordId).filter(Boolean));
    const at = new Map(pairs.map(([i, from, to]) => [i, [from, to]]));
    let room = BETWEEN_ROWS;
    let next = Math.min(0, ...out.map(r => r.index));
    const withGaps = out.flatMap((r, i) => {
      const gap = at.get(i);
      if (!gap || gap[1] - gap[0] + 1 > room) return [r];
      const rows = [];
      for (let n = gap[0]; n <= gap[1]; n++) {
        const record = store.getRecordBySheetRow(r.change.sheet, n);
        if (!record || record.missing || shown.has(record.id)) continue;
        room--;
        rows.push({
          index: --next,
          line: null,
          change: {
            key: `gap:${record.id}`,
            context: true,
            gap: true,
            recordId: record.id,
            sheet: r.change.sheet,
            row: record.row,
            label: record.label || '',
            values: {},
            note: '',
          },
        });
      }
      return [...rows, r];
    });
    return { rows: withGaps, outOfOrder };
  }

  /** A notebook page's proposal: its lines whose sheet rows go another way than the page (pageOrder), for the tools. */
  function orderDiffers(proposal) {
    const page = parse(proposal?.page_json ?? 'null');
    if (!page?.lines?.length) return {};
    const created = parse(proposal.created_json ?? 'null') ?? {};
    const { outOfOrder } = inSheetOrder(pageRows(parse(proposal.changes_json) ?? [], page), { created, open: false, page });
    return outOfOrder.length
      ? {
          orderDiffers: outOfOrder.map(({ key, ...o }) => o),
          orderNote:
            "Within a photo, each of these lines comes after `after` on the page but before it in the sheet (the table marks them): an ID may be misread, or the page was written out of order. Check them and tell the person.",
        }
      : {};
  }

  /**
   * The page's lines whose sheet rows go another way than the notebook: within
   * each photo (the photos come in any order), walking its lines top to bottom,
   * a line whose row (a new row's, where it will be written) comes before the
   * previous line's. Lines without a sheet row (not found, crossed out) are
   * skipped. Each as { photo, line, id, key, after: { line, id } }; the row is
   * marked too (`outOfOrder` on its { change, index, line }).
   */
  function pageOrder(placed, page) {
    const lines = placed
      .filter(r => r.line && r.at != null && !r.change.placeholder && r.change.sheet === page.sheet && r.change.sameErrorAs === undefined)
      .sort((a, b) => (a.line.photo ?? 0) - (b.line.photo ?? 0) || a.line.n - b.line.n);
    const out = [];
    for (const [i, r] of lines.entries()) {
      const prev = lines[i - 1];
      if (!prev || (prev.line.photo ?? 0) !== (r.line.photo ?? 0) || r.at >= prev.at) continue;
      r.outOfOrder = { line: prev.line.n, id: prev.change.label || prev.line.id || '' };
      out.push({ photo: r.line.photo ?? 0, line: r.line.n, id: r.change.label || r.line.id || '', key: r.change.key ?? rowKey(r.change), after: r.outOfOrder });
    }
    return out;
  }

  /**
   * A proposal's rows with the page they were read from: { change, index, line },
   * plus a row for each line without one (index < 0): the sheet's row as it is
   * (context), or the line as written (placeholder). inSheetOrder sorts them.
   */
  function pageRows(changes, page) {
    const rows = changes.map((change, index) => ({ change, index, line: null }));
    if (!page?.lines?.length) return rows;
    const idKey = v => String(v ?? '').replace(/\s+/g, '').toUpperCase();
    const lineOf = new Map(page.lines.map(l => [l.n, l]));
    const byRecord = new Map(page.lines.filter(l => l.recordId).map(l => [l.recordId, l]));
    const byId = new Map(page.lines.filter(l => l.id).map(l => [idKey(l.id), l]));
    for (const r of rows) {
      const c = r.change;
      r.line =
        (c.sheet === page.sheet && Number.isInteger(c.line) ? lineOf.get(c.line) : null) ??
        (c.recordId ? byRecord.get(c.recordId) : null) ??
        (c.label ? byId.get(idKey(c.label)) : null) ??
        null;
    }
    const covered = new Set(rows.filter(r => r.line && r.change.sheet === page.sheet).map(r => r.line.n));
    const keys = new Set(changes.map(rowKey));
    let synthetic = 0;
    for (const l of page.lines) {
      if (covered.has(l.n)) continue;
      const record = l.recordId && !keys.has(l.recordId) ? store.getRecord(l.recordId) : null;
      const live = record && !record.missing && record.sheet === page.sheet;
      if (live) keys.add(record.id);
      rows.push({
        index: -++synthetic,
        line: l,
        change: live
          ? { context: true, recordId: record.id, sheet: record.sheet, row: record.row, label: record.label, values: {}, note: '', line: l.n }
          : {
              context: true,
              placeholder: true,
              key: `line:${l.photo ?? 0}:${l.n}`,
              recordId: null,
              sheet: page.sheet,
              row: null,
              label: l.id ?? '',
              values: {},
              note: '',
              line: l.n,
            },
      });
    }
    return rows;
  }

  /**
   * The rows of a proposal plus the page lines shown as the sheet has them that
   * the person typed in (`keys`): such a line becomes a row of the proposal
   * (context until it has a value), as a context row from match_notebook.
   */
  function withPageRows(changes, page, keys) {
    if (!page?.lines?.length) return changes;
    const known = new Set(changes.map(rowKey));
    const out = [...changes];
    for (const key of new Set(keys)) {
      if (known.has(key)) continue;
      const line = page.lines.find(l => l.recordId === key);
      const record = line && store.getRecord(key);
      if (!record || record.missing || record.sheet !== page.sheet) continue;
      known.add(key);
      out.push({
        context: true,
        recordId: record.id,
        sheet: record.sheet,
        row: record.row,
        label: record.label,
        expectedVersion: record.version,
        before: {},
        values: {},
        replaceFormula: [],
        note: clip(`Línea ${line.n}: «${line.raw}»`, 300),
        line: line.n,
      });
    }
    return out;
  }

  /** Where a row is on the page; `alone`: a line without a row of its own (as written, with its state). */
  function pageLine(line, alone) {
    return {
      photo: line.photo ?? 0,
      line: line.n,
      ...(alone
        ? {
            raw: line.raw,
            status: line.status,
            ...(line.error ? { error: line.error } : {}),
            ...(line.message ? { message: line.message } : {}),
            ...(line.near ? { near: line.near } : {}),
          }
        : {}),
    };
  }

  /*
   * Live list of proposals for the Asistente tab: a revision per person that
   * changes whenever one of their proposals is added, applied or discarded, and
   * requests that wait (long polling) until it changes. A change made by a
   * page's own edit in the table (`page`, sent by lib/api.ts) does not wake that
   * page: the edit's answer brought it the proposal already.
   */
  const boot = randomUUID().slice(0, 8);
  const revisions = new Map();
  const waiters = new Map();
  /** Per person, the page whose own edit each recent revision was (null: anyone else's change). */
  const editors = new Map();
  /** A page's revision names whose list it is: a page that moves to another person's chat starts again. */
  const tagOf = ownerId => createHash('sha1').update(String(ownerId)).digest('base64url').slice(0, 6);
  const revisionOf = ownerId => `${boot}.${tagOf(ownerId)}.${revisions.get(ownerId) ?? 0}`;
  function changed(ownerId, page = null) {
    const n = (revisions.get(ownerId) ?? 0) + 1;
    revisions.set(ownerId, n);
    const by = editors.get(ownerId) ?? editors.set(ownerId, new Map()).get(ownerId);
    by.set(n, page);
    if (by.size > 50) by.delete(by.keys().next().value);
    for (const wake of [...(waiters.get(ownerId) ?? [])]) if (!page || wake.page !== page) wake();
  }
  // A row of a pending proposal (or of a table shown) saved to the local copy (edited in the sheet and read by a
  // sync or the sheet hook, or saved from the app): its owner's list changes, so the table shows the sheet's edits at once.
  store.watchRecords?.(rows => {
    const ids = new Set(rows.map(r => r.id));
    const insectary = rows.some(r => r.sheet === 'Insectary_data');
    let pending = [];
    try {
      pending = db.prepare("SELECT owner_id, changes_json, table_json FROM ai_proposals WHERE status IN ('pending', 'shown')").all();
    } catch {
      return; // the database is closing
    }
    const owners = new Set();
    for (const p of pending) {
      if (owners.has(p.owner_id)) continue;
      // A table shown (show_rows): one of its rows.
      if (p.table_json) {
        if ((parse(p.table_json)?.rows ?? []).some(id => ids.has(id))) owners.add(p.owner_id);
        continue;
      }
      const touches = c => (c.recordId && ids.has(c.recordId)) || (insectary && c.create && c.sheet === 'Insectary_data' && !!c.values?.Insectary_ID);
      if ((parse(p.changes_json) ?? []).some(touches)) owners.add(p.owner_id);
    }
    for (const ownerId of owners) changed(ownerId);
  });
  /** Whether a page holding revision `seen` has the list as it is: nothing changed since but its own edits. */
  function caughtUp(ownerId, seen, page) {
    if (seen === revisionOf(ownerId)) return true;
    const prefix = `${boot}.${tagOf(ownerId)}.`;
    const [from, to] = [Number(String(seen).slice(prefix.length)), revisions.get(ownerId) ?? 0];
    const by = editors.get(ownerId);
    if (!page || !by || !String(seen).startsWith(prefix) || !Number.isInteger(from) || from > to || to - from > 50) return false;
    for (let n = from + 1; n <= to; n++) if (by.get(n) !== page) return false;
    return true;
  }
  /** Waits for a change of the person's proposals, or until `moved()` says the chat to show changed (checked every 2 s). */
  function waitForChange(ownerId, seen, ms, moved = null, page = null) {
    if (!caughtUp(ownerId, seen, page)) return Promise.resolve();
    return new Promise(resolve => {
      const list = waiters.get(ownerId) ?? waiters.set(ownerId, new Set()).get(ownerId);
      const wake = () => {
        clearTimeout(timer);
        clearInterval(watch);
        list.delete(wake);
        if (!list.size && waiters.get(ownerId) === list) waiters.delete(ownerId);
        resolve();
      };
      wake.page = page;
      const timer = setTimeout(wake, ms);
      const watch = moved ? setInterval(() => moved() && wake(), 2000) : undefined;
      list.add(wake);
    });
  }

  // ------------------------------------------------------------ proposals by T3 chat
  const ago = (iso, ms) => new Date(Date.parse(iso) - ms).toISOString();
  /** The T3 chat of a tool call, if T3 has recorded the call already (by its tool-use id; Codex: the only chat answering). */
  function chatOfCall(context) {
    if (!t3) return null;
    const { toolUseId } = context.t3;
    const id = toolUseId ? t3.threadOfToolUse(toolUseId, ago(now(), 10 * 60_000)) : t3.onlyRunning(context.user.username);
    return id ? { id, title: t3.threads([id]).get(id)?.title ?? null } : null;
  }
  /*
   * Proposals from T3 Code whose chat is not known yet (T3 had not recorded the
   * call, or they were made before chats were recorded) are linked when the
   * list is asked for: by tool-use id at once (before the answer), and by the
   * chats' tool results naming the proposal at most every 5 s per person,
   * without holding the answer up. Not found 10 minutes after it was made:
   * left as a proposal outside the chats.
   */
  const unlinked = (ownerId, withToolUse = false) =>
    db
      .prepare(
        `SELECT p.id, p.created_at, p.t3_tool_use FROM ai_proposals p JOIN ai_threads t ON t.id = p.thread_id
         WHERE p.owner_id = ? AND p.t3_thread IS NULL AND t.title = 'T3 Code'${withToolUse ? ' AND p.t3_tool_use IS NOT NULL' : ''}`,
      )
      .all(ownerId);
  function link(ownerId, found) {
    if (!found.size) return;
    const titles = t3.threads([...found.values()]);
    const set = db.prepare('UPDATE ai_proposals SET t3_thread = ?, t3_title = ? WHERE id = ? AND t3_thread IS NULL');
    for (const [id, thread] of found) set.run(thread, titles.get(thread)?.title ?? null, id);
    changed(ownerId);
  }
  function linkByToolUse(ownerId) {
    if (!t3) return;
    const found = new Map();
    for (const r of unlinked(ownerId, true)) {
      const id = t3.threadOfToolUse(r.t3_tool_use, ago(r.created_at, 10 * 60_000));
      if (id) found.set(r.id, id);
    }
    link(ownerId, found);
  }
  const scannedAt = new Map();
  async function linkByResult(ownerId) {
    if (!t3 || Date.now() - (scannedAt.get(ownerId) ?? 0) < 5000) return;
    scannedAt.set(ownerId, Date.now());
    const rows = unlinked(ownerId);
    if (!rows.length || !t3.available) return;
    const since = ago(rows.map(r => r.created_at).sort()[0], 10 * 60_000);
    link(ownerId, await t3.findProposals(rows.map(r => r.id), since));
    const none = db.prepare("UPDATE ai_proposals SET t3_thread = '' WHERE id = ? AND t3_thread IS NULL");
    for (const r of rows) if (Date.now() - Date.parse(r.created_at) > 10 * 60_000) none.run(r.id);
  }

  /**
   * The chat whose proposals the panel shows when it follows T3 ("auto"): the
   * one open in T3 now (the page's T3 frame says it, `seen`: a thread id,
   * 'draft' for a new chat or 'none' for no chat on screen; else guessed, see
   * t3chats.mjs), else the one most recently active (a message in it, or one
   * of its proposals changed; 'app' = the proposals made outside T3 chats),
   * else all of them (no T3 chats at all).
   */
  function followed(user, groups, seen = null) {
    if (seen && seen !== 'none') return { chat: seen, how: 'open' };
    const open = seen ? null : (t3?.open(user.username) ?? null);
    if (open) return { chat: open, how: 'open' };
    const latest = t3?.chatsOf(user.username, 1)[0];
    let best = latest ? { chat: latest.id, at: latest.lastUserAt ?? '' } : null;
    for (const g of groups.values()) if (!best || g.at > best.at) best = { chat: g.id, at: g.at };
    if (!best || (best.chat === 'app' && !latest && groups.size === 1)) return { chat: 'all', how: 'all' };
    return { chat: best.chat, how: 'recent' };
  }
  /** The chats with pending proposals or tables shown (the panel's chat selector), most recently changed first. */
  function chatGroups(ownerId) {
    const rows = db
      .prepare(
        "SELECT t3_thread, t3_title, status, coalesce(updated_at, created_at) at FROM ai_proposals WHERE owner_id = ? AND status IN ('pending', 'applying', 'shown')",
      )
      .all(ownerId);
    const groups = new Map();
    for (const r of rows) {
      const id = r.t3_thread || 'app';
      const g = groups.get(id) ?? groups.set(id, { id, title: r.t3_title ?? null, pending: 0, at: '' }).get(id);
      if (r.status !== 'shown') g.pending++;
      if (r.at > g.at) g.at = r.at;
    }
    return groups;
  }

  /** Cells of a proposal edited in the sheet, as apply_proposal reports them (a row whose pre-made row was taken: rowTaken). */
  const sheetList = cells =>
    cells.slice(0, 60).map(c => ({
      index: c.index,
      label: c.label,
      ...(c.row ? { row: c.row } : {}),
      ...(c.field ? { field: c.field, read: c.read, now: c.now } : { rowTaken: c.rowTaken?.row ?? true }),
    }));

  /** Unreadable cells still empty, as apply_proposal reports them (the person is asked for their values). */
  const unreadableList = cells =>
    cells.slice(0, 60).map(u => ({
      index: u.index,
      label: u.label,
      ...(u.row ? { row: u.row } : {}),
      field: u.field,
      ...(u.reason ? { reason: u.reason } : {}),
      ...(u.partial?.length ? { partial: u.partial } : {}),
    }));

  /** query: one read-only statement on the sheets' copy, its rows as text (server/query-tool.mjs). */
  async function queryCopy(args) {
    if (!queries) return { error: 'There is no copy of the sheets on this server: use find_records and count_records.' };
    const sql = String(args.sql ?? '');
    const problem = sqlProblem(sql);
    if (problem) return { error: problem, hint: QUERY_HINT };
    let out = await queries.run(sql, args.limit);
    if (out.missing) {
      // The first copy is still being made (a few seconds).
      await sheetsCopy.rebuild();
      out = await queries.run(sql, args.limit);
    }
    if (out.timeout)
      return { error: out.error, hint: 'Narrow it: filter on an ID column (they are indexed) or a sheet, join fewer tables, or count instead of listing.' };
    if (out.error) return { error: `SQLite: ${out.error}`, ...(out.near ? { near: out.near } : {}), hint: QUERY_HINT };
    return formatRows(out, { limit: args.limit, pending: sheetsCopy.status().pending });
  }

  async function executeTool(name, args, context) {
    if (name === 'query') return queryCopy(args);
    if (name === 'search_records') {
      const query = clip(args.query, 100).trim();
      if (!query) return { error: 'Search query required' };
      const page = await store.searchRecords({
        module: clip(args.module, 100) || undefined,
        q: query,
        limit: 12,
        offset: 0,
      });
      // Within find_records' size budget (a Collection_data row with its lookups is a few kB).
      const records = [];
      let size = 0;
      for (const record of page.records ?? []) {
        const row = compact(record);
        size += json(row).length + 1;
        if (size > FIND_BUDGET && records.length) break;
        records.push(row);
        context.records.set(record.id, record);
        context.sources.set(record.id, recordSource(record));
      }
      const cut = (page.records?.length ?? 0) - records.length;
      return {
        records,
        total: page.total,
        ...(cut ? { truncated: true, next: `${cut} more rows not shown (size limit): use find_records with filters and fields.` } : {}),
      };
    }
    if (name === 'find_records') return findRows(args, context);
    if (name === 'count_records') return countRecords(db, args);
    if (name === 'describe_sheet') return describeSheet(args);
    if (name === 'get_record') {
      // Its app recordId, or its ID in the sheet (W2B, a clutch number), in `sheet` when given.
      const named = resolveRows(store, [args.key !== undefined ? { sheet: args.sheet, key: args.key } : clip(args.id ?? args.recordId, 120)], {
        sheet: args.sheet ? clip(args.sheet, 100) : null,
      });
      if (named.problems.length) return { error: named.problems[0].error };
      const record = store.getRecord(named.ids[0]);
      if (!record) return { error: 'Record not found' };
      context.records.set(record.id, record);
      context.sources.set(record.id, recordSource(record));
      return compact(record, { allFormulas: true });
    }
    if (['search_knowledge', 'read_document', 'list_documents', 'sync_documents'].includes(name))
      return runKnowledgeTool(knowledge, name, args, context);
    if (name === 'run_report') {
      const response = await reports.build({ kind: args.kind, module: args.module, field: args.field });
      if (response.status !== 200) return response.body;
      for (const item of response.body.sources) context.sources.set(item.id, { ...item, type: 'record' });
      // The chart's series repeats the rows: the model reads the rows.
      const result = { ...response.body, series: undefined, sources: response.body.sources.slice(0, 30) };
      context.results.push(result);
      const more = kept =>
        kept < result.rows.length ? { truncated: true, next: `${result.rows.length - kept} more rows not shown: count_records with filters and groupBy gives them in parts` } : {};
      return fitList(result, 'rows', RESULT_BUDGET - 500, more).out;
    }
    if (name === 'check_data') {
      const out = checkData(store, {
        sheet: args.sheet ? clip(args.sheet, 100) : undefined,
        kind: args.kind ? clip(args.kind, 300) : undefined,
        recordId: args.recordId ? clip(args.recordId, 120) : undefined,
        limit: Math.min(Number(args.limit) || 50, 200),
        offset: args.offset,
      });
      // The kinds explained only in an answer that is not about some of them.
      const page = { ...out, ...(args.kind ? { kinds: undefined } : {}), issues: withoutMsgs(out.issues) };
      const more = kept =>
        kept < out.issues.length || out.offset + out.issues.length < out.total
          ? { truncated: true, next: `Issues from ${out.offset + kept} on not shown${kept < out.issues.length ? ' (size limit)' : ''}: check_data with offset: ${out.offset + kept}, or one kind or sheet` }
          : {};
      const fitted = fitList({ ...page, ...more(page.issues.length) }, 'issues', RESULT_BUDGET - 500, more);
      for (const issue of fitted.out.issues.filter(i => i.recordId))
        context.sources.set(issue.recordId, { id: issue.recordId, type: 'record', sheet: issue.sheet, row: issue.row, label: issue.label });
      return fitted.out;
    }
    if (name === 'queue_wikiloc') {
      if (!EDITORS.includes(context.user.role)) return { error: 'Your role cannot queue walks' };
      return queueWalk(store, { url: clip(args.url, 500), refresh: !!args.refresh }, context.user);
    }
    if (name === 'get_walk')
      return walkDraft(store, {
        walkId: args.walkId ? clip(args.walkId, 60) : undefined,
        url: args.url ? clip(args.url, 500) : undefined,
        date: args.date ? clip(args.date, 10) : undefined,
        collector: args.collector ? clip(args.collector, 120) : undefined,
      });
    if (name === 'propose_changes') return proposeChanges(args, context);
    if (name === 'update_proposal') return updateProposal(args, context);
    if (name === 'get_proposal') return getProposal(args, context);
    if (name === 'list_proposals') return listProposals(args, context);
    if (name === 'show_rows') return showRows(args, context);
    if (name === 'list_agreed_fixes') return agreedFixes(store, { kind: args.kind ? clip(args.kind, 300) : undefined, limit: args.limit });
    if (name === 'list_suggested_edits') {
      const out = await suggestionPage(store, {
        source: args.source ? clip(args.source, 300) : undefined,
        certainty: args.certainty ? clip(args.certainty, 100) : undefined,
        sheet: args.sheet ? clip(args.sheet, 100) : undefined,
        recordId: args.recordId ? clip(args.recordId, 120) : undefined,
        q: args.q ? clip(args.q, 100) : undefined,
        limit: Math.min(Number(args.limit) || 50, 200),
        offset: args.offset,
      });
      for (const s of out.items)
        context.sources.set(s.recordId, { id: s.recordId, type: 'record', sheet: s.sheet, row: s.row, label: s.label });
      return { ...withoutMsgs(out), certainties: CERTAINTIES };
    }
    if (name === 'get_alerts') return withoutMsgs(alerts(store));
    if (name === 'match_notebook') return matchNotebook(args, context);
    if (HISTORY_TOOL_NAMES.has(name)) return runHistoryTool(store, name, args, context, { publicUrl: config.publicUrl });
    if (name === 'apply_proposal') {
      const proposal = db
        .prepare('SELECT * FROM ai_proposals WHERE id = ? AND thread_id = ?')
        .get(String(args.proposalId ?? ''), context.threadId);
      if (!proposal) return { error: 'Proposal not found in this conversation' };
      if (isTable(proposal)) return { error: NOT_A_PROPOSAL };
      try {
        const out = await applyProposal(proposal, context.user, {
          requestId: `ai-${randomUUID()}`,
          indexes: args.indexes,
          reason: tpl('Confirmado en el chat'),
          doubtful: args.confirmDoubtful === true ? 'confirm' : args.skipDoubtful === true ? 'skip' : null,
        });
        context.applied.push(proposal.id);
        return {
          status: out.status,
          rows: out.applied.length,
          ...(out.keptFromSheet ? { keptFromSheet: sheetList(out.keptFromSheet) } : {}),
          ...(out.doubtful ? { doubtful: out.doubtful } : {}),
          ...(out.unreadable
            ? {
                unreadable: unreadableList(out.unreadable),
                unreadableNote:
                  'These cells could not be read and nobody filled them: they were left as the sheet has them (empty). Ask the person for their values; they go in a new proposal.',
              }
            : {}),
        };
      } catch (e) {
        if (e.code === 'sheet_changed_again')
          return {
            error: 'Not applied: cells edited in the sheet again after the person chose',
            cells: sheetList(e.details.again),
            todo: 'The person chooses again in the table (the sheet\'s value or yours), or you re-check them and update_proposal.',
          };
        if (e.code === 'nothing_selected' && e.details?.unreadable)
          return {
            error: 'Not applied: nothing to write yet, only unreadable cells still empty',
            unreadable: unreadableList(e.details.unreadable),
            todo: 'Ask the person for the values of these cells (they type them in the table, or tell you: update_proposal), then apply again.',
          };
        // Nothing was written: the person has to look at the doubtful cells first.
        if (e.code === 'doubtful_unchecked') {
          const changes = parse(proposal.changes_json) ?? [];
          const readable = (sheet, field, value) =>
            moduleMap.get(sheet)?.fields.find(f => f.key === field)?.type === 'date' && typeof value === 'number' ? isoDate(value) : value;
          return {
            error: 'Not applied: doubtful cells not checked yet',
            doubtful: e.details.doubtful.slice(0, 60).map(d => ({
              index: d.index,
              label: d.label,
              field: d.field,
              value: readable(d.sheet, d.field, d.value),
              alternatives: d.alternatives.map(a => readable(d.sheet, d.field, a)),
              ...(d.reason ? { reason: d.reason } : {}),
            })),
            count: e.details.doubtful.length,
            rows: changes.length,
            ...(e.details.unreadable?.length
              ? {
                  unreadable: unreadableList(e.details.unreadable),
                  unreadableNote: 'Cells nobody could read, still empty: ask the person for them too; applying leaves them as the sheet has them.',
                }
              : {}),
            todo: 'Ask the person about these cells (value, alternatives, why). They check them in the table (edit, pick an alternative, or «Marcar revisadas»), or tell you: then update_proposal (the value they say, or rows[].checked for the ones they confirm) and apply again. Only when they explicitly say to apply them as they are: apply_proposal with confirmDoubtful; to write only the sure cells: skipDoubtful.',
          };
        }
        // Google's own reason for a rejected save (a protected range, a bad request) helps fix it.
        return {
          error: clip(e.message, 300),
          details: e.details?.items?.slice(0, 10),
          ...(e.details?.cause ? { cause: clip(e.details.cause, 300) } : {}),
        };
      }
    }
    return { error: 'Unknown tool' };
  }

  function initialsFor(user) {
    const name = String(user.displayName || user.username || '').trim();
    const words = name.toLowerCase().split(/\s+/).filter(Boolean);
    let known = [];
    try {
      known = db
        .prepare(
          "SELECT DISTINCT json_extract(values_json, '$.Collector') c FROM records WHERE sheet = 'Collection_data' AND json_extract(values_json, '$.Collector') LIKE '% - %'",
        )
        .all()
        .map(r => String(r.c));
    } catch {
      /* No mirrored rows (tests). */
    }
    const match = known.find(c => words.length && words.every(w => c.toLowerCase().includes(w)));
    const letters = words.map(w => w.replace(/[^\p{L}]/gu, '')[0]).filter(Boolean);
    return match ? match.split(' - ')[0].trim() : letters.join('').toUpperCase() || 'APP';
  }

  /**
   * An agent outside the app (T3 Code) using a personal token: it acts as that
   * person, and its proposals go to their "T3 Code" conversation for review.
   */
  const agents = new Map();
  /** A person's conversation with a fixed title (T3 Code, Revisión de datos), created on first use. */
  function namedThread(user, title) {
    const found = db.prepare('SELECT id FROM ai_threads WHERE owner_id = ? AND title = ?').get(owner(user), title);
    if (found) return found.id;
    const id = randomUUID();
    db.prepare('INSERT INTO ai_threads (id,owner_id,title,created_at,updated_at) VALUES (?,?,?,?,?)').run(
      id,
      owner(user),
      title,
      now(),
      now(),
    );
    return id;
  }
  function agentTurn(token) {
    const hash = createHash('sha256').update(token).digest('hex');
    const row = db
      .prepare(
        'SELECT u.* FROM ai_tokens t JOIN users u ON u.id = t.user_id WHERE t.token_hash = ? AND t.revoked_at IS NULL AND u.active = 1',
      )
      .get(hash);
    if (!row) return null;
    const user = { id: row.id, username: row.username, displayName: row.display_name, role: row.role };
    const thread = { id: namedThread(user, 'T3 Code') };
    const cached = agents.get(hash);
    if (cached?.context.threadId === thread.id) return cached;
    const turn = {
      context: {
        threadId: thread.id,
        user,
        records: new Map(),
        sources: new Map(),
        results: [],
        proposals: [],
        applied: [],
      },
    };
    agents.set(hash, turn);
    return turn;
  }

  /** MCP (streamable HTTP, JSON replies) for T3 Code's chats, with the person's token (scripts/t3-provision.mjs). */
  async function mcp(headers, body) {
    const token = /^Bearer\s+(\S+)$/.exec(String(headers.authorization ?? ''))?.[1];
    const turn = token && agentTurn(token);
    const id = body?.id ?? null;
    if (!turn)
      return { status: 401, body: { jsonrpc: '2.0', id, error: { code: -32001, message: 'Unauthorized' } } };
    const result = value => ({ status: 200, body: { jsonrpc: '2.0', id, result: value } });
    const method = String(body?.method ?? '');
    if (method.startsWith('notifications/')) return { status: 202, body: null };
    if (method === 'initialize')
      return result({
        protocolVersion: body.params?.protocolVersion ?? '2025-06-18',
        capabilities: { tools: { listChanged: false } },
        serverInfo: { name: 'ithomiini', version: '1.0.0' },
      });
    if (method === 'ping') return result({});
    if (method === 'tools/list') return result({ tools: mcpTools() });
    if (method === 'tools/call') {
      let out;
      // A T3 Code chat's call: its own list of proposals (chats call at the same time), and Claude's
      // tool-use id, which T3 records with the call: the proposals it drafts are shown with that chat.
      const context = { ...turn.context, proposals: [], t3: { toolUseId: clip(body.params?._meta?.['claudecode/toolUseId'], 100) || null } };
      const before = context.proposals.length;
      try {
        out = await executeTool(String(body.params?.name ?? ''), body.params?.arguments ?? {}, context);
      } catch (e) {
        out = { error: clip(e.message, 300) };
      }
      // Proposals from T3 Code are shown in the app for review (Asistente → Cambios propuestos).
      if (context.proposals.length > before) {
        const fresh = context.proposals.splice(before);
        insertMessage(turn.context.threadId, 'assistant', 'Propuesta desde T3 Code', [], [], fresh);
        out = {
          ...out,
          review: 'The person reviews it in the app: Asistente → Cambios propuestos, or tells you to apply it.',
        };
      }
      // Within one answer's size (server/tool-budget.mjs): the tools cut their own lists where they
      // can say how to go on; anything still too long loses the end of its longest lists, and says so.
      out = fitResult(out, {
        narrow: HISTORY_TOOL_NAMES.has(String(body.params?.name ?? ''))
          ? 'narrow it with recordId, field(s), text or dates, or a smaller maxChanges or limit'
          : 'ask for less (filters, fewer columns, a smaller limit) or page with offset',
      });
      // `query` answers in text lines (its rows), the other tools in JSON.
      return result({ content: [{ type: 'text', text: typeof out === 'string' ? out : json(out) }], isError: Boolean(out?.error) });
    }
    return { status: 200, body: { jsonrpc: '2.0', id, error: { code: -32601, message: 'Method not found' } } };
  }

  const notebooks = createNotebookMatcher({ store, db, newIds: idsFor, draftChanges, initialsFor });

  /**
   * A transcribed notebook page matched with its sheet (match_notebook): one
   * proposal for the page, replacing the page's earlier one when Claude matches
   * it again after a correction.
   */
  function matchNotebook(args, context) {
    let matched;
    try {
      matched = notebooks.match(args, context.user);
    } catch (e) {
      return { error: clip(e.message, 300) };
    }
    const editor = EDITORS.includes(context.user.role);
    const replaced = args.replaceProposalId
      ? db
          .prepare("SELECT * FROM ai_proposals WHERE id = ? AND owner_id = ? AND status = 'pending'")
          .get(String(args.replaceProposalId), owner(context.user))
      : null;
    const { review } = matched;
    // The same rows in another pending proposal: the page matched again, maybe in another conversation.
    // Context rows (includeUnchanged) write nothing: they neither overlap nor make a proposal alone.
    const writes = matched.changes.some(c => !c.context);
    const rows = new Set(matched.changes.filter(c => !c.context).map(c => c.recordId).filter(Boolean));
    const overlaps = rows.size
      ? db
          .prepare("SELECT id, reason, changes_json FROM ai_proposals WHERE owner_id = ? AND status = 'pending' AND id != ?")
          .all(owner(context.user), replaced?.id ?? '')
          .map(p => ({
            id: p.id,
            reason: p.reason,
            rows: (parse(p.changes_json) ?? []).filter(c => !c.context && rows.has(c.recordId)).map(c => c.label),
          }))
          .filter(p => p.rows.length)
          .map(p => ({ proposalId: p.id, reason: p.reason, rows: p.rows.slice(0, 10), count: p.rows.length }))
      : [];
    const reason = `Cuaderno ${KINDS[review.kind].label} (${review.sheet})${args.title ? `: ${clip(args.title, 120)}` : ''}`;
    const sheets = [...new Set([review.sheet, ...matched.changes.map(c => c.sheet)])];
    const view = readView(args.view, sheets, replaced ? parse(replaced.view_json ?? 'null') : null);
    if (view?.error) return { error: `${view.error}. Nothing was proposed.` };
    let proposal = null;
    let conflicts = [];
    if (editor && replaced && writes) {
      // The corrected page takes the place of its proposal (same id): the table beside the chat changes in place.
      const carried = carryPersonEdits(parse(replaced.changes_json) ?? [], matched.changes, context.user);
      conflicts = carried.conflicts;
      db.prepare('UPDATE ai_proposals SET view_json = ? WHERE id = ?').run(view ? json(view) : null, replaced.id);
      if (saveRevision(replaced, carried.changes, 'ai', reason) !== null)
        proposal = { id: replaced.id, chat: chatOf(replaced, context) };
    } else if (editor && replaced) {
      db.prepare("UPDATE ai_proposals SET status = 'discarded' WHERE id = ? AND status = 'pending'").run(replaced.id);
      changed(owner(context.user));
    }
    if (editor && writes && !proposal) proposal = saveProposal(matched.changes, reason, context, [], view);
    // The page and its photos (attachments of this chat), kept with the proposal: its table follows
    // the whole page, beside the photo. A page matched again without photos keeps the ones it had.
    let refused = [];
    if (proposal) {
      const stored = db.prepare('SELECT t3_thread, page_json FROM ai_proposals WHERE id = ?').get(proposal.id);
      const chat = stored?.t3_thread || (context.t3 ? chatOfCall(context)?.id : null) || null;
      const given = photosOf(config.t3?.home, args, chat);
      refused = given.refused;
      const photos = args.photo ? given.photos : (parse(stored?.page_json ?? 'null')?.photos ?? []);
      db.prepare('UPDATE ai_proposals SET page_json = ? WHERE id = ?').run(json({ ...matched.page, photos }), proposal.id);
    }
    const order = proposal ? orderDiffers(db.prepare('SELECT * FROM ai_proposals WHERE id = ?').get(proposal.id)) : {};
    return {
      ...matchSummary(matched, proposal?.id),
      ...(proposal ? proposalLink(proposal.id, proposal.chat) : {}),
      ...(refused.length
        ? {
            photoNotShown: refused,
            photoNote: 'Give `photo` as the file name of this chat\'s attachment, from "[Attached image … saved at …]"',
          }
        : {}),
      ...(replaced ? { replaced: replaced.id } : {}),
      ...(conflicts.length
        ? {
            conflicts,
            conflictNote: 'Cells the person corrected by hand in the table were kept. Tell the person where your new reading differs.',
          }
        : {}),
      ...(overlaps.length ? { overlaps } : {}),
      ...order,
      ...(!editor ? { note: 'This person can only read the workbook: nothing was proposed' } : {}),
    };
  }

  let photoCopies = null;
  /**
   * GET /api/proposals/:id/photos/:n?size=thumb|view: a photo of a notebook
   * page's proposal, upright (`raw` bytes for index.mjs). For its owner and for
   * anyone who may edit proposals (a chat handed over), and only an attachment
   * of the T3 chat the proposal comes from.
   */
  async function proposalPhoto(id, n, user, query, headers = {}) {
    // Its owner, or anyone on the team who may edit proposals (a chat handed over to finish).
    const find = () => teamProposal(id, user);
    let proposal = find();
    // A proposal whose chat is not linked yet (T3 records the call a moment later).
    if (proposal && !proposal.t3_thread) {
      linkByToolUse(proposal.owner_id);
      proposal = find();
    }
    const photo = proposal ? parse(proposal.page_json ?? 'null')?.photos?.[n] : null;
    const file = photo && proposal.t3_thread ? attachmentFile(config.t3?.home, photo.file, proposal.t3_thread) : null;
    if (!file) return bad(404, 'not_found', 'Photo not found.');
    photoCopies ??= createPhotoCopies({
      dir: (() => {
        const base = config.photoCacheDir || photoCacheDir({}, db.location?.() ?? null);
        return base ? `${base}/proposals` : null;
      })(),
    });
    const copy = await photoCopies.get(file.path, photo.rotate ?? 0, PHOTO_SIZES[query.size] ?? PHOTO_SIZES.thumb);
    const cache = { etag: copy.etag, 'cache-control': 'private, max-age=86400' };
    if (headers['if-none-match'] === copy.etag) return { status: 304, raw: null, headers: cache };
    return {
      status: 200,
      raw: copy.data,
      headers: { ...cache, 'content-type': copy.mime, 'x-content-type-options': 'nosniff', 'content-security-policy': 'sandbox; default-src none' },
    };
  }

  async function handle({ method, path, body = {}, user, query = {}, page = null, headers = {} }) {
    if (!/^\/api\/(chat|reports|knowledge|ai|proposals)(?:\/|$)/.test(path)) return null;
    if (!user || !owner(user)) return bad(401, 'unauthorized', 'Sign in to use the assistant.');
    const photoMatch = /^\/api\/proposals\/([0-9a-f-]{36})\/photos\/(\d{1,2})$/.exec(path);
    if (photoMatch && method === 'GET') return proposalPhoto(photoMatch[1], Number(photoMatch[2]), user, query, headers);
    if (path === '/api/reports') return reports.handle({ method, path, query, user });

    if (path === '/api/knowledge' && method === 'GET') {
      const passages = await knowledge.search({ query: query.q, kind: query.kind, from: query.from, to: query.to, perDoc: 1 });
      return { status: 200, body: { documents: passages } };
    }
    const docMatch = /^\/api\/knowledge\/([A-Za-z0-9_-]{20,80})$/.exec(path);
    if (docMatch && method === 'GET') {
      const doc = await knowledge.get(docMatch[1]);
      return doc
        ? {
            status: 200,
            body: { id: doc.id, title: doc.title, kind: doc.kind, date: doc.date, text: doc.text, sourceUrl: doc.sourceUrl },
          }
        : bad(404, 'not_found', 'Document not found.');
    }
    if (path === '/api/chat/proposals' && method === 'GET') {
      const me = owner(user);
      // chat: 'all' (default), 'app' (made outside T3 chats), a T3 thread id, or 'auto': the chat
      // T3 shows (see followed()); follow = that chat as the page last got it. seen = the chat the
      // page's T3 frame shows (server/t3bridge.mjs): a thread id, 'draft' or 'none'.
      const asked = String(query.chat ?? 'all');
      const follow = String(query.follow ?? '') || null;
      const seen = /^(?:[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}|draft|none)$/.test(String(query.seen ?? ''))
        ? String(query.seen)
        : null;
      // wait=1 with the revision the page holds: answer when a proposal is added, applied or
      // discarded (or after 20 s), so the Asistente tab shows edits as the assistant drafts them;
      // with T3, also when another chat is opened there (a page whose frame says so asks again itself).
      const held = String(query.revision ?? '');
      // Another person's chat on screen (or asked for), or another person's proposal: its proposals, live.
      const UUID_RE = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/;
      const thread = query.only ? null : asked === 'auto' ? (UUID_RE.test(seen ?? '') ? seen : null) : UUID_RE.test(asked) ? asked : null;
      const team = EDITORS.includes(user.role);
      const of = !team ? me : query.only ? (anyProposal(query.only)?.owner_id ?? me) : thread ? (ownerOfChat(thread) ?? me) : me;
      if (query.wait)
        await waitForChange(
          of,
          held,
          config.proposalWaitMs ?? 20000,
          t3 && !query.only && !seen ? () => followed(user, chatGroups(me)).chat !== follow : null,
          page,
        );
      linkByToolUse(of);
      void linkByResult(of).catch(e => console.error('Proposals by chat:', e.message));
      const revision = revisionOf(of);
      const groups = chatGroups(me);
      const followNow = followed(user, groups, seen);
      const scope = query.only ? { chat: 'all', how: 'only' } : asked === 'auto' ? followNow : { chat: asked, how: 'chosen' };
      // One chat (or one proposal): whoever's it is; otherwise the person's own.
      const anyone = team && (!!query.only || (scope.chat !== 'all' && scope.chat !== 'app' && scope.chat !== 'draft'));
      const select = `SELECT p.*, t.title FROM ai_proposals p JOIN ai_threads t ON t.id = p.thread_id WHERE ${anyone ? '1 = 1' : 'p.owner_id = ?'}`;
      // A new chat in T3 (a draft): no proposals yet.
      const where = query.only
        ? ' AND p.id = ?'
        : scope.chat === 'all'
          ? ''
          : scope.chat === 'app'
            ? " AND coalesce(p.t3_thread, '') = ''"
            : scope.chat === 'draft'
              ? ' AND 0'
              : ' AND p.t3_thread = ?';
      const args = [...(anyone ? [] : [me]), ...(query.only ? [String(query.only)] : ['all', 'app', 'draft'].includes(scope.chat) ? [] : [scope.chat])];
      const order = 'ORDER BY p.created_at DESC, p.rowid DESC';
      // all=1: the pending ones and the last few reviewed (the panel shows five), not every old proposal on each change.
      // Tables shown (show_rows) go with the pending ones; a closed one only on its own page.
      const rows = [
        ...db.prepare(`${select}${where} AND p.status IN ('pending', 'applying', 'shown') ${order} LIMIT 200`).all(...args),
        ...(query.all
          ? db
              .prepare(
                `${select}${where} AND p.status NOT IN ('pending', 'applying', 'shown'${query.only ? '' : ", 'closed'"}) ${order} LIMIT ?`,
              )
              .all(...args, Math.min(Number(query.reviewed) || 5, 50))
          : []),
      ];
      // Titles as T3 shows them now (T3 names a chat after its first message, and it can be renamed).
      const threadIds = [...groups.keys(), scope.chat, followNow.chat, ...rows.map(r => r.t3_thread)];
      const named = id => id && id !== 'app' && id !== 'all' && id !== 'draft';
      const titles = t3 ? t3.threads(threadIds.filter(named)) : new Map();
      const titleOf = id => (named(id) ? (titles.get(id)?.title ?? groups.get(id)?.title ?? null) : null);
      const chats = [...groups.values()]
        .sort((a, b) => (a.at < b.at ? 1 : -1))
        .map(g => ({ id: g.id, title: titleOf(g.id), pending: g.pending }));
      const head = {
        scope: { ...scope, title: titleOf(scope.chat) },
        follow: { ...followNow, title: titleOf(followNow.chat) },
        chats,
      };
      // stamp: the chats and titles the page shows besides the proposals. Still the page's, and no
      // change but its own edits: it keeps its list (a long poll that ran out says only that).
      const stamp = createHash('sha1')
        .update(json([head, rows.map(r => [r.t3_thread, titles.get(r.t3_thread)?.title ?? null])]))
        .digest('base64url')
        .slice(0, 12);
      if (query.wait && held && caughtUp(of, held, page) && query.stamp === stamp)
        return { status: 200, body: { unchanged: true, revision, stamp } };
      // The proposals the page holds as they are now (their digests, `have`) are not sent again: a long
      // proposal (hundreds of rows) goes once, then only when it changes.
      const have = new Set(String(query.have ?? '').split(',').slice(0, 400).filter(d => d.length === 12));
      const proposals = rows.map(r => listedView(r, titles)).map(p => (have.has(p.digest) ? { id: p.id, digest: p.digest, same: true } : p));
      // The first request of a page (no revision held): tagged, so a reload with nothing new is a 304.
      return { status: 200, tagged: !held, body: { revision, stamp, ...head, proposals } };
    }
    // A cell edited by the person in the table (Asistente → Cambios propuestos), checked as a save checks it.
    const editMatch = /^\/api\/chat\/proposals\/([0-9a-f-]{36})\/edit$/.exec(path);
    if (editMatch && method === 'POST') {
      if (!EDITORS.includes(user.role)) return bad(403, 'forbidden', 'Your role cannot edit proposals.');
      const proposal = teamProposal(editMatch[1], user);
      if (!proposal) return bad(404, 'not_found', 'Proposal not found.');
      if (isTable(proposal)) return bad(409, 'read_only_table', 'Una tabla del asistente solo se lee.');
      if (proposal.status !== 'pending') return bad(409, 'proposal_used', 'La propuesta ya no está pendiente.');
      const cells = body.cells ?? [];
      const scalar = v => v === null || ['string', 'number', 'boolean'].includes(typeof v);
      if (
        !Array.isArray(cells) ||
        cells.length > 2000 ||
        cells.some(
          c =>
            typeof c?.key !== 'string' ||
            typeof c.field !== 'string' ||
            c.field.length > 120 ||
            !scalar(c.value ?? null) ||
            (c.use !== undefined && c.use !== 'sheet' && c.use !== 'ai'),
        )
      )
        return bad(400, 'invalid_cells', 'cells must be a list of { key, field, value, use? }.');
      // Cells edited in the sheet since the proposal read them: keep the sheet's value, or write the proposal's over it.
      const sheet = Array.isArray(body.sheet)
        ? body.sheet
            .filter(c => typeof c?.key === 'string' && typeof c.field === 'string' && c.field.length <= 120 && ['sheet', 'proposal'].includes(c.use))
            .slice(0, 2000)
            .map(c => ({ ref: c.key, field: c.field, use: c.use }))
        : [];
      const remove = Array.isArray(body.remove) ? body.remove.filter(k => typeof k === 'string').slice(0, 100) : [];
      // «Marcar revisadas»: doubtful cells the person looked at and leaves as they are (or unmarks).
      const check = Array.isArray(body.check)
        ? body.check
            .filter(c => typeof c?.key === 'string' && typeof c.field === 'string' && c.field.length <= 120)
            .slice(0, 2000)
            .map(c => ({ ref: c.key, field: c.field, checked: c.checked !== false }))
        : [];
      const addEmpty = Array.isArray(body.add) ? body.add.filter(a => typeof a?.sheet === 'string').slice(0, 20) : [];
      const out = reviseChanges(
        withPageRows(parse(proposal.changes_json) ?? [], parse(proposal.page_json ?? 'null'), cells.map(c => c.key)),
        {
          // use: the buttons for the selected cells, "Valor de la hoja" (back to the sheet's value,
          // the assistant's kept aside) and "Valor de la IA" (the assistant's value again).
          set: cells.map(c => ({
            ref: c.key,
            values: {
              [c.field]:
                c.use === 'sheet'
                  ? DROP
                  : c.use === 'ai'
                    ? AI_VALUE
                    : typeof c.value === 'string'
                      ? clip(c.value, 2000)
                      : (c.value ?? null),
            },
            ...(c.before !== undefined && !c.use && scalar(c.before) ? { before: { [c.field]: c.before } } : {}),
          })),
          check,
          sheet,
          remove,
          addEmpty,
        },
        { by: 'person', user },
      );
      if (out.changes.length > PROPOSAL_ROWS) {
        const m = msg('Una propuesta tiene como máximo {n} filas', { n: PROPOSAL_ROWS });
        return { status: 409, body: { error: { code: 'too_many_rows', message: m.text, messageMsg: m.msg } } };
      }
      if (json(out.changes) !== proposal.changes_json && saveRevision(proposal, out.changes, 'person', null, page) === null)
        return bad(409, 'proposal_used', 'La propuesta ya no está pendiente.');
      return {
        status: 200,
        body: { proposal: listedView(ownProposalListed(proposal.id)), rejected: out.rejected, overrode: out.overrode },
      };
    }
    // The obvious fixes of chosen issues as one proposal to confirm (the old Tablas → Revisión de datos list; kept for links and tools).
    if (path === '/api/chat/proposals/from-checks' && method === 'POST') {
      if (!Array.isArray(body.ids) || !body.ids.length || body.ids.length > 100)
        return bad(400, 'invalid_ids', 'Choose 1 to 100 issues.');
      const wanted = new Set(body.ids.map(String));
      const merged = new Map();
      for (const issue of allIssues(store).issues) {
        if (!wanted.has(issue.id) || !issue.fix) continue;
        const change = merged.get(issue.fix.recordId) ?? { recordId: issue.fix.recordId, values: {}, notes: [] };
        Object.assign(change.values, issue.fix.values);
        change.notes.push(issue.problem);
        merged.set(issue.fix.recordId, change);
      }
      if (!merged.size) return bad(409, 'no_fixes', 'Those issues have no obvious fix, or were already fixed.');
      const threadId = namedThread(user, 'Revisión de datos');
      const context = { threadId, user, records: new Map(), sources: new Map(), results: [], proposals: [], applied: [] };
      const out = proposeChanges(
        {
          reason: tpl('Arreglos de la Revisión de datos'),
          changes: [...merged.values()].map(c => ({ recordId: c.recordId, values: c.values, note: c.notes.join(' · ') })),
        },
        context,
        { literal: true },
      );
      if (out.error) return bad(409, 'invalid_fix', out.error);
      insertMessage(threadId, 'assistant', 'Arreglos propuestos desde Revisión de datos', [], [], context.proposals);
      return { status: 201, body: out };
    }
    // "Preparar propuesta" in the Revisión tab: every agreed fix as one proposal to confirm.
    if (path === '/api/chat/proposals/from-review' && method === 'POST') {
      const agreed = agreedFixes(store, { kind: body.kind ? clip(body.kind, 300) : undefined, limit: 100 });
      if (!agreed.fixes.length) return bad(409, 'no_fixes', 'No accepted fixes are waiting.');
      const merged = new Map();
      for (const f of agreed.fixes) {
        const change = merged.get(f.recordId) ?? { recordId: f.recordId, values: {}, notes: [] };
        Object.assign(change.values, f.values);
        change.notes.push(f.note);
        merged.set(f.recordId, change);
      }
      const threadId = namedThread(user, 'Revisión de datos');
      const context = { threadId, user, records: new Map(), sources: new Map(), results: [], proposals: [], applied: [] };
      const out = proposeChanges(
        {
          reason: tpl('Correcciones acordadas en Revisión'),
          changes: [...merged.values()].map(c => ({ recordId: c.recordId, values: c.values, note: c.notes.join(' · ') })),
          issueIds: agreed.fixes.map(f => f.issueId),
        },
        context,
        { literal: true },
      );
      if (out.error) return bad(409, 'invalid_fix', out.error);
      insertMessage(threadId, 'assistant', 'Correcciones acordadas en Revisión', [], [], context.proposals);
      return { status: 201, body: { ...out, fixes: agreed.fixes.length, tasks: agreed.tasks.length } };
    }
    // "Tell the assistant": the person's message about a proposal into the T3 chat it comes from, when the app
    // can send it there (server/t3tell.mjs); otherwise the page copies it for them to paste.
    const tellMatch = /^\/api\/chat\/proposals\/([0-9a-f-]{36})\/tell$/.exec(path);
    if (tellMatch && method === 'POST') {
      if (!EDITORS.includes(user.role)) return bad(403, 'forbidden', 'Your role cannot edit proposals.');
      const proposal = teamProposal(tellMatch[1], user);
      if (!proposal) return bad(404, 'not_found', 'Proposal not found.');
      // Into the chat of whoever's proposal it is (a chat handed over: the message goes there as typed in it).
      const author = db.prepare('SELECT username FROM users WHERE id = ?').get(proposal.owner_id)?.username ?? user.username;
      const text = typeof body.text === 'string' ? body.text.trim() : '';
      if (!text || text.length > 4000) return bad(400, 'invalid_text', 'text must be 1 to 4000 characters.');
      const out = await tellChat({
        t3: config.t3,
        chats: t3,
        threadId: proposal.t3_thread || null,
        username: author,
        text,
        ...(config.t3Fetch ? { fetchImpl: config.t3Fetch } : {}),
      });
      return { status: 200, body: { ...out, chat: proposal.t3_thread || null } };
    }
    const discardMatch = /^\/api\/chat\/proposals\/([0-9a-f-]{36})\/discard$/.exec(path);
    if (discardMatch && method === 'POST') {
      const proposal = teamProposal(discardMatch[1], user);
      if (!proposal) return bad(404, 'not_found', 'Proposal not found.');
      // A table shown (show_rows) is closed; a proposal, discarded.
      const [from, to] = isTable(proposal) ? ['shown', 'closed'] : ['pending', 'discarded'];
      const done = db.prepare('UPDATE ai_proposals SET status = ? WHERE id = ? AND status = ?').run(to, discardMatch[1], from);
      if (done.changes) changed(proposal.owner_id);
      return done.changes
        ? { status: 200, body: { proposalId: discardMatch[1], status: to } }
        : bad(409, 'proposal_used', 'Proposal is no longer pending.');
    }
    const proposalMatch = /^\/api\/chat\/proposals\/([0-9a-f-]{36})\/apply$/.exec(path);
    if (proposalMatch && method === 'POST') {
      const proposal = teamProposal(proposalMatch[1], user);
      if (!proposal) return bad(404, 'not_found', 'Proposal not found.');
      if (isTable(proposal)) return bad(409, 'read_only_table', 'Una tabla del asistente solo se lee.');
      if (proposal.status !== 'pending') return bad(409, 'proposal_used', 'Proposal has already been applied.');
      const requestId = typeof body.requestId === 'string' ? body.requestId : '';
      if (requestId.length < 8 || requestId.length > 120)
        return bad(400, 'request_id_required', 'A unique requestId of 8 to 120 characters is required.');
      if (body.indexes !== undefined && (!Array.isArray(body.indexes) || body.indexes.some(i => !Number.isInteger(i))))
        return bad(400, 'invalid_indexes', 'indexes must be a list of row numbers.');
      // The rows were chosen on the revision the table showed: if the assistant changed it since, look again.
      if (body.revision !== undefined && Number(body.revision) !== proposal.revision)
        return bad(409, 'proposal_changed', 'La propuesta cambió mientras la revisabas: mira la tabla y vuelve a aplicar.');
      try {
        const doubtful = body.doubtful === 'confirm' || body.doubtful === 'skip' ? body.doubtful : null;
        const out = await applyProposal(proposal, user, { requestId, indexes: body.indexes, reason: body.reason, doubtful });
        return { status: out.status === 'applied' ? 200 : 409, body: out };
      } catch (cause) {
        const current = (parse(proposal.changes_json) ?? []).map(change => {
          const record = store.getRecord(change.recordId);
          return {
            recordId: change.recordId,
            version: record?.version ?? null,
            values: record ? Object.fromEntries(Object.keys(change.values).map(f => [f, record.values?.[f]])) : null,
          };
        });
        const status = db.prepare('SELECT status FROM ai_proposals WHERE id = ?').get(proposal.id)?.status;
        return {
          status: cause.status ?? 409,
          body: {
            error: {
              code: cause.code ?? 'apply_failed',
              message: clip(cause.message, 300),
              details: {
                proposalId: proposal.id,
                status,
                current,
                items: cause.details?.items?.slice(0, 20) ?? [],
                ...(cause.details?.doubtful ? { doubtful: cause.details.doubtful.length } : {}),
                ...(cause.details?.again ? { again: cause.details.again } : {}),
                ...(cause.details?.keptFromSheet ? { keptFromSheet: cause.details.keptFromSheet } : {}),
                ...(cause.details?.unreadable?.length ? { unreadable: cause.details.unreadable.length } : {}),
              },
            },
          },
        };
      }
    }
    if (path === '/api/ai/transcribe' && method === 'POST') {
      if (!ai.transcriptionModel)
        return bad(501, 'unsupported', 'Audio transcription is not configured. Use text entry.');
      const mime = String(body.mimeType ?? '');
      if (!/^audio\/(mpeg|mp3|mp4|m4a|ogg|wav|webm|flac)$/.test(mime))
        return bad(400, 'invalid_audio', 'Unsupported audio type.');
      const data = String(body.dataBase64 ?? '');
      if (!/^[A-Za-z0-9+/]+={0,2}$/.test(data) || data.length > 14_000_000)
        return bad(400, 'invalid_audio', 'Audio data is missing or too large.');
      const format = mime === 'audio/mpeg' ? 'mp3' : mime.split('/')[1];
      try {
        let text;
        if (ai.transcriptionMode === 'chat') {
          const response = await complete(
            ai,
            [
              {
                role: 'system',
                content:
                  'Transcribe the audio verbatim in its original language. Return only the transcript. Do not invent unclear words.',
              },
              {
                role: 'user',
                content: [
                  { type: 'text', text: 'Transcribe this field voice note verbatim.' },
                  { type: 'input_audio', input_audio: { data, format } },
                ],
              },
            ],
            [],
            ai.transcriptionModel,
          );
          text = response.content;
        } else if (String(ai.baseUrl).includes('openrouter.ai')) {
          const response = await providerFetch(ai, 'audio/transcriptions', {
            model: ai.transcriptionModel,
            input_audio: { data, format },
          });
          text = response.text;
        } else {
          const form = new FormData();
          form.set('model', ai.transcriptionModel);
          form.set('file', new Blob([Buffer.from(data, 'base64')], { type: mime }), `recording.${format}`);
          const response = await providerFetch(ai, 'audio/transcriptions', form, true);
          text = response.text;
        }
        if (typeof text !== 'string' || !text.trim()) throw new Error('Missing transcription');
        return { status: 200, body: { text, draft: true, requiresReview: true } };
      } catch {
        return bad(502, 'provider_error', 'Audio transcription failed. Try again or enter text.');
      }
    }
    if (path === '/api/ai/extract' && method === 'POST') {
      if (!ai.visionModel)
        return bad(501, 'unsupported', 'Image extraction is not configured. Enter the observation manually.');
      const mime = String(body.mimeType ?? '');
      if (!/^image\/(png|jpeg|webp)$/.test(mime)) return bad(400, 'invalid_image', 'Use PNG, JPEG, or WebP.');
      const data = String(body.dataBase64 ?? '');
      if (!/^[A-Za-z0-9+/]+={0,2}$/.test(data) || data.length > 14_000_000)
        return bad(400, 'invalid_image', 'Image data is missing or too large.');
      try {
        const answer = await complete(
          ai,
          [
            {
              role: 'system',
              content:
                'Transcribe visible text from this field note or label as a draft. Return JSON with keys text and uncertain. Do not guess specimen identity, taxon, sex, or biological outcome. If unclear, say so in uncertain. No record write is permitted.',
            },
            {
              role: 'user',
              content: [
                { type: 'text', text: clip(body.prompt || 'Extract visible text.', 500) },
                { type: 'image_url', image_url: { url: `data:${mime};base64,${data}` } },
              ],
            },
          ],
          [],
          ai.visionModel,
        );
        const raw = String(answer.content ?? '');
        const parsed = parse(raw.replace(/^```(?:json)?\s*|\s*```$/g, ''));
        return {
          status: 200,
          body: {
            text: clip(parsed?.text ?? raw, 12000),
            uncertain: parsed?.uncertain ?? [],
            draft: true,
            requiresReview: true,
          },
        };
      } catch {
        return bad(502, 'provider_error', 'Image extraction failed. Enter the observation manually.');
      }
    }
    return bad(404, 'not_found', 'Assistant route not found.');
  }
  /**
   * After each sync that read the sheets: the proposals in needs_review compared with them.
   * All their cells in the sheet: applied (written after all, or by hand); else the cells
   * that differ are kept (check_json) for get_proposal and the table.
   */
  function checkNeedsReview() {
    const rows = db.prepare("SELECT * FROM ai_proposals WHERE status = 'needs_review'").all();
    const out = { applied: [], differ: [] };
    for (const p of rows) {
      const changes = parse(p.changes_json) ?? [];
      const { cells, differ } = compareWithSheet(store, changes, parse(p.created_json ?? 'null') ?? {});
      if (!cells) continue;
      const at = now();
      if (!differ.length) {
        const written = changes.map((c, i) => (!c.context && Object.keys(c.values ?? {}).length ? i : -1)).filter(i => i >= 0);
        db.prepare(
          "UPDATE ai_proposals SET status = 'applied', applied_at = ?, applied_json = ?, check_json = ? WHERE id = ? AND status = 'needs_review'",
        ).run(at, json(written), json({ at, matched: cells, differ: [] }), p.id);
        out.applied.push(p.id);
        console.log(`Proposal ${p.id}: every cell (${cells}) is in the sheet; marked applied`);
      } else {
        db.prepare('UPDATE ai_proposals SET check_json = ? WHERE id = ?').run(json({ at, matched: cells - differ.length, differ: differ.slice(0, 200), count: differ.length }), p.id);
        out.differ.push(p.id);
      }
      changed(p.owner_id);
    }
    return out;
  }
  const stopWatching = store.watchSyncs?.(status => {
    if (status?.state === 'error') return;
    try {
      checkNeedsReview();
    } catch (e) {
      console.error('needs_review check:', e.message);
    }
  });

  return {
    handle,
    mcp,
    tools: mcpTools,
    t3,
    sheetsCopy,
    checkNeedsReview,
    close() {
      stopWatching?.();
      sheetsCopy?.close();
      queries?.close();
    },
  };
}
