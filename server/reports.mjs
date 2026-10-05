import { cachedRows } from './records-tool.mjs';
import { makeSourceUrl } from './schema.mjs';

const MAX_ROWS = 100000;
const PAGE_SIZE = 500;

const error = (status, code, message) => ({ status, body: { error: { code, message } } });
const nonempty = value =>
  value !== null &&
  value !== undefined &&
  !['', 'NA', 'N/A', '-', '—', 'NULL'].includes(String(value).trim().toUpperCase());
const source = record => ({
  id: record.id,
  sheet: record.sheet,
  row: record.row,
  version: record.version,
  sourceUrl: record.sourceUrl,
});
const safeDate = value => {
  const serial = Number(value);
  if (value !== null && value !== '' && Number.isFinite(serial) && serial >= 20000 && serial < 100000) {
    return new Date(Date.UTC(1899, 11, 30) + Math.floor(serial) * 86400000).toISOString().slice(0, 10);
  }
  const date = String(value ?? '').slice(0, 10);
  return /^\d{4}-\d{2}-\d{2}$/.test(date) ? date : null;
};

function report(title, columns, rows, method, records, extra = {}) {
  return {
    title,
    columns,
    rows,
    series: [{ type: 'bar', x: columns[0]?.key, y: columns[1]?.key, data: rows }],
    method:
      records.length > 500
        ? `${method} The response includes the first 500 source links; sourceCount gives the total included record count.`
        : method,
    sources: records.slice(0, 500).map(source),
    sourceCount: records.length,
    generatedAt: new Date().toISOString(),
    ...extra,
  };
}

function countBy(records, key) {
  const counts = new Map();
  for (const record of records) {
    const value = nonempty(record.values?.[key]) ? String(record.values[key]).trim() : '(missing)';
    counts.set(value, (counts.get(value) ?? 0) + 1);
  }
  return [...counts]
    .map(([value, count]) => ({ value, count }))
    .sort((a, b) => b.count - a.count || a.value.localeCompare(b.value));
}

function moduleName(module) {
  return typeof module === 'string' ? module : (module.sheet ?? module.id);
}

/**
 * The rows a report reads, the most recently changed first. From the parsed rows
 * the assistant's tools share (server/records-tool.mjs): a report of 20k rows read
 * by pages of 500 took a second or more.
 */
async function collect(store, module) {
  if (store.db) {
    try {
      const records = cachedRows(store, module)
        .filter(r => r.observed)
        .sort((a, b) => (a.updatedAt < b.updatedAt ? 1 : a.updatedAt > b.updatedAt ? -1 : b.row - a.row));
      return { records, total: records.length, truncated: false };
    } catch {
      /* A test store may not expose the core records table: read it by pages. */
    }
  }
  const records = [];
  let total = 0;
  for (let offset = 0; offset < MAX_ROWS; offset += PAGE_SIZE) {
    const page = await store.searchRecords({ module, observedOnly: true, limit: PAGE_SIZE, offset });
    const batch = page?.records ?? [];
    records.push(...batch);
    total = Number(page?.total ?? records.length);
    if (!batch.length || records.length >= total) break;
  }
  return { records, total, truncated: total > records.length };
}

function usable(records) {
  // A preallocated identifier alone is not an observed individual or clutch.
  return records.filter(
    record =>
      record.observed === true ||
      (record.observed !== false &&
        Object.entries(record.values ?? {}).some(
          ([key, value]) =>
            nonempty(value) &&
            !Object.hasOwn(record.formulas ?? {}, key) &&
            !/^(Insectary_ID|CAM_ID|FieldMark_ID|CLUTCH NUMBER|Data_entry_order)$/i.test(key),
        )),
  );
}

export function createReports({ store }) {
  async function build(query = {}) {
    const kind = String(query.kind ?? 'overview').toLowerCase();
    const moduleDefinitions = await store.listModules();
    const modules = moduleDefinitions.map(moduleName);
    const named = query.module && modules.includes(query.module) ? query.module : null;
    const target =
      named ??
      {
        stages: 'Insectary_stocks',
        crosses: 'Melinaea_crosses',
        samples: 'Pheromones_data',
        weekly: 'Insectary_data',
        daily: 'Collection_data',
        quality: 'Collection_data',
      }[kind];
    if (query.module && !named) return error(400, 'invalid_module', 'Unknown module.');
    if (target && !modules.includes(target))
      return error(404, 'module_unavailable', 'The report source module is unavailable.');

    if (kind === 'overview') {
      const rows = [];
      const sources = [];
      let truncated = false;
      for (const module of modules) {
        const definition = moduleDefinitions.find(item => moduleName(item) === module);
        let count = definition?.observedCount;
        if (count === undefined && store.db) {
          try {
            count = store.db
              .prepare('SELECT count(*) AS n FROM records WHERE sheet = ? AND missing = 0 AND observed = 1')
              .get(module).n;
          } catch {
            /* A test store may not expose the core records table. */
          }
        }
        if (count === undefined) {
          const data = await collect(store, module);
          count = usable(data.records).length;
          truncated ||= data.truncated;
        }
        const sample = await store.searchRecords({ module, observedOnly: true, limit: 20, offset: 0 });
        rows.push({ module, count, sourceRows: definition?.recordCount ?? sample.total });
        sources.push(...usable(sample.records ?? []));
      }
      return {
        status: 200,
        body: report(
          'Recorded rows by source table',
          [
            { key: 'module', label: 'Table' },
            { key: 'count', label: 'Rows with data' },
            { key: 'sourceRows', label: 'All indexed rows' },
          ],
          rows,
          'Counts records with source observations according to the store observation rule. These are record counts, not individuals. The source list samples up to 20 records per table.',
          sources,
          { truncated, sourceCount: rows.reduce((sum, row) => sum + row.count, 0), sourceLinkCount: sources.length },
        ),
      };
    }

    if (!['stages', 'crosses', 'samples', 'daily', 'weekly', 'quality', 'counts'].includes(kind))
      return error(400, 'invalid_report', 'Unknown report kind.');
    const data = await collect(store, target);
    const records = usable(data.records);
    let output;
    if (kind === 'counts') {
      const field = String(query.groupBy ?? query.field ?? 'SPECIES');
      if (!records.some(record => Object.hasOwn(record.values ?? {}, field)))
        return error(400, 'invalid_field', 'Field is not present in the selected module.');
      output = report(
        `Rows by ${field}`,
        [
          { key: 'value', label: field },
          { key: 'count', label: 'Rows' },
        ],
        countBy(records, field),
        `Counts nonempty indexed ${target} rows by the exact ${field} value. Blank values are shown as missing. Does not deduplicate specimens.`,
        records,
      );
    } else if (kind === 'stages') {
      const fields = ['NUMBER OF EGGS', 'NUMBER OF LARVAE', 'NUMBER OF PUPA', 'NUMBER OF ADULTS'];
      const rows = fields.map(field => {
        const observed = records.filter(
          record => Number.isFinite(Number(record.values?.[field])) && nonempty(record.values?.[field]),
        );
        return {
          stage: field.replace('NUMBER OF ', ''),
          total: observed.reduce((sum, record) => sum + Number(record.values[field]), 0),
          recordedClutches: observed.length,
          missingClutches: records.length - observed.length,
        };
      });
      output = report(
        'Recorded clutch stage totals',
        [
          { key: 'stage', label: 'Stage' },
          { key: 'total', label: 'Recorded count' },
          { key: 'recordedClutches', label: 'Clutches with a value' },
          { key: 'missingClutches', label: 'Clutches without a value' },
        ],
        rows,
        'Sums numeric stage fields in Insectary_stocks once per populated clutch row. Missing values are excluded from totals. These fields may represent different observation dates; totals are neither current live occupancy nor a cohort survival calculation.',
        records,
      );
    } else if (kind === 'crosses') {
      const rows = [
        { state: 'Attempt rows', count: records.length },
        {
          state: 'Observed mating entered',
          count: records.filter(record => nonempty(record.values?.Mating_started)).length,
        },
        {
          state: 'Mating observation missing',
          count: records.filter(record => !nonempty(record.values?.Mating_started)).length,
        },
        {
          state: 'Egg count entered',
          count: records.filter(record => nonempty(record.values?.['Number of Eggs laid'])).length,
        },
      ];
      output = report(
        'Cross attempt records',
        [
          { key: 'state', label: 'Recorded field' },
          { key: 'count', label: 'Rows' },
        ],
        rows,
        'Counts populated Melinaea_crosses rows. Mating is counted only when Mating_started is entered. Egg entries do not imply mating or fertilization. Rows overlap across categories.',
        records,
      );
    } else if (kind === 'samples') {
      const rows = [
        { state: 'Sample rows', count: records.length },
        { state: 'CAM ID missing', count: records.filter(record => !nonempty(record.values?.CAM_ID)).length },
        {
          state: 'Tube ID missing',
          count: records.filter(
            record => !['Tube_1_id', 'Tube_2_id', 'Tube_3_id'].some(field => nonempty(record.values?.[field])),
          ).length,
        },
        { state: 'Treatment missing', count: records.filter(record => !nonempty(record.values?.Treatment)).length },
      ];
      output = report(
        'Pheromone sample record completeness',
        [
          { key: 'state', label: 'Check' },
          { key: 'count', label: 'Rows' },
        ],
        rows,
        'Counts populated Pheromones_data rows and missing source fields. A tube ID does not prove physical custody or completed extraction. Missing categories can overlap.',
        records,
      );
    } else if (kind === 'quality') {
      const key = String(query.field ?? 'CAM_ID');
      if (!records.some(record => Object.hasOwn(record.values ?? {}, key)))
        return error(400, 'invalid_field', 'Field is not present in the selected module.');
      const valueCounts = new Map();
      for (const record of records)
        if (nonempty(record.values[key]))
          valueCounts.set(String(record.values[key]), (valueCounts.get(String(record.values[key])) ?? 0) + 1);
      const rows = [
        { check: 'Rows with data', count: records.length },
        { check: `${key} missing`, count: records.filter(record => !nonempty(record.values[key])).length },
        { check: `${key} repeated values`, count: [...valueCounts.values()].filter(count => count > 1).length },
      ];
      output = report(
        `${key} completeness`,
        [
          { key: 'check', label: 'Check' },
          { key: 'count', label: 'Count' },
        ],
        rows,
        `Checks populated ${target} rows only. Repeated values are candidate identity conflicts, not confirmed duplicate specimens.`,
        records,
      );
    } else {
      const dateFields = ['Intro2Insectary_date', 'Collection_date', 'DATE_OF_COLLECTION', 'Date', 'Start date'];
      const counts = new Map();
      let missing = 0;
      for (const record of records) {
        const date = dateFields.map(field => safeDate(record.values?.[field])).find(Boolean);
        if (!date) {
          missing++;
          continue;
        }
        const start = new Date(`${date}T12:00:00Z`);
        if (kind === 'weekly') start.setUTCDate(start.getUTCDate() - ((start.getUTCDay() + 6) % 7));
        const period = start.toISOString().slice(0, 10);
        counts.set(period, (counts.get(period) ?? 0) + 1);
      }
      const today = new Date();
      const days = kind === 'weekly' ? 112 : 14;
      const cutoff = new Date(
        Date.UTC(today.getUTCFullYear(), today.getUTCMonth(), today.getUTCDate()) - (days - 1) * 86400000,
      )
        .toISOString()
        .slice(0, 10);
      const rows = [...counts]
        .filter(([date]) => date >= cutoff)
        .sort(([a], [b]) => a.localeCompare(b))
        .map(([date, count]) => ({ date, count }));
      const name = kind === 'weekly' ? 'week' : 'day';
      output = report(
        `Source records by ${name}`,
        [
          { key: 'date', label: kind === 'weekly' ? 'Week starting Monday' : 'Date' },
          { key: 'count', label: 'Rows' },
        ],
        rows,
        `Groups populated ${target} rows by the first available date in ${dateFields.join(', ')} and displays the past ${days} days. ${missing} rows have no usable date and are excluded. A source date can precede entry; counts are not a census or staff activity log.`,
        records,
        {
          excluded: {
            missingDate: missing,
            outsideWindow: [...counts].filter(([date]) => date < cutoff).reduce((sum, [, count]) => sum + count, 0),
          },
        },
      );
    }
    // Rows read from the shared copy carry no link: made for the sources shown.
    output.sources = output.sources.map(s =>
      s.sourceUrl !== undefined ? s : { ...s, sourceUrl: store.localMode || !store.sheets ? null : makeSourceUrl(s.sheet, s.row, store.sheets.spreadsheetId) },
    );
    output.truncated = data.truncated;
    if (data.truncated)
      output.method += ` The first ${records.length} of ${data.total} indexed rows were examined; this report is incomplete.`;
    return { status: 200, body: output };
  }

  return {
    build,
    async handle({ method, path, query, user }) {
      if (path !== '/api/reports') return null;
      if (!user) return error(401, 'unauthorized', 'Sign in to view reports.');
      if (method !== 'GET') return error(405, 'method_not_allowed', 'Use GET.');
      return build(query);
    },
  };
}
