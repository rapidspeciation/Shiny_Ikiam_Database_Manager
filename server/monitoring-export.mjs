// Monitoring data to download (Monitoreo → Wikiloc), for anyone signed in:
// the monitoring rows of Collection_data as CSV (with the place of their
// Wikiloc point), the Wikiloc points as CSV or GPX, and the walks' GPS lines as
// GPX, for one walk or all. Also the inventory of the Wikiloc data in the app
// and the corrections it suggests (server/suggestions/wikiloc-transects.mjs).
// Nothing is written.

import { serialToIso } from '../frontend/src/lib/dates.ts';
import { formatMinutes } from '../frontend/src/lib/monitoring.ts';
import { MAX_SECTION_DISTANCE } from '../frontend/src/lib/transects.ts';
import { moduleMap } from './schema.mjs';
import { analyse } from './suggestions/wikiloc-transects.mjs';

const fail = (code, message, status = 400) => Object.assign(new Error(message), { code, status });
const clean = v => (v === null || v === undefined ? '' : String(v).trim());

/** A CSV cell; text that a spreadsheet would read as a formula is prefixed with '. */
export function csvCell(value) {
  if (typeof value === 'number' || typeof value === 'boolean') return String(value);
  const raw = value == null ? '' : String(value);
  const text = /^[=+\-@\t]/.test(raw) ? `'${raw}` : raw;
  return /[",\r\n]/.test(text) ? `"${text.replaceAll('"', '""')}"` : text;
}
/** CSV with a BOM, so spreadsheet programs read the accents. */
const csv = (header, lines) => `\uFEFF${[header, ...lines].map(l => l.map(csvCell).join(',')).join('\r\n')}\r\n`;
const xml = v =>
  String(v ?? '').replace(/[<>&"']/g, c => ({ '<': '&lt;', '>': '&gt;', '&': '&amp;', '"': '&quot;', "'": '&apos;' })[c]);
const isoDate = v => (typeof v === 'number' && v > 0 ? serialToIso(v) : v);
const hhmm = v => (typeof v === 'number' && v >= 0 && v < 1 ? formatMinutes(Math.round(v * 1440)) : v);
const photoUrl = (photos, sources) => photos.map(id => sources.get(String(id))).filter(Boolean).join(' ');
const fileDate = (walk) => `${walk.date}_${clean(walk.collector).split(' - ')[0] || 'NA'}`;

/** Public (wklcdn) addresses of the stored photos. */
function photoSources(store) {
  return new Map(store.db.prepare('SELECT id, source_url FROM monitoring_photos').all().map(r => [r.id, r.source_url]));
}

/** One walk (its id: a stored track or a Wikiloc walk waiting for review) or all, with their points. */
function walksOf(store, walkId) {
  const { points, walks } = analyse(store);
  if (!walkId) return { points, walks };
  const chosen = walks.filter(w => w.walk.id === walkId);
  if (!chosen.length) throw fail('WALK_NOT_FOUND', 'Walk not found', 404);
  return { points: points.filter(p => p.walk.id === walkId), walks: chosen };
}

// ------------------------------------------------------------------ CSV

/**
 * Every monitoring row of Collection_data with every column as in the sheet
 * (dates as YYYY-MM-DD, times as h:mm), plus where its Wikiloc point is: the
 * point's coordinates and elevation, the section they give, its distance to
 * the trail, the note, the walk's link and the photos.
 */
export function monitoringRowsCsv(store) {
  const { points, monitoring } = analyse(store);
  const fields = moduleMap.get('Collection_data').fields;
  const sources = photoSources(store);
  const byRow = new Map();
  for (const p of points) if (p.row && !byRow.has(p.row.id)) byRow.set(p.row.id, p);
  const header = [
    'Sheet_row',
    ...fields.map(f => f.key),
    'Wikiloc_latitude',
    'Wikiloc_longitude',
    'Wikiloc_elevation_m',
    'Wikiloc_transect_section',
    'Wikiloc_distance_to_trail_m',
    'Wikiloc_note',
    'Wikiloc_walk',
    'Wikiloc_photos',
  ];
  const lines = monitoring.map(r => {
    const p = byRow.get(r.id);
    const values = fields.map(f => {
      const v = r.values[f.key];
      if (f.type === 'date') return isoDate(v);
      return f.key === 'Collection_time' ? hhmm(v) : v;
    });
    return [
      r.row,
      ...values,
      ...(p
        ? [
            p.capture.lat,
            p.capture.lon,
            p.capture.ele ?? '',
            p.pos.distance <= MAX_SECTION_DISTANCE ? p.pos.section : '',
            Math.round(p.pos.distance),
            p.capture.text,
            p.walk.url || '',
            photoUrl(p.capture.photos || [], sources),
          ]
        : ['', '', '', '', '', '', '', '']),
    ];
  });
  return { name: 'monitoring_rows.csv', type: 'text/csv; charset=utf-8', body: csv(header, lines) };
}

/** The Wikiloc points (one walk or all) with their sheet row and what it says. */
export function pointsCsv(store, walkId = null) {
  const { points, walks } = walksOf(store, walkId);
  const sources = photoSources(store);
  const header = [
    'Walk_date',
    'Collector',
    'Walk',
    'Walk_link',
    'On_map',
    'Point',
    'Latitude',
    'Longitude',
    'Elevation_m',
    'Note',
    'Transect_section_GPS',
    'Distance_to_trail_m',
    'Sheet_row',
    'Pairing',
    'SPECIES',
    'Subspecies_Form',
    'Sex',
    'FieldMark_ID',
    'Release_Collect',
    'Collection_time',
    'Transect_section',
    'Photos',
  ];
  const lines = points.map(p => {
    const v = p.row?.values || {};
    return [
      p.walk.date,
      p.walk.collector || '',
      p.walk.name,
      p.walk.url || '',
      p.walk.stored ? 'yes' : 'waiting review',
      p.index + 1,
      p.capture.lat,
      p.capture.lon,
      p.capture.ele ?? '',
      p.capture.text,
      p.pos.distance <= MAX_SECTION_DISTANCE ? p.pos.section : '',
      Math.round(p.pos.distance),
      p.row?.row ?? '',
      p.row ? p.pairing : p.pairing === 'none-chosen' ? 'not a sheet row' : 'none',
      v.SPECIES ?? '',
      v.Subspecies_Form ?? '',
      v.Sex ?? '',
      v.FieldMark_ID ?? '',
      v.Release_Collect ?? '',
      hhmm(v.Collection_time) ?? '',
      v.Transect_section ?? '',
      photoUrl(p.capture.photos || [], sources),
    ];
  });
  const name = walkId ? `wikiloc_points_${fileDate(walks[0].walk)}.csv` : 'wikiloc_points_all.csv';
  return { name, type: 'text/csv; charset=utf-8', body: csv(header, lines) };
}

// ------------------------------------------------------------------ GPX

/**
 * GPX 1.1 of one walk or all: each point as a waypoint (its note as the name,
 * its row's species, sex, mark and section in the description, the photos as
 * links) and each walk's GPS line as a track. Lines read from Wikiloc's public
 * pages have no times; only GPX files uploaded to the app keep them.
 */
export function walksGpx(store, walkId = null) {
  const { points, walks } = walksOf(store, walkId);
  const sources = photoSources(store);
  const wpt = p => {
    const v = p.row?.values || {};
    const desc = [
      `${p.walk.date} ${clean(p.walk.collector)}`,
      p.row ? `Collection_data row ${p.row.row}` : 'no sheet row',
      [clean(v.SPECIES), clean(v.Subspecies_Form), clean(v.Sex), clean(v.FieldMark_ID)].filter(s => s && s !== 'NA').join(' '),
      p.pos.distance <= MAX_SECTION_DISTANCE ? `T${p.pos.section}` : '',
    ]
      .filter(Boolean)
      .join(' · ');
    const links = (p.capture.photos || [])
      .map(id => sources.get(String(id)))
      .filter(Boolean)
      .map(href => `<link href="${xml(href)}"><type>image/jpeg</type></link>`)
      .join('');
    const ele = p.capture.ele === null || p.capture.ele === undefined ? '' : `<ele>${p.capture.ele}</ele>`;
    return `<wpt lat="${p.capture.lat}" lon="${p.capture.lon}">${ele}<name>${xml(p.capture.text)}</name><desc>${xml(desc)}</desc>${links}</wpt>`;
  };
  const trk = (walk, track) => {
    if (!track.length) return '';
    const pts = track
      .map(([lat, lon, ele, time]) => `<trkpt lat="${lat}" lon="${lon}">${ele === null || ele === undefined ? '' : `<ele>${ele}</ele>`}${time ? `<time>${xml(time)}</time>` : ''}</trkpt>`)
      .join('');
    const link = walk.url ? `<link href="${xml(walk.url)}"/>` : '';
    return `<trk><name>${xml(`${walk.date} ${walk.name}`)}</name><desc>${xml(clean(walk.collector))}</desc>${link}<trkseg>${pts}</trkseg></trk>`;
  };
  const name = walkId ? `wikiloc_${fileDate(walks[0].walk)}.gpx` : 'wikiloc_all_walks.gpx';
  const body = [
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<gpx version="1.1" creator="Ikiam Insectary DB" xmlns="http://www.topografix.com/GPX/1/1">',
    `<metadata><name>${xml(walkId ? `${walks[0].walk.date} ${walks[0].walk.name}` : 'Ikiam monitoring walks')}</name></metadata>`,
    ...points.map(wpt),
    ...walks.map(w => trk(w.walk, w.track)),
    '</gpx>',
    '',
  ].join('\n');
  return { name, type: 'application/gpx+xml; charset=utf-8', body };
}

// ------------------------------------------------------------------ inventory and corrections

/**
 * What Wikiloc data the app holds: walks on the map and waiting for review,
 * points, photos, whether their lines have GPS times, per collector with the
 * monitoring days that have rows but no walk; and the walks, for downloading one.
 */
export function wikilocInventory(store) {
  const { points, walks, daysWithoutWalk } = analyse(store);
  const byWalk = new Map(walks.map(({ walk, track }) => [walk, { ...walk, points: 0, linked: 0, timed: track.some(t => t[3]) }]));
  for (const p of points) {
    const w = byWalk.get(p.walk);
    w.points++;
    if (p.row) w.linked++;
  }
  const list = [...byWalk.values()].sort((a, b) => b.date.localeCompare(a.date) || clean(a.collector).localeCompare(clean(b.collector)));
  const photos = store.db.prepare('SELECT count(*) n, coalesce(sum(length(data)),0) bytes FROM monitoring_photos').get();
  const people = new Map();
  for (const w of list) {
    const key = w.collector || '';
    const c = people.get(key) || { collector: w.collector, stored: 0, waiting: 0, first: w.date, last: w.date, points: 0, linked: 0, daysWithoutWalk: 0 };
    c[w.stored ? 'stored' : 'waiting']++;
    c.points += w.points;
    c.linked += w.linked;
    if (w.date < c.first) c.first = w.date;
    if (w.date > c.last) c.last = w.date;
    people.set(key, c);
  }
  for (const d of daysWithoutWalk) {
    const c = [...people.values()].find(p => clean(p.collector).split(' - ')[0] === clean(d.collector).split(' - ')[0]);
    if (c) c.daysWithoutWalk++;
  }
  const profiles = store.db.prepare('SELECT name, collector, last_checked FROM wikiloc_profiles ORDER BY created_at').all();
  return {
    stored: list.filter(w => w.stored).length,
    waiting: list.filter(w => !w.stored).length,
    first: list.at(-1)?.date ?? null,
    last: list[0]?.date ?? null,
    points: points.length,
    linked: points.filter(p => p.row).length,
    photos: photos.n,
    photoBytes: photos.bytes,
    timed: list.filter(w => w.timed).length,
    collectors: [...people.values()].sort((a, b) => clean(a.collector).localeCompare(clean(b.collector))),
    profiles: profiles.map(p => ({ name: p.name, collector: p.collector, lastChecked: p.last_checked })),
    walks: list.map(({ id, date, collector, name, url, stored, points: n, linked, timed }) => ({ id, date, collector, name, url, stored, points: n, linked, timed })),
  };
}

/** The corrections the Wikiloc points suggest, the other discrepancies and the inventory (Monitoreo → Wikiloc). */
export function wikilocCorrections(store) {
  const { suggestions, discrepancies, daysWithoutWalk, counts } = analyse(store);
  return { counts, suggestions, discrepancies, daysWithoutWalk, inventory: wikilocInventory(store) };
}
