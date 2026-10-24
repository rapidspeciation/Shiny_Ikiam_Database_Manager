import test from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { buildLexicon, envelopeOfReadings, isoDate, noteCorrections, parseEnvelope, similarity } from '../server/envelope-fields.mjs';
import { importBundle, readCuration, readGallery, readWingSides } from '../server/envelope-import.mjs';
import { bumpReviewRevision, nameView, photoName, wingProblem } from '../server/photodata.mjs';
import { PHOTO_FACTS_TOOLS, photoFactsData, runPhotoFacts } from '../server/photo-facts.mjs';
import { planPhotoLinks } from '../server/photo-links.mjs';
import { FOLDER } from '../server/drive.mjs';

const ENVELOPES = fileURLToPath(new URL('./fixtures/envelope/', import.meta.url));
const WINGS = fileURLToPath(new URL('./fixtures/wings/', import.meta.url));

// The workbook's names, as the envelopes are matched against them (n: rows using the name).
const lexicon = buildLexicon({
  taxa: [
    ['Mechanitis polymnia proceriformis', 300],
    ['Mechanitis polymnia eurydice', 60],
    ['Mechanitis polymnia werneri x proceriformis', 100],
    ['Mechanitis messenoides deceptus', 200],
    ['Mechanitis messenoides intermedia', 150],
    ['Mechanitis lysimnia', 200],
    ['Melinaea mothone mothone', 80],
    ['Melinaea isocomma simulator', 20],
    ['Melinaea menophilus zaneka', 70],
    ['Ithomia hyala hyala', 5],
    ['Ithomia salapia salapia', 100],
    ['Hypothyris mamercus mamercus', 3],
    ['Hypothyris ninonia mamercus', 9],
    ['Methona confusa', 10],
    ['Methona grandior', 0],
    ['Polygonia interrogationis', 0],
    ['Macrosoma intermedia', 0],
    ['Episcada sulphurea', 4],
    ['Callicorina pulchra', 2],
    ['Temenis pulchra', 0],
    ['Temenis laothoe', 5],
  ].map(([name, n]) => ({ name, n })),
  localities: [
    ['Pozo Cuyabeno', 200],
    ['Finca Narupa bridge', 130],
    ['Mina Narupa', 230],
    ['Cavernas Templo de Ceremonia', 700],
    ['Quinta de Goulaine', 100],
    ['Puerto Libre - Linea 2', 110],
    ['Loreto - Comunidad Pano', 30],
    ['Ikiam', 1000],
    ['Rio Pusuno', 110],
  ].map(([name, n]) => ({ name, n })),
});
const lines = (...texts) => texts.map(t => (Array.isArray(t) ? t : [t, 1]));
const read = (...texts) => parseEnvelope(lines(...texts), lexicon);

test('similarity: 1 for the same, edits over the longer length, 0 below the floor', () => {
  assert.equal(similarity('mechanitis', 'mechanitis'), 1);
  assert.equal(similarity('methanitis', 'mechanitis'), 0.9);
  assert.equal(similarity('polymnia', 'proceriformis', 0.8), 0);
  assert.equal(isoDate('30', 'AUG', '2025'), '2025-08-30');
  assert.equal(isoDate('9', 'Sep', '24'), '2024-09-09');
  assert.equal(isoDate('31', '2', '23'), null);
});

test('dates as the archive writes them and the reader reads them', () => {
  const dateOf = text => read(text).dates.map(d => d.value);
  // Month names run into the day or the year, after a colon, in Spanish and whole.
  assert.deepEqual(dateOf('29 Nov23'), ['2023-11-29']);
  assert.deepEqual(dateOf('1Sep22'), ['2022-09-01']);
  assert.deepEqual(dateOf('29Apr25'), ['2025-04-29']);
  assert.deepEqual(dateOf('16: nov 2022'), ['2022-11-16']);
  assert.deepEqual(dateOf('15/July/2025'), ['2025-07-15']);
  assert.deepEqual(dateOf('3 agosto 2024'), ['2024-08-03']);
  assert.deepEqual(dateOf('26/Ag/2024'), ['2024-08-26']);
  // A handwritten month name read with one letter wrong or a letter as a digit; two months that near: none.
  assert.deepEqual(dateOf('27/Nor/2022'), ['2022-11-27']);
  assert.deepEqual(dateOf('4-Mov-25'), ['2025-11-04']);
  assert.deepEqual(dateOf('20/Fe6/2024'), ['2024-02-20']);
  assert.deepEqual(dateOf('8/9ep/2025'), ['2025-09-08']);
  assert.deepEqual(dateOf('18/Oec/23'), ['2023-12-18']);
  assert.deepEqual(dateOf('22/jue/22'), []);
  // Roman months between separators, the reader's 1 for I; not a letter run into the day, not a lone I.
  assert.deepEqual(dateOf('11.IV.25'), ['2025-04-11']);
  assert.deepEqual(dateOf('18.1V.2025'), ['2025-04-18']);
  assert.deepEqual(dateOf('22V07/22'), []);
  assert.deepEqual(dateOf('26.I.25'), []);
  // The time written against the year.
  assert.deepEqual(dateOf('7/4/2310:30'), ['2023-04-07']);
  assert.deepEqual(dateOf('11/4/202312:40'), ['2023-04-11']);
  assert.deepEqual(dateOf('11/4/231:00am'), ['2023-04-11']);
  // A label line names the next line's date only.
  assert.deepEqual(read('Callicore', 'Preserved dead', '12h', 'Añangu', '25/11/23').dates.map(d => d.kind), ['date']);
  assert.deepEqual(read('wing clip:', '31/Aug/24').dates.map(d => d.kind), ['wingClip']);
  // What stays unread: codes and words with digits are no months.
  assert.deepEqual(dateOf('(Reared ID: 4MT) 25'), []);
  assert.deepEqual(dateOf('Km7.2 Pongo-Barranquita'), []);
});

test('an envelope read into fields: real lines of the archive', () => {
  // CAM070361: a reared butterfly, with its insectary code and two dated events.
  const a = read(['(I15)', 0.916], 'CAM070361', ['Mechanitis', 0.949], ['polymnia eurydice', 0.963], '♀', 'Pozo Cuyabeno', ['Death: 27/11/22', 0.976], ['Preserved: 29/11/22', 0.971]);
  assert.equal(a.species.value, 'Mechanitis polymnia eurydice');
  assert.ok(a.species.conf > 0.9 && a.species.conf < 0.95, 'the lowest OCR confidence of its lines');
  assert.deepEqual([a.sex.value, a.locality.value, a.code.value, a.camid.value], ['female', 'Pozo Cuyabeno', 'I15', 'CAM070361']);
  assert.deepEqual(a.dates.map(d => [d.kind, d.value]), [['death', '2022-11-27'], ['preserved', '2022-11-29']]);
  // CAM070359v: a misread ID line less sure than the right one; the sex inside the species line; the workbook's locality name.
  const b = read(['CAM0703104', 0.71], ['(F77)', 0.999], 'CAM070359', 'Melinaea', ['mothone ♂', 0.973], 'Finca Narupa', '3/12/22');
  assert.equal(b.camid.value, 'CAM070359');
  assert.deepEqual([b.species.value, b.sex.value, b.code.value], ['Melinaea mothone', 'male', 'F77']);
  assert.deepEqual([b.locality.value, b.locality.read], ['Finca Narupa bridge', 'Finca Narupa']);
  assert.deepEqual(b.dates, [{ kind: 'date', value: '2022-12-03', conf: 1, line: '3/12/22' }]);
  // CRISPR envelopes: "M.m." for genus and species, month names, the CRISPR number.
  const c = read('CAM078518', 'M.m. deceptus', 'Peak-AB', '30/AUG/2025', 'CRISPR #166', '♂');
  assert.deepEqual([c.species.value, c.crispr.value, c.dates[0].value], ['Mechanitis messenoides deceptus', '#166', '2025-08-30']);
  // A hybrid over two lines, a reared ID, and a date labelled on the line before it.
  const d = read('CAM076261', 'Mechanitis', 'polymnia', '♂', 'werneri x', 'proceriformis', '(Reared ID: 4MT)', 'wing clip:', '9/Sep/2024');
  assert.equal(d.species.value, 'Mechanitis polymnia werneri x proceriformis');
  assert.equal(d.code.value, '4MT');
  assert.deepEqual(d.dates.map(x => [x.kind, x.value]), [['wingClip', '2024-09-09']]);
  assert.deepEqual(read('Preserved:', '21 Nov 2022').dates.map(x => [x.kind, x.value]), [['preserved', '2022-11-21']]);
});

test('species names written short, split or with another genus are read as the workbook writes them', () => {
  // "M. polymnia" is Mechanitis, not the genus Polygonia (a word that reads better as a species is no genus).
  assert.equal(read('M. polymnia', '♂', 'Rio Pusuno', '21-1-2022').species.value, 'Mechanitis polymnia');
  // A genus and a subspecies: the species that has it; a subspecies alone: the one in use, not a rare species of that name.
  assert.equal(read('Mechanitis', 'deceptus').species.value, 'Mechanitis messenoides deceptus');
  assert.equal(read('CRISPR 50', 'intermedia', '♂').species.value, 'Mechanitis messenoides intermedia');
  // The nominal subspecies repeats the species' word.
  assert.equal(read('Ithomia hyala', 'hyala', '♀').species.value, 'Ithomia hyala hyala');
  // The genus written wrong: the species by its word, a little less sure.
  const slip = read('Mechanitis', 'isocomma', 'simulator ♂');
  assert.equal(slip.species.value, 'Melinaea isocomma simulator');
  assert.ok(slip.species.conf < 0.9);
  // Two genera on the envelope: the one that has the species written.
  assert.equal(read('(Temenis)', 'Callicorina', 'pulchra').species.value, 'Callicorina pulchra');
  // Localities: abbreviations, hyphens, a name over two lines.
  assert.equal(read('Q. de Goulaine').locality.value, 'Quinta de Goulaine');
  assert.equal(read('Pto. Libre-Linea 2').locality.value, 'Puerto Libre - Linea 2');
  assert.equal(read('Loreto-Pano').locality.value, 'Loreto - Comunidad Pano');
  assert.equal(read('Cavernas Templo', 'Ceremonia').locality.value, 'Cavernas Templo de Ceremonia');
  assert.equal(read('Ikiam').species, null);
});

test('sex: symbols and words; a lone symbol on a strip without names counts half', () => {
  // "dead male": the sex, and the date stays the collection date.
  const a = read('dead male', 'M. messenoides', 'deceptus', 'CAM042052', '14-1-2022', 'Apuya');
  assert.equal(a.sex.value, 'male');
  assert.equal(a.dates[0].kind, 'date');
  // Both symbols: the surer, at half.
  const b = read('Mechanitis lysimnia ♀', ['♂', 0.6]);
  assert.deepEqual([b.sex.value, b.sex.conf, b.sex.both], ['female', 0.5, true]);
  // The top of the envelope only: the ID and a symbol.
  const c = read('CAM078718', '♂');
  assert.deepEqual([c.sex.value, c.sex.conf, c.sex.alone], ['male', 0.5, true]);
  assert.equal(c.species, null);
});

test("a butterfly's photos together: each field the surest reading, `differ` when they disagree", () => {
  const { envelope } = envelopeOfReadings(
    [
      { name: 'CAM070999d', camid: 'CAM070999', conf: 0.99, text: lines('Mechanitis', 'lysimnia ♀', 'Ikiam', '7/4/2023') },
      { name: 'CAM070999v', camid: 'CAM070999', conf: 0.98, text: lines('Mechanitis', ['polymnia', 0.7], 'Ikiam', '7/4/2023') },
    ],
    lexicon,
  );
  assert.equal(envelope.photos, 2);
  assert.equal(envelope.species.value, 'Mechanitis lysimnia');
  assert.equal(envelope.camid.seen, 2);
  assert.deepEqual(envelope.differ, ['species']);
  assert.equal(envelope.dates.length, 1, 'a date read on both photos once');
  // The genus alone on the surer photo, the genus with its species on the other: the fuller name, and no difference.
  const { envelope: two } = envelopeOfReadings(
    [
      { name: 'CAM070998d', camid: 'CAM070998', conf: 0.99, text: lines('Mechanitis', 'Ikiam') },
      { name: 'CAM070998v', camid: 'CAM070998', conf: 0.99, text: lines('Mechanitis', ['lysimnia ♀', 0.8], 'Ikiam') },
    ],
    lexicon,
  );
  assert.deepEqual([two.species.value, two.species.seen, two.differ], ['Mechanitis lysimnia', 2, undefined]);
});

test('the corrections written in the notes: species, sex and CAM_ID changes, with their sentence', () => {
  const one = noteCorrections('4Oct24 AB Species ID changed from *Mechanitis polymnia* (identified by NA) to *Hypothiris mamercus mamercus*', lexicon);
  assert.deepEqual(
    one.map(c => [c.field, c.from, c.to]),
    [['species', 'Mechanitis polymnia', 'Hypothyris mamercus mamercus']],
  );
  // An abbreviated genus is the one of the workbook's species with that word.
  const two = noteCorrections('25Sep26 AB Species ID changed from M. grandior (identified CD) to M. confusa', lexicon);
  assert.deepEqual([two[0].from, two[0].to], ['Methona grandior', 'Methona confusa']);
  const three = noteCorrections('Preserved while dying. | 27/06/24 CD: Originally identify as H. mamercus mamercus, but has been reidentified as H. ninonia mamercus after revision', lexicon);
  assert.deepEqual([three[0].from, three[0].to], ['Hypothyris mamercus mamercus', 'Hypothyris ninonia mamercus']);
  assert.equal(three[0].text, '27/06/24 CD: Originally identify as H. mamercus mamercus, but has been reidentified as H. ninonia mamercus after revision');
  // Words of a sentence are no species; a sex change is its own correction.
  assert.deepEqual(noteCorrections('24/06/24 CD: ID status changed from complete but verify to complete', lexicon), []);
  const sex = noteCorrections('27/06/24 CD: reidentified as H. ninonia mamercus, and wrongly identify as female but is male | AB 7Jun24 Sex id changed from female to male, after checking wings', lexicon);
  assert.deepEqual(sex.filter(c => c.field === 'sex').map(c => [c.from, c.to]), [['female', 'male']]);
  assert.ok(!sex.some(c => c.from === 'female' && c.field === 'species'));
  assert.deepEqual(
    noteCorrections('31Jan26 AB CAM_ID changed from CAM070503 to CAM079600. Manifest 18.').map(c => [c.field, c.from, c.to]),
    [['camid', 'CAM070503', 'CAM079600']],
  );
  assert.deepEqual(noteCorrections('in Sanger tubes. 31Jan25 AB Species and ssp. ID curated by AB').map(c => [c.field, c.checked]), [['species', true]]);
});

test('photo names: CAM IDs and the M series; the view at the end of a name', () => {
  assert.deepEqual(photoName('M6681d2.JPG'), { cam: 'M6681', view: 'dorsal', stem: 'M6681d2' });
  assert.equal(photoName('Marianne_10d.JPG'), null);
  assert.equal(photoName('IMG_0420.JPG'), null);
  assert.deepEqual(['CAM070046d2', 'CAM078863_v.JPG', 'CAMO77654v', '10d', 'IMG_0271'].map(nameView), ['d', 'v', 'v', 'd', null]);
});

// ------------------------------------------------------------ the wing results and the tool on a small workbook
const EPOCH = Date.UTC(1899, 11, 30);
const serial = iso => Math.round((Date.parse(`${iso}T00:00:00Z`) - EPOCH) / 864e5);
const ids = JSON.parse(readFileSync(join(ENVELOPES, 'ids.json'), 'utf8'));
const preserved = (row, cam, SPECIES, more = {}) => ({
  row,
  values: { Release_Collect: 'Collected_Preserved', CAM_ID: cam, SPECIES, Collection_location: 'Ikiam', Collection_date: serial('2023-03-01'), Collector: 'AB', ...more },
});
const seed = {
  Collection_data: [
    preserved(2, 'CAM000101', 'Oleria gunilla'),
    preserved(3, 'CAM000111', 'Hypothyris anastasia'),
    preserved(4, 'CAM000105', 'Mechanitis polymnia', { Sex: 'female' }),
    preserved(5, 'CAM000106', 'Mechanitis polymnia', { Sex: 'male' }),
    // Its note says the species was changed from what the envelope shows.
    preserved(6, 'CAM000107', 'Ithomia salapia', { Subspecies_Form: 'salapia', Notes_Collection_data: '31Jan25 AB Species ID changed from Episcada sulphurea (identified by AB) to Ithomia salapia' }),
    preserved(7, 'CAM000108', 'Ithomia salapia', { Subspecies_Form: 'salapia' }),
    preserved(8, 'CAM000110', 'Oleria gunilla'),
  ],
  Taxonomy_v18Jun25: ['Episcada sulphurea', 'Ithomia salapia', 'Oleria gunilla', 'Hypothyris anastasia', 'Mechanitis polymnia'].map((species, i) => ({ row: i + 2, values: { species } })),
  Photo_links: ['CAM000105d', 'CAM000105v', 'CAM000107d', 'CAM000107v'].map((name, i) => ({
    row: i + 2,
    values: { 'Full Path': `Wild/${name}.JPG`, Name: `${name}.JPG`, Type: 'File', URL: `https://drive.google.com/file/d/${ids[name]}/view` },
  })),
};
async function fixture() {
  const store = new Store({ localMode: true }, { sheets: new LocalSheets(seed) });
  await store.sync({ sheets: Object.keys(seed) });
  importBundle(store.db, { ...readCuration(ENVELOPES, join(ENVELOPES, 'manifest.csv')), ...(await readGallery(join(ENVELOPES, 'gallery'))) });
  const wings = readWingSides([join(WINGS, 'eval'), join(WINGS, 'eval', 'new_photos_20261015')], join(ENVELOPES, 'manifest.csv'));
  importBundle(store.db, { wings: wings.wings });
  return { store, wings };
}

test('the wing results are read with their Drive IDs, turns and pair verdicts, and imported again the same', async () => {
  const { store, wings } = await fixture();
  assert.deepEqual(wings.wingSources, ['archive', 'new_photos_20261015']);
  const by = Object.fromEntries(wings.wings.map(w => [w.name, w]));
  assert.equal(by.CAM000105d.fileId, ids.CAM000105d, 'from the manifest');
  assert.equal(by.CAM000105d.pair, 'suffix swapped');
  assert.deepEqual([by.CAM000107d.turn, by.CAM000107d.degrees], ['upside down', 180]);
  assert.equal(by.CAM000108v1.side, null, 'a whole specimen is not judged');
  assert.equal(by.CAM000110d.fileId, 'drive-id-new-00000000110d');
  assert.equal(by.CAM000110d.turn, 'upside down');
  assert.equal(by.CAM000110v.turn, undefined);
  assert.equal(by['10d'].cam, null);
  assert.deepEqual(importBundle(store.db, { wings: wings.wings }), { wings: 8 });
  assert.equal(store.db.prepare('SELECT count(*) n FROM photo_wing_sides').get().n, 8);
  // A photo renamed since: the wings are judged against its name now.
  assert.deepEqual(wingProblem(by.CAM000105d), { side: 'ventral', pDorsal: 0.0123, pair: 'suffix swapped' });
  assert.equal(wingProblem(by.CAM000105d, 'CAM000105v.JPG'), null);
});

test('photo_facts: per CAM, the envelope, the workbook with its notes, the gallery, the wings and the files', async () => {
  const { store } = await fixture();
  assert.equal(PHOTO_FACTS_TOOLS[0].function.name, 'photo_facts');
  const { cams } = runPhotoFacts(store, { cams: ['cam000105', 'CAM000107', 'CAM000999'], lines: true });
  const [a, b, none] = cams;
  assert.deepEqual(a.envelope.species, ['Mechanitis polymnia', 0.97]);
  assert.deepEqual(a.envelope.sex, ['male', 0.97]);
  assert.equal(a.envelope.lines.CAM000105d[1][0], 'Mechanitis');
  assert.equal(a.workbook[0].sex, 'female');
  assert.equal(a.gallery.species, 'Mechanitis polymnia 0.9, Other one 0.1');
  assert.equal(a.wings.pair, 'suffix swapped');
  assert.deepEqual(a.wings.otherSide, ['CAM000105d shows ventral', 'CAM000105v shows dorsal']);
  assert.deepEqual(a.files.map(f => f.name), ['CAM000105d.JPG', 'CAM000105v.JPG']);
  assert.deepEqual(a.disagree.map(d => d.kind), ['sex', 'wings']);
  // The envelope's species is the one the note says was changed: explained, with the note's sentence.
  assert.equal(b.envelope.species[0], 'Episcada sulphurea');
  assert.deepEqual(b.workbook[0].notes.map(n => [n.from, n.to]), [['Episcada sulphurea', 'Ithomia salapia']]);
  assert.deepEqual(b.disagree.map(d => d.kind), ['wings'], 'the species is explained; the photo is turned');
  assert.match(b.explained[0], /^species: note: changed from Episcada sulphurea to Ithomia salapia/);
  assert.equal(none.envelope, 'not read (not in the archive scan)');
  // A save that changes no name keeps the envelopes as read; the rows are read again.
  const before = photoFactsData(store);
  store.db.prepare("UPDATE records SET version = version + 1, values_json = json_set(values_json, '$.Sex', 'male') WHERE sheet = 'Collection_data' AND row_num = 4").run();
  const after = photoFactsData(store);
  assert.notEqual(after, before);
  assert.equal(after.envelopes, before.envelopes);
  assert.equal(after.byCam.get('CAM000105')[0].values.Sex, 'male');
  assert.equal(none.workbook, 'no row with this CAM_ID');
});

test('photo_facts with the photo readings: the top 5, why a sex is not supported, the photos alone that give another species, wings counted and missing', async () => {
  const { store } = await fixture();
  // The photo readings' columns and table come with the app's photo tables (server/photodata.mjs).
  const top5 = JSON.stringify([['Mechanitis polymnia', 0.8123], ['Mechanitis messenoides', 0.1], ['Melinaea menophilus', 0.05], ['Mechanitis lysimnia', 0.02], ['Hypothyris anastasia', 0.011], ['Oleria onega', 0.004]]);
  store.db.prepare("UPDATE photo_predictions SET species_json=?, sex='female', sex_conf=0.71, sex_supported=0, sex_status='below_confidence' WHERE cam='CAM000105'").run(top5);
  store.db.prepare("UPDATE photo_wing_sides SET wings=3, missing_json='[\"hindwing_left\"]', side='mixed', fore_side='ventral', hind_side='dorsal' WHERE name='CAM000105v'").run();
  store.db.prepare("INSERT INTO photo_file_predictions(file_id,name,cam,view,species_json,imported_at) VALUES('f-d','CAM000105d','CAM000105','d',?,'now'),('f-v','CAM000105v','CAM000105','v',?,'now')").run(
    JSON.stringify([['Mechanitis polymnia', 0.7]]),
    JSON.stringify([['Mechanitis messenoides', 0.55]]),
  );
  bumpReviewRevision(store.db);
  const [a] = runPhotoFacts(store, { cams: ['CAM000105'] }).cams;
  assert.equal(a.gallery.species, 'Mechanitis polymnia 0.81, Mechanitis messenoides 0.1, Melinaea menophilus 0.05, Mechanitis lysimnia 0.02, Hypothyris anastasia 0.01');
  assert.deepEqual(a.gallery.sex, ['female', 0.71, 'not supported: below_confidence']);
  assert.deepEqual(a.gallery.photos, { CAM000105v: 'Mechanitis messenoides 0.55' });
  assert.match(a.wings.photos.CAM000105v, /^mixed [\d.]+ \(forewings ventral, hindwings dorsal\), 3 wings \(no hindwing left\)$/);
});

test('photo_facts without CAMs: the disagreements of the archive, the surest first, without those settled', async () => {
  const { store } = await fixture();
  const page = runPhotoFacts(store, {});
  assert.deepEqual(page.counts, { species: 0, sex: 1, gallery: 1, aisex: 0, camid: 2, wings: 3 });
  // The species the note says was changed is settled.
  assert.deepEqual(page.settled, { species: 1 });
  const byKind = Object.fromEntries(page.items.map(i => [`${i.kind}:${i.cam}`, i]));
  assert.deepEqual([byKind['sex:CAM000105'].envelope, byKind['sex:CAM000105'].workbook], ['male', 'female']);
  assert.deepEqual([byKind['gallery:CAM000110'].gallery, byKind['gallery:CAM000110'].conf], ['Hypothyris anastasia', 0.95]);
  assert.deepEqual([byKind['camid:CAM000101'].envelope, byKind['camid:CAM000101'].curation], ['CAM000111', 'file-name-wrong']);
  // CAM000102's envelope CAM was found a reading error by the curation, and read under 90 %: not listed.
  assert.equal(byKind['camid:CAM000102'], undefined);
  assert.match(byKind['wings:CAM000107'].wings, /upside down/);
  assert.equal(byKind['wings:CAM000105'].pair, 'suffix swapped');
  assert.ok(page.items.every((i, n) => n === 0 || page.items[n - 1].conf >= i.conf), 'ranked by confidence');
  assert.equal(runPhotoFacts(store, { kind: 'sex' }).items.length, 1);
  assert.equal(runPhotoFacts(store, { q: 'anastasia' }).total, 1);
  assert.equal(runPhotoFacts(store, { limit: 2 }).next, 'offset 2');
  assert.match(runPhotoFacts(store, { kind: 'colour' }).error, /^kind is one or more of/);
  // A person's verdict in Revisión settles one too.
  store.db.exec('CREATE TABLE IF NOT EXISTS issue_verdicts(id INTEGER PRIMARY KEY AUTOINCREMENT, issue_id TEXT NOT NULL, kind TEXT NOT NULL, verdict TEXT NOT NULL, value TEXT, comment TEXT, user_id TEXT NOT NULL, user_name TEXT NOT NULL, at TEXT NOT NULL, proposal_id TEXT, snapshot_json TEXT NOT NULL)');
  store.db.prepare("INSERT INTO issue_verdicts(issue_id,kind,verdict,user_id,user_name,at,snapshot_json) VALUES('envelope_sex:CAM000105:Collection_data','envelope_sex','rejected','u1','Ana','2026-10-17','{}')").run();
  assert.equal(runPhotoFacts(store, {}).counts.sex, 0);
});

test('a Photo_links reading lists the photos whose wings show the other side or that are turned', async () => {
  const { store } = await fixture();
  const fid = n => `drive-id-${String(n).padStart(20, '0')}`;
  const item = (id, name, parent, extra = {}) => [id, { id, name, parent, mimeType: 'image/jpeg', webViewLink: `https://drive.google.com/file/d/${id}/view`, size: '2048', ...extra }];
  const items = new Map([
    [fid(1), { id: fid(1), name: 'Photos', mimeType: FOLDER }],
    [fid(2), { id: fid(2), name: 'Wild', parent: fid(1), mimeType: FOLDER, webViewLink: `https://drive.google.com/drive/folders/${fid(2)}` }],
    item(ids.CAM000105d, 'CAM000105d.JPG', fid(2)),
    item(ids.CAM000105v, 'CAM000105v.JPG', fid(2)),
    item(ids.CAM000107d, 'CAM000107d.JPG', fid(2)),
    item('drive-id-new-00000000110d', 'CAM000110d.JPG', fid(2)),
  ]);
  const rows = [{ id: 'r2', row: 2, values: { 'Full Path': 'Wild', Name: 'Wild', Type: 'Folder', URL: `https://drive.google.com/drive/folders/${fid(2)}` } }];
  const { wingSides } = await import('../server/photodata.mjs');
  const plan = planPhotoLinks({ rows, items, home: fid(1), ids: new Set(['CAM000105', 'CAM000107', 'CAM000110']), wings: wingSides(store) });
  const w = plan.summary.wings;
  assert.equal(w.wingSide.count, 2);
  assert.deepEqual(w.wingSide.examples[0], { name: 'CAM000105d.JPG', in: 'Wild', url: `https://drive.google.com/file/d/${ids.CAM000105d}/view`, side: 'ventral', pair: 'suffix swapped' });
  assert.deepEqual(w.wingTurned.examples.map(e => [e.name, e.degrees]), [['CAM000107d.JPG', 180], ['CAM000110d.JPG', 180]]);
  // On the rows to write too, beside the other checks.
  const line = plan.display.find(l => l.name === 'CAM000107d.JPG');
  assert.ok(line.checks.some(c => c.kind === 'wingTurned'));
  // Renamed the right way round since: nothing to say.
  items.set(ids.CAM000105d, { ...items.get(ids.CAM000105d), name: 'CAM000105v.JPG' });
  items.set(ids.CAM000105v, { ...items.get(ids.CAM000105v), name: 'CAM000105d.JPG' });
  const again = planPhotoLinks({ rows, items, home: fid(1), ids: new Set(['CAM000105']), wings: wingSides(store) });
  assert.equal(again.summary.wings.wingSide.count, 0);
});
