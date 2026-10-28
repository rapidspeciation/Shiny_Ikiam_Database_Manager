# Data-quality audit, 28 Oct 2026. Read-only: the local copy of the app's database
# (~/.cache/ithomiini-lab/app-workbook/app.sqlite, records table, synced 28 Oct) and the app's own
# Revisión checks run on a throwaway copy (/tmp/audit28/issues.json, from tools: see REPORT.md).
# Writes findings.json and proposals/*.json (propose_changes arguments) here. Never touches the server.
import json, re, sqlite3, collections, datetime, difflib, unicodedata, os, sys
os.chdir(os.path.dirname(os.path.abspath(__file__)))

SRC = os.path.expanduser('~/.cache/ithomiini-lab/app-workbook/app.sqlite')
db = sqlite3.connect(f'file:{SRC}?mode=ro', uri=True)
EPOCH = datetime.date(1899, 12, 30)
TODAY = datetime.date(2026, 10, 28)
iso = lambda s: (EPOCH + datetime.timedelta(days=round(s))).isoformat()
serial = lambda d: (d - EPOCH).days

def rows(sheet, observed_only=True):
    q = 'select id,row_num,observed,label,values_json,formulas_json from records where sheet=? and missing=0'
    out = []
    for i, r, o, lab, v, f in db.execute(q, (sheet,)):
        if observed_only and not o:
            continue
        out.append({'id': i, 'row': r, 'label': lab, 'v': json.loads(v), 'f': json.loads(f)})
    return out

COL, INS = rows('Collection_data'), rows('Insectary_data')
PHOTO = rows('Photo_links')
TAX = rows('Taxonomy_v18Jun25')
OPTS = {}
sdb = sqlite3.connect('file:' + os.path.expanduser('~/.cache/ithomiini-lab/app-workbook/sheets.sqlite') + '?mode=ro', uri=True)
for sh, col, strict, lst in sdb.execute("select sheet,column,strict,list from _columns where list is not null"):
    OPTS[(sh, col)] = (bool(strict), json.loads(lst))

T = lambda x: '' if x is None else str(x)
findings = []
proposals = collections.defaultdict(list)  # file -> changes

def ex(sheet, r, field, value, extra=None):
    d = {'sheet': sheet, 'row': r['row'], 'id': r['label'], 'field': field, 'value': value}
    if extra:
        d.update(extra)
    return d

def add(rule, title, items, confidence, fix, covered=None, examples=None):
    findings.append({'rule': rule, 'title': title, 'count': len(items), 'confidence': confidence, 'fix': fix,
                     'covered': covered, 'examples': (examples or items)[:3], 'all': items})

def propose(fname, r, values, note):
    if any(r['f'].get(k) for k in values):
        return False  # never write formula cells
    proposals[fname].append({'recordId': r['id'], 'values': values, 'note': note, '_sheet_row': r['row'], '_label': r['label']})
    return True

SHEETS = {'Collection_data': COL, 'Insectary_data': INS}

# ---------------------------------------------------------------- 1. whitespace in typed text cells
ws, ws_person = [], []
RAW_LIST = {T(x) for r in rows('Lists') for x in r['v'].values() if isinstance(x, str)}
RAW_LIST |= {T(r['v'].get('Collection_location')) for r in rows('Location_data')}  # the site dropdown's source
for sh, rs in SHEETS.items():
    for r in rs:
        for k, val in r['v'].items():
            if not isinstance(val, str) or r['f'].get(k) or k.startswith('Notes') or k == 'NOTES':
                continue
            fixed = re.sub(r'\s+', ' ', val.replace('\u2060', '').replace('\u00a0', ' ')).strip()
            if fixed == val or not fixed:
                continue
            opt = OPTS.get((sh, k))
            if val in RAW_LIST:
                # The Lists sheet itself has the space: the dropdown would refuse the trimmed value.
                ws_person.append(ex(sh, r, k, val, {'fixed': fixed, 'why': 'the dropdown source (Lists / Location_data) has the same space; trim it there first'}))
            elif opt and fixed not in opt[1]:
                ws_person.append(ex(sh, r, k, val, {'fixed': fixed, 'why': 'trimmed value is not in the dropdown either'}))
            elif fixed.upper() in ('BUSCAR',):
                ws_person.append(ex(sh, r, k, val, {'why': 'placeholder text, not an ID'}))
            else:
                ws.append(ex(sh, r, k, val, {'fixed': fixed}))
                propose('01_whitespace.json', r, {k: fixed}, f'Espacios de más: «{val}» → «{fixed}»')
add('whitespace', 'Leading/trailing/double spaces in typed cells (notes excluded)', ws, 'safe',
    'Trim and collapse spaces.')
add('whitespace_list', 'Spaces that come from the dropdown source itself (Lists: "ABV - Alejandra Barros Veliz ", "Abundance estimation ") or whose trimmed value is still off-list ("Apuya Y ")', ws_person, 'needs a person',
    'Trim the entry in Lists first (sheet owner), then the cells; "Apuya Y" needs its Location_data row.')

# ---------------------------------------------------------------- 2. IDs: shape and case
camre = re.compile(r'^CAM\d{6}$')
bad_cam = []
for sh, rs in SHEETS.items():
    for r in rs:
        for k in ('CAM_ID', 'CAM_ID_insectary', 'CAM_ID_CollData'):
            val = r['v'].get(k)
            if r['f'].get(k) or val in (None, '', 'NA', 'NOT_COLLECTED'):
                continue
            s = str(val)
            # other projects' numbering (ME22_…, FS…, LTS…, M584…) and "not given" are not CAM attempts
            if not camre.match(s) and (re.search(r'cam', s, re.I) or re.fullmatch(r'\s*\d{5,7}\s*', s)):
                fixed = re.sub(r'\s+', '', s).upper()
                fixed = fixed if camre.match(fixed) else None
                bad_cam.append(ex(sh, r, k, s, {'fixed': fixed}))
                if fixed:
                    propose('02_cam_format.json', r, {k: fixed}, f'CAM con formato irregular: «{s}» → {fixed}')
add('cam_format', 'CAM IDs not shaped CAM + 6 digits (typed cells)', bad_cam, 'safe where an obvious fix exists, else needs a person',
    'Upper-case / strip spaces when that yields CAM+6 digits; otherwise check the envelope or tube.')

ins_ids = [ex('Insectary_data', r, 'Insectary_ID', r['v'].get('Insectary_ID')) for r in INS
           if isinstance(r['v'].get('Insectary_ID'), str) and re.search(r'\s|[a-z]', r['v']['Insectary_ID'])]
add('insectary_id_shape', 'Insectary IDs with spaces or lower case (e.g. "T 1")', ins_ids, 'needs a person',
    'Was 18 in data-entry-audit.md (27 Sep), still unfixed. The wing label decides the right form; renaming an ID touches crosses and clutches that cite it.',
    covered='data-entry-audit.md §2.7 (still unfixed)')

# Duplicates: CAM within and across main sheets (beyond the app's "repeat"/"cam_cross" kinds, just counted)
cams = collections.defaultdict(list)
for sh, rs in SHEETS.items():
    for r in rs:
        c = r['v'].get('CAM_ID')
        if isinstance(c, str) and camre.match(c.strip()):
            cams[c.strip()].append((sh, r))
dup_within = []
for c, lst in cams.items():
    for sh in SHEETS:
        same = [r for s, r in lst if s == sh]
        if len(same) > 1:
            dup_within.append({'CAM': c, 'sheet': sh, 'rows': [r['row'] for r in same],
                               'species': [T(r['v'].get('SPECIES')) for r in same]})
add('cam_dup_within', 'Same CAM_ID on two or more rows of one sheet', dup_within, 'needs a person',
    'Check the envelope/tube of each row; one row gets a new CAM. The app lists these under «ID o tubo repetido».',
    covered="app Revisión 'repeat' (open)")

insd = collections.defaultdict(list)
for r in INS:
    i = r['v'].get('Insectary_ID')
    if isinstance(i, str) and i.strip() and i.strip() != 'NA':
        insd[i.strip().upper()].append(r)
dup_ins = [{'Insectary_ID': k, 'rows': [r['row'] for r in v], 'species': [T(r['v'].get('SPECIES')) for r in v],
            'intro': [iso(r['v']['Intro2Insectary_date']) if isinstance(r['v'].get('Intro2Insectary_date'), (int, float)) else T(r['v'].get('Intro2Insectary_date')) for r in v]}
           for k, v in insd.items() if len(v) > 1]
add('insectary_id_dup', 'Same Insectary_ID on two or more observed rows', dup_ins, 'needs a person',
    'Most are older formats reused across years (docs: 95 in Sep). Give the later one a ".1" suffix only after checking the wing label.',
    covered='database-findings.md / data-entry-audit.md §2.7 (still unfixed); app Revisión repeat')

# ---------------------------------------------------------------- 3. dates
DATE_COLS = {'Collection_data': ['Collection_date', 'Death_date', 'Preservation_date', 'DATE_OF_COLLECTION'],
             'Insectary_data': ['Intro2Insectary_date', 'Death_date', 'Preservation_date']}
text_dates, old_dates = [], []
for sh, cols in DATE_COLS.items():
    for r in SHEETS[sh]:
        for k in cols:
            val = r['v'].get(k)
            if r['f'].get(k) or val in (None, '', 'NA'):
                continue
            if isinstance(val, str):
                text_dates.append(ex(sh, r, k, val))
            elif isinstance(val, (int, float)) and val < serial(datetime.date(2015, 1, 1)):
                old_dates.append(ex(sh, r, k, iso(val)))
DUTCH = {'jan': 1, 'feb': 2, 'mrt': 3, 'apr': 4, 'mei': 5, 'jun': 6, 'jul': 7, 'aug': 8, 'sep': 9, 'okt': 10, 'nov': 11, 'dec': 12}
for e in text_dates:
    r = next(x for x in SHEETS[e['sheet']] if x['row'] == e['row'])
    m = re.fullmatch(r'(\d{1,2})-([a-z]{3})-(\d{2})', e['value'].strip())
    if m and m.group(2) in DUTCH:
        d = datetime.date(2000 + int(m.group(3)), DUTCH[m.group(2)], int(m.group(1)))
        cd = r['v'].get('Collection_date')
        if isinstance(cd, (int, float)) and serial(d) >= cd and d <= TODAY:
            e['fixed'] = d.isoformat()
            propose('03_dates_text.json', r, {e['field']: d.isoformat()},
                    f'Fecha escrita como texto con mes en neerlandés («{e["value"]}», mrt = marzo) → {d.isoformat()}')
    elif e['value'].strip().strip('`').upper() == 'NA':
        e['fixed'] = 'NA'
        propose('03_dates_text.json', r, {e['field']: 'NA'}, f'«{e["value"]}» → NA')
add('date_text', 'Text instead of a date in a date column (mixed formats)', text_dates, 'safe (53), needs a person (1)',
    '53 of 54 are unambiguous (Dutch "9-mrt-26" = 9 Mar 2026, "na", "NA`"): safe. "9/30" has no year: needs a person.',
    covered="app Revisión 'bad_date' (open)")
add('date_too_old', 'Dates before 2015 (the project collects from 2021/22)', old_dates, 'needs a person',
    'Usually a year slip (2002 for 2022); check the envelope/notebook.')

# day/month swap: the value is far from its neighbours but the swapped date is close
swaps = []
for sh, cols in DATE_COLS.items():
    rs = sorted(SHEETS[sh], key=lambda r: r['row'])
    for k in cols:
        vals = [(r, r['v'].get(k)) for r in rs]
        for i, (r, val) in enumerate(vals):
            if r['f'].get(k) or not isinstance(val, (int, float)) or val <= 0:
                continue
            if k in ('Death_date', 'Preservation_date') and T(r['v'].get('Death_cause')) == 'Disappearance':
                continue  # a cage-count date, far from its neighbours by nature
            d = EPOCH + datetime.timedelta(days=round(val))
            if d.day > 12 or d.day == d.month:
                continue
            nb = [v for _, v in vals[max(0, i - 6):i] + vals[i + 1:i + 7] if isinstance(v, (int, float)) and v > 0]
            if len(nb) < 4:
                continue
            nb.sort(); med = nb[len(nb) // 2]
            try:
                sw = serial(datetime.date(d.year, d.day, d.month))
            except ValueError:
                continue
            if abs(val - med) > 45 and abs(sw - med) <= 10:
                swaps.append(ex(sh, r, k, d.isoformat(), {'swapped': iso(sw), 'neighbours_median': iso(med)}))
add('date_swap', 'Probable day/month swap (date far from neighbouring rows; swapped date fits them)', swaps, 'needs a person',
    'Check the notebook; if confirmed, write the swapped date.')

# order checks not in the app: preservation before death; death long after intro (lifespan)
pre_before_death, long_life, death_no_cause, cause_no_date, disap_late = [], [], [], [], []
for sh, rs in SHEETS.items():
    for r in rs:
        v = r['v']; dd, pd = v.get('Death_date'), v.get('Preservation_date')
        if isinstance(dd, (int, float)) and isinstance(pd, (int, float)) and pd < dd:
            pre_before_death.append(ex(sh, r, 'Preservation_date', iso(pd), {'Death_date': iso(dd)}))
for r in INS:
    v = r['v']; dd, it = v.get('Death_date'), v.get('Intro2Insectary_date')
    if isinstance(dd, (int, float)) and isinstance(it, (int, float)) and dd - it > 180 and T(v.get('Death_cause')) == 'Disappearance':
        disap_late.append(ex('Insectary_data', r, 'Death_date', iso(dd), {'Intro2Insectary_date': iso(it), 'days': round(dd - it)}))
    elif isinstance(dd, (int, float)) and isinstance(it, (int, float)) and dd - it > 180:
        long_life.append(ex('Insectary_data', r, 'Death_date', iso(dd), {'Intro2Insectary_date': iso(it), 'days': round(dd - it)}))
    dc = T(v.get('Death_cause')).strip()
    has_date = isinstance(dd, (int, float)) or (isinstance(dd, str) and dd.strip() not in ('', 'NA'))
    if has_date and not dc and not r['f'].get('Death_cause'):
        death_no_cause.append(ex('Insectary_data', r, 'Death_cause', '', {'Death_date': iso(dd) if isinstance(dd, (int, float)) else dd}))
    if dc and dc not in ('NA',) and not has_date and not r['f'].get('Death_date'):
        cause_no_date.append(ex('Insectary_data', r, 'Death_date', '', {'Death_cause': dc}))
add('preserved_before_death', 'Preservation_date earlier than Death_date', pre_before_death, 'needs a person',
    'One of the two dates is wrong (team rule: preservation = death day in 98.5 %).',
    covered="partly app 'date_order' (Insectary Preservation_date vs intro); not this pair")
add('long_insectary_life', 'Adult alive > 180 days in the insectary (Death_date - Intro2Insectary_date)', long_life, 'needs a person',
    'Usually a year slip in one of the dates; check the notebook.')
add('disappearance_late', 'Disappearance recorded > 180 days after Intro2Insectary_date (cage-count date, not a death date)', disap_late, 'needs a person',
    'Not fixable row by row: the butterfly vanished earlier. Treat Death_date of Disappearance as "last known after" in analyses; a team rule could add the last-seen date.')
add('death_without_cause', 'Insectary row with a Death_date but no Death_cause', death_no_cause, 'needs a person',
    'Add the cause from the daily-round notes; "Unknown" if nothing is known.')
add('cause_without_date', 'Insectary row with a Death_cause but no Death_date', cause_no_date, 'needs a person',
    'Add the death date (notes often give it).')

# ---------------------------------------------------------------- 4. death cause
allowed_dc = set(OPTS[('Insectary_data', 'Death_cause')][1])
bad_dc = []
for r in INS:
    val = r['v'].get('Death_cause')
    if val in (None, '') or r['f'].get('Death_cause'):
        continue
    s = str(val)
    if s not in allowed_dc:
        m = [a for a in allowed_dc if a.lower() == s.strip().lower()]
        bad_dc.append(ex('Insectary_data', r, 'Death_cause', s if not isinstance(val, (int, float)) else f'{val} (= {iso(val)})', {'fixed': m[0] if m else None}))
        if m:
            propose('04_death_cause.json', r, {'Death_cause': m[0]}, f'Causa de muerte escrita distinto: «{s}» → {m[0]}')
add('death_cause_list', 'Death_cause outside the sheet list', bad_dc, 'needs a person (a date typed in the cause column)',
    'Move the date to Death_date if that is empty, and ask for the cause.')

# ---------------------------------------------------------------- 5. sex
sex_spell = []
for r in COL:
    val = r['v'].get('Sex')
    if isinstance(val, str) and re.fullmatch(r'(male|female)\s*_?\s*\?', val.strip()) and val not in ('male ?', 'female ?'):
        fixed = val.strip().split('_')[0].replace('?', '').strip() + ' ?'
        sex_spell.append(ex('Collection_data', r, 'Sex', val, {'fixed': fixed}))
        propose('05_sex_spelling.json', r, {'Sex': fixed}, f'Sexo dudoso escrito distinto: «{val}» → «{fixed}» (valor de la lista)')
add('sex_spelling', 'Doubtful sex spelled "female_?", "male_?", "male?" instead of the list values "female ?"/"male ?"', sex_spell, 'safe',
    'Write the list spelling. (Whether the doubt should become its own column is a separate team decision, data-entry-audit §2.8.)',
    covered="app Revisión 'list' (open); data-entry-audit.md §2.8")
sex_blank = []
for sh, rs in SHEETS.items():
    for r in rs:
        val = r['v'].get('Sex')
        if (val is None or str(val).strip() == '') and not r['f'].get('Sex') and T(r['v'].get('SPECIES')).strip():
            sex_blank.append(ex(sh, r, 'Sex', '', {'SPECIES': r['v'].get('SPECIES')}))
add('sex_blank', 'Specimen with a species but an empty Sex cell (lists say NA / NOT_COLLECTED for "not known")', sex_blank, 'needs a person',
    'Fill from the envelope or photo; NOT_COLLECTED when it cannot be known.')

# Methona confusa pre-15 May 2023
meth = []
cut = serial(datetime.date(2023, 5, 15))
for sh, rs in SHEETS.items():
    dk = 'Collection_date' if sh == 'Collection_data' else 'Intro2Insectary_date'
    for r in rs:
        if not T(r['v'].get('SPECIES')).lower().startswith('methona confusa'):
            continue
        d = r['v'].get(dk)
        if isinstance(d, (int, float)) and d < cut and T(r['v'].get('Sex')).startswith('male'):
            notes = T(r['v'].get('Notes_Collection_data') or r['v'].get('Notes_Insectary_data'))
            meth.append(ex(sh, r, 'Sex', r['v'].get('Sex'), {'date': iso(d), 'genitalia_note': bool(re.search('genital', notes, re.I))}))
add('methona_sex', 'Methona confusa sexed "male" before 15 May 2023 (hair-pencil rule; females have hair pencils too)', meth, 'needs a person',
    'Do not overwrite blindly: set Sex to "male ?" (or keep and add a note) unless genitalia are checked; see WingsClassificator reports/sex_label_review_20261027/AI_REVIEW_AND_METHONA.md. Prepared as a separate, unposted draft.')
for r in [x for x in COL if x['label'] in {m['id'] for m in meth if m['sheet'] == 'Collection_data'}]:
    if r['v'].get('Sex') == 'male' and r['v'].get('Collection_date', 0) < cut:
        propose('90_methona_sex_DRAFT_needs_person.json', r, {'Sex': 'male ?', 'Notes_Collection_data': 'Sexo «male» por la regla de pelos (hair pencils) antes del 15 may 2023; las hembras de M. confusa también los tienen. Sin revisar genitales.'},
                'Methona confusa antes del 15 may 2023: sexo no comprobado')

# ---------------------------------------------------------------- 6. taxonomy
tax_species = {}
tax_genera = set()
for r in TAX:
    s = T(r['v'].get('species')).strip()
    if s and s != 'NA':
        tax_species[s] = r['v']
    g = T(r['v'].get('genus')).strip()
    if g:
        tax_genera.add(g)
tax_lower = {k.lower(): k for k in tax_species}
strict_species = set(OPTS[('Collection_data', 'SPECIES')][1])

sp_case, sp_unknown = [], []
for sh, rs in SHEETS.items():
    for r in rs:
        val = r['v'].get('SPECIES')
        if not isinstance(val, str) or not val.strip() or val.strip() in ('NA', 'NOT_COLLECTED') or r['f'].get('SPECIES'):
            continue
        s = re.sub(r'\s+', ' ', val).strip()
        if sh == 'Insectary_data':
            # "Genus species subsp", hybrids "a x b", crosses "A VS B": check every binomial inside
            parts = re.split(r'\s+(?:x|X|VS|vs)\s+', s)
            bins = []
            for p in parts:
                w = p.split()
                if len(w) >= 2 and w[0][:1].isupper():
                    bins.append(' '.join(w[:2]))
            bad = [b for b in bins if b not in tax_species]
            if bad == ['Heliconus numata']:
                fixed = s.replace('Heliconus numata', 'Heliconius numata')
                sp_case.append(ex(sh, r, 'SPECIES', val, {'fixed': fixed}))
                propose('06_species_case.json', r, {'SPECIES': fixed}, f'Género mal escrito: «{val}» → {fixed}')
                continue
            if bad:
                fx = [tax_lower.get(b.lower()) for b in bad]
                sp_unknown.append(ex(sh, r, 'SPECIES', val, {'not_in_taxonomy': bad, 'case_match': fx}))
            continue
        if s in tax_species or s in strict_species:
            continue
        if s.lower() in tax_lower:
            fixed = tax_lower[s.lower()]
            sp_case.append(ex(sh, r, 'SPECIES', val, {'fixed': fixed}))
            propose('06_species_case.json', r, {'SPECIES': fixed}, f'Especie con mayúsculas distintas a Taxonomy_v18Jun25: «{val}» → {fixed}')
        elif s in ('Riodinidae', 'Nymphalidae', 'Lycaenidae') and f'{s} (family)' in strict_species:
            sp_case.append(ex(sh, r, 'SPECIES', val, {'fixed': f'{s} (family)'}))
            propose('06_species_case.json', r, {'SPECIES': f'{s} (family)'}, f'Familia escrita sin «(family)», como en la lista: «{val}» → {s} (family)')
        elif s in ('Perrybris pamela', 'Heliconus numata'):
            fixed = {'Perrybris pamela': 'Perrhybris pamela', 'Heliconus numata': 'Heliconius numata'}[s]
            sp_case.append(ex(sh, r, 'SPECIES', val, {'fixed': fixed}))
            propose('06_species_case.json', r, {'SPECIES': fixed}, f'Género mal escrito: «{val}» → {fixed}')
        else:
            close = difflib.get_close_matches(s, list(tax_species), n=1, cutoff=0.9)
            sp_unknown.append(ex(sh, r, 'SPECIES', val, {'closest': close[0] if close else None}))
add('species_case', 'SPECIES typos with one obvious taxonomy name (genus misspelt; family written without "(family)")', sp_case, 'safe', 'Write the taxonomy / dropdown spelling.')
add('species_not_in_taxonomy', 'SPECIES (or a binomial inside an Insectary hybrid/cross name) not in Taxonomy_v18Jun25', sp_unknown, 'needs a person',
    'Typos get the taxonomy spelling; names missing from the taxonomy (new records, synonyms) go to whoever keeps Taxonomy_v18Jun25.',
    covered="Collection_data part also in app Revisión 'list' (open)")

# Genus column (formula from SPECIES) giving NOT_FOUND
nf = [ex('Collection_data', r, 'Genus', r['v'].get('Genus'), {'SPECIES': r['v'].get('SPECIES')}) for r in COL if T(r['v'].get('Genus')) == 'NOT_FOUND']
add('genus_not_found', 'Collection rows whose Family/Genus formulas give NOT_FOUND (SPECIES not in the taxonomy)', nf, 'needs a person', 'Fix the SPECIES spelling or add the name to the taxonomy.')

# Subspecies/form: variants of the same name within a species
forms = collections.defaultdict(collections.Counter)
form_rows = collections.defaultdict(list)
for r in COL:
    sp, fm = T(r['v'].get('SPECIES')).strip(), r['v'].get('Subspecies_Form')
    if not sp or not isinstance(fm, str) or not fm.strip() or r['f'].get('Subspecies_Form'):
        continue
    forms[sp][fm] += 1
    form_rows[(sp, fm)].append(r)
def norm(s):
    s = unicodedata.normalize('NFKD', s).encode('ascii', 'ignore').decode().lower()
    return re.sub(r'[\s_\-]+', ' ', s).strip()
form_case, form_typo = [], []
for sp, cnt in forms.items():
    names = list(cnt)
    for a in names:
        for b in names:
            if a == b or cnt[b] < cnt[a] or (cnt[b] == cnt[a] and b < a):
                continue
            # a is the rarer spelling, b the more common one
            if norm(a) == norm(b):
                for r in form_rows[(sp, a)]:
                    form_case.append(ex('Collection_data', r, 'Subspecies_Form', a, {'SPECIES': sp, 'fixed': b, 'n_common': cnt[b]}))
                    propose('07_subspecies_variants.json', r, {'Subspecies_Form': b}, f'Forma escrita distinto a la habitual en {sp}: «{a}» → «{b}» ({cnt[b]} filas)')
            elif cnt[b] >= 3 and cnt[a] <= 3 and len(norm(a)) > 4 and difflib.SequenceMatcher(None, norm(a), norm(b)).ratio() >= 0.85:
                form_typo.append({'SPECIES': sp, 'value': a, 'n': cnt[a], 'likely': b, 'n_likely': cnt[b],
                                  'rows': [r['row'] for r in form_rows[(sp, a)]][:10]})
SAFE_FORM = {('Mechanitis messenoides', 'messnoides'), ('Callithomia lenea', 'metonella'), ('Mechanitis polymnia', 'euridice'),
             ('Ithomia agnosia', 'angosia'), ('Brevioleria arzalia', 'lorenia'), ('Dismorphia theucharila', 'leuconae'),
             ('Lycorea halia', 'atergalis'), ('Hesperiidae (family)', 'eudamine')}
for t in form_typo:
    t['safe'] = (t['SPECIES'], t['value']) in SAFE_FORM
    if t['safe']:
        for r in form_rows[(t['SPECIES'], t['value'])]:
            propose('07_subspecies_variants.json', r, {'Subspecies_Form': t['likely']},
                    f'Forma con una letra cambiada en {t["SPECIES"]}: «{t["value"]}» → «{t["likely"]}» ({t["n_likely"]} filas)')
add('subspecies_variant', 'Subspecies_Form differing from the common spelling of the same species only by case, accents, spaces or "_"', form_case, 'safe',
    'Write the common spelling.')
add('subspecies_typo', 'Rare Subspecies_Form spellings close to a common one of the same species (typos)', form_typo, 'safe (8 one-letter typos), needs a person (3)',
    'One-letter typos get the common spelling; "f. intermedia" vs "intermedia" is a naming convention and "berolla" vs "beronilla" is not one letter: a person decides.')

# Insectary SPECIES (non-strict list): case/space variants of list values
ins_list = OPTS[('Insectary_data', 'SPECIES')][1]
ins_lower = {norm(x): x for x in ins_list}
ins_var = []
for r in INS:
    val = r['v'].get('SPECIES')
    if isinstance(val, str) and val not in ins_list and not r['f'].get('SPECIES') and norm(val) in ins_lower and val.strip() != val:
        pass  # whitespace-only handled by rule 1
    if isinstance(val, str) and val.strip() == re.sub(r'\s+', ' ', val).strip() and val not in ins_list and norm(val) in ins_lower and not r['f'].get('SPECIES'):
        fixed = ins_lower[norm(val)]
        ins_var.append(ex('Insectary_data', r, 'SPECIES', val, {'fixed': fixed}))
        propose('08_insectary_species_case.json', r, {'SPECIES': fixed}, f'Especie escrita distinto a la lista del insectario: «{val}» → «{fixed}»')
add('insectary_species_variant', 'Insectary SPECIES differing from its dropdown value only by case/accents', ins_var, 'safe', 'Write the list value.')

# ---------------------------------------------------------------- 7. photos
all_cams = set(cams)
for r in rs:
    pass
wing_cams = set()
for sh in ('Wing_tissue', 'CRISPR', 'Barcoding_DNA', 'Pheromones_data', 'Sperm_dissections', 'Life_History', 'F1/F2_MutationRate', 'ME_TempData'):
    for r in rows(sh):
        for x in r['v'].values():
            if isinstance(x, str):
                wing_cams.update(m.upper() for m in re.findall(r'CAM\d{6}', x, re.I))
lists_cams = {x for r in rows('Lists') for x in r['v'].values() if isinstance(x, str) and camre.match(x)}
ph_cols = list(PHOTO[0]['v'].keys()) if PHOTO else []
ph_cam_key = next((k for k in ph_cols if k.upper().startswith('CAM')), None)
photo_orphans = collections.defaultdict(list)
photo_cams = set()
for r in PHOTO:
    if r['v'].get('Type') != 'File':
        continue
    m = re.match(r'(CAM\d{6})', T(r['v'].get('Name')).strip(), re.I)
    if not m:
        continue
    c = m.group(1).upper()
    photo_cams.add(c)
    if camre.match(c) and c not in all_cams and c not in wing_cams:
        photo_orphans[c].append(r)
orph = [{'CAM': c, 'photos': len(v), 'file': T(v[0]['v'].get('Full Path')), 'in_Lists_pool': c in lists_cams} for c, v in sorted(photo_orphans.items())]
add('photo_orphan', 'Photo_links CAMs with no specimen row in any sheet (Collection, Insectary, Wing_tissue, CRISPR, Barcoding, Pheromones, ...)', orph, 'needs a person',
    'Either the specimen row is missing (type it from the envelope) or the photo file name has a wrong CAM.',
    covered="partly app Revisión 'photo_camid'")
odd_names_ = [{'file': T(r['v'].get('Full Path'))} for r in PHOTO if r['v'].get('Type') == 'File'
             and re.search(r'^cam', T(r['v'].get('Name')), re.I) and not re.match(r'CAM\d{6}(?:[dv]|[_-]|\.|\s|$)', T(r['v'].get('Name')))]
odd_names = odd_names_
add('photo_name_shape', 'Photo files named "CAMO7…" (letter O for zero) or otherwise not CAM+6 digits', odd_names, 'safe, but a Drive rename (organize_photos), not a sheet edit',
    'Rename CAMO→CAM0 in Drive with the assistant\'s organize_photos (its own card); Photo_links follows on the next sync. Not part of the sheet proposals.')

# ---------------------------------------------------------------- 8. other
# "Alive" preservation with a death date (docs say 5,211 / 1,798 in Sep)
alive = {sh: [ex(sh, r, 'Preserved', 'Alive') for r in rs if T(r['v'].get('Preserved_dead_alive') or r['v'].get('Preserved_Dead_Alive')) == 'Alive' and isinstance(r['v'].get('Death_date'), (int, float))]
         for sh, rs in SHEETS.items()}
add('alive_with_death', '"Alive" in Preserved_dead_alive with a Death_date', alive['Collection_data'] + alive['Insectary_data'], 'needs a person',
    'Meaning of "Alive" (preserved while alive) differs from "still alive"; a team rule is needed before any change.',
    covered='database-findings.md (still unfixed; counts given per sheet below)')
# Collection ↔ insectary wild butterflies: link mismatch counted by the app; just referenced.

json.dump({'generated': TODAY.isoformat(), 'source': SRC, 'findings': findings,
           'alive_by_sheet': {k: len(v) for k, v in alive.items()}}, open('findings.json', 'w'), ensure_ascii=False, indent=1, default=str)

os.makedirs('proposals', exist_ok=True)  # files are overwritten, never deleted
ALL = {sh: rows(sh, observed_only=False) for sh in SHEETS}
REC = {r['id']: (sh, r) for sh, rs in ALL.items() for r in rs}
KEYCOLS = ['CAM_ID', 'Insectary_ID', 'FieldMark_ID', 'Tube_1_id', 'Tube_2_id', 'CAM_ID_insectary', 'Collection_location', 'Collector', 'SPECIES', 'Subspecies_Form', 'Sex']
idk = lambda v: str(v if v is not None else '').strip().lower()
def portable_key(rid, values):
    # {column: value} that picks this one row in the sheet, with the old value of each changed text cell in it,
    # so the change only applies where the cell still holds what was audited (the lab copy is not the team's workbook).
    sh, r = REC[rid]
    key = {k: r['v'][k] for k in values if isinstance(r['v'].get(k), str) and not k.startswith('Notes')}
    for col in KEYCOLS + [None]:
        if col and (col in key or idk(r['v'].get(col)) in ('', 'na') or r['v'].get(col) is None or not isinstance(r['v'].get(col), str)):
            continue
        if col:
            key[col] = r['v'][col]
        if key:
            hits = [x for x in ALL[sh] if all(idk(x['v'].get(k)) == idk(v) for k, v in key.items())]
            obs = [x for x in hits if x['id'] in {y['id'] for y in SHEETS[sh]}]
            if len(hits) == 1 or (len(hits) > 1 and len(obs) == 1 and obs[0]['id'] == rid):
                return sh, key
    return sh, None
REASON = {
    '01_whitespace.json': 'Auditoría 28 oct: espacios de más en celdas escritas',
    '03_dates_text.json': 'Auditoría 28 oct: fechas escritas como texto (mes en neerlandés «mrt» = marzo)',
    '02_cam_format.json': 'Auditoría 28 oct: CAM con formato irregular',
    '04_death_cause.json': 'Auditoría 28 oct: causa de muerte escrita distinto a la lista',
    '05_sex_spelling.json': 'Auditoría 28 oct: sexo dudoso escrito como en la lista',
    '06_species_case.json': 'Auditoría 28 oct: especie con mayúsculas de la taxonomía',
    '07_subspecies_variants.json': 'Auditoría 28 oct: formas escritas como la variante habitual',
    '08_insectary_species_case.json': 'Auditoría 28 oct: especie del insectario como en la lista',
    '90_methona_sex_DRAFT_needs_person.json': 'BORRADOR (decide una persona): Methona confusa antes del 15 may 2023, sexo no comprobado',
}
summary = {}
# A cell proposed by two rules (e.g. a trailing space in a typo'd form) goes in the first file only, its notes joined.
first = {}
for fname in sorted(proposals):
    keep = []
    for c in proposals[fname]:
        dup = [k for k in c['values'] if (c['recordId'], k) in first]
        for k in dup:
            prev = first[(c['recordId'], k)]
            if fname.startswith('9'):
                continue
            prev['note'] += ' · ' + c['note']
            if not prev['fname'].startswith('01'):
                continue
            prev['values'][k] = c['values'][k]  # the later rule's value already includes the trim
        rest = {k: v for k, v in c['values'].items() if k not in dup or fname.startswith('9')}
        if rest:
            c = {**c, 'values': rest, 'fname': fname}
            keep.append(c)
            for k in rest:
                first.setdefault((c['recordId'], k), c)
    proposals[fname] = keep
proposals = {k: v for k, v in proposals.items() if v}
for fname, changes in sorted(proposals.items()):
    # one change per record: merge values of the same record
    merged = {}
    for c in changes:
        m = merged.setdefault(c['recordId'], {'recordId': c['recordId'], 'values': {}, 'note': [], '_sheet_row': c['_sheet_row'], '_label': c['_label']})
        m['values'].update(c['values']); m['note'].append(c['note'])
    out, unkeyed = [], 0
    for m in merged.values():
        sh, key = portable_key(m['recordId'], m['values'])
        note = ' · '.join(m['note']) + f' (fila {m["_sheet_row"]} en la copia del 28 oct)'
        if key:
            out.append({'sheet': sh, 'key': key, 'values': m['values'], 'note': note})
        else:
            unkeyed += 1
            out.append({'sheet': sh, 'id': m['_label'], 'values': m['values'], 'note': note + ' · SIN CLAVE ÚNICA: elegir la fila a mano'})
    if unkeyed:
        print('unkeyed', fname, unkeyed)
    chunks = [out[i:i + 500] for i in range(0, len(out), 500)]
    for n, ch in enumerate(chunks, 1):
        name = fname if len(chunks) == 1 else fname.replace('.json', f'_part{n}.json')
        json.dump({'reason': REASON[fname] + (f' ({n}/{len(chunks)})' if len(chunks) > 1 else ''), 'changes': ch},
                  open(os.path.join('proposals', name), 'w'), ensure_ascii=False, indent=1)
        summary[name] = len(ch)
for f in findings:
    print(f"{f['count']:6d}  {f['confidence'][:14]:14s} {f['rule']}")
print(summary)
