# Read-only: copies the observed rows of a few sheets from the local app.sqlite cache into work.db (flat tables).
import sqlite3, json, sys
SRC = '/home/franz/.cache/ithomiini-lab/app-workbook/app.sqlite'
src = sqlite3.connect(f'file:{SRC}?mode=ro', uri=True)
dst = sqlite3.connect('work.db'); 
SHEETS = ['Collection_data','Insectary_data','Photo_links','Taxonomy_v18Jun25','Lists','Wing_tissue','Pheromones_data','Insectary_stocks']
for sh in SHEETS:
    rows = src.execute("select id,row_num,observed,values_json from records where sheet=? and missing=0", (sh,)).fetchall()
    cols = []
    for r in rows:
        for k in json.loads(r[3]).keys():
            if k not in cols: cols.append(k)
    t = sh
    seen=set(); names=[]
    for c in cols:
        n=c
        while n.lower() in seen: n=n+'_U'
        seen.add(n.lower()); names.append(n)
    dst.execute(f'drop table if exists "{t}"')
    dst.execute(f'create table "{t}" (_id text, _row int, _obs int, ' + ','.join(f'"{c}"' for c in names) + ')')
    for r in rows:
        v = json.loads(r[3])
        dst.execute(f'insert into "{t}" values ({",".join("?"*(3+len(cols)))})', [r[0], r[1], r[2]] + [json.dumps(v.get(c)) if isinstance(v.get(c),(dict,list)) else v.get(c) for c in cols])
    print(sh, len(rows), len(cols))
dst.commit()
