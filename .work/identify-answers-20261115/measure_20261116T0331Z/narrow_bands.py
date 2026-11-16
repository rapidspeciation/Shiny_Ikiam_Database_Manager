"""Narrow 0.05 species bands, from results.json's cumulative grid (species named at score >= t): band [t, t+0.05) =
n(t) - n(t+0.05), k(t) - k(t+0.05). Written by the lead for PREREG D5 (16 Nov 2026). Usage: python3 narrow_bands.py results.json"""
import json, sys
r = json.load(open(sys.argv[1]))
eng = {'E1': 'small', 'E2s': 'BioCLIP server', 'E2p': 'BioCLIP phone', 'E3': 'gallery'}
print('| engine | photos | ' + ' | '.join(f'{t:.2f}-{t+0.05:.2f}' for t in [0.5 + 0.05 * i for i in range(6)]) + ' |')
print('|---|---|' + '---|' * 6)
for e, name in eng.items():
    for dom in ['field+inhand', 'inhand']:
        g = r['engines']['strict'][e]['grid'][f'{dom}|ALL']
        cells = []
        for i in range(6):
            t = round(0.5 + 0.05 * i, 2); u = round(t + 0.05, 2)
            a, b = g[f'{t:g}']['species'], g[f'{u:g}']['species']
            n, k = a['n'] - b['n'], a['k'] - b['k']
            cells.append(f'{100 * k / n:.1f} (n {n})' if n else '-')
        print(f'| {name} | {dom} | ' + ' | '.join(cells) + ' |')
