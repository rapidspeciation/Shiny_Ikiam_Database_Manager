# Zero-shot view tags for plant photos with open_clip. python classify.py <model> <list.json> <out.json> [batch]
# list.json: [{key, url or path}]; out: {key: [view, p_view, p_leaves_or_habit, p_flower]}.
import json, sys, io, concurrent.futures as cf, urllib.request, torch, open_clip
from PIL import Image
MODELS = {
  'vitb32': ('ViT-B-32', 'laion2b_s34b_b79k'),
  'bioclip2': ('hf-hub:imageomics/bioclip-2', None),
  'vitl14': ('ViT-L-14', 'laion2b_s32b_b82k'),
}
VIEWS = {
  'leaves': ['a photo of the leaves of a plant', 'a photo of a leafy branch', 'a photo of green leaves on a stem', 'a photo of a vine with many leaves'],
  'habit': ['a photo of a whole shrub growing in nature', 'a photo of a climbing vine covering trees', 'a photo of a tree or bush in a landscape'],
  'flower': ['a close-up photo of a flower', 'a photo of a colorful flower', 'a photo of a cluster of flowers', 'a photo of a flower held in a hand', 'a photo of a passion flower'],
  'fruit': ['a photo of green unripe fruits', 'a photo of a fruit hanging from a branch', 'a photo of seed pods'],
  'herbarium': ['a herbarium specimen sheet', 'a pressed dried plant specimen on paper'],
  'other': ['a photo of an insect', 'a photo of a label or text', 'a blurry photo'],
}
name, src, out = sys.argv[1], sys.argv[2], sys.argv[3]
batch = int(sys.argv[4]) if len(sys.argv) > 4 else 64
arch, tag = MODELS[name]
dev = 'cuda' if torch.cuda.is_available() else 'cpu'
model, _, pre = open_clip.create_model_and_transforms(arch, pretrained=tag) if tag else open_clip.create_model_and_transforms(arch)
tok = open_clip.get_tokenizer(arch)
model = model.to(dev).eval()
labels = list(VIEWS)
with torch.no_grad():
    t = model.encode_text(tok([p for v in labels for p in VIEWS[v]]).to(dev)).float()
    t = t / t.norm(dim=-1, keepdim=True)
    owner = torch.tensor([i for i, v in enumerate(labels) for _ in VIEWS[v]], device=dev)
items = json.load(open(src))
def load(it):
    try:
        if it.get('path'):
            im = Image.open(it['path'])
        else:
            data = urllib.request.urlopen(urllib.request.Request(it['url'], headers={'User-Agent': 'ithomiini-database'}), timeout=30).read()
            im = Image.open(io.BytesIO(data))
        return it['key'], pre(im.convert('RGB'))
    except Exception:
        return it['key'], None
res = {}
with cf.ThreadPoolExecutor(32) as ex:
    for i in range(0, len(items), batch):
        got = [g for g in ex.map(load, items[i:i + batch]) if g[1] is not None]
        if not got: continue
        x = torch.stack([g[1] for g in got]).to(dev)
        with torch.no_grad():
            f = model.encode_image(x).float(); f = f / f.norm(dim=-1, keepdim=True)
            sim = (100 * f @ t.T).softmax(-1)
            p = torch.zeros(len(got), len(labels), device=dev).index_add_(1, owner, sim)
        for (k, _), row in zip(got, p.tolist()):
            v = max(range(len(labels)), key=lambda j: row[j])
            res[k] = [labels[v], round(row[v], 3), round(row[0] + row[1], 3), round(row[2], 3)]
        if (i // batch) % 20 == 0: print(i + len(got), file=sys.stderr, flush=True)
json.dump(res, open(out, 'w'))
print('done', len(res), 'of', len(items))
