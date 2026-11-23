import test from 'node:test';
import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';

const ROOT = dirname(dirname(fileURLToPath(import.meta.url)));
const TOOLS_ID = join(ROOT, 'tools', 'identifier');
const MERGES_PATH = join(TOOLS_ID, 'leaf_merges_6fa6ab78.json');
const TAXONOMY_PATH = join(TOOLS_ID, 'taxonomy_6fa6ab78.json');

test('leaf_merges_6fa6ab78.json holds the exact 48 approved labels_q pairs with zero chains and quisqueya separate', () => {
  const doc = JSON.parse(readFileSync(MERGES_PATH, 'utf8'));
  const tax = JSON.parse(readFileSync(TAXONOMY_PATH, 'utf8'));
  assert.equal(doc.schema, 'ithomiini-leaf-merges/1');
  assert.equal(doc.source_sha256, '05e847e69d187106955955cef615efd83aefe07a441014024f59bea3fca43603');
  assert.equal(doc.count, 48);
  assert.equal(Object.keys(doc.merges).length, 48);
  assert.equal(doc.merges['Greta diaphanus quisqueya'], undefined);
  assert.equal(doc.merges['Greta diaphanus diaphanus'], 'Greta diaphanus');
  assert.equal(doc.merges['Mechanitis polymnia proceriformis'], undefined);
  assert.equal(doc.merges['Mechanitis polymnia eurydice'], undefined);
  assert.equal(doc.merges['Melinaea marsaeus mothone'], 'Melinaea mothone mothone');
  assert.equal(doc.merges['Melinaea menophilus mothone'], 'Melinaea mothone mothone');
  assert.equal(doc.merges['Melinaea marsaeus messenina'], 'Melinaea mothone messenina');
  assert.equal(doc.merges['Melinaea menophilus messenina'], 'Melinaea mothone messenina');
  for (const [oldLeaf, newLeaf] of Object.entries(doc.merges)) {
    assert.notEqual(oldLeaf, newLeaf);
    assert.equal(doc.merges[newLeaf], undefined, `chain at ${newLeaf}`);
    const sp = newLeaf.split(' ').slice(0, 2).join(' ');
    assert.ok(tax.species[sp], `target species missing from taxonomy_6fa6ab78.json: ${sp}`);
  }
});

test('tools/identifier/leaf_merges.py conserves probability and matches JS display-merge math across all 48 pairs', () => {
  const script = `
import json, sys
import numpy as np
sys.path.insert(0, sys.argv[1])
import leaf_merges

merges = leaf_merges.load_leaf_merges(sys.argv[2], "6fa6ab78c6c3b76566a15d322061d2a5bec95c7dd705797c75c95799e1603552", required=True)
assert len(merges) == 48, len(merges)
assert leaf_merges.load_leaf_merges(sys.argv[2], "0" * 64) == {}
try:
    leaf_merges.load_leaf_merges("/nonexistent/leaf_merges.json", required=True)
    raise AssertionError("expected FileNotFoundError")
except FileNotFoundError:
    pass
try:
    leaf_merges.load_leaf_merges(sys.argv[2], "0" * 64, required=True)
    raise AssertionError("expected ValueError on head mismatch")
except ValueError:
    pass

controls = [
    "Greta diaphanus quisqueya",
    "Mechanitis polymnia proceriformis",
    "Mechanitis polymnia eurydice",
    "Oleria onega janarilla",
]
raw_leaves = sorted(set(merges.keys()) | (set(merges.values()) - {"Melinaea mothone messenina"}) | set(controls))
raw_sp_vocab = sorted({ " ".join(lf.split()[:2]) for lf in raw_leaves })
raw_ge_vocab = sorted({ lf.split()[0] for lf in raw_leaves })
sp_i = {s: i for i, s in enumerate(raw_sp_vocab)}
ge_i = {g: i for i, g in enumerate(raw_ge_vocab)}
raw_l2sp = np.array([sp_i[" ".join(lf.split()[:2])] for lf in raw_leaves], dtype=np.int64)
raw_l2ge = np.array([ge_i[lf.split()[0]] for lf in raw_leaves], dtype=np.int64)

space = leaf_merges.build_display_space(raw_leaves, raw_sp_vocab, raw_ge_vocab, raw_l2sp, raw_l2ge, merges, np, strict=True)
assert len(space["leaves"]) == len(raw_leaves) - 47
try:
    leaf_merges.build_display_space(raw_leaves[1:], raw_sp_vocab, raw_ge_vocab, raw_l2sp[1:], raw_l2ge[1:], merges, np, strict=True)
    raise AssertionError("expected ValueError on missing merge key")
except ValueError:
    pass
assert "Melinaea mothone messenina" in space["leaves"]
assert "Greta diaphanus quisqueya" in space["leaves"]
for old in merges:
    assert old not in space["leaves"]
for gone in ("Greta a", "Hypothyris noinonia", "Ithomia jacunda", "Godyris gonussa", "Mechanitis isthmia", "Napeogenes cyrianassa"):
    assert gone not in space["sp_vocab"]

rng = np.random.default_rng(20261123)
logits = rng.normal(0.0, 2.0, size=len(raw_leaves))
e = np.exp(logits - logits.max())
raw_p = e / e.sum()
merged_p = leaf_merges.merge_leaf_probs(raw_p, space["old_to_new"], len(space["leaves"]), np)
assert abs(float(merged_p.sum()) - 1.0) < 1e-13

sp_p = np.zeros(len(space["sp_vocab"]), dtype=np.float64)
np.add.at(sp_p, space["leaf2sp"], merged_p)
ge_p = np.zeros(len(space["ge_vocab"]), dtype=np.float64)
np.add.at(ge_p, space["leaf2ge"], merged_p)
assert abs(float(sp_p.sum()) - 1.0) < 1e-13
assert abs(float(ge_p.sum()) - 1.0) < 1e-13

print(json.dumps({
    "raw_leaves": raw_leaves,
    "raw_p": raw_p.tolist(),
    "display_leaves": space["leaves"],
    "merged_p": merged_p.tolist(),
    "sp_vocab": space["sp_vocab"],
    "sp_p": sp_p.tolist(),
    "ge_vocab": space["ge_vocab"],
    "ge_p": ge_p.tolist(),
}))
`;
  const out = JSON.parse(execFileSync('python3', ['-I', '-c', script, TOOLS_ID, MERGES_PATH], { encoding: 'utf8' }));
  const merges = JSON.parse(readFileSync(MERGES_PATH, 'utf8')).merges;

  // Independent JS reference calculation over the exact same raw_p vector.
  const jsLeaf = new Map();
  for (let i = 0; i < out.raw_leaves.length; i++) {
    const tgt = merges[out.raw_leaves[i]] ?? out.raw_leaves[i];
    jsLeaf.set(tgt, (jsLeaf.get(tgt) ?? 0) + out.raw_p[i]);
  }
  const jsSp = new Map();
  const jsGe = new Map();
  for (const [lf, p] of jsLeaf) {
    const sp = lf.split(' ').slice(0, 2).join(' ');
    const ge = lf.split(' ')[0];
    jsSp.set(sp, (jsSp.get(sp) ?? 0) + p);
    jsGe.set(ge, (jsGe.get(ge) ?? 0) + p);
  }

  assert.equal(out.display_leaves.length, jsLeaf.size);
  for (let i = 0; i < out.display_leaves.length; i++) {
    const name = out.display_leaves[i];
    assert.ok(Math.abs(out.merged_p[i] - jsLeaf.get(name)) < 1e-14, `leaf mismatch: ${name}`);
  }
  for (let i = 0; i < out.sp_vocab.length; i++) {
    const name = out.sp_vocab[i];
    assert.ok(Math.abs(out.sp_p[i] - jsSp.get(name)) < 1e-14, `species mismatch: ${name}`);
  }
  for (let i = 0; i < out.ge_vocab.length; i++) {
    const name = out.ge_vocab[i];
    assert.ok(Math.abs(out.ge_p[i] - jsGe.get(name)) < 1e-14, `genus mismatch: ${name}`);
  }
});
