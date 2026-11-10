// A made-up list of the collection sex model's locked test portion (COLLECTION_SEX_SKIP_FILE), for the tests that
// read the collection sex: without a valid list no sex is scored, stored or shown (server/sex-skip.mjs). Imported,
// it sets the list for the whole file (and the child processes it starts). Its CAM is no fixture's; never the real
// list (the team's repositories are public).
import { mkdtempSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';

export const LOCKED_TEST_CAM = 'CAM999990';
const dir = mkdtempSync(join(tmpdir(), 'sex-skip-'));
export const SEX_SKIP_FILE = join(dir, 'locked.txt');
writeFileSync(SEX_SKIP_FILE, `${LOCKED_TEST_CAM}\n`);
process.env.COLLECTION_SEX_SKIP_FILE = SEX_SKIP_FILE;
process.once('exit', () => rmSync(dir, { recursive: true, force: true }));
