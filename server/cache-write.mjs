// A cache file written whole or not at all: a temporary file of its own (the tools' worker threads share one pid),
// then a rename. A cache that cannot be written is only slower next time: false, never thrown.

import { randomBytes } from 'node:crypto';
import { mkdirSync, renameSync, rmSync, writeFileSync } from 'node:fs';
import { dirname } from 'node:path';
import { threadId } from 'node:worker_threads';

export function writeCacheFile(file, data) {
  const tmp = `${file}.${process.pid}-${threadId}-${randomBytes(4).toString('hex')}.tmp`;
  try {
    mkdirSync(dirname(file), { recursive: true });
    writeFileSync(tmp, data);
    renameSync(tmp, file);
    return true;
  } catch {
    rmSync(tmp, { force: true });
    return false;
  }
}
