#!/usr/bin/env python3
"""Memory of lab copies one minute after their start (VmRSS + VmSwap of the main pid).

  measure.py --label before --copies 3 --out <fresh dir> [--env K=V ...] [--hold 60]

Starts N copies one after the other with tools/lab/dev-app.sh --fresh (each waits for its first sync), samples
each copy's main pid every 5 s until `hold` seconds after the last one became ready (and at least 60 s after its
own start), then stops every copy it started. Writes <out>/<label>.json.
"""
import argparse, json, os, subprocess, threading, time
from pathlib import Path

HERE = Path(__file__).resolve().parents[2]
DEV = HERE / 'tools/lab/dev-app.sh'


def status(pid):
    out = {}
    try:
        for line in Path(f'/proc/{pid}/status').read_text().splitlines():
            k, _, v = line.partition(':')
            if k in ('VmRSS', 'VmSwap', 'VmHWM'):
                out[k] = int(v.split()[0]) // 1024
    except FileNotFoundError:
        return None
    return out


def units():
    out = subprocess.run(['systemctl', '--user', 'list-units', 'ithomiini-dev-*', '--plain', '--no-legend', '--state=active'],
                         capture_output=True, text=True).stdout
    return [l.split()[0] for l in out.splitlines() if l.strip()]


def unit_prop(unit, prop):
    return subprocess.run(['systemctl', '--user', 'show', '-p', prop, '--value', unit], capture_output=True, text=True).stdout.strip()


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--label', required=True)
    ap.add_argument('--copies', type=int, default=1)
    ap.add_argument('--out', required=True)
    ap.add_argument('--env', action='append', default=[])
    ap.add_argument('--hold', type=int, default=60)
    a = ap.parse_args()
    out = Path(a.out)
    out.mkdir(parents=True, exist_ok=True)
    copies = []
    # Samples every copy from its unit's start (found as a new ithomiini-dev-* unit), while the others start.
    before = set(units())
    seen = {}
    stop = threading.Event()
    def sampler():
        while not stop.is_set():
            for u in units():
                if u in before:
                    continue
                if u not in seen:
                    pid = int(unit_prop(u, 'MainPID') or 0)
                    mono = int(unit_prop(u, 'ExecMainStartTimestampMonotonic') or 0) / 1e6
                    if not pid or not mono:
                        continue
                    seen[u] = {'pid': pid, 'started_mono': mono, 'samples': []}
                c = seen[u]
                st = status(c['pid'])
                if st:
                    st['t'] = round(time.monotonic() - c['started_mono'], 1)
                    st['mem_current_mb'] = int(unit_prop(u, 'MemoryCurrent') or 0) // 1048576
                    c['samples'].append(st)
            stop.wait(5)
    th = threading.Thread(target=sampler, daemon=True)
    th.start()
    try:
        for i in range(a.copies):
            avail = int([l for l in open('/proc/meminfo') if l.startswith('MemAvailable')][0].split()[1]) // 1024
            cmd = ['bash', str(DEV), '--fresh', '--ttl', '30m'] + sum((['--env', e] for e in a.env), [])
            t0 = time.time()
            t0m = time.monotonic()
            r = subprocess.run(cmd, capture_output=True, text=True)
            ready = time.time() - t0
            ready_m = time.monotonic()
            print(r.stdout.strip(), r.stderr.strip()[-500:], flush=True)
            port = None
            for word in r.stdout.split():
                if word.startswith('http://127.0.0.1:'):
                    port = int(word.rsplit(':', 1)[1].rstrip(')').split('/')[0])
                    break
            if r.returncode != 0 or port is None:
                raise SystemExit(f'start {i} failed: {r.returncode}')
            unit = f'ithomiini-dev-{port}.service'
            pid = int(unit_prop(unit, 'MainPID'))
            started = int(unit_prop(unit, 'ExecMainStartTimestampMonotonic')) / 1e6
            copies.append({'port': port, 'unit': unit, 'pid': pid, 'ready_s': round(ready, 1), 'started_mono': started,
                           'waited_for_lock_s': round(started - t0m, 1), 'unit_start_to_ready_s': round(ready_m - started, 1),
                           'mem_available_before_mb': avail})
        last_ready = time.monotonic()
        while True:
            now = time.monotonic()
            if now - last_ready >= a.hold and all(now - c['started_mono'] >= 65 for c in copies):
                break
            time.sleep(5)
        stop.set()
        th.join()
        for c in copies:
            c['samples'] = seen[c['unit']]['samples']
            at60 = min(c['samples'], key=lambda s: abs(s['t'] - 60))
            c['at_60s'] = at60
            c['at_60s_rss_plus_swap_mb'] = at60['VmRSS'] + at60.get('VmSwap', 0)
            last = c['samples'][-1]
            c['at_end'] = last
            c['at_end_rss_plus_swap_mb'] = last['VmRSS'] + last.get('VmSwap', 0)
            c['log_tail'] = ''
        result = {'label': a.label, 'env': a.env, 'copies': copies, 'when': time.strftime('%Y-%m-%dT%H:%M:%S')}
        (out / f'{a.label}.json').write_text(json.dumps(result, indent=1))
        for c in copies:
            print(a.label, c['port'], 'ready', c['ready_s'], 's (waited', c['waited_for_lock_s'], 's, own start', c['unit_start_to_ready_s'], 's); at 60 s RSS+swap', c['at_60s_rss_plus_swap_mb'], 'MB',
                  c['at_60s'], '; end', c['at_end_rss_plus_swap_mb'], 'MB at', c['at_end']['t'], 's', flush=True)
    finally:
        for c in copies:
            subprocess.run(['bash', str(DEV), '--stop', '--port', str(c['port'])])


if __name__ == '__main__':
    main()
