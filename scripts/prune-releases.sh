#!/usr/bin/env bash
# Deletes old releases: keeps the newest KEEP (default 5), plus the current release and the
# one before it (shared/previous-release, for a rollback) whatever their age.
# Usage: scripts/prune-releases.sh /home/ubuntu/ithomiini [KEEP]
set -euo pipefail
app="${1:?app directory (holding releases/, current, shared/)}"
keep="${2:-5}"
case "$keep" in '' | *[!0-9]*) echo "KEEP must be a number" >&2; exit 1 ;; esac
[ "$keep" -ge 2 ] || { echo "Keep at least 2 releases" >&2; exit 1; }
cd "$app/releases"
current="$(basename "$(readlink ../current 2>/dev/null || true)")"
previous="$(basename "$(cat ../shared/previous-release 2>/dev/null || true)")"
# Release names are UTC timestamps (20260930T033826Z): newest first by name.
ls -1 | { grep -E '^[0-9]{8}T[0-9]{6}Z$' || true; } | sort -r | tail -n "+$((keep + 1))" | while read -r old; do
  [ "$old" = "$current" ] || [ "$old" = "$previous" ] || rm -rf -- "$old"
done
