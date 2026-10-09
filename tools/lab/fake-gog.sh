#!/bin/bash
# The lab's mail: instead of sending through Gmail (gog), each email the lab app sends is
# saved in ~/.cache/ithomiini-lab/mail/ (recipient, subject, text), to read while testing.
dir="$HOME/.cache/ithomiini-lab/mail"
mkdir -p "$dir"
to='' subject=''
while [ $# -gt 0 ]; do
  case "$1" in
    --to) to="$2"; shift ;;
    --subject) subject="$2"; shift ;;
  esac
  shift
done
file="$dir/$(date +%Y%m%d-%H%M%S-%N).txt"
{ echo "To: $to"; echo "Subject: $subject"; echo; cat; } > "$file"
