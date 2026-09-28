#!/usr/bin/env bash
# Taps the on-screen element whose text contains $1 (Android UI, via uiautomator). Prints what it tapped.
ADB="$(dirname "$0")/sdk/platform-tools/adb"
$ADB shell uiautomator dump /sdcard/ui.xml >/dev/null 2>&1
$ADB shell cat /sdcard/ui.xml | python3 -c '
import sys, re
want = sys.argv[1].lower()
for node in re.findall(r"<node [^>]*>", sys.stdin.read()):
    text = (re.search(r"text=\"([^\"]*)\"", node) or [None, ""])[1]
    desc = (re.search(r"content-desc=\"([^\"]*)\"", node) or [None, ""])[1]
    if want in text.lower() or (desc and want in desc.lower()):
        x1, y1, x2, y2 = map(int, re.search(r"bounds=\"\[(\d+),(\d+)\]\[(\d+),(\d+)\]\"", node).groups())
        print((x1 + x2) // 2, (y1 + y2) // 2, text or desc)
        break
' "$1" | { read -r x y label && $ADB shell input tap "$x" "$y" && echo "tapped: $label"; }
