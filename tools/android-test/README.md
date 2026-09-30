# Testing on a real Android phone (emulator)

Chrome on Android with its real on-screen keyboard (Gboard), driven by script: taps go
through Chrome's gesture handling (single, double), the page is read over Chrome's DevTools
socket, and screenshots come from the phone (keyboard included).

```sh
tools/android-test/setup.sh                      # once (~3 GB, needs KVM)
~/.local/share/android-test/run-emulator.sh &    # start the phone (no window); boots in ~15 s
~/.local/share/android-test/sdk/platform-tools/adb wait-for-device
# first time only: open Chrome and get past its welcome screens
adb shell am start -a android.intent.action.VIEW -d https://ithomiini-ikiam.com/ com.android.chrome
tools/android-test/tap-text.sh "Use without an account"; tools/android-test/tap-text.sh "No thanks"
node tools/android-test/doubletap.mjs species    # double tap a Colecta cell; logs editor, focus, keyboard
node tools/android-test/flow.mjs                 # type with Gboard + Enter; ▾ list without keyboard
```

The tests log in with the Wikiloc worker account (`~/.config/ithomiini-wikiloc/worker.json`)
and use Playwright from `~/.local/share/ithomiini-wikiloc/node_modules`. The keyboard is judged
by the page's visible height (Android's own flag can be stale). They add rows to the Colecta
list of that account and empty it at the end; nothing is saved to the workbook.

SwiftKey (the keyboard the team uses) can replace Gboard: download its APK, check the signer is
TouchType Limited (`build-tools/35.0.0/apksigner verify --print-certs`), then
`adb install -r swiftkey.apk` and `adb shell ime set com.touchtype.swiftkey/com.touchtype.KeyboardService`.
It is taller than Gboard (the page keeps ~40 % of the height while typing).

What the emulator showed (2026-09-28):
- the action bar appeared over a tapped cell near the bottom, so the second tap of a double
  tap pressed "Borrar";
- Tabulator's list editor did not focus its box (no keyboard) and closes on the window
  resize event Chrome fires when the keyboard opens.
