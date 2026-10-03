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

Muertes on a phone (`deaths.mjs`) saves deaths and undoes them, so it refuses any app but a local one
(LOCAL_MODE, its own database). `adb reverse tcp:8796 tcp:8796` makes the phone's `localhost:8796` this
PC's (a secure context, which `crypto.randomUUID` needs; `10.0.2.2` is not):
`APP=http://localhost:8796/ CREDS=~/.cache/ithomiini-lab/credentials.json DB=/tmp/app.sqlite node tools/android-test/deaths.mjs A4E A3E A2E`
(three living IDs of that copy; `ORIENT=landscape` turns the phone). It also selects a card (its own cause),
opens a card's details with its ›, and opens the Deaths history. It closes the keyboard by leaving the box, not with the back key, which can
leave the page.

Clutches as cards (`clutches.mjs`) saves counts and notes and marks checks, so it too refuses any app
but a local one: `adb reverse tcp:8797 tcp:8797`, then
`APP=http://localhost:8797/ CREDS=~/.cache/ithomiini-lab/credentials.json node tools/android-test/clutches.mjs`
(`ROTATION=1` with the phone on its side). It also types a new total over a count's total, opens and closes
the calendar of «Correct» (tap outside, Cancel or ✕, Correct again) and undoes the clutch's changes from «Today». It opens its own tab, and measures the keyboard right after
tapping a box: text sent with `adb shell input text` makes SwiftKey fold into its bar, as for a
hardware keyboard. Two local apps on `localhost` share one session cookie (cookies ignore the port):
signing in on one signs the other out.

Colecta as cards (`collect.mjs`) saves a collecting day and undoes it, so it too refuses any app but a
local one: `adb reverse tcp:8811 tcp:8811`, then
`APP=http://localhost:8811/ CREDS=~/.cache/ithomiini-lab/credentials.json node tools/android-test/collect.mjs`.
It fills the day with taps, adds a butterfly from a species chip, types its time, adds «another like this»
as preserved with its weight, finds a third with the notebook's shorthand («mech mess»), saves and undoes.

The tests log in with the Wikiloc worker account (`~/.config/ithomiini-wikiloc/worker.json`)
and use Playwright from `~/.local/share/ithomiini-wikiloc/node_modules`. The keyboard is judged
by the page's visible height (Android's own flag can be stale). They add rows to the Colecta
list of that account and empty it at the end; nothing is saved to the workbook.

Tubos as cards (`tubes.mjs`) pastes a range, types a tube on one card with the number pad and checks that
Enter goes to the next card's tube with its suggestion selected; it never saves:
`adb reverse tcp:8847 tcp:8847`, then
`APP=http://localhost:8847/ CREDS=~/.cache/ithomiini-lab/credentials.json node tools/android-test/tubes.mjs Z8D-A2E Z9D A0E`.

SwiftKey (the keyboard the team uses) can replace Gboard: download its APK, check the signer is
TouchType Limited (`build-tools/35.0.0/apksigner verify --print-certs`), then
`adb install -r swiftkey.apk` and `adb shell ime set com.touchtype.swiftkey/com.touchtype.KeyboardService`.
It is taller than Gboard (the page keeps ~40 % of the height while typing).

What the emulator showed (2026-09-28):
- the action bar appeared over a tapped cell near the bottom, so the second tap of a double
  tap pressed "Borrar";
- Tabulator's list editor did not focus its box (no keyboard) and closes on the window
  resize event Chrome fires when the keyboard opens.
