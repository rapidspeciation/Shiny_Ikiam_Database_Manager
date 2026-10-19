"""JSON-line browser process for the Wikiloc Node worker.

Each worker job owns one Camoufox instance. Standard output is reserved for
responses; browser diagnostics stay on standard error.

Cloudflare's check ("Performing security verification", a "Verify you are human"
checkbox in a closed shadow root) is passed by clicking the checkbox:
- on the server, Camoufox runs on a private Xvfb display and the click is real X
  input (xinput.py), the only kind that passed from the server's address in our tests;
- elsewhere (a home computer without Xvfb), Camoufox runs headless and Playwright clicks.
The browser presents itself as Firefox on Windows (os="windows", as cloudflare-solver,
github.com/art3m4ik3/cloudflare-solver, MIT, does). Results: tools/wikiloc/README.md.
"""

import asyncio
import json
import os
import random
import shutil
import sys
import time

HOME_PAGE = "https://es.wikiloc.com/"
# Screenshots of the pages whose check did not clear, the latest few (the worker's log says where).
FAILURES = os.environ.get("ITHOMIINI_WIKILOC_FAILURES") or os.path.join(
    os.path.expanduser("~"), ".cache", "ithomiini-wikiloc", "failures"
)
# Clicks on one page's checkbox, and how long the check gets in all.
MAX_CLICKS = 4
CHECK_SECONDS = 100


def log(text):
    """A line for the worker's log (standard error; standard output is for answers)."""
    print(f"wikiloc: {text}", file=sys.stderr, flush=True)


def is_challenge(title):
    return any(
        term in title.lower()
        for term in (
            "just a moment",
            "un momento",
            "attention required",
            "verify you are human",
            "verifica que eres humano",
            "performing security verification",
            "verificación de seguridad",
        )
    )


# The checkbox widget: the box around Turnstile's response field (the iframe itself is in a
# closed shadow root), and where the page's viewport is on the screen.
WIDGET = """mw:(() => {
  const input = document.querySelector('input[name="cf-turnstile-response"]');
  if (!input) return null;
  const r = input.parentElement.getBoundingClientRect();
  return { x: r.x, y: r.y, w: r.width, h: r.height,
           sx: window.mozInnerScreenX, sy: window.mozInnerScreenY, dpr: window.devicePixelRatio };
})()"""


async def click_checkbox(page, display):
    """One click on the checkbox (~28 px from the widget's left edge, mid-height); False when no widget."""
    box = await page.evaluate(WIDGET)
    if not box or box["w"] < 50 or box["h"] < 20:
        return False
    x = box["x"] + random.uniform(22, 32)
    y = box["y"] + box["h"] / 2 + random.uniform(-4, 4)
    if display:
        await asyncio.to_thread(display.click, (box["sx"] + x) * box["dpr"], (box["sy"] + y) * box["dpr"])
    else:
        await page.mouse.move(x, y, steps=random.randint(15, 30))
        await page.mouse.down()
        await page.wait_for_timeout(random.randint(60, 120))
        await page.mouse.up()
    return True


async def clear_challenge(page, display):
    """Waits for Cloudflare's check to clear, clicking its checkbox when it shows one."""
    started = time.monotonic()
    clicks = 0
    next_click = 0.0
    while time.monotonic() - started < CHECK_SECONDS:
        if not is_challenge(await page.title()):
            return clicks
        now = time.monotonic()
        # The widget needs a moment after it appears; then a click every ~10 s while it asks again.
        if clicks < MAX_CLICKS and now >= next_click:
            await page.wait_for_timeout(random.randint(1200, 2500))
            if await click_checkbox(page, display):
                clicks += 1
                log(f"checkbox clicked ({'X input' if display else 'page'}), click {clicks}")
                next_click = time.monotonic() + 10
        await page.wait_for_timeout(1000)
    raise TimeoutError("Cloudflare challenge did not clear")


async def keep_failure(page, url):
    """The page as Cloudflare left it, to see what it asked (the latest five kept)."""
    try:
        os.makedirs(FAILURES, mode=0o700, exist_ok=True)
        path = os.path.join(FAILURES, time.strftime("%Y%m%d-%H%M%S") + ".png")
        await page.screenshot(path=path, full_page=False)
        for old in sorted(os.listdir(FAILURES))[:-5]:
            os.remove(os.path.join(FAILURES, old))
        log(f"page kept: {path} ({url})")
    except Exception as error:
        log(f"page not kept: {error}")


async def open_page(page, url, warmed, display):
    """
    A Wikiloc page past Cloudflare's check. The first page of a browser is Wikiloc's home
    (its check, when there is one, then covers the walk's page too); a check that does not
    clear is tried once more on a fresh load of the page.
    """
    if not warmed and url.rstrip("/") != HOME_PAGE.rstrip("/"):
        await open_page(page, HOME_PAGE, True, display)
    for attempt in (1, 2):
        started = time.monotonic()
        await page.goto(url, wait_until="domcontentloaded", timeout=60_000)
        await page.wait_for_timeout(2000)
        checked = is_challenge(await page.title())
        if checked:
            log(f"check on {url}, try {attempt}")
        try:
            clicks = await clear_challenge(page, display)
            if checked:
                log(f"check cleared in {time.monotonic() - started:.0f} s ({clicks} clicks)")
            return
        except TimeoutError:
            log(f"check did not clear in {time.monotonic() - started:.0f} s (title: {await page.title()!r})")
            if attempt == 2:
                await keep_failure(page, url)
                raise
            await page.wait_for_timeout(5000)


def private_display():
    """The server's way: a private Xvfb display for real clicks (None without Xvfb or when turned off)."""
    if os.environ.get("ITHOMIINI_WIKILOC_XVFB") == "0" or not shutil.which("Xvfb"):
        return None
    from xinput import XDisplay

    return XDisplay()


async def serve():
    browser_manager = None
    browser = None
    page = None
    display = None
    warmed = False
    try:
        while line := await asyncio.to_thread(sys.stdin.readline):
            request = {}
            try:
                request = json.loads(line)
                operation = request["op"]
                if operation == "close":
                    result = None
                elif operation == "goto":
                    if page is None:
                        from camoufox.async_api import AsyncCamoufox

                        display = private_display()
                        if display:
                            os.environ["DISPLAY"] = display.name
                        browser_manager = AsyncCamoufox(
                            headless=display is None,
                            os="windows",
                            humanize=True,
                            main_world_eval=True,
                        )
                        browser = await browser_manager.__aenter__()
                        page = await browser.new_page()
                    await open_page(page, request["url"], warmed, display)
                    warmed = True
                    result = None
                elif operation == "title":
                    result = await page.title()
                elif operation == "evaluate":
                    # Wikiloc stores waypoints and track lines in page globals.
                    # Camoufox's isolated world cannot see these globals.
                    result = await page.evaluate(f"mw:({request['code']})()")
                else:
                    raise ValueError(f"Unknown browser operation: {operation}")
                print(json.dumps({"id": request["id"], "data": result}), flush=True)
                if operation == "close":
                    break
            except Exception as error:
                print(
                    json.dumps({"id": request.get("id"), "error": f"{type(error).__name__}: {error}"}),
                    flush=True,
                )
    finally:
        if browser_manager is not None:
            await browser_manager.__aexit__(None, None, None)
        if display is not None:
            display.close()


if __name__ == "__main__":
    asyncio.run(serve())
