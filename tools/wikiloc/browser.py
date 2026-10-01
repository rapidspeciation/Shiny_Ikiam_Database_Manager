"""JSON-line browser process for the Wikiloc Node worker.

Each worker job owns one Camoufox instance. Standard output is reserved for
responses; browser diagnostics stay on standard error.
"""

import asyncio
import json
import sys


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
        )
    )


async def challenge_type(page):
    from playwright_captcha.solvers.click.cloudflare.utils.detection import detect_cloudflare_challenge

    try:
        if await detect_cloudflare_challenge(page, "interstitial"):
            return "interstitial"
        if await detect_cloudflare_challenge(page, "turnstile"):
            return "turnstile"
    except Exception:
        # The iframe may disappear while its JavaScript check resolves.
        return None
    return None


async def clear_challenge(page):
    from playwright_captcha import CaptchaType, ClickSolver, FrameworkType

    # A JavaScript challenge often clears without a checkbox. Wait for that
    # first; the click solver only works once Cloudflare's iframe is present.
    for _ in range(10):
        if not is_challenge(await page.title()):
            return
        if await challenge_type(page):
            break
        await page.wait_for_timeout(1000)

    for _ in range(2):
        if not is_challenge(await page.title()):
            return
        kind = await challenge_type(page)
        if kind is None:
            await page.wait_for_timeout(3000)
            continue
        try:
            async with ClickSolver(framework=FrameworkType.CAMOUFOX, page=page) as solver:
                await asyncio.wait_for(
                    solver.solve_captcha(
                        captcha_container=page,
                        captcha_type=(
                            CaptchaType.CLOUDFLARE_INTERSTITIAL
                            if kind == "interstitial"
                            else CaptchaType.CLOUDFLARE_TURNSTILE
                        ),
                    ),
                    timeout=35,
                )
        except Exception:
            # Cloudflare may have replaced the iframe during solver setup.
            # One bounded re-detection below handles that race.
            pass
        for _ in range(10):
            if not is_challenge(await page.title()):
                return
            await page.wait_for_timeout(1000)
    raise TimeoutError("Cloudflare challenge did not clear")


async def serve():
    browser_manager = None
    browser = None
    page = None
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

                        browser_manager = AsyncCamoufox(
                            headless="virtual",
                            humanize=1.5,
                            config={"forceScopeAccess": True},
                            main_world_eval=True,
                        )
                        browser = await browser_manager.__aenter__()
                        page = await browser.new_page()
                    await page.goto(request["url"], wait_until="domcontentloaded", timeout=60_000)
                    await page.wait_for_timeout(2000)
                    await clear_challenge(page)
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


if __name__ == "__main__":
    asyncio.run(serve())
