"""Real pointer input on the browser's own X display, for Cloudflare's checkbox.

Cloudflare's Turnstile checkbox sits in a cross-origin iframe inside a closed shadow root.
Playwright's synthetic clicks reach it but do not pass the check from the app server, while
clicks the X server itself produces (the XTEST extension, as xdotool uses) do, sometimes: the
widget then runs its check like a person's. Measured on 19 Oct 2026 against identi.io, which
always shows the checkbox: see tools/wikiloc/README.md.

No package is needed: libX11 and libXtst come with Xvfb's and the browser's own libraries
(ctypes). The mouse moves like a hand: a curved path (a cubic Bezier with random control
points and jitter, eased), a slight overshoot and back, a short dwell over the box, then a
60–120 ms press. The idea of clicking the widget's left edge (~28 px in, at mid-height) comes
from EzSolver (github.com/ismoiloffS/EzSolver, MIT); the code here is our own.
"""

import ctypes
import ctypes.util
import math
import os
import random
import subprocess
import time


def human_path(x0, y0, x1, y1, rng=random):
    """Points from (x0, y0) to (x1, y1): curved, eased, jittered, overshooting a few px and back."""
    dist = math.hypot(x1 - x0, y1 - y0)
    over = rng.uniform(3, 8)
    angle = math.atan2(y1 - y0, x1 - x0)
    ox, oy = x1 + math.cos(angle) * over, y1 + math.sin(angle) * over
    c1 = (
        x0 + (ox - x0) * rng.uniform(0.2, 0.4) + rng.uniform(-80, 80),
        y0 + (oy - y0) * rng.uniform(0.2, 0.4) + rng.uniform(-80, 80),
    )
    c2 = (
        x0 + (ox - x0) * rng.uniform(0.6, 0.85) + rng.uniform(-40, 40),
        y0 + (oy - y0) * rng.uniform(0.6, 0.85) + rng.uniform(-40, 40),
    )
    steps = max(25, int(dist / 12))
    points = []
    for i in range(1, steps + 1):
        t = i / steps
        e = t * t * (3 - 2 * t)
        bx = (1 - e) ** 3 * x0 + 3 * (1 - e) ** 2 * e * c1[0] + 3 * (1 - e) * e**2 * c2[0] + e**3 * ox
        by = (1 - e) ** 3 * y0 + 3 * (1 - e) ** 2 * e * c1[1] + 3 * (1 - e) * e**2 * c2[1] + e**3 * oy
        points.append((bx + rng.uniform(-0.8, 0.8), by + rng.uniform(-0.8, 0.8)))
    for i in range(1, 6):
        t = i / 5
        points.append((ox + (x1 - ox) * t, oy + (y1 - oy) * t))
    return points


def click_plan(x0, y0, x1, y1, rng=random):
    """The click as timed steps: ('move', x, y, pause s) along the path, a dwell, ('press'|'release', pause)."""
    points = human_path(x0, y0, x1, y1, rng)
    total = rng.uniform(0.4, 1.2)
    plan = [("move", round(px), round(py), total / len(points) * rng.uniform(0.6, 1.4)) for px, py in points]
    plan[-1] = ("move", round(x1), round(y1), rng.uniform(0.15, 0.4))
    plan.append(("press", None, None, rng.uniform(0.06, 0.12)))
    plan.append(("release", None, None, 0))
    return plan


class XDisplay:
    """A private X display (Xvfb) for the browser, and real pointer events on it."""

    def __init__(self, width=1920, height=1080):
        read, write = os.pipe()
        # -displayfd: Xvfb picks a free display number and writes it on the pipe.
        self.proc = subprocess.Popen(
            ["Xvfb", "-displayfd", str(write), "-screen", "0", f"{width}x{height}x24", "-nolisten", "tcp"],
            pass_fds=(write,),
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        os.close(write)
        number = b""
        with os.fdopen(read, "rb") as pipe:
            while not number.endswith(b"\n"):
                chunk = pipe.read(1)
                if not chunk:
                    break
                number += chunk
        if not number.strip():
            self.proc.terminate()
            raise RuntimeError("Xvfb did not start")
        self.name = f":{number.strip().decode()}"
        self.pointer = [random.randint(300, 900), random.randint(400, 800)]
        self._x11 = ctypes.CDLL(ctypes.util.find_library("X11") or "libX11.so.6")
        self._xtst = ctypes.CDLL(ctypes.util.find_library("Xtst") or "libXtst.so.6")
        self._x11.XOpenDisplay.restype = ctypes.c_void_p
        self._x11.XOpenDisplay.argtypes = [ctypes.c_char_p]
        self._x11.XFlush.argtypes = [ctypes.c_void_p]
        self._x11.XCloseDisplay.argtypes = [ctypes.c_void_p]
        self._xtst.XTestFakeMotionEvent.argtypes = [ctypes.c_void_p, ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_ulong]
        self._xtst.XTestFakeButtonEvent.argtypes = [ctypes.c_void_p, ctypes.c_uint, ctypes.c_int, ctypes.c_ulong]

    def click(self, x, y):
        """A hand-like click at screen pixel (x, y) (blocking: run it in a thread)."""
        display = self._x11.XOpenDisplay(self.name.encode())
        if not display:
            raise RuntimeError(f"Cannot open X display {self.name}")
        try:
            for kind, px, py, pause in click_plan(self.pointer[0], self.pointer[1], x, y):
                if kind == "move":
                    self._xtst.XTestFakeMotionEvent(display, -1, int(px), int(py), 0)
                else:
                    self._xtst.XTestFakeButtonEvent(display, 1, 1 if kind == "press" else 0, 0)
                self._x11.XFlush(display)
                time.sleep(pause)
            self.pointer = [x, y]
        finally:
            self._x11.XCloseDisplay(display)

    def close(self):
        self.proc.terminate()
        try:
            self.proc.wait(5)
        except subprocess.TimeoutExpired:
            self.proc.kill()
