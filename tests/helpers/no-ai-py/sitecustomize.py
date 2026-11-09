# Python processes a test starts (PYTHONPATH from the guard): the same guard, reporting to the test.
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import no_ai_guard  # noqa: E402,F401  (installs on import)

sys.path.pop(0)
