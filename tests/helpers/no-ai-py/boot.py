# For `python -I` / `python -E` children (they ignore PYTHONPATH, so sitecustomize.py does not load): the node guard
# starts them as `python <their options> boot.py -c|-m|--file <target> <args>`. This loads the Python guard from its
# absolute path, then runs what the child was given, with the same sys.argv, __name__ and sys.path[0].
import os
import runpy
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import no_ai_guard  # noqa: E402,F401  (installs on import)

sys.path.pop(0)
kind, target, *rest = sys.argv[1:]
plain = not sys.flags.isolated and not getattr(sys.flags, "safe_path", False)
if plain and sys.path:
    sys.path[0] = os.path.dirname(os.path.abspath(target)) if kind == "--file" else ("" if kind == "-c" else os.getcwd())
elif not plain and sys.path and sys.path[0] == os.path.dirname(os.path.abspath(__file__)):
    sys.path.pop(0)
if kind == "-c":
    sys.argv = ["-c", *rest]
    exec(compile(target, "<string>", "exec"), {"__name__": "__main__", "__builtins__": __builtins__})
elif kind == "-m":
    sys.argv = [target, *rest]
    runpy.run_module(target, run_name="__main__", alter_sys=True)
else:
    sys.argv = [target, *rest]
    runpy.run_path(target, run_name="__main__")
