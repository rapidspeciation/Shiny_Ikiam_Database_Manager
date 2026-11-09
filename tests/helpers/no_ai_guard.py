"""No test may use Franz's AI subscriptions (his rule, 9 Nov 2026: "I never want a test to use my ai subscriptions").

The Python side of tests/helpers/no-ai-core.mjs, installed on import by tests/helpers/no-ai-py/sitecustomize.py in
every Python process a test starts (PYTHONPATH, set by the node guard), which reports to the test. (The maps repo keeps
the same file as test/no_ai_guard.py, for pytest too.)
It refuses, and writes down,
  - starting a model CLI (claude, codex, t3, opencode, gemini, ...), wacli or ssh (subprocess, os.system, os.exec*,
    os.posix_spawn*, and a shell or env given a script or arguments that name one), unless it is a stub a test wrote in
    a temporary folder or in tests/ (the real ones are also shadowed on PATH by tests/helpers/no-ai-bin);
  - a connection to an AI host (Anthropic, OpenAI, Gemini's API, OpenRouter, ElevenLabs, ...), to T3 Code's hosts, or to
    the T3 ports 3773-3775 on any address, refused before any name lookup, so it needs no network;
takes the AI tokens and the AI and T3 addresses out of the environment, and gives child processes the guard (Python:
PYTHONPATH; node: NODE_OPTIONS, tests/helpers/no-ai.mjs), also when a test passes its own env (its PATH is kept).
"""
import atexit
import os
import re
import shutil
import socket
import subprocess
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
SHIMS = os.path.join(HERE, "no-ai-bin")
PYDIR = os.path.join(HERE, "no-ai-py")
# tests/ (this file in tests/helpers, as in the database repo) or the folder it is in (the maps repo's test/).
TESTS = os.path.dirname(HERE) if os.path.basename(HERE) == "helpers" else HERE
NODE_PRELOAD = os.path.join(HERE, "no-ai.mjs")
CLIS = {"claude", "codex", "t3", "opencode", "gemini", "ollama", "aider", "cursor-agent", "qwen", "crush", "goose", "amp",
        "llm", "deepseek", "wacli", "ssh"}
SHELLS = {"sh", "bash", "zsh", "dash", "ksh", "env"}
AI_HOSTS = ("anthropic.com", "claude.ai", "claude.com", "openai.com", "chatgpt.com", "oaiusercontent.com", "openrouter.ai",
            "generativelanguage.googleapis.com", "aiplatform.googleapis.com", "elevenlabs.io", "deepseek.com", "mistral.ai",
            "groq.com", "x.ai", "cohere.ai", "cohere.com", "together.xyz", "together.ai", "perplexity.ai", "z.ai",
            "bigmodel.cn", "moonshot.ai", "moonshot.cn", "fireworks.ai", "replicate.com", "opencode.ai",
            "api-inference.huggingface.co")
T3_HOSTS = ("t3.codes", "t3.ithomiini-ikiam.com")
T3_PORTS = {3773, 3774, 3775}
SECRET_ENV = re.compile(r"^(ANTHROPIC_|CLAUDE_CODE_OAUTH_TOKEN$|CLAUDE_CODE_MESSAGING_|OPENAI_|AI_API_KEY$|AI_MODEL$|ITHOMIINI_AI_|"
                        r"ITHOMIINI_T3_|T3CODE_|CLAUDE_CONFIG_DIR$|CODEX_HOME$|T3_URL$|GEMINI_|GOOGLE_API_KEY$|ELEVENLABS_|"
                        r"OPENROUTER_|DEEPSEEK_|MISTRAL_|GROQ_|OLLAMA_HOST$|WA_)")

FIRST = "NOAI_GUARD_PARENT" not in os.environ
_own = os.path.join(tempfile.gettempdir(), f"no-ai-guard-py-{os.getpid()}.log")
LOGS = [l for l in os.environ.get("NOAI_GUARD_LOGS", "").split(os.pathsep) if l] + [_own]
_given_test = os.environ.get("NOAI_GUARD_FILE", "?")  # the test that started this process (a node test, a pytest test)
_test = lambda: os.environ.get("PYTEST_CURRENT_TEST") or _given_test


def _stub(path):
    path = os.path.abspath(path)
    inside = lambda d: path == d or path.startswith(d.rstrip(os.sep) + os.sep)
    return (inside(tempfile.gettempdir()) or inside("/tmp") or inside(TESTS)) and not inside(SHIMS)


def blocked_cli(cmd, env=None):
    """The real CLI a command would start, or None."""
    cmd = os.fsdecode(cmd)
    if os.path.basename(cmd) not in CLIS:
        return None
    if "/" in cmd:
        return None if _stub(cmd) else cmd
    found = shutil.which(cmd, path=(env or os.environ).get("PATH"))
    if not found:
        return None
    return found if found.startswith(SHIMS + os.sep) or not _stub(found) else None


def blocked_in_shell(line, env=None):
    """The first blocked CLI a shell line names as a command word, or as the path of an executable file named like one."""
    line = re.sub(r"(\b(?:sh|bash|zsh|dash|ksh)\s+(?:-\S+\s+)*-[a-z]*c\s+)['\"]", r"\1;", os.fsdecode(line))
    for w in re.findall(r"(?:^|[;&|(`\n]|\$\()\s*(?:(?:[A-Za-z_][A-Za-z0-9_]*=\S*|exec|env|nohup|command|nice|-\S+)\s+)*"
                        r"([^\s;&|()`'\"<>]+)", line):
        if blocked_cli(w, env):
            return blocked_cli(w, env)
    for w in re.findall(r"[^\s;&|()<>'\"`=]*/(?:%s)(?=$|[\s;&|()<>'\"`])" % "|".join(map(re.escape, CLIS)), line):
        if os.path.isfile(w) and os.access(w, os.X_OK) and blocked_cli(w, env):
            return blocked_cli(w, env)
    return None


def blocked_args(args, env=None, shell=False):
    if shell or isinstance(args, (str, bytes)):
        return blocked_in_shell(args if isinstance(args, (str, bytes)) else " ".join(map(os.fsdecode, args)), env)
    args = [os.fsdecode(a) for a in args]
    if args and os.path.basename(args[0]) in SHELLS:
        return blocked_in_shell(" ".join(args[1:]), env)
    return blocked_cli(args[0], env) if args else None


def blocked_host(host, port=None):
    h = str(host or "").lower().rstrip(".").strip("[]")
    if port in T3_PORTS:
        return f"T3 port {h or 'localhost'}:{port}"  # on loopback, the tailnet or any address
    if h in ("", "localhost", "::1", "0.0.0.0", "::") or h.startswith("127."):
        return None
    if any(h == d or h.endswith("." + d) for d in AI_HOSTS):
        return f"AI host {h}:{port}"
    if any(h == d or h.endswith("." + d) for d in T3_HOSTS):
        return f"T3 host {h}:{port}"
    return None


def _refuse(kind, target):
    line = f"no-ai guard: refused {kind} {target} (test {_test()}, pid {os.getpid()})"
    for log in LOGS:
        try:
            with open(log, "a") as fh:
                fh.write(line.replace("\n", " ") + "\n")
        except OSError:
            pass
    return line


def take_attempts():
    """The attempts since the last call: this process's, and those of the processes it started."""
    if not os.path.exists(_own):
        return []
    with open(_own) as fh:
        fresh = [l for l in fh.read().splitlines() if l]
    os.remove(_own)
    return fresh


def guard_env(env):
    """A child's env with the guard's preloads and reporting; its PATH as given."""
    env = dict(env)
    if NODE_PRELOAD not in env.get("NODE_OPTIONS", ""):
        env["NODE_OPTIONS"] = f"{env.get('NODE_OPTIONS', '')} --import={NODE_PRELOAD}".strip()
    if PYDIR not in env.get("PYTHONPATH", "").split(os.pathsep):
        env["PYTHONPATH"] = os.pathsep.join(p for p in (PYDIR, env.get("PYTHONPATH")) if p)
    env.update(NOAI_GUARD_LOGS=os.pathsep.join(LOGS), NOAI_GUARD_PARENT=str(os.getpid()),
               NOAI_GUARD_FILE=_test())
    return env


if FIRST:
    for _k in list(os.environ):
        if SECRET_ENV.match(_k):
            del os.environ[_k]
    os.environ["PATH"] = SHIMS + os.pathsep + os.environ.get("PATH", "")
else:  # a child: what it refused is in its parents' logs already
    atexit.register(lambda: os.path.exists(_own) and os.remove(_own))
os.environ.update(guard_env(os.environ))

_getaddrinfo, _connect, _connect_ex = socket.getaddrinfo, socket.socket.connect, socket.socket.connect_ex


def _check(host, port):
    why = blocked_host(host.decode() if isinstance(host, bytes) else host, port)
    if why:
        raise ConnectionRefusedError(_refuse("connection to", why))


def _guarded_getaddrinfo(host, port, *args, **kwargs):
    _check(host, port)
    return _getaddrinfo(host, port, *args, **kwargs)


def _guarded_connect(self, address):
    if isinstance(address, tuple) and len(address) >= 2:
        _check(address[0], address[1])
    return _connect(self, address)


def _guarded_connect_ex(self, address):
    if isinstance(address, tuple) and len(address) >= 2:
        _check(address[0], address[1])
    return _connect_ex(self, address)


_popen_init = subprocess.Popen.__init__


def _guarded_popen(self, args, *rest, **kwargs):
    # Its own env or this one: with the guard's settings and the test running now.
    kwargs["env"] = guard_env(os.environ if kwargs.get("env") is None else kwargs["env"])
    hit = blocked_args(args, kwargs.get("env"), kwargs.get("shell"))
    if hit:
        _refuse("CLI", f"{hit} ({args!r:.160})")
        args, kwargs = ["/bin/sh", "-c", f"echo 'no-ai guard: {os.path.basename(hit)} is not started in tests' >&2; exit 1"], {
            **kwargs, "shell": False, "executable": None}
    return _popen_init(self, args, *rest, **kwargs)


def _refusing(orig, cmd_of, env_at=None):
    """os.system / os.exec* / os.posix_spawn*: refused with PermissionError (os.system: exit status 1) on a blocked CLI;
    an env given to execve/execvpe/posix_spawn* gets the guard's settings (env_at: its position)."""
    def wrapped(*a, **k):
        if env_at is not None and len(a) > env_at and a[env_at] is not None:
            a = (*a[:env_at], guard_env(a[env_at]), *a[env_at + 1:])
        elif env_at is not None and k.get("env") is not None:
            k["env"] = guard_env(k["env"])
        hit = cmd_of(a)
        if hit:
            line = _refuse("CLI", f"{hit} ({orig.__name__})")
            if orig.__name__ == "system":
                return 256
            raise PermissionError(line)
        return orig(*a, **k)
    return wrapped


socket.getaddrinfo = _guarded_getaddrinfo
socket.socket.connect = _guarded_connect
socket.socket.connect_ex = _guarded_connect_ex
subprocess.Popen.__init__ = _guarded_popen
os.system = _refusing(os.system, lambda a: blocked_in_shell(a[0]))
for _name, _env_at in (("execv", None), ("execve", 2), ("execvp", None), ("execvpe", 2), ("posix_spawn", 2), ("posix_spawnp", 2)):
    if hasattr(os, _name):
        setattr(os, _name, _refusing(getattr(os, _name), lambda a: blocked_args([a[0], *list(a[1])[1:]]), _env_at))
