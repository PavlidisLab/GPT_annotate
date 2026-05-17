"""Annotate mouse strains in a GEO experiment using an open-weights model
served via an OpenAI-compatible API.

Defaults to Together AI's Llama 3.3 70B Instruct. Override with:

    OPEN_PROVIDER=together|openrouter|groq|fireworks|deepseek|...
    OPEN_BASE_URL=https://api.together.xyz/v1     # only if no preset for provider
    OPEN_MODEL=meta-llama/Llama-3.3-70B-Instruct-Turbo

Auth: resolves <PROVIDER>_API_KEY from env, falling back to macOS Keychain
service `<PROVIDER>_API_KEY` (e.g. `TOGETHER_API_KEY`). Keychain entry name
can be overridden with OPEN_KEYCHAIN_ENTRY.

Mirrors strain_annotate.py: identical system prompt, strain list, and
report_strains schema (translated from Anthropic tool_use to OpenAI
function-calling tool format)."""
# Path bootstrap so flat `from strain_annotate import X` and other
# revisions/-local imports keep working after subdir reorganisation.
import sys as _sys
from pathlib import Path as _Path
_REV = _Path(__file__).resolve().parents[1]
for _sub in ("", "annotators", "runners", "baselines", "build", "analysis", "metrics"):
    _p = str(_REV / _sub) if _sub else str(_REV)
    if _p not in _sys.path:
        _sys.path.insert(0, _p)
del _sys, _Path, _REV, _sub, _p

import json
import os
import re
import subprocess
import sys
import time
from typing import Optional

from openai import OpenAI, RateLimitError, APIError

from geo_fetch import build_input
from strain_annotate import STRAIN_LIST_PATH, PROMPT_PATH, STRAIN_TOOL

PROVIDER = os.environ.get("OPEN_PROVIDER", "together").lower()
MODEL = os.environ.get("OPEN_MODEL", "meta-llama/Llama-3.3-70B-Instruct-Turbo")

PROVIDER_PRESETS = {
    "together":   {"base_url": "https://api.together.xyz/v1",      "keychain": "TOGETHERAI_API_KEY"},
    "openrouter": {"base_url": "https://openrouter.ai/api/v1",     "keychain": "OPENROUTER_API_KEY"},
    "groq":       {"base_url": "https://api.groq.com/openai/v1",   "keychain": "GROQ_API_KEY"},
    "fireworks":  {"base_url": "https://api.fireworks.ai/inference/v1", "keychain": "FIREWORKS_API_KEY"},
    "deepseek":   {"base_url": "https://api.deepseek.com/v1",      "keychain": "DEEPSEEK_API_KEY"},
    "openai":     {"base_url": "https://api.openai.com/v1",        "keychain": "OPENAI_API_KEY"},
}


def _resolve_key(env_var: str, keychain_entries: list[str]) -> str:
    if env_var in os.environ and os.environ[env_var]:
        return os.environ[env_var]
    for entry in keychain_entries:
        if not entry:
            continue
        try:
            return subprocess.check_output(
                ["security", "find-generic-password", "-s", entry, "-w"],
                text=True, stderr=subprocess.DEVNULL,
            ).strip()
        except subprocess.CalledProcessError:
            continue
    raise SystemExit(
        f"ERROR: no API key found. Set {env_var} or add Keychain entry "
        f"named one of: {keychain_entries}"
    )


def get_client() -> OpenAI:
    preset = PROVIDER_PRESETS.get(PROVIDER, {})
    base_url = os.environ.get("OPEN_BASE_URL") or preset.get("base_url")
    if not base_url:
        raise SystemExit(f"ERROR: no base_url for provider '{PROVIDER}'. Set OPEN_BASE_URL.")
    env_var = f"{PROVIDER.upper()}_API_KEY"
    keychain_entries = [
        os.environ.get("OPEN_KEYCHAIN_ENTRY", ""),
        preset.get("keychain", ""),
        env_var,
    ]
    api_key = _resolve_key(env_var, keychain_entries)
    return OpenAI(api_key=api_key, base_url=base_url)


def _to_openai_tool(anthropic_tool: dict) -> dict:
    """Translate the Anthropic-shaped STRAIN_TOOL to OpenAI function-calling shape."""
    return {
        "type": "function",
        "function": {
            "name": anthropic_tool["name"],
            "description": anthropic_tool["description"],
            "parameters": anthropic_tool["input_schema"],
        },
    }


OPENAI_STRAIN_TOOL = _to_openai_tool(STRAIN_TOOL)


def build_system_prompt() -> str:
    with open(PROMPT_PATH) as f:
        prompt = f.read().rstrip()
    with open(STRAIN_LIST_PATH) as f:
        strain_list = json.load(f)
    return prompt + "\n" + json.dumps(strain_list)


def _extract_tool_call(msg) -> Optional[dict]:
    """Pull a report_strains tool call out of the assistant message.

    Handles three response shapes seen in the wild:
      - native tool_calls with JSON-string arguments (Together, Fireworks);
      - tool_calls with already-parsed dict arguments (some OpenRouter routes);
      - content fallback where the model emits ```json ...``` instead of a tool call.
    """
    tool_calls = getattr(msg, "tool_calls", None) or []
    for tc in tool_calls:
        if tc.function.name != "report_strains":
            continue
        args = tc.function.arguments
        if isinstance(args, str):
            try:
                return json.loads(args)
            except json.JSONDecodeError:
                return None
        if isinstance(args, dict):
            return args
    content = (getattr(msg, "content", None) or "").strip()
    if not content:
        return None
    if content.startswith("```"):
        content = content.strip("`")
        if content.startswith("json"):
            content = content[4:]
        content = content.strip()
    try:
        parsed = json.loads(content)
        if "strains" in parsed:
            return parsed
    except json.JSONDecodeError:
        pass
    return None


_RETRY_AFTER_RE = re.compile(r"try again in ([\d.]+)s")


def _completion_with_retry(client, max_retries=30, **kwargs):
    """Wrap chat.completions.create with sleep-on-429 retry. Parses the
    suggested retry-after seconds from the error body when present;
    otherwise waits long enough for the TPM window to fully turn over
    (OpenAI's TPM is a rolling 60s window, so 65s is a safe floor)."""
    for attempt in range(max_retries):
        try:
            return client.chat.completions.create(**kwargs)
        except RateLimitError as e:
            msg = str(e)
            m = _RETRY_AFTER_RE.search(msg)
            wait = float(m.group(1)) + 1.5 if m else 65.0
            print(f"  [rate-limit] sleeping {wait:.1f}s (attempt {attempt+1}/{max_retries})", file=sys.stderr)
            time.sleep(wait)
        except APIError as e:
            if getattr(e, "status_code", None) and 500 <= e.status_code < 600:
                wait = min(5 * (2 ** attempt), 60)
                print(f"  [transient {e.status_code}] sleeping {wait}s", file=sys.stderr)
                time.sleep(wait)
                continue
            raise
    raise RuntimeError(f"exceeded {max_retries} retries against rate-limit / transient errors")


def annotate(gse: str, client: Optional[OpenAI] = None, max_tokens: int = 2048) -> dict:
    """Issue a single strain annotation request.

    When PROVIDER == 'openai' the request is shaped to match Rogic et al.
    `inst/gpt.py::ask_gpt` exactly: `response_format` json_schema with the
    `mouse_strain` schema (not tool-use), `seed=1`, `top_p=1`,
    `max_tokens=1024`, `temperature=0`. This is the same generation
    payload Rogic submits via the Batch API; the only difference is the
    submission channel (real-time here, queued there), which OpenAI
    documents as producing identical outputs for identical bodies.

    For all other providers (Together / OpenRouter / Groq / Fireworks /
    DeepSeek) we use the tool-use mechanism, which is what those
    OpenAI-compatible endpoints accept reliably."""
    if client is None:
        client = get_client()
    sys_prompt = build_system_prompt()
    user_input = build_input(gse)
    messages = [
        {"role": "system", "content": sys_prompt},
        {"role": "user", "content": json.dumps(user_input)},
    ]
    if PROVIDER == "openai":
        # Rogic-faithful path: response_format json_schema, seed, max_tokens=1024.
        # Imported lazily so non-OpenAI provider runs don't depend on this file.
        from gpt4o_batch import STRAIN_RESPONSE_FORMAT
        resp = _completion_with_retry(
            client,
            model=MODEL,
            max_tokens=1024,
            temperature=0,
            top_p=1,
            seed=1,
            messages=messages,
            response_format=STRAIN_RESPONSE_FORMAT,
        )
        msg = resp.choices[0].message
        content = getattr(msg, "content", None) or ""
        try:
            parsed = json.loads(content)
        except json.JSONDecodeError:
            return {"gse": gse, "error": "JSON parse failure", "raw_content": content, "model": MODEL}
    else:
        # OpenAI-compatible providers: tool-use path (Together's grammar engine etc.)
        resp = _completion_with_retry(
            client,
            model=MODEL,
            max_tokens=max_tokens,
            temperature=0,
            messages=messages,
            tools=[OPENAI_STRAIN_TOOL],
            tool_choice={"type": "function", "function": {"name": "report_strains"}},
        )
        msg = resp.choices[0].message
        parsed = _extract_tool_call(msg)
        if parsed is None:
            return {
                "gse": gse,
                "error": "no tool_call / parseable JSON in response",
                "raw_content": getattr(msg, "content", None),
                "model": MODEL,
            }
    return {
        "gse": gse,
        "strains": parsed.get("strains", []),
        "usage": {
            "input_tokens": resp.usage.prompt_tokens,
            "output_tokens": resp.usage.completion_tokens,
        },
        "stop_reason": resp.choices[0].finish_reason,
        "model": MODEL,
        "provider": PROVIDER,
        "system_fingerprint": getattr(resp, "system_fingerprint", None),
    }


if __name__ == "__main__":
    gse = sys.argv[1] if len(sys.argv) > 1 else "GSE9471"
    print(json.dumps(annotate(gse), indent=2))
