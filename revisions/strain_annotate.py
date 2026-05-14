"""Annotate mouse strains in a GEO experiment using Claude with structured tool output."""
import json
import os
import subprocess
import sys
from typing import Optional

import anthropic

from geo_fetch import build_input

MODEL = os.environ.get("CLAUDE_MODEL", "claude-sonnet-4-6")
STRAIN_LIST_PATH = "revisions/data/strain_list.json"
PROMPT_PATH = os.environ.get("STRAIN_PROMPT_PATH", "revisions/strain_prompt.txt")


def get_api_key() -> str:
    if "ANTHROPIC_API_KEY" in os.environ:
        return os.environ["ANTHROPIC_API_KEY"]
    out = subprocess.check_output(
        ["security", "find-generic-password", "-s", "ANTHROPIC_API_KEY", "-w"],
        text=True,
    ).strip()
    return out


STRAIN_TOOL = {
    "name": "report_strains",
    "description": (
        "Report the mouse strains used for differential expression analysis in the experiment. "
        "Use an empty array if no strains can be identified from the provided list."
    ),
    "input_schema": {
        "type": "object",
        "additionalProperties": False,
        "required": ["strains"],
        "properties": {
            "strains": {
                "type": "array",
                "items": {
                    "type": "object",
                    "additionalProperties": False,
                    "required": ["value", "URI", "quote"],
                    "properties": {
                        "value": {
                            "type": "string",
                            "description": "Name of the mouse strain, must match a value in the provided list.",
                        },
                        "URI": {
                            "type": "string",
                            "description": "URI of the mouse strain, must match a URI in the provided list.",
                        },
                        "quote": {
                            "type": "array",
                            "items": {"type": "string"},
                            "description": "Verbatim quotes from the input that justify this decision.",
                        },
                    },
                },
            }
        },
    },
}


def build_system_prompt() -> str:
    with open(PROMPT_PATH) as f:
        prompt = f.read().rstrip()
    with open(STRAIN_LIST_PATH) as f:
        strain_list = json.load(f)
    return prompt + "\n" + json.dumps(strain_list)


def annotate(gse: str, client: Optional[anthropic.Anthropic] = None, max_tokens: int = 2048) -> dict:
    if client is None:
        client = anthropic.Anthropic(api_key=get_api_key())
    sys_prompt = build_system_prompt()
    user_input = build_input(gse)
    kwargs = dict(
        model=MODEL,
        max_tokens=max_tokens,
        system=sys_prompt,
        tools=[STRAIN_TOOL],
        tool_choice={"type": "tool", "name": "report_strains"},
        messages=[{"role": "user", "content": json.dumps(user_input)}],
    )
    # Opus 4.7 deprecates temperature; Sonnet 4.6 / Haiku 4.5 still accept it.
    if not MODEL.startswith("claude-opus-4-7"):
        kwargs["temperature"] = 0
    msg = client.messages.create(**kwargs)
    tool_use = next((b for b in msg.content if b.type == "tool_use"), None)
    if tool_use is None:
        return {"error": "no tool_use in response", "raw": msg.model_dump()}
    return {
        "gse": gse,
        "strains": tool_use.input.get("strains", []),
        "usage": {
            "input_tokens": msg.usage.input_tokens,
            "output_tokens": msg.usage.output_tokens,
        },
        "stop_reason": msg.stop_reason,
        "model": MODEL,
    }


if __name__ == "__main__":
    gse = sys.argv[1] if len(sys.argv) > 1 else "GSE9471"
    result = annotate(gse)
    print(json.dumps(result, indent=2))
