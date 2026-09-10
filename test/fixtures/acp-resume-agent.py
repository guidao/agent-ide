"""Minimal persistent ACP peer for local lifecycle tests; no model or network."""

import json
import sys
from pathlib import Path


state_path = Path(sys.argv[1])
capability = sys.argv[2]
state = json.loads(state_path.read_text()) if state_path.exists() else {
    "requests": [], "messages": [], "created": False
}


def emit(value):
    print(json.dumps({"jsonrpc": "2.0", **value}), flush=True)


def update(kind, text, message_id):
    emit({"method": "session/update", "params": {
        "sessionId": "fixture-session",
        "update": {"sessionUpdate": kind, "messageId": message_id,
                   "content": {"type": "text", "text": text}},
    }})


for line in sys.stdin:
    request = json.loads(line)
    method = request["method"]
    params = request.get("params", {})
    state["requests"].append(method)
    result = {}
    if method == "initialize":
        caps = {}
        if capability in ("both", "resume"):
            caps["sessionCapabilities"] = {"resume": {}}
        if capability in ("both", "load"):
            caps["loadSession"] = True
        result = {"protocolVersion": 1, "agentCapabilities": caps}
    elif method == "session/new":
        state["created"] = True
        result = {"sessionId": "fixture-session"}
    elif method in ("session/load", "session/resume", "session/prompt"):
        if not state["created"] or params["sessionId"] != "fixture-session":
            emit({"id": request["id"], "error": {
                "code": -32000, "message": "Session not found"}})
            continue
        if method == "session/load":
            for index, (kind, text) in enumerate(state["messages"]):
                update(kind, text, str(index))
        elif method == "session/prompt":
            prompt = params["prompt"][0]["text"]
            answer = "Answer: " + prompt
            state["messages"].extend([
                ("user_message_chunk", prompt), ("agent_message_chunk", answer)
            ])
            update("agent_message_chunk", answer, str(len(state["messages"]) - 1))
            result = {"stopReason": "end_turn"}
    state_path.write_text(json.dumps(state))
    if "id" in request:
        emit({"id": request["id"], "result": result})
