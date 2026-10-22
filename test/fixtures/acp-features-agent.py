"""ACP feature peer for tests: no credentials, models or network."""
import json
import sys
from pathlib import Path

state_path = Path(sys.argv[1])
state = {"requests": [], "responses": []}
prompt_id = None
flag = True


def emit(value):
    print(json.dumps({"jsonrpc": "2.0", **value}), flush=True)


def options():
    return [{"id": "flag", "name": "Flag", "type": "boolean", "currentValue": flag},
            {"id": "engine", "name": "Model", "type": "select", "category": "model",
             "currentValue": "fake-model", "options": [{"value": "fake-model", "name": "Fake model"}]}]


for line in sys.stdin:
    req = json.loads(line)
    if "method" not in req:
        state["responses"].append(req)
        state_path.write_text(json.dumps(state))
        if prompt_id and len(state["responses"]) == 2:
            emit({"id": prompt_id, "result": {"stopReason": "end_turn"}})
        continue
    state["requests"].append(req)
    state_path.write_text(json.dumps(state))
    method = req["method"]
    if method == "initialize":
        result = {"protocolVersion": 1, "agentInfo": {"name": "fixture", "title": "Test Agent"},
                  "agentCapabilities": {"promptCapabilities": {"image": True}}}
    elif method == "session/new":
        result = {"sessionId": "features", "configOptions": options()}
    elif method == "session/set_config_option":
        flag = req["params"]["value"]
        result = {"configOptions": options()}
    elif method == "session/prompt":
        prompt_id = req["id"]
        emit({"method": "session/update", "params": {"sessionId": "features", "update": {
            "sessionUpdate": "session_info_update", "title": "Image discussion"}}})
        emit({"id": 700, "method": "session/request_permission", "params": {
            "sessionId": "features", "toolCall": {"toolCallId": "test-tool", "title": "Test permission"},
            "options": [{"optionId": "allow", "kind": "allow_once", "name": "Allow"}]}})
        emit({"id": 701, "method": "elicitation/create", "params": {
            "sessionId": "features", "mode": "form", "message": "Choose a strategy", "requestedSchema": {
                "type": "object", "properties": {"strategy": {"type": "string", "enum": ["safe", "fast"]}},
                "required": ["strategy"]}}})
        continue
    elif method == "session/cancel":
        continue
    else:
        emit({"id": req["id"], "error": {"code": -32601, "message": "Unknown method"}})
        continue
    emit({"id": req["id"], "result": result})
