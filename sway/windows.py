import subprocess
import json
import sys

if len(sys.argv) > 1:
    workspace = sys.argv[1].split()[0]
    subprocess.run(["swaymsg", "workspace", workspace], stdout=subprocess.DEVNULL)
    exit(0)

windows = {}


def listwin():
    proc = subprocess.Popen([
        "swaymsg",
        "-t",
        "get_tree"
    ], stdout=subprocess.PIPE)

    tree = json.loads(proc.stdout.read())

    for node in tree["nodes"]:
        get_children(node)

    ids = list(windows.keys())
    ids.sort()
    for id in ids:
        for win in windows[id]:
            print(id, "-", win["class"], "-", win["name"])


def get_children(node, workspace=None):
    if "class" in node:
        window_list = []
        if workspace in windows:
            window_list = windows[workspace]
        window = {
            "workspace": workspace,
            "name": node["name"],
            "class": node["class"],
        }
        if node["class"] is None:
            window["class"] = node["app_id"]

        window_list.append(window)
        windows[workspace] = window_list

    if node["type"] == "workspace":
        workspace = node["name"]

    if type(node["nodes"]) is list:
        for child in node["nodes"]:
            get_children(child, workspace)

    if type(node["floating_nodes"]) is list:
        for child in node["floating_nodes"]:
            get_children(child, workspace)


listwin()
