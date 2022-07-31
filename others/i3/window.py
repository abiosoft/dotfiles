import subprocess
import sys
import argparse
import time


def parse_cli():
    parser = argparse.ArgumentParser()
    command_group = parser.add_mutually_exclusive_group()
    command_group.add_argument("--window-count",
                               dest="window_count",
                               action="store_true",
                               help="show number of windows on current workspace")
    command_group.add_argument("--close-window",
                               dest="close_window",
                               action="store_true",
                               help="close the currently active window",
                               )
    command_group.add_argument("--window-switcher",
                               dest="window_switcher",
                               action="store_true",
                               help="show window switcher for all windows",
                               )
    command_group.add_argument("--window-switcher-current",
                               dest="window_switcher_current",
                               action="store_true",
                               help="show window switcher for current workspace",
                               )
    command_group.add_argument("--move-window",
                               dest="move_window",
                               action="store_true",
                               help="move active window to other position or workspace",
                               )
    command_group.add_argument("--move-display",
                               dest="move_display",
                               action="store_true",
                               help="move window or workspace to display",
                               )

    args = parser.parse_args()

    if args.window_count:
        print(get_window_count())
    elif args.close_window:
        code = close_dialog()
        if code is -1:
            print("failed getting active window", file=sys.stderr)
            exit(1)
        elif code is 0:
            close_i3_window()
    elif args.window_switcher:
        show_window_switcher(current_workspace=False)
    elif args.window_switcher_current:
        show_window_switcher()
    elif args.move_window:
        move_window()
    elif args.move_display:
        move_display()
    else:
        parser.print_usage()


class Window:

    def __init__(self, id: str):
        self.id = id
        self.title = ''
        self.app = ''
        self.x = 0
        self.y = 0
        self.width = 0
        self.height = 0


def get_windows(workspace: str = None) -> list:
    if workspace is None:
        workspace = get_workspace()

    cmd = subprocess.Popen(["wmctrl", "-lx"], stdout=subprocess.PIPE)
    lines = cmd.stdout.readlines()
    windows = []
    for line in lines:
        cols = line.decode().split(None, 3)
        if cols[1] is not workspace:
            continue
        app = cols[2].split(".")[1]
        title = cols[-1].strip()
        id = cols[0]

        window = Window(id)
        window.app = app
        window.title = title
        windows.append(window)
    return windows


def move_window():
    # i3 enable mode
    cmd = subprocess.run(["i3-msg",
                          "mode",
                          "move_window"])
    if cmd.returncode is not 0:
        print("error enabling i3 mode for window window movement", file=sys.stderr)
        return
    msg = """
    Click on any window to activate and use the following keys to move window.

    Arrow Keys    
        Move window within visible workspaces.    

    Shift+Left , Shift+Right   
        Move window to other workspaces on current display.

    Shift+Up 
        Move window to new workspace on current display.
    """

    # show info dialog
    args = yad_args("Move Window",
                    msg,
                    "--no-focus",
                    "--button=gtk-cancel:0",
                    "--fixed",
                    )
    if subprocess.run(args).returncode is not 0:
        print("error showing info dialog", file=sys.stderr)

    # i3 disable mode
    subprocess.run(["i3-msg", "mode", "default"])


def move_display():
    # i3 enable mode
    cmd = subprocess.run(["i3-msg",
                          "mode",
                          "move_display"])
    if cmd.returncode is not 0:
        print("error enabling i3 mode for window movement", file=sys.stderr)
        return
    msg = """
    Click on any window to activate and use the following keys to move window.

    Arrow Keys    
        Move window to display.    

    Shift+Left , Shift+Right , Shift+Up , Shift+Down 
        Move workspace to display.

    """

    # show info dialog
    args = yad_args("Move Window",
                    msg,
                    "--no-focus",
                    "--button=gtk-cancel:0",
                    "--fixed",
                    )
    if subprocess.run(args).returncode is not 0:
        print("error showing info dialog", file=sys.stderr)

    # i3 disable mode
    subprocess.run(["i3-msg", "mode", "default"])


def yad_args(title: str, message: str, *extra_args)->list:
    args = [
        "yad",
        "--text={}".format(message),
        "--skip-taskbar",
        "--splash",
        "--on-top",
        "--title={}".format(title),
        "--buttons-layout=center",
        "--center",
        "--sticky",
    ]
    for arg in extra_args:
        args.append(arg)

    return args


def get_workspace() -> str:
    cmd = subprocess.Popen(["wmctrl", "-d"], stdout=subprocess.PIPE)
    lines = cmd.stdout.readlines()
    for line in lines:
        cols = line.split()
        if cols[1] is not b'*':
            continue
        return cols[0].decode()
    return None


def close_i3_window():
    cmd = subprocess.run(["i3-msg", "kill"])
    if cmd.returncode is not 0:
        print("cannot close window, exit code {}".format(
            cmd.returncode), file=sys.stderr)


def close_dialog(window_id: str = None) -> int:
    if window_id is None:
        window_id = get_active_window_id()
        if window_id is None:
            return -1
    # focus window
    subprocess.run(["wmctrl", "-i", "-a", window_id])

    window = get_window_pos(window_id)
    close_dialog = subprocess.run([
        "yad",
        "--text=Close this window?",
        "--skip-taskbar",
        "--splash",
        "--on-top",
        "--close-on-unfocus",
        "--title={}".format(window.title),
        "--text-align=center",
        "--buttons-layout=center",
        "--posx={}".format(window.x + window.width / 2),
        "--posy={}".format(window.y + window.height / 2),
    ])
    return close_dialog.returncode


def get_window_pos(window_id: str) -> Window:
    window = Window(window_id)

    cmd = subprocess.Popen(
        ["xwininfo", "-id", window_id], stdout=subprocess.PIPE)
    lines = cmd.stdout.readlines()

    for line in lines:
        line = line.decode()
        value = 0
        cols = line.split()
        if len(cols) > 0:
            try:
                value = int(cols[-1])
            except:
                pass

        if "Absolute upper-left X:" in line:
            window.x = value
        elif "Absolute upper-left Y:" in line:
            window.y = value
        elif "Width:" in line:
            window.width = value
        elif "Height:" in line:
            window.height = value
        elif "xwininfo: Window id: " in line:
            cols = line.split(None, 4)
            window.title = cols[4].strip()[1:-1]

    return window


def get_active_window_id() -> str:
    """
    get the currently active window
    """
    cmd = subprocess.Popen(
        ["xdotool", "getactivewindow"], stdout=subprocess.PIPE)
    code = cmd.wait()
    if code is not 0:
        return None
    return cmd.stdout.read().decode().strip()


def get_window_count(workspace: str = None) -> int:
    """
    get number of windows in the current workspace
    """
    windows = get_windows(workspace)
    return len(windows)


def show_window_switcher(current_workspace: bool = True):
    """
    switch window using rofi
    """
    count = 0
    if current_workspace:
        count = get_window_count()
        if count > 10:
            count = 10
        if count is 0:
            return
    args = [
        "rofi",
        "-show-icons",
        "-location", "1",
        "-window-format", "{c} {t}",
        "-yoffset", "30",
        "-me-select-entry", "",
        "-me-accept-entry", "MousePrimary",
        "-show"
    ]
    if current_workspace:
        args.append("windowcd")
        args.append("-lines")
        args.append("{}".format(count))
    else:
        args.append("window")

    if subprocess.run(args).returncode is not 0:
        print("error showing window switcher", file=sys.stderr)


parse_cli()
