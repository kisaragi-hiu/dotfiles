# this doesn't work with osu, I'm afraid

# import re
from xkeysnail.transform import define_keymap, define_modmap, \
    define_multipurpose_modmap
from xkeysnail.transform import Key, K, launch

PREFIX = "C-x"


def osu_select_sorting(sorting):
    alist = {
        "artist": "30",
        "bpm": "60",
        "creator": "100",
        "date": "130",
        "difficulty": "170",
        "length": "200",
        "rank": "230",
        "title": "280"
    }
    launch([
        "bash -c 'xdotool mousemove 1750 55;" +
        "xdotool click 1;" +
        "sleep 0.1;" +
        "xdotool mousemove_relative 0 " + alist[sorting] + ";" +
        "xdotool click 1;'"
    ])


# Global Modemap like xmodmap
define_modmap({
    Key.CAPSLOCK: Key.ISO,
    Key.PAGE_UP: Key.COMPOSE  # openbox might grab this
})

# xcape -e <original>=<held> is equal to <original>: [<original> <held>]
define_multipurpose_modmap({
    Key.RIGHT_ALT: [Key.ESC, Key.RIGHT_ALT]
})

define_keymap(lambda wm_class: wm_class in ("osu!.exe"), {
    K("KP1"): K("z"),
    K("KP2"): K("x"),
    K("KP4"): K("TAB"),
    K("KP5"): K("GRAVE"),
    K("KP6"): K("Shift-TAB"),
    K("KP7"): launch(["xdotool click 1"]),
    K("KP8"): K("F2"),
    K(PREFIX): {
        K("o"): {
            K("a"): osu_select_sorting("artist"),
            K("b"): osu_select_sorting("bpm"),
            K("c"): osu_select_sorting("creator"),
            K("d"): osu_select_sorting("date"),
            K("e"): osu_select_sorting("difficulty"),
            K("l"): osu_select_sorting("length"),
            K("r"): osu_select_sorting("rank"),
            K("t"): osu_select_sorting("title")
        }
    }
})
