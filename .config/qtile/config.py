# -*- coding: utf-8 -*-
import os
import socket
from libqtile import bar, layout, widget
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal

colors = {
"border_active": "33b1ff", #  #33b1ff
"border_inactive": "484848", # #484848 is grey.
"tree_background": "161616",
"tree_active_bg": "484848",
"tree_active_fg": "be95ff",
"tree_inactive_bg":"282828",
"tree_inactive_fg": "e4e4e5"
}

mod = "mod4"
terminal = "alacritty"
myBrowser = "firefox"

keys = [
    # A list of available commands that can be bound to keys can be found
    # at https://docs.qtile.org/en/latest/manual/config/lazy.html
    # Switch between windows
    Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),
    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(), desc="Move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),
    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key([mod, "control"], "h", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key([mod, "control"], "l", lazy.layout.grow_right(), desc="Grow window to the right"),
    Key([mod, "control"], "j", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),


    Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),
    Key([mod, "shift"], "Return", lazy.spawncmd(), desc="Spawn a command using a prompt widget" ),

        # Toggle between different layouts as defined below
    Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod], "w", lazy.window.kill(), desc="Kill focused window"),
    Key([mod, "control"], "r", lazy.reload_config(), desc="Reload the config"),
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod], "r", lazy.spawncmd(), desc="Spawn a command using a prompt widget"),
]

groups = [Group(i) for i in "123456789"]

for i in groups:
    keys.extend(
        [
            # mod1 + letter of group = switch to group
            Key(
                [mod],
                i.name,
                lazy.group[i.name].toscreen(),
                desc="Switch to group {}".format(i.name),
            ),
            # mod1 + shift + letter of group = switch to & move focused window to group
            Key(
                [mod, "shift"],
                i.name,
                lazy.window.togroup(i.name, switch_group=True),
                desc="Switch to & move focused window to group {}".format(i.name),
            ),
            # Or, use below if you prefer not to switch to that group.
            # # mod1 + shift + letter of group = move focused window to group
            # Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
            #     desc="move focused window to group {}".format(i.name)),
        ]
    )

layout_theme = {"border_width": 3,
                "margin": 10,
                "border_focus": colors["border_active"],
                "border_normal": colors["border_inactive"],
                }

layouts = [
    layout.Max(**layout_theme),
    layout.MonadTall(**layout_theme),
    layout.RatioTile(**layout_theme),
    layout.TreeTab(
    font = "Ubuntu",
    fontsize = 12,
    sections = ["FIRST", "SECOND", "THIRD", "FOURTH"],
    section_fontsize=12,
        border_width=2,
        bg_color= colors["tree_background"],
        active_bg = colors["tree_active_bg"],
        active_fg = colors["tree_active_fg"],
        inactive_bg = colors["tree_inactive_bg"],
        inactive_fg = colors["tree_inactive_fg"],
        padding_left = 0,
        padding_x = 0,
        padding_y = 5,
        section_top = 10,
        section_bottom = 20,
        level_shift=8,
        vspace=3,
        panel_width = 200,
    ),
    layout.VerticalTile(**layout_theme),
    layout.Stack(num_stacks=4,**layout_theme),

    # layout.Columns(**layout_theme)
    # layout.Bsp(**layout_theme),
    # layout.Matrix(**layout_theme),
    # layout.MonadWide(**layout_theme),
    # layout.Tile(shift_windows=True, **layout_theme),
    # layout.Zoomy(**layout_theme),
]

colors = [
["#282828", "#484848"],
["#ee5396", "#f16da6"],
["#25be6a", "#46c880"],
["#08bdba", "#2dc7c4"],
["#78a9ff", "#8cb6ff"],
["#be95ff", "#c8a5ff"],
["#33b1ff", "#52bdff"],
["#dfdfe0", "#e4e4e5"],
]

prompt = "{0}@{1}: ".format(os.environ["USER"], socket.gethostname())

widget_defaults = dict(
    font="Ubuntu Bold",
   fontsize = 12,
    padding = 2,
    background=colors[0]
)

standard_separator = widget.Sep(
    linewidth=0,
    padding=6,
    foreground=colors[0],
    background=colors[4]
)
def init_widget_list():
    widgets_list = [
        standard_separator,
        widget.Image(
            filename = "~/.config/qtile/icons/python-white.png",
            scale="False",
            mouse_callbacks = {"Button1": lambda: qtile.cmd_spawn(myTerm)}
        ),
        standard_separator,
        widget.GroupBox(
            font = "Ubuntu Bold",
            fontsize=11,
            margin_y = 3,
            margin_x = 0,
            padding_y = 5,
            padding_x = 3,
            borderwidth=3,
            active = colors[5],
            inactive = colors[6],
            rounded=True,
            highlight_color = colors[1],
            highlight_method = "line",
            this_current_screen_border = colors[3],
            this_screen_border = colors[4],
            other_current_screen_border = colors[2],
            other_screen_border=colors[1],
            foreground=colors[2],
            background=colors[0]
        )
        ]

if __name__ in ["config", "__main__"]:
    screens = Screen(top=bar.Bar(widgets=init_widget_list(), opacity=1.0, size=2.0))
    widgets_list = init_widget_list()

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
    ]
)
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
