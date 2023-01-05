#!/usr/bin/env bash 

/usr/bin/emacs --daemon &
conky -c $HOME/.config/conky/qtile/farside-01.conkyrc
nm-applet &
nitrogen --restore &
compton --inactive-dim 0.3 --focus-exclude 'class_g = "Rofi"'
