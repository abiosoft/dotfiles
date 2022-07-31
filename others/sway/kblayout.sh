#!/usr/bin/env bash

# get list of inputs with the following and pass it as first arg to this command.
#   swaymsg -t get_inputs

KB=${1:-"1452:615:Apple_Inc._Magic_Keyboard"}

echo setting keyboard layout for $KB...

# uncomment which line works
# swaymsg input "$KB" xkb_options ctrl:swap_lwin_lctl #Apple Keyboard
# swaymsg input "$KB" xkb_options ctrl:swap_lalt_lctl #PC keyboard
