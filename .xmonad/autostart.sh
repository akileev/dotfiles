#!/bin/bash

urxvt &
transmission-gtk &
audacious &
thunar --daemon &
thunar &
thunderbird &
pidgin &
chromium &
virtualbox &
skype &

setxkbmap -layout 'us,ru' -option 'grp:alt_shift_toggle, grp_led:scroll'

source ~/.bash_profile