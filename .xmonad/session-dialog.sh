#!/bin/bash

sel=$(zenity --list --title="Exit Options" --text="Make your selection" --column= "Выйти" "Перезагрузить" "Выключить")

if [ "$sel" = "Выйти" ]; then
	pkill -15 -u `whoami`
elif [ "$sel" = "Перезагрузить" ]; then
	/usr/bin/dbus-send --system --print-reply --dest="org.freedesktop.ConsoleKit" /org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Restart
elif [ "$sel" = "Выключить" ]; then
	/usr/bin/dbus-send --system --print-reply --dest="org.freedesktop.ConsoleKit" /org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Stop
fi

exit 0