#!/usr/bin/env bash

# set a cron job to run this script to run every 5 minutes

# sources: https://bbs.archlinux.org/viewtopic.php?id=138755
#          https://faq.i3wm.org/question/1730/warning-popup-when-battery-very-low.1.html

BATTINFO=`acpi -b`
if [[ `echo $BATTINFO | grep Discharging` && `echo $BATTINFO | cut -f 5 -d " "` < 00:15:00 ]] ; then
    DISPLAY=:0.0 /usr/bin/notify-send -t 60000 -u critical "low battery" "$BATTINFO"
    /usr/bin/i3-nagbar -m "Low battery" &
fi






