#!/bin/sh


# source: https://askubuntu.com/a/208230/89663

#
#    To have this automatically execute upon startup
#    place the following line in the crontab:
#
#        @reboot /home/mperdikeas/repos/prj/environment/local-startup-apps-ThinkStation-P320/change-microsoft-mouse-acceleration.sh
#
xinput --set-prop 14 340 1


# https://chat.deepseek.com/a/chat/s/255f01aa-d93c-4957-addd-52305269dcaa

# $ xinput list
# $ xinput list props <device-id>
