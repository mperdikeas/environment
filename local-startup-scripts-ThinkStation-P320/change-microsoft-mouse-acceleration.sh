#!/bin/sh


# source: https://askubuntu.com/a/208230/89663

#
#    To have this automatically execute upon startup
#    place the following line in the crontab:
#
#        @reboot /home/mperdikeas/repos/prj/environment/local-startup-apps-ThinkStation-P320/change-microsoft-mouse-acceleration.sh
#
xinput --set-prop 10 340 1
