#!/bin/bash

# http://askubuntu.com/a/261076/89663
# http://askubuntu.com/q/452826/89663

# copy this to /etc/pm/sleep.d/

case "$1" in
    suspend)
        nmcli nm wifi off
        ;;
    resume)
        nmcli nm wifi on
        ;;
    *)
        ;;
esac
