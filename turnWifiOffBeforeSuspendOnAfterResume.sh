#!/bin/bash

# http://askubuntu.com/a/261076/89663
# http://askubuntu.com/q/452826/89663

# copy this to /etc/pm/sleep.d/

case "$1" in
    suspend)
    # Ubuntu 14.04: nmcli nm wifi off
        nmcli r wifi off
        ;;
    resume)
        # Ubuntu 14.04: nmcli nm wifi on
        nmcli r wifi on
        ;;
    *)
        ;;
esac
