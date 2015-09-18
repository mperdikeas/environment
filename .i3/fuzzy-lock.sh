#!/usr/bin/env bash

type "scrot" > /dev/null 2>/dev/null
if [[ $? -eq 0 ]] ; then
    scrot_exists=true
else
    scrot_exists=false
fi

type "mogrify" > /dev/null 2>/dev/null
if [[ $? -eq 0 ]] ; then
    mogrify_exists=true
else
    mogrify_exists=false
fi

if $scrot_exists && $mogrify_exists ; then
    scrot_and_mogrify_exist=true
else
    scrot_and_mogrify_exist=false
fi

# requires:
# sudo apt-get install scrot
# sudo apt-get install imagemagick

image_to_use=~/.i3/lock-fallback.png

if [ "$scrot_and_mogrify_exist" = true ] ; then
    image_to_use=/tmp/screen-locked.png
    # Take a screenshot
    scrot "$image_to_use"
    # Pixellate it 10x
    mogrify -scale 10% -scale 1000% "$image_to_use"
    # Lock screen displaying this image.
fi
i3lock -t -i "$image_to_use"
# Turn the screen off after a delay.
sleep 600; pgrep i3lock && xset dpms force off
