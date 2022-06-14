#!/bin/bash

# scrot -s - | xclip -selection clipboard -target image/png

FILE="/tmp/i3-screen-capture-clipboard.png"

read -r X Y W H < <(slop --noopengl --format "%x %y %w %h"; echo "")
scrot -a "$X,$Y,$W,$H" --overwrite "$FILE"
xclip -selection clipboard -target image/png -i "$FILE"
rm "$FILE"
