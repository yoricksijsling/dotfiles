#!/bin/bash

# set -euxo pipefail

TMPFILE="$(mktemp -t screencast-XXXXXXX).mkv"
OUTPUT="$HOME/Pictures/Screencasts/$(date +%F-%H-%M-%S)"

read -r X Y W H < <(slop --noopengl --format "%x %y %w %h"; echo "")
ffmpeg -f x11grab -s "${W}x$H" -i ":0.0+$X,$Y" "$TMPFILE"

notify-send 'generating palette'
ffmpeg -y -i "$TMPFILE"  -vf fps=10,palettegen /tmp/palette.png
pkill notify-osd

notify-send 'generating gif'
ffmpeg -i "$TMPFILE" -i /tmp/palette.png -filter_complex "paletteuse" "$OUTPUT.gif"
pkill notify-osd

notify-send 'generating webm'
ffmpeg -i "$TMPFILE" -c:v libvpx-vp9 -b:v 2M "$OUTPUT.webm"
pkill notify-osd

mv "$TMPFILE" "$OUTPUT.mkv"


# notify-send "size $(du -h $OUTPUT.gif | awk '{print $1}')"

# eog "$OUTPUT.gif"

# trap "rm -f '$TMPFILE'" 0
