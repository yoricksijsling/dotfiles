#!/bin/bash

set -euxo pipefail

TMPFILE="$(mktemp -t screencast-XXXXXXX).mp4"
OUTPUT="$HOME/Videos/captured/$(date '+%F %H:%M:%S')"

read -r X Y W H < <(slop --noopengl --format "%x %y %w %h"; echo "")
# sleep 1

# Record using a lossless encoding
ffmpeg -video_size "${W}x$H" -framerate 30 -f x11grab -i "$DISPLAY.0+$X,$Y" -c:v libx264rgb -crf 0 -preset ultrafast "$TMPFILE" || echo "done"

notify-send 'generating palette'
ffmpeg -y -i "$TMPFILE"  -vf fps=10,palettegen /tmp/palette.png
pkill notify-osd
notify-send 'generating gif'
ffmpeg -i "$TMPFILE" -i /tmp/palette.png -filter_complex "paletteuse" "$OUTPUT.gif"
pkill notify-osd

# webm, using VP9 video codec, bitrate target 2M
notify-send 'generating webm'
ffmpeg -i "$TMPFILE" -codec:v libvpx-vp9 -b:v 2M "$OUTPUT.webm"
pkill notify-osd

# a more compact mp4, using x264 codec
notify-send 'generating mp4'
ffmpeg -i "$TMPFILE" -codec:v libx264 "$OUTPUT.mp4"
pkill notify-osd

rm -f "$TMPFILE"


# notify-send "size $(du -h $OUTPUT.gif | awk '{print $1}')"

# eog "$OUTPUT.gif"

# trap "rm -f '$TMPFILE'" 0
