#!/bin/bash

# This starts a polybar for every monitor.
# Best to call this from your i3 config.

# Terminate already running bar instances
pkill polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

IFS=$'\n'       # make newlines the only separator
for m in $(polybar --list-monitors); do
  # m is something like "HDMI-1: 1920x1080+0+0 (primary)"
  monitor=$(echo "$m" | cut -d":" -f1)
  if [[ "$m" =~ \(primary\)$ ]]; then
    # echo "$monitor primary"
    MONITOR=$monitor polybar --reload primary &
  else
    # echo "$monitor non-primary"
    MONITOR=$monitor polybar --reload non-primary &
  fi
done
