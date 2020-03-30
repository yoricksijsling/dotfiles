#!/bin/bash

#
# Start watching a process using `./i3-procwatch.sh register 1234`
#
# Use `./i3-procwatch.sh watch` in i3blocks to watch processes

shopt -s nullglob

I3_PROCWATCH_DIR="$HOME/.i3-procwatch"

function register()
{
    local pid="$1"
    local proc_name
    proc_name=$(ps -p "$pid" -o comm=)
    echo "$proc_name" > "$I3_PROCWATCH_DIR/$pid"
}

function watch()
{
    local proc_name
    cd "$I3_PROCWATCH_DIR" || exit
    for pid in *; do
        if [[ ! $pid =~ [0-9]+ ]]; then continue; fi
        proc_name=$(cat "$pid")
        if ps -p "$pid">/dev/null; then
            echo -n "$pid $proc_name "
        else
            rm "$pid"
            pkill notify-osd
            notify-send "Finished $pid $proc_name"
        fi
    done
}

case "$1" in
    "register") register "$2";;
    "watch") watch;;
    *) exit 1;;
esac
