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

function list-potentials()
{
    local current_tty
    current_tty=$(tty | tail -c+6)

    local awkscript='
      BEGIN {
        FIELDWIDTHS = "7 8 13 20"
      }
      {
        tty = $2
        # Started within the last 24 hours:
        # started_recently = $3~"^ +([0-9][0-9]:)?[0-9][0-9]:[0-9][0-9] $"
        # Started within the last 9999 seconds (almost 3 hours):
        started_recently = $3~"^ +[0-9]{1,4} $"
        comm = $4
        if (tty  ~ "^pts" &&
            tty !~ "^"current_tty &&
            tty != printed_tty &&
            started_recently &&
            comm !~ "^bash") {
          print $0
          printed_tty=$2  # Only print the first non-bash process in a tty
        }
      }
      '
    ps a --no-headers --sort=-tty,-etime -o pid:6,tty:7,etimes:12,comm:19 \
        | awk -v current_tty="$current_tty" "$awkscript" \
        | sort -k3
}

function interactive()
{
    local potentials
    readarray -t potentials <<< "$(list-potentials)"

    select choice in "${potentials[@]}"; do
        local pid
        pid=$(echo "$choice" | head -c 7 | tr -d ' ')
        if [[ "$choice" =~ [0-9]+ ]]; then
            register "$pid"
        else
            echo "Invalid choice"
        fi
        exit
    done
}

case "$1" in
    "register") register "$2";;
    "watch") watch;;
    "list-potentials") list-potentials;;
    *) interactive;;
esac
