#!/bin/bash

# Use this script to show microphone status in i3 blocks
#
# Enable/disable microphone:
#
#   amixer -D pulse sset Capture cap
#   amixer -D pulse sset Capture nocap
#

function watch()
{
    local status
    status=$(amixer -D pulse sget Capture | grep -o '\[\(off\|on\)\]$' | head -n1)
    case "$status" in
        "[off]")
            echo -e "Mic off";;
        "[on]")
            echo "Mic ON"
            echo "Mic ON"
            echo "#FF6666"
            ;;
        *) echo "mic ?";;
    esac
}

function set_capture()
{
    amixer -q -D pulse sset Capture "$1"
    # Tell i3blocks to refresh this widget
    pkill -RTMIN+12 i3blocks
}

case "$1" in
    "watch") watch;;
    "toggle") set_capture "toggle";;
    "on") set_capture "cap";;
    "off") set_capture "nocap";;
    *) exit 1;;
esac

