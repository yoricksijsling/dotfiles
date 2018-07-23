#!/bin/bash

function get-focussed-window()
{
    i3-msg -t get_tree | jq -r ".. | select(.focused? == true).window_properties.class"
}

function i3-move()
{
    i3-msg focus "$1"
}

function emacs-move()
{
    emacsclient -e "(windmove-$1)"
}

function perform-move()
{
    focussed_workspace=$(get-focussed-window)
    if [ "$focussed_workspace" = "Emacs" ]; then
        emacs-move "$1"
        result=$?
        # emacsclient -e "(message \"$result\")"
        if [ $result -ne 0 ]; then
            i3-move "$1"
        fi
    # elif [ "$focussed_workspace" = "Firefox" ]; then
    #     case "$1" in
    #         left)
    #             wmctrl -a firefox; xdotool --key Ctrl+Shift+Tab;;
    #         right)
    #             wmctrl -a firefox; xdotool key Ctrl+Tab;;
    #         *) i3-move "$1";;
    #     esac
    else
        i3-move "$1"
    fi
}

case "$1" in
    left) ;&
    right) ;&
    up) ;&
    down)
        perform-move "$1";;
    *) echo "command not found";;
esac
