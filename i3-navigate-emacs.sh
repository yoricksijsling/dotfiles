#!/bin/bash


function debuginfo()
{
    # echo "$1" >> ~/i3-nav-emacs-log
    return 0
}

function get-focussed-window()
{
    i3-msg -t get_tree | jq -r ".. | select(.focused? == true).window_properties.class"
}

function i3-move()
{
    debuginfo "i3-move \"$1\""
    i3-msg focus "$1"
}

function emacs-move()
{
    debuginfo "emacs-move \"$1\""

    # This returns "nil" if it fails, or something like "#<window 3 on yorick.el>" if it succeeds
    result=$(emacsclient -e "(ignore-errors (windmove-$1))")

    if [[ $result == "nil" ]]; then
        debuginfo "failed"
        return 1
    else
        debuginfo "OK - moved within emacs"
        return 0
    fi
}

function perform-move()
{
    focussed_workspace=$(get-focussed-window)
    debuginfo "focus = $focussed_workspace"
    if [[ "$focussed_workspace" == "Emacs" ]]; then
        emacs-move "$1" || i3-move "$1"
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
    debuginfo ""
}

case "$1" in
    left) ;&
    right) ;&
    up) ;&
    down)
        perform-move "$1";;
    *) echo "command not found";;
esac
