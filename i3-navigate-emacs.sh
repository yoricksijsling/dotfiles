#!/bin/bash

# Based on https://gist.github.com/mijoharas/b9d09daed9654ca8d0d081015209ecd0

function debuginfo()
{
    # echo "$1" >> ~/i3-nav-emacs-log
    return 0
}

function get-focused-window-class()
{
    i3-msg -t get_tree | jq -r ".. | select(.focused? == true).window_properties.class"
}
function get-focused-window()
{
    i3-msg -t get_tree | jq -r ".. | select(.focused? == true).window"
}

function i3-move()
{
    debuginfo "i3-move \"$1\""
    i3-msg focus "$1"
}

function emacs-move()  # window_id, direction
{
    debuginfo "emacs-move \"$1\" \"$2\""

    # If there are multiple emacs frames on one screen, emacsclient picks the one under the mouse
    # cursor. That's not always the currently focused window. Luckily we have a window id from i3
    # that we can use to find the right frame. Requires dash.
    elisp_selected_frame="(--first (string-equal \"$1\" (frame-parameter it 'outer-window-id)) (frame-list))"
    elisp_move="(with-selected-frame $elisp_selected_frame (windmove-$2))"

    # This returns "nil" if it fails, or something like "#<window 3 on foo.el>" if it succeeds
    result=$(emacsclient -e "(ignore-errors $elisp_move)")

    if [[ $result == "nil" ]]; then
        debuginfo "emacs-move failed"
        return 1
    else
        debuginfo "OK - moved within emacs"
        return 0
    fi
}

function perform-move()
{
    focused_window_class=$(get-focused-window-class)
    focused_window=$(get-focused-window)

    debuginfo "focused_window_class = $focused_window_class"
    debuginfo "focused_window = $focused_window"

    if [[ "$focused_window_class" == "Emacs" ]]; then
        emacs-move "$focused_window" "$1" || i3-move "$1"
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
