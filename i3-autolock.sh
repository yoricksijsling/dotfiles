#!/bin/bash

# The structure of this script is taken from /usr/share/doc/xss-lock/transfer-sleep-lock-i3lock.sh
#
# Run with:
#
#     xss-lock -l -- /home/yorick/dotfiles/i3-autolock.sh
#

# Take a screenshot (bmp is faster than png)
scrot -o /tmp/screen_locked.bmp

# Blur it using one of these methods
blur_method=$(($RANDOM % 5))
case $blur_method in
    # Pixellate 10x
    0) blur_options="-scale 10% -scale 1000%" ;;
    # Blur
    1) blur_options="-virtual-pixel Mirror -scale 50% -blur 0x10 -scale 200%" ;;
    # Random pixels nearby
    2) blur_options="-virtual-pixel Mirror -spread 20" ;;
    # Edges (via difference eroded and dilated shape). Edge is double the size of kernel.
    3) blur_options="-morphology Edge Disk:4 -negate" ;;
    # Pixelate and edges
    4) blur_options="-scale 10% -scale 1000% -morphology Edge Disk:3 -negate" ;;
esac
# Storing as bmp is faster, but i3lock needs a png. The lower compression level helps at least.
convert /tmp/screen_locked.bmp $blur_options -define png:compression-level=0 /tmp/screen_locked.png

# DPMS off after 10 seconds. Turns off monitors and spotify.
xset dpms 0 0 10

# Suspending is a separate thing, I'm using autosuspend.sh for that.

# Lock screen uses this image.
i3lock_options="-i /tmp/screen_locked.png"

# We set a trap to kill the locker if we get killed, then start the locker and
# wait for it to exit. The waiting is not that straightforward when the locker
# forks, so we use this polling only if we have a sleep lock to deal with.
if [[ -e /dev/fd/${XSS_SLEEP_LOCK_FD:--1} ]]; then
    kill_i3lock() {
        pkill -xu $EUID "$@" i3lock
    }

    trap kill_i3lock TERM INT

    # we have to make sure the locker does not inherit a copy of the lock fd
    i3lock $i3lock_options {XSS_SLEEP_LOCK_FD}<&-

    # now close our fd (only remaining copy) to indicate we're ready to sleep
    exec {XSS_SLEEP_LOCK_FD}<&-

    while kill_i3lock -0; do
        sleep 0.5
    done
else
    trap 'kill %%' TERM INT
    i3lock -n $i3lock_options &
    wait
fi

# After the lock has finished, disable the timer that turns everything off
xset dpms 0 0 0
