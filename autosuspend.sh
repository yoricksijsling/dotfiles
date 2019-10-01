#!/bin/bash

# Automatically suspend if i3lock has been on for 5 minutes
#
# In the current setup, i3lock is started via xss-lock when the screensaver
# triggers.
#

suspend_time=""
while true
do
    if [[ $(pgrep i3lock) ]]; then
        if [[ $suspend_time ]]; then
            if [[ $(date +%s) -ge $suspend_time ]]; then
                # Timer will start over after resuming from suspend
                suspend_time=""
                # echo "SUSPEND $(date +%s)"
                systemctl suspend
            # else
            #     echo "no suspend $(date +%s)"
            fi
        else
            suspend_time=$(date -ud "10 minute" +%s)
            # echo "Set suspend_time $suspend_time"
        fi
    else
        suspend_time=""
    fi
    sleep 10
done
