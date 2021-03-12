#!/bin/bash

# Automatically commit and push notes repo.
#
# Make sure that the daemon always runs, e.g. by adding this to your i3 config:
#
#     exec --no-startup-id /home/yorick/dotfiles/i3-notes-sync.sh daemon
#
# You can manually trigger a sync:
#
#     /home/yorick/dotfiles/i3-notes-sync.sh run
#
# Status updates go to $STATUS_FILE, in a format suitable for polybar. Polybar config:
#
#     [module/notes]
#     type = custom/script
#     tail = true
#     exec = tail -n1 -f /tmp/notes-sync-status
#     format-foreground = ${colors.foreground-alt}
#     click-left = /home/yorick/dotfiles/i3-notes-sync.sh run
#

set -euo pipefail


STATUS_FILE=/tmp/notes-sync-status
NOTES_FOLDER="$HOME/notes"

# After changes are made, a sync will occur within this time frame:
SYNC_DELAY_AFTER_CHANGES="10 second"

# Regardless of changes, syncs will on this timer:
SYNC_TIMER="2 minute"

UP_COLOR='#55aa55'
RUNNING_COLOR='#ffb52a'


function daemon()
{
    cd "$NOTES_FOLDER"
    local will_sync_at=""
    local change_count
    local early_sync_time
    local now

    will_sync_at=$(date -ud "$SYNC_TIMER" +%s)
    echo "$(date +%s): Planning timer sync at $will_sync_at"

    while true
    do
        now=$(date +%s)
        if [[ $now > $will_sync_at ]]; then
            echo "$now: Going for sync"
            run
            will_sync_at=$(date -ud "$SYNC_TIMER" +%s)
            echo "$now: Planning timer sync at $will_sync_at"
        else
            change_count=$(change-count)
            echo "$now: change count $change_count"
            if [[ $change_count != 0 ]]; then
                # There are changes, make sure that a sync will happen soon.
                early_sync_time=$(date -ud "$SYNC_DELAY_AFTER_CHANGES" +%s)
                if [[ $early_sync_time < $will_sync_at ]]; then
                    will_sync_at="$early_sync_time"
                    echo "$now: Planning sync due to changes at $will_sync_at"
                    post-status "$RUNNING_COLOR" "NOTES"
                fi
            else
                post-status "$UP_COLOR" "NOTES"
            fi
        fi
        sleep 2  # seconds
    done
}

function post-status()
{
    echo "%{u$1}%{+u}$2%{u-}" >> "$STATUS_FILE"
}

function change-count()
{
    local gstatus
    gstatus=$(git status --porcelain)
    echo "${#gstatus}"
}

function run()
{
    cd "$NOTES_FOLDER"
    if [[ $(change-count) == 0 ]]; then
        post-status "$RUNNING_COLOR" "PULL"
	git pull --ff-only
        post-status "$UP_COLOR" "NOTES"
    else
        git add --all
        git commit -m "Notes sync"
        post-status "$RUNNING_COLOR" "PULL"
	git pull --ff-only
        post-status "$RUNNING_COLOR" "PUSH"
        git push
        post-status "$UP_COLOR" "NOTES"
    fi
}

command=$1
shift

case "${command}" in
    "daemon") daemon;;
    "run") run;;
    *)
        echo "Unknown command $command"
        show-help
        exit 1
    ;;
esac
