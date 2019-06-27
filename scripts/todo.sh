#! /bin/bash

# Prompt the user for some input using dmenu
# ung.py does some channable-specific url shortening
content_ung=$(dmenu -p "Todo in #Work" <&- | ~/channable/scripts/ung.py)

# Content will be empty if the user cancelled
if [[ ! -z "$content_ung" ]]; then

    # Relies on todoist go app. See note 'Todoist CLI'.
    ~/opensource/todoist/bin/todoist quick "#Work today $content_ung"

fi


