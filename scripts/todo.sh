#! /bin/bash

# Prompt the user for some input using rofi
# ung.py does some channable-specific url shortening
content_ung=$(rofi -dmenu -p "Todo" -filter "#Work tod " -lines 0 -width 90 | ~/channable/scripts/ung.py)

# Content will be empty if the user cancelled
if [[ ! -z "$content_ung" ]]; then

    # Relies on todoist go app. See note 'Todoist CLI'.
    ~/opensource/todoist/bin/todoist quick "$content_ung"

fi


