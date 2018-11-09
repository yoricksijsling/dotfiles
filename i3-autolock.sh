#!/bin/bash

# Take a screenshot
scrot /tmp/screen_locked.png

# Pixellate it 10x
# mogrify -scale 10% -scale 1000% /tmp/screen_locked.png
# mogrify -charcoal 5 -blur 0x10 /tmp/screen_locked.png
# mogrify -blur 0x20 /tmp/screen_locked.png
mogrify -scale 50% -blur 0x10 -scale 200% /tmp/screen_locked.png

# Lock screen displaying this image.
i3lock -i /tmp/screen_locked.png

# Turn the screen off after a delay.
sleep 60; pgrep i3lock && xset dpms force off
