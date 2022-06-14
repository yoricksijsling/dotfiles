{ config, lib, pkgs, ... }:

{
  services = {
    polybar = {
      enable = true;
      package = pkgs.polybar.override { i3Support = true; pulseSupport = true; };

      # We can specify a script and then we'll get a polybar
      # service. This kind of works, but it starts with an environment
      # that doesn't have our full PATH available. We start polybar
      # from the i3 config instead.
      script = "";

      extraConfig = ''
[colors]
background = #222
background-alt = #444
foreground = #dfdfdf
foreground-alt = #999
primary = #ffb52a
secondary = #ff5555
alert = #ff5555

good = #55aa55
degraded = #f5a70a
bad = #ff5555

[bar/common]
monitor = ''${env:MONITOR:}

width = 100%
height = 20
fixed-center = true

enable-ipc = true

background = ''${colors.background}
foreground = ''${colors.foreground}

bottom = true

line-size = 3
line-color = #f00

border-bottom-size = 0
border-color = #00000000

padding-left = 0
padding-right = 1

module-margin-left = 1
module-margin-right = 1

; https://github.com/polybar/polybar/wiki/Fonts
font-0 = Comic Sans MS:pixelsize=9;1
font-1 = Font Awesome 6 Free Regular:pixelsize=10;1
font-2 = Font Awesome 6 Brands Regular:pixelsize=10;1
font-3 = Font Awesome 6 Free Solid :pixelsize=10;1

tray-position = right
tray-padding = 1

cursor-click = pointer

scroll-up = "#i3.prev"
scroll-down = "#i3.next"

modules-left = i3 xwindow
modules-right = mic pulseaudio  backlight  filesystem memory cpu   date battery powermenu

[bar/primary]
inherit = bar/common
modules-center = notes

[bar/non-primary]
inherit = bar/common
modules-center = notes

[module/xwindow]
type = internal/xwindow
label = %title:0:200:...%

[module/filesystem]
type = internal/fs
interval = 25

mount-0 = /

label-mounted = %mountpoint%: %free%
label-unmounted = %mountpoint%: 
label-unmounted-foreground = ''${colors.foreground-alt}

[module/i3]
type = internal/i3
format = <label-state> <label-mode>
index-sort = true
pin-workspaces = true
wrapping-scroll = false

; Only show workspaces on the same output as the bar
;pin-workspaces = true

label-mode-padding = 1
label-mode-foreground = #000
label-mode-background = ''${colors.primary}

; focused = Active workspace on focused monitor
label-focused = %index%
label-focused-background = ''${colors.background-alt}
label-focused-underline= ''${colors.primary}
label-focused-padding = 1

; unfocused = Inactive workspace on any monitor
label-unfocused = %index%
label-unfocused-padding = 1

; visible = Active workspace on unfocused monitor
label-visible = %index%
label-visible-background = ''${self.label-focused-background}
label-visible-underline = ''${self.label-focused-underline}
label-visible-padding = ''${self.label-focused-padding}

; urgent = Workspace with urgency hint set
label-urgent = %index%
label-urgent-background = ''${colors.alert}
label-urgent-padding = 1

[module/cpu]
type = internal/cpu
interval = 2
; format-prefix = " "
; format-prefix-foreground = ''${colors.foreground-alt}
label = %percentage:2%%

[module/memory]
type = internal/memory
interval = 2
format-prefix = " "
format-prefix-foreground = ''${colors.foreground-alt}
label = %gb_free%

[module/date]
type = internal/date
interval = 1

date = " %Y-%m-%d"
date-alt = " %Y-%m-%d"

time = %H:%M %Z
time-alt = %H:%M:%S %Z

; format-prefix = 
; format-prefix-foreground = ''${colors.foreground-alt}

label = %date% %time%

[module/pulseaudio]
type = internal/pulseaudio

format-volume = <ramp-volume> <label-volume> <bar-volume>
label-volume = %percentage%%
label-volume-foreground = ''${root.foreground}

label-muted = " muted"
label-muted-minlen = 17
label-muted-alignment = center
label-muted-foreground = #666

ramp-volume-0 = 
ramp-volume-1 = 
ramp-volume-2 = 

bar-volume-width = 10
bar-volume-foreground-0 = #55aa55
bar-volume-foreground-1 = #55aa55
bar-volume-foreground-2 = #55aa55
bar-volume-foreground-3 = #55aa55
bar-volume-foreground-4 = #55aa55
bar-volume-foreground-5 = #f5a70a
bar-volume-foreground-6 = #ff5555
bar-volume-gradient = false
bar-volume-indicator = |
; bar-volume-indicator-font = 2
bar-volume-fill = |
; bar-volume-fill-font = 2
bar-volume-empty = |
; bar-volume-empty-font = 2
bar-volume-empty-foreground = ''${colors.foreground-alt}

[module/mic]
type = custom/ipc
hook-0 = /home/yorick/dotfiles/i3-mic.sh watch
initial = 1  ; This refers to hook-0 :( :( :(

[module/notes]
type = custom/script
tail = true
exec = ${pkgs.coreutils}/bin/tail -n1 -f /tmp/notes-sync-status
format-foreground = ''${colors.foreground-alt}
click-left = /home/yorick/dotfiles/i3-notes-sync.sh run

[module/battery]
type = internal/battery
battery = BAT0
adapter = AC

; [module/procwatch]
; type = custom/script
; interval = 1
; exec = /home/yorick/dotfiles/i3-procwatch.sh watch
; color=#FF6666

[module/powermenu]
type = custom/menu

expand-right = true

format-spacing = 1

label-open = 
label-open-foreground = ''${colors.secondary}
label-close = 
label-close-foreground = ''${colors.secondary}
label-separator = |
label-separator-foreground = ''${colors.foreground-alt}

menu-0-0 =  reboot
menu-0-0-exec = reboot
menu-0-1 =  power off
menu-0-1-exec = poweroff
menu-0-2 =  logout
menu-0-2-exec = gnome-session-quit --logout --no-prompt

[module/media]
type = custom/script
exec = playerctl metadata --player playerctld --format "{{ title }} - {{ artist }}" || true
interval = 1
format = <label>
format-prefix = " "
format-prefix-foreground = ''${colors.foreground-alt}
format-suffix = " "
format-suffix-foreground = ''${colors.foreground-alt}
click-left = playerctl play-pause --player playerctld
double-click-left = playerctl next --player playerctld
click-right = playerctl previous --player playerctld

[settings]
screenchange-reload = true

[global/wm]
margin-top = 0
margin-bottom = 0

; vim:ft=dosini

'';
    };
  };
}
