{ config, dotfiles, lib, pkgs, ... }:

# I3, Xsession, and other stuff related to the GUI.
{
  imports = [
    ./polybar.nix
  ];

  programs = {

    rofi = {
      enable = true;

      # For theme options check https://github.com/davatorium/rofi/blob/1.7.3/doc/rofi-theme.5.markdown
      theme = let inherit (config.lib.formats.rasi) mkLiteral; in {
        "*" = {
          font = "Comic Sans MS 14";
          background-color = mkLiteral "#2d303b";
          text-color = mkLiteral "#f3f4f5";
        };
        "window" = {
          padding = 50;
        };
        "element-text selected" = {
          text-color = mkLiteral "#ffffff";
          background-color = mkLiteral "#00000055";  #RGBA
        };
      };

      extraConfig = {
        scroll-method = 1;
        kb-page-next = "Control+v,Page_Down";
        kb-page-prev = "Alt+v,Page_Up";
        kb-secondary-paste = "Control+y";
      };
    };
  };

  # autorandr is outside of home-manager (so installed through apt and
  # manually configured profiles). We do provide the polybar-restart
  # hook from here.
  # TODO: This script isn't working the way I want it to. Maybe xrandr
  # isn't giving the right output yet I'm not sure.. For now I'll just
  # restart i3 manually when I plug/unplug the dock.
#   xdg.configFile.polybar_autorandr = {
#     target = "autorandr/postswitch.d/polybar";
#     text = ''
# #!/bin/bash
# #i3-msg exec /home/yorick/dotfiles/i3-start-polybar.sh
# i3-msg restart
# '';
#     executable = true;
#   };

  # home.pointerCursor = {
  #   name = "Vanilla-DMZ";
  #   package = pkgs.vanilla-dmz;
  #   # name = "Adwaita";
  #   # package = pkgs.gnome.adwaita-icon-theme;
  #   size = 16;
  #   gtk.enable = true;
  #   x11.enable = true;
  # };

  # Picom prevents all the screen tearing and artifacts that you can get with i3
  services.picom = {
    package = pkgs.picom;
    enable = true;
    vSync = true;
    # On my work laptop I need the xrender backend
    backend = "xrender";
  };

  xresources.properties = {
    "Xft.antialias" = "1";
    "Xft.autohint" = "0";
    "Xft.hinting" = "1";
    "Xft.hintstyle" = "hintnone";
    "Xft.lcdfilter" = "lcddefault";
    "Xft.rgba" = "rgb";
  };

  xsession = {
    enable = true;  # Take over ~/.xsession and ~/.xprofile

    initExtra = ''
# Dvorak, with caps lock as super key
setxkbmap dvorak -option caps:super
# This is for use with my apple keyboard, where alt and cmd are the wrong way round
# setxkbmap dvorak -option caps:super -option altwin:swap_lalt_lwin

# Screensaver after 5 minutes
xset s 300

# Swap mouse buttons
# xmodmap -e "pointer = 3 2 1"

# Disable repeat of F1
xset -r 67

# Mouse settings
xinput --set-prop 'PixArt Microsoft USB Optical Mouse' 'libinput Accel Speed' 0.5
#xinput --set-prop 'PixArt Microsoft USB Optical Mouse' 'libinput Natural Scrolling Enabled' 1

xinput --set-prop 'DLL0945:00 04F3:311C Touchpad' 'libinput Natural Scrolling Enabled' 1
'';

    windowManager.i3 = {
      enable = true;

      config = {

        # See http://i3wm.org/docs/userguide.html for a complete reference.
        # To discover names of keys use `xev`.

        # This is the win key. I've rebound caps to win in ~/.xsessionrc.
        modifier = "Mod4";

        fonts = {
          names = ["Comic Sans MS"];
          style = "";
          size = 8.0;
        };

        # Use Mouse+Mod4 to drag floating windows to their wanted position
        floating.modifier = "Mod4";

        # Don't use built-in bars, we use polybar instead
        bars = [];

        # Switch back to previous workspace when switching to current
        workspaceAutoBackAndForth = true;

        # Default monitors for workspaces
        workspaceOutputAssign = [
          # The below combination is messy, but happens to work for home, office
          # and laptop-only setups.
          { workspace="1"; output="primary"; }
          { workspace="2"; output="primary"; }
          { workspace="3"; output="primary"; }
          { workspace="4"; output="primary"; }
          # These should go on the 'secondary' monitor, the first connected
          # monitor from the list is used.
          { workspace="5"; output="DP-1-3 DP-1-2 DP-2-3 DP-2-2 DP-3-3 DP-3-2P eDP-1"; }
          { workspace="6"; output="DP-1-3 DP-1-2 DP-2-3 DP-2-2 DP-3-3 DP-3-2 eDP-1"; }
          { workspace="7"; output="DP-1-3 DP-1-2 DP-2-3 DP-2-2 DP-3-3 DP-3-2 eDP-1"; }
          { workspace="8"; output="DP-1-3 DP-1-2 DP-2-3 DP-2-2 DP-3-3 DP-3-2 eDP-1"; }
          # Always default to laptop
          { workspace="9"; output="eDP-1"; }
          { workspace="10"; output="eDP-1"; }
        ];

        # Reset key bindings and modes, they're provided in extraConfig.
        keybindings = {};
        modes = {};

        # for_window stuff
        window = {
          border = 2;
          titlebar = false;
          # commands = [ { command = "border pixel 1"; criteria = { class = "XTerm"; } ; } ]
        };
      };

      extraConfig = ''
        # start a terminal
        bindsym Mod4+Return exec gnome-terminal --hide-menubar

        # start emacs
        bindsym Mod4+backslash exec emacsclient --create-frame --no-wait -e "(switch-to-buffer (other-buffer (current-buffer)))"

        # kill focused window
        bindsym Mod4+Shift+k kill

        # program launcher, includes applications with a .desktop file
        bindsym Mod4+d exec --no-startup-id rofi -show combi -combi-modi "drun#run"

        # change focus
        bindsym Mod4+b exec --no-startup-id ${dotfiles}/i3-navigate-emacs.sh left
        bindsym Mod4+n exec --no-startup-id ${dotfiles}/i3-navigate-emacs.sh down
        bindsym Mod4+p exec --no-startup-id ${dotfiles}/i3-navigate-emacs.sh up
        bindsym Mod4+f exec --no-startup-id ${dotfiles}/i3-navigate-emacs.sh right

        # move focused window
        bindsym Mod4+Shift+b move left
        bindsym Mod4+Shift+n move down
        bindsym Mod4+Shift+p move up
        bindsym Mod4+Shift+f move right

        # split direction
        bindsym Mod4+h split h
        bindsym Mod4+v split v

        # enter fullscreen mode for the focused container
        bindsym Mod4+u fullscreen toggle

        # change container layout (stacked, tabbed, toggle split)
        bindsym Mod4+s layout stacking
        # bindsym Mod4+t layout tabbed
        bindsym Mod4+e layout toggle split

        # toggle tiling / floating
        bindsym Mod4+Shift+space floating toggle

        # change focus between tiling / floating windows
        bindsym Mod4+space focus mode_toggle

        # focus the parent container
        bindsym Mod4+a focus parent

        # focus the child container
        #bindsym Mod4+d focus child

        # switch to workspace
        bindsym Mod4+1 workspace 1
        bindsym Mod4+2 workspace 2
        bindsym Mod4+3 workspace 3
        bindsym Mod4+4 workspace 4
        bindsym Mod4+5 workspace 5
        bindsym Mod4+6 workspace 6
        bindsym Mod4+7 workspace 7
        bindsym Mod4+8 workspace 8
        bindsym Mod4+9 workspace 9
        bindsym Mod4+0 workspace 10

        # scroll to move through workspaces
        bindsym --whole-window Mod4+button4 workspace next_on_output
        bindsym --whole-window Mod4+button5 workspace prev_on_output

        # move focused container to workspace
        bindsym Mod4+Shift+1 move container to workspace 1
        bindsym Mod4+Shift+2 move container to workspace 2
        bindsym Mod4+Shift+3 move container to workspace 3
        bindsym Mod4+Shift+4 move container to workspace 4
        bindsym Mod4+Shift+5 move container to workspace 5
        bindsym Mod4+Shift+6 move container to workspace 6
        bindsym Mod4+Shift+7 move container to workspace 7
        bindsym Mod4+Shift+8 move container to workspace 8
        bindsym Mod4+Shift+9 move container to workspace 9
        bindsym Mod4+Shift+0 move container to workspace 10

        # reload the configuration file
        bindsym Mod4+Shift+c reload
        # restart i3 inplace (preserves your layout/session, can be used to upgrade i3)
        bindsym Mod4+Shift+r restart
        # exit i3 (logs you out of your X session)
        bindsym Mod4+Shift+e exec "i3-nagbar -t warning -m 'You pressed the exit shortcut. Do you really want to exit i3? This will end your X session.' -b 'Yes, exit i3' 'i3-msg exit'"

        # resize window (you can also use the mouse for that)
        mode "resize" {
                # These bindings trigger as soon as you enter the resize mode

                # Pressing left will shrink the window’s width.
                # Pressing right will grow the window’s width.
                # Pressing up will shrink the window’s height.
                # Pressing down will grow the window’s height.
                bindsym b resize shrink width 1 px or 1 ppt
                bindsym n resize grow height 1 px or 1 ppt
                bindsym p resize shrink height 1 px or 1 ppt
                bindsym f resize grow width 1 px or 1 ppt

                # back to normal: Enter or Escape
                bindsym Return mode "default"
                bindsym Escape mode "default"
        }

        bindsym Mod4+r mode "resize"


        # Use a custom script to kill+start polybar for every monitor.
        exec_always --no-startup-id ${dotfiles}/i3-start-polybar.sh
        # exec_always --no-startup-id polybar-msg cmd restart

        # Ibus is started by default for input methods, emoji support and such. I don't want it.
        exec --no-startup-id ibus exit

        # Lock via screensaver activation
        bindsym Mod4+l exec xset s activate

        # Use autolock script when screensaver activates or on suspend
        exec --no-startup-id xss-lock -l -- ${dotfiles}/i3-autolock.sh &
        exec --no-startup-id ${dotfiles}/autosuspend.sh &
        # suspend is `sudo pm-suspend`

        # Note sync
        exec --no-startup-id ${dotfiles}/i3-notes-sync.sh daemon

        # Pulse Audio
        exec --no-startup-id i3-msg 'exec /usr/bin/pulseaudio --start'
        bindsym XF86AudioRaiseVolume exec amixer -q -D pulse sset Master 3%+ && pkill -RTMIN+10 i3blocks
        bindsym XF86AudioLowerVolume exec amixer -q -D pulse sset Master 3%- && pkill -RTMIN+10 i3blocks
        bindsym XF86AudioMute exec amixer -q -D pulse sset Master toggle && pkill -RTMIN+10 i3blocks
        # Repeat of F1 is disabled in ~/.xsessionrc
        bindsym F1 exec ${dotfiles}/i3-mic.sh on && pkill -RTMIN+12 i3blocks
        bindsym --release F1 exec ${dotfiles}/i3-mic.sh off && pkill -RTMIN+12 i3blocks
        bindsym F3 exec ${dotfiles}/i3-mic.sh on && pkill -RTMIN+12 i3blocks

        # Spotify pause with Pause/Break
        bindsym Pause exec dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause

        # Calculator
        bindsym XF86Calculator exec gnome-terminal --hide-menubar -e 'bc -q -l'

        # Screenshot selected region directly to clipboard
        bindsym --release Print exec ${dotfiles}/i3-screen-capture.sh
        bindsym --release XF86Tools exec ${dotfiles}/i3-screen-capture.sh

        # Screencast
        bindsym --release Control+Print exec ${dotfiles}/i3-gif-capture.sh
        bindsym --release Control+Shift+Print exec pkill ffmpeg
        bindsym --release Control+XF86Tools exec ${dotfiles}/i3-gif-capture.sh
        bindsym --release Control+Shift+XF86Tools exec pkill ffmpeg

        # Brightness
        bindsym --release XF86MonBrightnessUp exec light -T 2.0
        bindsym --release XF86MonBrightnessDown exec light -T 0.5

        # Add todo to todoist
        bindsym Mod4+t exec ${dotfiles}/scripts/todo.sh

        # Disable title bar in general. 0-pixel border for some applications. (Use `i3-msg -t get_tree` to
        # figure out the class)
        for_window [] border pixel 2
        # Firefox browser has window_role="browser", firefox inspector has window_role="toolbox"
        for_window [class="Firefox" window_role="browser"] border pixel 0
        # Firefox 98.0 has changed the class name to lower case
        for_window [class="firefox" window_role="browser"] border pixel 0
        for_window [class="Spotify"] border pixel 0
        for_window [class="Emacs"] border pixel 0
      '';
    };
  };
}

