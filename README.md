# Features

A quick overview:
* Everything is configured through home manager, config starts in [home.nix](./hm/home.nix)
* [i3 window manager](./hm/i3.nix) with:
  * [automatic locking](./i3-autolock.sh) and [suspending](./autosuspend.sh)
  * [push-to-speak microphone toggle](./i3-mic.sh)
  * [seamless emacs navigation integration](i3-navigate-emacs.sh)
  * [automatic synchronisation of notes repository](i3-notes-sync.sh)
  * [screenshot](i3-gif-capture.sh) and [video capture](i3-screen-capture.sh)
  * picom to prevent screen tearing
* Home manager config is exposed [as a nix flake](./hm/flake.nix),
  so that I can extend it in specific environments without including that configuration in this repository
* [git configuration](./hm/git.nix) through home manager
* [firefox](./hm/firefox.nix) through home manager, with actually working webgl
* [polybar](./hm/polybar.nix) through home manager
* [emacs](./hm/emacs.nix) with all packages through home manager, configuration in [./emacs](./emacs)

# Setup

This is how i need to set up my stuff on new systems.

I'm assuming checkout in `~/dotfiles`:

```bash
~$ git checkout git@github.com:yoricksijsling/dotfiles.git
```


## Install home manager

Almost everything is done through nix home manager.

For Channable setup: This all goes _after_ the devmachine provisioning.

Home manager is installed via the home manager config.
To bootstrap it, follow instructions at https://nix-community.github.io/home-manager/index.html#ch-nix-flakes

Example configs:
https://github.com/Misterio77/nix-config/blob/main/flake.nix
https://github.com/bbigras/nix-config/

## Use home manager config

This dotfiles repo is a flake that exposes a default home-manager configuration:

```bash
cd ~/dotfiles
nix flake update .
home-manager build --flake .#default
home-manager switch --flake .#default
```

The flake _also_ exposes a `makeHomeConfiguration` function, to allow injection of
additional modules. This is useful for work-related stuff that I don't want to
include in my dotfiles repo. It's easy to write a custom flake like so:

```nix
{
  inputs = {
    dotfiles.url = "path:/home/yorick/dotfiles";
    # dotfiles.url = "github:yoricksijsling/dotfiles";
  };
  outputs = { self, dotfiles, ... }:
    {
      homeConfigurations.yorick = dotfiles.makeHomeConfiguration {
        extraModules = [ ./extra.nix ];
      };
    };
}
```

## Configure i3

To get Ubuntu to recognize the unconventional install of i3 window manager, we
have to add the following to `/usr/share/xsessions/xsession.desktop`:
```
[Desktop Entry]
Name=XSession
Comment=This session uses the custom xsession file
Exec=/etc/X11/Xsession
Type=Application
# DesktopName is important, it's used by e.g. Gnome-Terminal to determine if a title bar must be shown
DesktopNames=i3
X-Ubuntu-Gettext-Domain=gnome-flashback
```

On a related note, you can specify input sources for the login screen in
`/var/lib/AccountsService/users/yorick` by adding:
```
[InputSource0]
xkb=us+dvorak

[InputSource1]
xkb=us
```

## Graphics card drivers

Programs that require opengl can run with the `nixGL` wrapper program. This tries to find your graphics card drivers in an non-pure manner, and relies on the drivers that you have installed on your system. To check which drivers are available:
```
sudo apt-get update
ubuntu-drivers devices
```

And you can install the `recommended` drivers from that list with:
```
sudo ubuntu-drivers autoinstall
```

Make sure to reboot and `home-manager switch` afterwards, to get the `nixGL` wrapper to be regenerated.

Now hopefully this works:
```
nix run nixpkgs.glxinfo -c glxinfo
```

A nice way to debug this stuff:

```
strace -f -o /tmp/alacritty alacritty
grep mesa /tmp/alacritty
```

## Emacs

I'm not committing `~/.emacs.d/init.el` because i put location-dependent and potentially
confidential stuff in there. It might be something like this:

```elisp
;; Packages from other sources
(add-to-list 'load-path "~/opensource/ghcid/plugins/emacs")

(load "~/dotfiles/emacs/yorick.el")

(setq-default fill-column 100)

;; Use system ssh agent. Provided by keychain-environment package
(keychain-refresh-environment)

;; Postgres connections
(add-to-list 'sql-connection-alist
             '(my-database (sql-product 'postgres)
               (sql-user "admin") (sql-password "admin")
               (sql-server "localhost") (sql-database "my-database") (sql-port 5432)))
(configure-sql-connections)
```
