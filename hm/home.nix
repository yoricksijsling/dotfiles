{ config, pkgs, inputs, ... }:

let
  python3WithPackages = pkgs.python3.withPackages (ps: [
    ps.jedi-language-server  # Used in emacs with lsp
    ps.numpy  # Used by google cloud
  ]);
in
{
  imports = [
    ./i3.nix
    ./emacs.nix
    ./git.nix
    ./firefox.nix
  ];

  # Necessary to set up some basic stuff for non-nixos installs,
  # including modifications to XDG_DATA_DIRS and XCURSOR_PATH so that
  # home-manager installed applications can be found.
  targets.genericLinux.enable = true;

  # Necessary for some packages, like spotify
  nixpkgs.config.allowUnfreePredicate = (pkg: true);
  # You'd prefer to just set nixpkgs.config.allowUnfree, but that's broken at
  # the moment (https://github.com/nix-community/home-manager/issues/2942#issuecomment-1119760100)
  # nixpkgs.config.allowUnfree = true;

  home = {
    username = "yorick";
    homeDirectory = "/home/yorick";

    packages = [
      pkgs.arandr  # GUI for xrandr (monitor management)
      pkgs.gimp
      pkgs.imagemagick
      pkgs.jq  # Used in i3-navigate-emacs.sh
      pkgs.notify-osd  # So that notify-send actually works
      pkgs.read-edid  # Read identification codes for monitors
      pkgs.ripgrep
      pkgs.scrot  # Used in i3-autolock.sh
      pkgs.shellcheck
      pkgs.slop  # Used in i3-gif-capture.sh
      pkgs.ffmpeg  # Used in i3-gif-capture.sh
      pkgs.spotify # Temporarily disabled because allowUnfree isn't being picked up??
      pkgs.undistract-me  # Notifications for long-running commands
      pkgs.xclip  # Used in i3-screen-capture.sh and pbcopy/pbpaste aliases
      pkgs.pinta
      pkgs.libreoffice
      pkgs.light  # For easy brightness adjustment
      pkgs.chromium

      # I'm using a globally installed stack for emacs (would be better to take
      # it from the local nix env..)
      pkgs.haskellPackages.stack

      # pkgs.haskellPackages.ghcid
      # pkgs.haskellPackages.hlint
      # pkgs.haskellPackages.profiteur
      # pkgs.haskellPackages.stylish-haskell
      # pkgs.haskellPackages.ghc-prof-flamegraph
      # pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.cabal2nix
      # pkgs.haskellPackages.ghc

      python3WithPackages

      # At the moment i3lock from home-manager doesn't seem to unlock
      # properly, I'm installing i3lock and xss-lock with apt instead.
      # pkgs.i3lock
      # pkgs.xss-lock

      # nixGL as defined below in the overlay
      pkgs.nixgl.nixGLIntel

      # Fonts. Check https://github.com/polybar/polybar/wiki/Fonts for font debugging
      pkgs.comic-relief  # Comic Sans MS
      pkgs.dejavu_fonts
      pkgs.font-awesome  # Used in polybar
      pkgs.liberation_ttf  # Times New Roman, Arial, and Courier New (as serif/sans/mono)
      pkgs.noto-fonts
    ];

    sessionVariables = {
      # I probably did something wrong, but by default home-manager
      # sets the NIX_PATH to use the channels of the root user. I want
      # the nixpkgs and home-manager channels that I've set up for my
      # own user.
      NIX_PATH = "$HOME/.nix-defexpr/channels";

      # Google Cloud SDK complains when it doesn't have numpy, so make sure it finds it.
      CLOUDSDK_PYTHON = "${python3WithPackages}/bin/python";
      CLOUDSDK_PYTHON_SITEPACKAGES=1;

      # Set this early to prevent complaints about 'environment variable $SSH_AUTH_SOCK not set'.
      SSH_AUTH_SOCK = "/run/user/1000/keyring/ssh";
    };

    sessionPath = [
      "$HOME/.local/bin"  # Pipenv and stack installs
    ];

    # This value determines the Home Manager release that your
    # configuration is compatible with. This helps avoid breakage
    # when a new Home Manager release introduces backwards
    # incompatible changes.
    #
    # You can update Home Manager without changing this value. See
    # the Home Manager release notes for a list of state version
    # changes in each release.
    stateVersion = "22.11";
  };

  # This allows fonts packages (like comic-relief) to be found
  fonts.fontconfig.enable = true;

  # Let Home Manager install and manage itself.
  programs = {
    home-manager.enable = true;

    keychain = {
      enable = true;

      # The xsession integration puts a keychain call in
      # ~/.Xsession. That seems to interrupt the login flow such that
      # it doesn't start i3.
      enableXsessionIntegration = false;

      # Puts 'eval `keychain --eval --quiet id_ed25519`' in
      # ~/.bashrc. You have to enter your ssh password once you open
      # a terminal.
      enableBashIntegration = true;

      keys = [ "id_ed25519" ];
    };

    # Color support for ls. Sets LS_COLORS.
    dircolors.enable = true;

    # Lesspipe preprocessor for less. Sets LESSOPEN.
    lesspipe.enable = true;

    bash = {
      # Take over management of .profile, .bash_profile and .bashrc
      enable = true;

      shellAliases = {
        ls = "ls --color=auto";
        grep = "grep --color=auto";
        less = "less -R";  # Pass through colors
        pbcopy = "xclip -selection clipboard";
        pbpaste = "xclip -selection clipboard -o";
        e = "emacsclient -n";  # Open with emacs, don't wait
      };

      # Commands that should be placed in ~/.bashrc. Note that these
      # commands will be run even in non-interactive shells.
      bashrcExtra = ''
source ${config.home.homeDirectory}/dotfiles/set_ps1.sh
set_ps1  # From functions.sh

# Enable programmable completion features
. /usr/share/bash-completion/bash_completion

# Show notifications for long running processes. Will overwrite DEBUG trap.
source ${pkgs.undistract-me}/share/undistract-me/long-running.bash
notify_when_long_running_commands_finish_install
'';

      # Extra commands that should be run when initializing an interactive shell.
      initExtra = "";

      # Extra commands that should be run when initializing a login shell.
      # profileExtra = "";
    };

  };

  nixpkgs.overlays =
    let more = (self: super: {

          # nixGLWrapper taken from https://github.com/crtschin/dotfiles/blob/master/work.nix
          # This wrapper still relies on the drivers that you have installed on your system, see README.

          # It looks like I specifically need nixGLIntel on my work laptop.
          nixGLWrapper = program: pkgs.writeShellScriptBin program.pname ''
            #!/bin/sh
            ${self.nixgl.nixGLIntel}/bin/nixGLIntel ${program}/bin/${program.pname} "$@"
          '';

          # Use picom compositor to prevent screen tearing in i3
          picom = self.nixGLWrapper super.picom;
        });
    in [ inputs.nixgl.overlay inputs.emacs-overlay.overlay more ];
}
