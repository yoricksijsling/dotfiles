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
    ./nixgl.nix
    ./ubuntu.nix
  ];


  home = {
    username = "yorick";
    homeDirectory = "/home/yorick";

    packages = [
      pkgs.gimp
      pkgs.pinta
      pkgs.libreoffice
      pkgs.light  # For easy brightness adjustment
      pkgs.chromium

      # pkgs.haskellPackages.ghcid
      pkgs.haskellPackages.hlint
      # pkgs.haskellPackages.profiteur
      pkgs.haskellPackages.stylish-haskell
      pkgs.haskellPackages.ghc-prof-flamegraph
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.cabal2nix
      pkgs.haskellPackages.ghc

      # I'm using a globally installed stack for emacs (would be better to take
      # it from the local nix env..)
      pkgs.haskellPackages.stack

      python3WithPackages

      # At the moment i3lock from home-manager doesn't seem to unlock
      # properly, I'm installing i3lock and xss-lock with apt instead.
      # pkgs.i3lock
      # pkgs.xss-lock

      # Profiling, see notes
      pkgs.flamegraph
      pkgs.linuxKernel.packages.linux_5_15.perf
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
    };
  };
}
