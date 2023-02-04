{ config, pkgs, inputs, ... }:

{
  imports = [
    ./i3.nix
    ./emacs.nix
    ./git.nix
    ./ubuntu.nix
  ];


  home = {
    username = "yorick";
    homeDirectory = "/home/yorick";

    packages = [
      pkgs.firefox

      # pkgs.gimp
      # pkgs.pinta
      # pkgs.libreoffice
      # pkgs.light  # For easy brightness adjustment
      # pkgs.chromium

      # pkgs.haskellPackages.ghcid
      # pkgs.haskellPackages.hlint
      # pkgs.haskellPackages.profiteur
      # pkgs.haskellPackages.stylish-haskell
      # pkgs.haskellPackages.ghc-prof-flamegraph
      # pkgs.haskellPackages.cabal-install
      # pkgs.haskellPackages.cabal2nix
      # pkgs.haskellPackages.ghc

      # I'm using a globally installed stack for emacs (would be better to take
      # it from the local nix env..)
      # pkgs.haskellPackages.stack
    ];
  };

  programs.firefox = {
    enable = true;
  };
}
