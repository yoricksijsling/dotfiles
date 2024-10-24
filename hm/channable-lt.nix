{ config, pkgs, inputs, ... }:

let

  # Example python overlay:
  # pythonOverlay = self: super: {
  #   jedi-language-server = super.jedi-language-server.overridePythonAttrs rec {
  #     pname = "jedi-language-server";
  #     # Bumping version to get support for pydantic v2
  #     version = "0.41.3";
  #     src = pkgs.fetchFromGitHub {
  #       owner = "pappasam";
  #       repo = pname;
  #       rev = "refs/tags/v${version}";
  #       hash = "sha256-+k4WOoEbVe7mlPyPj0ttBM+kmjq8V739yHi36BDYK2U=";
  #     };
  #   };
  # };
  # python = pkgs.python311.override { packageOverrides = pythonOverlay; };
  python = pkgs.python3;
  pythonWithPackages = python.withPackages (ps: [
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

      pkgs.nix-output-monitor  # Use nom instead of nix to get a nicer output

      # pkgs.haskellPackages.ghcid
      pkgs.haskellPackages.hlint
      # pkgs.haskellPackages.profiteur
      pkgs.haskellPackages.stylish-haskell
      pkgs.haskellPackages.ghc-prof-flamegraph
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.cabal2nix
      pkgs.haskellPackages.ghc


      pythonWithPackages

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
      CLOUDSDK_PYTHON = "${pythonWithPackages}/bin/python";
      CLOUDSDK_PYTHON_SITEPACKAGES=1;
    };
  };
}
