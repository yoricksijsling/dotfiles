{
  inputs = {
    nixpkgs.url = "flake:nixpkgs/nixos-24.05";
    nixpkgs-unstable.url = "flake:nixpkgs/nixpkgs-unstable";

    flake-utils.url = "github:numtide/flake-utils";

    home-manager = {
      # url = "github:nix-community/home-manager";
      url = "github:nix-community/home-manager/release-24.05";

      # Follow existing nixpkgs for better caching. This can break things if your
      # versions don't match! In our case we use version 24.05 for both home
      # manager and nixpkgs.
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixgl = {
      url = "github:nix-community/nixGL";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

    # Use this one for bleeding-edge versions of emacs, as well as the emacs packages.
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };
  outputs = { self, nixpkgs, nixpkgs-unstable, home-manager, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      pkgs-unstable = nixpkgs-unstable.legacyPackages.${system};

      # I don't want to include work stuff in this repo, so we expose a function that constructs
      # the home manager configuration if you pass it a list of additional modules that you want
      # to include.
      # The homeManagerConfiguration call must be done inside this flake, because it refers to
      # files within the repo (for example mc-lists.el).
      makeHomeConfiguration = {extraModules ? []}: home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        extraSpecialArgs = {
          # Pass all inputs to every module. It's a bit excessive, but allows us to easily refer
          # to stuff like inputs.nixgl.
          inherit inputs;
          # The dotfiles argument always points to the flake root.
          dotfiles = self;
          inherit pkgs-unstable;
        };
        modules = [ ./hm/channable-lt.nix ] ++ extraModules;
      };
    in
      {
        inherit makeHomeConfiguration;

        homeConfigurations = {
          # For convenience we also export a home configuration without extra
          # modules. It can be used directly by home manager.
          channable-lt = makeHomeConfiguration {};

          personal-lt = home-manager.lib.homeManagerConfiguration {
            inherit pkgs;
            extraSpecialArgs = {
              inherit inputs;
              dotfiles = self;
            };
            modules = [ ./hm/personal-lt.nix ];
          };
        };
      };
}
