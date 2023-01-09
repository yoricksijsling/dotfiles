{
  inputs = {
    # nixpkgs.url = "flake:nixpkgs/nixpkgs-unstable";
    nixpkgs.url = "flake:nixpkgs/nixos-22.11";

    flake-utils.url = "github:numtide/flake-utils";

    home-manager = {
      # url = "github:nix-community/home-manager";
      url = "github:nix-community/home-manager/release-22.11";

      # Follow existing nixpkgs for better caching. This can break things if your
      # versions don't match! In our case we use version 22.11 for both home
      # manager and nixpkgs.
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.utils.follows = "flake-utils";
    };

    nixgl = {
      url = "github:guibou/nixGL";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };
  outputs = { self, nixpkgs, home-manager, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};

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
        };
        modules = [ ./hm/home.nix ] ++ extraModules;
      };
    in
      {
        inherit makeHomeConfiguration;

        # For convenience we also export a home configuration without extra
        # modules. It can be used directly by home manager.
        homeConfigurations.default = makeHomeConfiguration {};
      };
}
