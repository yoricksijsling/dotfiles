{ config, pkgs, inputs, ... }:

{
  home = {

    packages = [
      # nixGL as defined below in the overlay
      pkgs.nixgl.nixGLIntel
    ];
  };

  nixpkgs.overlays =
    let nixgl_stuff = (self: super: {

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
    in [ inputs.nixgl.overlay nixgl_stuff ];
}
