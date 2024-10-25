{ config, dotfiles, lib, pkgs, ... }:

# Emacs configuration is included in the repo, but we don't link to it
# directly from here. You have to create ~/.emacs.d/init.el yourself
# and at least include a `(load "~/dotfiles/emacs/yorick.el")` call
# there. See README.md for further details.

let
  # Emacs29 comes with nixpkgs. For newer versions we could use emacs-unstable from the emacs
  # overlay https://github.com/nix-community/emacs-overlay
  emacsPackage = pkgs.emacs29;
in {

  # services.emacs = {
  #   enable = true;
  #   client.enable = true;
  #   defaultEditor = true;
  # };

  home.file = {
    # This file is used and modified by multiple-cursors, changes have
    # to be committed. This should work fine because it's a symlink.
    "emacs mc lists" = {
      source = "${dotfiles}/emacs.d/.mc-lists.el";
      target = ".emacs.d/.mc-lists.el";
    };
  };

  programs = {
    emacs = {
      enable = true;
      package = emacsPackage;
      overrides = (self: super: rec {
        # By default, we get fairly recent packages through our emacs-overlay. If a package is
        # broken we may want to use a more stable source. Check 'load-path' variable for currently
        # used versions.

        # I was getting some errors in magit, because it couldn't use a variable that was removed in
        # git-commit. Drop it back to an older version:
        git-commit = self.melpaStablePackages.git-commit;
      });
      extraPackages = (epkgs: with epkgs; [
        async
        auto-highlight-symbol
        buffer-move
        company
        counsel
        counsel-projectile
        dash
        ein
        elpy
        envrc
        epl
        f
        find-file-in-project
        flycheck
        ghub
        git-commit
        github-browse-file
        gnu-elpa-keyring-update
        handlebars-mode
        handlebars-sgml-mode
        haskell-mode
        highlight-indentation
        hydra
        imenu-list
        ivy
        ivy-purpose
        keychain-environment
        let-alist
        lsp-mode
        magit
        magit-popup
        markdown-mode
        multiple-cursors
        nix-mode
        package-build
        phi-search
        pkg-info
        popup
        powerline
        projectile
        rich-minority
        rustic
        s
        sbt-mode
        scala-mode
        shut-up
        smart-mode-line
        smart-mode-line-powerline-theme
        swiper
        virtualenvwrapper
        web-mode
        wgrep
        which-key
        window-purpose
        with-editor
        yaml-mode
        yasnippet
      ]);
    };
  };
}
