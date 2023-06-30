{ config, dotfiles, lib, pkgs, ... }:

# Emacs configuration is included in the repo, but we don't link to it
# directly from here. You have to create ~/.emacs.d/init.el yourself
# and at least include a `(load "~/dotfiles/emacs/yorick.el")` call
# there. See README.md for further details.

let
  emacsPackage = pkgs.emacs28NativeComp;
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
        lsp-jedi
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
