
;; My own settings
;; This file is meant to be portable across OS'es, ideally even within a terminal


(load "~/dotfiles/emacs/essentials.el")
(load "~/dotfiles/emacs/general.el")

;; Pallet to maintain entries in ~/.emacs.d/Cask automatically
(require 'pallet)
(pallet-mode t)


;; Changing these faces via customize still works, but it will duplicate all the
;; settings to init.el. After a change, copy the entire `custom-set-faces` call
;; from init.el to here.
(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "#222" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 128 :width normal :foundry "DAMA" :family "Ubuntu Mono"))))
 '(fixed-pitch ((t (:family "Ubuntu Mono"))))
 '(magit-diff-context-highlight ((t (:background "lemon chiffon" :foreground "grey50"))))
 '(markdown-code-face ((t (:inherit fixed-pitch :background "gray96" :height 1.1))))
 '(markdown-header-face ((t (:foreground "medium blue" :weight bold))))
 '(variable-pitch ((t (:height 0.9 :family "Comic Sans MS")))))


(load "~/dotfiles/emacs/languages.el")
(load "~/dotfiles/emacs/haskell.el")
(load "~/dotfiles/emacs/python.el")
(load "~/dotfiles/emacs/sql.el")
