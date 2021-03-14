
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
 '(magit-diff-context-highlight ((t (:background "lemon chiffon" :foreground "gray50"))))
 '(markdown-code-face ((t (:inherit fixed-pitch :background "gray96" :height 0.88))))
 '(markdown-inline-code-face ((t (:inherit fixed-pitch :height 0.88))))
 '(markdown-pre-face ((t (:inherit markdown-code-face))))
 '(markdown-header-face ((t (:foreground "dark slate blue" :weight bold :family "EB Garamond SC"))))
 '(markdown-header-rule-face ((t (:inherit markdown-markup-face))))
 '(markdown-language-keyword-face ((t (:inherit shadow))))
 '(markdown-link-face ((t (:inherit link :underline (:color "#adbdeb")))))
 '(markdown-url-face ((t (:inherit shadow))))
 '(region ((t (:background "navajo white" :distant-foreground "gtk_selection_fg_color"))))
 '(shadow ((t (:foreground "gray60"))))
 '(variable-pitch ((t (:height 1.13 :family "EB Garamond")))))


(load "~/dotfiles/emacs/languages.el")
(load "~/dotfiles/emacs/markdown.el")
(load "~/dotfiles/emacs/haskell.el")
(load "~/dotfiles/emacs/python.el")
(load "~/dotfiles/emacs/sql.el")
