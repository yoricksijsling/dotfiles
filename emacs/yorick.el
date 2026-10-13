
;; My own settings
;; This file is meant to be portable across OS'es, ideally even within a terminal


(load "~/dotfiles/emacs/essentials.el")
(load "~/dotfiles/emacs/general.el")

;; Customized settings go below
(setq custom-file "~/dotfiles/emacs/yorick.el")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "white" :foreground "#222" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 105 :width normal :foundry "NONE" :family "Comic Mono"))))
 '(fixed-pitch ((t (:family "Comic Mono"))))
 '(lsp-ui-doc-background ((t (:background "azure"))))
 '(magit-diff-context-highlight ((t (:background "lemon chiffon" :foreground "gray50"))))
 '(markdown-code-face ((t (:inherit fixed-pitch))))
 '(markdown-header-face ((t (:foreground "dark slate blue" :weight bold :family "Comic Relief"))))
 '(markdown-header-rule-face ((t (:inherit markdown-markup-face))))
 '(markdown-inline-code-face ((t (:inherit fixed-pitch))))
 '(markdown-language-keyword-face ((t (:inherit shadow))))
 '(markdown-link-face ((t (:inherit link :underline (:color "#adbdeb")))))
 '(markdown-pre-face ((t (:inherit markdown-code-face))))
 '(markdown-url-face ((t (:inherit shadow))))
 '(region ((t (:background "navajo white" :distant-foreground "gtk_selection_fg_color"))))
 '(shadow ((t (:foreground "gray60"))))
 '(variable-pitch ((t (:height 1.0 :family "Comic Relief")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((lsp-haskell-plugin-fourmolu-config-external . t) (lsp-haskell-formatting-provider . "fourmolu")
     (buffer-file-coding-system . utf-8-unix))))

(load "~/dotfiles/emacs/languages.el")
(load "~/dotfiles/emacs/markdown.el")
(load "~/dotfiles/emacs/haskell.el")
(load "~/dotfiles/emacs/python.el")
(load "~/dotfiles/emacs/sql.el")
(load "~/dotfiles/emacs/1password.el")
(load "~/dotfiles/emacs/llm.el")
(load "~/dotfiles/emacs/direnv.el")  ;; Should be done last, then hooks are added in front of others

(message "OK")
