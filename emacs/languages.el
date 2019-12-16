
;; Stuff for miscellaneous programming/markup/configuration languages


;; Use shellcheck for bash scripts
(add-hook 'sh-mode-hook 'flycheck-mode)


;; Markdown
(require 'markdown-mode)
(setq markdown-gfm-use-electric-backquote nil)
(setq markdown-fontify-code-blocks-natively t)
(add-to-list 'auto-mode-alist '("\\.md\\'"       . gfm-mode)) ;; github-flavoured-markdown
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode)) ;; github-flavoured-markdown
(add-hook 'gfm-mode-hook 'variable-pitch-mode) ;; Proportional font
(setq markdown-header-scaling t)
;; (setq markdown-header-scaling-values '(2.0 1.5 1.25 1.0 0.875 0.85))  ;; Github sizes
(setq markdown-header-scaling-values '(1.73 1.44 1.2 1.0 1.0 1.0))  ;; max(1, 1.2^(n-2))
(markdown-update-header-faces markdown-header-scaling markdown-header-scaling-values)
(define-key markdown-mode-map (kbd "C-c C-r") 'markdown-sql-send)


;; Hide/show in xml files
(require 'hideshow)
(require 'sgml-mode)
(require 'nxml-mode)
(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))
(add-hook 'nxml-mode-hook 'hs-minor-mode)
(define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)


;; Hide/show in js and json
(require 'js)
(add-to-list 'hs-special-modes-alist
             '(js-mode "{" "}" "/[*/]" nil))
(add-hook 'js-mode-hook 'hs-minor-mode)
(define-key js-mode-map (kbd "C-c h") 'hs-toggle-hiding)
