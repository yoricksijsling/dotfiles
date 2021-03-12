
;; Stuff for miscellaneous programming/markup/configuration languages


;; Use shellcheck for bash scripts
(add-hook 'sh-mode-hook 'flycheck-mode)


;;--------------------------------------------------------------------------------
;; XML

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

(require 'handlebars-sgml-mode)
(handlebars-use-mode 'global) ;; always use handlebars-minor-mode
;; (handlebars-use-mode 'off)       ;; Never use handlebars-mode (the default)
;; (handlebars-use-mode 'minor)  ;; Only use if in 'handlebars-sgml-minor-mode
(add-to-list ' auto-mode-alist '("\\.hbs$" . html-mode))
(setq sgml-basic-offset 4)

;;--------------------------------------------------------------------------------
;; Javascript / JSON

(require 'hideshow)
(require 'js)

;; Hide/show
(add-to-list 'hs-special-modes-alist
             '(js-mode "{" "}" "/[*/]" nil))
(add-hook 'js-mode-hook 'hs-minor-mode)
(define-key js-mode-map (kbd "C-c h") 'hs-toggle-hiding)
