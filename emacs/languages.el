
;; Stuff for miscellaneous programming/markup/configuration languages


;; Use shellcheck for bash scripts
(add-hook 'sh-mode-hook 'flycheck-mode)


;;--------------------------------------------------------------------------------
;; Rust

;; There's default rust integration in flycheck, this package is required though
;; to set it up properly.
;; (require 'flycheck-rust)
;; (with-eval-after-load 'rust-mode
;;   (add-hook 'flycheck-mode-hook #' flycheck-rust-setup))
;; (add-hook 'rust-mode-hook 'flycheck-mode)

(require 'rustic)
(require 'lsp-rust)  ;; Included in lsp-mode package

(setq lsp-eldoc-hook nil)  ;; documentation shown in the minibuffer


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


;;--------------------------------------------------------------------------------
;; HTML / PHP

;; https://web-mode.org/
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (setq-local tab-width 4)))
(add-to-list 'ahs-modes 'web-mode)

;; Other options:
;; js, jsx, css, scss, xml
;;(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
;;(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;;(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
;;(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;;(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;;(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;;(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;;(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

