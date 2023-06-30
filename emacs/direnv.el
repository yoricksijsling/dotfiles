;; --------------------------------------------------------------------------------
;; Direnv

;; From the readme:
;;
;;   It's probably wise to do this late in your startup sequence: you normally want envrc-mode to be
;;   initialized in each buffer before other minor modes like flycheck-mode which might look for
;;   executables. Counter-intuitively, this means that envrc-global-mode should be enabled after
;;   other global minor modes, since each prepends itself to various hooks.
;;
(envrc-global-mode)

(with-eval-after-load 'envrc
  (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map))
