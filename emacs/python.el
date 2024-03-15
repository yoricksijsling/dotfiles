

;; --------------------------------------------------------------------------------
;; LSP using jedi language server

;; Uses 'jedi-language-server' command by default

(require 'lsp-jedi)

(add-hook 'python-mode-hook 'lsp)

(add-to-list 'lsp-disabled-clients 'pyls)
;; (add-to-list 'lsp-enabled-clients 'jedi)  ;; By default all clients are considered enabled, so keep this at nil

;; Jedi can give us diagnostics about syntax errors, these are reported through
;; the lsp-diagnostics minor mode
(setq lsp-jedi-diagnostics-enable t)
(setq lsp-jedi-diagnostics-did-open t)
(setq lsp-jedi-diagnostics-did-save t)

;; Don't use a global python3, because direnv may set one locally for the project. By setting this
;; lambda, the python3 executable will be determined when the jedi-language-server starts. This will
;; ensure that the right python executable will be used, including all the python dependencies.
(lsp-register-custom-settings '(("jedi.workspace.environmentPath" (lambda () (executable-find "python3")) t)))


;; We can get lsp-diagnostics to communicate diagnostics on change (without
;; saving), but only when idle-change is in the
;; flycheck-check-syntax-automatically list.
(setq lsp-jedi-diagnostics-did-change t)
(add-hook 'python-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'flycheck-check-syntax-automatically)
                         'idle-change)))

;; Replace the 'python3' command for these checkers with the 'python3' from
;; virtualenv, so that it still works if the checker runs after we left the
;; buffer.
;; (add-hook 'pyvenv-post-activate-hooks
;;           (lambda ()
;;             (let ((python3 (executable-find "python3")))
;;               ;; (message "setting python3 executable in hook %s" python3)
;;               (flycheck-set-checker-executable 'python-pylint python)
;;               (flycheck-set-checker-executable 'python-mypy python)
;;               )))



;; --------------------------------------------------------------------------------
;; Mypy checker in flycheck

(require 'flycheck)

;; Run mypy when jedi did not give errors. The mypy checker only runs when the
;; buffer contains no modifications.
;; Lsp checker only exists after we start lsp mode.
(add-hook 'lsp-mode-hook
          (lambda ()
            (flycheck-add-next-checker 'lsp '(warning . python-mypy))))


;; --------------------------------------------------------------------------------
;; Auto-complete imports

(defun company-python-imports (command &optional arg &rest ignored)
  "Company mode completions for imports"
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-python-imports))
    (prefix (company-grab-line "^\\(from\\|import\\)....*"))
    (candidates (search-python-imports arg))
    (no-cache t)
    ))

(add-hook 'python-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends)
                         'company-python-imports)))

(defun search-python-imports (arg)
  "Return suggestions by searching through the imports in all
python files throughout the project. Depends on projectile and
ag."
  (let* ((ignored (mapconcat (lambda (i)
                               (concat "--glob !"
                                       (shell-quote-argument i)))
                             (append (projectile--globally-ignored-file-suffixes-glob)
                                     (projectile-ignored-files-rel)
                                     (projectile-ignored-directories-rel))
                             " "))
         (command (format (concat "rg "
                                  ignored
                                  " --color never"
                                  " --no-heading"
                                  " --no-filename"
                                  " --ignore-case"
                                  " --glob *.py"
                                  " -- ")))
         (regex (concat "^(from|import) .*"
                        (s-join ".*" (-map (lambda (w)
                                             (concat "\\b" w)
                                             )
                                           (cdr (split-string arg))))))
         (command-list (append (split-string command) (list regex)))
         (result (let ((default-directory (projectile-project-root)))
                   (apply 'process-lines command-list)))
         )
    (cl-remove-duplicates result :test 'equal)))
