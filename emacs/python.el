


;; --------------------------------------------------------------------------------
;; Virtual environments

(require 'pyvenv)
(setq pyvenv-mode nil)

(defvar my-venv-pretty-names nil
  "Association list with prettier names for virtual environments. For example:

    (\"todoist-python-mLoYWY0A\" . \"todoist\")")

(defvar my-venv-extra-projects nil
  "Association list to look up virtual environments based on project directories.")

(defun my-venv-add-known-project (project-dir venv-dir &optional pretty-name)
  (add-to-list 'my-venv-extra-projects (cons project-dir venv-dir))
  (add-to-list 'my-venv-pretty-names (cons venv-dir pretty-name)))

;; We can autodetect all projects known by pipenv
(defun my-venv-get-pipenv-projects ()
    (--map
     (let ((project-dir (with-temp-buffer
                          (insert-file-contents it)
                          (file-name-as-directory (buffer-string))))
           (venv-dir (file-name-directory it)))
       (cons project-dir venv-dir))
     (file-expand-wildcards "/home/yorick/.local/share/virtualenvs/*/.project")))

(defvar my-venv-pipenv-projects (my-venv-get-pipenv-projects)
  "Assocation list to find pipenv virtual environment based on project directories.")


(defun my-venv-projectile-auto-workon ()
  (let* ((projectile-require-project-root nil) ;; So projectile-project-root doesn't err
         (known-projects (append my-venv-extra-projects my-venv-pipenv-projects))
         (venv-path (cdr (assoc (projectile-project-root) known-projects))))
    (if venv-path
        (progn
          ;; (message "switching %s" venv-path)
          (pyvenv-activate venv-path)
          )
      ;; (message "deact")
      (pyvenv-deactivate)
      ))
  ;; (message nil)
  )
(add-hook 'my-selected-window-changed-hook 'my-venv-projectile-auto-workon)

(defun pyvenv-activate--advice (directory)
  ;; (message "activate advice! %s" directory)
  (setq pyvenv-virtual-env-name
        (or (my-shorten-venv-name pyvenv-virtual-env) pyvenv-virtual-env-name))
  ;; lsp-pyright-venv-path is an absolute path
  ;; (setq lsp-pyright-venv-path directory)
  (setq lsp-pyls-plugins-jedi-environment directory)

  ;; When using python shells, we want one shell per virtual env
  ;; instead of one global shell.
  (setq python-shell-buffer-name (format "Python %s" pyvenv-virtual-env-name))
  )
(advice-add 'pyvenv-activate :after #'pyvenv-activate--advice)

(defun my-shorten-venv-name (venv-dir)
  "Find the shorter name for the given venv nam. May return nil."
  (cdr (assoc venv-dir my-venv-pretty-names)))


(defun run-python--projectile-advice (orig-fun &rest args)
  (let ((default-directory (projectile-project-root)))
    (apply orig-fun args)))
(advice-add 'run-python :around #'run-python--projectile-advice)


;; --------------------------------------------------------------------------------
;; LSP using PYLS

;; ;; This enables pyls. Note that this is Palantir's unmaintained version, we should swith to the new pylsp some time soon.
;; (setq lsp-pyls-server-command "/home/yorick/channable/devops/dev-environment/channable-pyls.py")

;; (lsp-register-custom-settings
;;  '(
;;    ;; By default pyls-mypy uses 'live mode', but then mypy is unaware of what file path is being
;;    ;; checked so it can't find imports and stubs.
;;    ("pyls.plugins.pyls_mypy.live_mode" nil t)))

;; ;; Generally use black instead of autopep8
;; ;; It will automatically use black by simply having lysp-black installed.
;; (setq lsp-pyls-plugins-autopep8-enabled nil)
;; (setq lsp-pyls-plugins-pylint-enabled nil)
;; (setq lsp-pyls-plugins-pycodestyle-enabled nil)

;; --------------------------------------------------------------------------------
;; LSP using pyright

;; Start single server for every workspace, to prevent https://github.com/emacs-lsp/lsp-pyright/issues/6
;; (setq lsp-pyright-multi-root nil)
;; (require 'lsp-pyright)

;; --------------------------------------------------------------------------------
;; LSP using Microsoft's python-language-server

;; (setq lsp-python-ms-executable (executable-find "python-language-server"))
;; (require 'lsp-python-ms)


;; --------------------------------------------------------------------------------
;; LSP using jedi language server

;; Uses 'jedi-language-server' command by default

(require 'lsp-jedi)

(add-hook 'python-mode-hook 'lsp)

(add-to-list 'lsp-disabled-clients 'pyls)
(add-to-list 'lsp-enabled-clients 'jedi)

;; Jedi can give us diagnostics about syntax errors, these are reported through
;; the lsp-diagnostics minor mode
(setq lsp-jedi-diagnostics-enable t)
(setq lsp-jedi-diagnostics-did-open t)
(setq lsp-jedi-diagnostics-did-save t)

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
(add-hook 'pyvenv-post-activate-hooks
          (lambda ()
            (let ((python3 (executable-find "python3")))
              ;; (message "setting python3 executable in hook %s" python3)
              (flycheck-set-checker-executable 'python-pylint (executable-find "python3"))
              (flycheck-set-checker-executable 'python-mypy (executable-find "python3"))
              )))

;; --------------------------------------------------------------------------------
;; Mypy checker in flycheck

(require 'flycheck)

;; Run mypy when jedi did not give errors. The mypy checker only runs when the
;; buffer contains no modifications.
;; Lsp checker only exists after we start lsp mode.
(add-hook 'lsp-mode-hook
          (lambda ()
            (flycheck-add-next-checker 'lsp '(warning . python-mypy))))

;; Same as default python-mypy checker, but using the mypy module instead of the
;; executable so that it can be called in a venv.
;;
;; Changes to apply for upstreaming:
;; - Replace "mypy" command with "python3" and the module-args thing
;; - Add :verify lambda
;; - Add :enabled lambda
;; - Also: python-pycompile claims that it can run but it can't, would be good
;;   to include similar :verify and :enabled clauses.
;; - The :working-directory must be set correctly, but I'm not sure how to do that without projectile
;;
;; Refer to https://github.com/flycheck/flycheck/pull/1354
;; and https://github.com/flycheck/flycheck/pull/1113
;;
(flycheck-define-checker python-mypy
  "Mypy syntax and type checker.  Requires mypy>=0.580.

See URL `http://mypy-lang.org/'."
  :command ("python3"
            ;; (eval (flycheck-python-module-args 'python-mypy "mypy"))
            "-m" "mypy"
            "--show-column-numbers"
            "--follow-imports" "skip"
            (config-file "--config-file" flycheck-python-mypy-config)
            (option "--cache-dir" flycheck-python-mypy-cache-dir)
            source-original)
  :error-patterns
  ((error line-start (file-name) ":" line (optional ":" column)
          ": error:" (message) line-end)
   (warning line-start (file-name) ":" line (optional ":" column)
            ": warning:" (message) line-end)
   (info line-start (file-name) ":" line (optional ":" column)
         ": note:" (message) line-end))
  :modes python-mode
  ;; Only run when the file is saved, to work around
  ;; https://github.com/python/mypy/issues/4746.
  :predicate flycheck-buffer-saved-p
  :verify (lambda (_) (flycheck-python-verify-module 'python-mypy "mypy"))
  :enabled (lambda ()
             (or (not (flycheck-python-needs-module-p 'python-mypy))
                 (flycheck-python-find-module 'python-mypy "mypy")))
  :working-directory (lambda (_) (projectile-project-root))
  )

;; --------------------------------------------------------------------------------
;; Emacs IPython Notebook

;; (setq ein:url-localhost "localhost")
;; (setq ein:url-localhost-template "http://localhost:%s")


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
                               (concat "--ignore "
                                       (shell-quote-argument i)
                                       " "))
                             (append (projectile-ignored-files-rel)
                                     (projectile-ignored-directories-rel))
                             ""))
         (command (format (concat "ag "
                                  ignored
                                  " --nocolor"
                                  " --nogroup"
                                  " --nofilename"
                                  " --nobreak"
                                  " --ignore-case"
                                  " --file-search-regex .*py$"
                                  " -- ")))
         (regex (concat "^(from|import) .*"
                        (s-join ".*" (-map (lambda (w)
                                             (concat "\\b" w)
                                             )
                                           (cdr (split-string arg))))))
         (command-list (append (split-string command) (list regex)))
         (default-directory (projectile-project-root))  ;; For process-lines
         (result (condition-case nil
                     (apply 'process-lines command-list)
                   ;; Error handler
                   (error nil)
                     ))
         )
    (cl-remove-duplicates result :test 'equal)))
