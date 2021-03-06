
;; --------------------------------------------------------------------------------
;; Python

;; Python projects
(elpy-enable)
(add-hook 'elpy-mode-hook 'flycheck-mode)
(setq elpy-rpc-backend "jedi")
(setq elpy-rpc-python-command "python3.7")  ;; Black doesn't work with python2.7
(setq elpy-rpc-timeout 3)  ;; Timeout for black reformatting needs to be more than 1 second
(setq pyvenv-mode nil)
;; https://elpy.readthedocs.io/en/latest/ide.html#interpreter-setup
(setq python-shell-interpreter "ipython")
(setq python-shell-interpreter-args "-i --simple-prompt")

;; Emacs IPython Notebook
;; (setq ein:url-localhost "localhost")
;; (setq ein:url-localhost-template "http://localhost:%s")

(defvar my-venv-pretty-names nil
  "Association list with prettier names for virtual environments. For example:

    (\"todoist-python-mLoYWY0A\" . \"todoist\")")

(defvar my-venv-known-projects nil
  "Association list to look up virtual environments based on project directories.")

;; Initially fill my-venv-known-projects with those known by pipenv.
(--each (file-expand-wildcards "~/.local/share/virtualenvs/*/.project")
  (let ((project-dir (with-temp-buffer
                       (insert-file-contents it)
                       (file-name-as-directory (buffer-string))))
        (venv-dir (file-name-directory it)))
    (add-to-list 'my-venv-known-projects (cons project-dir venv-dir))))

(defun my-venv-projectile-auto-workon ()
  (let* ((projectile-require-project-root nil) ;; So projectile-project-root doesn't err
         (venv-path (cdr (assoc (projectile-project-root) my-venv-known-projects))))
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

(defadvice pyvenv-activate (after venv-name-shorten activate)
 (setq pyvenv-virtual-env-name
       (or (my-shorten-venv-name pyvenv-virtual-env-name) pyvenv-virtual-env-name)))

(defun my-shorten-venv-name (venv-name)
  "Find the shorter name for the given venv nam. May return nil."
  (cdr (assoc venv-name my-venv-pretty-names)))


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

(add-hook 'elpy-mode-hook
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
