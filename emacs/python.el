
;; --------------------------------------------------------------------------------
;; LSP using basedpyright + ruff

;; Must set this before require'ing lsp-pyright, otherwise it registers a wrong dependency
(setq-default lsp-pyright-langserver-command "basedpyright")
(require 'lsp-pyright)  ;; Will get required via lsp-mode
(add-hook 'python-mode-hook 'lsp)

(let ((client (gethash 'pyright lsp-clients)))
   ;; When we follow definitions into certain directories, we want LSP to know that this is a library,
   ;; and it shouldn't nag us about importing the project root.
   (setf (lsp--client-library-folders-fn client)
         (lambda (_workspace) (list "/usr" "/nix/store")))

   ;; Pyright seems to support multi-root just fine, we get an appropriate workspace/configuration
   ;; request with an item like this:
   ;;
   ;;   {"scopeUri": "file:///mydir/", "section": "python"}
   ;;
   ;; In lsp-mode, lsp--build-workspace-configuration-response is used to determine a response, but
   ;; it doesn't use the scopeUri. So we always respond with the same pythonPath, while we should
   ;; decide that based on the scopeUri. As long as we don't do multi-root, lsp-mode+lsp-pyright
   ;; uses the direnv-provided python3 (including library dependencies) for the workspace.
   (setf (lsp--client-multi-root client) nil)
   )

;; lsp-ruff is included by default as well. We can use it for formatting.
;; At the moment, a `ruff` version is provided in our nix direnv, but we want a newer version for
;; LSP so let's grab the most recent one:
(setq-default lsp-ruff-server-command '("nix" "run" "nixpkgs#ruff" "--" "server"))
;; (add-to-list 'lsp-disabled-clients 'ruff)




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
