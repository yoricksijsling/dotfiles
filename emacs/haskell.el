;; --------------------------------------------------------------------------------
;;   Haskell

(require 'ghcid)   ;; Custom version

(require 'haskell)   ;; Custom version, see init.el
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(require 'company)
(add-hook 'haskell-mode-hook 'company-mode)

;; Interactive-haskell-mode is the mode in the haskell file buffer
(define-key interactive-haskell-mode-map (kbd "C-c C-s") 'my-hoogle)
(define-key interactive-haskell-mode-map (kbd "C-c C-d") 'my-eval-decl)
(define-key interactive-haskell-mode-map (kbd "C-c C-f") 'my-partial-load)

;; Haskell-interactive-mode is the mode in the ghci buffer
(add-hook 'haskell-interactive-mode-hook 'init-ghci)
(defun init-ghci ()
  "Run this within the Interactive-Haskell buffer to initialise"
  (goto-address-mode t)
  (make-local-variable 'scroll-conservatively)
  (setq scroll-conservatively 100))

(setq haskell-align-imports-pad-after-name t)
(setq haskell-process-suggest-remove-import-lines nil)
;; (setq haskell-process-suggest-hoogle-imports t)
(setq haskell-tags-on-save t)
(setq haskell-process-args-stack-ghci
      '("--ghci-options=-ferror-spans" ;; -ddump-splices -ddump-to-file"
        "--bench" "--test" ;; Include load paths for testing and benchmark packages
        ;; Default included no-build, but i want to build all my stuff
        "--no-load" ;; Don't load modules initially, just wait for user
        ))
(setq haskell-interactive-popup-errors nil)

(define-key haskell-interactive-mode-map (kbd "RET") 'my-haskell-interactive-mode-return)
(defun my-haskell-interactive-mode-return ()
  (interactive)
  (let* ((overlays (overlays-at (point)))
         (overlay-first-props (--map (car (overlay-properties it)) overlays)))
    (if (--any? (equal it 'goto-address) overlay-first-props)
        (goto-address-at-point)
      (haskell-interactive-mode-return)
      )))

(defun my-hoogle (query &optional info)
  "Do a Hoogle search for QUERY.

If prefix argument INFO is given, then hoogle is asked to show
extra info for the items matching QUERY.

To regenerate the database for a stack project I use 'hoogle-build.sh'."
  (interactive
   (let ((def (haskell-ident-at-point)))
     (if (and def (symbolp def)) (setq def (symbol-name def)))
     (list (read-string (if def
                            (format "Hoogle query (default %s): " def)
                          "Hoogle query: ")
                        nil nil def)
           current-prefix-arg)))
  (let* ((p (haskell-interactive-process))
         (local-hoogle-root (string-trim-right (shell-command-to-string "stack path --local-hoogle-root")))
         (cmd (concat ":!stack exec -- hoogle search "
                      "\'" query "\'"
                      ;; " --count=100"
                      " --link"
                      (if info " -i" "")
                      " --database=" local-hoogle-root "/database.hoo"
                      ))
         (result (haskell-process-queue-sync-request p cmd))
         (text (concat ">> hoogle " query "\n" result)))
    (haskell-interactive-mode-insert-garbage (haskell-interactive-session) text)
    ))


(defun my-partial-load (arg)
  (interactive "P")
  (let ((p (haskell-interactive-process))
        (modulename (my-get-module-name)))
    (my-load-extensions)

    (when arg
      (haskell-process-queue-without-filters p ":set -fobject-code")
      ;; http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#module-and-load
      ;; Make sure that all necessary modules are in the set of loaded modules (check `:show modules`)
      (haskell-process-queue-without-filters p (concat ":load " modulename))
      ;; We don't want anything in the import list though. The following will empty the set of imports
      ;; (`:show imports`) but keeps the set of loaded modules.
      (haskell-process-queue-without-filters p ":module")
      (haskell-process-queue-without-filters p ":set -fbyte-code")
      ;; At this point, `:show modules` should tell you that all relevant modules are loaded with an
      ;; object file. Loading the file will use those compiled object files as well, making sure
      ;; that everything stays fast. Changes to modules will toggle them to interpreted
      ;; automatically.
      )

    ;; Now we can run all the imports, because all necessary modules are loaded
    (my-send-imports)
  ;; (haskell-process-queue-flush (haskell-interactive-process))
  ;; (sleep-for 1)
  ;; (save-mark-and-excursion
  ;;   (mark-page)
  ;;   (my-eval-decl (point-min) (point-max)))
  ))

(defun my-get-module-name ()
  "Pretty broken way of finding a Haskell module name for current buffer"
  (save-excursion
    (beginning-of-buffer)
    (re-search-forward "^module *\\([^[:space:]\n]*\\)" nil t)
    (match-string 1)))

(defun my-load-extensions ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward "^{-# LANGUAGE \\([^ ]*\\) #-}" nil t)
      (let ((cmd (concat ":seti -X" (match-string 1))))
        (haskell-process-queue-without-filters (haskell-interactive-process) cmd)))
    ))

(defun my-send-imports ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let ((process (haskell-interactive-process))
          (session (haskell-interactive-session))
          (import-regex (concat "^import[ ]+"
                                "\\(qualified \\)?"
                                "[ ]*\\(\"[^\"]*\" \\)?"  ;; Stuffs
                                "[ ]*\\([A-Za-z0-9_.']*\\)"  ;; Module name
                                ".*"  ;; Rest of first line (without \n)
                                "\\(\n +.*\\)*"   ;; Additional lines
                                )))
      (while (re-search-forward import-regex nil t)
        (let* ((import (match-string 0))
               (notice (concat "importing " (match-string 3) "\n"))
               (cmd (haskell-interactive-mode-new-multi-line process import)))
          (haskell-process-queue-command
           process
           (make-haskell-command
            :state (list :session session
                         :process process
                         :cmd cmd
                         :notice notice)
            :go (lambda (state)
                  (haskell-process-send-string
                   (plist-get state ':process) (plist-get state ':cmd)))
            :complete (lambda (state response)
                        (haskell-interactive-mode-insert-garbage
                         (plist-get state ':session)
                         (concat (plist-get state ':notice)))
                        (unless (string-blank-p response)
                          (haskell-interactive-mode-compile-error
                           (plist-get state ':session)
                           response
                           )))))
          )))))


;; TODO
;; - Use my-safe-linum to store the line number before evaluating a decl
;; - Process errors from eval-decl, offsetting with that line number
;; NOPE use %l in prompt to get current line number!
;; (defun my-safe-linum ()
;;   (let* ((s (haskell-interactive-session))
;;          (p (haskell-interactive-process)))
;;     (haskell-process-queue-command
;;      p
;;      (make-haskell-command
;;       :state (list :session s
;;                    :process p)  ;; use plist get state ':process
;;       :go (lambda (state)
;;             (haskell-process-send-string
;;              (plist-get state ':process)
;;              "xxxxxxxxxxxxxxxx = undefined\n:i xxxxxxxxxxxxxxxx"))
;;       :complete (lambda (state response)
;;                   (let ((num (with-temp-buffer
;;                                (insert response)
;;                                (goto-char 1)
;;                                (re-search-forward "xxxxxxxxxxxxxxxx.*<interactive>:\\([0-9]*\\)")
;;                                (match-string 1))))
;;                     (setq my-haskell-stored-linum (string-to-number num))))))))


(defun my-eval-decl (begin end)
  "Evaluate the code in region from BEGIN to END in the REPL.
If the region is unset, the current declaration will be used."
  (interactive "r")
  (save-excursion
    ;; Set begin and end if no region
    (unless (use-region-p)
      (forward-char)
      (beginning-of-defun)
      (setq begin (point))
      (end-of-defun)
      (setq end (point)))

    ;; Find decls in region
    (save-restriction
      (narrow-to-region begin end)
      (goto-char (point-min))
      (setq names nil)
      (while (let ((r (haskell-ds-generic-find-next-decl (haskell-ds-bird-p))))
               ;; r is nil or ((NAME . (START-POSITION . NAME-POSITION)) . TYPE)
               (setq name (car (car r))))
        (add-to-list 'names name t)))
    )

  (let* ((session (haskell-interactive-session))
         (process (haskell-interactive-process))
         (notice (concat (string-join names "\n") "\n"))
         (expr (buffer-substring-no-properties begin end))
         (cmd (haskell-interactive-mode-new-multi-line process expr))
         )

    ;; Following construction is very similar to the send-import one. Abstract away?
    (haskell-process-queue-command
     process
     (make-haskell-command
      :state (list :session session
                   :process process
                   :cmd cmd
                   :notice notice)
      :go (lambda (state)
            (haskell-process-send-string
             (plist-get state ':process) (plist-get state ':cmd)))
      :complete (lambda (state response)
                  (haskell-interactive-mode-insert-garbage
                   (plist-get state ':session)
                   (plist-get state ':notice))
                  (unless (string-blank-p response)
                    (haskell-interactive-mode-compile-error
                     (plist-get state ':session)
                     response
                     )))))
    ))

(defun company-haskell-imports (command &optional arg &rest ignored)
  "Company mode completions for imports"
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-haskell-imports))
    (prefix (company-grab-line "^import....*"))
    (candidates (search-haskell-imports arg))
    (no-cache t)
    ))

(add-hook 'haskell-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends)
                         'company-haskell-imports)))

(defun search-haskell-imports (arg)
  "Return suggestions by searching through the imports in all
haskell files throughout the project. Depends on projectile and
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
                                  " --file-search-regex .*hs$"
                                  " -- ")))
         (regex (concat "^import .*"
                        (s-join ".*" (-map (lambda (w)
                                             (concat "\\b" w)
                                             )
                                           (cdr (split-string arg))))))
         (command-list (append (split-string command) (list regex)))
         (result (let ((default-directory (projectile-project-root)))
                   (apply 'process-lines command-list)))
         )
    (cl-remove-duplicates result :test 'equal)))
