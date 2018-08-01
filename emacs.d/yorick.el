
;; My own settings
;; This file is meant to be portable across OS'es, ideally even within a terminal


;; --------------------------------------------------------------------------------
;; IVY completion

(require 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)  ;; Enable bookmarks and recentf
(global-set-key (kbd "M-s") 'swiper) ;; And use c-7 within swiper for multiple cursors
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-h i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "C-h u") 'counsel-unicode-char)
(global-set-key (kbd "C-x l") 'counsel-locate)
;; Maybe include command for changing ivy-occur to wgrep and mc mark?
;;   https://github.com/abo-abo/swiper/issues/589


;; --------------------------------------------------------------------------------
;;   Project navigation

;; Projectile
(require 'projectile)
;; (projectile-global-mode +1) ;; Replaced by counsel-projectile-mode
(setq projectile-show-paths-function 'projectile-hashify-with-relative-paths)
(setq projectile-completion-system 'ivy)

(require 'counsel-projectile)
(counsel-projectile-mode)


;; --------------------------------------------------------------------------------
;;   Git and diff

;; Slow git? See http://stackoverflow.com/questions/6724471/git-slows-down-emacs-to-death-how-to-fix-this/42414275#42414275

(require 'vc)
(setq vc-handled-backends nil)  ;; Magit replaces this

(require 'smerge-mode)
(setq smerge-command-prefix (kbd "C-c s"))

(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-bury-buffer-function 'magit-mode-quit-window) ;; Default was 'magit-restore-window-configuration
;; (setq magit-bury-buffer-function 'magit-restore-window-configuration) ;; Default was 'magit-restore-window-configuration

(custom-set-faces
 '(magit-diff-context-highlight ((t (:background "lemon chiffon" :foreground "grey50")))))


;; --------------------------------------------------------------------------------
;;   Haskell

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
        ;; Default included no-load, but i want all modules to be loaded??
        ))
(setq haskell-interactive-popup-errors nil)

(defun my-hoogle (query &optional info)
  "Do a Hoogle search for QUERY.

If prefix argument INFO is given, then hoogle is asked to show
extra info for the items matching QUERY.

Use `stack hoogle --rebuild --test --bench --no-run-tests --no-run-benchmarks` to generate the database.
"
  (interactive
   (let ((def (haskell-ident-at-point)))
     (if (and def (symbolp def)) (setq def (symbol-name def)))
     (list (read-string (if def
                            (format "Hoogle query (default %s): " def)
                          "Hoogle query: ")
                        nil nil def)
           current-prefix-arg)))
  (let* ((p (haskell-interactive-process))
         (cmd (concat ":!stack hoogle -- search "
                      "\'" query "\'"
                      ;; " --count=100"
                      " --link"
                      (if info " -i" "")
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
    (prefix (and (derived-mode-p 'haskell-mode)
                 (company-grab-line "import....*")))
    (candidates (search-haskell-imports arg))
    (no-cache t)
    ))
(add-to-list 'company-backends 'company-haskell-imports)

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
         (command (format counsel-ag-base-command ;; Reuse counsel config
                          (concat ignored
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


;; --------------------------------------------------------------------------------
;; Python

;; Python projects
(elpy-enable)

(setq venv-dirlookup-names '("ansible-venv" "requestmachine-venv" ".venv" "venv"))
(setq elpy-rpc-backend "jedi")

(setq pyvenv-mode nil)
(defun my-venv-projectile-auto-workon ()
  (interactive)
  (let* ((projectile-require-project-root nil) ;; So projectile-project-root doesn't err
         (path (--first
                (file-exists-p it)
                (--map (concat (projectile-project-root) it)
                        venv-dirlookup-names))))
    (if path
        (pyvenv-activate path)
      (pyvenv-deactivate)
      )))

;; Powerline figured out how to detect buffer/window focus pretty
;; reliably, so we reuse that to detect which venv we should switch
;; to. Use `pyvenv-virtual-env-name` to see the current venv.
(defadvice powerline-set-selected-window (after venv-set-selected-window activate)
  (my-venv-projectile-auto-workon))


;; --------------------------------------------------------------------------------
;; Purpose

(require 'window-purpose)   ;; Custom version, see init.el
(purpose-mode)

;; All magit windows have different magit purposes. This way magit is allowed to pop buffers (for
;; instance diffs) into windows that are not currently magit purpose.
(purpose-x-magit-multi-on)

(add-to-list 'purpose-user-mode-purposes '(haskell-interactive-mode . repl))
(add-to-list 'purpose-user-mode-purposes '(inferior-python-mode . repl))

(purpose-compile-user-configuration)

(require 'ivy-purpose)
(setq purpose-preferred-prompt 'vanilla)

;; If i do C-x b, i want my buffer to go in the current window.
(define-key purpose-mode-map (kbd "C-x b") 'ivy-purpose-switch-buffer-without-purpose)

;; For debugging, check out the default behaviours in purpose-action-sequences.
;; Set up debug logging of purpose's actions with this:
;; (setq purpose-message-on-p t)


;; --------------------------------------------------------------------------------
;; Window navigation / management

(winner-mode)

(menu-bar-mode 0)

;; Navigate buffers
(global-set-key (kbd "C-M-b") 'windmove-left)
(global-set-key (kbd "C-M-f") 'windmove-right)
(global-set-key (kbd "C-M-p") 'windmove-up)
(global-set-key (kbd "C-M-n") 'windmove-down)
(global-set-key (kbd "S-b") 'windmove-left)
(global-set-key (kbd "S-f") 'windmove-right)
(global-set-key (kbd "S-p") 'windmove-up)
(global-set-key (kbd "S-n") 'windmove-down)

;; Navigate frames
(require 'cl)
;; (require 'framemove)   ;; Disabled framemove for i3
;; (setq framemove-hook-into-windmove t)
;; (setq display-buffer-reuse-frames t)
(add-to-list 'display-buffer-alist
             '("." nil (reusable-frames . t))) ;; Because display-buffer-reuse-frames is obsolete
(add-hook 'next-error-hook 'raise-frame) ;; next-error will select the right window, but may have raised another frame due to reusable-frames.



;; --------------------------------------------------------------------------------
;; Hydra

(require 'hydra)

(global-set-key (kbd "C-`") 'hydra-hydra/body)
(defhydra hydra-hydra (:hint nil :exit t)
  "
_w_indow
_s_merge %`smerge-mode
_q_uit
"
  ("s" hydra-smerge/body)
  ("C-s" hydra-smerge/body)
  ("w" hydra-window/body)
  ("C-w" hydra-window/body)
  ("q" nil)
  )

(defhydra hydra-window (:hint nil)
  "
_b_/_f_/_p_/_n_ Movement  _0_ Delete window   _+_/_-_ Vert resize     _d_/_D_ Dedicate to purpose/buffer
^^^^^^      _z_ Undo      _1_ Delete others   _{_/_}_ Horiz resize    _k_ Kill buffer
^^^^^^      _Z_ Redo      _2_ Split below     ^^^^                    _q_/_SPC_/_RET_ Quit
^^^^^^      ^^            _3_ Split right
"
   ("b" windmove-left )
   ("n" windmove-down )
   ("p" windmove-up )
   ("f" windmove-right )
   ("0" delete-window)
   ("1" delete-other-windows)
   ("2" split-window-below)
   ("3" split-window-right)
   ("+" enlarge-window)
   ("-" shrink-window)
   ("=" enlarge-window)
   ("{" shrink-window-horizontally)
   ("}" enlarge-window-horizontally)
   ("z" (progn
          (winner-undo)
          (setq this-command 'winner-undo))
   )
   ("Z" winner-redo)
   ("d" purpose-toggle-window-purpose-dedicated)
   ("D" purpose-toggle-window-buffer-dedicated)
   ("o" ivy-purpose-switch-buffer-without-purpose)
   ("k" kill-this-buffer)
   ("q" nil)
   ("SPC" nil)
   ("RET" nil)
   )

(defhydra hydra-smerge (:hint nil :pre (smerge-mode 1))
  "
^Move^ ^Keep^ ^Diff^ ^Pair^
------------------------------------------------------
_n_ext _b_ase _R_efine _<_: base-mine
_p_rev _m_ine _E_diff _=_: mine-other
^ ^ _o_ther _C_ombine _>_: base-other
^ ^ _a_ll _r_esolve
_q_uit _RET_: current
"
    ("RET" smerge-keep-current)
    ("C" smerge-combine-with-next)
    ("E" smerge-ediff)
    ("R" smerge-refine)
    ("a" smerge-keep-all)
    ("b" smerge-keep-base)
    ("m" smerge-keep-mine)
    ("n" smerge-next)
    ("o" smerge-keep-other)
    ("p" smerge-prev)
    ("r" smerge-resolve)
    ("<" smerge-diff-base-mine)
    ("=" smerge-diff-mine-other)
    (">" smerge-diff-base-other)
    ("q" nil :color blue)
    )

;; --------------------------------------------------------------------------------
;; Misc

(global-set-key (kbd "C-z") 'undo)

(put 'narrow-to-region 'disabled nil)

;; Disable the tool bar
(tool-bar-mode -1)

;; Backup files in temp dir
(setq backup-directory-alist '(("." . "~/.saves"))
      auto-save-file-name-transforms '((".*" "~/.saves/" t))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Smaller steps for text-scale-adjust
(setq-default text-scale-mode-step 1.04)

;; Flycheck only check on save and mode-enabled (not on new-line or idle-change)
(setq flycheck-check-syntax-automatically '(mode-enabled save))

;; Truncate lines instead of wrapping
(set-default 'truncate-lines t)
(setq truncate-partial-width-windows nil)

;; Server for editing from command line
(server-start)

(setq column-number-mode t)         ;; Show column by default
(global-auto-revert-mode 1)         ;; Update contents of a file automatically
(setq-default indent-tabs-mode nil) ;; Spaces, no tabs
(setq-default show-trailing-whitespace t) ;;
(setq-default indicate-empty-lines t) ;;
(show-paren-mode 1)                 ;; Highlight matching parentheses
(setq split-height-threshold 30)
(setq split-width-threshold 100)

;; Pallet to maintain entries in ~/.emacs.d/Cask automatically
(require 'pallet)
(pallet-mode t)

;; Multiple cursor stuff
(require 'multiple-cursors)
(global-set-key (kbd "C-M-l") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Normal search does not work well with multiple cursors
(require 'phi-search)
(global-set-key (kbd "C-s") 'phi-search)
(global-set-key (kbd "C-r") 'phi-search-backward)
(setq phi-search-case-sensitive  'guess)

;; Auto highlight
(setq auto-highlight-symbol-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-,"   ) 'ahs-backward            )
        (define-key map (kbd "C-."   ) 'ahs-forward             )
        ;; (define-key map (kbd "M-S-<left>"  ) 'ahs-backward-definition )
        ;; (define-key map (kbd "M-S-<right>" ) 'ahs-forward-definition  )
        ;; (define-key map (kbd "M--"    ) 'ahs-back-to-start       )
        ;; (define-key map (kbd "C-x C-'"     ) 'ahs-change-range        )
        ;; (define-key map (kbd "C-x C-a"     ) 'ahs-edit-mode           )
        map))
(require 'auto-highlight-symbol)
(setq ahs-default-range 'ahs-range-whole-buffer)
(add-to-list 'ahs-modes 'haskell-mode)
(global-auto-highlight-symbol-mode 1)


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

;; Dired
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; Keep track of the actually selected window for mode line
(defvar my-real-selected-window nil)
(add-function :before pre-redisplay-function
              (lambda (_wins) (setq my-real-selected-window (selected-window))))

;; Clean up mode line
(setq sml/theme 'light-powerline)
(setq sml/no-confirm-load-theme t)
(setq sml/name-width 50)
(sml/setup)

(setq-default mode-line-format
      (quote ("%e"
         mode-line-front-space
         mode-line-mule-info
         mode-line-client
         mode-line-modified
         mode-line-remote
         mode-line-frame-identification
         mode-line-buffer-identification
         sml/pos-id-separator
         mode-line-position
         ;; (vc-mode vc-mode)
         sml/pre-modes-separator
         mode-line-modes
         mode-line-misc-info
         mode-line-end-spaces
         (:eval (when (eq (selected-window) my-real-selected-window)
                  pyvenv-virtual-env-name))
         )))

;; These minor modes do not have to be shown in mode line
(assq-delete-all 'auto-highlight-symbol-mode minor-mode-alist)
(assq-delete-all 'company-mode minor-mode-alist)
(assq-delete-all 'ivy-mode minor-mode-alist)
(assq-delete-all 'projectile-mode minor-mode-alist)
(assq-delete-all 'interactive-haskell-mode minor-mode-alist)
(assq-delete-all 'auto-revert-mode minor-mode-alist)
(assq-delete-all 'which-key-mode minor-mode-alist)


;; --------------------------------------------------------------------------------
;; Custom functions

;; Duplicate with 'C-c d', comment and duplicate with 'C-u C-c d'
(defun duplicate-line-or-region (&optional arg)
  ""
  (interactive "*P")
  (let ((n         (if (listp arg) 1 arg))
	(docomment (consp arg))
	(beginning (if (use-region-p) (region-beginning) (line-beginning-position)))
	(end       (if (use-region-p) (region-end)       (+ (line-end-position) 1))))
    (let ((text (buffer-substring beginning end)))
      (goto-char end)
      (when docomment (comment-region beginning end))
      (save-excursion
	(dotimes (i n) (insert text))))))
(global-set-key (kbd "C-c d") 'duplicate-line-or-region)

;; Use C-' for commenting
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))
(global-set-key (kbd "C-'") 'comment-or-uncomment-region-or-line)

;; C-a toggles between column 0 and beginning of indented line
(defun beginning-of-line-or-indentation ()
  "Move to beginning of line, or indentation"
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))
(global-set-key (kbd "C-a") 'beginning-of-line-or-indentation)



;; For multiple cursors: sorting lines by region
(defvar mc--lists-to-replace nil)
(defun mc--replace-region-list ()
  (interactive)
  (destructuring-bind (prefix . (str . postfix)) (car mc--lists-to-replace)
    (let* ((x1 (region-beginning))
           (x2 (region-end))
           (x0 (progn (goto-char x1) (line-beginning-position)))
           (x3 (progn (goto-char x2) (line-end-position)))
           )
      (delete-region x0 x3)
      (insert prefix)
      (save-excursion (insert postfix))
      (let (xx (point-marker))
        (insert str)
        (set-mark xx)
        (setq deactivate-mark nil))
      )
    )
  (setq mc--lists-to-replace (cdr mc--lists-to-replace)))

(defun mc--replace-region-lists (lists)
  (setq mc--lists-to-replace lists)
  (mc/for-each-cursor-ordered
   (mc/execute-command-for-fake-cursor 'mc--replace-region-list cursor)))

;; mc--grab-region-lists :: [(Prefix, RegionText, Postfix)]
(defun mc--get-region-line-lists ()
  (let (xs)
    (save-excursion
      (mc/for-each-cursor-ordered
       (let* ((x1 (mc/cursor-beg cursor))
              (x2 (mc/cursor-end cursor))
              (x0 (progn (goto-char x1) (line-beginning-position)))
              (x3 (progn (goto-char x2) (line-end-position)))
              (prefix  (buffer-substring-no-properties x0 x1))
              (str     (buffer-substring-no-properties x1 x2))
              (postfix (buffer-substring-no-properties x2 x3))
              )
         ;; (delete-region x0 x3)
         (add-to-list 'xs (cons prefix (cons str postfix))))))
    xs))

(defun mc/sort-lines-by-region ()
  (interactive)
  (let* ((old    (mc--get-region-line-lists))
         (sorted (sort old (lambda (a b) (string< (car (cdr a)) (car (cdr b))))))
         )
    (mc--replace-region-lists sorted)
    ))
