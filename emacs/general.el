
;; Generally useful stuff, not specific to any programming/markup/configuration language


;; Dired
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(put 'narrow-to-region 'disabled nil)

;; Svg can crash emacs image-mode, and I prefer them in text anyways
(setq auto-mode-alist (remove '("\\.svgz?\\'" . image-mode) auto-mode-alist))

;; Smaller steps for text-scale-adjust
(setq-default text-scale-mode-step 1.04)

;; Flycheck only check on save and mode-enabled (not on new-line or idle-change)
(setq flycheck-check-syntax-automatically '(mode-enabled save))

;; Server for editing from command line
(server-start)


;; --------------------------------------------------------------------------------
;; Project navigation

(require 'projectile)
;; (projectile-global-mode +1) ;; Replaced by counsel-projectile-mode
(setq projectile-show-paths-function 'projectile-hashify-with-relative-paths)
(setq projectile-completion-system 'ivy)

;; Use .projectile file ignores when finding files
(setq projectile-indexing-method 'hybrid)

;; Note:
;; projectile-find-file uses .projectile, .ignore and .gitignore
;; projectile-ripgrep uses .ignore and .gitignore

;; Default is "rg --with-filename --no-heading --line-number --color never %s"
;; I want to include hidden directories and files
(setq counsel-rg-base-command "rg --hidden --with-filename --no-heading --line-number --color never %s")

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(require 'counsel-projectile)
(counsel-projectile-mode)

;; https://github.com/bbatsov/projectile/issues/1777#issuecomment-1170113133
(setq projectile-globally-ignored-directories
  '(".idea"
    ".vscode"
    ".ensime_cache"
    ".eunit"
    ".git"
    ".hg"
    ".fslckout"
    "_FOSSIL_"
    ".bzr"
    "_darcs"
    ".pijul"
    ".tox"
    ".svn"
    ".stack-work"
    ".ccls-cache"
    ".cache"
    ".clangd"))


;; --------------------------------------------------------------------------------
;; Which-key

(require 'which-key)
(setq which-key-idle-delay 1.0)
(which-key-mode)


;; --------------------------------------------------------------------------------
;; LSP

;; Keybindings: https://emacs-lsp.github.io/lsp-mode/page/keybindings/
(setq lsp-keymap-prefix "C-c l")  ;; Default is "s-l" but that's my screen lock
(require 'lsp-mode)
(add-hook 'lsp-mode-hook 'lsp-enable-which-key-integration)

;; More optional stuff
;;   lsp-ui
;;   lsp-ivy
;;   dap-mode

(require 'yasnippet)  ;; Necessary to make the placeholders in lsp company autocompletions work
(yas-global-mode 1)




;; --------------------------------------------------------------------------------
;; Auto highlight symbols

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

;; The below method to replace the keymap doesn't seem to work, haven't investigated why
;; (setq auto-highlight-symbol-mode-map (make-sparse-keymap))
;; (define-key auto-highlight-symbol-mode-map (kbd "C-,"   ) 'ahs-backward            )
;; (define-key auto-highlight-symbol-mode-map (kbd "C-."   ) 'ahs-forward             )
;; (define-key auto-highlight-symbol-mode-map (kbd "M-S-<left>"  ) 'ahs-backward-definition )
;; (define-key auto-highlight-symbol-mode-map (kbd "M-S-<right>" ) 'ahs-forward-definition  )
;; (define-key auto-highlight-symbol-mode-map (kbd "M--"    ) 'ahs-back-to-start       )
;; (define-key auto-highlight-symbol-mode-map (kbd "C-x C-'"     ) 'ahs-change-range        )
;; (define-key auto-highlight-symbol-mode-map (kbd "C-x C-a"     ) 'ahs-edit-mode           )

(setq ahs-default-range 'ahs-range-whole-buffer)
(add-to-list 'ahs-modes 'haskell-mode)
(global-auto-highlight-symbol-mode 1)


;; --------------------------------------------------------------------------------
;; Purpose

(require 'window-purpose)   ;; Custom version, see init.el
(require 'ivy-purpose)
(purpose-mode)

;; All magit windows have different magit purposes. This way magit is allowed to pop buffers (for
;; instance diffs) into windows that are not currently magit purpose.
(purpose-x-magit-multi-on)

(setq purpose-preferred-prompt 'vanilla)

(add-to-list 'purpose-user-mode-purposes '(haskell-interactive-mode . repl))
(add-to-list 'purpose-user-mode-purposes '(inferior-python-mode . repl))
(add-to-list 'purpose-user-mode-purposes '(ein:notebook-multilang-mode . repl))
(add-to-list 'purpose-user-name-purposes '("*ghcid*" . repl))
(add-to-list 'purpose-user-name-purposes '("*SQL*" . repl))
(purpose-compile-user-configuration)

;; If i do C-x b, i want my buffer to go in the current window.
(define-key purpose-mode-map (kbd "C-x b") 'ivy-purpose-switch-buffer-without-purpose)

;; For debugging, check out the default behaviours in purpose-action-sequences.
;; Set up debug logging of purpose's actions with this:
;; (setq purpose-message-on-p t)


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

(defun my-git-relevant-branches-for-rev (rev)
  (-non-nil (list rev
                  (magit-get-upstream-branch rev)
                  (magit-get-push-branch rev))))

(defun my-git-relevant-branches-for-revs (revs)
  (-distinct
   (-map 'substring-no-properties
         (-mapcat 'my-git-relevant-branches-for-rev revs))))

(defun magit-log-current-relevant (revs &optional args files)
  "Log current, upstream and push branches.
Similar interactive behaviour as `magit-log-current'."
  (interactive (cons (magit-log-read-revs t)
                     (magit-log-arguments)))
  (magit-log-setup-buffer (my-git-relevant-branches-for-revs revs)
                          args
                          files))

(defun magit-log-current-relevant-master (revs &optional args files)
  "Show current, upstream and push branches. For master as well.
Similar interactive behaviour as `magit-log-current'."
  (interactive (cons (magit-log-read-revs t)
                     (magit-log-arguments)))
  (magit-log-setup-buffer (my-git-relevant-branches-for-revs (cons "master" revs))
                          args
                          files))

(transient-append-suffix 'magit-log "h"
  '("c" "current and relevant" magit-log-current-relevant))
(transient-append-suffix 'magit-log "c"
  '("C" "current, relevant and master" magit-log-current-relevant-master))


;; --------------------------------------------------------------------------------
;; Keep track of selected window and buffer

(defvar my-selected-window nil
  "Currently selected window as the user sees it.
Changes when the user moves to another buffer or focuses another window.
Does not change when using `with-temporary-buffer' or `with-selected-window'.
   ")

(defvar my-selected-window-changed-hook nil
  "Hook called after `my-selected-window' has changed.")

(setq my-selected-window/invalidated nil)
(defun my-invalidate-selected-window ()
  (setq my-selected-window/invalidated t))
(defun my-refresh-selected-window ()
  (when my-selected-window/invalidated
    (setq my-selected-window/invalidated nil)
    (let ((new-selected-window (selected-window)))
      (when (not (eq new-selected-window my-selected-window))
        ;; (message "Setting selected window to %s" (selected-window))
        ;; (message nil)
        (setq my-selected-window (selected-window))
        (run-hooks 'my-selected-window-changed-hook)
    ))))
(add-hook 'buffer-list-update-hook 'my-invalidate-selected-window)
(add-hook 'post-command-hook 'my-refresh-selected-window)
(add-function :before pre-redisplay-function (lambda (_wins) (my-refresh-selected-window)))


;; --------------------------------------------------------------------------------
;; Mode line

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
         ;; mode-line-position
         ;; (vc-mode vc-mode)
         ;; sml/pre-modes-separator
         mode-line-modes       ;; Major mode and minor modes
         mode-line-misc-info   ;; Would normally includes venv
         (:eval (when (eq (selected-window) my-selected-window)
                  pyvenv-virtual-env-name))
         mode-line-end-spaces
         )))

;; These minor modes do not have to be shown in mode line
(assq-delete-all 'auto-highlight-symbol-mode minor-mode-alist)
(assq-delete-all 'company-mode minor-mode-alist)
(assq-delete-all 'ivy-mode minor-mode-alist)
(assq-delete-all 'projectile-mode minor-mode-alist)
(with-eval-after-load 'haskell
  (assq-delete-all 'interactive-haskell-mode minor-mode-alist))
(assq-delete-all 'auto-revert-mode minor-mode-alist)
(assq-delete-all 'which-key-mode minor-mode-alist)
(assq-delete-all 'highlight-indentation-mode minor-mode-alist)
(assq-delete-all 'outline-minor-mode minor-mode-alist)
(assq-delete-all 'buffer-face-mode minor-mode-alist)
(with-eval-after-load 'python
  (assq-delete-all 'lsp-mode minor-mode-alist))
(assq-delete-all 'eldoc-mode minor-mode-alist)
(assq-delete-all 'yas-minor-mode minor-mode-alist)


;; --------------------------------------------------------------------------------
;; Hydra

(require 'hydra)

(global-set-key (kbd "C-`") 'hydra-hydra/body)
(defhydra hydra-hydra (:hint nil :exit t)
  "
_w_indow
_s_merge %`smerge-mode
_m_arkdown
_q_uit
"
  ("s" hydra-smerge/body)
  ("C-s" hydra-smerge/body)
  ("w" hydra-window/body)
  ("C-w" hydra-window/body)
  ("m" hydra-markdown/body)
  ("C-m" hydra-markdown/body)
  ("q" nil)
  )

;; (defhydra hydra-yank-pop ()
;;   "yank"
;;   ("C-y" yank nil)
;;   ("M-y" yank-pop nil)
;;   ("y" (yank-pop 1) "next")
;;   ("Y" (yank-pop -1) "prev"))
;; (global-set-key (kbd "M-y") #'hydra-yank-pop/yank-pop)
;; (global-set-key (kbd "C-y") #'hydra-yank-pop/yank)

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

(defhydra hydra-smerge (:hint nil :pre (smerge-mode 1) :post (smerge-mode -1))
  "
_p_rev    _<_: Diff base-mine   _k_<: Keep mine   _C_ombine
_n_ext    _=_: Diff mine-other  _k_=: Keep base   _R_efine
^^        _>_: Diff base-other  _k_>: Keep other  _r_esolve
_q_uit    _0_: Close diff       _k_a: Keep all    _E_diff
"
  ("p" smerge-prev)
  ("n" smerge-next)
  ("<" smerge-diff-base-mine)
  ("=" smerge-diff-mine-other)
  (">" smerge-diff-base-other)
  ("0" delete-smerge-diff-window)
  ("k" hydra-smerge-keep/body :exit t)
  ("C" smerge-combine-with-next)
  ("R" smerge-refine)
  ("r" smerge-resolve)
  ("E" smerge-ediff)
  ("q" nil)
  ("RET" nil)
  ;; Basic navigation
  ("C-p" previous-line)
  ("C-n" next-line)
  ("C-l" recenter-top-bottom)
  )

(defun delete-smerge-diff-window ()
  (interactive)
  (let ((window (get-buffer-window smerge-diff-buffer-name)))
    (when window (delete-window window))))


;; Called from hydra-smerge
(defhydra hydra-smerge-keep (:hint nil :exit t)
  "
^^      ^^                    k_<_: Keep mine
^^      ^^                    k_=_: Keep base
_RET_: Quit^^                 k_>_: Keep other
_q_:   back to hydra-smerge^^ k_a_: Keep all
"
  ("<" (progn (smerge-keep-mine) (hydra-smerge/body)))
  ("=" (progn (smerge-keep-base) (hydra-smerge/body)))
  (">" (progn (smerge-keep-other) (hydra-smerge/body)))
  ("a" (progn (smerge-keep-all) (hydra-smerge/body)))
  ("q" hydra-smerge/body)
  ("RET" nil)
  )

(defhydra hydra-markdown (:hint nil)
  "
^^         ^^                  With current heading:
_a_ll      _p_revious heading  TAB: Cycle visibility
_o_utline  _n_ext heading      _s_: Hide subtree
_q_uit                         _t_: Show subtree
"
  ("a" outline-show-all)
  ("o" (progn (outline-show-branches) (outline-hide-leaves)))
  ("p" markdown-outline-previous)
  ("C-p" markdown-outline-previous)
  ("n" markdown-outline-next)
  ("C-n" markdown-outline-next)
  ("TAB" markdown-cycle)
  ("s" outline-hide-subtree)
  ("t" outline-show-subtree)
  ;; ("l" markdown-hide-leaves)
  ;; ("C-k" markdown-show-branches)
  ;; ("C-p" markdown-outline-previous)
  ;; ("C-n" markdown-outline-next)
  ;; ("C-b" markdown-outline-previous-same-level)
  ;; ("C-f" markdown-outline-next-same-level)
  ;; ("C-t" outline-hide-body)       ;; Default C-c @ C-t
  ;; ("C-c" outline-hide-entry)      ;; Default C-c @ C-c
  ;; ("C-l" outline-hide-leaves)     ;; Default C-c @ C-l
  ;; ("C-o" outline-hide-other)      ;; Default C-c @ C-o
  ;; ("C-q" outline-hide-sublevels)  ;; Default C-c @ C-q
  ;; ("C-d" outline-hide-subtree)    ;; Default C-c @ C-d
  ;; ("C-x" outline-toggle-children) ;; No shortcut by default
  ;; ("C-a" outline-show-all)        ;; Default C-c @ C-a
  ;; ("C-k" outline-show-branches)   ;; Default C-c @ C-k
  ;; ("TAB" outline-show-children)   ;; Default C-c @ TAB
  ;; ("C-e" outline-show-entry)      ;; Default C-c @ C-e
  ;; ("C-s" outline-show-subtree)    ;; Default C-c @ C-s
  ("q" nil)
  ("RET" nil)
  )


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
  (let* ((prefix  (car  (car mc--lists-to-replace)))
         (str     (cadr (car mc--lists-to-replace)))
         (postfix (cddr (car mc--lists-to-replace)))
         (x1 (region-beginning))
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
    (reverse xs)))

(defun mc/sort-lines-by-region ()
  (interactive)
  (let* ((old    (mc--get-region-line-lists))
         (sorted (sort old (lambda (a b) (string< (cadr a) (cadr b)))))
         )
    (mc--replace-region-lists sorted)
    ))
