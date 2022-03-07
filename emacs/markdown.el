
;;--------------------------------------------------------------------------------
;; Markdown

;; Toggle tree visibility with TAB
;; Navigate between headers with C-c C-p and C-c C-n

(require 'markdown-mode)

(setq markdown-gfm-use-electric-backquote nil)
(setq markdown-fontify-code-blocks-natively t)
(add-to-list 'auto-mode-alist '("\\.md\\'"       . gfm-mode)) ;; github-flavoured-markdown
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode)) ;; github-flavoured-markdown
(add-hook 'gfm-mode-hook 'variable-pitch-mode) ;; Proportional font
(define-key markdown-mode-map (kbd "C-c C-r") 'markdown-sql-send)

;; Headers
(setq markdown-header-scaling t)
;; (setq markdown-header-scaling-values '(2.0 1.5 1.25 1.0 0.875 0.85))  ;; Github sizes
(setq markdown-header-scaling-values '(1.73 1.44 1.2 1.0 1.0 1.0))  ;; max(1, 1.2^(n-2))
(markdown-update-header-faces markdown-header-scaling markdown-header-scaling-values)

;; Outline minor mode is necessary to toggle tree visibility
(add-hook 'markdown-mode-hook 'outline-minor-mode)

;;--------------------------------------------------------------------------------
;; Wiki links

(setq markdown-enable-wiki-links t)

;; Don't replace spaces. This matches GitJournal, Obsidian,
;; and Gollum wiki (https://github.com/gollum/gollum/wiki#tags)
(setq markdown-link-space-sub-char " ")

;; To keep things simple, all notes are in the root directory. Just like wikipedia.
(add-hook 'gfm-mode-hook
          (lambda ()
            ;; GFM sets this variable but we don't want it
            (setq markdown-wiki-link-search-subdirectories nil)))

;; Pushing and popping for wiki links
(define-key markdown-mode-map (kbd "M-.") 'markdown-follow-wiki-link-at-point)

(defun markdown-follow-wiki-link-at-point (&optional arg)
  "Find Wiki Link at point.
With prefix argument ARG, open the file in other window.
See `markdown-wiki-link-p' and `markdown-follow-wiki-link'."
  (interactive "P")
  (if (markdown-wiki-link-p)
      (progn
        (xref-push-marker-stack (point-marker))
        (markdown-follow-wiki-link (markdown-wiki-link-link) arg)
        )
    (user-error "Point is not at a Wiki Link")))



;;--------------------------------------------------------------------------------
;; Completion for wiki links

(add-hook 'gfm-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends)
                         'company-markdown-wiki-links)))

(defun company-markdown-wiki-links (command &optional arg &rest ignored)
  "Company mode completions for markdown wiki links"
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-markdown-wiki-links))
    (prefix (and (not (markdown-code-block-at-point-p)) (company-grab-line "\\[\\[.*")))
    (candidates (search-markdown-wiki-links arg))
    (no-cache t)
    ))

(defun search-markdown-wiki-links (arg)
  (message "Looking for %s" arg)
  (let* ((regex (s-concat (s-join ".*" (split-string (s-chop-prefix "[[" arg))) ".*.md"))
         (files (directory-files "." nil regex)))

    (--map (s-concat "[[" (s-chop-suffix ".md" it)) files)
    ))


;; --------------------------------------------------------------------------------
;; Scratch notes

(defun scratch ()
  (interactive)
  (let ((basename (format-time-string "/home/yorick/notes/%Y-%m-%d scratch%%i.md"))
        (i 0))
    (while (and (< i 10) (get-file-buffer (format basename i)))
      (setq i (1+ i)))
    (let* ((filename (format basename i))
           (buffer (get-buffer-create filename)))
      (message "Created scratch buffer %s" filename)
      (with-current-buffer buffer
        (set-visited-file-name filename)
        (gfm-mode)
        )
      (pop-to-buffer buffer)
      ))
  )
