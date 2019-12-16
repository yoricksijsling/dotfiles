
;; Stuff that I really need to do anything useful


;; (require 'cl) ;; Not sure why we need this one

(global-set-key (kbd "C-z") 'undo)

;; Disable tool bar and menu bar
(tool-bar-mode -1)
(menu-bar-mode 0)

;; Backup files in temp dir
(setq backup-directory-alist '(("." . "~/.saves"))
      auto-save-file-name-transforms '((".*" "~/.saves/" t))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq column-number-mode t)         ;; Show column by default
(global-auto-revert-mode 1)         ;; Update contents of a file automatically
(setq-default indent-tabs-mode nil) ;; Spaces, no tabs
(setq-default show-trailing-whitespace t) ;;
(setq-default indicate-empty-lines t) ;;
(show-paren-mode 1)                 ;; Highlight matching parentheses
(setq split-height-threshold 30)
(setq split-width-threshold 100)

;; Truncate lines instead of wrapping
(set-default 'truncate-lines t)
(setq truncate-partial-width-windows nil)
;; Line wrapping for text modes
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
;; (setq visual-line-fringe-indicators '(nil nil))
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

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

;; --------------------------------------------------------------------------------
;; Window navigation / management

(winner-mode)

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
(add-to-list 'display-buffer-alist
             '("." nil (reusable-frames . t))) ;; Because display-buffer-reuse-frames is obsolete
(add-hook 'next-error-hook 'raise-frame) ;; next-error will select the right window, but may have raised another frame due to reusable-frames.


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
