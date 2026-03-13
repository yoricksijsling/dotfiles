
(require 'gptel)
(gptel-make-gh-copilot "Copilot")

;; (setq gptel-model 'claude-3.7-sonnet)
(setq gptel-backend (gptel-make-gh-copilot "Copilot"))

;; gptel


(require 'copilot)

(setq copilot-idle-delay 3)
(keymap-set copilot-completion-map "<right>" #'copilot-accept-completion)
(keymap-set copilot-completion-map "C-f" #'copilot-accept-completion)
(keymap-set copilot-completion-map "M-<right>" #'copilot-accept-completion-by-word)
(keymap-set copilot-completion-map "M-f" #'copilot-accept-completion-by-word)
(keymap-set copilot-completion-map "C-e" #'copilot-accept-completion-by-line)
(keymap-set copilot-completion-map "<end>" #'copilot-accept-completion-by-line)
(keymap-set copilot-completion-map "M-n" #'copilot-next-completion)
(keymap-set copilot-completion-map "M-p" #'copilot-previous-completion)
