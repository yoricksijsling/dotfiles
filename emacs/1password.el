

;; (defvar-local op-session nil
;;   "Sessions are buffer-local, and for simplicity we only allow 1 session per buffer"
;;   )

;; (defun op-signin-buffer-local (pass)
;;   "Interactively ask password and sign in to 1password"
;;   (interactive (list (read-passwd "pass ")))
;;   (let* ((user-uuid (op-account-1-user-uuid))
;;          (session (--op-signin-session user-uuid pass))
;;          )
;;     (message "Setting session for %s to %s" user-uuid session)
;;     (setq op-session (list user-uuid session))))


(cl-defun op-signin-and-get (item fields &key vault)
  (let* ((user-uuid (op-account-1-user-uuid))
         (pass (read-passwd "1password pass: "))
         (session (--op-signin-session user-uuid pass))
         )
    (op-item-get item :fields fields :vault vault :account user-uuid :session session)
    )
  )

(defun --op-signin-session (user-uuid pass)
  (with-output-to-string ;; sets standard-output
    (with-temp-buffer
      (insert pass)
      (call-process-region
       (point-min)
       (point-max)
       "op"
       nil
       standard-output
       nil
       "signin"
       "--raw"
       "--account"
       user-uuid
       )
      )
    )
  )

;; (defun --op-signout (user-uuid)

(defun op-account-1-user-uuid ()
  "Get user uuid of the first available account."
  (interactive)
  (with-temp-buffer
    (call-process "op" nil (current-buffer) nil "account" "list" "--format" "json")
    (beginning-of-buffer)  ;; json-parse-buffer starts at point
    (let ((parsed (json-parse-buffer :object-type 'alist :array-type 'list)))
      (alist-get 'user_uuid (car parsed))
      )
    )
  )

(cl-defun op-item-get (item &key fields vault account session)
  (with-temp-buffer
    (let* ((args (append
                  (list "item" "get" item)
                  (when fields (list "--fields" fields))
                  (when vault (list "--vault" vault))
                  (when account (list "--account" account))
                  (when session (list "--session" session))
                  )
                 )
           (status (apply 'call-process "op" nil (current-buffer) nil args))
           (output (buffer-string))
           )
      (if (eq status 0)
          (string-trim-right output)
        (error "op exited with status %s and output %s" status output))
      )
    )
  )


;; (defun op-get-field (item field)
;;   (let* (;; TODO: use proper construction of command (no string concat)
;;          ;; TODO: use json for response
;;          ;; TODO: include optional --vault
;;          (cmd (concat "op item get '" item "' --fields " field))
;;          (result (shell-command-to-string cmd)))
;;     (string-trim-right result)))


;; (defun op-get-fields (item fields)
;;   (let* (;; TODO: use proper construction of command (no string concat)
;;          ;; TODO: use json for response
;;          ;; TODO: include optional --vault
;;          (cmd (concat "op item get '" item "' --fields " (s-join "," fields)))
;;          (result (shell-command-to-string cmd)))
;;     (s-split "," (string-trim-right result))))

;; (defun op-get-field (item fields)
;;   (with-temp-buffer
;;     (let* ((args (list "item" "get" item "--fields" fields))
;;            (status (apply 'call-process "op" nil (current-buffer) nil args))
;;            (output (buffer-string))
;;            )
;;       (if (eq status 0)
;;           (string-trim-right output)
;;         (error "op exited with status %s and output %s" status output))
;;       )
;;     )
;;   )
