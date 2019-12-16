;; --------------------------------------------------------------------------------
;; SQL

(require 'sql)
(setq sql-postgres-login-params
      '((user :default "yorick")
        (database :default "yorick")
        password
        server))

(defun sql-start-session (connection &optional dont-pop-to-buffer)
  "Open connection in a buffer called *SQL: my-connection*."
  (interactive
   (list (sql-read-connection "Connection: " nil '(nil))))

  (let* ((buffer-name (format "*SQL: %s*" connection))
         (existing-buffer (get-buffer buffer-name))
         ;; sql-connect requires sql-product to be set. We take it from the
         ;; connection. Weird that sql-connect doesn't already do that.
         (connect-set (assoc-string connection sql-connection-alist t))
         (initial-window (selected-window))
         )

    ;; When we use commands like sql-connect and sql-list-table in the current
    ;; buffer, the product of the given connection must be used.
    (set (make-local-variable 'sql-product)
         (eval (cadr (assoc 'sql-product (cdr connect-set)))))

    ;; Enable product-specific syntax highlighting in the current buffer
    (sql-highlight-product)
    (if (and existing-buffer (get-buffer-process existing-buffer))
        (progn
          ;; Ensure that the local sql-buffer variable is correctly set.
          (set (make-local-variable 'sql-buffer) existing-buffer)
          (unless dont-pop-to-buffer (pop-to-buffer existing-buffer))
          )
      (let ((new-buffer (sql-connect connection connection)))
        (set (make-local-variable 'sql-buffer) new-buffer))
      (when dont-pop-to-buffer
        ;; sql-connect always pops to the new buffer, so try to undo it.
        (select-window initial-window)))
    ))

(defun markdown-sql-send (current-point)
  "Send the sql expression at point to an sql session."
  (interactive "d")  ;; Get point
  (let ((code-bounds (get-text-property current-point 'markdown-gfm-code)))
    (unless code-bounds (error "Not in code block"))

    ;; markdown-get-enclosing-fenced-block-construct also extracts the current
    ;; code block, but includes the start and end tags.
    (let ((code (buffer-substring-no-properties (cl-first code-bounds) (cl-second code-bounds)))
          (lang (save-excursion (markdown-code-block-lang)))
          (after-code (save-excursion (goto-char (cl-second code-bounds))
                                      (next-line)
                                      (point))))

      (when (s-starts-with? "sql-" lang)
        (let ((connection (s-chop-prefix "sql-" lang)))
          (sql-start-session connection t)
          (sql-redirect-one sql-buffer code "*SQL-GFM-TEMP*" nil)

          (let* ((raw (with-current-buffer "*SQL-GFM-TEMP*"
                        (buffer-substring-no-properties (point-min) (point-max))))

                 ;; (prompts (list (sql-get-product-feature sql-product :prompt-cont-regexp)
                 ;;                (sql-get-product-feature sql-product :prompt-regexp)))
                 (prompts (list "^\\(\\w*[-(][#>] \\)*"  ;; Remove multiple continuation prompts
                                "^\\w*=[#>] "))
                 (without-prompts (--reduce-from (s-replace-regexp it "" acc) raw prompts))

                 (processed (apply 's-concat
                                   (--map (concat "|" (s-truncate 1000 it "| ...") "\n")
                                          (--remove (or (s-blank? it)
                                                        (s-matches? "^([0-9]+ rows?)$" it))
                                                    (s-lines without-prompts))))))

            (save-excursion
              ;; Move after the code block
              (goto-char (cl-second code-bounds))
              (forward-line 1)

              ;; After the code block, remove all consecutive lines starting with a | character
              (while (looking-at "^|")
                (delete-region (point) (progn (forward-line 1) (point))))

              (when (not (s-blank? processed))
                (insert processed)
                (forward-line -1)
                (markdown-table-forward-cell)))
            ))

        ))))

(defun my-sql-comint-postgres (product options)
  "Create comint buffer and connect to Postgres.

Unlike `sql-comint-postgres' this also sets the postgres password (through an
environment variable)."
  (let ((process-environment (cons (concat "PGPASSWORD=" sql-password)
                                   process-environment)))
    (sql-comint-postgres product options)))
(sql-set-product-feature 'postgres :sqli-comint-func 'my-sql-comint-postgres)

(defun configure-sql-connections ()
  "Call this after adding your connections to `sql-connection-alist'."
  (--each sql-connection-alist
    (add-to-list 'purpose-user-name-purposes
                 (cons (concat "*SQL: " (symbol-name (car it)) "*") 'repl))
    (add-to-list 'markdown-code-lang-modes
                 (cons (concat "sql-" (symbol-name (car it))) 'sql-mode))
    )
  (purpose-compile-user-configuration)
  )
