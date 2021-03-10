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
  (let ((block (--gfm-get-sql-block current-point)))
    (when block
      (let* ((without-prompts (--gfm-run-sql (gfm-sql-block-connection block)
                                             (gfm-sql-block-query block)))
             (cleaned-lines (--remove (or (s-blank? it)
                                          (s-matches? "^([0-9]+ rows?)$" it))
                                      (s-lines without-prompts)))
             (processed (apply 's-concat
                               (--map (concat "|" (s-truncate 1000 it "| ...") "\n")
                                      cleaned-lines
                                      ))))

        (save-excursion
          ;; Move after the code block
          (goto-char (cl-second (gfm-sql-block-bounds block)))
          (forward-line 1)

          ;; After the code block, remove all consecutive lines starting with a | character
          (while (looking-at "^|")
            (delete-region (point) (progn (forward-line 1) (point))))

          (when (not (s-blank? processed))
            (insert processed)
            (forward-line -1)
            (markdown-table-forward-cell)))
        ))))

(defun markdown-sql-explanalyze (current-point)
  "Explain/analyze the sql expression at point."
  (interactive "d")  ;; Get point
  (let ((block (--gfm-get-sql-block current-point)))
    (when block
      (let* ((explain-query (s-concat "EXPLAIN (ANALYZE, COSTS, VERBOSE, BUFFERS, FORMAT JSON) "
                                      (gfm-sql-block-query block)))
             (connection (gfm-sql-block-connection block))
             (o1 (--gfm-run-sql connection "\\pset format unaligned"))
             (result (--gfm-run-sql connection explain-query))
             (o2 (--gfm-run-sql connection "\\pset format aligned"))
             (url (s-concat "file:///home/yorick/pev2/index.html"
                            "?plan="
                            (url-hexify-string result)
                            "&query="
                            (url-hexify-string explain-query)))
             )
        (browse-url url)))))

(cl-defstruct gfm-sql-block bounds query connection)

(defun --gfm-get-sql-block (current-point)
  ;; markdown-get-enclosing-fenced-block-construct also extracts the current
  ;; code block, but includes the start and end tags.
  (let ((code-bounds (get-text-property current-point 'markdown-gfm-code)))
    (when code-bounds
      (let ((lang (save-excursion (markdown-code-block-lang))))
        (when (s-starts-with? "sql-" lang)
          (make-gfm-sql-block
           :bounds code-bounds
           :query (buffer-substring-no-properties (cl-first code-bounds) (cl-second code-bounds))
           :connection (s-chop-prefix "sql-" lang)
           ))))))


(defun --gfm-run-sql (connection query)
  """
  Run the query in the sql code block at point and return the result string.
  """
  (sql-start-session connection t)
  (sql-redirect-one sql-buffer query "*SQL-GFM-TEMP*" nil)

  (let* ((raw (with-current-buffer "*SQL-GFM-TEMP*"
                (buffer-substring-no-properties (point-min) (point-max))))
         (prompts (list "^\\(\\w*[-(][#>] \\)*"  ;; Remove multiple continuation prompts
                        "^\\w*=[#>] "))
         (without-prompts (--reduce-from (s-replace-regexp it "" acc) raw prompts))
         )
    without-prompts))


(defun my-sql-comint-postgres (product options &optional buf-name)
  "Create comint buffer and connect to Postgres.

Unlike `sql-comint-postgres' this also sets the postgres password (through an
environment variable)."
  (let ((process-environment (cons (concat "PGPASSWORD=" sql-password)
                                   process-environment)))
    (sql-comint-postgres product options buf-name)))
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
