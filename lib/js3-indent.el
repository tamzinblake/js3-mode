;;; js3-indent.el --- indentation for js3-mode

;;; Commentary:

;; This code is taken as much as possible from the current version of
;; js.el included with emacs 24, with modifications.

;; Unlike js2-mode, this does not support bounce-indent.

;;; Code:

(defconst js3-possibly-braceless-keyword-re
  (regexp-opt
   '("catch" "do" "else" "finally" "for" "if" "try" "while" "with" "let" "each")
   'words)
  "Regular expression matching keywords that are optionally
followed by an opening brace.")

(defconst js3-indent-operator-re
  (concat "[-+*/%<>=&^|?:.]\\([^-+*/]\\|$\\)\\|"
          (regexp-opt '("in" "instanceof") 'words))
  "Regular expression matching operators that affect indentation
of continued expressions.")

(defconst js3-indent-lazy-operator-re
  (concat "[-+*/%<>=&^|?:]\\([^-+*/]\\|$\\)\\|"
          (regexp-opt '("in" "instanceof") 'words))
  "Regular expression matching operators that affect indentation
of continued expressions in lazy-operator-first style.")

(defconst js3-indent-operator-first-re
  (concat "[-+*/%<>!=&^|?:.]\\([^-+*/]\\|$\\)\\|"
          (regexp-opt '("in" "instanceof") 'words))
  "Regular expression matching operators that affect indentation
of continued expressions with operator-first style.")

(defconst js3-indent-brace-re
  "[[({]"
  "Regexp matching opening braces that affect indentation.")

(defconst js3-indent-operator-brace-re
  "[[(]"
  "Regexp matching opening braces that affect operator indentation.")

(defconst js3-skip-newlines-re
  "[ \t\n]*"
  "Regexp matching any amount of trailing whitespace and newlines.")

(defconst js3-opt-cpp-start "^\\s-*#\\s-*\\([[:alnum:]]+\\)"
  "Regexp matching the prefix of a cpp directive.
This includes the directive name, or nil in languages without
preprocessor support.  The first submatch surrounds the directive
name.")

(defun js3-backward-sws ()
  "Move backward through whitespace and comments."
  (interactive)
  (while (forward-comment -1)))

(defun js3-forward-sws ()
  "Move forward through whitespace and comments."
  (interactive)
  (while (forward-comment 1)))

(defun js3-beginning-of-macro (&optional lim)
  (let ((here (point)))
    (save-restriction
      (if lim (narrow-to-region lim (point-max)))
      (beginning-of-line)
      (while (eq (char-before (1- (point))) ?\\)
        (forward-line -1))
      (back-to-indentation)
      (if (and (<= (point) here)
               (looking-at js3-opt-cpp-start))
          t
        (goto-char here)
        nil))))

;; This function has horrible results if you're typing an array
;; such as [[1, 2], [3, 4], [5, 6]].  Bounce indenting -really- sucks
;; in conjunction with electric-indent, so just disabling it.
(defsubst js3-code-at-bol-p ()
  "Return t if the first character on line is non-whitespace."
  nil)

(defun js3-insert-and-indent (key)
  "Run command bound to key and indent current line. Runs the command
bound to KEY in the global keymap and indents the current line."
  (interactive (list (this-command-keys)))
  (let ((cmd (lookup-key (current-global-map) key)))
    (if (commandp cmd)
        (call-interactively cmd)))
  ;; don't do the electric keys inside comments or strings,
  ;; and don't do bounce-indent with them.
  (let ((parse-state (parse-partial-sexp (point-min) (point)))
        (js3-bounce-indent-p (js3-code-at-bol-p)))
    (unless (or (nth 3 parse-state)
                (nth 4 parse-state))
      (indent-according-to-mode))))


(defun js3-re-search-forward-inner (regexp &optional bound count)
  "Helper function for `js3-re-search-forward'."
  (let ((parse)
        str-terminator
        (orig-macro-end (save-excursion
                          (when (js3-beginning-of-macro)
                            (c-end-of-macro)
                            (point)))))
    (while (> count 0)
      (re-search-forward regexp bound)
      (setq parse (syntax-ppss))
      (cond ((setq str-terminator (nth 3 parse))
             (when (eq str-terminator t)
               (setq str-terminator ?/))
             (re-search-forward
              (concat "\\([^\\]\\|^\\)" (string str-terminator))
              (point-at-eol) t))
            ((nth 7 parse)
             (forward-line))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?\/) (eq (char-after) ?\*)))
             (re-search-forward "\\*/"))
            ((and (not (and orig-macro-end
                            (<= (point) orig-macro-end)))
                  (js3-beginning-of-macro))
             (c-end-of-macro))
            (t
             (setq count (1- count))))))
  (point))


(defun js3-re-search-forward (regexp &optional bound noerror count)
  "Search forward, ignoring strings, cpp macros, and comments.
This function invokes `re-search-forward', but treats the buffer
as if strings, cpp macros, and comments have been removed.

If invoked while inside a macro, it treats the contents of the
macro as normal text."
  (unless count (setq count 1))
  (let ((saved-point (point))
        (search-fun
         (cond ((< count 0) (setq count (- count))
                #'js3-re-search-backward-inner)
               ((> count 0) #'js3-re-search-forward-inner)
               (t #'ignore))))
    (condition-case err
        (funcall search-fun regexp bound count)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (signal (car err) (cdr err)))))))


(defun js3-re-search-backward-inner (regexp &optional bound count)
  "Auxiliary function for `js3-re-search-backward'."
  (let ((parse)
        str-terminator
        (orig-macro-start
         (save-excursion
           (and (js3-beginning-of-macro)
                (point)))))
    (while (> count 0)
      (re-search-backward regexp bound)
      (when (and (> (point) (point-min))
                 (save-excursion (backward-char) (looking-at "/[/*]")))
        (forward-char))
      (setq parse (syntax-ppss))
      (cond ((setq str-terminator (nth 3 parse))
             (when (eq str-terminator t)
               (setq str-terminator ?/))
             (re-search-backward
              (concat "\\([^\\]\\|^\\)" (string str-terminator))
              (point-at-bol) t)
	     (when (not (string= "" (match-string 1)))
	       (forward-char)))
            ((nth 7 parse)
             (goto-char (nth 8 parse)))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?/) (eq (char-after) ?*)))
             (re-search-backward "/\\*"))
            ((and (not (and orig-macro-start
                            (>= (point) orig-macro-start)))
                  (js3-beginning-of-macro)))
            (t
             (setq count (1- count))))))
  (point))


(defun js3-re-search-backward (regexp &optional bound noerror count)
  "Search backward, ignoring strings, preprocessor macros, and comments.

This function invokes `re-search-backward' but treats the buffer
as if strings, preprocessor macros, and comments have been
removed.

If invoked while inside a macro, treat the macro as normal text."
  (js3-re-search-forward regexp bound noerror (if count (- count) -1)))


(defun js3-looking-back (regexp)
  "This function returns t if regexp matches text before point, ending at point, and nil otherwise.

This function is similar to `looking-back' but ignores comments and strings"
  (save-excursion
    (let ((r (if (and (= ?\= (elt regexp (1- (length regexp))))
		      (= ?\\ (elt regexp (- (length regexp) 2))))
		 regexp
	       (concat regexp "\\="))))
      (numberp (js3-re-search-backward r (point-min) t)))))

(defun js3-looking-at (regexp)
  "This function returns t if regexp matches text after point, beginning at point, and nil otherwise.

This function is similar to `looking-at' but ignores comments and strings"
  (save-excursion
    (let ((r (if (and (= ?\= (elt regexp 1))
		      (= ?\\ (elt regexp 0)))
		 regexp
	       (concat "\\=" regexp))))
      (numberp (js3-re-search-forward r nil t)))))

(defun js3-looking-at-operator-p ()
  "Return non-nil if point is on a JavaScript operator, other than a comma."
  (save-match-data
    (and (looking-at js3-indent-operator-re)
         (or (not (= (following-char) ?\:))
             (save-excursion
               (and (js3-re-search-backward "[?:{]\\|\\_<case\\_>" nil t)
                    (= (following-char) ?\?)))))))


(defun js3-continued-expression-p ()
  "Return non-nil if the current line continues an expression."
  (save-excursion
    (back-to-indentation)
    (or (js3-looking-at-operator-p)
        (and (js3-re-search-backward "\n" nil t)
             (progn
               (skip-chars-backward " \t")
               (or (bobp) (backward-char))
               (and (> (point) (point-min))
                    (save-excursion (backward-char) (not (looking-at "[/*]/")))
                    (js3-looking-at-operator-p)
                    (and (progn (backward-char)
                                (not (looking-at "++\\|--\\|/[/*]"))))))))))


(defun js3-end-of-do-while-loop-p ()
  "Return non-nil if point is on the \"while\" of a do-while statement.
Otherwise, return nil.  A braceless do-while statement spanning
several lines requires that the start of the loop is indented to
the same column as the current line."
  (interactive)
  (save-excursion
    (save-match-data
      (when (looking-at "\\s-*\\_<while\\_>")
        (if (save-excursion
              (skip-chars-backward (concat js3-skip-newlines-re "}"))
              (looking-at (concat js3-skip-newlines-re "}")))
            (save-excursion
              (backward-list) (forward-symbol -1) (looking-at "\\_<do\\_>"))
          (js3-re-search-backward "\\_<do\\_>" (point-at-bol) t)
          (or (looking-at "\\_<do\\_>")
              (let ((saved-indent (current-indentation)))
                (while (and (js3-re-search-backward "^\\s-*\\_<" nil t)
                            (/= (current-indentation) saved-indent)))
                (and (looking-at "\\s-*\\_<do\\_>")
                     (not (js3-re-search-forward
                           "\\_<while\\_>" (point-at-eol) t))
                     (= (current-indentation) saved-indent)))))))))


(defun js3-backward-whitespace ()
  "Helper function for `js3-proper-indentation'.
Skip backwards over whitespace and comments."
  (let ((rv nil))
    (when (js3-looking-back "[ \t\n]")
      (setq rv t)
      (js3-re-search-backward (concat "[^ \t\n]" js3-skip-newlines-re)
			      (point-min) t)
      (forward-char))
    rv))


(defun js3-backward-sexp ()
  "Helper function for `js3-proper-indentation'.
Go backwards over matched braces, rather than whole expressions.
Only skip over strings while looking for braces.
Functionality does not exactly match backward-sexp."
  (let ((brackets 0)
	(rv nil))
    (while (js3-looking-back (concat "[]})]" js3-skip-newlines-re))
      (setq rv t)
      (js3-re-search-backward (concat "[]})]"
				      js3-skip-newlines-re)
			      (point-min) t)
      (cond
       ((= (following-char) ?\])
        (setq brackets (1+ brackets))
        (while (/= brackets 0)
          (js3-re-search-backward "[][]" (point-min) t)
          (cond
           ((= (following-char) ?\])
            (setq brackets (1+ brackets)))
           ((= (following-char) ?\[)
            (setq brackets (1- brackets))))))

       ((= (following-char) ?\})
        (setq brackets (1+ brackets))
        (while (/= brackets 0)
          (js3-re-search-backward "[}{]" (point-min) t)
          (cond
           ((= (following-char) ?\})
            (setq brackets (1+ brackets)))
           ((= (following-char) ?\{)
            (setq brackets (1- brackets))))))

       ((= (following-char) ?\))
        (setq brackets (1+ brackets))
        (while (/= brackets 0)
          (js3-re-search-backward "[)(]" (point-min) t)
          (cond
           ((= (following-char) ?\))
            (setq brackets (1+ brackets)))
           ((= (following-char) ?\()
            (setq brackets (1- brackets))))))))
    rv))


(defun js3-backward-clean ()
  "Helper function for `js3-proper-indentation'.
Calls js3-backward-sexp and js3-backward-whitespace until they are done."
  (let ((rv nil))
    (while (or (js3-backward-whitespace) (js3-backward-sexp))
      (setq rv t))
    rv))


(defun js3-ctrl-statement-indentation ()
  "Helper function for `js3-proper-indentation'.
Return the proper indentation of the current line if it starts
the body of a control statement without braces; otherwise, return
nil."
  (save-excursion
    (back-to-indentation)
    (when (save-excursion
            (and (not (eq (point-at-bol) (point-min)))
                 (not (= (following-char) ?\{))
                 (progn
                   (js3-re-search-backward "[[:graph:]]" nil t)
                   (or (eobp) (forward-char))
                   (when (= (char-before) ?\)) (backward-list))
                   (skip-syntax-backward " ")
                   (skip-syntax-backward "w_")
                   (looking-at js3-possibly-braceless-keyword-re))
                 (not (js3-end-of-do-while-loop-p))))
      (save-excursion
        (goto-char (match-beginning 0))
        (+ (current-indentation) js3-indent-level)))))

(defun js3-get-c-offset (symbol anchor)
  (let ((c-offsets-alist
         (list (cons 'c js3-comment-lineup-func))))
    (c-get-syntactic-indentation (list (cons symbol anchor)))))

(defun js3-back-offset (abs offset)
  "Helper function for `js3-proper-indentation'."
  (goto-char abs)
  (while (= (preceding-char) ?\ )
    (backward-char))
  (backward-char offset)
  (current-column))

(defun js3-back-offset-re (abs re)
  "Helper function for `js3-proper-indentation'."
  (goto-char abs)
  (js3-re-search-forward re nil t)
  (backward-char)
  (current-column))

(defun js3-proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (let ((node (js3-node-at-point)))
      (if (not node)
	  0
	(let ((char (following-char))
	      (abs (js3-node-abs node))
	      (type (js3-node-type node)))
	  (cond

	   ;;inside a multi-line comment
	   ((nth 4 parse-status)
	    (cond
	     ((= (char-after) ?*)
	      (goto-char abs)
	      (1+ (current-column)))
	     (t
	      (goto-char abs)
	      (if (not (looking-at "/\\*\\s-*\\S-"))
		  (current-column)
		(forward-char 2)
		(re-search-forward "\\S-" nil t)
		(1- (current-column))))))

	   ;;inside a string - indent to 0 since you can't do that.
	   ((nth 8 parse-status) 0)

	   ;;comma-first and operator-first
	   ((or
	     (and (not js3-lazy-commas)
		  (= (following-char) ?\,))
	     (and (not js3-lazy-operators)
		  (looking-at js3-indent-operator-first-re)
		  (or (not (= (following-char) ?\.))
		      (not js3-lazy-dots))))
	    (cond
	     ;;bare statements
	     ((= type js3-VAR)
	      (goto-char abs)
	      (+ (current-column) 2))
	     ((= type js3-RETURN)
	      (goto-char abs)
	      (+ (current-column) 5))

	     ;;lists
	     ((= type js3-ARRAYLIT)
	      (js3-back-offset-re abs "[[]"))
	     ((= type js3-OBJECTLIT)
	      (js3-back-offset-re abs "{"))
	     ((= type js3-FUNCTION)
	      (js3-back-offset-re abs "("))
	     ((= type js3-CALL)
	      (js3-back-offset-re abs "("))

	     ;;operators
	     ((and (>= type 9)
		   (<= type 18)) ; binary operators
	      (js3-back-offset abs 1))
	     ((= type js3-COMMA)
	      (message "found comma operator")
	      (js3-back-offset abs 1))
	     ((= type js3-ASSIGN)
	      (js3-back-offset abs 1))
	     ((= type js3-HOOK)
	      (js3-back-offset abs 1))

	     ((= type js3-GETPROP) ; dot operator
	      (goto-char abs)
	      (if (js3-looking-at ".*\\..*")
		  (progn (js3-re-search-forward "\\." nil t)
			 (backward-char)
			 (current-column))
		(+ (current-column)
		   js3-expr-indent-offset js3-indent-level)))

	     ;; multi-char operators
	     ((and (>= type 19)
		   (<= type 24)) ; 2-char binary operators
	      (js3-back-offset abs 2))
	     ((or (= type js3-URSH)
		  (= type js3-SHEQ)
		  (= type js3-SHNE)) ;3-char binary operators
	      (js3-back-offset abs 3))
	     ((and (>= type 103)
		   (<= type 104)) ; logical and/or
	      (js3-back-offset abs 2))

	     ;;multi-char assignment
	     ((and (>= type 90)
		   (<= type 97)) ; assignment 2-char
	      (js3-back-offset abs 2))
	     ((and (>= type 98)
		   (<= type 99)) ; assignment 3-char
	      (js3-back-offset abs 3))
	     ((= type 100)       ; assignment 4-char
	      (js3-back-offset abs 4))

	     (t
	      (goto-char abs)
	      (+ (current-column) js3-indent-level js3-expr-indent-offset))))

	   ;;lazy comma-first
	   ((and js3-lazy-commas
		 (= (following-char) ?\,))
	    (js3-backward-sexp)
	    (cond

	     ((js3-looking-back (concat "^[ \t]*,.*"
					js3-skip-newlines-re))
	      (js3-re-search-backward (concat "^[ \t]*,.*"
					      js3-skip-newlines-re)
				      (point-min) t)
	      (back-to-indentation)
	      (current-column))

	     ((looking-back (concat "^[ \t]*[^ \t\n].*"
				    js3-skip-newlines-re))
	      (re-search-backward (concat "^[ \t]*[^ \t\n].*"
					  js3-skip-newlines-re)
				  (point-min) t)
	      (back-to-indentation)
	      (if (< (current-column) 2)
		  (current-column)
		(- (current-column) 2)))

	     (t
	      (+ js3-indent-level js3-expr-indent-offset))))

	   ;;lazy dot-first
	   ((and js3-lazy-dots
		 (= (following-char) ?\.))
	    (save-excursion
	      (js3-backward-sexp)
	      (if (looking-back (concat "^[ \t]*[^ \t\n].*"
					js3-skip-newlines-re))
		  (progn
		    (re-search-backward (concat "^[ \t]*[^ \t\n].*"
						js3-skip-newlines-re)
					(point-min) t)
		    (back-to-indentation)
		    (+ (current-column) js3-indent-level))
		(+ js3-indent-level js3-expr-indent-offset))))

	   ;;lazy operator-first
	   ((and js3-lazy-operators
		 (looking-at js3-indent-lazy-operator-re))
	    (save-excursion
	      (js3-backward-sexp)
	      (if (looking-back (concat "^[ \t]*[^ \t\n].*"
					js3-skip-newlines-re))
		  (progn
		    (re-search-backward (concat "^[ \t]*[^ \t\n].*"
						js3-skip-newlines-re)
					(point-min) t)
		    (back-to-indentation)
		    (if (or (looking-at js3-indent-lazy-operator-re)
			    (< (current-column) 2))
			(current-column)
		      (- (current-column) 2)))
		(+ js3-indent-level js3-expr-indent-offset))))

	   ;;var special case for non-comma-first continued var statements
	   ((and js3-pretty-vars
		 (looking-at "[^]})]")
		 (not (looking-at "\\<var\\>"))
		 (js3-node-at-point)
		 (js3-node-parent (js3-node-at-point))
		 (js3-node-type (js3-node-parent (js3-node-at-point)))
		 (= js3-VAR
		    (js3-node-type (js3-node-parent (js3-node-at-point)))))
	    (save-excursion
	      (js3-re-search-backward "\\<var\\>" (point-min) t)
	      (+ (current-column) 4)))

	   ;;indent control statement body without braces, if applicable
	   ((js3-ctrl-statement-indentation))

	   ;;c preprocessor - indent to 0
	   ((eq (char-after) ?#) 0)

	   ;;we're in a cpp macro - indent to 4 why not
	   ((save-excursion (js3-beginning-of-macro)) 4)

	   ;;inside a parenthetical grouping
	   ((nth 1 parse-status)
	    ;; A single closing paren/bracket should be indented at the
	    ;; same level as the opening statement.
	    (let ((same-indent-p (looking-at
				  "[]})]"))
		  (continued-expr-p (js3-continued-expression-p)))
	      (goto-char (nth 1 parse-status)) ; go to the opening char
	      (if (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
		  (progn ; nothing following the opening paren/bracket
		    (skip-syntax-backward " ")
		    (when (eq (char-before) ?\)) (backward-list)) ;skip arg list
		    (if (and (not js3-consistent-level-indent-inner-bracket)
			     (js3-looking-back (concat
						"\\(:\\|,\\)"
						js3-skip-newlines-re
						"\\<function\\>"
						js3-skip-newlines-re)))
			(progn
			  (js3-re-search-backward (concat
						   "\\(:\\|,\\)"
						   js3-skip-newlines-re
						   "\\<function\\>"
						   js3-skip-newlines-re))
			  (js3-backward-clean)
			  (if (looking-back "[{[(,][^{[(,\n]*")
			      (progn
				(js3-re-search-backward "[{[(,][^{[(,\n]*")
				(forward-char)
				(js3-re-search-forward "[ \t]*"))
			    (progn
			      (js3-re-search-backward "^")
			      (back-to-indentation)
			      (while (\= (char-after) ?f)
				(forward-char)))))
		      (back-to-indentation))
		    (cond (same-indent-p
			   (current-column))
			  (continued-expr-p
			   (+ (current-column) (* 2 js3-indent-level)
			      js3-expr-indent-offset))
			  (t
			   (+ (current-column) js3-indent-level
			      (case (char-after (nth 1 parse-status))
				    (?\( js3-paren-indent-offset)
				    (?\[ js3-square-indent-offset)
				    (?\{ js3-curly-indent-offset))))))
		;; If there is something following the opening
		;; paren/bracket, everything else should be indented at
		;; the same level.
		(unless same-indent-p
		  (forward-char)
		  (skip-chars-forward " \t"))
		(current-column))))

	   ;;in a continued expression not handled by earlier cases
	   ((js3-continued-expression-p)
	    (+ js3-indent-level js3-expr-indent-offset))

	   ;;if none of these cases, then indent to 0
	   (t 0)))))))

(defun js3-indent-line ()
  "Indent the current line as JavaScript."
  (interactive)
  (when js3-reparse-on-indent (js3-reparse))
  (save-restriction
    (widen)
    (let* ((parse-status
            (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation))))
      (indent-line-to (js3-proper-indentation parse-status))
      (when (> offset 0) (forward-char offset)))))

;;; js3-indent.el ends here
