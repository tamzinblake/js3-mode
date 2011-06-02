;;; js.el --- Major mode for editing JavaScript

;; Copyright (C) 2008-2011 Free Software Foundation, Inc.

;; Author: Karl Landstrom <karl.landstrom@brgeight.se>
;;         Daniel Colascione <dan.colascione@gmail.com>
;; Maintainer: Daniel Colascione <dan.colascione@gmail.com>
;; Version: 9
;; Date: 2009-07-25
;; Keywords: languages, javascript

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary

;; This is based on Karl Landstrom's barebones javascript-mode. This
;; is much more robust and works with cc-mode's comment filling
;; (mostly).
;;
;; The main features of this JavaScript mode are syntactic
;; highlighting (enabled with `font-lock-mode' or
;; `global-font-lock-mode'), automatic indentation and filling of
;; comments, C preprocessor fontification, and MozRepl integration.
;;
;; General Remarks:
;;
;; XXX: This mode assumes that block comments are not nested inside block
;; XXX: comments
;;
;; Exported names start with "js-"; private names start with
;; "js--".

;;; Code:


(require 'cc-mode)
(require 'newcomment)
(require 'thingatpt)                    ; forward-symbol etc
(require 'imenu)
(require 'moz nil t)
(require 'json nil t)

(eval-when-compile
  (require 'cl)
  (require 'comint)
  (require 'ido))

(defvar inferior-moz-buffer)
(defvar moz-repl-name)
(defvar ido-cur-list)
(declare-function ido-mode "ido")
(declare-function inferior-moz-process "ext:mozrepl" ())

;;; User Customization

(defgroup js nil
  "Customization variables for JavaScript mode."
  :tag "JavaScript"
  :group 'languages)

(defcustom js-indent-level 2
  "Number of spaces for each indentation step in `js-mode'."
  :type 'integer
  :group 'js)

(defcustom js-expr-indent-offset 0
  "Number of additional spaces for indenting continued expressions.
The value must be no less than minus `js-indent-level'."
  :type 'integer
  :group 'js)

(defcustom js-paren-indent-offset 0
  "Number of additional spaces for indenting expressions in parentheses.
The value must be no less than minus `js-indent-level'."
  :type 'integer
  :group 'js
  :version "24.1")

(defcustom js-square-indent-offset 0
  "Number of additional spaces for indenting expressions in square braces.
The value must be no less than minus `js-indent-level'."
  :type 'integer
  :group 'js
  :version "24.1")

(defcustom js-curly-indent-offset 0
  "Number of additional spaces for indenting expressions in curly braces.
The value must be no less than minus `js-indent-level'."
  :type 'integer
  :group 'js
  :version "24.1")

(defcustom js-auto-indent-flag t
  "Whether to automatically indent when typing punctuation characters.
If non-nil, the characters {}();,: also indent the current line
in Javascript mode."
  :type 'boolean
  :group 'js)

;;; KeyMap

(defvar js-mode-map
  (let ((keymap (make-sparse-keymap)))
    (mapc (lambda (key)
	    (define-key keymap key #'js-insert-and-indent))
	  '("{" "}" "(" ")" ":" ";" ","))
    (define-key keymap [(control ?c) (meta ?:)] #'js-eval)
    (define-key keymap [(control ?c) (control ?j)] #'js-set-js-context)
    (define-key keymap [(control meta ?x)] #'js-eval-defun)
    (define-key keymap [(meta ?.)] #'js-find-symbol)
    (easy-menu-define nil keymap "Javascript Menu"
      '("Javascript"
        ["Select New Mozilla Context..." js-set-js-context
         (fboundp #'inferior-moz-process)]
        ["Evaluate Expression in Mozilla Context..." js-eval
         (fboundp #'inferior-moz-process)]
        ["Send Current Function to Mozilla..." js-eval-defun
         (fboundp #'inferior-moz-process)]))
    keymap)
  "Keymap for `js-mode'.")

(defun js-insert-and-indent (key)
  "Run the command bound to KEY, and indent if necessary.
Indentation does not take place if point is in a string or
comment."
  (interactive (list (this-command-keys)))
  (call-interactively (lookup-key (current-global-map) key))
  (let ((syntax (save-restriction (widen) (syntax-ppss))))
    (when (or (and (not (nth 8 syntax))
                   js-auto-indent-flag)
              (and (nth 4 syntax)
                   (eq (current-column)
                       (1+ (current-indentation)))))
      (indent-according-to-mode))))


;;; Syntax table and parsing

(defvar js-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?$ "_" table)
    table)
  "Syntax table for `js-mode'.")

;;; Indentation

(defconst js--possibly-braceless-keyword-re
  (js--regexp-opt-symbol
   '("catch" "do" "else" "finally" "for" "if" "try" "while" "with"
     "each"))
  "Regexp matching keywords optionally followed by an opening brace.")

(defconst js--indent-operator-re
  (concat "[-+*/%<>=&^|?:.]\\([^-+*/]\\|$\\)\\|"
          (js--regexp-opt-symbol '("in" "instanceof")))
  "Regexp matching operators that affect indentation of continued expressions.")


(defun js--looking-at-operator-p ()
  "Return non-nil if point is on a JavaScript operator, other than a comma."
  (save-match-data
    (and (looking-at js--indent-operator-re)
         (or (not (looking-at ":"))
             (save-excursion
               (and (js--re-search-backward "[?:{]\\|\\_<case\\_>" nil t)
                    (looking-at "?")))))))


(defun js--continued-expression-p ()
  "Return non-nil if the current line continues an expression."
  (save-excursion
    (back-to-indentation)
    (or (js--looking-at-operator-p)
        (and (js--re-search-backward "\n" nil t)
	     (progn
	       (skip-chars-backward " \t")
	       (or (bobp) (backward-char))
	       (and (> (point) (point-min))
                    (save-excursion (backward-char) (not (looking-at "[/*]/")))
                    (js--looking-at-operator-p)
		    (and (progn (backward-char)
				(not (looking-at "++\\|--\\|/[/*]"))))))))))


(defun js--end-of-do-while-loop-p ()
  "Return non-nil if point is on the \"while\" of a do-while statement.
Otherwise, return nil.  A braceless do-while statement spanning
several lines requires that the start of the loop is indented to
the same column as the current line."
  (interactive)
  (save-excursion
    (save-match-data
      (when (looking-at "\\s-*\\_<while\\_>")
	(if (save-excursion
	      (skip-chars-backward "[ \t\n]*}")
	      (looking-at "[ \t\n]*}"))
	    (save-excursion
	      (backward-list) (forward-symbol -1) (looking-at "\\_<do\\_>"))
	  (js--re-search-backward "\\_<do\\_>" (point-at-bol) t)
	  (or (looking-at "\\_<do\\_>")
	      (let ((saved-indent (current-indentation)))
		(while (and (js--re-search-backward "^\\s-*\\_<" nil t)
			    (/= (current-indentation) saved-indent)))
		(and (looking-at "\\s-*\\_<do\\_>")
		     (not (js--re-search-forward
			   "\\_<while\\_>" (point-at-eol) t))
		     (= (current-indentation) saved-indent)))))))))


(defun js--ctrl-statement-indentation ()
  "Helper function for `js--proper-indentation'.
Return the proper indentation of the current line if it starts
the body of a control statement without braces; otherwise, return
nil."
  (save-excursion
    (back-to-indentation)
    (when (save-excursion
            (and (not (eq (point-at-bol) (point-min)))
                 (not (looking-at "[{]"))
                 (progn
                   (js--re-search-backward "[[:graph:]]" nil t)
                   (or (eobp) (forward-char))
                   (when (= (char-before) ?\)) (backward-list))
                   (skip-syntax-backward " ")
                   (skip-syntax-backward "w_")
                   (looking-at js--possibly-braceless-keyword-re))
                 (not (js--end-of-do-while-loop-p))))
      (save-excursion
        (goto-char (match-beginning 0))
        (+ (current-indentation) js-indent-level)))))

(defun js--get-c-offset (symbol anchor)
  (let ((c-offsets-alist
         (list (cons 'c js-comment-lineup-func))))
    (c-get-syntactic-indentation (list (cons symbol anchor)))))

(defun js--proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (cond

     ((looking-at ",")
      (let ((spos
	     (save-excursion
	       (while (looking-back "\\(}\\|]\\|\"\\|)\\|'\\)[\t\n ]*")
		 (backward-sexp))

	       (cond
		((looking-back "\\(,\\|(\\|\\[\\|{\\).*[ \t\n]*")
		 (re-search-backward "\\(,\\|(\\|\\[\\|{\\).*[ \t\n]*" (point-min) t)
		 (current-column))

		((looking-back "\\<var\\>.*[ \t\n]*")
		 (re-search-backward "\\<var\\>.*[ \t\n]*" (point-min) t)
		 (+ (current-column) 2))

		((looking-back "\\<return\\>.*[ \t\n]*")
		 (re-search-backward "\\<return\\>.*[ \t\n]*" (point-min) t)
		 (+ (current-column) 5))
		(t
		 nil)))))
	(if spos
	    spos
	  (+ js-indent-level js-expr-indent-offset))))

     ((looking-at "\\(+\\|/[^/]\\|*\\|-\\)")
      (let ((spos
	     (save-excursion
	       (while (looking-back "\\(}\\|]\\|\"\\|)\\|'\\)[\t\n ]*")
		 (backward-sexp))

	       (cond
		((looking-back "\\(=\\|+\\|/[^/]\\|*\\|-\\|(\\).*[ \t\n]*")
		 (re-search-backward "\\(=\\|+\\|/[^/]\\|*\\|-\\|(\\).*[ \t\n]*" (point-min) t)
		 (current-column))

		(t
		 nil)))))
	(if spos
	    spos
	  (+ js-indent-level js-expr-indent-offset))))

     ((nth 4 parse-status)
      (js--get-c-offset 'c (nth 8 parse-status)))
     ((nth 8 parse-status) 0) ; inside string
     ((js--ctrl-statement-indentation))
     ((eq (char-after) ?#) 0)
     ((save-excursion (js--beginning-of-macro)) 4)
     ((nth 1 parse-status)
      ;; A single closing paren/bracket should be indented at the
      ;; same level as the opening statement. Same goes for
      ;; "case" and "default".
      (let ((same-indent-p (looking-at
			    "[]})]\\|\\_<case\\_>\\|\\_<default\\_>"))
	    (continued-expr-p (js--continued-expression-p)))
	(goto-char (nth 1 parse-status)) ; go to the opening char
	(if (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
	    (progn ; nothing following the opening paren/bracket
	      (skip-syntax-backward " ")
	      (when (eq (char-before) ?\)) (backward-list))
	      (back-to-indentation)
	      (cond (same-indent-p
		     (current-column))
		    (continued-expr-p
		     (+ (current-column) (* 2 js-indent-level)
			js-expr-indent-offset))
		    (t
		     (+ (current-column) js-indent-level
			(case (char-after (nth 1 parse-status))
			      (?\( js-paren-indent-offset)
			      (?\[ js-square-indent-offset)
			      (?\{ js-curly-indent-offset))))))
	  ;; If there is something following the opening
	  ;; paren/bracket, everything else should be indented at
	  ;; the same level.
	  (unless same-indent-p
	    (forward-char)
	    (skip-chars-forward " \t"))
	  (current-column))))

     ((and (js2-node-at-point)
	   (js2-node-parent (js2-node-at-point))
	   (js2-node-type (js2-node-parent (js2-node-at-point)))
	   (= js2-VAR (js2-node-type (js2-node-parent (js2-node-at-point)))))
      (save-excursion
	(re-search-backward "\\<var\\>" (point-min) t)
	(+ (current-column) 4)))

     ((js--continued-expression-p)
      (+ js-indent-level js-expr-indent-offset))
     (t 0))))

(defun js-indent-line ()
  "Indent the current line as JavaScript."
  (interactive)
  (save-restriction
    (widen)
    (let* ((parse-status
            (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation))))
      (indent-line-to (js--proper-indentation parse-status))
      (when (> offset 0) (forward-char offset)))))

;;; Main Function

;;;###autoload
(define-derived-mode js-mode prog-mode "Javascript"
  "Major mode for editing JavaScript."
  :group 'js

  (set (make-local-variable 'indent-line-function) 'js-indent-line)
  (set (make-local-variable 'beginning-of-defun-function)
       'js-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       'js-end-of-defun)

  (set (make-local-variable 'open-paren-in-column-0-is-defun-start) nil)
  (set (make-local-variable 'font-lock-defaults)
       (list js--font-lock-keywords))
  (set (make-local-variable 'syntax-propertize-function)
       js-syntax-propertize-function)

  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'which-func-imenu-joiner-function)
       #'js--which-func-joiner)

  ;; Comments
  (setq comment-start "// ")
  (setq comment-end "")
  (set (make-local-variable 'fill-paragraph-function)
       'js-c-fill-paragraph)

  ;; Parse cache
  (add-hook 'before-change-functions #'js--flush-caches t t)

  ;; Frameworks
  (js--update-quick-match-re)

  ;; Imenu
  (setq imenu-case-fold-search nil)
  (set (make-local-variable 'imenu-create-index-function)
       #'js--imenu-create-index)

  ;; for filling, pretend we're cc-mode
  (setq c-comment-prefix-regexp "//+\\|\\**"
        c-paragraph-start "$"
        c-paragraph-separate "$"
        c-block-comment-prefix "* "
        c-line-comment-starter "//"
        c-comment-start-regexp "/[*/]\\|\\s!"
        comment-start-skip "\\(//+\\|/\\*+\\)\\s *")

  (let ((c-buffer-is-cc-mode t))
    ;; FIXME: These are normally set by `c-basic-common-init'.  Should
    ;; we call it instead?  (Bug#6071)
    (make-local-variable 'paragraph-start)
    (make-local-variable 'paragraph-separate)
    (make-local-variable 'paragraph-ignore-fill-prefix)
    (make-local-variable 'adaptive-fill-mode)
    (make-local-variable 'adaptive-fill-regexp)
    (c-setup-paragraph-variables))

  (set (make-local-variable 'syntax-begin-function)
       #'js--syntax-begin-function)

  ;; Important to fontify the whole buffer syntactically! If we don't,
  ;; then we might have regular expression literals that aren't marked
  ;; as strings, which will screw up parse-partial-sexp, scan-lists,
  ;; etc. and produce maddening "unbalanced parenthesis" errors.
  ;; When we attempt to find the error and scroll to the portion of
  ;; the buffer containing the problem, JIT-lock will apply the
  ;; correct syntax to the regular expresion literal and the problem
  ;; will mysteriously disappear.
  ;; FIXME: We should actually do this fontification lazily by adding
  ;; calls to syntax-propertize wherever it's really needed.
  (syntax-propertize (point-max)))

;;;###autoload
(defalias 'javascript-mode 'js-mode)

(eval-after-load 'folding
  '(when (fboundp 'folding-add-to-marks-list)
     (folding-add-to-marks-list 'js-mode "// {{{" "// }}}" )))

(provide 'js)

;; js.el ends here
