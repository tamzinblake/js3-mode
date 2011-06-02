(autoload 'js-mode "js")
(defun my-js2-indent-function ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (js--proper-indentation parse-status))
           node)

      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset)))))

(defun my-js2-mode-hook ()
  (require 'js)
  (setq js-indent-level 2
        indent-tabs-mode nil
        c-basic-offset 2
	show-paren-mode t)
  (c-toggle-auto-state 0)
  (c-toggle-hungry-state 1)
  (set (make-local-variable 'indent-line-function) 'my-js2-indent-function)
  (if (featurep 'js2-highlight-vars)
    (js2-highlight-vars-mode))
  (message "My JS2 hook"))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
