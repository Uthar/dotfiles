;; -*- lexical-binding: t -*-

;; Enable other brackets as part of Lisp syntax. This makes
;; 'evil-jump-item' (%) and 'show-paren-mode' work with them.
(with-eval-after-load 'lisp-mode
  (modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" lisp-mode-syntax-table)
  (modify-syntax-entry ?\} "){" lisp-mode-syntax-table))

;; Indent [...] {...} for reader macros
(defun kaspi/indent-braces-function (function &rest args)
  (cl-destructuring-bind (point column char)
      (save-excursion
        (backward-up-list)
        (list (point)
              (current-column)
              (char-after)))
    (if (or (eql char ?\[)
            (eql char ?\{))
        (1+ column)
        (apply function args))))

(with-eval-after-load 'cl-indent
  (advice-add 'common-lisp-indent-function :around 'kaspi/indent-braces-function))

;;;; Adds sexp flashing, a'la SLIME's C-c C-c, to other sexp evaluation commands

(defun kaspi/flash-defun (&rest _)
  (let ((beginning
         (save-excursion
           (end-of-defun)
           (beginning-of-defun)
           (point))))
    (pulse-momentary-highlight-region
     beginning
     (save-excursion
       (goto-char beginning)
       (forward-sexp)
       (point)))))

(defun kaspi/flash-last-sexp (&rest _)
  (pulse-momentary-highlight-region
   (save-excursion
     (backward-sexp)
     (point))
   (point)))

(advice-add 'eval-last-sexp :after 'kaspi/flash-last-sexp)
(advice-add 'eval-defun :after 'kaspi/flash-defun)

;; Fixes e.g. make-todo or add-note from being highlighted (todo-module.el)
(advice-add 'lisp-mode-variables :filter-args 
  (cl-constantly nil)
  '((name . kaspi/keywords-case-insensitive)))

;; CL-style indentation
(put 'if 'lisp-indent-function 4)
(put 'if-let 'lisp-indent-function 4)
