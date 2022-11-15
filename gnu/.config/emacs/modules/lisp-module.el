;; -*- lexical-binding: t -*-

;; Enable other brackets as part of Lisp syntax. This makes
;; 'evil-jump-item' (%) and 'show-paren-mode' work with them.
(with-eval-after-load 'lisp-mode
  (modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" lisp-mode-syntax-table)
  (modify-syntax-entry ?\} "){" lisp-mode-syntax-table))

;;;; Adds sexp flashing, a'la SLIME's C-c C-c, to other sexp evaluation commands

(defun kaspi/flash-defun (&rest _)
  (let ((beginning
         (save-excursion
           (unless (eql (char-after (line-beginning-position)) ?\()
             (beginning-of-defun))
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

;; CL-style indentation
(put 'if 'lisp-indent-function 4)
(put 'if-let 'lisp-indent-function 4)
