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
 (lambda (&rest args)
   (cl-destructuring-bind (&optional lisp-syntax keywords-case-insensitive elisp) args
     (list lisp-syntax nil elisp)))
 '((name . kaspi/keywords-case-insensitive)))

;; CL-style indentation
(put 'if 'lisp-indent-function 4)
(put 'if-let 'lisp-indent-function 4)

;; Work like SLIME - can always C-u C-e
(define-key emacs-lisp-mode-map (kbd "C-j") 'newline-and-indent)
(define-key lisp-interaction-mode-map (kbd "C-j") 'newline-and-indent)

;;;; Lisp indentation

;;;; lisp-mode's lisp-indent-offset is too powerful - it prevents special
;;;; keywords from being indented specially. I want to indent arguments to
;;;; normal function calls on next lines with a smaller indent because to me
;;;; that looks better. I also want to increase the default indent for arguments
;;;; to function calls where all of the arguments are on the next line (the
;;;; default indentation of 1 space is too small). But I still want some
;;;; builtins to use the predefined indentation. One solution is to wrap any
;;;; lisp-indent-function to detect those special keywords and delegate the
;;;; indentation to the real function, but override anything else with a hard
;;;; coded indentation.

(defvar +kaspi/lisp-indent-offset+ 2
  "How many spaces to indent non special funcalls by.")

(defvar +kaspi/lisp-specially-indented-symbols+
  '(lambda defun or and case if list vector destructuring-bind loop
    + - * /
    hash-map -> ->>)
  "List of functions to delegate the indentation of to some elisp library.")

(defun kaspi/lisp-indent-specially (f point state)
  "Indent lisp form either by a fixed offset or by delegating to f"
  (cl-destructuring-bind (depth containing-list &rest) state
    (let ((sym (when (integerp containing-list)
                 (save-excursion
                   (goto-char containing-list)
                   (forward-char)
                   (symbol-at-point)))))
      (if (memq sym +kaspi/lisp-specially-indented-symbols+)
          (funcall f point state)
          (+ (if (not (integerp containing-list))
                 0
                 (save-excursion
                   (goto-char containing-list)
                   (current-column)))
             +kaspi/lisp-indent-offset+)))))

(defun kaspi/common-lisp-indent-function (point state)
  (kaspi/lisp-indent-specially #'common-lisp-indent-function point state))

(defun kaspi/clojure-indent-function (point state)
  (kaspi/lisp-indent-specially #'clojure-indent-function point state))

;; Default - primarily for emacs-lisp-mode
(setq lisp-indent-function 'kaspi/common-lisp-indent-function)
