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

;; Compact indentation for function calls with long names where the first
;; argument is a "dispatch object" and so it makes sense for it to stay on the
;; same line as the function name, but following arguments would waste too much
;; screen space when indented normally.
(defun kaspi/lisp-indent-sexp-2 ()
  (interactive)
  ;; Prosty przypadek.
  ;;
  ;; Zamiast tego:
  ;; (sqlite:execute *sqlite*
  ;;                 "insert into foo values (?,?)" 1 2)
  ;;
  ;; ...To:
  ;; (sqlite:execute *sqlite*
  ;;   "insert into foo values (?,?)" 1 2)
  (let ((lisp-indent-offset 2))
    (indent-sexp))
  ;; Przypadek, kiedy tylko pierwsze wcięcie powinno zostać tak ułożone.
  ;;
  ;; Zamiast tego:
  ;; (elasticsearch:search *es*
  ;;   {:query
  ;;     {:bool
  ;;       {:filter
  ;;         [{:range {"created_at" {:gte "2024-05-13"
  ;;                                  :lte "2024-05-13"}}}
  ;;           {:term {"user_name" screenname}}]}}
  ;;     :size 100})
  ;;
  ;; ...To:
  ;; (elasticsearch:search *es*
  ;;   {:query
  ;;    {:bool
  ;;     {:filter
  ;;      [{:range {"created_at" {:gte "2024-05-13"
  ;;                              :lte "2024-05-13"}}}
  ;;       {:term {"user_name" screenname}}]}}
  ;;    :size 100})
  (when current-prefix-arg
    (save-excursion
      (forward-line)
      (mark-sexp)
      (forward-line)
      (indent-region (region-beginning) (region-end)))))

(global-set-key (kbd "C-c l q") 'kaspi/lisp-indent-sexp-2)

(defun kaspi/unprogn-sexp ()
  "Przekształca wskazane sexpr usuwając obejmujące je wyrażenie PROGNopodobne."
  (interactive)
  (let ((start (point)))
    (forward-sexp)
    (delete-char -1)
    (goto-char start)
    (forward-char)
    (kill-sexp)
    (delete-char -1)))

(global-set-key (kbd "C-c l u") 'kaspi/unprogn-sexp)
