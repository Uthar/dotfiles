;; -*- lexical-binding: t -*-

;; Enable other brackets as part of Lisp syntax. This makes
;; 'evil-jump-item' (%) and 'show-paren-mode' work with them.
(with-eval-after-load 'lisp-mode
  (modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" lisp-mode-syntax-table)
  (modify-syntax-entry ?\} "){" lisp-mode-syntax-table))
