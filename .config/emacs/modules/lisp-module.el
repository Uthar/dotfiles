;; -*- lexical-binding: t -*-

;; Enable other brackets as part of Lisp syntax. This makes
;; 'evil-jump-item' (%) and 'show-paren-mode' work with them.
(with-eval-after-load 'lisp-mode
  (modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" lisp-mode-syntax-table)
  (modify-syntax-entry ?\} "){" lisp-mode-syntax-table))

;; Adds sexp flashing, a'la SLIME's C-c C-c, to other sexp evaluation commands
(add-to-list 'load-path (concat +vendor-dir+ "eval-sexp-fu"))
(autoload 'turn-on-eval-sexp-fu-flash-mode "eval-sexp-fu")
(add-hook 'lisp-mode-hook 'turn-on-eval-sexp-fu-flash-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eval-sexp-fu-flash-mode)
(setq eval-sexp-fu-flash-duration 0.2)
(setq eval-sexp-fu-flash-face 'secondary-selection)


