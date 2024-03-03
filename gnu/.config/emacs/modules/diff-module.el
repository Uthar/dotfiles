;; -*- lexical-binding: t -*-

;; Configure syntax highlighting in diff buffers
(setq diff-font-lock-syntax t)

;; Prevent messing with the diff by removing save hooks.
(add-hook 'diff-mode-hook
  (lambda ()
    (setq-local require-final-newline nil)
    (setq-local before-save-hook nil)))

;; Ediff control window in same frame as A/B windows
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Better for wide screens which I use
(setq ediff-split-window-function 'split-window-horizontally)
