;; -*- lexical-binding: t -*-

(setq diff-font-lock-syntax nil)

;; Prevent messing with the diff by removing save hooks.
(add-hook 'diff-mode-hook
  (lambda ()
    (setq-local require-final-newline nil)
    (setq-local before-save-hook nil)))
