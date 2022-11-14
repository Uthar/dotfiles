;; -*- lexical-binding: t -*-

(add-to-list 'load-path (concat +vendor-dir+ "emacs-direnv"))

;; Much less spammy when changing buffers very quickly (buffers that
;; are in different directories with an .envrc).
(setq direnv-always-show-summary nil)

(autoload 'direnv-mode "direnv")

(add-hook 'after-init-hook 'direnv-mode)
