;; -*- lexical-binding: t -*-

(add-to-list 'load-path (concat +vendor-dir+ "eat"))

(autoload 'eat "eat" "" t)
(autoload 'eat-project "eat" "" t)

(advice-add 'eat-kill-process :override 'kaspi/noop)

(add-hook 'eat-mode-hook 'toggle-truncate-lines)

(setopt eat-line-input-ring-size 10000)

(setopt eat-term-scrollback-size (* 1 1024 1024)) ;1MiB

(with-eval-after-load "eat"
  (define-key eat-semi-char-mode-map (kbd "M-o") 'other-window))
