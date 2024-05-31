;; -*- lexical-binding: t -*-

(add-to-list 'load-path (concat +vendor-dir+ "eat"))

(autoload 'eat "eat" "" t)

(advice-add 'eat-kill-process :override 'kaspi/noop)

(add-hook 'eat-mode-hook 'toggle-truncate-lines)

(setopt eat-line-input-ring-size 1000000)

(setopt eat-term-scrollback-size (* 2 1024 1024 1024)) ;2GiB
