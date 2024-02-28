;; -*- lexical-binding: t -*-

(add-to-list 'load-path (concat +vendor-dir+ "eat"))

(autoload 'eat "eat" "" t)

(advice-add 'eat-kill-process :override 'kaspi/noop)

(add-hook 'eat-mode-hook 'toggle-truncate-lines)
