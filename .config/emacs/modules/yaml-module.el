;; -*- lexical-binding: t -*-

(add-to-list 'load-path (concat +vendor-dir+ "yaml-mode"))

(autoload 'yaml-mode "yaml-mode" "" t)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

(add-hook 'yaml-mode-hook 'display-line-numbers-mode)
