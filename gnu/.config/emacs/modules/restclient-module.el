;; -*- lexical-binding: t -*-

(add-to-list 'load-path (concat +vendor-dir+ "restclient"))

(autoload 'restclient-mode "restclient" "" t)

(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))
