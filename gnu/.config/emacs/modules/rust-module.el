;; -*- lexical-binding: t -*-

(add-to-list 'load-path (concat +vendor-dir+ "rust-mode"))

(autoload 'rust-mode "rust-mode" "" t)

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
