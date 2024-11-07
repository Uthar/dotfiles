;; -*- lexical-binding: t -*-

(add-to-list 'load-path (concat +vendor-dir+ "nix-mode"))

(autoload 'nix-mode "nix-mode")

;; 'nix-repl' is actually in nix-repl.el, but this is convenient to be
;; able to load the rest of the library by running 'nix-repl'
(autoload 'nix-repl "nix-mode" "" t)

(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
