;; -*- lexical-binding: t -*-

;; 'which-key-mode' shows a list of available keybinding shortly after
;; pressing a key. This is useful to quickly remember what commands
;; are available under a particular prefix.

(setq which-key-lighter "")
(setq which-key-dont-use-unicode t)
(add-to-list 'load-path (concat +vendor-dir+ "emacs-which-key"))
(autoload 'which-key-mode "which-key")
(add-hook 'after-init-hook 'which-key-mode)
