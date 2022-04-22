;; -*- lexical-binding: t -*-

(add-to-list 'load-path (concat +vendor-dir+ "editorconfig-emacs"))
(autoload 'editorconfig-mode "editorconfig")

;; Without this autoload, editorconfig will fail with an error saying
;; that this function is undefined. This is because they have switched
;; to a pure elisp editorconfig parser. But it is not loaded by
;; default.
(autoload 'editorconfig-core-get-properties-hash "editorconfig-core")

(add-hook 'after-init-hook 'editorconfig-mode)
(setq editorconfig-mode-lighter "")
