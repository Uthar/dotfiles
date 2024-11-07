;; -*- lexical-binding: t -*-

(add-to-list 'load-path (concat +vendor-dir+ "markdown-mode"))

(add-to-list 'auto-mode-alist
  '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))

(autoload 'markdown-mode "markdown-mode" "" t)

(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("readme\\.md\\'" . gfm-mode))

(autoload 'gfm-mode "markdown-mode" "" t)

;; Disable very annoying pop up when entering the ``` code block
(setopt markdown-gfm-use-electric-backquote nil)
