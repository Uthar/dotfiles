;; -*- lexical-binding: t -*-

(add-to-list 'load-path (concat +vendor-dir+ "consult"))

(autoload 'consult-line "consult" "" t)
(autoload 'consult-ripgrep "consult" "" t)
(autoload 'consult-find "consult" "" t)
(autoload 'consult-mark "consult" "" t)

(global-set-key (kbd "C-c c l") 'consult-line)
(global-set-key (kbd "C-c c r") 'consult-ripgrep)
(global-set-key (kbd "C-c c f") 'consult-find)
(global-set-key (kbd "C-c c m") 'consult-mark)
