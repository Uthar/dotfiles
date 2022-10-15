;; -*- lexical-binding: t -*-

(add-to-list 'load-path (concat +vendor-dir+ "wgrep"))
(add-to-list 'load-path (concat +vendor-dir+ "rg"))

(setq wgrep-change-readonly-file t)
(setq wgrep-auto-save-buffer t)

(with-eval-after-load 'grep
  (define-key grep-mode-map (kbd "e") 'wgrep-change-to-wgrep-mode))

(autoload 'rg "rg" "" t)
(autoload 'wgrep-change-to-wgrep-mode "wgrep" "" t)
