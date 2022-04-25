;; -*- lexical-binding: t -*-

(add-to-list 'load-path (concat +vendor-dir+ "wgrep"))
(add-to-list 'load-path (concat +vendor-dir+ "rg"))

(setq wgrep-change-readonly-file t)

(autoload 'rg "rg" "" t)
(autoload 'wgrep-change-to-wgrep-mode "wgrep" "" t)
