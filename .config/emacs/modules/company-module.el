;; -*- lexical-binding: t -*-

(add-to-list 'load-path (concat +vendor-dir+ "company-mode"))

;; Who would ever want a completion to get downcased?
(setq company-dabbrev-downcase nil)

(setq company-dabbrev-ignore-case t)
(setq company-minimum-prefix-length 2)
(setq company-show-numbers 'left)
(setq company-lighter "")

(autoload 'global-company-mode "company")

(add-hook 'after-init-hook 'global-company-mode)
