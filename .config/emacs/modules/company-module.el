;; -*- lexical-binding: t -*-

(add-to-list 'load-path (concat +vendor-dir+ "company-mode"))

;; Who would ever want a completion to get downcased?
(setq company-dabbrev-downcase nil)

;; Match case-insensitively
(setq company-dabbrev-ignore-case t)

;; Disable automatic completion pop-ups for less distraction
(setq company-idle-delay nil)

;; Enable quick match selection with M-{1..9}
(setq company-show-numbers 'left)

;; Don't clutter the modeline
(setq company-lighter "")

(autoload 'global-company-mode "company")

(add-hook 'after-init-hook 'global-company-mode)

(global-set-key (kbd "C-M-<tab>") 'completion-at-point)
(global-set-key (kbd "M-<tab>") 'company-complete)
