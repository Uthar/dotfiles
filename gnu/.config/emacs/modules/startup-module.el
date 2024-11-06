;; -*- lexical-binding: t -*-

;; This is the first code to run when Emacs starts up.
;;
;; It makes the startup faster by inhibiting GC during the loading of
;; the init file.
;;
;; Default GC operation is restored afterwards.

(setq gc-cons-threshold 1000000000)
(setq inhibit-message t)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq inhibit-message nil)
    (setq gc-cons-threshold 800000)
    (message "booted in %s" (emacs-init-time))))
