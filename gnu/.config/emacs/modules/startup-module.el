;; -*- lexical-binding: t -*-

;; This is the first code to run when Emacs starts up.
;;
;; It makes the startup faster by inhibiting GC during the loading of
;; the init file.
;;
;; Default GC operation is restored afterwards.

(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
  (lambda ()
    (message "booted in %s" (emacs-init-time))
    (setq gc-cons-threshold
          (car (get 'gc-cons-threshold 'standard-value)))))
