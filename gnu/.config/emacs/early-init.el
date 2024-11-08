;; -*- lexical-binding: t -*-

;; Speed up init time by not running any package code.
;;
;; It's faster, though less convenient, to precisely say what and when
;; should be loaded. This is exactly what code in the 'modules'
;; subdirectory is doing.
(setq package-enable-at-startup nil)

;; Zapisuj stan do katalogu ze stanem.
(setq user-emacs-directory "~/.local/state/emacs/")

;; Zapisuj cache do katalogu z cachem.
(when (native-comp-available-p)
  (startup-redirect-eln-cache "~/.local/cache/eln-cache/"))

(setq inhibit-x-resources t)
