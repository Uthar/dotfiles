;; -*- lexical-binding: t -*-

;; Speed up init time by not running any package code.
;;
;; It's faster to precisely say what and when should be loaded. This
;; is exactly what code in the 'modules' subdirectory is doing.
(setq package-enable-at-startup nil)

;; Disable the package system.
;;
;; Running random code downloaded from the internet is not so smart.
;; It's safer to explicitly download the code, read it, then decide
;; whether to use it or not.
(setq package-user-dir nil)
(setq package-directory-list nil)
