;; -*- lexical-binding: t -*-

;; Speed up init time by not running any package code.
;;
;; It's faster, though less convenient, to precisely say what and when
;; should be loaded. This is exactly what code in the 'modules'
;; subdirectory is doing.
(setq package-enable-at-startup nil)
