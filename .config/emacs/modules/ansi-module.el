;; -*- lexical-binding: t -*-

;; Show ansi colors in compilation buffers
(with-eval-after-load 'compile 
  (require 'ansi-color)
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter))
