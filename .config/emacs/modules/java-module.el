;; -*- lexical-binding: t -*-

;; Don't indent things below a top-level Java class
(add-hook 'java-mode-hook
  (lambda ()
    (c-set-offset 'inclass 0)))
