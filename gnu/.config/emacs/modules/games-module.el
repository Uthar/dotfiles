;; -*- lexical-binding: t -*-

;; Settings for the built-in games.

(with-eval-after-load 'tetris
  (define-key tetris-mode-map "z" 'tetris-rotate-next)
  (define-key tetris-mode-map "x" 'tetris-rotate-prev))
