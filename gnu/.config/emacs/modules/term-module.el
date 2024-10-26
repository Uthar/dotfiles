;; -*- lexical-binding: t -*-

(add-to-list 'load-path (concat +vendor-dir+ "eat"))

(autoload 'eat "eat" "" t)
(autoload 'eat-project "eat" "" t)

(advice-add 'eat-kill-process :override 'kaspi/noop)

(add-hook 'eat-mode-hook 'toggle-truncate-lines)

;; Zmniejszenie tych limitów nie pomogło, czasem eat zacina całego emacs i
;; zaczyna się cons/gc infinite loop. Zaczęło się to po włączeniu bash
;; integration. Nie da się odblokować C-g ani SIGUSR2. Może to dlatego, że nie
;; używam eatowego terminfo?
(setopt eat-line-input-ring-size 10000)

(setopt eat-term-scrollback-size (* 1 1024 1024)) ;1MiB

(with-eval-after-load "eat"
  (define-key eat-semi-char-mode-map (kbd "M-o") 'other-window)
  (define-key eat-semi-char-mode-map (kbd "C-?") 'kaspi/noop)
  (define-key eat-semi-char-mode-map (kbd "C-/") 'kaspi/noop)
  (define-key eat-semi-char-mode-map (kbd "<undo>") 'kaspi/noop)
  (define-key eat-semi-char-mode-map (kbd "<redo>") 'kaspi/noop))
