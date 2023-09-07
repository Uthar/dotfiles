;; -*- lexical-binding: t -*-

(global-set-key (kbd "C-;") 'kaspi/comment-or-uncomment)

;; TODO(kaspi): Possibly add back the repl window code.
;; (global-set-key (kbd "<f1>") 'toggle-repl-window)
(global-set-key (kbd "<f1>") 'eshell)
(global-set-key (kbd "<f2>") 'dired-jump)
(global-set-key (kbd "<f3>") 'kaspi/live-fd)
(global-set-key (kbd "<f4>") 'kaspi/live-rg)
(global-set-key (kbd "<f5>") 'previous-buffer)
(global-set-key (kbd "<f6>") 'next-buffer)
(global-set-key (kbd "<f7>") 'recentf-open)
(global-set-key (kbd "<f8>") 'kaspi/select-or-exit-minibuffer)
(global-set-key (kbd "<f9>") 'kill-current-buffer)
(global-set-key (kbd "<f10>") 'delete-window)
(global-set-key (kbd "<f11>") 'kill-buffer-and-window)
(global-set-key (kbd "<f12>") 'universal-argument)

;; Wyłącza wkurzający domyślny skrót na 'suspend-emacs', który
;; dodatkowo gryzie się z evilowym 'evil-emacs-state'.
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "ESC ESC ESC"))

(global-set-key (kbd "C-M-z") 'zap-up-to-char)

(global-set-key (kbd "C-M-r") 'raise-sexp)

(global-set-key (kbd "C-c l d") 'duplicate-line)
(global-set-key (kbd "C-c l L") 'kaspi/copy-line)
(global-set-key (kbd "C-c l l") 'kaspi/copy-line*)

(global-set-key (kbd "C-x O") 'previous-window-any-frame)
