;; -*- lexical-binding: t -*-

(global-set-key (kbd "C-;") 'kaspi/comment-or-uncomment)

(global-set-key (kbd "<f1>") 'eat-project)
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

(global-set-key (kbd "S-<up>") 'scroll-down-line)
(global-set-key (kbd "S-<down>") 'scroll-up-line)

(global-set-key (kbd "<undo>") 'undo)
(global-set-key (kbd "<redo>") 'undo-redo)

;; Możliwość skakania z powrotem po przewinięciach ekranu.
;;
;; (Domyślnie scroll-*-command nie pamięta marka)
(advice-add 'scroll-up-command :before 'push-mark)
(advice-add 'scroll-down-command :before 'push-mark)
;; O tyle inne od domyślnego exchange-point-and-mark, że nie aktywuje regionu.
(global-set-key (kbd "C-c x") 'pop-to-mark-command)
(defvar kaspi/pop-to-mark-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map "x" 'pop-to-mark-command)
    map))
(put 'pop-to-mark-command 'repeat-map 'kaspi/pop-to-mark-repeat-map)
