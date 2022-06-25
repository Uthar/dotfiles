;; -*- lexical-binding: t -*-

;; Stuff that's coming in Emacs 29

(setq help-window-keep-selected t) 

(add-to-list
 'menu-bar-buffers-menu-command-entries
 '(kill-current-buffer menu-item "Kill buffer" kill-current-buffer :help "Delete the current buffer")
 t)

(setq switch-to-prev-buffer-skip-regexp '("*Help* *inferior-lisp*"))

(setq compilation-hidden-output (""))

;; Można zapobiec wklejaniu ogromnych łańcuchów
(add-to-list 'yank-transform-functions
  (lambda ()           
    t))

(global-set-key (kbd "M-<up>") 'minibuffer-previous-completion)
(global-set-key (kbd "M-<down>") 'minibuffer-next-completion)
(global-set-key (kbd "M-RET") 'minibuffer-choose-completion)


(setq find-sibling-rules
      '(("\\([^/]+\\)\\.c\\'" "\\1.h")))


;; (global-set-key (kbd "?") 'duplicate-line)

(global-set-key (kbd "<f7>") 'recentf-open)
