;; -*- lexical-binding: t -*-

;; Stuff that's coming in Emacs 29

(setq help-window-keep-selected t) 

(add-to-list
 'menu-bar-buffers-menu-command-entries
 '(kill-current-buffer menu-item "Kill buffer" kill-current-buffer :help "Delete the current buffer")
 t)

(setq switch-to-prev-buffer-skip-regexp '("*Help* *inferior-lisp*"))

(setq compilation-hidden-output (""))
