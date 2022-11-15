;; -*- lexical-binding: t -*-

(defun kaspi/call-with-minibuffer-contents (command transform)
  (interactive)
  (when (and (minibufferp) (not (string-empty-p (minibuffer-contents))))
    (funcall command (funcall transform (minibuffer-contents)))))

(defun kaspi/live-compile (query fn)
  (interactive)
  (advice-add 'sit-for :override (cl-constantly nil) '((name . noop)))
  (setopt compilation-always-kill t)
  (add-hook 'post-command-hook fn)
  (unwind-protect
      (let ((inhibit-message t))
        (read-string query))
    (remove-hook 'post-command-hook fn)
    (setopt compilation-always-kill nil)
    (advice-remove 'sit-for 'noop)))

(defun kaspi/fd ()
  (kaspi/run-with-minibuffer-contents 'fd 'identity))

(defun kaspi/fd2 ()
  (interactive)
  (kaspi/live-compile "find file: " 'kaspi/fd))

(defvar fd-last-buffer nil)

(defvar fd-regexp-alist
  '(("^\\(.*\\)$" 1 nil nil 0)))

(require 'compile)

(define-compilation-mode fd-mode "fd"
  "Mode to find files with the fd program"
  (setq fd-last-buffer (current-buffer))
  (setq-local compilation-error-regexp-alist
              fd-regexp-alist))

(defun fd (regex)
  (interactive)
  (compilation-start (format "fd --color=never %s" regex) #'fd-mode))






 
