;; -*- lexical-binding: t -*-

(defun kaspi/call-for-live-compile (fn)
  (lambda ()
    (let ((contents (minibuffer-contents)))
      (when (and (minibufferp)
                 (not (string-empty-p contents))
                 (memq this-command lcr-commands))
        (funcall fn contents)))))

(defun kaspi/live-compile (query fn)
  (interactive)
  (let ((hook (kaspi/call-for-live-compile fn)))
    ;; compile mode has hard coded sleep after command termination
    (advice-add 'sit-for :override (cl-constantly nil) '((name . noop)))
    (setopt compilation-always-kill t)
    (add-hook 'post-command-hook hook)
    (unwind-protect
        (let ((inhibit-message t))
          (read-string query))
      (remove-hook 'post-command-hook hook)
      (setopt compilation-always-kill nil)
      (advice-remove 'sit-for 'noop))))

(defvar fd-last-buffer nil)

(defvar fd-regexp-alist
  '(("^\\(.+\\)$" 1)))

(require 'compile)
(require 'dired)

(define-compilation-mode fd-mode "fd"
  "Mode to find files with the fd program"
  (setq fd-last-buffer (current-buffer))
  (setq-local compilation-error-regexp-alist
              fd-regexp-alist)
  (setq-local compilation-error-face
              dired-symlink-face))

(defun kaspi/fd (regex)
  (interactive)
  (compilation-start (format "fd --color=never %s" regex) #'fd-mode))

(defun kaspi/fd2 ()
  (interactive)
  (kaspi/live-compile "find file: " 'kaspi/fd))

(defun kaspi/rg (regex)
  (interactive)
  (rg regex "all" default-directory))

(defun kaspi/rg2 ()
  (interactive)
  (kaspi/live-compile "grep for: " 'kaspi/rg))
