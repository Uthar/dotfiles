;; -*- lexical-binding: t -*-

(defun kaspi/%call-for-live-compile (fn)
  (lambda ()
    (let ((contents (minibuffer-contents)))
      (when (and (minibufferp)
                 (= 1 (minibuffer-depth))
                 (not (string-empty-p contents))
                 (memq this-command '(self-insert-command
                                      delete-backward-char
                                      kill-region yank undo)))
        (funcall fn contents)))))

(defun kaspi/%live-compile (fn)
  (let* ((input-hook (kaspi/%call-for-live-compile fn))
         (buffer-name (format "*%s*" (symbol-name fn)))
         (query (format "%s in %s for: " (symbol-name fn) (file-name-nondirectory (directory-file-name default-directory)))))
    ;; compile mode has hard coded sleep after command termination
    (advice-add 'sit-for :override 'kaspi/noop)
    (setopt compilation-always-kill t)
    (add-hook 'post-command-hook input-hook)
    (unwind-protect
        (let* ((inhibit-message t))
          (read-string query)
          (select-window (get-buffer-window buffer-name)))
      (remove-hook 'post-command-hook input-hook)
      (setopt compilation-always-kill nil)
      (advice-remove 'sit-for 'kaspi/noop))))

(defvar kaspi/fd-regexp-alist
  '(("^\\(.+\\)$" 1)))

(define-derived-mode kaspi/fd-mode grep-mode "Grep/fd"
  "Mode to find files with the fd program"
  (setq-local compilation-error-regexp-alist kaspi/fd-regexp-alist))

(define-derived-mode kaspi/rg-mode grep-mode "Grep/rg"
  "Mode to search files with the rg program")

(defun kaspi/fd (regex)
  (compilation-start (format "fd -H -c never %s" regex) #'kaspi/fd-mode))

(setq grep-use-headings t)

(defun kaspi/rg (regex)
  (compilation-start (format "rg --hidden --no-heading -nH %s" regex) #'kaspi/rg-mode))
  
(defun kaspi/live-fd ()
  (interactive)
  (kaspi/with-sensible-directory
   (kaspi/%live-compile 'kaspi/fd)))

(defun kaspi/live-rg ()
  (interactive)
  (kaspi/with-sensible-directory
   (kaspi/%live-compile 'kaspi/rg)))

;; Writeable grep
(add-to-list 'load-path (concat +vendor-dir+ "wgrep"))

(setq wgrep-change-readonly-file t)
(setq wgrep-auto-save-buffer t)

(with-eval-after-load 'grep
  (define-key grep-mode-map (kbd "e") 'wgrep-change-to-wgrep-mode))

(autoload 'wgrep-change-to-wgrep-mode "wgrep" "" t)
