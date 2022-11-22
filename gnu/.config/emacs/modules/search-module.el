;; -*- lexical-binding: t -*-

(defun kaspi/%call-for-live-compile (fn)
  (lambda ()
    (let ((contents (minibuffer-contents)))
      (when (and (minibufferp)
                 (= 1 (minibuffer-depth))
                 (not (string-empty-p contents))
                 (memq this-command refresh-completions-commands))
        (funcall fn contents)))))

(defun kaspi/%live-compile (fn)
  (let* ((input-hook (kaspi/%call-for-live-compile fn))
         (buffer-name (format "*%s*" (symbol-name fn)))
         (query (format "%s for: " (symbol-name fn))))
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

(define-derived-mode kaspi/fd-mode grep-mode "Fd"
  "Mode to find files with the fd program"
  (setq-local compilation-error-regexp-alist kaspi/fd-regexp-alist))

(define-derived-mode kaspi/rg-mode grep-mode "Rg"
  "Mode to search files with the rg program")

(defun kaspi/fd (regex)
  (compilation-start (format "fd -H -c never %s" regex) #'kaspi/fd-mode))

(defun kaspi/rg (regex)
  (compilation-start (format "rg --hidden --no-heading -nH %s" regex) #'kaspi/rg-mode))

(defun kaspi/sensible-directory ()
  (cond (current-prefix-arg (read-directory-name "Dir: "))
        ((project-current) (project-root (project-current)))
        (t default-directory)))

(defun kaspi/call-with-sensible-directory (fn &rest args)
  (let ((default-directory (kaspi/sensible-directory)))
    (apply fn args)))

(cl-defmacro kaspi/with-sensible-directory (&body body)
  `(kaspi/call-with-sensible-directory (lambda () ,@body)))
  
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
