;; -*- lexical-binding: t -*-

;;;;;;; Builtins for sanity

(setopt completions-format 'one-column)
(setopt completion-auto-help 'visible)
(setopt completion-show-help nil)

(setopt completion-ignore-case t)
(setopt read-buffer-completion-ignore-case t)
(setopt read-file-name-completion-ignore-case t)

(add-to-list 'completion-styles 'flex)

;;;;;;;; Limit completions buffer size

(setq temp-buffer-max-height 15)
(temp-buffer-resize-mode)

;;;;;;;; Complete filenames with C-M-i

(add-to-list 'completion-at-point-functions 'comint--complete-file-name-data)

;;;;;;;; Fix to screen jump in emacs 29-trunk

(defun kaspi/minibuffer-choose-completion ()
  (interactive)
  (unwind-protect
      (with-minibuffer-completions-window
        (choose-completion))
    (completion-in-region-mode -1)))

(define-key completion-in-region-mode-map
            (kbd "RET")
            'kaspi/minibuffer-choose-completion)

;;;;;;;; Add file CAPF

(add-to-list 'load-path (concat +vendor-dir+ "cape"))
(autoload 'cape-file "cape")
(add-to-list 'completion-at-point-functions 'cape-file)

;;;;;;;; Select first candidate or minibuffer contents in minibuffer completion

(defun kaspi/minibuffer-end-completion ()
  (interactive)
  ;; Now really insert the completion into the minibuffer
  ;; Previously I just "select" it for the visual aspect
  (when (get-buffer-window (get-buffer "*Completions*"))
    (minibuffer-previous-completion)
    (minibuffer-next-completion))
  (minibuffer-complete-and-exit))

(define-key minibuffer-local-must-match-map
            (kbd "RET")
            'kaspi/minibuffer-end-completion)

;;;;;;;; Same for in-buffer completion

(advice-add 'completion-at-point
            :after
            (lambda (&rest _) (minibuffer-next-completion))
            '((name . next-completion)))

;;;;;;;; Switch to completions shortcut

;; (define-key completion-in-region-mode-map (kbd "M-c") 'switch-to-completions)
;; (define-key minibuffer-mode-map (kbd "M-c") 'switch-to-completions)

;;;;;;;; Auto refresh completions buffer after typing

(defvar lcr-timer nil)
(defvar lcr-delay 0.01)

(defvar lcr-commands
  (list 'self-insert-command
        'delete-backward-char
        'backward-delete-char-untabify)
  "Commands to trigger completion help after, whether in region or minibuffer")

(defvar lcr-minibuffer-disabled-commands
  (list 'query-replace
        'query-replace-regexp)
  "Minibuffer commands to not refresh completions for")

(defun lcr-refresh ()
  (cond
   ((and (minibufferp)
         (not (memq current-minibuffer-command
                    lcr-minibuffer-disabled-commands)))
    (let ((minibuffer-completion-auto-choose nil))
      (minibuffer-completion-help)
      (minibuffer-next-completion)))
   (completion-in-region-mode
    (completion-help-at-point)
    (minibuffer-next-completion))))

(defun lcr-after-change (&rest _)
  (when lcr-timer
    (cancel-timer lcr-timer))
  (when (and (memq this-command lcr-commands)
             (or completion-in-region-mode
                 (minibufferp)))
    (setf lcr-timer (run-at-time lcr-delay nil 'lcr-refresh))))

(define-minor-mode global-lcr-mode
  "Live Completion-In-Region Mode"
  :global t
  (cond
   (global-lcr-mode
    (add-hook 'post-command-hook 'lcr-after-change))
   (t
    (remove-hook 'post-command-hook 'lcr-after-change))))

(add-hook 'after-init-hook 'global-lcr-mode)

(define-key completion-in-region-mode-map (kbd "C-n") 'minibuffer-next-completion)
(define-key completion-in-region-mode-map (kbd "C-p") 'minibuffer-previous-completion)

(define-key minibuffer-mode-map (kbd "C-n") 'minibuffer-next-completion)
(define-key minibuffer-mode-map (kbd "C-p") 'minibuffer-previous-completion)