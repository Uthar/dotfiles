;; -*- lexical-binding: t -*-

(add-to-list 'load-path (concat +vendor-dir+ "company-mode"))

;; Who would ever want a completion to get downcased?
(setq company-dabbrev-downcase nil)

;; Match case-insensitively
(setq company-dabbrev-ignore-case t)

;; Disable automatic completion pop-ups for less distraction
(setq company-idle-delay nil)

;; Enable quick match selection with M-{1..9}
(setq company-show-numbers 'left)

;; Don't clutter the modeline
(setq company-lighter "")

(autoload 'global-company-mode "company")

(add-hook 'after-init-hook 'global-company-mode)

(global-set-key (kbd "C-M-<tab>") 'completion-at-point)
(global-set-key (kbd "M-<tab>") 'company-complete)

(setopt completions-format 'one-column)
(setopt completion-auto-help 'visible)
(setopt completion-show-help nil)
(setopt display-buffer-alist '(("\\*Completions\\*"
                                display-buffer-at-bottom
                                (window-height . 0.25))))
(add-to-list 'completion-styles 'flex)

;;;;;;;; live completion-in-region

(defvar lcr-timer nil)
(defvar lcr-delay 0.2)

(defun lcr-refresh ()
  (when completion-in-region-mode
    (completion-help-at-point)
    (with-minibuffer-completions-window
      (setq-local cursor-face-highlight-nonselected-window t)
      (first-completion))))

(defun lcr-after-change (beg end len)
  (when completion-in-region-mode
    (when lcr-timer
      (cancel-timer lcr-timer))
    (setf lcr-timer (run-at-time lcr-delay nil 'lcr-refresh))))

(define-minor-mode lcr-mode
  "Live Completion-In-Region Mode"
  :global nil
  (cond
   (lcr-mode
    (add-hook 'after-change-functions 'lcr-after-change nil t)
    (add-hook 'completion-setup-hook 'minibuffer-next-completion nil t))
   (t
    (remove-hook 'after-change-functions 'lcr-after-change t)
    (remove-hook 'completion-setup-hook 'minibuffer-next-completion t))))

(defun kaspi/minibuffer-choose-completion ()
  (interactive)
  (with-minibuffer-completions-window
   (choose-completion)))

(define-key completion-in-region-mode-map (kbd "M-RET") 'kaspi/minibuffer-choose-completion)
