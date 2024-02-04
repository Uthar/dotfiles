;; -*- lexical-binding: t -*-

(setq

 ;; Prevent creating stray buffers when mass-walking directories.
 dired-kill-when-opening-new-dired-buffer t

 ;; Makes it show directories first
 ;; Also show dotfiles before normal files with -v
 dired-listing-switches "--group-directories-first -vlh"

 dired-dwim-target t

 ;; Limit filenames to window length
 dired-filename-display-length 'window

 )

(defun kaspi/dired-toggle-hidden ()
  (interactive)
  (if (string-match-p "a" dired-actual-switches)
      (dired "." (remove ?a dired-listing-switches))
      (dired "." (concat dired-listing-switches "a")))
  (setf dired-listing-switches dired-actual-switches))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "M-h") 'kaspi/dired-toggle-hidden)
  (define-key dired-mode-map [mouse-1] 'dired-find-file))

;; TODO(kasper): Make this persist between directories.
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(add-hook 'dired-mode-hook
  (lambda ()
    (local-set-key "b" 'dired-up-directory)
    (setq-local mouse-1-click-follows-link nil)))
