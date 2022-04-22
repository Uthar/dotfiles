;; -*- lexical-binding: t -*-

(setq

 ;; Prevent creating stray buffers when mass-walking directories.
 dired-kill-when-opening-new-dired-buffer t

 ;; Makes it show directories first like the ranger program.
 dired-listing-switches "--group-directories-first -lh"

 )

(defun kaspi/dired-toggle-hidden ()
  (interactive)
  (if (string-match-p "a" dired-actual-switches)
      (dired "." (remove ?a dired-listing-switches))
      (dired "." (concat dired-listing-switches "a")))
  (setf dired-listing-switches dired-actual-switches))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "M-h") 'kaspi/dired-toggle-hidden)
  (define-key dired-mode-map "N" nil)
  (define-key dired-mode-map "n" nil)
  (define-key dired-mode-map [mouse-1] 'dired-find-file))

;; TODO(kasper): Make this persist between directories.
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(add-hook 'dired-mode-hook
  (lambda ()
    (setq-local mouse-1-click-follows-link nil)
    (local-set-key "j" 'dired-next-line)
    (local-set-key "k" 'dired-previous-line)
    (local-set-key "l" 'dired-find-file)
    (local-set-key "h" 'dired-up-directory)))
