;; -*- lexical-binding: t -*-

(setq

 ;; Prevent creating stray buffers when mass-walking directories.
 dired-kill-when-opening-new-dired-buffer t

 ;; Makes it show directories first
 ;; Also show dotfiles before normal files with -v
 dired-listing-switches "--group-directories-first -Bvlh"

 ;; Przy kopiowaniu pomiędzy katalogami, od razu podpowiadaj dired otwarty obok.
 ;; Nie wiem czy tego nie wyłączę, bo wtedy przeszkadza w kopiowaniu w tym samym
 ;; katalogu. Niby można po prostu o tym pamiętać.
 dired-dwim-target t

 ;; Limit filenames to window length
 dired-filename-display-length 'window

 ;; Lepszy default
 dired-recursive-copies 'always

 )

(defun kaspi/dired-toggle-hidden ()
  (interactive)
  (if (string-match-p "a" dired-actual-switches)
      (dired "." (remove ?a dired-listing-switches))
      (dired "." (concat dired-listing-switches "a")))
  (setq dired-listing-switches dired-actual-switches))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "M-h") 'kaspi/dired-toggle-hidden)
  (define-key dired-mode-map (kbd "(") 'kaspi/dired-toggle-details)
  (define-key dired-mode-map [mouse-1] 'dired-find-file))

(defvar kaspi/dired-details 1)

(defun kaspi/dired-toggle-details ()
  (interactive)
  (setq kaspi/dired-details (- kaspi/dired-details))
  (dired-hide-details-mode kaspi/dired-details))
  
(add-hook 'dired-mode-hook
  (lambda ()
    (hl-line-mode)
    (dired-hide-details-mode kaspi/dired-details)
    (local-set-key "b" 'dired-up-directory)
    (local-set-key "e" 'wdired-change-to-wdired-mode)
    (local-set-key (kbd "<mouse-8>") 'dired-up-directory)
    (setq-local mouse-1-click-follows-link nil)))
