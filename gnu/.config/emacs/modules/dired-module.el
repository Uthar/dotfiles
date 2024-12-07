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

 ;; Przeciąganie plików myszą
 dired-mouse-drag-files t

 ;; Omija puste linijki w dired podczas poruszania się, w szczególności gdy
 ;; przekraczana jest granica między subdirs.
 dired-movement-style 'bounded

 )

(defun kaspi/dired-toggle-hidden ()
  (interactive)
  (cond
   ((string-match-p "a" dired-listing-switches)
    (setq dired-listing-switches (remove ?a dired-listing-switches))
    (setq dired-actual-switches (remove ?a dired-actual-switches)))
   (:else
    (setq dired-listing-switches (concat dired-listing-switches "a"))
    (setq dired-actual-switches (concat dired-actual-switches "a"))))
  (dired-revert))

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
    (local-set-key (kbd "C-M-k") 'dired-kill-subdir)
    (local-set-key (kbd "<mouse-8>") 'dired-up-directory)
    (setq-local switch-to-buffer-obey-display-actions t)
    (setq-local mouse-1-click-follows-link nil)))

;; W emacs -Q środkuje domyślnie... coś mam popsute w configu
(advice-add 'dired-revert :around
  (lambda (fun &rest args)
    (let ((pos (window-start)))
      (unwind-protect
        (apply fun args)
        (set-window-start (selected-window) pos)))))

(defun kaspi/call-with-display-buffer-in-mru-window-overriding-action (fun &rest args)
  (let ((display-buffer-overriding-action '(display-buffer-use-some-window (some-window . mru))))
    (apply fun args)))

(advice-add 'dired-view-file :around 'kaspi/call-with-display-buffer-in-mru-window-overriding-action)
