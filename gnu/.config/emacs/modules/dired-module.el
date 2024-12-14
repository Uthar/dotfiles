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

 ;; Przejdź automatycznie do pierwszego pliku przy otwieraniu nowego subtree.
 dired-trivial-filenames t

 )

(defun kaspi/dired-toggle-hidden ()
  (interactive)
  (cond
   ((string-match-p "A" dired-listing-switches)
    (setq dired-listing-switches (remove ?A dired-listing-switches))
    (setq dired-actual-switches (remove ?A dired-actual-switches)))
   (:else
    (setq dired-listing-switches (concat dired-listing-switches "A"))
    (setq dired-actual-switches (concat dired-actual-switches "A"))))
  (dired-revert))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "M-h") 'kaspi/dired-toggle-hidden)
  (define-key dired-mode-map (kbd "(") 'kaspi/dired-toggle-details)
  (define-key dired-mode-map [mouse-1] nil))

(defvar kaspi/dired-details 1)

(defun kaspi/dired-toggle-details ()
  (interactive)
  (setq kaspi/dired-details (- kaspi/dired-details))
  (dired-hide-details-mode kaspi/dired-details))
  
(defun kaspi/dired-next-subdir ()
  (interactive)
  (dired-next-subdir 1)
  (dired-goto-next-nontrivial-file))

(defun kaspi/dired-prev-subdir ()
  (interactive)
  (dired-prev-subdir 1)
  (dired-goto-next-nontrivial-file))

(defun kaspi/dired-goto-subdir ()
  (interactive)
  (call-interactively #'dired-goto-subdir)
  (dired-goto-next-nontrivial-file))

;; (setq dired-after-readin-hook (cdr dired-after-readin-hook))
(add-hook 'dired-after-readin-hook
  (lambda ()
    (let ((inhibit-read-only t)
          (prefix (1- (length (expand-file-name default-directory)))))
      (cl-loop for (subdir . next) on dired-subdir-alist do
        (when next
          (cl-destructuring-bind (dir . marker) subdir
            (let ((beg (+ 2 marker)))
              (add-text-properties beg (+ beg prefix) '(invisible dired))
              (add-text-properties beg (+ beg (length dir)) `(help-echo ,dir)))))))))

(defun kaspi/dired-copy-truename-as-kill ()
  (interactive)
  (dired-copy-filename-as-kill 0))
  
(add-hook 'dired-mode-hook
  (lambda ()
    (hl-line-mode)
    (dired-hide-details-mode kaspi/dired-details)
    (setq header-line-format '("  " default-directory))
    (local-set-key "b" 'dired-up-directory)
    (local-set-key "e" 'wdired-change-to-wdired-mode)
    (local-set-key "W" 'kaspi/dired-copy-truename-as-kill)
    (local-set-key (kbd "C-M-n") 'kaspi/dired-next-subdir)
    (local-set-key (kbd "C-M-p") 'kaspi/dired-prev-subdir)
    (local-set-key (kbd "C-M-k") 'dired-kill-subdir)
    (local-set-key (kbd "M-G") 'kaspi/dired-goto-subdir)
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
(advice-add 'dired-display-file :around 'kaspi/call-with-display-buffer-in-mru-window-overriding-action)
(advice-add 'dired-find-file-other-window :around 'kaspi/call-with-display-buffer-in-mru-window-overriding-action)
(advice-add 'dired-mouse-find-file-other-window :around 'kaspi/call-with-display-buffer-in-mru-window-overriding-action)
