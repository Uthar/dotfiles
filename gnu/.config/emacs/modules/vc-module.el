;; -*- lexical-binding: t -*-

(defun kaspi/toggle-hook (hook function)
  (if (and (consp (symbol-value hook))
           (memq function (symbol-value hook)))
      (remove-hook hook function)
    (add-hook hook function)))

(defun kaspi/vc-annotate-toggle-annotation-visibility* ()
  (kaspi/toggle-hook 'vc-annotate-mode-hook
                     'vc-annotate-toggle-annotation-visibility))

;; Don't display the actual Git/Hg/Fossil commands in the minibuffer.
;; This used to be fun, but not anymore after installing 'diff-hl'
;; which constantly calls VC commands.
(setq vc-command-messages nil)

(setq vc-annotate-background-mode nil)

(with-eval-after-load 'vc-annotate
  ;; Make the v key in 'vc-annotation-mode' persist between revision
  ;; changes. Useful for "time machine" functionality, because there's
  ;; no need to constantly disable annotations when all you want is
  ;; the code.
  (define-key vc-annotate-mode-map (kbd "v")
    (lambda ()
      (interactive)
      (vc-annotate-toggle-annotation-visibility)
      (kaspi/vc-annotate-toggle-annotation-visibility*))))

;; Chcę zobaczyć diffa nawet jeśli plik nie jest zapisany.
;;
;; Poprawia też sytuację, gdy log-edit-show-diff, do której nie da się jej
;; przekazać argumentu NOT-URGENT wywala się gdy któryś z usuniętych plików ma
;; otwarty bufor.
;;
;; Być może nie jest to najlepsze rozwiązanie, bo nie wziąłem pod uwagę
;; wszystkich innych miejsc, w których użyta jest VC-BUFFER-SYNC.
(advice-add 'vc-buffer-sync :filter-args 
  (lambda (&rest _) '(t))
  '((name . kaspi/always-not-urgent)))

;; Automatically pop up an emphemeral diff buffer (via C-x 4 4 C-c C-d) with the
;; current changes in 'vc-log-edit'. I almost always want this, so this saves me
;; those 2 key chords on each check-in
(with-eval-after-load 'log-edit
  (add-hook 'log-edit-hook
    (lambda ()
      (let ((log-window (selected-window))
            (diff-buffer nil))
        (other-window-prefix)
        (log-edit-show-diff)
        (setq diff-buffer (current-buffer))
        (select-window log-window)
        (add-hook 'kill-buffer-hook
          (lambda ()
            (kill-buffer diff-buffer))
          100 t)))
    100))

;; Ensure smerge detects diff conflicts.
;; (Stock regexes were sometimes wrong)
(with-eval-after-load 'smerge-mode
  (setq smerge-begin-re "^<<<<<<< \\(.*\\)\n")
  (setq smerge-end-re "^>>>>>>> \\(.*\\)\n")
  (setq smerge-base-re "^||||||| \\(.*\\)\n")
  (setq smerge-lower-re "^=======\\(.*\\)\n"))

;; Fossil support
(add-to-list 'load-path (concat +vendor-dir+ "vc-fossil"))
(add-to-list 'vc-handled-backends 'Fossil t)
(autoload 'vc-fossil-registered "vc-fossil")


;; Highlight modified lines in the fringe
(setq diff-hl-highlight-revert-hunk-function 'diff-hl-revert-highlight-first-column)

(add-to-list 'load-path (concat +vendor-dir+ "diff-hl"))
(autoload 'turn-on-diff-hl-mode "diff-hl" "" t)
(autoload 'diff-hl-dired-mode-unless-remote "diff-hl-dired" "" t)
(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
(add-hook 'conf-mode-hook 'turn-on-diff-hl-mode)
(add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode-unless-remote)

(defun kaspi/vc-filter-command-function (command file-or-list flags)
  (let ((flags (cond
                ((and (string= command "git")
                      (string= (cl-first flags) "merge"))
                 (cl-list* (cl-first flags)
                           "--no-ff" "--no-commit"
                           (cl-rest flags)))
                ((and (string= command "git")
                      (string= (cl-first flags) "pull"))
                 (cl-list* (cl-first flags)
                           "--no-commit"
                           (cl-rest flags)))
                (t flags))))
    (list command file-or-list flags)))

;; Ogranicz destrukcyjne działanie gita
(setq vc-filter-command-function 'kaspi/vc-filter-command-function)

;; I want to see the diff of the file at point, not the diff of marked files.
;;
;; The workflow is the following:
;; 1. See the changes in a file (=)
;; 2. Decide whether to commit it or not (m)
;; 3. Repeat for all changed files (n)
;;
;; By default, after the first file is marked (m), the next diff (=) will only
;; show the changes in that one file, not the next (n) file currently under
;; point.
;;
;; (If I want to see the diff of marked files, I can just go try to commit (v)
;; and the diff will appear.)
(defun kaspi/vc-dir-diff-current-file ()
  (interactive)
  (vc-diff nil t (list vc-dir-backend (list (vc-dir-current-file)))))

(add-hook 'vc-dir-mode-hook
  (lambda ()
    (define-key vc-dir-mode-map (kbd "=") 'kaspi/vc-dir-diff-current-file)))

(add-hook 'log-view-mode-hook 'hl-line-mode)
