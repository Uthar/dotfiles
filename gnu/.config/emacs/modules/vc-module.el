;; -*- lexical-binding: t -*-

;; Don't display the actual Git/Hg/Fossil commands in the minibuffer.
;; This used to be fun, but not anymore after installing 'diff-hl'
;; which constantly calls VC commands.
(setq vc-command-messages nil)

(setq vc-annotate-background-mode nil)

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

(with-eval-after-load 'log-edit
  ;; I tak na to nie patrzę, a to dodatkowe okno do przeskakiwania.
  (remove-hook 'log-edit-hook 'log-edit-show-files)
  ;; Otwieraj diffa - żeby było wiadomo co wejdzie do repo.
  (add-hook 'log-edit-hook
    (lambda ()
      (let ((log-window (selected-window))
            (diff-buffer nil))
        (other-window-prefix)
        (log-edit-show-diff)
        (setq diff-buffer (current-buffer))
        (select-window log-window)
        (cl-flet ((close-diff-buffer () (kill-buffer diff-buffer)))
          (add-hook 'kill-buffer-hook #'close-diff-buffer t t))))))

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

;; Żeby nie wchodził w nieśledzone podkatalogi.
;; (Niestety normalny filter nie wystarczy bo to jest wewnętrzne).
;; Inaczej duże katalogi source czy outputs z nix develop sprawiają kłopoty.
(advice-add 'vc-git-command :filter-args
  (lambda (args)
    (cl-destructuring-bind (buffer okstatus file-or-list &rest flags) args
      (cl-destructuring-bind (cmd &rest flags) (or flags '(nil))
        (cond
         ((cl-endp flags) args)
         ((and (string= "ls-files" cmd)
               (or (cl-find "-o" flags :test #'string=)
                   (cl-find "--others" flags :test #'string=)))
          (cl-list* buffer okstatus file-or-list cmd "--directory" flags))
         (t args)))))
  '((name . kaspi/dont-recurse-untracked-dirs)))

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

(with-eval-after-load 'vc-dir
  (define-key vc-dir-mode-map (kbd "=") 'kaspi/vc-dir-diff-current-file))

(add-hook 'log-view-mode-hook 'hl-line-mode)
