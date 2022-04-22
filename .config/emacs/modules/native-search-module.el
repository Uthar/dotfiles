;; -*- lexical-binding: t -*-

(defun kaspi/guess-directory (cmd-name)
  (if current-prefix-arg
      (counsel-read-directory-name (concat cmd-name " in directory: "))
      (or (ignore-errors (project-root (project-current)))
          default-directory)))

;; It's called 'counsel-fzf-command', but fzf doesn't write the
;; results to stdout until *all* the results have been processed. This
;; makes it work really slow on huge directories. Though, by simply
;; substituting for fd here, I get a snappier experience because fd
;; writes to stdout right away.
(setq counsel-fzf-cmd "fd --hidden --follow -c never \"%s\"")


;; NOTE(kasper): That both fd and rg have the save flags is just a
;; coincidence.

(defun kaspi/fd-dwim ()
  (interactive)
  (let ((dir (kaspi/guess-directory "fd")))
    (counsel-fzf "" dir (concat "fd in " dir ": "))))

(defun kaspi/rg-dwim ()
  (interactive)
  (let ((dir (kaspi/guess-directory "rg")))
    (counsel-rg "" dir " --hidden --follow " (concat "rg in " dir ": "))))
