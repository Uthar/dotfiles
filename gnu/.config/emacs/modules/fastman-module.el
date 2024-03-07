;; -*- lexical-binding: t -*-

;; Man is slow. This is fast(er).

(defun kaspi/man-configuration-nix ()
  (interactive)
  (let ((auto-mode-alist nil))
    ;; (unless (file-exists-p (concat user-emacs-directory "configuration.nix.manpage"))
    (find-file "/home/kasper/Repos/testpipes/configuration.nix")
    (read-only-mode))
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max)))
  )

(defun read-manpage ()
  (interactive)
  (let ((all (list "")))
    (dolist (dir (directory-files "~/.nix-profile/share/man" t "man"))
      (nconc all (directory-files dir t)))
    (dolist (dir (directory-files "/run/current-system/sw/share/man" t "man"))
      (nconc all (directory-files dir t)))
    (completing-read "Manpage: " all)))

(defun fastman (manpage)
  (interactive (list (read-manpage)))
  (let ((process-environment '("PATH=/run/current-system/sw/bin"
                               "PAGER=cat"
                               "MANWIDTH=80"
                               "GROFF_SGR=1")))
    (make-process :name "fastman"
                  :buffer "*Fastman*"
                  :command (list "man" manpage)
                  :filter (lambda (prod data)
                            (with-current-buffer "*Fastman*"
                              (let ((inhibit-read-only t))
                                (save-excursion
                                  (goto-char (point-min))
                                  (insert data))))))
    (switch-to-buffer "*Fastman*")
    (read-only-mode)
    (goto-char (point-min))))
