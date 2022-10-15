;; -*- lexical-binding: t -*-

;; Like 'comment-or-uncomment-region', but doesn't unmark the region
;; afterwards, so that there's no need to call 'evil-visual-restore'.
;;
;; Also comments the current line when there is no region.
(defun kaspi/comment-or-uncomment ()
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
  (setq deactivate-mark nil))

(defun kaspi/x-copy ()
  (interactive)
  (when (region-active-p)
    (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
    (message "Yanked region to clipboard")
    (deactivate-mark)))

(defun kaspi/x-paste ()
  (interactive)
  (insert (shell-command-to-string "xsel -o -b")))

(defun kaspi/select-or-exit-minibuffer ()
  (interactive)
  (if current-prefix-arg
      (exit-minibuffer))
      (select-window (minibuffer-window)))

(defun kaspi/bash ()
  (interactive)
  (ansi-term "bash"))

(defun kaspi/xdg-open ()
  (interactive)
  (start-process "xdg-open" nil "xdg-open" (ffap-string-at-point)))

(defun kaspi/open-init-file ()
  (interactive)
  (find-file user-init-file))

(defun kaspi/reload-init-file ()
  (interactive)
  (load-file user-init-file))

(defun kaspi/copy-line ()
  (interactive)
  (kill-ring-save (line-beginning-position) (line-end-position)))

(defvar kaspi/duplicate-line-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" 'duplicate-line)
    map))

(put 'duplicate-line 'repeat-map 'kaspi/duplicate-line-repeat-map)

(defun kaspi/ignore-arguments (f)
  (lambda (&rest _)
    (interactive)
    (funcall f)))

(advice-add 'duplicate-line :after (kaspi/ignore-arguments 'next-line))


