;; -*- lexical-binding: t -*-


;;;; Ustawienia

;; Configure syntax highlighting in diff buffers
(setq diff-font-lock-syntax t)

;; Nie przechodź do następnego hunka po zaaplikowaniu, bo zrobię to w advice.
;; Dlaczego? Bo domyślnie skacze jeszcze przed wykonaniem after advice, a
;; najpierw chcę jeszcze zapisać zmieniony bufor.
(setq diff-advance-after-apply-hunk nil)

(defvar kaspi/diff-after-apply-hunk-actions '(save next))

(defun kaspi/diff-after-apply-hunk (&rest _)
  "Save/kill/advance in the diff bufer after applying a hunk."
  (when (memq 'save kaspi/diff-after-apply-hunk-actions)
    (save-window-excursion
      (when-let* ((file (diff-find-file-name)))
        (find-file-other-window file)
        (recenter)
        (save-buffer))))
  (cond
   ((memq 'kill kaspi/diff-after-apply-hunk-actions) (diff-hunk-kill))
   ((memq 'next kaspi/diff-after-apply-hunk-actions) (diff-hunk-next))))

(advice-add 'diff-apply-hunk :after 'kaspi/diff-after-apply-hunk)

(add-hook 'diff-mode-hook
  (lambda ()
    ;; Nie psuj mi diffa
    (setq-local require-final-newline nil)
    (setq-local before-save-hook nil)
    (define-key diff-mode-map (kbd "M-<backspace>") 'backward-kill-word)
    (define-key diff-mode-map (kbd "M-o") 'other-window)))


;;;; Podświetlanie hunka pod kursorem

(add-hook 'diff-mode-hook 'hl-hunk-mode)

(defvar-local hl-hunk-overlay-arrow-position nil)

(define-minor-mode hl-hunk-mode
  "Toggle highlighting of the current hunk."
  :global nil
  (if hl-hunk-mode
    (progn
      (make-local-variable 'overlay-arrow-variable-list) 
      (add-to-list 'overlay-arrow-variable-list 'hl-hunk-overlay-arrow-position)
      (add-hook 'post-command-hook #'hl-hunk-highlight nil t))
    (progn
      (setq hl-hunk-overlay-arrow-position nil)
      (remove-hook 'post-command-hook #'hl-hunk-highlight t)
      (remove-from-list 'overlay-arrow-variable-list 'hl-hunk-overlay-arrow-position))))

(defun hl-hunk-highlight ()
  (unless (memq this-command '(self-insert-command delete-backward-char kill-region yank undo))
    (setq hl-hunk-overlay-arrow-position (copy-marker (kaspi/diff-beginning-of-hunk-position)))))

(defun kaspi/diff-beginning-of-hunk-position ()
  (ignore-errors (save-excursion (diff-beginning-of-hunk) (point))))

(defun kaspi/diff-end-of-hunk-position ()
  (ignore-errors (save-excursion (diff-end-of-hunk) (point))))
  
(defun kaspi/diff-hunk-span ()
  (cons
   (kaspi/diff-beginning-of-hunk-position)
   (kaspi/diff-end-of-hunk-position)))

(defun kaspi/flash-line (&rest _)
  (pulse-momentary-highlight-region
   (line-beginning-position)
   (line-end-position)))


;;;; Ediff

;; Ediff control window in same frame as A/B windows
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Better for wide screens which I use
(setq ediff-split-window-function 'split-window-horizontally)

;; Combine A and B during merge
(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
    (concat 
     (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
     (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

(add-hook 'ediff-keymap-setup-hook
  (lambda ()
    (define-key ediff-mode-map (kbd "x") 'ediff-copy-both-to-C)))
