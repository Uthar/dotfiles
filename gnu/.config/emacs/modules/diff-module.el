;; -*- lexical-binding: t -*-

;; Configure syntax highlighting in diff buffers
(setq diff-font-lock-syntax t)

;; Przejdź do następnego hunka po zaaplikowaniu.
(setopt diff-advance-after-apply-hunk t)

;; Zapisuj pliki od razu po zaaplikowaniu hunka, bo i tak bym to zrobił.
(advice-add 'diff-apply-hunk :after
  (lambda (&rest _)
    (save-window-excursion
      (pop-to-buffer (diff-find-file-name))
      (recenter)
      (save-buffer)))
  '((name . kaspi/after-apply)))

(add-hook 'diff-mode-hook
  (lambda ()
    ;; Prevent messing with the diff by removing save hooks.
    (setq-local require-final-newline nil)
    (setq-local before-save-hook nil)
    (define-key diff-mode-map (kbd "M-<backspace>") 'backward-kill-word)
    (define-key diff-mode-map (kbd "M-o") 'other-window)
    ;; Highlight current hunk at point
    (hl-hunk-mode)))

(defvar hl-hunk-overlay nil
  "Overlay used to highlight the hunk under point.")

(defface hl-hunk
  '((t :background "CornflowerBlue"))
  "Default face for highlighting the current hunk in hl-hunk mode.")

(defvar hl-hunk-face 'hl-hunk)

(define-minor-mode hl-hunk-mode
  "Toggle highlighting of the current hunk."
  :global nil
  (if hl-hunk-mode
    (progn
      (add-hook 'post-command-hook #'hl-hunk-highlight nil t))
    (progn
      (remove-hook 'post-command-hook #'hl-hunk-highlight t)
      (delete-overlay hl-hunk-overlay))))

(defun hl-hunk-highlight ()
  ;; This happens on 'revert-buffer'.
  (when-let ((ov hl-hunk-overlay))
    (unless (eq (current-buffer) (overlay-buffer ov))
      (delete-overlay ov)
      (setq-local hl-hunk-overlay nil)))
  (unless hl-hunk-overlay
    (setq-local hl-hunk-overlay (make-overlay 1 1))
      (overlay-put hl-hunk-overlay 'line-prefix
        (propertize "|" 'display `(left-fringe vertical-bar ,hl-hunk-face))))
  (when-let ((pos (hl-hunk-position)))
    (cl-destructuring-bind (start . end) pos
      (let ((ov hl-hunk-overlay))
        (unless (and (= start (overlay-start ov))
                     (= end (overlay-end ov)))
          (move-overlay ov start end))))))

(defun hl-hunk-position ()
  (ignore-errors
    (cons
     (save-excursion (diff-beginning-of-hunk) (point))
     (save-excursion (diff-end-of-hunk) (point)))))

(defun kaspi/flash-line (&rest _)
  (pulse-momentary-highlight-region
   (line-beginning-position)
   (line-end-position)))

(advice-add 'diff-hunk-next :after 'kaspi/flash-line)
(advice-add 'diff-hunk-prev :after 'kaspi/flash-line)
(advice-add 'diff-file-next :after 'kaspi/flash-line)
(advice-add 'diff-file-prev :after 'kaspi/flash-line)

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
