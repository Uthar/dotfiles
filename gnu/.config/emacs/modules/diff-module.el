;; -*- lexical-binding: t -*-

;; Configure syntax highlighting in diff buffers
(setq diff-font-lock-syntax t)

;; Don't jump after apply
(setopt diff-advance-after-apply-hunk nil)

(add-hook 'diff-mode-hook
  (lambda ()
    ;; Prevent messing with the diff by removing save hooks.
    (setq-local require-final-newline nil)
    (setq-local before-save-hook nil)
    ;; Highlight current hunk at point
    (require 'hl-line) ; BUG? otherwise custom face doesn't work
    (setq-local hl-line-range-function 'kaspi/diff-current-hunk-position)
    (setq-local hl-line-face 'bold)
    (hl-line-mode)))

(defun kaspi/flash-line (&rest _)
  (pulse-momentary-highlight-region
   (line-beginning-position)
   (line-end-position)))

(advice-add 'diff-hunk-next :after 'kaspi/flash-line)
(advice-add 'diff-hunk-prev :after 'kaspi/flash-line)
(advice-add 'diff-file-next :after 'kaspi/flash-line)
(advice-add 'diff-file-prev :after 'kaspi/flash-line)

(defun kaspi/diff-current-hunk-position ()
  (ignore-errors
    (cons
     (save-excursion (diff-beginning-of-hunk) (point))
     (save-excursion (diff-end-of-hunk) (point)))))

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
