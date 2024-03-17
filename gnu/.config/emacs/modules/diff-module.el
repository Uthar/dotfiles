;; -*- lexical-binding: t -*-

;; Configure syntax highlighting in diff buffers
(setq diff-font-lock-syntax t)

;; Prevent messing with the diff by removing save hooks.
(add-hook 'diff-mode-hook
  (lambda ()
    (setq-local require-final-newline nil)
    (setq-local before-save-hook nil)))

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
