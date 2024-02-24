;; -*- lexical-binding: t -*-

;;;; Clickable links:

(defun kaspi/browse-url-at-mouse-except-at-eol (event)
  "Open the link unless the click happened the end of it or at eol"
  (interactive "e")
  (unless (or (eql (point) (pos-eol))
              (eql (char-after) ?\s))
    (browse-url-at-mouse event)))

(defvar kaspi/links-font-lock-keymap
  (let ((map (make-sparse-keymap)))
    ;; Manual says it should be mouse-2 but this works for me.
    (define-key map [mouse-1] 'kaspi/browse-url-at-mouse-except-at-eol)
    map))

(defvar kaspi/links-font-lock-keywords
  '(("http[s]?://[[:graph:]]+"  0 
     `(face link mouse-face highlight keymap ,kaspi/links-font-lock-keymap) t)))

;; TODO minor mode?

(defun kaspi/enable-clickable-links ()
  (interactive)
  (font-lock-add-keywords nil kaspi/links-font-lock-keywords)
  (font-lock-update))

(defun kaspi/disable-clickable-links ()
  (interactive)  
  (font-lock-remove-keywords nil kaspi/links-font-lock-keywords)
  (font-lock-update))

(add-hook 'prog-mode-hook
  (lambda ()
    (make-local-variable 'font-lock-extra-managed-props)
    (add-to-list 'font-lock-extra-managed-props 'mouse-face)
    (add-to-list 'font-lock-extra-managed-props 'keymap)))

(add-hook 'conf-mode-hook
  (lambda ()
    (make-local-variable 'font-lock-extra-managed-props)
    (add-to-list 'font-lock-extra-managed-props 'mouse-face)
    (add-to-list 'font-lock-extra-managed-props 'keymap)))

(add-hook 'prog-mode-hook 'kaspi/enable-clickable-links)
(add-hook 'conf-mode-hook 'kaspi/enable-clickable-links)

;; See http://example.org/foo?bar=krasn#baz
;; See https://example.org/foo?bar=krasn&blah=foo%20baz#baz 123
