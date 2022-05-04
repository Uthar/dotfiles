;; -*- lexical-binding: t -*-

(defun kaspi/toggle-hook (hook function)
  (if (and (consp (symbol-value hook))
           (memq function (symbol-value hook)))
      (remove-hook hook function)
    (add-hook hook function)))

(defun kaspi/vc-annotate-toggle-annotation-visibility* ()
  (kaspi/toggle-hook 'vc-annotate-mode-hook
                     'vc-annotate-toggle-annotation-visibility))

;; Display the actual Git/Hg/Fossil commands in the minibuffer.
(setq vc-command-messages t)

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

;; Fossil support
(add-to-list 'load-path (concat +vendor-dir+ "vc-fossil"))
(add-to-list 'vc-handled-backends 'Fossil t)
(autoload 'vc-fossil-registered "vc-fossil")

;; TODO(kasper): Decide if these are even needed over plain vc
;; (add-to-list 'load-path (concat user-emacs-directory "vendor/efsl"))
;; (autoload 'efsl "efsl")

;; (setq magit-define-global-key-bindings nil)
;; (add-to-list 'load-path (concat user-emacs-directory "vendor/magit"))
;; (autoload 'magit "magit")
