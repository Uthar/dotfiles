;; -*- lexical-binding: t -*-

;; Disable completion pop ups
(setq ivy-do-completion-in-region nil)

(add-to-list 'load-path (concat +vendor-dir+ "swiper"))

(autoload 'ivy-mode "ivy")
(autoload 'counsel-mode "counsel")
(autoload 'swiper "swiper")

(add-hook 'after-init-hook 'ivy-mode)
(add-hook 'after-init-hook 'counsel-mode)

(add-hook 'ivy-mode-hook
  (lambda ()
    (when-let ((cell (assoc 'ivy-mode minor-mode-alist)))
      (rplacd cell '("")))))

(add-hook 'counsel-mode-hook
  (lambda ()
    (when-let ((cell (assoc 'counsel-mode minor-mode-alist)))
      (rplacd cell '("")))))

(global-set-key (kbd "C-s") 'swiper)

;; Make the default action of swiper in dired to find the file

(defun kaspi/swiper-dired (&optional initial-input)
  (interactive)
  (swiper initial-input))

(defun kaspi/swiper-dired-find-file (&rest _)
  (with-ivy-window
    (dired-find-file)))

(with-eval-after-load 'swiper
  (ivy-set-actions 'kaspi/swiper-dired
                   '(("o" kaspi/swiper-dired-find-file "find file"))))
