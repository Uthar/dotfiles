;; -*- lexical-binding: t -*-

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
