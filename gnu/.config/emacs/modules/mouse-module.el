;; -*- lexical-binding: t -*-


;; A little faster mouse scroll.
(rplaca mouse-wheel-scroll-amount 2)

;; Makes mouse scroll more predictable by not accelerating it while
;; moving a lot of lines.
(setq mouse-wheel-progressive-speed nil)

;; BUG Causes unpleasant stuttering on scrolling up when cursor is at the top
;; (add-hook 'after-init-hook 'pixel-scroll-precision-mode)
(setq pixel-scroll-precision-interpolate-page t)
(setq pixel-scroll-precision-interpolate-mice t)

(defun kaspi/pixel-scroll-up ()
  (interactive)
  (pixel-scroll-precision-interpolate
   (* 0.5 (window-text-height nil t))
   nil 1))

(defun kaspi/pixel-scroll-down ()
  (interactive)
  (pixel-scroll-precision-interpolate
   (* -0.5 (window-text-height nil t))
   nil 1))

;; (when window-system
;;   (global-set-key (kbd "M-v") 'kaspi/pixel-scroll-up)
;;   (global-set-key (kbd "C-v") 'kaspi/pixel-scroll-down))
