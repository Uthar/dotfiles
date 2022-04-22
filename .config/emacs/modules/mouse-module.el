;; -*- lexical-binding: t -*-


;; A little faster mouse scroll.
(rplaca mouse-wheel-scroll-amount 2)

;; Makes mouse scroll more predictable by not accelerating it while
;; moving a lot of lines.
(setq mouse-wheel-progressive-speed nil)
