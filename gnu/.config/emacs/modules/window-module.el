;; -*- lexical-binding: t -*-

(defvar kaspi/winner-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map "," 'winner-undo)
    (define-key map "." 'winner-redo)
    map))

(defvar kaspi/window-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map ";" 'windmove-right)
    (define-key map "j" 'windmove-left)
    (define-key map "k" 'windmove-down)
    (define-key map "l" 'windmove-up)
    (define-key map ":" 'windmove-swap-states-right)
    (define-key map "J" 'windmove-swap-states-left)
    (define-key map "K" 'windmove-swap-states-down)
    (define-key map "L" 'windmove-swap-states-up)
    map))

(global-set-key (kbd "C-c w ,") 'winner-undo)
(global-set-key (kbd "C-c w .") 'winner-redo)
(global-set-key (kbd "C-c w :") 'windmove-swap-states-right)
(global-set-key (kbd "C-c w J") 'windmove-swap-states-left)
(global-set-key (kbd "C-c w K") 'windmove-swap-states-down)
(global-set-key (kbd "C-c w L") 'windmove-swap-states-up)
(global-set-key (kbd "C-c w ;") 'windmove-right)
(global-set-key (kbd "C-c w j") 'windmove-left)
(global-set-key (kbd "C-c w k") 'windmove-down)
(global-set-key (kbd "C-c w l") 'windmove-up)

(with-eval-after-load 'winner
  (put 'winner-undo 'repeat-map 'kaspi/winner-repeat-map)
  (put 'winner-redo 'repeat-map 'kaspi/winner-repeat-map))

(with-eval-after-load 'windmove
  (put 'windmove-swap-states-right 'repeat-map 'kaspi/window-repeat-map)
  (put 'windmove-swap-states-left 'repeat-map 'kaspi/window-repeat-map)
  (put 'windmove-swap-states-down 'repeat-map 'kaspi/window-repeat-map)
  (put 'windmove-swap-states-up 'repeat-map 'kaspi/window-repeat-map)
  (put 'windmove-right 'repeat-map 'kaspi/window-repeat-map)
  (put 'windmove-left 'repeat-map 'kaspi/window-repeat-map)
  (put 'windmove-down 'repeat-map 'kaspi/window-repeat-map)
  (put 'windmove-up 'repeat-map 'kaspi/window-repeat-map))

(defvar kaspi/*saved-window-configuration* nil)

(defun kaspi/save-window-configuration ()
  (interactive)
  (setq kaspi/*saved-window-configuration* (current-window-configuration))
  (message "Zapamiętano rozmieszczenie okien."))

(defun kaspi/restore-window-configuration ()
  (interactive)
  (set-window-configuration kaspi/*saved-window-configuration*)
  (message "Przywrócono rozmieszczenie okien."))

(global-set-key (kbd "C-c w s") 'kaspi/save-window-configuration)
(global-set-key (kbd "C-c w r") 'kaspi/restore-window-configuration)

(put 'previous-window-any-frame 'repeat-map 'other-window-repeat-map)
