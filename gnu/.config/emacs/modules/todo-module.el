;; -*- lexical-binding: t -*-

(defvar kaspi/todo-font-lock-keywords
  '(("\\<TODO\\>"  0 '(:foreground "#e5534b" :weight bold) t)
    ("\\<FIXME\\>" 0 '(:foreground "#e0823d" :weight bold) t)
    ("\\<NOTE\\>"  0 '(:foreground "#57ab5a" :weight bold) t)
    ("\\<BUG\\>"   0 '(:foreground "#9b30ff" :weight bold) t)))

;; TODO
;; FIXME
;; NOTE
;; BUG

(defun kaspi/disable-todo-highlighting ()
  (interactive)
  (font-lock-remove-keywords nil kaspi/todo-font-lock-keywords))

(defun kaspi/enable-todo-highlighting ()
  (interactive)
  (font-lock-add-keywords nil kaspi/todo-font-lock-keywords))

(add-hook 'prog-mode-hook 'kaspi/enable-todo-highlighting)

;; Because the expansion is upcased...
(add-hook 'slime-macroexpansion-minor-mode-hook 'kaspi/disable-todo-highlighting)
