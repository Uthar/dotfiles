;; -*- lexical-binding: t -*-

(defun kaspi/enable-todo-highlighting ()
  (interactive)
  (font-lock-add-keywords nil
      '(("\\<TODO\\>[^-]"  0 '(:foreground "#e5534b" :weight bold) t)
        ("\\<FIXME\\>[^-]" 0 '(:foreground "#e0823d" :weight bold) t)
        ("\\<NOTE\\>[^-]"  0 '(:foreground "#57ab5a" :weight bold) t))))

(add-hook 'prog-mode-hook 'kaspi/enable-todo-highlighting)
