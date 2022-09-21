;; -*- lexical-binding: t -*-

(add-to-list 'load-path (concat +vendor-dir+ "winum"))
(autoload 'winum-mode "winum")
(add-hook 'after-init-hook 'winum-mode)

(setq winum-auto-setup-mode-line t)

(dotimes (n 9)
  (let* ((n (1+ n))
         (key (kbd (format "M-%d" n)))
         (command (intern (format "winum-select-window-%d" n))))
    (global-set-key key command)))
