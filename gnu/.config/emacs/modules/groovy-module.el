;; -*- lexical-binding: t; -*-

(add-to-list 'load-path (concat +vendor-dir+ "groovy-mode"))

(autoload 'groovy-mode "groovy-mode" "" t)
(setq groovy-indent-offset 2)

(add-to-list 'auto-mode-alist '("\\.g\\(?:ant\\|roovy\\|radle\\)\\'" . groovy-mode))
(add-to-list 'auto-mode-alist '("/Jenkinsfile\\'" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))
