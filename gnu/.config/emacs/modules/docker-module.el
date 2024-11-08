;; -*- lexical-binding: t -*-

(add-to-list 'load-path (concat +vendor-dir+ "dockerfile-mode"))
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(autoload 'dockerfile-mode "dockerfile-mode" "" t)

;; Z jakiegoś powodu tramp się wysypuje przy edytowaniu plików w kontenerach...
(advice-add 'tramp-get-remote-path :filter-return 
  (lambda (path)
    (cons "/bin" path))
  '((name . add-bin)))
