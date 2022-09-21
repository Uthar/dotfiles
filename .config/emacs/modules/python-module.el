;; -*- lexical-binding: t -*-

(add-to-list 'load-path (concat +vendor-dir+ "elpy"))

(autoload 'elpy-enable "elpy" "" t)
(add-hook 'python-mode-hook 'elpy-enable)

(setq elpy-modules '(
                     elpy-module-sane-defaults
                     elpy-module-company
                     elpy-module-eldoc
                     elpy-module-flymake
                     ;; elpy-module-highlight-indentation
                     elpy-module-pyvenv
                     ;; elpy-module-yasnippet
                     ;; elpy-module-django
                     ))

;; Disable auto complete
(add-hook 'elpy-mode-hook
  (lambda ()
    (setq-local company-idle-delay nil)))
