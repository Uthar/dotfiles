;; -*- lexical-binding: t -*-

(add-to-list 'load-path (concat +vendor-dir+ "dockerfile-mode"))
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(autoload 'dockerfile-mode "dockerfile-mode" "" t)

(add-to-list 'load-path (concat +vendor-dir+ "docker-tramp"))
(autoload 'docker-tramp "docker-tramp")
(with-eval-after-load 'tramp
  (require 'docker-tramp))
