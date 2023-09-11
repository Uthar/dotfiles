;; -*- lexical-binding: t -*-

;; Don't indent in namespaces
(add-hook 'c++-mode-hook (lambda () (c-set-offset 'innamespace 0)))
