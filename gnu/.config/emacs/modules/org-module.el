;; -*- lexical-binding: t -*-

(setopt org-startup-folded 't)

(with-eval-after-load "org"
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t))))
