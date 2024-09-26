;; -*- lexical-binding: t -*-

(setopt org-startup-folded 'show2levels)

(with-eval-after-load "org"
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (lisp . t))))

;; For <q TAB and <s TAB spinnets
(with-eval-after-load "org"
  (require 'org-tempo))
