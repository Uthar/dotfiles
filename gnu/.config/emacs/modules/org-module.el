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

(setopt org-agenda-files '("~/Org/"))
(setopt org-agenda-span 20)

(with-eval-after-load "org"
  (make-directory "~/Org/" t))

;; Poniedziałek
(setopt calendar-week-start-day 1)

(setopt zoneinfo-style-world-list
  '(("UTC" "UTC")
    ("Europe/Oslo" "Oslo")
    ;; ("Europe/Athens" "Ateny")
    ;; ("Japan" "Tokio")
    ;; ("US/Pacific" "Seattle")
    ;; ("America/New_York" "Nowy Jork")
    ;; ("US/Eastern" "Virginia")
    ;; ("US/Central" "Texas")
    ;; ("Asia/Kolkata" "Indie")
    ;; ("Japan" "Tokio")
    ;; ("America/Sao_Paulo" "Sao Paulo")
    ;; ("Chile/Continental" "Santiago")
    ))
