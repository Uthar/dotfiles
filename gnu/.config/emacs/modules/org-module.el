;; -*- lexical-binding: t -*-

(setq org-startup-folded 'show2levels)

(with-eval-after-load "org"
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (lisp . t))))

;; For <q TAB and <s TAB spinnets
(with-eval-after-load "org"
  (require 'org-tempo))

(setq org-agenda-skip-unavailable-files t)
(setq org-default-notes-file "~/.notes.org")
(setq org-agenda-files '("~/Org/" "~/.notes.org"))
(setq org-agenda-span 'week)
(setq org-agenda-mouse-1-follows-link t)

;; Poniedziałek
(setq calendar-week-start-day 1)

(setq zoneinfo-style-world-list
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

(setq org-clock-persist t)

(setq org-log-done 'time)
