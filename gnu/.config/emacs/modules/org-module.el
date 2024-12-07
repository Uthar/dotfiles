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
(setq org-agenda-span '30)
(setq org-agenda-start-day "-7d")
(setq org-agenda-mouse-1-follows-link t)

(setq calendar-today-marker 'lazy-highlight)
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)

(setopt calendar-month-name-array
  ["styczeń" "luty" "marzec" "kwiecień" "maj" "czerwiec"
   "lipiec" "sierpień" "wrzesień" "październik" "listopad" "grudzień"])

(setopt calendar-day-name-array
  ["poniedziałek" "wtorek" "środa" "czwartek" "piątek" "sobota" "niedziela"])

(setq calendar-week-start-day 0)
(setq calendar-weekend-days '(5 6))
(setq org-agenda-weekend-days '(0 6))
(setq org-agenda-format-date "%a %e %b %Y W%W")

(setq calendar-holidays
  '((holiday-fixed 1 1 "Nowy Rok")
    (holiday-fixed 1 6 "Święto Trzech Króli")
    (holiday-easter-etc 0 "pierwszy dzień Wielkiej Nocy")
    (holiday-easter-etc 1 "drugi dzień Wielkiej Nocy")
    (holiday-fixed 5 1 "Święto Pracy")
    (holiday-fixed 5 3 "Święto Narodowe Trzeciego Maja")
    (holiday-easter-etc 49 "Zielone Świątki")
    (holiday-easter-etc 60 "dzień Bożego Ciała")
    (holiday-fixed 8 15 "Wniebowzięcie Najświętszej Maryi Panny")
    (holiday-fixed 11 1 "Wszystkich Świętych")
    (holiday-fixed 11 11 "Narodowe Święto Niepodległości")
    (holiday-fixed 12 25 "pierwszy dzień Bożego Narodzenia")
    (holiday-fixed 12 26 "drugi dzień Bożego Narodzenia")
    ))

(setq calendar-mark-holidays-flag t)

(setq zoneinfo-style-world-list
  '(("UTC" "UTC")
    ("Europe/Warsaw" "Warszawa")
    ("Europe/Oslo" "Oslo")
    ;; ("Europe/Athens" "Ateny")
    ;; ("Japan" "Tokio")
    ;; ("US/Pacific" "Seattle")
    ("America/New_York" "Nowy Jork")
    ;; ("US/Eastern" "Virginia")
    ;; ("US/Central" "Texas")
    ;; ("Asia/Kolkata" "Indie")
    ;; ("Japan" "Tokio")
    ;; ("America/Sao_Paulo" "Sao Paulo")
    ("Chile/Continental" "Santiago")
    ))

(setq org-clock-persist t)

(setq org-log-done 'time)
