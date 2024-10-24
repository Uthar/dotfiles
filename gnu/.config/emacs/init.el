;; -*- lexical-binding: t -*-

;; The 'modules' directory contains elisp code modules that tweaks
;; Emacs to either change its default behavior or add minor new
;; functionality. They may also tweak third-party elisp modules.
(defvar +module-dir+ "~/.config/emacs/modules/")

;; The 'vendor' directory contains third party elisp code.
;; It provides major new functionality that's too big to be put in
;; 'modules', because it would load too slow.
(defvar +vendor-dir+ "~/.config/emacs/vendor/")

;; Run to recompile elisp files. This is worth it for third party
;; libraries in particular, because they are huge. They will load and
;; run much faster when compiled.
;;
;; For the smaller modules, this does not matter so much.
(defun kaspi/recompile-vendor-dir ()
  (interactive)
  (byte-recompile-directory +vendor-dir+ 0))

(add-to-list 'load-path +module-dir+)

(load "startup-module")
(load "mouse-module")
(load "games-module")
(load "emacs-module")
(load "disabled-commands-module")
(load "lisp-module")
(load "keys-module")
(load "cl-module")
(load "nix-module")
(load "yaml-module")
(load "direnv-module")
(load "vc-module")
(load "dired-module")
(load "diff-module")
(load "recentf-module")
(load "utility-module")
(load "completion-module")
(load "markdown-module")
(load "todo-module")
(load "java-module")
(load "clojure-module")
(load "glsl-module")
(load "docker-module")
(load "groovy-module")
(load "restclient-module")
(load "window-module")
(load "scheme-module")
(load "org-module")
(load "rust-module")
(load "search-module")
(load "blog-module")
(load "c++-module")
(load "term-module")
(load "links-module")
(load "asm-module")
(load "proced-module")
(load "novinky-module")
(load "project-module")

(let ((hour (decoded-time-hour (decode-time))))
  (cond
   ((not window-system)
    (load-theme 'modus-vivendi))
   ((<= 7 hour 20)
    (load-theme 'modus-operandi-tinted))
   (t
    (load-theme 'modus-vivendi-tinted))))

(unless window-system
  (xterm-mouse-mode))

;; notatki:

;; diff a/ b/ w eshell otwiera buffor diff-mode
;; Podobnie = w dired
;; C-x v ~  - pyta o vc revision pliku i otwiera ją obok
;; M-x reverse-region : przestawia linijki od ostatniej
;; C-x v M D - pokazuje diffa między branchami
;; C-x v M L - pokazuje log między branchami
;; (global-)prettify-symbols-mode - upiększenia typu λ zamiast lambda
;; M-x scratch-buffer - otwiera lub tworzy usunięty *scratch*
;; C-x e  wywołuje kb macro
;; j in dired, przeskakuje do pliku o danej nazwie (z completing read)
;; C-x v C-h - pokazuje klawisze pod przedrostkiem C-x v (lub dowolnym innym)
;; align-regexp: układa kolumny: Przykład:
;;   asdasd            = 1
;;   asdasdasdasdasdas = 2
;; - Używam ediffa do wygodnego porównywania plików z jednym po lewej a drugin po prawej
;;   (w przeciwieństwie do klasycznego "patch" gdzie są poprzeplatane)
;; C-j - Wybiera surowy tekst wpisany w minibuffer (nie wybierając zaznaczonego completion)
;; C-x 4 {b,f,d,.} - Buffer/find file/dired/M-. w oknie obok (skrócona wersja C-x 4 4 M-.)
;; Otwiera plik po przedrostkach folderów: C-x C-f ~/R/n/p/d/l-m ->~/Repos/nixpkgs/pkgs/development/lisp-modules/
;; C-x z - powtarza ostatni command:
;;  M-z / (delete until '/')
;;  C-x z z z z z (powtórz 5 razy)
;; C-M-t transpose-sexps
;; M-s . M-s o - occur dla symbol at point
;; Info: przeglądarka dokumentacji  (C-h i C-h m)
;;   (`i` w *Help* przechodzi do podręcznika info)
;; C-h m -> pomoc dla major mode
;; Powiadomenia przez dbus: (require 'notifications)
;; Przydaje się rfc2047-decode-region do oglądania patchy z git format-patches][

;; Dla ekranów z wysoką rozdzielczością:
;; (set-face-attribute 'default nil :height 130)

(setq ring-bell-function 'ignore)

;; Odpalaj litery tylko przy zapisie pliku 
(setq flymake-no-changes-timeout nil)

(add-to-list 'default-frame-alist '(font . "Terminus-11"))

;; Niestety bardzo wolne
;; (require 'marginalia)
;; (add-hook 'completion-list-mode-hook 'toggle-truncate-lines)
;; (marginalia-mode 'toggle)

;; ٠١٢٣٤٥٦٧٨٩
(with-eval-after-load "quail/arabic"
  (let ((quail-current-package (assoc "arabic" quail-package-alist)))
    (quail-define-rules
     ((append . t))
     ("0" "\u0660") ("1" "\u0661") ("2" "\u0662") ("3" "\u0663") ("4" "\u0664")
     ("5" "\u0665") ("6" "\u0666") ("7" "\u0667") ("8" "\u0668") ("9" "\u0669"))))
