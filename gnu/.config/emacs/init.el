;; -*- lexical-binding: t -*-

;; The 'modules' directory contains elisp code modules that tweaks
;; Emacs to either change its default behavior or add minor new
;; functionality. They may also tweak third-party elisp modules.
(defvar +module-dir+ (concat user-emacs-directory "modules/"))

;; The 'vendor' directory contains third party elisp code.
;; It provides major new functionality that's too big to be put in
;; 'modules', because it would load too slow.
(defvar +vendor-dir+ (concat user-emacs-directory "vendor/"))

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

(let ((hour (nth 2 (decode-time))))
  (cond
   ((not window-system)
    (load-theme 'modus-vivendi))
   ((<= 7 hour 20)
    (load-theme 'modus-operandi-tinted))
   (t
    (load-theme 'modus-vivendi-tinted))))

(unless window-system
  (xterm-mouse-mode))

;; notes:

;; diff a/ b/ in eshell creates a diff-mode buffer
;; Also = in dired
;; C-x v ~  - visits other revision of file in other window
;; M-x reverse-region : reverse order of lines in buffer
;; C-x v M D - best vc command, show CHANGES in one branch relative to another
;; C-x v M L - best vc command, show COMMITS in one branch relative to another
;; aggressive-completion : autorefreshing in completions buffer
;; (global-)prettify-symbols-mode, makes it easy to have ligatures
;; M-x scratch-buffer - recreate *scratch*
;; C-x e  call kb macro
;; j in dired, fast go to file or directory by name
;; C-x v C-h - show keybindings starting with C-x v (can be whatever)
;; align-regexp: align columns by some regex: Example:
;;   asdasd            = 1
;;   asdasdasdasdasdas = 2
;; Check out ediff instead of smerge: (smerge-ediff) - https://www.youtube.com/watch?v=9S2pMZ6U5Tc
;; - Używam ediffa do wygodnego porównywania plików z jednym po lewej a drugin po prawej
;;   (w przeciwieństwie do klasycznego "patch" gdzie są poprzeplatane)
;; C-j - Select whatever is in minibuffer, ignoring completions
;; C-x 4 {b,f,d,.} - Buffer/find file/dired/M-. in other window (shorter than C-x 4 4 M-.)
;; Search for files with prefixes: C-x C-f ~/R/n/p/d/l-m ->~/Repos/nixpkgs/pkgs/development/lisp-modules/
;; C-x z - repeat last operation - for example:
;;  M-z / (delete until '/' character)
;;  C-x z z z z z (do that 5 additional times)
;; C-M-t transpose-sexps
;; M-s . M-s o - immediately open occur buffer of symbol at point
;; Info: the best documentation browser - simple design, keyboard shortcuts - encourages high focus
;;   expecially "index" feature is amazing
;;   (C-h i C-h m)
;; C-h m -> H.K.O.N. endorsed communication channel interface - achieve tranquillity.

;; Dla ekranów z wysoką rozdzielczością:
;; (set-face-attribute 'default nil :height 130)

(setq ring-bell-function 'ignore)

;; Odpalaj litery tylko przy zapisie pliku 
(setq flymake-no-changes-timeout nil)

(add-to-list 'default-frame-alist '(font . "Terminus-11"))

;; Unfortunately this is really slow
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
