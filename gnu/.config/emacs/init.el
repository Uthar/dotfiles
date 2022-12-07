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
(load "ansi-module")
(load "docker-module")
(load "restclient-module")
(load "window-module")
(load "scheme-module")
(load "org-module")
(load "rust-module")
(load "search-module")
(load "blog-module")

(let ((hour (cl-nth-value 2 (decode-time))))
  (if (and window-system (<= 7 hour 20))
      (load-theme 'modus-operandi)
      (load-theme 'modus-vivendi)))

;; notes:

;; diff a/ b/ in eshell creates a diff-mode buffer
;; C-x v ~  - visits other revision of file in other window
;; M-x reverse-region : reverse order of lines in buffer
;; C-x v M D - best vc command, show CHANGES in one branch relative to another
;; C-x v M L - best vc command, show COMMITS in one branch relative to another
;; aggressive-completion : autorefreshing in completions buffer
;; (global-)prettify-symbols-mode, makes it easy to have ligatures
;; M-x scratch-buffer - recreate *scratch*
;; C-x e  call kb macro
;; j in dired, fast go to file or directory by name
