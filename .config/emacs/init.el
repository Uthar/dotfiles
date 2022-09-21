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
;;
;; (byte-recompile-directory +vendor-dir+ 0)
;; (byte-recompile-directory +module-dir+ 0)

(add-to-list 'load-path +module-dir+)

(load "startup-module")
(load "mouse-module")
(load "games-module")
(load "emacs-module")
(load "disabled-commands-module")
(load "lisp-module")
(load "keys-module")
(load "vim-module")
(load "slime-module")
(load "nix-module")
(load "yaml-module")
(load "swiper-module")
(load "direnv-module")
(load "vc-module")
(load "dired-module")
(load "diff-module")
(load "recentf-module")
(load "org-module")
(load "which-key-module")
(load "utility-module")
(load "editorconfig-module")
(load "company-module")
(load "native-search-module")
(load "markdown-module")
(load "wgrep-module")
(load "todo-module")
(load "java-module")
(load "cider-module")
(load "glsl-module")
(load "ansi-module")
(load "docker-module")
(load "winum-module")

(setq custom-theme-directory (concat user-emacs-directory "themes/"))
(setq custom-safe-themes t)

(run-at-time "0.5 sec" nil
  (lambda () 
    (load-theme 'modus-operandi)))
