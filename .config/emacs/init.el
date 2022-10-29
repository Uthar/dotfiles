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

(defvar +modules+
  (list "startup-module"
        "mouse-module"
        "games-module"
        "emacs-module"
        "disabled-commands-module"
        "consult-module"
        "lisp-module"
        "keys-module"
        "cl-module"
        "nix-module"
        "yaml-module"
        "direnv-module"
        "vc-module"
        "dired-module"
        "diff-module"
        "recentf-module"
        "utility-module"
        "company-module"
        "markdown-module"
        "wgrep-module"
        "todo-module"
        "java-module"
        "clojure-module"
        "glsl-module"
        "ansi-module"
        "docker-module"
        "python-module"
        "restclient-module"
        "window-module"
        "scheme-module"))

(defun amalgamate ()
  (with-temp-buffer
    (dolist (module +modules+)
      (insert-file-contents (concat +module-dir+ module ".el"))
      (goto-char (point-max)))
    (write-file "amalgamation.el")))

(amalgamate)

(load-file "amalgamation.el")
;; (load-theme 'modus-operandi)

;; notes:

;; diff a/ b/ in eshell creates a diff-mode buffer
;; C-x v ~  - visits other revision of file in other window
;; M-x reverse-region : reverse order of lines in buffer
;; C-x v M D - best vc command, show changes in one branch relative to another
;; aggressive-completion : autorefreshing in completions buffer
;; (global-)prettify-symbols-mode, makes it easy to have ligatures
;; M-x scratch-buffer - recreate *scratch*
