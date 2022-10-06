;; -*- lexical-binding: t -*-

;; CIDER config

(with-eval-after-load 'cider 
  (define-key cider-repl-mode-map (kbd "C-c M-o") 'cider-repl-clear-buffer)
  (add-hook 'cider-connected-hook 'cider-repl-clear-buffer)
  (require 'cider-macroexpansion))
(autoload 'slime-random-words-of-encouragement "slime")
(setq cider-connection-message-fn 'slime-random-words-of-encouragement)
(setq cider-repl-display-help-banner nil)
(setq cider-offer-to-open-cljs-app-in-browser nil)
(setq cider-show-error-buffer 'except-in-repl)
(setq cider-use-fringe-indicators nil)
(setq cider-mode-line-show-connection nil)
(setq cider-use-overlays nil)

;; It has a lot of dependencies:

(add-to-list 'load-path (concat +vendor-dir+ "cider"))
(add-to-list 'load-path (concat +vendor-dir+ "cider/lib/clojure-mode"))
(add-to-list 'load-path (concat +vendor-dir+ "cider/lib/parseclj"))
(add-to-list 'load-path (concat +vendor-dir+ "cider/lib/parseedn"))
(add-to-list 'load-path (concat +vendor-dir+ "cider/lib/queue"))
(add-to-list 'load-path (concat +vendor-dir+ "cider/lib/sesman"))
(add-to-list 'load-path (concat +vendor-dir+ "cider/lib/spinner"))

(autoload 'cider-jack-in-clj "cider" "" t)
(autoload 'cider-connect-clj "cider" "" t)
(autoload 'cider-jack-in-cljs "cider" "" t)
(autoload 'cider-connect-cljs "cider" "" t)

(global-set-key (kbd "C-c M-j") 'cider-jack-in-clj)
(global-set-key (kbd "C-c M-c") 'cider-connect-clj)
(global-set-key (kbd "C-c M-J") 'cider-jack-in-cljs)
(global-set-key (kbd "C-c M-C") 'cider-connect-cljs)

(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.edn\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojurescript-mode))
(add-to-list 'auto-mode-alist '("\\.cljc\\'" . clojurec-mode))

(autoload 'clojure-mode "clojure-mode" "" t)
(autoload 'clojurescript-mode "clojure-mode" "" t)
(autoload 'clojurec-mode "clojure-mode" "" t)

(add-to-list 'load-path (concat +vendor-dir+ "cider-eval-sexp-fu"))
(with-eval-after-load 'cider
  (require 'cider-eval-sexp-fu))
