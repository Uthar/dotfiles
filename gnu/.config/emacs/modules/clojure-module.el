;; -*- lexical-binding: t -*-

;; CIDER config

(with-eval-after-load 'cider 
  (define-key cider-repl-mode-map (kbd "C-c M-o") 'cider-repl-clear-buffer)
  (add-hook 'cider-connected-hook 'cider-repl-clear-buffer)
  (require 'cider-macroexpansion)
  (require 'cider-xref)
  (require 'cider-format)
  (require 'cider-ns))

(setq cider-connection-message-fn (cl-constantly '("Pozdrowienia z Neptuna")))
(setq cider-repl-display-help-banner nil)
(setq cider-offer-to-open-cljs-app-in-browser nil)
(setq cider-show-error-buffer 'except-in-repl)
(setq cider-use-fringe-indicators nil)
(setq cider-mode-line-show-connection nil)
(setq cider-use-overlays nil)
(setq cider-use-tooltips nil)
(setq cider-repl-history-file (locate-user-emacs-file "cider-history"))
(setq cider-repl-tab-command 'indent-for-tab-command)

(add-hook 'cider-after-eval-done-hook 'cider-repl-history-just-save)

(add-hook 'cider-repl-mode-hook
  (lambda () 
   ;; Parse only code after the current prompt for TAB
   ;; indentation. This prevents indenting against printed parens from
   ;; previous repl results. It's achieved by providing a starting
   ;; point to parse from to calculate-lisp-indent so that it does not
   ;; try to guess it itself.
   (setq-local indent-line-function (lambda (&rest _) (lisp-indent-line (calculate-lisp-indent cider-repl-input-start-mark))))
   ;; Same but for indent-region
   (setq-local indent-region-function 'indent-region-line-by-line)))

(add-to-list 'load-path (concat +vendor-dir+ "cider"))
(add-to-list 'load-path (concat +vendor-dir+ "cider/lib/clojure-mode"))
(add-to-list 'load-path (concat +vendor-dir+ "cider/lib/parseclj"))
(add-to-list 'load-path (concat +vendor-dir+ "cider/lib/parseedn"))
(add-to-list 'load-path (concat +vendor-dir+ "cider/lib/queue"))
(add-to-list 'load-path (concat +vendor-dir+ "cider/lib/sesman"))
(add-to-list 'load-path (concat +vendor-dir+ "cider/lib/spinner"))

(autoload 'cider-jack-in-clj "cider" "" t)
(autoload 'cider-connect-clj "cider" "" t)

;; linting
(add-to-list 'load-path (concat +vendor-dir+ "cider/lib/flymake-kondor"))
(autoload 'flymake-kondor-setup "flymake-kondor")
(add-hook 'clojure-mode-hook 'flymake-kondor-setup)
(add-hook 'clojure-mode-hook 'flymake-mode)
  
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.edn\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojurescript-mode))
(add-to-list 'auto-mode-alist '("\\.cljc\\'" . clojurec-mode))

(autoload 'clojure-mode "clojure-mode" "" t)
(autoload 'clojurescript-mode "clojure-mode" "" t)
(autoload 'clojurec-mode "clojure-mode" "" t)

(advice-add 'cider-eval-last-sexp :before 'kaspi/flash-last-sexp)
(advice-add 'cider-eval-defun-at-point :before 'kaspi/flash-defun)
