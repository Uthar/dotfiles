;; -*- lexical-binding: t -*-

(setq

 ;; Prevent screen clobber with long stack frames.
 slime-truncate-lines nil

 ;; UTF-8 is a sane default.
 slime-net-coding-system 'utf-8-unix

 ;; Default CL. Run 'slime'/'slime-connect' with 'universal-argument'
 ;; to choose a different one.
 inferior-lisp-program "sbcl --disable-ldb --dynamic-space-size 4096"

 ;; Maintain pretty printing in repl even when the window is
 ;; resized. Otherwise things would be always printed at the width of
 ;; the repl window as it was originally created
 slime-repl-auto-right-margin t

 ;; Keep much more history, because memory is cheap.
 slime-repl-history-size 10000

 ;; TODO(kasper): unzip hyperspec in emacs user dir
 ;; common-lisp-hyperspec-root "@clhs@/HyperSpec/"
 ;; common-lisp-hyperspec-symbol-table "@clhs@/HyperSpec/Data/Map_Sym.txt"

 slime-contribs '(slime-asdf slime-quicklisp slime-fancy)

 )

(global-set-key (kbd "C-c s") 'slime-selector)

(with-eval-after-load 'slime
  (define-key slime-mode-map (kbd "C-c C-z") 'slime-repl)
  (define-key slime-mode-map (kbd "C-c h") 'slime-hyperspec-lookup))

;; Disable annoying tab completion buffers. (I prefer to explicitly C-M-I)
;; Careful: both 'slime-repl' and 'inferior-slime' set this.
;; With M-x 'slime' this is enough because only 'slime-repl' is loaded.
;; Probably wouldn't work if using comint (but who would want to?).
(add-hook 'slime-repl-mode-hook (lambda () (setq-local tab-always-indent t)))

(with-eval-after-load 'slime-repl
  (defslime-repl-shortcut nil ("delete-package" "dp")
    (:handler (lambda ()
                (interactive)
                (let ((package (slime-read-package-name "Package: ")))
                  (slime-repl-shortcut-eval `(cl:delete-package ,package)))))
    (:one-liner "Delete a package.")))

;; Inspect presentations in repl with a mouse click.
(with-eval-after-load 'slime-presentations
  (define-key
    slime-presentation-map
    [mouse-1]
    'slime-inspect-presentation-at-mouse))

(with-eval-after-load 'slime-cl-indent
  (define-common-lisp-style "kasper"
    "Personal style."
    (:inherit "modern")
    (:indentation))
  (setq common-lisp-style-default "kasper"))

(add-to-list 'load-path (concat +vendor-dir+ "slime"))
(autoload 'slime "slime" "" t)
(autoload 'slime-connect "slime" "" t)
(autoload 'slime-selector "slime" "" t)

(add-to-list 'load-path (concat +vendor-dir+ "sly"))
(autoload 'sly "sly" "" t)
(setq sly-symbol-completion-mode nil)

