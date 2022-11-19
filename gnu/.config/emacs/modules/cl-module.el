;; -*- lexical-binding: t -*-

(setq

 ;; Prevent screen clobber with long stack frames.
 slime-truncate-lines nil

 ;; UTF-8 is a sane default.
 slime-net-coding-system 'utf-8-unix

 ;; Default CL. Run 'slime'/'slime-connect' with 'universal-argument'
 ;; to choose a different one.
 inferior-lisp-program "sbcl"

 ;; Maintain pretty printing in repl even when the window is
 ;; resized. Otherwise things would be always printed at the width of
 ;; the repl window as it was originally created
 slime-repl-auto-right-margin t

 ;; Keep much more history, because memory is cheap.
 slime-repl-history-size 10000

 ;; TODO(kasper): unzip hyperspec in emacs user dir
 ;; common-lisp-hyperspec-root "@clhs@/HyperSpec/"
 ;; common-lisp-hyperspec-symbol-table "@clhs@/HyperSpec/Data/Map_Sym.txt"

 slime-contribs
 '(
   slime-asdf
   slime-autodoc
   slime-banner
   slime-compiler-notes-tree
   slime-editing-commands
   slime-fancy-inspector
   slime-fancy-trace
   slime-fontifying-fu
   slime-indentation
   slime-macrostep
   slime-mdot-fu
   slime-mrepl
   slime-package-fu
   slime-presentations
   slime-quicklisp
   slime-references
   slime-repl
   slime-scratch
   slime-sprof
   slime-trace-dialog
   slime-tramp
   slime-xref-browser
   )

 )

(global-set-key (kbd "C-c s") 'slime-selector)

(defun kaspi/slime-capf ()
  (let* ((end (move-marker (make-marker) (slime-symbol-end-pos)))
         (beg (move-marker (make-marker) (slime-symbol-start-pos)))
         (completion-result (slime-contextual-completions beg end))
         (completion-set (cl-first completion-result)))
    (list beg (max (point) end) completion-set)))

(with-eval-after-load 'slime
  (define-key slime-mode-map (kbd "C-c C-z") 'slime-repl)
  (define-key slime-mode-map (kbd "C-c h") 'slime-hyperspec-lookup)
  (add-to-list 'slime-completion-at-point-functions 'kaspi/slime-capf))

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
(setq sly-complete-symbol-function 'sly-simple-completions)
(with-eval-after-load 'sly
  (add-to-list 'sly-contribs 'sly-asdf)
  (add-to-list 'sly-contribs 'sly-stepper))

