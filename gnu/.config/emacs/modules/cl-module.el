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
   slime-fancy
   slime-banner
   slime-compiler-notes-tree
   slime-mrepl
   slime-sprof
   slime-xref-browser
   )

 )

(add-to-list 'load-path (concat +vendor-dir+ "slime/contrib"))
(autoload 'inferior-slime-mode "inferior-slime")
(add-hook 'inferior-lisp-mode-hook 'inferior-slime-mode)

(defun kaspi/slime-capf ()
  (let* ((endpos (slime-symbol-end-pos))
         (begpos (slime-symbol-start-pos))
         (end (move-marker (make-marker) endpos))
         (beg (move-marker (make-marker) begpos)))
    (list begpos endpos
      (completion-table-dynamic
       (lambda (&rest _)
         (cl-first (slime-contextual-completions beg end)))))))
          
(with-eval-after-load 'slime
  (define-key slime-mode-map (kbd "C-c C-z") 'slime-repl)
  (define-key slime-mode-map (kbd "C-c h") 'slime-hyperspec-lookup)
  (define-key slime-inspector-mode-map [mouse-8] 'slime-inspector-pop)
  )

(advice-add 'slime-flash-region :override 'kaspi/noop)
(advice-add 'slime-eval-defun :before 'kaspi/flash-defun)
(advice-add 'slime-compile-defun :before 'kaspi/flash-defun)
(advice-add 'slime-eval-last-expression :before 'kaspi/flash-last-sexp)

(with-eval-after-load 'slime-repl
  (defslime-repl-shortcut nil ("delete-package" "dp")
    (:handler (lambda ()
                (interactive)
                (let ((package (slime-read-package-name "Package: ")))
                  (slime-repl-shortcut-eval `(cl:delete-package ,package)))))
    (:one-liner "Delete a package.")))

(add-hook 'slime-repl-mode-hook
 (lambda ()
   ;; Prevent prompt from being too close to the botttom of the window
   ;; (slime sets this to 0)
   (setq-local scroll-margin 1)
   (setq-local slime-completion-at-point-functions '(slime-filename-completion kaspi/slime-capf))
   ;; Don't want completions like this
   (setq-local tab-always-indent t)
   ;; Parse only code after the current prompt for TAB
   ;; indentation. This prevents indenting against printed parens from
   ;; previous repl results. It's achieved by providing a starting
   ;; point to parse from to calculate-lisp-indent so that it does not
   ;; try to guess it itself.
   (setq-local indent-line-function (lambda (&rest _) (lisp-indent-line (calculate-lisp-indent slime-repl-input-start-mark))))
   ;; Same but for indent-region
   (setq-local indent-region-function 'indent-region-line-by-line)))

;; (with-eval-after-load 'slime-repl
;;   (add-hook 'slime-repl-return-hooks (lambda (_) (slime-repl-save-merged-history) nil)))

(add-hook 'slime-mode-hook
 (lambda ()
   (setq-local slime-completion-at-point-functions '(slime-filename-completion kaspi/slime-capf))))

;; Inspect presentations in repl with a mouse click.
(with-eval-after-load 'slime-presentations
  (define-key slime-presentation-map [mouse-1] 'slime-inspect-presentation-at-mouse))

(add-to-list 'load-path (concat +vendor-dir+ "slime"))
(autoload 'slime-lisp-mode-hook "slime")
(autoload 'slime "slime" "" t)
(autoload 'slime-connect "slime" "" t)
(add-hook 'lisp-mode-hook 'slime-lisp-mode-hook)
