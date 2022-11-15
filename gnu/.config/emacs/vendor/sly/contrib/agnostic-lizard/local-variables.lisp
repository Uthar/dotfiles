(defpackage
  :agnostic-lizard-lexenv-wrapper
  (:use :agnostic-lizard :common-lisp)
  (:export
    #:pry
    #:with-saved-lexenvs
    #:list-locvars
    #:list-locfuncs
    #:locvar
    #:locfunc
    #:lexenv-cursor))
(in-package :agnostic-lizard-lexenv-wrapper)

(defvar *saved-lexenvs* nil
  "A stack of saved lexical environments")
(defvar *saved-lexenv-cursor* 0
  "The current position on the lexenv stack")

(defun pry (&key reason)
  "Launch an interactive debugging session"
  (cerror "Exit PRY session and continue execution"
          (or reason "Lightweight PRY session invoked")))

(defmacro with-saved-lexenvs (form)
  "Walk the form wrapping each expression into saving of the lexical environment"
  `(macro-walk-form
     ,form
     :on-every-form
     (lambda (f e)
       `(let* ((*saved-lexenvs*
                 (cons
                   (list
                     :functions
                      (list
                        ,@
                        (loop
                          for f in (metaenv-function-like-entries e)
                          when (equal (second f) '(:plain))
                          collect `',(first f)
                          when (equal (second f) '(:plain))
                          collect `(function ,(first f))))
                     :variables
                     (list
                       ,@
                       (loop
                        for v in (metaenv-variable-like-entries e)
                        when (equal (second v) '(:plain))
                        collect `',(first v)
                        when (equal (second v) '(:plain))
                        collect
                        `(lambda (&optional (x nil xp))
                           (if xp (setf ,(first v) x) ,(first v))))))
                   *saved-lexenvs*)))
          ,f))))

(defun lexenv-cursor (&optional n rel)
  (if n
    (progn
      (if rel (incf *saved-lexenv-cursor* n) (setf *saved-lexenv-cursor* n))
      (setf *saved-lexenv-cursor* (max 0 *saved-lexenv-cursor*))
      (setf *saved-lexenv-cursor*
            (min (1- (length *saved-lexenvs*)) *saved-lexenv-cursor*)))
    *saved-lexenv-cursor*))

(defun list-locvars (&optional (n *saved-lexenv-cursor*))
  (loop
    with varlist := (getf (elt *saved-lexenvs* n) :variables)
    while varlist
    collect (first varlist)
    do (setf varlist (cddr varlist))))

(defun list-locfuncs (&optional (n *saved-lexenv-cursor*))
  (loop
    with varlist := (getf (elt *saved-lexenvs* n) :functions)
    while varlist
    collect (first varlist)
    do (setf varlist (cddr varlist))))

(defmacro locvar (name &optional (value nil valuep))
  `(let* ((fn (getf (getf (elt *saved-lexenvs* *saved-lexenv-cursor*)
                          :variables) ',name)))
     ,(if valuep `(funcall fn ,value) `(funcall fn))))

(defmacro locfunc (name &rest args)
  `(let* ((fn (getf (getf (elt *saved-lexenvs* *saved-lexenv-cursor*)
                          :functions) ',name)))
     (funcall fn ,@ args)))
