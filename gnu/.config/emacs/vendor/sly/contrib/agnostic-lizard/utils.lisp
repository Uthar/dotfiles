(in-package :agnostic-lizard)

(defmacro eval-with-current-environment ((var) &body code &environment env)
  "Evaluate code in a lexical environment where var is bound to the lexical
environment object corresponding to the macro call position"
  `',(funcall (eval `(lambda (,var) ,@code)) env))

(defun separate-declarations (entries &key (enable-docstring t))
  "Separate a list of entries into declarations and forms, with a possible docstring, if enabled"
  (loop
    with docstring := (not enable-docstring)
    with declarations := nil
    with forms := nil
    for header := t then header-continued
    for entry in entries
    for operator := (and (consp entry) (car entry))
    for header-continued :=
    (and
      header
      (or
        (and (stringp entry) (not docstring) (setf docstring entry) t)
        (eq operator 'declare)))
    do (if header-continued (push entry declarations) (push entry forms))
    finally
    (return
      ; (lambda () "asd") is the same as (constantly "asd")
      (if (and (null forms) (stringp docstring)
               (eq docstring (car declarations)))
        (list (reverse (cdr declarations)) (list docstring))
        (list (reverse declarations) (reverse forms))))))

(defclass function-information ()
  ((name :initarg :name :accessor function-information-name)
   (named :initarg :named :accessor function-information-named)
   (qualifiers :initarg :qualifiers :accessor function-information-qualifiers)
   (arglist :initarg :arglist :accessor function-information-arglist)
   (declarations :initarg :declarations :accessor function-information-declarations)
   (body :initarg :body :accessor function-information-body)))

(defun separate-function-definition-parts (form env)
  (let* ((name (second form))
         (qualifiers nil)
         (arglist (third form))
         (body-and-decls (cdddr form)))
    (loop while (not (listp arglist))
          do (push arglist qualifiers)
          do (setf arglist (pop body-and-decls)))
    (setf arglist (metaenv-macroexpand-lambdalist arglist env t t))
    (setf qualifiers (reverse qualifiers))
    (destructuring-bind (declarations body) (separate-declarations body-and-decls)
      (make-instance 'function-information
                     :name name
                     :named t
                     :qualifiers qualifiers
                     :arglist arglist
                     :body body
                     :declarations declarations))))

(defun separate-lambda-expression-parts (form env)
  (let* ((named
           (find (symbol-name (first form))
                 (list "named-lambda" "lambda-block") :test 'equalp))
         (form (cdr form))
         (name (when named (pop form)))
         (arglist (pop form))
         (arglist (metaenv-macroexpand-lambdalist arglist env t t)))
    (destructuring-bind (declarations body) (separate-declarations form)
      (make-instance 'function-information
                     :named named
                     :name name
                     :arglist arglist
                     :declarations declarations
                     :body body))))

(defun go-tag-p (x) (or (integerp x) (symbolp x)))

(defun lambda-list-variable-names (l)
  (loop
    for name in l
    unless (find name lambda-list-keywords)
    collect 
    (cond
      ((symbolp name) name)
      ((symbolp (first name)) (first name))
      (t (second (first name))))
    when (and (listp name) (third name)) collect (third name)))

