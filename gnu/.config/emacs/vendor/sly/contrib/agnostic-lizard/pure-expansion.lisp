(in-package :agnostic-lizard)

; In this file we pretend we only want a macroexpand-all function

(defgeneric metaenv-macroexpand-1 (form env)
            (:documentation
              "Do a single step of macroexpansion on the top level of form in the environment env"))

(defmethod metaenv-macroexpand-1 (x env)
  (cond
    ((symbolp x)
     (let*
       ((local-match
          (find x (metaenv-variable-like-entries env) :key 'first))
        (local-content (second local-match))
        (local-payload (second local-content)))
       (ecase (first local-content)
         ((:plain) x)
         ((:macro) local-payload)
         ((:macro-from) (macroexpand-1 x local-payload))
         ((nil) (macroexpand-1 x (metaenv-fallback-env env))))))
    ((consp x)
     (let*
       ((local-match
          (find (first x) (metaenv-function-like-entries env) :key 'first))
        (local-content (second local-match)))
       (ecase (first local-content)
         ((:plain) x)
         ((:macro :macro-from :macro-function-code :macro-code)
          (with-metaenv-built-env env 'lisp-env `((macroexpand-1 ',x lisp-env))))
         ((nil)
          (cond
            ; a lambda expression
            ((consp (first x)) x)
            ; not a macro
            ((null (macro-function (first x) (metaenv-fallback-env env))) x)
            ; the fallback is the global environment anyway
            ((null (metaenv-fallback-env env))
             (with-metaenv-built-env env 'lisp-env `((macroexpand-1 ',x lisp-env))))
            ; no macro defined or an flet shadows the macro
            ((null (macro-function (first x) (metaenv-fallback-env env))) x)
            ; metaenv does not contain anything meaningful
            ((metaenv-irrelevant-for-macroexpand env)
             (macroexpand-1 x (metaenv-fallback-env env)))
            ; the macro is from the global environment or defined inside
            ; the walked fragment
            ((eq (macro-function (first x) nil)
                 (macro-function (first x) (metaenv-fallback-env env)))
             (with-metaenv-built-env env 'lisp-env `((macroexpand-1 ',x lisp-env))))
            ; Policy override says not to pass fallback environment
            ; Need to pull the name into the environment used, otherwise the
            ; expansion won't happen at all
            ((metaenv-never-pass-fallback-env env)
             (let*
               ((envcopy (metaenv-clone env)))
               (metaenv-ensure-name-from-environment
                 envcopy (first x) (metaenv-fallback-env env))
               (with-metaenv-built-env envcopy 'lisp-env `((macroexpand-1 ',x lisp-env)))))
            ; The macro is from a macrolet reflected in the fallback environment
            ; Assume macroexpand calls inside the macro function expect some other
            ; outside macrolet to be in effect
            (t (macroexpand-1 x (metaenv-fallback-env env))))))))
    (t x)))

(defun metaenv-macroexpand (form env)
  "Perform full macroexpansion of the top level of the form in the environment env"
  (let*
    ((expansion (metaenv-macroexpand-1 form env)))
    (if (eq expansion form) form
      (metaenv-macroexpand expansion env))))

(defgeneric metaenv-macroexpand-all (form env)
            (:documentation
              "Expand all macros on all levels of form in the environment env"))
(defgeneric metaenv-macroexpand-all-special-form (operator form env)
            (:documentation
              "A handler for dispatching macroexpand-all handling of special forms and some macros"))

; When not passing an environment, initialize a metaenv

(defmethod metaenv-macroexpand-all (form (env (eql nil)))
  (metaenv-macroexpand-all form (make-instance 'metaenv)))

(defmethod metaenv-macroexpand-all-special-form (operator form (env (eql nil)))
  (metaenv-macroexpand-all-special-form operator form (make-instance 'metaenv)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *hardwired-operators*
    `(
      ; named-lambda expansions are complicated, better just to walk
      defun defmethod defmacro
      ; these are said to expand to something non-portable in some
      ; implementations
      cond multiple-value-bind handler-bind)
    "The list of hardwired macros; they are code-walked without macroexpansion"))

(defmethod metaenv-macroexpand-all (form (env metaenv))
  (let* ((initial-operator (and (consp form) (car form)))
         (hardwiredp (find initial-operator *hardwired-operators*))
         (expanded (if hardwiredp form (metaenv-macroexpand form env))))
    (and
      expanded
      (if (not (consp expanded)) expanded
        (let* ((operator (car expanded)))
          (if (not
                (or
                  (and
                    (symbolp operator)
                    (special-operator-p operator))
                  (find operator *hardwired-operators*)))
            (cons operator
                  (loop
                    for arg in (cdr expanded)
                    collect (metaenv-macroexpand-all arg env)))
            (metaenv-macroexpand-all-special-form operator expanded env)))))))

(defun metaenv-map-macroexpand-all (forms env)
  "Apply metaenv-macroexpand-all to all the forms using the same env"
  (loop for form in forms collect (metaenv-macroexpand-all form env)))

(defun metaenv-map-macroexpand-all-after-declarations (entries env enable-docstring)
  "Apply metaenv-map-macroexpand-all to all the forms among the entries, but use the declarations as-is"
  (let*
    ((declarations-and-forms (separate-declarations entries :enable-docstring enable-docstring)))
    (append
      (first declarations-and-forms)
      (metaenv-map-macroexpand-all (second declarations-and-forms) env))))

(defun macroexpand-all (form &optional env &key names never-pass-fallback-env)
  "Recursively expand all macro calls in form with initial environment described by env"
  (cond
    ((null env) (metaenv-macroexpand-all form nil))
    ((typep env 'metaenv) (metaenv-macroexpand-all form env))
    (t (let*
         ((metaenv
            (make-instance
              'metaenv
              :fallback-env env
              :never-pass-fallback-env never-pass-fallback-env)))
         (metaenv-ensure-names-from-environment metaenv names env)
         (metaenv-macroexpand-all form metaenv)))))
