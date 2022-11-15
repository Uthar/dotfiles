(in-package :agnostic-lizard)

(defmacro metaenv-macro-macroexpand-all (form &environment env &optional extra-env)
  "A helper macro for recapturing environment"
  (let*
    ((envcopy (cond
                ((null extra-env) (make-instance 'macro-walker-metaenv))
                ((consp extra-env) (eval extra-env))
                (t (metaenv-clone extra-env)))))
    (setf (metaenv-create-macro envcopy) nil)
    (setf (metaenv-fallback-env envcopy) env)
    (metaenv-macroexpand-all form envcopy)))

(defmacro metaenv-macro-macroexpand-all-wrapper (form env)
  "This wrapper does nothing but protects form from metaenv-macro-walker-turn-to-quoted"
  (declare (ignorable env))
  form)

(defmethod metaenv-macroexpand-all :around (form (env macro-walker-metaenv))
  (if (metaenv-create-macro env)
    (progn
    `(metaenv-macro-macroexpand-all-wrapper
       ,(funcall
          (metaenv-on-macro-walker-create-macro env)
          `(metaenv-macro-macroexpand-all ,form ,env) env)
       ,env))
    (let* ((envcopy (metaenv-clone env)))
      (setf (metaenv-create-macro envcopy) t)
      (call-next-method form envcopy))))

(defmethod metaenv-macroexpand-1 (form (env macro-walker-metaenv))
  (macroexpand-1 form (metaenv-fallback-env env)))

(defun metaenv-macro-walker-turn-to-quoted (form env &optional cdr-branch)
  "Convert a partially walked form into code producing the expansion of the form after the walk is completed"
  (cond
    ((not (consp form)) `(quote ,form))
    ((and
       (not cdr-branch)
       (find (first form)
             '(metaenv-macro-macroexpand-all-wrapper))
       (eq (metaenv-recursive-label (ignore-errors (third form)))
           (metaenv-recursive-label env)))
     form)
    (t `(cons ,(metaenv-macro-walker-turn-to-quoted (car form) env)
              ,(metaenv-macro-walker-turn-to-quoted (cdr form) env t)))))

(defmacro macro-walk-form (form &rest handler-definitions)
  "Walk the form inside the current lexical environment using the handlers from handler-definitions.
See walk-form for details about handlers.
The resulting form after walking is returned as the macroexpansion."
  `(metaenv-macro-macroexpand-all
     ,form
     (make-instance 'macro-walker-metaenv
       ,@ handler-definitions)))

(defmacro macro-macroexpand-all (form &rest handler-definitions &key
                                      (on-every-form ''values)
                                      (on-macro-walker-create-macro ''values))
  "Produce code returning the full macroexpansion of the form in the current lexical environment.
Optionally, run handlers. :on-every-form will be run before quoting and wrapping in environment handling helpers."
  `(macro-walk-form
     ,form
     :on-every-form (lambda (form env)
                      (metaenv-macro-walker-turn-to-quoted
                        (funcall ,on-every-form
                                 form env) env))
     :on-macro-walker-create-macro
     (lambda (form env)
       (metaenv-wrap-form env
         (funcall ,on-macro-walker-create-macro form env)))
     ,@ handler-definitions))
