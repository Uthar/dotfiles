(in-package :agnostic-lizard)

(defmethod metaenv-macroexpand-all (form (env walker-metaenv))
  (let*
    ((replacement (funcall (metaenv-on-every-form-pre env) form env))
     (hardwiring-needed-p
       (and (consp replacement)
            (find (first replacement) *hardwired-operators*)))
     (expanded-raw (if hardwiring-needed-p replacement
                     (metaenv-macroexpand replacement env)))
     (expanded (funcall (metaenv-on-macroexpanded-form env) expanded-raw env))
     (function-like-p (and expanded (consp expanded)))
     (operator (and function-like-p (first expanded)))
     ; If the operator has a dual macro/special-operator implementation,
     ; it is already macroexpanded by that point.
     (specialp (and (symbolp operator) (special-operator-p operator)))
     (function-replacement
       (if (or specialp hardwiring-needed-p)
         (funcall (metaenv-on-special-form-pre env) expanded env)
         (funcall (metaenv-on-function-form-pre env) expanded env)))
     (full-expansion
       (cond
         ; Our analysis of the expression type is no longer valid
         ((not (eq function-replacement expanded))
          (metaenv-macroexpand-all function-replacement env))
         ((not function-like-p) expanded)
         (specialp (metaenv-macroexpand-all-special-form
                     operator function-replacement env))
         ; Default handler works fine for both progn and function call forms,
         ; we define the same handler once more for progn just for the sake of
         ; explicitness
         (t (metaenv-macroexpand-all-special-form
              operator function-replacement env))))
     (full-expansion-replacement
       (cond
         ((and function-like-p (not specialp) (not hardwiring-needed-p))
          (funcall (metaenv-on-function-form env) full-expansion env))
         (function-like-p
           (funcall (metaenv-on-special-form env) full-expansion env))
         (t (funcall (metaenv-on-every-atom env) full-expansion env))))
     (result (funcall (metaenv-on-every-form env) full-expansion-replacement env)))
    result))

(defun walk-form (form env &rest handler-definitions)
  "Walk the form inside the environment described by env using the handlers from handler-definitions.
Handlers get a form and a walker-metaenv environment description.
The return value of a handler is used instead of the form passed to the handler during further processing.

Handlers can be:
:on-every-form-pre — called before processing each form in an executable position
:on-macroexpanded-form — called for each form after macroexpanding its top operation; hardwired macros are passed unexpanded
:on-special-form-pre — called before processing a special form or a hardwired macro
:on-function-form-pre — called before processing a function call
:on-special-form — called after processing a special form or a hardwired macro
:on-every-atom — called after processing a form expanded to an atom
:on-function-form — called after processing a function call
:on-every-form — called after expanding each form in an executable position

env can be metaenv or walker-metaenv
"
  (metaenv-macroexpand-all
    form
    (apply
      'make-instance
      (if (typep env 'walker-metaenv)
        (class-of env)
        'walker-metaenv)
      (append
        handler-definitions
        (when (typep env 'metaenv)
          (metaenv-clone-args env))))))
