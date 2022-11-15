(in-package :agnostic-lizard)

(defun environment-entries-for-name
  (name environment)
  "Create metaenv entries corresponding to environment for name.

  Returns a list: variable-like entry, function-like entry."
  (list
    (cond
      ((and (macroexpand-1 name environment)
            (eq
              (macroexpand-1 name environment)
              (macroexpand-1 name nil)))
       nil)
      ((macroexpand-1 name environment)
       `(,name (:macro ,(macroexpand-1 name environment))))
      ((macroexpand-1 name nil)
       `(,name (:plain)))
      (t nil))
    (cond
      ((and
         (macro-function name environment)
         (eq
           (macro-function name environment)
           (macro-function name nil)))
       nil)
      ((macro-function name environment)
       `(,name (:macro ,(macro-function name environment))))
      ((macro-function name nil)
       `(,name (:plain)))
      (t nil))))

(defun metaenv-ensure-name-from-environment
  (metaenv name environment)
  (destructuring-bind
    (ve fe)
    (environment-entries-for-name
      name environment)
    (when fe (metaenv-add-function-like-entry metaenv fe))
    (when ve (metaenv-add-variable-like-entry metaenv ve))))

(defun metaenv-ensure-names-from-environment
  (metaenv names environment)
  (loop for name in names do
        (metaenv-ensure-name-from-environment
          metaenv name environment)))
