(defpackage :agnostic-lizard-debugger-hooks
  (:use :common-lisp :agnostic-lizard)
  (:export #:debugger-hooks-state
           #:debugger-hooks-active
           #:debugger-hooks-callback
           #:debugger-hooks-command
           #:debugger-hooks-alias
           #:debugger-hooks-assign
           #:*debugger-hooks-state*
           #:*local-debugger-hooks*
           #:with-debugger-hook-calls
           #:with-local-debugger-hook
           #:set-local-debugger-hook))

(in-package :agnostic-lizard-debugger-hooks)

(defclass debugger-hooks-state ()
  ((debugger-hooks-active :initarg :active :initform nil
                          :accessor debugger-hooks-active)
   (debugger-hooks-callback :initarg :callback :initform nil
                            :accessor debugger-hooks-callback)
   (debugger-hooks-alias :initarg :alias :initform nil
                         :accessor debugger-hooks-alias)
   (debugger-hooks-command :initarg :command :initform nil
                           :accessor debugger-hooks-command)))

(defvar *debugger-hooks-state* (make-instance 'debugger-hooks-state))

(defgeneric debugger-hooks-assign (object &key))

(defmethod debugger-hooks-assign ((object debugger-hooks-state)
                                  &key
                                  (active nil activep)
                                  (callback nil callbackp)
                                  (command nil commandp))
  (when activep (setf (debugger-hooks-active object) active))
  (when callbackp (setf (debugger-hooks-callback object) callback))
  (when commandp (setf (debugger-hooks-command object) command)))

(defclass debugger-hook-walker-env (walker-metaenv)
  ((lexenvp :initarg :lexenvp :initform t
            :accessor debugger-hook-walker-lexenv-p)
   (result-var :accessor debugger-hook-walker-result-var
               :initarg :result-var :initform (gensym))
   (function-marker :initarg :function-marker :initform nil
                    :accessor debugger-hook-walker-function-marker)
   (pending-variables :initarg :pending-variables :initform nil
                      :accessor debugger-hook-walker-pending-variables)
   (pending-functions :initarg :pending-functions :initform nil
                      :accessor debugger-hook-walker-pending-functions)))

(defmethod metaenv-clone-args ((env debugger-hook-walker-env))
  (append (call-next-method)
          (list :function-marker (debugger-hook-walker-function-marker env)
                :lexenvp (debugger-hook-walker-lexenv-p env)
                :result-var (debugger-hook-walker-result-var env)
                :pending-variables (debugger-hook-walker-pending-variables env)
                :pending-functions (debugger-hook-walker-pending-functions env))))

(defmethod metaenv-clone ((obj debugger-hook-walker-env)
                          &optional overrides)
  (apply
    'make-instance 'debugger-hook-walker-env
    (append
      overrides
      (metaenv-clone-args obj))))

(defmethod print-object ((object debugger-hook-walker-env) stream)
  (format stream "#<debugger-hook-walker-env>"))

(defmethod agnostic-lizard::metaenv-add-function-like-entry
  ((obj debugger-hook-walker-env) (entry t))
  (push entry (debugger-hook-walker-pending-functions obj))
  (call-next-method))

(defmethod agnostic-lizard::metaenv-add-variable-like-entry
  ((obj debugger-hook-walker-env) (entry t))
  (push entry (debugger-hook-walker-pending-variables obj))
  (call-next-method))

(defmethod agnostic-lizard::metaenv-wrap-form ((obj debugger-hook-walker-env)
                                               form)
  (let* ((form form)
         (form (agnostic-lizard::wrap-function-like-env
                 (metaenv-function-like-entries obj) form))
         (form (agnostic-lizard::wrap-variable-like-env
                 (metaenv-variable-like-entries obj) form)))
    form))

(defun default-debugger-hook-callback (&key result) result)

(defun debugger-hook-call (&key form env position result marker function
                                expanded-form)
  (let ((res (debugger-hook-walker-result-var env)))
    `(let ((,res ,result))
       (funcall (or (and (debugger-hooks-active *debugger-hooks-state*)
                         (debugger-hooks-callback *debugger-hooks-state*))
                    #'default-debugger-hook-callback)
                :allow-other-keys t
                :form ',form
                :expanded-form ',expanded-form
                :function ',function
                :environment ',env
                :position ,position
                :result ,res
                :marker ,marker
                :function-marker
                ,(debugger-hook-walker-function-marker env)
                :variables
                ',(loop for v in (when (debugger-hook-walker-lexenv-p env)
                                   (debugger-hook-walker-pending-variables env))
                        when (equal (second v) '(:plain))
                        collect `(,(first v) .
                                             (lambda (&optional (value
                                                                  nil valuep))
                                               (if valuep
                                                 (setf ,(first v) value)
                                                 ,(first v)))))
                :functions
                ',(loop for f in (when (debugger-hook-walker-lexenv-p env)
                                   (debugger-hook-walker-pending-functions env))
                        when (equal (second f) '(:plain))
                        collect `(',(first f) . #',(first f)))))))

(defvar *marker-counter* 0)

(defmethod agnostic-lizard::metaenv-macroexpand-all-special-form-defun
  (operator form (env debugger-hook-walker-env))
  (let* ((envcopy (metaenv-clone env (list :toplevel nil 
                                           :function-marker
                                           (incf *marker-counter*))))
         (function-info (separate-function-definition-parts
                          form envcopy)))
    (when (function-information-named function-info)
      (agnostic-lizard::metaenv-add-block
        envcopy (function-information-name function-info)))
    `(,operator
       ,(function-information-name function-info)
       ,@(function-information-qualifiers function-info)
       ,(function-information-arglist function-info)
       ,@(function-information-declarations function-info)
       (if (debugger-hooks-active *debugger-hooks-state*)
         (progn
           ,(debugger-hook-call
              :env envcopy
              :position :before-function
              :function (function-information-name
                          function-info))
           ,(debugger-hook-call
              :env envcopy :position :after-function
              :function (function-information-name function-info)
              :result
              `(progn
                 ,@(loop for f in (function-information-body
                                    function-info)
                         collect (metaenv-macroexpand-all
                                   f
                                   (metaenv-clone
                                     envcopy
                                     (list :pending-variables nil
                                           :pending-functions nil)))))))
         (progn ,@(function-information-body function-info))))))

(defmethod agnostic-lizard::metaenv-expand-lambda-expression
  (designator (env debugger-hook-walker-env))
  (let* ((envcopy (metaenv-clone env (list :toplevel nil 
                                           :function-marker
                                           (incf *marker-counter*))))
         (function-info
           (separate-lambda-expression-parts designator envcopy)))
    (when (function-information-named function-info)
      (agnostic-lizard::metaenv-add-block
        envcopy (function-information-name function-info)))
    `(,(first designator)
       ,@(when (function-information-named function-info)
           (list (function-information-name function-info)))
       ,(function-information-arglist function-info)
       ,@(function-information-declarations function-info)
       (if (debugger-hooks-active *debugger-hooks-state*)
         (progn
           ,(debugger-hook-call
              :env envcopy
              :position :before-function
              :function (function-information-name
                          function-info))
           ,(debugger-hook-call
              :env envcopy :position :after-function
              :function (function-information-name function-info)
              :result
              `(progn
                 ,@(loop for f in (function-information-body
                                    function-info)
                         collect (metaenv-macroexpand-all
                                   f (metaenv-clone
                                       envcopy
                                       (list :pending-variables nil
                                             :pending-functions nil)))))))
         (progn ,@(function-information-body function-info))))))

(defun debugger-hook-walker-replace-form (form env expanded-form)
  (let* ((op (and (listp form) (first form)))
         (tl (metaenv-toplevel env)))
    (cond ((and tl (find op '(eval-when progn locally
                               symbol-macrolet macrolet
                               defmethod defun defmacro
                               defclass defgeneric)))
           (let* ((marker (incf *marker-counter*)))
             `(progn 
                ,(debugger-hook-call :form form :env env
                                     :expanded-form expanded-form
                                     :result nil :marker marker
                                     :position :before)
                ,expanded-form
                ,(debugger-hook-call :form form :env env
                                     :expanded-form expanded-form
                                     :result nil :marker marker
                                     :position :after)
                nil)))
          ((find op '(throw go return-from))
           (let* ((marker (incf *marker-counter*)))
             `(progn
                ,(debugger-hook-call :form form :env env
                                     :expanded-form expanded-form
                                     :position :before-control-transfer
                                     :result nil :marker marker)
                ,expanded-form)))
          (t (let* ((marker (incf *marker-counter*)))
               `(progn
                  ,(debugger-hook-call :form form :env env
                                       :expanded-form expanded-form
                                       :result nil :marker marker
                                       :position :before)
                    ,(debugger-hook-call :form form :env env
                                         :expanded-form expanded-form
                                         :result expanded-form :marker marker
                                         :position :after)))))))

(defmethod metaenv-macroexpand-all (form (env debugger-hook-walker-env))
  (let* ((expanded-form (call-next-method
                          form (metaenv-clone
                                 env (list :pending-variables nil
                                           :pending-functions nil))))
         (wrapped-form (debugger-hook-walker-replace-form
                         form env expanded-form)))
    (if (and (consp expanded-form)
             (not (eq 'quote (first expanded-form))))
      wrapped-form expanded-form)))

(defmacro with-debugger-hook-calls (top-level-form &key (lexenvp t))
  (metaenv-macroexpand-all
    top-level-form
    (make-instance
      'debugger-hook-walker-env
      :toplevel t :lexenvp lexenvp)))

(defvar *local-debugger-hooks* (make-hash-table :test 'equal))

(defmacro with-local-debugger-hook
  ((name &key (active '(debugger-hooks-active *debugger-hooks-state*))
         (callback '(debugger-hooks-callback *debugger-hooks-state*))
         (class 'debugger-hooks-state))
   &body body)
  `(let* ((*debugger-hooks-state* (make-instance ',class
                                                 :alias ,name
                                                 :callback ,callback
                                                 :active ,active)))
     (setf (gethash ,name *local-debugger-hooks*) *debugger-hooks-state*)
     ,@body))

(defun set-local-debugger-hook (name &rest args)
  (apply 'debugger-hooks-assign
         (gethash name *local-debugger-hooks*) args))

#+nil
(defun demo-debugger-hooks ()
  (with-local-debugger-hook
    ('demo-debugger-hooks
     :active t
     :callback (lambda (&key form marker function-marker function
                             result position environment)
                 (format
                   t "~s~%"
                   (list :trace
                         form position
                         (or marker function-marker)
                         function
                         :res result
                         :env
                         (agnostic-lizard::metaenv-function-like-entries
                           environment)
                         (agnostic-lizard::metaenv-variable-like-entries
                           environment)))
                 result))
    (with-debugger-hook-calls
      (defun test-debugger-hooks () (+ 1 2) (* 7 8)))
    (with-debugger-hook-calls
      (test-debugger-hooks))
    (with-debugger-hook-calls (+ 1 (* 3 4)))
    (format t "Now without debugger hooks~%")
    (set-local-debugger-hook 'demo-debugger-hooks :active nil)
    (with-debugger-hook-calls (+ 1 (* 3 4)))
    (with-debugger-hook-calls (test-debugger-hooks))
    (with-debugger-hook-calls
      (defun test-debugger-hooks-2 () (+ 9 7) (* 7 8)))
    (with-debugger-hook-calls (test-debugger-hooks-2))
    (format t "Now with debugger hooks again~%")
    (set-local-debugger-hook 'demo-debugger-hooks :active t)
    (with-debugger-hook-calls (+ 1 2))
    (with-debugger-hook-calls (test-debugger-hooks-2))))
