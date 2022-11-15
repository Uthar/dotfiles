(in-package :agnostic-lizard)

; basic metaenv class interface and NOP default implementations

(defgeneric metaenv-function-like-entries (obj)
            (:documentation
              "Query the list of function-like entries from a metaenv object"))
(defgeneric metaenv-variable-like-entries (obj)
            (:documentation
              "Query the list of variable-like entries from a metaenv object"))
(defgeneric metaenv-blocks (obj)
            (:documentation
              "Query the list of defined blocks from a metaenv object"))
(defgeneric metaenv-tags (obj)
            (:documentation
              "Query the list of defined tags from a metaenv object"))
(defgeneric metaenv-toplevel (obj)
            (:documentation
              "Query if a metaenv object represents toplevel environment"))
(defgeneric metaenv-fallback-env (obj)
            (:documentation
              "Query the fallback environment object from a metaenv object"))
(defgeneric metaenv-never-pass-fallback-env (obj)
            (:documentation
              "Query whether the fallback environment object should never be passed to macroexpand-1"))

(defgeneric metaenv-add-function-like-entry (obj entry)
            (:documentation
              "Add a function-like entry into the metaenv object.
The entry should follow the format described in wrap-function-like-env.
The entry will shadow previously available entries with the same name."))
(defgeneric metaenv-add-variable-like-entry (obj entry)
            (:documentation
              "Add a variable-like entry into the metaenv object.
The entry should follow the format described in wrap-variable-like-env.
The entry will shadow previously available entries with the same name." ))
(defgeneric metaenv-add-block (obj name)
            (:documentation
              "Add a block into the metaenv object."))
(defgeneric metaenv-add-tag (obj name)
            (:documentation
              "Add a tag into the metaenv object."))

(defgeneric metaenv-clone-args (obj)
            (:documentation
              "The needed initargs for copying a metaenv-related object"))
(defgeneric metaenv-clone (obj &optional overrides)
            (:documentation
              "Create a copy of a metaenv-related object"))

(defmethod metaenv-function-like-entries ((obj t)))
(defmethod metaenv-variable-like-entries ((obj t)))
(defmethod metaenv-blocks ((obj t)))
(defmethod metaenv-tags ((obj t)))
(defmethod metaenv-toplevel ((obj t)))
(defmethod metaenv-fallback-env ((obj t)))
(defmethod metaenv-never-pass-fallback-env ((obj t)))

(defmethod metaenv-add-function-like-entry ((obj t) (entry t)))
(defmethod metaenv-add-variable-like-entry ((obj t) (entry t)))
(defmethod metaenv-add-block ((obj t) (name t)))
(defmethod metaenv-add-tag ((obj t) (name t)))

; The interface is enough to define building a similar environment

(defgeneric metaenv-wrap-form (obj form)
   (:documentation 
     "Wrap the form into environment-setting wrappers corresponding to all the entries of obj."))

(defun with-metaenv-built-env (obj var code)
  "Evaluate code in the lexical environment where var is bound to the lexical environment object described by obj"
  (eval
    (metaenv-wrap-form
      obj
      `(eval-with-current-environment
         (,var) ,@code))))

; The basic metaenv class and methods

(defclass metaenv ()
  ((function-like
     :initarg :function-like :initarg :func :initarg :fun :initarg :fn
     :initform nil :accessor metaenv-function-like-entries)
   (variable-like :initarg :variable-like :initarg :var :initform nil
                  :accessor metaenv-variable-like-entries)
   (blocks :initarg :blocks :initarg :block :initform nil
           :accessor metaenv-blocks)
   (tags :initarg :tags :initarg :tag :initform nil :accessor metaenv-tags)
   (toplevel :initarg :toplevel :initform nil :accessor metaenv-toplevel)
   (fallback-env
     :initarg :fallback-env :initarg :env :initform nil
     :accessor metaenv-fallback-env
     :documentation "A slot for storing environment obtained via an &environment parameter")
   (never-pass-fallback-env
     :initarg :never-pass-fallback-env :initform nil
     :accessor metaenv-never-pass-fallback-env
     :documentation "A slot storing the policy switch for never passing the fallback env to macroexpand-1"))
  (:documentation "The basic object holding the lexical environment data for macroexpansion"))

(defmethod metaenv-add-function-like-entry ((obj metaenv) (entry t))
  (push entry (metaenv-function-like-entries obj)))
(defmethod metaenv-add-variable-like-entry ((obj metaenv) (entry t))
  (push entry (metaenv-variable-like-entries obj)))
(defmethod metaenv-add-block ((obj metaenv) (name t))
  (push name (metaenv-blocks obj)))
(defmethod metaenv-add-tag ((obj metaenv) (name t))
  (pushnew name (metaenv-tags obj)))

(defmethod metaenv-clone-args ((obj metaenv))
  (list
    :function-like (metaenv-function-like-entries obj)
    :variable-like (metaenv-variable-like-entries obj)
    :blocks (metaenv-blocks obj)
    :tags (metaenv-tags obj)
    :toplevel (metaenv-toplevel obj)
    :fallback-env (metaenv-fallback-env obj)
    :never-pass-fallback-env (metaenv-never-pass-fallback-env obj)))

(defmethod metaenv-clone ((obj metaenv) &optional overrides)
  (apply
    'make-instance (class-of obj)
    (append
      overrides
      (metaenv-clone-args obj))))

; A helper for output
(defmethod print-object ((obj metaenv) stream)
  (format stream "#<~a:~{ ~S~}>"
          (type-of obj)
          (loop
            for arg in (metaenv-clone-args obj)
            collect
            (cond
              ((keywordp arg) arg)
              ((symbolp arg) `',arg)
              ((consp arg) `',arg)
              (t arg)))))

; A useful helper for expansions
(defun metaenv-irrelevant-for-macroexpand (obj)
  "Check if the metaenv obj is the same as the fallback environment for macroexpand-1"
  (and
    (loop
      with fbe := (metaenv-fallback-env obj)
      with seen := (make-hash-table)
      for fn in (metaenv-function-like-entries obj)
      when (second fn)
      unless (or
               (gethash (first fn) seen)
               (and
                 (eq (first (second fn)) :plain)
                 (null (macro-function (first fn) fbe))))
      return nil
      do (setf (gethash (first fn) seen) t)
      finally (return t))
    (loop
      with fbe := (metaenv-fallback-env obj)
      with seen := (make-hash-table)
      for fn in (metaenv-variable-like-entries obj)
      when (second fn)
      unless (or
               (gethash (first fn) seen)
               (and
                 (eq (first (second fn)) :plain)
                 (eq (first fn) (macroexpand-1 (first fn) fbe))))
      return nil
      do (setf (gethash (first fn) seen) t)
      finally (return t))))

(defmethod metaenv-wrap-form ((obj metaenv) form)
  (let*
    ((form form)
     (form
       (wrap-function-like-env
         (metaenv-function-like-entries obj) form))
     (form
       (wrap-variable-like-env
         (metaenv-variable-like-entries obj) form))
     (form (wrap-block-env (metaenv-blocks obj) form))
     (form (wrap-tag-env (metaenv-tags obj) form)))
    form))

(defclass walker-metaenv (metaenv)
  ((on-function-form :initform #'values :initarg :on-function-form
                     :accessor metaenv-on-function-form)
   (on-function-form-pre
     :initform #'values :initarg :on-function-form-pre
     :accessor metaenv-on-function-form-pre)
   (on-every-atom
     :initform #'values :initarg :on-every-atom
     :accessor metaenv-on-every-atom)
   (on-every-form :initform #'values :initarg :on-every-form
                  :accessor metaenv-on-every-form)
   (on-every-form-pre :initform #'values :initarg :on-every-form-pre
                  :accessor metaenv-on-every-form-pre)
   (on-special-form-pre :initform #'values :initarg :on-special-form-pre
                     :accessor metaenv-on-special-form-pre)
   (on-special-form :initform #'values :initarg :on-special-form
                     :accessor metaenv-on-special-form)
   (on-macroexpanded-form :initform #'values :initarg :on-macroexpanded-form
                     :accessor metaenv-on-macroexpanded-form))
  (:documentation "An extended walker environment object.
Here we keep a few handlers to allow transformations of the code during walking."))

(defmethod metaenv-clone-args :around ((obj walker-metaenv))
  (append
    (call-next-method)
    (list
      :on-function-form (metaenv-on-function-form obj)
      :on-function-form-pre (metaenv-on-function-form-pre obj)
      :on-special-form (metaenv-on-special-form obj)
      :on-special-form-pre (metaenv-on-special-form-pre obj)
      :on-every-atom (metaenv-on-every-atom obj)
      :on-every-form (metaenv-on-every-form obj)
      :on-every-form-pre (metaenv-on-every-form-pre obj)
      :on-macroexpanded-form (metaenv-on-macroexpanded-form obj))))

(defclass macro-walker-metaenv (walker-metaenv)
  ((create-macro
     :initform t :initarg :create-macro
     :accessor metaenv-create-macro
     :documentation "Whether to do expansion or just create a macro call that will be able to get proper environment")
   (recursive-label
     :initform (gensym) :initarg :recursive-label
     :accessor metaenv-recursive-label
     :documentation
     "A label to distinguish the forms freshly created in relation to the current environment.")
   (on-macro-walker-create-macro
     :initform 'values :initarg :on-macro-walker-create-macro
     :accessor metaenv-on-macro-walker-create-macro
     :documentation
     "A handler invoked when macro-walker creates a new macro"))
  (:documentation "A version of walker-metaenv that can store additional information for expanding to a form with macro calls.
This approach allows to use Common Lisp implementation's own handling of environment.
We still keep track of environment just for portable access to the list of discovered lexical environment entries."))

(defmethod metaenv-clone-args :around ((obj macro-walker-metaenv))
  (append
    (call-next-method)
    (list
      :create-macro (metaenv-create-macro obj)
      :recursive-label (metaenv-recursive-label obj)
      :on-macro-walker-create-macro (metaenv-on-macro-walker-create-macro obj))))

(defmethod metaenv-recursive-label (obj) nil)

