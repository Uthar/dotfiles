(in-package :agnostic-lizard)

(defmethod metaenv-macroexpand-all-special-form (operator form env)
  (metaenv-macroexpand-all-special-form-progn operator form env))

(defmethod metaenv-macroexpand-all-special-form ((operator (eql 'block)) form env)
  (let ((envcopy (metaenv-clone env)))
    (metaenv-add-block envcopy (second form))
    (setf (metaenv-toplevel envcopy) nil)
    `(block ,(second form)
            ,@(metaenv-map-macroexpand-all (cddr form) envcopy))))

(defgeneric metaenv-macroexpand-lambdalist (entries env self-visible wait-for-keyword)
            (:documentation
              "Macroexpand assignments/default values in entries and destructively add the names to env"))

(defmethod metaenv-macroexpand-lambdalist
  ((entries list) env self-visible wait-for-keyword)
  (let* ((variables nil))
    (flet ((remember-variable (name)
                              (if self-visible
                                (metaenv-add-variable-like-entry
                                  env `(,name (:plain)))
                                (push name variables))))
      ; possibly add variables after all the expansions
      (prog1
        (loop with can-expand-second := (not wait-for-keyword)
              for e in entries
              collect
              (cond
                ((find e lambda-list-keywords) (setf can-expand-second t) e)
                ((symbolp e) (remember-variable e) e)
                ((not (listp e))
                 (error
                   "Lambda/let list entries should be symbols or lists"))
                ; It must be a specialiser
                ((not can-expand-second) (remember-variable (first e)) e)
                ((= (length e) 3)
                 ; expand before changing the environment
                 (prog1
                   (list (first e)
                         (metaenv-macroexpand-all (second e) env)
                         (third e))
                   (remember-variable
                     (if (listp (first e))
                       ; keyword argument override
                       (second (first e))
                       (first e)))
                   (remember-variable (third e))))
                ((<= 1 (length e) 2)
                 ; expand before changing the environment
                 (prog1
                   (cons (first e) (and (cdr e) (list (metaenv-macroexpand-all (second e) env))))
                   (remember-variable
                     (if (listp (first e))
                       ; keyword argument override
                       (second (first e))
                       (first e)))))
                (t
                 (error "Bad lambda list element - ~S" e))))
        (unless self-visible
          (loop for name in variables do
                (metaenv-add-variable-like-entry env `(,name (:plain)))))))))

(defgeneric metaenv-macroexpand-all-special-form-let (operator form env)
            (:documentation
              "Expand a form that is similar to let"))

(defmethod metaenv-macroexpand-all-special-form-let (operator form env)
  (let*
    ((envcopy (metaenv-clone env (list :toplevel nil)))
     (definitions
       (metaenv-macroexpand-lambdalist (second form) envcopy
                                       (not (eq operator 'let)) nil)))
    `(,operator
       ,definitions
       ,@ (metaenv-map-macroexpand-all-after-declarations (cddr form) envcopy nil))))

(defmethod metaenv-macroexpand-all-special-form ((operator (eql 'let)) form env)
  (metaenv-macroexpand-all-special-form-let operator form env))
(defmethod metaenv-macroexpand-all-special-form ((operator (eql 'let*)) form env)
  (metaenv-macroexpand-all-special-form-let operator form env))

(defmethod metaenv-macroexpand-all-special-form ((operator (eql 'setq)) form env)
  (let*
    ((envcopy (metaenv-clone env (list :toplevel nil)))
     (pairs (loop
              with source := (cdr form)
              while source
              collect (list (first source) (second source))
              do (setf source (cddr source))))
     (expanded-names
       (loop
         for pair in pairs
         collect (metaenv-macroexpand (first pair) envcopy)))
     ; symbol macros among target variable names make setq behave like setf
     ; on the other hand, setf to a non-symbol-macro variable name is the same as setq
     (conversion-needed (remove-if 'atom expanded-names))
     (assignment-arguments
       (loop
         for var in expanded-names
         for pair in pairs
         for value := (second pair)
         collect (list var value))))
    (if conversion-needed
      (metaenv-macroexpand-all
        `(setf ,@ (reduce 'append assignment-arguments))
        envcopy)
      `(setq ,@ (loop
                  for pair in assignment-arguments
                  collect (first pair)
                  collect (metaenv-macroexpand-all (second pair) envcopy))))))

(defmethod metaenv-macroexpand-all-special-form ((operator (eql 'tagbody)) form env)
  (let*
    ((envcopy (metaenv-clone env (list :toplevel nil))))
    (loop
      for entry in (cdr form)
      when (go-tag-p entry)
      do (metaenv-add-tag envcopy entry))
    `(tagbody
       ,@(loop
           for entry in (cdr form)
           for tagp := (go-tag-p entry)
           for expansion :=
           (and (not tagp)
                (metaenv-macroexpand-all entry envcopy))
           for final-entry := (if tagp entry expansion)
           collect final-entry))))

(defgeneric metaenv-expand-lambda-expression (designator env)
            (:documentation
              "Expand a lambda expression or a named-lambda expression"))

(defmethod metaenv-expand-lambda-expression (designator env)
  (let*
    ((envcopy (metaenv-clone env (list :toplevel nil)))
     (function-info (separate-lambda-expression-parts designator envcopy)))
    `(,(first designator)
       ,@(when (function-information-named function-info)
           (list (function-information-name function-info)))
       ,(function-information-arglist function-info)
       ,@(function-information-declarations function-info)
       ,@ (metaenv-map-macroexpand-all
            (function-information-body function-info) envcopy))))

(defgeneric metaenv-macroexpand-all-special-form-progn (operator form env)
            (:documentation
              "Expand a form similar to a funcall or to a progn"))

(defmethod metaenv-macroexpand-all-special-form-progn (operator form env)
  (let ((envcopy (metaenv-clone env)))
    (unless (eq operator 'progn)
      (setf (metaenv-toplevel envcopy) nil))
  `(,(if (consp operator)
       ; Common Lisp allows a lambda expression to be used as a function name in a normal function call form
       ; Note that setf functions cannot be called like that
       (metaenv-expand-lambda-expression
         operator envcopy)
       operator)
     ,@ (loop for entry in (cdr form)
              collect (metaenv-macroexpand-all entry envcopy)))))

(defmethod metaenv-macroexpand-all-special-form ((operator (eql 'if)) form env)
  (metaenv-macroexpand-all-special-form-progn operator form env))
(defmethod metaenv-macroexpand-all-special-form ((operator (eql 'progn)) form env)
  (metaenv-macroexpand-all-special-form-progn operator form env))
(defmethod metaenv-macroexpand-all-special-form ((operator (eql 'throw)) form env)
  (metaenv-macroexpand-all-special-form-progn operator form env))
(defmethod metaenv-macroexpand-all-special-form ((operator (eql 'catch)) form env)
  (metaenv-macroexpand-all-special-form-progn operator form env))
(defmethod metaenv-macroexpand-all-special-form ((operator (eql 'multiple-value-prog1)) form env)
  (metaenv-macroexpand-all-special-form-progn operator form env))
(defmethod metaenv-macroexpand-all-special-form ((operator (eql 'multiple-value-call)) form env)
  (metaenv-macroexpand-all-special-form-progn operator form env))
(defmethod metaenv-macroexpand-all-special-form ((operator (eql 'unwind-protect)) form env)
  (metaenv-macroexpand-all-special-form-progn operator form env))

(defmethod metaenv-macroexpand-all-special-form ((operator (eql 'quote)) form env)
  (declare (ignore env)) form)
(defmethod metaenv-macroexpand-all-special-form ((operator (eql 'go)) form env)
  (declare (ignorable env)) form)

(defgeneric metaenv-macroexpand-all-special-form-the (operator form env)
            (:documentation
              "Expand a special form similar to the"))

(defmethod metaenv-macroexpand-all-special-form-the (operator form env)
  `(,operator ,(second form)
              ,@ (metaenv-map-macroexpand-all (cddr form) env)))

(defmethod metaenv-macroexpand-all-special-form ((operator (eql 'return-from)) form env)
  (metaenv-macroexpand-all-special-form-the operator form env))
(defmethod metaenv-macroexpand-all-special-form ((operator (eql 'the)) form env)
  (let ((envcopy (metaenv-clone env (list :toplevel nil))))
    (metaenv-macroexpand-all-special-form-the operator form envcopy)))
(defmethod metaenv-macroexpand-all-special-form ((operator (eql 'eval-when)) form env)
  (metaenv-macroexpand-all-special-form-the operator form env))

(defgeneric metaenv-macroexpand-all-special-form-function (operator form env)
            (:documentation "Expand a form similar to function"))

(defmethod metaenv-macroexpand-all-special-form-function
  (operator form env)
  (let*
    ((envcopy (metaenv-clone env (list :toplevel nil)))
     (designator (second form)))
    (cond
      ; named function in CLISP
      ((and (third form)
            (consp (third form))
            (symbolp designator))
       `(,operator
          ,designator
          ,(second
             (metaenv-macroexpand-all-special-form
               operator `(,operator ,(third form)) envcopy))))
      ((not (consp designator)) form)
      ((eq (car designator) 'setf) form)
      ((eq (car designator) 'lambda)
       `(,operator
          ,(metaenv-expand-lambda-expression designator envcopy)))
      ((and
         (symbolp (car designator))
         (find (symbol-name (car designator))
               '("named-lambda" "lambda-block")
               :test 'equalp))
       `(,operator
          ,(metaenv-expand-lambda-expression designator envcopy)))
      (t form))))

(defmethod metaenv-macroexpand-all-special-form
  ((operator (eql 'function)) form env)
  (metaenv-macroexpand-all-special-form-function
    operator form env))

; Try to catch CCL's nfunction and similar constructions
; Do a very very opportunistic scan…
(defmacro define-handle-nfunction ()
  (let*
    ((nfunction nil)
     (function-name (gensym))
     (argument-name (gensym)))
    (labels
      ((find-nfunction
         (form)
         (let*
           ((operator (first form))
            (name-found
              (eq (second form) function-name))
            (maybe-lambda
              (or
                (and (listp (second form)) (second form))
                (and name-found (listp (third form)) (third form))))
            (lambda-found
              (and
                maybe-lambda
                (or
                  (and
                    (listp (second maybe-lambda))
                    (eq (first (second maybe-lambda)) argument-name))
                  (and
                    (eq (second maybe-lambda) function-name)
                    (listp (third maybe-lambda))
                    (eq (first (third maybe-lambda)) argument-name))))))
           (if lambda-found
             (setf nfunction operator)
             (loop
               for subform in form
               for expanded-subform :=
               (and (listp subform)
                    (not (eq 'quote (first subform)))
                    (macroexpand subform))
               for found := (find-nfunction expanded-subform)
               while (not found))))))
      (find-nfunction
        (macroexpand
          `(defun ,function-name (,argument-name)
             (+ 2 ,argument-name)))))
    (when nfunction
      `(defmethod metaenv-macroexpand-all-special-form
         ((operator (eql ',nfunction)) form env)
         (metaenv-macroexpand-all-special-form-function
           operator form env)))))
(define-handle-nfunction)

(defmethod metaenv-macroexpand-all-special-form
  ((operator (eql 'locally)) form env)
  `(,operator
     ,@ (metaenv-map-macroexpand-all-after-declarations (cdr form) env nil)))

(defmethod metaenv-macroexpand-all-special-form
  ((operator (eql 'load-time-value)) form env)
  `(,operator
     ,(metaenv-macroexpand-all (second form)
                               (make-instance (type-of env)))
     ,@(cddr form)))

(defmethod metaenv-macroexpand-all-special-form
  ((operator (eql 'progv)) form env)
  (let ((envcopy (metaenv-clone env (list :toplevel nil))))
    `(,operator
       ,(metaenv-map-macroexpand-all (second form) envcopy)
       ,(metaenv-map-macroexpand-all (third form) envcopy)
       ,@ (metaenv-map-macroexpand-all (cdddr form) envcopy))))

(defgeneric metaenv-macroexpand-all-special-form-macrolet (operator form env)
            (:documentation
              "Expand a form similar to macrolet"))

(defmethod metaenv-macroexpand-all-special-form-macrolet
  (operator form env)
  (let* ((envcopy (metaenv-clone env)))
    (loop
      for entry in (second form)
      for name := (first entry)
      for expansion := (cdr entry)
      do
      (if (eq operator 'macrolet)
        (metaenv-add-function-like-entry
          envcopy `(,name (:macro-code ,@expansion)))
        (metaenv-add-variable-like-entry
          envcopy `(,name (:macro ,@expansion)))))
    `(,operator
       ,(second form)
       ,@ (metaenv-map-macroexpand-all-after-declarations
            (cddr form) envcopy nil))))

(defmethod metaenv-macroexpand-all-special-form
  ((operator (eql 'macrolet)) form env)
  (metaenv-macroexpand-all-special-form-macrolet
    operator form env))
(defmethod metaenv-macroexpand-all-special-form
  ((operator (eql 'symbol-macrolet)) form env)
  (metaenv-macroexpand-all-special-form-macrolet
    operator form env))

(defgeneric metaenv-macroexpand-all-special-form-flet (operator form env)
            (:documentation
              "Expand a form similar to flet"))

(defmethod metaenv-macroexpand-all-special-form-flet (operator form env)
  (let*
    ((envcopy (metaenv-clone env (list :toplevel nil)))
     (function-definitions
       (loop
         for f in (second form)
         for fname := (first f)
         for arglist := (second f)
         for envcopyfun := (metaenv-clone (if (eq operator 'flet) env envcopy))
         for expanded-arglist := (metaenv-macroexpand-lambdalist 
                                   arglist envcopyfun t t)
         for funbody := (progn
                          (metaenv-map-macroexpand-all-after-declarations
                            (cddr f) envcopyfun t))
         do (metaenv-add-function-like-entry envcopy `(,fname (:plain)))
         collect `(,fname ,expanded-arglist ,@ funbody))))
    `(,operator
       ,function-definitions
       ,@ (metaenv-map-macroexpand-all-after-declarations (cddr form) envcopy nil))))

(defmethod metaenv-macroexpand-all-special-form
  ((operator (eql 'flet)) form env)
  (metaenv-macroexpand-all-special-form-flet operator form env))

; Refuse to expand meta-code
(defmethod metaenv-macroexpand-all-special-form
  ((operator (eql 'defmacro)) form env)
  (declare (ignorable env))
  form)

(defgeneric metaenv-macroexpand-all-special-form-defun (operator form env)
            (:documentation "Expand a form that is similar to defun"))

(defmethod metaenv-macroexpand-all-special-form-defun
  (operator form env)
  (let*
    ((envcopy (metaenv-clone env (list :toplevel nil)))
     (function-info (separate-function-definition-parts
                      form envcopy)))
    (when (and
            (function-information-named function-info)
            (symbolp (function-information-name function-info)))
      (metaenv-add-block envcopy
                         (function-information-name function-info)))
    `(,operator
       ,(function-information-name function-info)
       ,@(function-information-qualifiers function-info)
       ,(function-information-arglist function-info)
       ,@(function-information-declarations function-info)
       ,@(metaenv-map-macroexpand-all
           (function-information-body function-info)
           envcopy))))

(defmethod metaenv-macroexpand-all-special-form
  ((operator (eql 'defun)) form env)
  (metaenv-macroexpand-all-special-form-defun operator form env))

(defmethod metaenv-macroexpand-all-special-form
  ((operator (eql 'defmethod)) form env)
  (metaenv-macroexpand-all-special-form-defun operator form env))

(defmethod metaenv-macroexpand-all-special-form
  ((operator (eql 'labels)) form env)
  (metaenv-macroexpand-all-special-form-flet operator form env))

(defmethod metaenv-macroexpand-all-special-form
  ((operator (eql 'handler-bind)) form env)
  `(,operator
     (,@(loop
          for handler in (second form)
          collect
          (list (first handler)
                (metaenv-macroexpand-all (second handler) env))))
     ,@ (metaenv-map-macroexpand-all (cddr form) env)))

(defmethod metaenv-macroexpand-all-special-form
  ((operator (eql 'multiple-value-bind)) form env)
  (let*
    ((envcopy (metaenv-clone env (list :toplevel nil))))
    (loop for var in (second form)
          do (metaenv-add-variable-like-entry envcopy `(,var (:plain))))
    `(,operator
       ,(second form)
       ,(metaenv-macroexpand-all (third form) env)
       ,@ (metaenv-map-macroexpand-all-after-declarations
            (cdddr form) envcopy nil))))

(defmethod metaenv-macroexpand-all-special-form
  ((operator (eql 'cond)) form env)
  (let ((envcopy (metaenv-clone env (list :toplevel nil))))
    `(,operator
       ,@(loop
           for clause in (cdr form)
           collect (metaenv-map-macroexpand-all clause envcopy)))))
