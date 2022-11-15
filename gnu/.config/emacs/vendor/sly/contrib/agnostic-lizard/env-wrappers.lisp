(in-package :agnostic-lizard)

; Simple helpers to wrap a chunk of code

(defun macrolet-wrap (name function body)
  "Wrap a chunk of code with a macrolet defined by a macro-function"
  `(macrolet
     ((,name (&whole whole &rest args &environment environment)
             (declare (ignorable args))
             (funcall ,function whole environment)))
     ,@ body))

(defun macrolet-code-wrap (name code body)
  "Wrap a chunk of code with a macrolet given a literal macrolet definition"
  `(macrolet
     ((,name ,@code))
     ,@ body))

(defun symbol-macrolet-wrap (name expansion body)
  "Wrap a chunk of code with a symbol-macrolet"
  `(symbol-macrolet ((,name ,expansion)) ,@body))

(defun flet-wrap (name body)
  "Wrap a chunk of code with a pseudo-flet;
it is for macroexpansion only, so there is no body"
  `(flet ((,name (&rest args) (declare (ignorable args)) nil))
     ,@ body))

(defun let-wrap (name body)
  "Wrap a chunk of code with a pseudo-let;
it is for macroexpansion only, so there is no body;
some globals don't like to ever be bound to nil, so try a global value first"
  `(let ((,name ,(if (boundp name) name nil)))
     (declare (ignorable ,name))
     ,@body))

; Wrappers with multiple names to define

(defun wrap-function-like-env (entries form)
  "Wrap a form using the function-like entries.
An entry can be:
(name nil) — not defined in this environment
(name (:plain)) — an flet
(name (:macro macro-function)) — a macrolet, defined by a macro-function
(name (:macro-from environment)) — a macrolet that should be copied from an environment object
(name (:macro-code)) — a macrolet, defined by literal macrolet code
(name (:macro-function-code)) — a macrolet, defined by the code to define the macro-function
"
  (loop
    for res := form then new-res
    for entry in entries
    for name := (first entry)
    for op := (second entry)
    for kind := (first op)
    for new-res := (ecase kind
                     ((nil) res)
                     ((:plain) (flet-wrap name (list res)))
                     ((:macro) (macrolet-wrap name (second op) (list res)))
                     ((:macro-from)
                      (macrolet-wrap
                        name (macro-function name (second op)) (list res)))
                     ((:macro-code)
                      (macrolet-code-wrap name (cdr op) (list res)))
                     ((:macro-function-code)
                      (macrolet-wrap name `(lambda ,@ (cdr op)) (list res)))
                     )
    finally (return res)))

(defun wrap-variable-like-env (entries form)
  "Wrap a form using variable-like entries.
An entry can be:
(name nil) — not defined in this environment
(name (:plain)) — a let
(name (:macro expansion)) — a symbol-macro, defined by the expansion
(name (:macro-from environment)) — a symbol-macro to be copied from an environment
"
  (loop
    for res := form then new-res
    for entry in entries
    for name := (first entry)
    for op := (second entry)
    for kind := (first op)
    for new-res := (ecase kind
                     ((nil) res)
                     ((:plain) (let-wrap name (list res)))
                     ((:macro) (symbol-macrolet-wrap name (second op) (list res)))
                     ((:macro-from)
                      (symbol-macrolet-wrap
                        name (macroexpand-1 name (second op)) (list res)))
                     )
    finally (return res)))

(defun wrap-block-env (entries form)
  "Wrap a form to make sure all the blocks listed in entries are defined"
  (loop
    for res := form then new-res
    for entry in entries
    for new-res := `(block ,entry ,res)
    finally (return res)))

(defun wrap-tag-env (entries form)
  "Wrap a form to make sure all the tags listed in entries are defined"
  (if entries
    (let
      ((s (gensym)))
      `(catch
         ',s
         (tagbody
           ,@ entries
           (throw ',s ,form))))
    form))

