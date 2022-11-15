(in-package :agnostic-lizard)

(defun wrap-every-form-reader (callback normal-readtable)
  "Prepare a special reader for the readtable that reads every top-level form 
  using normal-readtable, then applies the callback to the form"
  (lambda (stream char)
    (unread-char char stream)
    (let*
      ((*readtable* normal-readtable)
       (eof-marker (gensym))
       (form (read stream nil eof-marker)))
      (cond
        ((equal form eof-marker) nil)
        (t (funcall callback form))))))

(defun install-wrap-every-form-reader (callback)
  "Add an entry to the readtable that reads top-level forms normally, then 
  applies the callback to each form"
  (setf *readtable* (copy-readtable))
  (set-macro-character
    #\( (wrap-every-form-reader callback (copy-readtable)) 
    ; #\) will work fine on its own; this comment also closes the parenthesis
    ))

(defmacro wrap-rest-of-input (callback)
  "Modify each top-level form in the rest of the current file with the callback"
  `(eval-when
     (:compile-toplevel :load-toplevel :execute)
     (install-wrap-every-form-reader ,callback)))

(defmacro with-wrap-every-form-reader (callback &body body)
  "Execute body using a modified readtable so that every top-level form read by
  the reader is modified using the callback"
  `(let*
     ((old-readtable *readtable*))
     (setf *readtable* (copy-readtable))
     (install-wrap-every-form-reader ,callback)
     (unwind-protect
       (progn ,@ body)
       (setf *readtable* old-readtable))))
