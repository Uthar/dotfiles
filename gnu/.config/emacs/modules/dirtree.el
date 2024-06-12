;; -*- lexical-binding: t; -*-

;;;; File-browser

(require 'eieio)

(defclass dirtree-node ()
  ((path
    :type string
    :initform (error "path required")
    :initarg :path
    :accessor .path)
   (kind
    :type (member :file :dir :link)
    :initarg :kind
    :accessor .kind)
   (parent
    :type dirtree-node
    :initarg :parent
    :accessor .parent)
   (children
    :type sequence
    :initform (list)
    :accessor .children)
   (depth
    :type number
    :initform 0
    :accessor .depth)
   (subtree-state
    :type (member :init :opened :closed)
    :initform :init
    :accessor .subtree-state)
   (subtree-overlay
    :type overlay
    :accessor .subtree-overlay)))

(defun dirtree-decode-kind (attr)
  (cl-etypecase attr
    (null :file)
    ((member t) :dir)
    (string :link)))

(cl-defmethod initialize-instance :after ((node dirtree-node) &optional args)
  (cl-destructuring-bind (&key path kind parent &allow-other-keys) args
    (unless kind
      (cl-destructuring-bind (dirp &rest _) (file-attributes path)
        (setf (.kind node) (dirtree-decode-kind dirp))))
    (when parent
      (setf (.path node) (concat (.path parent) "/" path))
      (setf (.depth node) (1+ (.depth parent))))))

(cl-defmethod cl-print-object ((node dirtree-node) stream)
  (princ (format "#<DIRTREE-NODE :path %s :kind %s :children %s :depth %s"
                 (.path node) (.kind node) (length (.children node)) (.depth node))
         stream))

(cl-print-object (make-instance 'dirtree-node :path "~/.bashrc") nil)
(.children (make-instance 'dirtree-node :path "~/.bashrc"))

(defun dirtree-node-at-point ()
  (get-text-property (point) 'dirtree-node))

(defun dirtree-expand ()
  (interactive)
  (let ((node (dirtree-node-at-point)))
    (cl-case (.kind node)
      ((:file :link) (error "Opening files not implemented"))
      (:dir (dirtree-expand-dir node)))))

;; (defun dirtree--DEBUG-find-last-child-position ()
;;   "Find last child position by finding the next child after us in our parent"
;;   (interactive)
;;   (let ((node (dirtree-node-at-point))
;;         (siblings (.children (.parent node)))
;;         (index 0))
;;     (cl-loop for sib in siblings 

(defun dirtree-expand-dir (node)
  (cl-case (.subtree-state node)
    (:init
     (let ((children (list))
           start end)
       (cl-loop for (path dirp) in (directory-files-and-attributes (.path node)) do
         (unless (member path '("." ".."))
           (push (make-instance 'dirtree-node
                                :path path
                                :parent node
                                :kind (dirtree-decode-kind dirp))
                 children)))
       (setf (.children node) children)
       (end-of-line)
       (newline)
       (setf start (point))
       (cl-loop for node in children do
                (insert (propertize (format "%s%s: %s\n"
                                            (make-string (.depth node) ?-)
                                            (.kind node)
                                            (file-name-nondirectory (.path node)))
                                    'dirtree-node node)))
       (setf end (point))
       (setf (.subtree-overlay node) (make-overlay start end))
       (setf (.subtree-state node) :opened)
       ;; (overlay-put (.subtree-overlay node) 'face 'highlight)
       ))
    (:opened
     (overlay-put (.subtree-overlay node) 'invisible t)
     (setf (.subtree-state node) :closed))
    (:closed
     (overlay-put (.subtree-overlay node) 'invisible nil)
     (setf (.subtree-state node) :opened))))
 
(defun dirtree ()
  (interactive)
  (let ((switch-to-buffer-obey-display-actions t)
        (display-buffer-overriding-action
         '(display-buffer-in-side-window . ((side . left)))))
    (pop-to-buffer "*Dirtree*")
    (dirtree-mode)))

(defvar *dirtree-root*)

(defun dirtree-close-dirtree ()
  (interactive)
  (delete-window))

(defun dirtree-refresh ()
  (interactive)
  (erase-buffer)
  (dirtree-expand-dir *dirtree-root*))

(defun dirtree-previous ()
  (interactive)
  (next-line -1))

(defun dirtree-next ()
  (interactive)
  (next-line 1))

(defun dirtree-previous-dir ()
  (interactive)
  (when (eq :dir (.kind (dirtree-node-at-point)))
    (dirtree-previous))
  (cl-loop for node = (dirtree-node-at-point)
           until (eq :dir (.kind node))
           do (dirtree-previous)))

(defun dirtree-next-dir ()
  (interactive)
  (when (eq :dir (.kind (dirtree-node-at-point)))
    (dirtree-next))
  (cl-loop for node = (dirtree-node-at-point)
           until (eq :dir (.kind node))
           do (dirtree-next)))

(defun dirtree-up-dir ()
  (interactive)
  (cl-loop with depth = (prog1 (.depth (dirtree-node-at-point))
                          (dirtree-previous))
           for node = (dirtree-node-at-point)
           until (= (1- depth) (.depth node))
           do (dirtree-previous)))

(defvar-keymap dirtree-mode-map
  :doc "Keymap for File Browser mode"
  "<return>" #'dirtree-expand
  "p" #'dirtree-previous
  "n" #'dirtree-next
  "{" #'dirtree-previous-dir
  "}" #'dirtree-next-dir
  "^" #'dirtree-up-dir
  "q" #'dirtree-close-dirtree
  "g" #'dirtree-refresh)

(define-derived-mode dirtree-mode nil "File Browser"
  ;; (setq-local *dirtree-root* (make-instance 'dirtree-node :path default-directory))
  (setq-local *dirtree-root* (make-instance 'dirtree-node :path "~/Repos"))
  (dirtree-expand-dir *dirtree-root*))
