;; -*- lexical-binding: t; -*-

;;;; File-browser

(require 'eieio)
(require 'subr-x)

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
      (cl-destructuring-bind (dirp &rest) (file-attributes path)
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
  (get-text-property (line-beginning-position) 'dirtree-node))

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
       (save-excursion
         (end-of-line)
         (newline)
         (setf start (point))
         (cl-loop for node in children do
                  ;; TODO defmethod dirtree-format-node
                  (insert (propertize (format "%s%s: %s\n"
                                              (make-string (.depth node) ?\ )
                                              (.kind node)
                                              (file-name-nondirectory (.path node)))
                                      'dirtree-node node)))
         ;; Always because it removes the initial newline, which is unneeded if
         ;; there were children, because they always write a trailing newline,
         ;; and it is unneeded when there were no children, because it creates
         ;; an extra empty line.
         (when t ;(consp children)
           (delete-char -1))
         (setf end (1+ (point))))
       (setf (.subtree-overlay node) (make-overlay start end))
       (setf (.subtree-state node) :opened)
       ;; For debugging
       ;; (overlay-put (.subtree-overlay node) 'dirtree-path (.path node))
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

;; (defun dirtree--refresh-subtree (node)
;;   (cl-loop for sub in (.children node) do
;;     ;; Only refresh inited dirs.
;;     ;; Init empty dirs too, because files could have been added to it.
;;     ;;
;;     ;; I'll refresh all initialized subtrees (including closed) in order to not
;;     ;; have to refresh those lazily. It's slower but gives simpler code.
;;     ;;
;;     ;; I have to work my way up from the deepest inited nodes so that deleting
;;     ;; lines does not mess up overlays.
;;     (when (and (eq :dir (.kind sub))
;;                (memq (.subtree-state sub) '(:opened :closed)))
;;       (dirtree--refresh-subtree sub))
;;     ;; After processing all the child nodes, we can refresh our own ls state.
;;     (cl-adjoin '("foo") '(("a") ("b") ("foo")) :test #'string= :key #'car)

;;     (setf ___tmp123 (list '("a") '("b") '("foo")))
;;     (cl-pushnew '("bar") ___tmp123 :test #'string= :key #'car)

;; (let ((*testvar* 0))
;;   (cl-labels ((testfun ()
;;                 (if (< *testvar* 10)
;;                   (let ((*testvar* (1+ *testvar*)))
;;                     (testfun))
;;                   *testvar*)))
;;     (testfun)))

(defun dirtree--refresh-subtree-2 (node)
    ;; ALTERNATIVE SOLUTION
    ;; First remember which nodes were opened.
    ;; Then wipe all nodes
    ;; Reinit root then reinit the remembered nodes, too.
    (let* ((oldroot node)
           (newroot (make-instance 'dirtree-node :path (.path oldroot))))
      (erase-buffer)
      (cl-labels
        ((index-nodes (nodes)
           (let ((index (make-hash-table :test 'equal)))
             (dolist (node nodes)
               (when (and (eq :dir (.kind node))
                          (eq :opened (.subtree-state node)))
                 (puthash (.path node) node index)))
             index))
         (reopen (node cousins)
           (when-let ((savedroot (gethash (.path node) cousins)))
             (dirtree-expand-dir node)
             ;; (pulse-momentary-highlight-one-line)
             ;; (sit-for 0.5)
             (let ((cousins (index-nodes (.children savedroot))))
               (cl-loop for child in (.children node) do
                 (dirtree-next)
                 (reopen child cousins))))))
        (reopen newroot (index-nodes (list oldroot)))
        ;; Save new root with recreated structure
        (setq-local *dirtree-root* newroot))))

(defun dirtree--DEBUG-visualise-children ()
  (interactive)
  ;; Only inited dirs have the overlay. It tracks their position.
  ;;
  ;; A node can only get inited when it has an inited parent, so we can walk
  ;; down starting from root.
  ;;
  ;; Let's jump to each such dir and refresh their children, starting from the
  ;; deepes child.
  (cl-loop for sub in (.children (dirtree-node-at-point)) do
    (when (slot-boundp sub 'subtree-overlay)
      (goto-char (1- (overlay-start (.subtree-overlay sub))))
      (sit-for 0.5)
      (pulse-momentary-highlight-one-line)
      (dirtree--DEBUG-visualise-children))))
                 
(defun dirtree-refresh ()
  "Refresh opened subtrees to detect added and deleted files."
  (interactive)
  ;; Remember it to bring the point back to it after refresh.
  (let ((position (window-start))
        (saved-node (dirtree-node-at-point)))
    (dirtree--refresh-subtree-2 *dirtree-root*)
    (goto-char (point-min))
    (dirtree-next)
    (cl-loop for node = (dirtree-node-at-point) while node
             until (string= (.path node) (.path saved-node))
             do (dirtree-next))
    ;; (recenter)
    (set-window-start (selected-window) position)
    ;; TODO żeby przywracał też scroll (scroll-excursion?)
    (pulse-momentary-highlight-one-line)))
  
;; I'll refresh all initialized subtrees (including closed) in order to not
;; have to refresh those lazily. It's slower but gives simpler code.
;;
;; I have to work my way up from the deepest inited nodes so that deleting
;; lines does not mess up overlays.
;;
;; After processing all the child nodes, we can refresh our own ls state.


(defun dirtree-previous ()
  (interactive)
  (forward-line -1))

(defun dirtree-next ()
  (interactive)
  (forward-line 1))

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

(defun dirtree-copy-path ()
  "Save full path of file under point to kill ring."
  (interactive)
  (kill-new (message (.path (dirtree-node-at-point)))))

(defvar-keymap dirtree-mode-map
  :doc "Keymap for File Browser mode"
  "<return>" #'dirtree-expand
  "p" #'dirtree-previous
  "n" #'dirtree-next
  "{" #'dirtree-previous-dir
  "}" #'dirtree-next-dir
  "^" #'dirtree-up-dir
  "q" #'dirtree-close-dirtree
  "w" #'dirtree-copy-path
  "g" #'dirtree-refresh)

;; TODO isearch?

(define-derived-mode dirtree-mode nil "File Browser"
  (setq-local *dirtree-root* (make-instance 'dirtree-node :path (string-remove-suffix "/" default-directory)))
  ;; (setq-local *dirtree-root* (make-instance 'dirtree-node :path "~/Repos"))
  (dirtree-expand-dir *dirtree-root*)
  ;; TODO read-only-mode
  )