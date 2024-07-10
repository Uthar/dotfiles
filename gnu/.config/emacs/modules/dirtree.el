;; -*- lexical-binding: t; -*-

;;;; File-browser

(require 'eieio)
(require 'subr-x)

;; TODO dirtree-narrow-to-subtree

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
   (prefix
    :type string
    :initform ""
    :accessor .prefix)
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
      (setf (.prefix node) (concat (.prefix parent) "├ "))
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
    (dirtree--expand-node node)))

(defun dirtree-mouse-expand (event)
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event)))
        node)
    (unless (windowp window)
      (error "No file chosen"))
    (with-current-buffer (window-buffer window)
      (goto-char pos)
      (setf node (dirtree-node-at-point))
      (dirtree--expand-node node))))

(defun dirtree--expand-node (node)
  (cl-case (.kind node)
    ((:file :link) (dirtree-open-file node))
    (:dir (dirtree-expand-dir node))))

(defun dirtree-open-file (node)
  (let ((display-buffer-overriding-action
         '(display-buffer-use-some-window (some-window . mru)))
        (switch-to-buffer-obey-display-actions t))
    (find-file (.path node))))

;; (defun dirtree--DEBUG-find-last-child-position ()
;;   "Find last child position by finding the next child after us in our parent"
;;   (interactive)
;;   (let ((node (dirtree-node-at-point))
;;         (siblings (.children (.parent node)))
;;         (index 0))
;;     (cl-loop for sib in siblings

(defun dirtree-sort-key (node)
  (let ((path (file-name-nondirectory (.path node))))
    (list (if (eq :dir (.kind node)) 1 2)
          (if (eql ?. (aref path 0)) 1 2)
          path)))

(defun dirtree-expand-dir (node)
  (cl-case (.subtree-state node)
    (:init
     (let ((children (list))
           (inhibit-read-only t)
           start end)
       ;; It's rendered by now anyway. It lets children inherit the fixed prefix
       ;; once instead of doing that same replacement from constructor.
       (subst-char-in-string ?└ ?  (.prefix node) t)
       (subst-char-in-string ?├ ?│  (.prefix node) t)
       (cl-loop for (path dirp) in (directory-files-and-attributes (.path node)) do
         (unless (member path '("." ".."))
           (push (make-instance 'dirtree-node
                                :path path
                                :parent node
                                :kind (dirtree-decode-kind dirp))
                 children)))
       (sort children :key 'dirtree-sort-key :in-place t)
       (setf (.children node) children)
       (when (consp children)
         (cl-replace (.prefix (car (last children))) "└ " :start1 (* 2 (.depth node))))
       (save-excursion
         (end-of-line)
         (insert ?\n)
         (setf start (point))
         (cl-loop for start2 = (line-beginning-position)
                  for (node morep) on children do
                  ;; TODO:line-prefix text property
                  ;; TODO: propertize once in constructor
                  (insert (propertize (.prefix node) 'face '(:foreground "gray80")))
                  (insert (propertize (file-name-nondirectory (.path node))
                                      'dirtree-beginning t
                                      'face (cl-case (.kind node)
                                              (:dir dired-directory-face)
                                              (:link dired-symlink-face)
                                              (:file 'default))
                                      'help-echo (.path node)
                                      'follow-link t
                                      'mouse-face 'highlight))
                  ;; TODO buttons
                  (add-text-properties start2 (point) (list 'dirtree-node node))
                  (when morep
                    (insert ?\n)))
         (when (eobp)
           (insert ?\n))
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
    (let* ((inhibit-read-only t)
           (oldroot node)
           (newroot (make-instance 'dirtree-node :path (.path oldroot))))
      (erase-buffer)
      (insert (propertize (.path newroot) 'face 'bold))
      (cl-labels
        ((index-nodes (nodes)
           (let ((index (make-hash-table :test 'equal :size (length nodes))))
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
    (unless (string= (.path saved-node) (.path *dirtree-root*))
      (cl-loop for node = (dirtree-node-at-point) while node
               until (string= (.path node) (.path saved-node))
               do (dirtree-next))
      (set-window-start (selected-window) position))
    (pulse-momentary-highlight-one-line)
    ))
  
;; I'll refresh all initialized subtrees (including closed) in order to not
;; have to refresh those lazily. It's slower but gives simpler code.
;;
;; I have to work my way up from the deepest inited nodes so that deleting
;; lines does not mess up overlays.
;;
;; After processing all the child nodes, we can refresh our own ls state.


(defun dirtree-previous ()
  (interactive)
  (cl-loop for match = (text-property-search-backward 'dirtree-beginning t t t)
           while match
           do (goto-char (prop-match-beginning match))
           while (invisible-p (point))
           finally (return match)))

(defun dirtree-next ()
  (interactive)
  (cl-loop for match = (text-property-search-forward 'dirtree-beginning t t t)
           while match
           do (goto-char (prop-match-beginning match))
           while (invisible-p (point))
           finally (return match)))

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
  (let (depth)
    (setf depth (.depth (dirtree-node-at-point)))
    (when (= 1 depth)
      (error "At root level"))
    (dirtree-previous)
    (cl-loop for node = (dirtree-node-at-point)
             until (= (1- depth) (.depth node))
             do (dirtree-previous))))

(defun dirtree-copy-path ()
  "Save full path of file under point to kill ring."
  (interactive)
  (kill-new (message (.path (dirtree-node-at-point)))))

(defun dirtree-chroot ()
  (interactive)
  (let* ((node (dirtree-node-at-point))
         (depth (.depth node)))
    (unless (eq :dir (.kind node))
      (error "Not a directory"))
    (when (memq (.subtree-state node) '(:closed :init))
      (dirtree-expand-dir node))
    (cl-labels ((adjust-depth (node)
                  (cl-decf (.depth node) depth)
                  (dolist (child (.children node))
                    (adjust-depth child))))
      (adjust-depth node))
    (setq-local *dirtree-root* node)
    (dirtree-refresh)))

(defun dirtree-chroot-up ()
  (interactive)
  (let* ((node *dirtree-root*)
         (path (.path node))
         (up (string-remove-suffix "/" (file-name-directory path)))
         (inhibit-read-only t))
    (setq-local *dirtree-root* (make-instance 'dirtree-node :path up))
    (erase-buffer)
    (insert (propertize (.path *dirtree-root*) 'face 'bold))
    (dirtree-expand-dir *dirtree-root*)
    (dirtree-next)
    (cl-loop for node2 = (dirtree-node-at-point)
             while node2
             until (string= (.path node2) (.path node))
             do (dirtree-next))
    (pulse-momentary-highlight-one-line)))

;; TODO set mark before jumping for C-x x

(defvar-keymap dirtree-mode-map
  :doc "Keymap for File Browser mode"
  "<return>" #'dirtree-expand
  "<mouse-2>" #'dirtree-mouse-expand
  "C" #'dirtree-chroot
  "U" #'dirtree-chroot-up
  "p" #'dirtree-previous
  "n" #'dirtree-next
  "{" #'dirtree-previous-dir
  "}" #'dirtree-next-dir
  "^" #'dirtree-up-dir
  "q" #'dirtree-close-dirtree
  "w" #'dirtree-copy-path
  "g" #'dirtree-refresh)

(defun dirtree ()
  (interactive)
  (let ((switch-to-buffer-obey-display-actions t)
        (display-buffer-overriding-action
         '(display-buffer-in-side-window . ((side . left)))))
    (if (get-buffer "*Dirtree*")
      (pop-to-buffer "*Dirtree*")
      (pop-to-buffer "*Dirtree*")
      (dirtree-mode))))

;; TODO isearch?

(define-derived-mode dirtree-mode fundamental-mode "File Browser"
  (setq-local *dirtree-root* (make-instance 'dirtree-node :path (string-remove-suffix "/" default-directory)))
  ;; (setq-local *dirtree-root* (make-instance 'dirtree-node :path "~/Repos"))
  (insert (propertize (.path *dirtree-root*) 'face 'bold))

  ;; (let ((bound (point))
  ;;       start)
  ;;   (beginning-of-line)
  ;;   (setf start (search-forward "/" bound t)) ;skip root
  ;;   (while start
  ;;     (set-text-properties start (setf start (search-forward "/" bound t))
    
  (dirtree-expand-dir *dirtree-root*)
  (read-only-mode)
  (hl-line-mode)
  ;; Prevent selection on double click
  (setq-local double-click-time nil)
  )

;; TODO isearch-open-invisible-temporary

;;;; VC integration

(defvar dirtree-vc-process-buffer nil)

(defun dirtree-vc-update ()
  (let* ((node (dirtree-node-at-point))
         (path (.path node))
         (backend (ignore-errors (vc-responsible-backend path)))
         (buffer (current-buffer))
         (children (make-hash-table :test 'equal))
         (offset 0))
    ;; (cl-labels
    ;;     ((add-children (n)
    ;;        (dolist (child (.children n))
    ;;          (puthash (cl-subseq (.path child) (1+ (length path))) child children)
    ;;          (add-children child))))
    ;;   (add-children node))
    (when backend
      (save-excursion
        (cl-loop with prefixlen = (1+ (length path))
                 for match = (dirtree-next)
                 for child = (dirtree-node-at-point)
                 while (and match (> (.depth child) (.depth node)))
                 do (puthash (cl-subseq (.path child) prefixlen)
                             (prop-match-end match)
                             children)))
      (setq dirtree-vc-process-buffer (generate-new-buffer " *dirtree-vc-process* tmp status"))
      (with-current-buffer dirtree-vc-process-buffer
        (setq default-directory (expand-file-name path))
        (erase-buffer)
        (vc-call-backend backend 'dir-status-files path (mapcar #'.path (.children node))
          (lambda (entries &optional more-to-come)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (dolist (entry entries)
                  (cl-destructuring-bind (file state &rest r) entry
                    (unless (eq 'up-to-date state)
                      (when-let ((end (gethash file children)))
                        (let ((inhibit-read-only t)
                              (tag (propertize (format " %s\n" (symbol-name state))
                                               'face '(:inherit shadow :slant italic))))
                          (put-text-property end (1+ end) 'display tag))))))))))))))



