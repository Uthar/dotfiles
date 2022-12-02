;;; nix-store.el -- run nix commands in Emacs -*- lexical-binding: t -*-

;; Author: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/NixOS/nix-mode
;; Keywords: nix
;; Version: 1.4.0

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'eieio)
(require 'nix)
(require 'cl-lib)

(defgroup nix-store nil
  "Nix-store customizations."
  :group 'nix)

(defun nix-store-realise (path)
  "Realise a path asynchronously.
PATH the path within /nix/store to realise"
  (make-process
    :buffer nil
    :command (list nix-store-executable "--realise" path)
    :noquery t
    :name (format "*nix-store*<%s>" path)))

(defvar-local nix-buffer-store-path nil "Buffer-local object holding an `nix-store-path` object.")

(defclass nix-store-path ()
  ((path      :initarg :path                       :accessor nix-store-path-path)
    (status   :initarg :status :initform 'realised :accessor nix-store-path-status)
    (hash     :initarg :hash                       :accessor nix-store-path-hash)
    (size     :initarg :size                       :accessor nix-store-path-size)
    (derivers :initarg :derivers                   :accessor nix-store-path-derivers)
    (outputs  :initarg :outputs                    :accessor nix-store-path-outputs)
    (references :initarg :references               :accessor nix-store-path-references)
    (referrers  :initarg :referrers                :accessor nix-store-path-referrers)
    (requisites :initarg :requisites               :accessor nix-store-path-requisites))
  "Nix-Store-Path Class holds all information of the path that
is displayed")

(cl-defmethod nix-store-fill-data ((object nix-store-path))
  "Query the nix store store via `nix-store-executable' and save that data into OBJECT."
  (oset object :size (nix-store--query 'size (nix-store-path-path object)))
  (oset object :hash (nix-store--query 'hash (nix-store-path-path object)))
  (oset object :derivers (nix-store--query 'deriver (nix-store-path-path object)))
  (oset object :outputs (nix-store--query 'outputs (nix-store-path-path object)))
  (oset object :referrers (nix-store--query 'referrers (nix-store-path-path object)))
  (oset object :requisites (nix-store--query 'requisites (nix-store-path-path object)))
  (oset object :references (nix-store--query 'references (nix-store-path-path object)))
  object)

(cl-defun nix-store--query (argument &optional (path (nix-store-path-path nix-buffer-store-path)))
  "Query the nix-store for information.
ARGUMENT is given to the executable as an argument. See
nix-store(1) for possibilities. PATH is the store object that is
being queried. Runs `nix-store-executable' to get that
information."
  (let ((nix-executable nix-store-executable))
    (cond
      ((eq 'deriver argument)
	;; Special treatment for 'derivers', we want to treat a single entry
	;; with this string as an empty list
	(remove "unknown-deriver"
	  (nix--process-lines "--query" "--deriver" path )))
      ((eq 'size argument) (string-to-number (nix--process-string "--query" "--size" path )))
      ((eq 'hash argument) (nix--process-string "--query" "--hash" path ))
      ((eq 'requisites argument) (nix--process-lines "--query" "--requisites" path ))
      ((eq 'references argument) (nix--process-lines "--query" "--references" path ))
      ((eq 'referrers argument) (nix--process-lines "--query" "--referrers" path ))
      ((eq 'outputs argument)
	(ignore-errors
	  ;; This can fail for non-derivation paths
	  (nix--process-lines "--query" "--outputs" path )))
      (t (error "Unknown argument to nix-store --query: %s" argument)))))

(cl-defun nix-store-path-insert-path (&optional (store-path nix-buffer-store-path))
  "Insert a section showing the path of STORE-PATH."
  (insert (propertize (format "%-11s" "Path:") 'face 'font-lock-keyword-face))
  (insert (format "%s" (nix-store-path-path store-path)))
  (newline))

(cl-defun nix-store-path-insert-size (&optional (store-path nix-buffer-store-path))
  "Insert a section showing the size of STORE-PATH."
  (insert (propertize (format "%-11s" "Size:") 'face 'font-lock-keyword-face))
  (insert (format "%s" (nix-store-path-size store-path)))
  (newline))

(cl-defun nix-store-path-insert-hash (&optional (store-path nix-buffer-store-path))
  "Insert a section showing the hash of STORE-PATH."
  (insert (propertize (format "%-11s" "Hash:") 'face 'font-lock-keyword-face))
  (insert (format "%s" (nix-store-path-hash store-path)))
  (newline))

(cl-defun nix-store-path-insert-status (&optional (store-path nix-buffer-store-path))
  "Insert a section showing the status of STORE-PATH."
  (insert (propertize (format "%-11s" "Status:") 'face 'font-lock-keyword-face))
  (insert (format "%s" (nix-store-path-status store-path)))
  (newline))

(defmacro nix-store--insert-section-list (type value label)
  "Helper macro for inserting a list as a magit-section.
TYPE and VALUE will be used as the type and value of the section
respectively. The LABEL is the text displayed."
  `(let ((value ,value))
     (when (and (listp value) (> (length value) 0))
       (insert (propertize (format "%-11s" ,label) 'face 'font-lock-keyword-face))
       (newline)
       (cl-loop for x in value
		do (progn
		     (insert x)
		     (newline)))
       (insert ?\n))))

(cl-defun nix-store-path-insert-derivers (&optional (store-path nix-buffer-store-path))
  "Insert sections showing all derivers of STORE-PATH."
  (nix-store--insert-section-list derivers (nix-store-path-derivers store-path) "Derivers:"))

(cl-defun nix-store-path-insert-outputs (&optional (store-path nix-buffer-store-path))
  "Insert sections showing all outputs of STORE-PATH."
  (nix-store--insert-section-list outputs (nix-store-path-outputs store-path) "Outputs:"))

(cl-defun nix-store-path-insert-references (&optional (store-path nix-buffer-store-path))
  "Insert sections showing all references of STORE-PATH."
  (nix-store--insert-section-list references (nix-store-path-references store-path) "References:"))

(cl-defun nix-store-path-insert-referrers (&optional (store-path nix-buffer-store-path))
  "Insert sections showing all referrers of STORE-PATH."
  (nix-store--insert-section-list referrers (nix-store-path-referrers store-path) "Referrers:"))

(cl-defun nix-store-path-insert-requisites (&optional (store-path nix-buffer-store-path))
  "Insert sections showing all requisites of STORE-PATH."
  (nix-store--insert-section-list requisites (nix-store-path-requisites store-path) "Requisites:"))

(defcustom nix-store-path-headers-hook
  '(nix-store-path-insert-path
     nix-store-path-insert-status
     nix-store-path-insert-hash
     nix-store-path-insert-size)
  "Hook run to insert headers into the nix-store buffer.
A list of function that each take one argument, the store path object."
  :group 'nix-store
  :type 'hook
  :options '(nix-store-path-insert-path
	      nix-store-path-insert-status
	      nix-store-path-insert-hash
	      nix-store-path-insert-size))

(defcustom nix-store-path-sections-hook
  '(nix-store-path-insert-derivers
     nix-store-path-insert-outputs
     nix-store-path-insert-references
     nix-store-path-insert-referrers
     nix-store-path-insert-requisites)
  "Hook run to insert sections into a nix-store buffer.
A list of function that each take one argument, the store path object."
  :group 'nix-store
  :type 'hook)

(defun nix-store-show-path (path)
  "Show a nix-store PATH.

If you want to change the order of the section lists (or even
implement your own ones) you can customize the variable
`nix-store-path-headers-hook' and
`nix-store-path-sections-hook'."
  (interactive "FNix-Store-Path: ")
  (setq path (expand-file-name path default-directory))
  (switch-to-buffer (format "Nix Store Path: %s" path))
  (nix-store-path-mode)
  (setq nix-buffer-store-path (nix-store-fill-data (make-instance 'nix-store-path :path path))
    list-buffers-directory path)
  (when (file-directory-p path)
    (setq default-directory path))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (run-hooks 'nix-store-path-headers-hook)
    (newline)
    (run-hooks 'nix-store-path-sections-hook)
    (goto-char (point-min))))

(defun nix-store-path-at-point ()
  "Return the nix-store path at point."
  ;; TODO extract this via magit-section values
  (substring-no-properties (thing-at-point 'filename)))

(defun nix-store-show-path-at-point ()
  "Opens the nix-store-path at point.

It uses \\[nix-store-show-path] to display the store path."
  (interactive)
  (nix-store-show-path (nix-store-path-at-point)))

(defvar nix-store-path-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'nix-store-show-path-at-point)
    map))

(defun nix-store--revert-buffer-function (&rest _ignore)
  "Helper function to be called by `revert-buffer'."
  (nix-store-show-path (nix-store-path-path nix-buffer-store-path)))

(define-derived-mode nix-store-path-mode nil "Nix Store Path"
  (setq-local revert-buffer-function #'nix-store--revert-buffer-function)
  (read-only-mode 1))

(provide 'nix-store)
;;; nix-store.el ends here
