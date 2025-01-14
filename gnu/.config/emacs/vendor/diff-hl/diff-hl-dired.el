;;; diff-hl-dired.el --- Highlight changed files in Dired -*- lexical-binding: t -*-

;; Copyright (C) 2012-2017  Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; To enable in all Dired buffers, add this to your init file:
;;
;; (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
;;
;; or
;;
;; (add-hook 'dired-mode-hook 'diff-hl-dired-mode-unless-remote)
;;
;; to do it only in local Dired buffers.

;;; Code:

(require 'diff-hl)
(require 'dired)
(require 'vc-hooks)

(defvar diff-hl-dired-process-buffers (make-hash-table :test 'equal))

(defgroup diff-hl-dired nil
  "VC diff highlighting on the side of a Dired window."
  :group 'diff-hl)

(defface diff-hl-dired-insert
  '((default :inherit diff-hl-insert))
  "Face used to highlight added files.")

(defface diff-hl-dired-delete
  '((default :inherit diff-hl-delete))
  "Face used to highlight directories with deleted files.")

(defface diff-hl-dired-change
  '((default :inherit diff-hl-change))
  "Face used to highlight changed files.")

(defface diff-hl-dired-unknown
  '((default :inherit dired-ignored))
  "Face used to highlight unregistered files.")

(defface diff-hl-dired-ignored
  '((default :inherit dired-ignored))
  "Face used to highlight unregistered files.")

(defcustom diff-hl-dired-extra-indicators t
  "Non-nil to indicate ignored files."
  :type 'boolean)

(defcustom diff-hl-dired-ignored-backends '(RCS)
  "VC backends to ignore.
The directories registered to one of these backends won't have
status indicators."
  :type `(repeat (choice ,@(mapcar
                            (lambda (name)
                              `(const :tag ,(symbol-name name) ,name))
                            vc-handled-backends))))

;;;###autoload
(define-minor-mode diff-hl-dired-mode
  "Toggle VC diff highlighting on the side of a Dired window."
  :lighter ""
  (if diff-hl-dired-mode
      (progn
        (diff-hl-maybe-define-bitmaps)
        (set (make-local-variable 'diff-hl-dired-process-buffers) (make-hash-table :test 'equal))
        (add-hook 'dired-after-readin-hook 'diff-hl-dired-update nil t))
    (remove-hook 'dired-after-readin-hook 'diff-hl-dired-update t)
    (diff-hl-dired-clear)))

(defun diff-hl-dired-update ()
  "Highlight the Dired buffer."
  (let ((backend (ignore-errors (vc-responsible-backend default-directory)))
        (dired-buffer (current-buffer)))
    (when (and backend (not (memq backend diff-hl-dired-ignored-backends)))
      (diff-hl-dired-clear)
      (dolist (subdir-entry dired-subdir-alist)
        (cl-destructuring-bind (subdir . marker) subdir-entry
          (let ((tmp-buffer (gethash subdir diff-hl-dired-process-buffers)))
            (if (buffer-live-p tmp-buffer)
                (let ((proc (get-buffer-process tmp-buffer)))
                  (when proc (kill-process proc)))
              (setq tmp-buffer
                (generate-new-buffer (concat " *diff-hl-dired* tmp status for " subdir)))
              (puthash subdir tmp-buffer diff-hl-dired-process-buffers))
            (with-current-buffer tmp-buffer
              (setq default-directory (expand-file-name subdir))
              (erase-buffer)
              (diff-hl-dired-status-files
               backend subdir
               (when diff-hl-dired-extra-indicators
                 (cl-loop for file in (directory-files subdir)
                          unless (member file '("." ".." ".hg"))
                          collect file))
               (let (files-alist dirs-alist)
                 (lambda (entries &optional more-to-come)
                   (when (buffer-live-p dired-buffer)
                     (with-current-buffer dired-buffer
                       (dolist (entry entries)
                         (cl-destructuring-bind (file state &rest r) entry
                           ;; Work around http://debbugs.gnu.org/18605
                           (setq file (replace-regexp-in-string "\\` " "" file))
                           (let ((type (plist-get
                                        '( edited change added insert removed delete
                                           unregistered unknown ignored ignored)
                                        state)))
                             (if (string-match "\\`\\([^/]+\\)/" file)
                                 (let* ((dir (match-string 1 file))
                                        (value (cdr (assoc dir dirs-alist))))
                                   (unless (eq value type)
                                     (cond
                                      ((eq state 'up-to-date))
                                      ((null value)
                                       (push (cons dir type) dirs-alist))
                                      ((not (eq type 'ignored))
                                       (setcdr (assoc dir dirs-alist) 'change)))))
                               (push (cons file type) files-alist)))))
                       (unless more-to-come
                         (diff-hl-dired-highlight-items
                          (append dirs-alist files-alist)
                          subdir))))
                     (unless more-to-come
                       (kill-buffer (gethash subdir diff-hl-dired-process-buffers))
                       (remhash subdir diff-hl-dired-process-buffers))))))
         ))))))

(defun diff-hl-dired-status-files (backend dir files update-function)
  "Using version control BACKEND, return list of (FILE STATE EXTRA) entries
for DIR containing FILES. Call UPDATE-FUNCTION as entries are added."
  (if (version< "25" emacs-version)
      (vc-call-backend backend 'dir-status-files dir files update-function)
    (vc-call-backend backend 'dir-status-files dir files nil update-function)))

(when (version< emacs-version "24.4.51.5")
  ;; Work around http://debbugs.gnu.org/19386
  (defadvice vc-git-dir-status-goto-stage (around
                                           diff-hl-dired-skip-up-to-date
                                           (stage files update-function)
                                           activate)
    (when (eq stage 'ls-files-up-to-date)
      (setq stage 'diff-index))
    ad-do-it))

(defun diff-hl-dired-highlight-items (alist &optional subdir)
  "Highlight ALIST containing (FILE . TYPE) elements."
  (dolist (pair alist)
    (let* ((file (car pair))
           (truename (cond
                      (subdir (concat subdir file))
                      (t (expand-file-name file))))
           (type (cdr pair)))
      (save-excursion
        (if subdir
          (dired-goto-subdir subdir)
          (goto-char (point-min)))
        (when (and type (dired-goto-file-1
                         file truename nil))
          (let* ((diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type)
                 (diff-hl-fringe-face-function 'diff-hl-dired-face-from-type)
                 (o (diff-hl-add-highlighting type 'single)))
            (overlay-put o 'modification-hooks '(diff-hl-overlay-modified))
            ))))))

(defun diff-hl-dired-face-from-type (type _pos)
  (intern (format "diff-hl-dired-%s" type)))

(defalias 'diff-hl-dired-clear 'diff-hl-remove-overlays)

;;;###autoload
(defun diff-hl-dired-mode-unless-remote ()
  (unless (file-remote-p default-directory)
    (diff-hl-dired-mode)))

(provide 'diff-hl-dired)

;;; diff-hl-dired.el ends here
