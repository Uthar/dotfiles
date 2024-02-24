;; -*- lexical-binding: t -*-

(require 'ansi-color)

(define-error 'pager-eof "EOF")

(defun pager-scroll-up ()
  "Scroll down. Maybe fetch more data if needed and available."
  (interactive)
  (condition-case nil
      (scroll-up nil)
    (end-of-buffer
     (end-of-buffer)
     (let ((start (point))
           (got-eof nil))
       (condition-case nil
           (dotimes (_ 30) (read-line))
         (pager-eof
          (setq got-eof t)))
       (recode-region start (point-max) 'utf-8 'binary)
       (ansi-color-apply-on-region start (point-max))
       (when got-eof
         (signal 'pager-eof nil))))))

(cl-defun read-line (&optional (file "/dev/stdin"))
  (cl-destructuring-bind (_ nbytes)
      (insert-file-contents file nil nil 1)
    (when (zerop nbytes)
        (signal 'pager-eof nil))
    (end-of-buffer))
  (while (not (char-equal ?\n (char-before (point))))
    (cl-destructuring-bind (_ nbytes)
        (insert-file-contents file nil nil 1)
      (end-of-buffer)
      (when (zerop nbytes)
        (signal 'pager-eof nil)))))

(defun pager-search (query)
  "Search, fetch more data until something is found or eof."
  (interactive (list (read-string "Search for: ")))
  (cl-assert (not (string-empty-p query)))
  (let ((match nil))
    (while (null match)
      (condition-case nil
          (setq match (save-excursion (search-forward query) (point)))
        (search-failed
         (pager-scroll-up))))
    (goto-char match)))

(defvar pager-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-v] 'pager-scroll-up)
    (define-key map [?\C-s] 'pager-search)
    map))

(define-derived-mode pager-mode nil "Pager"
  "Major mode for acting as a PAGER, reading data from stdin on an
as-needed basis."
  )

;; (cl-defun read-line-position (&optional (file "/dev/stdin") (position 0))
;;   (cl-destructuring-bind (_ nbytes)
;;       (insert-file-contents file nil position (1+ position))
;;     (cl-incf position nbytes)
;;     (end-of-buffer))
;;   (while (not (char-equal ?\n (char-before (point))))
;;     (cl-destructuring-bind (_ nbytes)
;;         (insert-file-contents file nil position (1+ position))
;;       (cl-incf position nbytes)
;;       (end-of-buffer)
;;       (when (zerop nbytes)
;;         (signal 'eof (list file position)))))
;;   position)

;; (ansi-color-apply-on-region (point-min) (point-max))

;; (let ((coding-system-for-read 'utf-8)
;;       (inhibit-read-only t))
;;   (with-current-buffer "In"
;;     (insert-file-contents "/dev/stdin" nil 0 10)
;;     ;; (insert-file-contents "/home/kasper/Repos/testpipes/conf.txt")
;;     ;; (read-line "/home/kasper/Repos/testpipes/ls.txt")
;;     (ansi-color-apply-on-region (point-min) (point-max))
;;     ))

;; (let ((pos 0)
;;       (coding-system-for-read 'utf-8))
;;   (dotimes (n 50)
;;     (with-current-buffer "In"
;;       (setq pos (read-line "/home/kasper/Repos/testpipes/ls.txt" pos)))))

;; (let ((pos 0)
;;       (coding-system-for-read 'binary))
;;   (dotimes (n 20)
;;     (with-current-buffer "In"
;;       (setq pos (read-line)))))

;; ;; Need to do that because I read byte-per-byte, and emacs doesn't have a chance
;; ;; to understand unicode characters
;; ;;
;; ;; Ah... this removes the text properties created by ansi-color. So need to do
;; ;; that first.
;; (with-current-buffer "In"
;;   (recode-region (point-min) (point-max) 'utf-8 'utf-8))

;; (with-current-buffer "In"
;;   (ansi-color-apply-on-region (point-min) (point-max)))


;; ;;;; Just testing

;; (let ((coding-system-for-read 'binary))
;;     (with-current-buffer "In"
;;       (insert-file-contents "/home/kasper/Repos/testpipes/ls.txt" nil 1 2)))
