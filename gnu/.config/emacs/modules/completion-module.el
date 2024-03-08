;; -*- lexical-binding: t -*-

;;;;;;; Builtins for sanity

(setopt completions-format 'one-column)
(setopt completion-auto-help 'visible)
(setopt completion-show-help nil)

(setopt completion-ignore-case t)
(setopt read-buffer-completion-ignore-case t)
(setopt read-file-name-completion-ignore-case t)

(add-to-list 'completion-styles 'flex)
(setq completion-category-overrides '((file (styles basic partial-completion))))

;;;;;;;; Limit completions buffer size

;; TODO consider completions-max-height
;; Seems like the difference between display-buffer-alist and
;; completions-max-height is that the former maintains the size always, while
;; the latter shrinks the window when there is fewer completions.
(add-to-list 'display-buffer-alist '("\\*Completions\\*" nil (window-height . 12)))
;; (setopt completions-max-height 12)

;; TODO consider completion-auto-deselect
;; Want this because I *always* want something selected as soon as I type -
;; that's the whole point of this module. Without this there is flickering in
;; completion-at-point (but not with minibuffer completion)
(setopt completion-auto-deselect nil)

;; FIXME make M-x sort by history - currently this doesn't work
;; (setopt completions-sort 'historical)

;;;;;;;; Complete filenames with C-M-i
(autoload 'comint-filename-completion "comint")
(add-to-list 'completion-at-point-functions 'comint-filename-completion)

;;;;;;;; Fix to screen jump in emacs 29-trunk

(define-key completion-in-region-mode-map
            (kbd "RET")
            'minibuffer-choose-completion)

;;;;;;;; Select first candidate or minibuffer contents in minibuffer completion

(defun kaspi/minibuffer-end-completion ()
  (interactive)
  ;; Now really insert the completion into the minibuffer
  ;; Previously I just "select" it for the visual aspect
  ;; (with `minibuffer-completion-auto-choose' bound to nil)
  (when (get-buffer-window (get-buffer "*Completions*"))
    (minibuffer-previous-completion)
    (minibuffer-next-completion))
  (exit-minibuffer))

(define-key minibuffer-local-must-match-map
            (kbd "RET")
            'kaspi/minibuffer-end-completion)

(define-key minibuffer-local-completion-map
            (kbd "RET")
            'kaspi/minibuffer-end-completion)

;;;;;;;; Switch to completions shortcut

;; TODO make M-c while in *Completions* buffer go back to the buffer that jumped
;; there

;; (define-key completion-in-region-mode-map (kbd "M-c") 'switch-to-completions)
;; (define-key minibuffer-mode-map (kbd "M-c") 'switch-to-completions)

;;;;;;;; Auto refresh completions buffer after typing

(defvar lcr-commands
  (list 'self-insert-command
        'delete-backward-char
        'backward-delete-char-untabify
        'kill-region
        'yank
        'undo)
  "Commands to trigger completion help after, whether in region or minibuffer")

(defvar lcr-minibuffer-disabled-commands
  (list 'query-replace
        'query-replace-regexp
        'shell-command
        'dired-create-directory
        'make-empty-file
        'eval-expression)
  "Minibuffer commands to not refresh completions for")

(defun lcr-refresh ()
  (let ((inhibit-message t))
  (cond
   ((and (minibufferp)
         (not (memq current-minibuffer-command
                    lcr-minibuffer-disabled-commands)))
    (let ((minibuffer-completion-auto-choose nil))
      (minibuffer-completion-help)))
   (completion-in-region-mode
    (completion-help-at-point)))))

;; TODO consider completion-fail-discreetly
;; TODO consider after-change-functions instead of post-command-hook

;; Experimented with it, only this value prevents hangs in the ui during heavy
;; consing completion (e.g. java class completion in cider).
;;
;; I think the issue is that completion in emacs is creating multiple garbage
;; lists: for all completions, filtered, sorted, grouped, etc.
;; BUG this gets overridden by startup-module
(setq gc-cons-threshold (* 64 800000))

(defun lcr-after-change (&rest _)
  (when (and (memq this-command lcr-commands)
             (or completion-in-region-mode
                 (minibufferp)))
    ;; VERY important - prevents "hanging" while waiting for completions
    ;; Before this, whatever was printing "Making completion list..." was slow.
    ;; FIXME: there's `inhibit-quit' though... Something seems to be inhibiting
    ;; the keypress wakeup in cider completion...
    ;; (NOTE: completion-preview-mode has the same problem...)
    ;; HACK:
    ;; (setq gc-cons-threshold 100000000) seems to fix this.
    ;; The pauses are probably not due to while-no-input not working. I set
    ;; post-gc-hook and saw that gc was being triggered pretty much on every
    ;; keypress. This must have caused the slowness and tearing.
    (while-no-input (sit-for 0.05) (lcr-refresh))))

(defun kaspi/next-completion (&rest _)
  (when (or completion-in-region-mode (minibufferp))
    (minibuffer-next-completion)))

(define-minor-mode global-lcr-mode
  "Live Completion-In-Region Mode"
  :global t
  (cond
   (global-lcr-mode
    ;; NOTE For some reason, adding it to completion-setup-hook doesn't work...
    (advice-add 'display-completion-list :after 'kaspi/next-completion)
    (add-hook 'post-command-hook 'lcr-after-change))
   (t 
    (advice-remove 'display-completion-list 'kaspi/next-completion)
    (remove-hook 'post-command-hook 'lcr-after-change))))

(add-hook 'after-init-hook 'global-lcr-mode)

(define-key completion-in-region-mode-map (kbd "C-n") 'minibuffer-next-completion)
(define-key completion-in-region-mode-map (kbd "C-p") 'minibuffer-previous-completion)

(define-key minibuffer-mode-map (kbd "C-n") 'minibuffer-next-completion)
(define-key minibuffer-mode-map (kbd "C-p") 'minibuffer-previous-completion)

;;;; Testing own completion function:

(defun tmp/my-completion-function (query pred flag)
  (let* ((all '("foo" "boo" "far" "faz" "baz" "baf"))
         (left (cl-remove-if-not pred all))
         (exact nil))
    (pcase flag
      (`nil
       (message "NIL case")
       (let* ((matches 
               (mapcar
                (lambda (comp)
                  (when-let*
                      ((start (cl-search query comp))
                       (end (+ start (length query))))
                    (cl-subseq comp start end)))
                left))
              (matches (cl-remove-if #'null matches)))
         (cond
          ((null (cdr matches)) nil)
          ((cddr matches) (seq-first (sort matches #'string>)))
          ((cdr matches) (setf exact t) (cdr matches)))))
     (`t (message "T case") left)
     (`lambda (message "LAMBDA case") exact)
     (`(boundaries . ,suffix) (message "BOUNDARIES case") `(boundaries 0 . 0))
     (`metadata
      (message "METADATA case")
      `(metadata
        . ((category . tmp/bogus)
           (annotation-function . ,(cl-constantly " <Annot>"))
           (group-function . ,(lambda (candidate transform)
                                (if transform
                                    candidate
                                    "<Group>")))))))))


;; (completing-read "Select foo: " 'tmp/my-completion-function)
