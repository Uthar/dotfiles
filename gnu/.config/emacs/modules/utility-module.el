;; -*- lexical-binding: t -*-

;; Like 'comment-or-uncomment-region', but doesn't unmark the region
;; afterwards, so that there's no need to call 'evil-visual-restore'.
;;
;; Also comments the current line when there is no region.
(defun kaspi/comment-or-uncomment ()
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
  (setq deactivate-mark nil))

(defun kaspi/x-copy ()
  (interactive)
  (when (region-active-p)
    (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
    (message "Yanked region to clipboard")
    (deactivate-mark)))

(defun kaspi/x-paste ()
  (interactive)
  (insert (shell-command-to-string "xsel -o -b")))

(defun kaspi/copy-buffer-file-name ()
  (interactive)
  (let ((name (buffer-file-name)))
    (kill-new name)
    (message name)))

(defun kaspi/select-or-exit-minibuffer ()
  (interactive)
  (if current-prefix-arg
      (exit-minibuffer))
      (select-window (minibuffer-window)))

(defun kaspi/noop (&rest _))

(defun kaspi/bash ()
  (interactive)
  (ansi-term "bash"))

(defun kaspi/xdg-open ()
  (interactive)
  (start-process "xdg-open" nil "xdg-open" (ffap-string-at-point)))

(defun kaspi/open-init-file ()
  (interactive)
  (find-file user-init-file))

(defun kaspi/reload-init-file ()
  (interactive)
  (load-file user-init-file))

(defun kaspi/copy-line ()
  (interactive)
  (prog1 (kill-ring-save (line-beginning-position) (line-end-position))
    (pulse-momentary-highlight-one-line)))

(defun kaspi/copy-line* ()
  (interactive)
  (let ((beg (progn
               (line-beginning-position)
               (back-to-indentation)
               (point)))
        (end (line-end-position)))
    (prog1 (kill-ring-save beg end)
      (pulse-momentary-highlight-region beg end))))

(defvar kaspi/duplicate-line-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" 'duplicate-line)
    (define-key map "n" 'move-lines-down)
    (define-key map "p" 'move-lines-up)
    map))

(put 'duplicate-line 'repeat-map 'kaspi/duplicate-line-repeat-map)

(defun kaspi/ignore-arguments (f)
  (lambda (&rest _)
    (interactive)
    (funcall f)))

(advice-add 'duplicate-line :after (kaspi/ignore-arguments 'next-line))

(defun kaspi/back-to-indentation* ()
  (interactive)
  (kill-region (point) (save-excursion
                         (search-forward-regexp "[ ]*")
                         (point))))

(global-set-key (kbd "C-c l m") 'kaspi/back-to-indentation*)
(global-set-key (kbd "M-M") 'kaspi/back-to-indentation*)

(defun kaspi/kill-line* ()
  (interactive)
  (save-excursion
    (goto-char (line-beginning-position))
    (let ((kill-whole-line t))
      (kill-line))))

(global-set-key (kbd "C-c l k") 'kaspi/kill-line*)
(global-set-key (kbd "C-S-k") 'kaspi/kill-line*)

(defvar kaspi/kill-line-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map "k" 'kaspi/kill-line*)
    map))

(put 'kaspi/kill-line* 'repeat-map 'kaspi/kill-line-repeat-map)

(add-to-list 'load-path (concat +vendor-dir+ "move-lines"))
(autoload 'move-lines-up "move-lines")
(autoload 'move-lines-down "move-lines")

(defvar kaspi/move-lines-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map "p" 'move-lines-up)
    (define-key map "n" 'move-lines-down)
    map))

(global-set-key (kbd "C-c l p") 'move-lines-up)
(global-set-key (kbd "C-c l n") 'move-lines-down)

(put 'move-lines-up 'repeat-map 'kaspi/move-lines-repeat-map)
(put 'move-lines-down 'repeat-map 'kaspi/move-lines-repeat-map)

(defun kaspi/paste-passwd ()
  "Wczytaj hasło z clipboard, usuwając potem wszelkie ślady po nim z pamięci."
  (interactive)
  (let ((passwd (funcall interprogram-paste-function)))
    (when passwd
      ;; Zapobiegawczo, żeby M-y nie wrzucało ochoczo haseł do kill ringu.
      (funcall interprogram-cut-function "")
      (unwind-protect
          (insert passwd)
        (clear-string passwd)))))

(defun kaspi/copy-passwd ()
  "Po prostu skopiuj region do systemowego schowka. Nie do kill ringa."
  (interactive)
  (let ((password (buffer-substring-no-properties (region-beginning) (region-end))))
    (unwind-protect
        (funcall interprogram-cut-function password)
      ;; żeby dało się paste-passwd
      (setq gui-last-cut-in-clipboard nil)
      (deactivate-mark)
      (message "Clearing clipboard in 30 seconds")
      (run-at-time 30 nil (lambda ()
                            (unwind-protect
                                (funcall interprogram-cut-function "")
                              (clear-string password)
                              (message "Cleared clipboard")))))))

;; upewniam się żeby tego nigdy nie włączyli
(setq save-interprogram-paste-before-kill nil)

(global-set-key (kbd "C-M-y") 'kaspi/paste-passwd)
(global-set-key (kbd "C-M-w") 'kaspi/copy-passwd)
