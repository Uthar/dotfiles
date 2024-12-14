;; -*- lexical-binding: t -*-

(defun kaspi/recentf-save-file-p (file)
  (string= file (expand-file-name recentf-save-file)))

(defun kaspi/recentf-save-current-buffer ()
  (let ((file-name (buffer-file-name (current-buffer)))
        (inhibit-message t))
    (when file-name
      (recentf-add-file file-name)
      (recentf-cleanup)
      (recentf-save-list))))

(with-eval-after-load 'recentf

  ;; Don't care about the recentf file itself.
  (add-to-list 'recentf-exclude 'kaspi/recentf-save-file-p)

  ;; Dont't care about fossil commit message files.
  (add-to-list 'recentf-exclude (regexp-opt '("ci-comment-")))

  ;; Save files to the recent list immediately when visiting
  ;; them. This is better in that recent files are not lost in the
  ;; event of a crash.
  ;; (add-hook 'buffer-list-update-hook 'kaspi/recentf-save-current-buffer)

  )
