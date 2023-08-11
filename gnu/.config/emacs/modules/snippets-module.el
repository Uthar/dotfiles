;; -*- lexical-binding: t; -*-

(defun kaspi/org-src-block ()
  (interactive)
  (let ((block (concat "#+BEGIN_SRC "
                       "\n#+END_SRC")))
    (insert block)
    (forward-line -1)
    (end-of-line)))
