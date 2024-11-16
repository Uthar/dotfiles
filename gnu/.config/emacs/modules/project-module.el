;; -*- lexical-binding: t; -*-

;; Obsługa lokalnych projektów nieśledzonych vc. Przydaje się na przykład do
;; przeglądania źródeł rozpakowanych z tarballa, ponieważ etags-regen-mode szuka
;; project-root, który domyślnie działa to tylko dla repozytoriów.

(defun kaspi/project-try-.project (dir)
  (when-let* ((.project (locate-dominating-file dir ".project")))
    (list '.project (file-name-directory .project))))

(with-eval-after-load "project"
  (add-to-list 'project-find-functions 'kaspi/project-try-.project)

  (cl-defmethod project-root ((project (head .project)))
    (nth 1 project)))

(setq etags-regen-tags-file
  (lambda (root)
    (let ((dir (file-name-concat "~/.cache/etags" (file-truename root))))
      (make-directory dir t)
      (expand-file-name "TAGS" dir))))
