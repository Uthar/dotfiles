;; -*- lexical-binding: t -*-

(defun kaspi/c-lineup-java-toplevel-class (langelem) 
  (save-excursion
    (goto-char (c-langelem-pos langelem))
    (ignore-errors
      (backward-up-list)
      c-basic-offset)))

(add-hook 'java-mode-hook
  (lambda ()
    ;; Don't indent anonymous classes twice
    (c-set-offset 'inexpr-class 0)
    ;; Don't indent members of the top-level class
    (c-set-offset 'inclass 'kaspi/c-lineup-java-toplevel-class)))

(with-eval-after-load 'eglot
  (cl-defmethod eglot-execute-command
    (_server (_cmd (eql java.apply.workspaceEdit)) arguments)
    (mapc #'eglot--apply-workspace-edit arguments)))
