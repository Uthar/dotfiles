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

;; "http://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz"
(with-eval-after-load 'eglot
  (rplacd (assoc 'java-mode eglot-server-programs)
          '("~/.local/share/jdtls/bin/jdtls"
            "-configuration" "~/.local/share/jdtls/config_linux"
            "-data" "~/.cache/jdtls")))

;; (with-eval-after-load 'eglot
;;   (rplacd (assoc 'java-mode eglot-server-programs)
;;           '("nbcode"
;;             "--start-java-language-server=stdio"
;;             :initializationOptions
;;             (:nbcodeCapabilities
;;              (:statusBarMessageSupport nil
;;               :testResultsSupport nil
;;               :showHtmlPageSupport nil
;;               :wantsJavaSupport t
;;               :wantsGroovySupport t)))))

;; (setq-default eglot-workspace-configuration
;;               '(:netbeans.javadoc.load.timeout 10000
;;                 :netbeans.java.onSave.organizeImports :json-true))

