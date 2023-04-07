(require 'asdf)

(ensure-directories-exist
 (merge-pathnames
  (make-pathname :directory '(:relative ".config" "emacs" "auto-save"))
  (user-homedir-pathname)))
