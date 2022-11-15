(asdf:defsystem
  ; The Lizard climbs and crawls the syntax tree, and it is
  ; implementation-agnostic
  :agnostic-lizard

  ; It is still a bit of a proof of concept — if there is an established
  ; opensource project that would benefit from shipping agnostic-lizard
  ; as opposed to using it as a debugging tool, adding more license options
  ; will be considered
  :licence "GPLv3+"

  :description
  "A portable code walker that makes a best effort to be correct in most cases"

  :author "Michael Raskin <38a938c2@rambler.ru>"
  :pathname "agnostic-lizard"
  :components
  ((:static-file "README")
   (:static-file "AUTHORS")
   (:static-file "COPYING")
   (:static-file "gpl-3.0.txt")
   (:static-file "examples.lisp")
   (:file "package")
   (:file "utils" :depends-on ("package"))
   (:file "env-wrappers" :depends-on ("package"))
   (:file "metaenv" :depends-on ("package" "env-wrappers" "utils"))
   (:file "lexenv" :depends-on ("package" "metaenv" "env-wrappers" "utils"))
   (:file "pure-expansion" :depends-on ("package" "utils" "metaenv"))
   (:file "special-form-handlers" :depends-on ("package" "utils" "metaenv" "pure-expansion"))
   (:file "generic-walking" :depends-on ("package" "utils" "metaenv" "pure-expansion"))
   (:file "macro-based-walking" :depends-on ("package" "utils" "metaenv" "pure-expansion"))
   (:file "local-variables" :depends-on ("package" "metaenv" "generic-walking"))
   (:file "wrapping-reader" :depends-on ("package"))
   (:file "debugger-hooks" :depends-on ("package" "metaenv" "generic-walking" "utils"))))

