(defpackage :agnostic-lizard
  (:use :common-lisp)
  (:export
    #:metaenv
    #:walker-metaenv
    #:macro-walker-metaenv

    #:metaenv-macroexpand-all
    #:metaenv-map-macroexpand-all

    #:metaenv-function-like-entries
    #:metaenv-variable-like-entries
    #:metaenv-blocks
    #:metaenv-tags
    #:metaenv-toplevel

    #:metaenv-clone
    #:metaenv-clone-args

    #:separate-declarations
    #:separate-function-definition-parts
    #:separate-lambda-expression-parts

    #:function-information
    #:function-information-named
    #:function-information-name
    #:function-information-qualifiers
    #:function-information-arglist
    #:function-information-declarations
    #:function-information-body

    #:metaenv-ensure-name-from-environment
    #:metaenv-ensure-names-from-environment

    #:macroexpand-all
    #:walk-form

    #:macro-walk-form
    #:macro-macroexpand-all
    #:metaenv-macro-macroexpand-all

    ; helpers for reader tricks
    #:wrap-every-form-reader
    #:install-wrap-every-form-reader
    #:wrap-rest-of-input
    #:with-wrap-every-form-reader
    ))
