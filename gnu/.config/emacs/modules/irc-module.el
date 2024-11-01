;; -*- lexical-binding: t; -*-

(with-eval-after-load "erc"
  (add-to-list 'erc-modules 'bufbar)
  (setopt erc-hide-list '("JOIN" "PART" "QUIT"))
  (setopt erc-autojoin-channels-alist
    '(("Libera.Chat" "#commonlisp" "#lisp" "#ecl" "#abcl" "#lispcafe" "#lisp-pl")
      ("hackint" "#tvl"))))

(defun kaspi/irc ()
  (interactive)
  (erc-tls :server "irc.libera.chat" :port 6697 :nick user-login-name)
  (erc-tls :server "irc.hackint.org" :port 6697 :nick user-login-name))



 
