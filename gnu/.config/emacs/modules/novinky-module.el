;; -*- lexical-binding: t; -*-

;;;; novinky

(setq newsticker-url-list-defaults nil)
(setq newsticker-url-list
  '(("SBCL" "https://sourceforge.net/projects/sbcl/rss?path=/sbcl")
    ("Quicklisp" "http://blog.quicklisp.org/feeds/posts/default")
    ("NixOS" "https://nixos.org/blog/announcements-rss.xml")
    ("Emacs NEWS" "https://git.savannah.gnu.org/cgit/emacs.git/atom/etc/NEWS?h=master")
    ("ECL" "https://common-lisp.net/project/ecl/rss.xml")
    ("Clojure" "https://clojure.org/feed.xml")
    ("ABCL" "https://gitlab.common-lisp.net/abcl/abcl.atom")))
(setq newsticker-automatically-mark-items-as-old nil)
(setq newsticker-obsolete-item-max-age most-positive-fixnum)
