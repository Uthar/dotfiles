;; -*- lexical-binding: t -*-

(add-to-list 'load-path (concat +vendor-dir+ "org"))
(autoload 'org-rss-publish-to-rss "ox-rss")
(autoload 'org-publish-all "org"  "" t)

(defun kaspi/format-rss-feed (title list)
  (concat "#+TITLE: " title "\n\n"
          (org-list-to-subtree list nil '(:istart ""))))

(defun kaspi/org-find-property (file property project)
  (thread-last
    (org-publish-find-property file property project)
    (org-element-interpret-data)
    (org-no-properties)))

(defun kaspi/format-rss-entry (entry style project)
  (let ((title (kaspi/org-find-property entry :title project))
        (date (kaspi/org-find-property entry :date project))
        (author (kaspi/org-find-property entry :author project))
        (link (concat (file-name-sans-extension entry) ".html")))
    (with-temp-buffer
      (insert (format "* [[file:%s][%s]]\n" entry title))
      (insert (format ":PROPERTIES:\n"))
      (insert (format ":CUSTOM_ID: %s\n" (string-replace " " "_" title)))
      (insert (format ":RSS_PERMALINK: %s\n" (url-encode-url link)))
      (insert (format ":RSS_TITLE: %s\n" title))
      (insert (format ":PUBDATE: %s\n" date))
      (insert (format ":AUTHOR: %s\n" author))
      (insert (format ":END:\n"))
      (buffer-string))))

(setq org-publish-project-alist
      '(("posts"
         :base-directory "~/blog/posts/"
         :base-extension "org"
         :publishing-directory "/ssh:jazajuk:/srv/blog/"
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :with-toc nil
         :with-author nil
         :with-date nil
         :auto-sitemap t
         :sitemap-function kaspi/format-rss-feed
         :sitemap-format-entry kaspi/format-rss-entry
         :sitemap-filename "index.org"
         :sitemap-title "Kasper Gałkowski - Blog"
         :sitemap-sort-files anti-chronologically
         :html-head-include-default-style nil
         :html-head-extra "
           <link rel=\"stylesheet\" href=\"files/blog.css\">
           <link rel=\"icon\" type=\"image/svg+xml\" href=\"images/alien.svg\">
           <link rel=\"alternate\" type=\"application/rss+xml\" title=\"RSS feed\" href=\"index.xml\">
         "
         :html-link-up "/index.html"
         :html-link-home "/index.html"
         :html-preamble nil
         :html-postamble nil
         :html-divs ((preamble  "header" "top")
                     (content   "main"   "content")
                     (postamble "footer" "postamble"))
         :html-doctype "html5"
         :html-html5-fancy t
         :html-self-link-headlines nil
         :html-validation-link nil)
        ("rss"
         :base-directory "~/blog/posts/"
         :base-extension "org"
         :html-link-home "https://galkowski.dev/"
         :html-link-use-abs-url t
         :rss-extension "xml"
         :rss-image-url "https://galkowski.dev/images/alien.png"
         :publishing-directory "/ssh:jazajuk:/srv/blog/"
         :publishing-function org-rss-publish-to-rss
         :section-numbers nil
         :exclude ".*"            ;; To exclude all files...
         :include ("index.org")   ;; ... except index.org.
         :table-of-contents nil)
        ("images"
         :base-directory "~/blog/images/"
         :base-extension "png\\|svg"
         :publishing-directory "/ssh:jazajuk:/srv/blog/images/"
         :publishing-function org-publish-attachment)
        ("files"
         :base-directory "~/blog/files/"
         :base-extension "css"
         :publishing-directory "/ssh:jazajuk:/srv/blog/files/"
         :publishing-function org-publish-attachment)
        ("blog" :components ("posts" "rss" "images" "files"))))
