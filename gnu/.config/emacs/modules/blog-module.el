;; -*- lexical-binding: t -*-

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
         :sitemap-filename "index.org"
         :sitemap-title "Kasper Gałkowski"
         :html-head-include-default-style nil
         :html-head-extra "<link rel=\"stylesheet\" href=\"files/blog.css\">"
         :html-link-up "/index.html"
         :html-link-home "/index.html"
         :html-preamble nil
         :html-postamble nil
         :html-doctype "html5"
         :html-html5-fancy t
         :html-self-link-headlines t
         :html-validation-link nil)
        ("images"
         :base-directory "~/blog/images/"
         :base-extension "jpg\\|gif\\|png"
         :publishing-directory "/ssh:jazajuk:/srv/blog/images/"
         :publishing-function org-publish-attachment)
        ("files"
         :base-directory "~/blog/files/"
         :base-extension "css"
         :publishing-directory "/ssh:jazajuk:/srv/blog/files/"
         :publishing-function org-publish-attachment)
        ("blog" :components ("posts" "images" "files"))))
