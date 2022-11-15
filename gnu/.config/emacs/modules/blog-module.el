;; -*- lexical-binding: t -*-

(setq org-publish-project-alist                                     
      '(("posts"                                                    
         :base-directory "~/blog/posts/"                            
         :base-extension "org"                                      
         :publishing-directory "/ssh:jazajuk:/srv/blog/posts/" 
         :publishing-function org-html-publish-to-html              
         :section-numbers nil                                       
         :with-toc nil                                              
         :auto-sitemap t                                            
         :html-preamble nil                                         
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
