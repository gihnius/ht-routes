(asdf:defsystem #:ht-routes
  :serial t
  :depends-on (#:cl-ppcre #:hunchentoot)
  :components ((:file "ht-routes")))

(asdf:defsystem #:ht-routes-test
  :serial t
  :depends-on (#:ht-routes #:rt)
  :components ((:file "test")))
