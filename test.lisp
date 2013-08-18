;; test using rt
(ql:quickload :rt)
(load "ht-routes.lisp")
(in-package :ht-routes)

(defvar route1 '("/" :get index))
(defvar route2 '("/path/to/file" :get get-file))
(defvar route3 '("/books/:id" :get show-book :id "\\d+"))
(defvar route4 '("/books/:authors" :get show-authors))
(defvar route5 '("/test/:param1/and/:param2" :get show-params :param2 "\\d+"))
(defparameter routes (list route1 route2 route3 route4 route5))

(rt:deftest test-can-get-params-names
  (dolist (r routes)
    (get-param-names (car r)))
  nil)
(rt:deftest test-can-get-params-matches
  (dolist (r routes)
    (get-param-matches r))
  nil)
(rt:deftest test-can-get-method-handlers
  (dolist (r routes)
    (get-method-handlers r))
  nil)
(rt:deftest test-can-make-path-scanner
  (dolist (r routes)
    (make-path-scanner r))
  nil)

(rt:deftest test-scanner-works-1
    (scan-to-strings (make-path-scanner route1) (car route1))
  "/" #())

(rt:deftest test-scanner-works-2
    (scan-to-strings (make-path-scanner route2) (car route2))
  "/path/to/file" #())

(rt:deftest test-scanner-works-3
    (scan-to-strings (make-path-scanner route3) (car route3))
  nil)
(rt:deftest test-scanner-works-3-1
    (scan-to-strings (make-path-scanner route3) "/books/123")
  "/books/123" #("123"))

(rt:deftest test-scanner-works-4
    (scan-to-strings (make-path-scanner route4) "/books/common-lisp")
  "/books/common-lisp" #("common-lisp"))
(rt:deftest test-scanner-works-4-1
    (scan-to-strings (make-path-scanner route4) "/books/wtf123")
  "/books/wtf123" #("wtf123"))
(rt:deftest test-scanner-works-4-2
    (scan-to-strings (make-path-scanner route4) "/books/1984")
  "/books/1984" #("1984"))

(rt:deftest test-scanner-works-5
    (scan-to-strings (make-path-scanner route5) "/test/hello/and/world")
  nil)
(rt:deftest test-scanner-works-5-2
    (scan-to-strings (make-path-scanner route5) "/test/hello/and/888")
  "/test/hello/and/888" #("hello" "888"))

(rt:do-tests)

;;; manual browser tests
(in-package :cl-user)

(defvar *ht* (make-instance 'hunchentoot:acceptor :port 3001))

;; (hunchentoot:start *ht*)

(ht-routes:map-routes
  ("/" :get index)
  ("/test/:param1/and/:param2" :get test-params)
  ("/params/:id" :get show-params :id "\\d+")
  ("/threads/:id/update" :post update-test :id "\\d+"))

(defun index ()
  (format nil "route-params: ~S method: ~S" ht-routes:*route-params* (hunchentoot:request-method*)))

(defun test-params ()
    (format nil "~S ~S route-params: ~S method: ~S" (getf ht-routes:*route-params* :param1) (getf ht-routes:*route-params* :param2) ht-routes:*route-params* (hunchentoot:request-method*)))

(defun show-params ()
  (format nil "~S route-params: ~S method: ~S" (getf ht-routes:*route-params* :id) ht-routes:*route-params* (hunchentoot:request-method*)))

;; post
;; curl -X POST
(defun update-test ()
  (format nil "~S route-params: ~S method: ~S" (getf ht-routes:*route-params* :id) ht-routes:*route-params* (hunchentoot:request-method*)))


;; (hunchentoot:stop *ht*)
