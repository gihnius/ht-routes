;;;; ht-routes.lisp
;;;;
;;;; This file is part of the ht-routes library, released under MIT.
;;;; See file README.org for details.
;;;;
;;;; Author: Gihnius lyj <gihnius@gmail.com>
;;;;

(defpackage :ht-routes
  (:use :cl :cl-ppcre :hunchentoot)
  (:export :map-routes
           :*allow-head*
           :ht-acceptor
           :add-route))

(in-package :ht-routes)

;; method: http request method (keyword)
;; path: url def
(defstruct route
  method
  path
  path-scanner
  handler)

;; list of routes struct
(defvar *routes* ())

(defparameter *route-param-scanner* (create-scanner "(:[^\\W]+)"))

(defparameter *default-param-regex* "([^\/]+)")

;; default allow :HEAD method
(defvar *allow-head* t)

;; make path matching regex
(defun make-path-scanner (path params-matches)
  (let* ((newpath path)
         (param-names (get-param-names path))
         ;; default pattern for any params
         (pattern *default-param-regex*))
    (if param-names
        (dolist (p param-names)
          (let ((it (getf params-matches (read-from-string p))))
            (if it
                (setf newpath (regex-replace p newpath (format nil "(~A)" it)))
                (setf newpath (regex-replace p newpath pattern))))))
    (create-scanner (format nil "^~A$" newpath))))

;; add routes by template:
;; (method "path" handler params-matches)
;; (get "/books" show-all-books)
;; (get "/books/:id" show-book :id "\\d+")
;; (post "/books/update/:id" update-book :id "\\d+")

(defmacro map-routes (&body templates)
  `(setq *routes* (list ,@(loop for template in templates
                                collect `(gen-route ',template)))))

(defun gen-route (template)
  (destructuring-bind (method path handler &rest params-matches) template
    (make-route
     :method (read-from-string (concatenate 'string ":" (string method)))
     :path path
     :path-scanner (make-path-scanner path params-matches)
     :handler handler)))

(defun add-route (template)
  (push (gen-route template) *routes*))

(defun get-param-names (path)
  "return param names(':param1' ':param2') from path '/path/:param1/:param2' etc."
  (all-matches-as-strings *route-param-scanner* path))

(defun get-param-matches (path)
  "return the param and match-regex pairs from route"
  (let ((matches (mapcar #'read-from-string (get-param-names path)))
        (dummy ())
        (res ()))
    (dolist (m matches)
      (push nil dummy))
    (mapc #'(lambda (&rest e) (setq res (append res e))) matches dummy)
    res))

(defun match-handler (route request)
  (let ((params (get-param-matches (route-path route))))
    (multiple-value-bind (match results)
        (scan-to-strings (route-path-scanner route) (script-name request))
      (when results
        (loop
           for v in (coerce results 'list)
           for (p vv) on params by #'cddr
           do (setf (getf params p) v)))
      (if (or (and match *allow-head* (eq (request-method request) :HEAD))
              (and match (eq (route-method route) (request-method request))))
          (return-from match-handler (list (route-handler route) params))
          (return-from match-handler (list nil nil))))))

(defclass ht-acceptor (acceptor)
  ())

(defun defunction (func)
  (if (fboundp func)
      func
      #'(lambda()
          (format nil "Handler not implemented!"))))

(defmethod acceptor-dispatch-request ((acceptor ht-acceptor) request)
  (loop for route in *routes*
     for (action params) = (match-handler route request)
     when action return (if params
                            (funcall (defunction action) params)
                            (funcall (defunction action)))
     finally (call-next-method)))
