
(defpackage :ht-routes
  (:use :cl :cl-ppcre :hunchentoot)
  (:export :*route-params*
           :map-routes))

(in-package :ht-routes)

(defvar *ht-routes* ())

(defvar *route-params* ())

(defparameter *route-param-scanner* (create-scanner "(:[^\\W]+)"))

(defparameter *default-param-regex* "([^\/]+)")

(defun strcat (&rest items)
  (format nil "~{~A~}" items))

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

;; return params name from path "/obj/:param1/:param2" etc.
(defun get-param-names (path)
  (all-matches-as-strings *route-param-scanner* path))

;; return the param and match-regex pairs from route
(defun get-param-matches (route)
  (let ((matches (map 'list #'read-from-string (get-param-names (car route))))
        (dummy ())
        (res ()))
    ;; keep res store the params from matches in order
    (dolist (m matches)
      (push nil dummy))
    (mapc #'(lambda (&rest e) (setq res (append res e))) matches dummy)
    (loop
       for p in matches
       for v = (getf (cdr route) p)
       do (setf (getf res p) v))
    res))

;; return methods and handlers pairs
(defun get-method-handlers (route)
  (let ((res (cdr route))
        (m (get-param-matches route)))
    (dolist (e m)
      (if (member e res :test #'equalp)
          (setq res (remove e res))))
    res))

(defun make-path-scanner (route)
  (let* ((path (car route))
         (newpath path)
         (param-names (get-param-names path))
         ;; default pattern for any params
         (pattern *default-param-regex*))
    (if param-names
        (dolist (p param-names)
          (aif (getf (cdr route) (read-from-string p))
               (setf pattern (strcat "(" it ")")))
          (setf newpath (regex-replace p newpath pattern))))
    (create-scanner (strcat "^" newpath "$"))))

#|
(map-routes
  ("/" :get index)
  ("/test/:param1/and/:param2" :get test-params)
  ("/params/:id" :get show-params :id "\\d+")
  ("/threads/:id/update" :post update-test :id "\\d+"))
|#

(defmacro map-routes (&body routes)
  `(setq *ht-routes* (list ,@(loop for route in routes
                                collect `(list (make-path-scanner ',route) ',route)))))

;; set the match url params into *route-params*
(defun set-current-route-params (route values)
  (let ((param-matches (get-param-matches route)))
    (setf *route-params* param-matches)
    (loop
       for v in values
       for (p r) on *route-params* by #'cddr
       do (setf (getf *route-params* p) v))))

(defun defunction (func)
  (if (fboundp func)
      func
      (setf (symbol-function func) #'(lambda() ))))

(defmethod acceptor-dispatch-request ((acceptor acceptor) request)
  (dolist (e *ht-routes*)
    (let ((scanner (car e))
          (route (car (cdr e))))
      (multiple-value-bind (match results)
          (scan-to-strings scanner (script-name*))
        (when results
          (set-current-route-params route (coerce results 'list)))
        (when match
          (return-from acceptor-dispatch-request
            (let ((method (request-method*)))
              (loop for (route-method handler) on (get-method-handlers route) by #'cddr do
                   (when (eq method route-method)
                     (return-from acceptor-dispatch-request (funcall (defunction handler))))
                 finally
                   (setf (return-code* *reply*) +http-not-found+)
                   (abort-request-handler)))))))))
