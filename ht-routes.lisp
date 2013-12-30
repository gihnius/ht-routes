(defpackage :ht-routes
  (:use :cl :cl-ppcre :hunchentoot)
  (:export :*route-params*
           :map-routes))

(in-package :ht-routes)

(defvar *ht-routes* ())

(defvar *route-params* ())

(defparameter *route-param-scanner* (create-scanner "(:[^\\W]+)"))

(defparameter *default-param-regex* "([^\/]+)")

(defun get-param-names (path)
  "return param names from path \"/obj/:param1/:param2\" etc."
  (all-matches-as-strings *route-param-scanner* path))

(defun get-param-matches (route)
  "return the param and match-regex pairs from route"
  (let ((matches (mapcar #'read-from-string (get-param-names (car route))))
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

(defun get-method-handlers (route)
  "return method and handler pairs"
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
          (let ((it (getf (cdr route) (read-from-string p))))
            (when it (setf pattern (format nil "(~A)" it))))
          (setf newpath (regex-replace p newpath pattern))))
    (create-scanner (format nil "^~A$" newpath))))

(defmacro map-routes (&body routes)
  `(setq *ht-routes* (list ,@(loop for route in routes
                                collect `(list (make-path-scanner ',route) ',route)))))

(defun set-current-route-params (route values)
  "set the match url params into *route-params*"
  (let ((param-matches (get-param-matches route)))
    (setf *route-params* param-matches)
    (loop
       for v in values
       for (p r) on *route-params* by #'cddr
       do (setf (getf *route-params* p) v))))

(defun defunction (func)
  (if (fboundp func)
      func
      (setf (symbol-function func) #'(lambda()))))

;; override hunchentoot dispatcher by default
;; define a new acceptor class is recommended here
(defmethod acceptor-dispatch-request ((acceptor acceptor) request)
  (if (null *ht-routes*)
      (let ((path (and (acceptor-document-root acceptor)
                       (request-pathname request))))
        (if path
            (handle-static-file
             (merge-pathnames (if (equal "/" (script-name request)) #p"index.html" path)
                              (acceptor-document-root acceptor)))))
      (dolist (e *ht-routes*)
        (let ((scanner (car e))
              (route (cadr e)))
          (multiple-value-bind (match results)
              (scan-to-strings scanner (script-name*))
            (when results
              (set-current-route-params route (coerce results 'list)))
            (when match
              (return-from acceptor-dispatch-request
                (let ((method (request-method*)))
                  (loop for (route-method handler) on (get-method-handlers route) by #'cddr do
                       (when (eq method route-method)
                         (return-from acceptor-dispatch-request (funcall (defunction handler))))))))))))
  (setf (return-code* *reply*) +http-not-found+)
  (abort-request-handler))
