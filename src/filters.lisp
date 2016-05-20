(in-package :with-filters)

(defparameter *filter-db* (make-hash-table :test #'equal))

(defun clear-filters ()
  (setf *filter-db* (make-hash-table :test #'equal)))

(defun list-filters ()
  (format t "found ~d filters:~%" (hash-table-count *filter-db*))
  
  (dolist (name (sort (loop for key being the hash-key of *filter-db*
			 collect (string-downcase (format nil "~a" key)))
		      #'string-lessp))
    (format t "  ~a~%" name))
  )

(defun define-filter (name callback)
  (setf (gethash name *filter-db*) callback))

;; TODO- little description here
(defmacro deffilter ((name data-var) &body body)
  `(define-filter ,name (lambda (next ,data-var)
			  (declare (ignorable ,data-var))
			  (macrolet ((yield (data) `(funcall next ,data)))
			    ,@body))))
       



(defun invoke-filter (name data next)
  (multiple-value-bind (method found) (gethash name *filter-db*)
    (if found
	(funcall method next data)))
  )

(defun codify-chain (chain value finally)
  (labels ((codify-callback (chain value)
	     (if chain
		 `(invoke-filter ,(car chain)
				 ,value
				 (lambda (,value)
				   ,(codify-callback (cdr chain)
						     value)))
		 `(funcall ,finally ,value))))
    
    (codify-callback chain value))
  )

;; TODO - these could be sped up by fetching the callbacks
;;   from the filter-db and invoking them here rather than calling
;;   invoke-filter each step but that would mean if filters were
;;   redefined (ie for debugging) then all the filter chain invocations
;;   would have to be re-compiled
(defmacro with-filter-chain ((value-var &rest chain) &body body)
  (codify-chain chain
		value-var
		`(lambda (,value-var)
		   (declare (ignorable ,value-var))
		   ,@body)))

;;
;; demo
;;

(clear-filters)

(deffilter (:outer value)
  (format t "outer start (value=~a) ~%" value)
  (yield (* value 2))
  (format t "outer end (value=~a)~%" value))

(deffilter (:inner value)
  (format t "inner start (value=~a)~%" value)
  (yield (* value 3))
  (format t "inner end (value=~a)~%" value))

(defun test-filter ()
  (let ((value 2))
    (with-filter-chain (value :outer :inner)
      (format t "CODE  value=~a~%" value))))


#|

(in-package :cl-user)

(defpackage :filters
  (:use :cl))

(in-package :filters)

(defparameter *filter-db* (make-hash-table :test #'equal))

(defun clear-filters ()
  (setf *filter-db* (make-hash-table :test #'equal)))

(defun list-filters ()
  (format t "found ~d filters:~%" (hash-table-count *filter-db*))
  
  (dolist (name (sort (loop for key being the hash-key of *filter-db*
			 collect (string-downcase (format nil "~a" key)))
		      #'string-lessp))
    (format t "  ~a~%" name))
  )

(defun define-filter (name callback)
  (setf (gethash name *filter-db*) callback))

;; TODO- little description here
(defmacro deffilter ((name) &body body)
  `(define-filter ,name (lambda (next)
			  (macrolet ((yield () `(funcall next)))
			    ,@body))))
       



(defun invoke-filter (name next)
  (multiple-value-bind (method found) (gethash name *filter-db*)
    (if found
	(funcall method next)))
  )

(defun codify-chain (chain finally)
  (labels ((codify-callback (chain)
	     (if chain
		 `(invoke-filter ,(car chain) (lambda ()
						,(codify-callback (cdr chain))))
		 `(funcall ,finally))))
    (codify-callback chain))
  )

;; TODO - these could be sped up by fetching the callbacks
;;   from the filter-db and invoking them here rather than calling
;;   invoke-filter each step but that would mean if filters were
;;   redefined (ie for debugging) then all the filter chain invocations
;;   would have to be re-compiled
(defmacro with-filter-chain ((&rest chain) &body body)
  (codify-chain chain `(lambda () ,@body)))

;;
;; demo
;;

(clear-filters)

(deffilter (:outer)
  (format t "outer start~%")
  (yield)
  (format t "outer end~%"))

(deffilter (:inner)
  (format t "inner start~%")
  (yield)
  (format t "inner end~%"))

(defun test-filter ()
  (with-filter-chain (:outer :inner)
    (format t "CODE~%")))


|#
