(in-package :with-filters)

(defparameter *filter-db* (make-hash-table :test #'equal))

(defun clear-filters ()
  "Clear internal filter database"
  (setf *filter-db* (make-hash-table :test #'equal)))

(defun list-filters ()
  "List filters defined in database"
  (format t "found ~d filters:~%" (hash-table-count *filter-db*))
  
  (dolist (name (sort (loop for key being the hash-key of *filter-db*
			 collect (string-downcase (format nil "~a" key)))
		      #'string-lessp))
    (format t "  ~a~%" name))
  )

(defun define-filter (name callback)
  "internal: push filter into database"
  (setf (gethash name *filter-db*) callback))

;; TODO- little description here
(defmacro deffilter ((name data-var) &body body)
  "define a filter with a name and a data variable"
  (let ((yield-symbol (intern "YIELD" *package*)))
	
    `(define-filter ,name (lambda (next ,data-var)
			    (declare (ignorable ,data-var))
			    (macrolet ((,yield-symbol (data) `(funcall next ,data)))
			      ,@body)))))
       

(defun invoke-filter (name data next)
  "internal: lookup filter and make that call"
  (multiple-value-bind (method found) (gethash name *filter-db*)
    (if found
	(funcall method next data)
	(error (format nil "filter '~s' not defined" name)))))

(defun codify-chain (chain value finally)
  "internal: take a chain of keywords and generate the nested
callback structure"
  (labels ((codify-callback (chain value)
	     (if chain
		 `(invoke-filter ,(car chain)
				 ,value
				 (lambda (,value)
				   ,(codify-callback (cdr chain)
						     value)))
		 `(funcall ,finally ,value))))
    
    (codify-callback chain value)))

;; TODO - these could be sped up by fetching the callbacks
;;   from the filter-db and invoking them here rather than calling
;;   invoke-filter each step but that would mean if filters were
;;   redefined (ie for debugging) then all the filter chain invocations
;;   would have to be re-compiled
(defmacro with-filter-chain ((value-var &rest chain) &body body)
  "wrap some code in a nested stack of callbacks"
  (codify-chain chain
		value-var
		`(lambda (,value-var)
		   (declare (ignorable ,value-var))
		   ,@body)))





