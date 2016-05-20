(in-package :cl-user)

(defpackage :filter-demo
  (:use :cl :with-filters))

(in-package :filter-demo)

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
