(defpackage :leap
  (:use :cl)
  (:export :leap-year-p))
(in-package :leap)

(defun leap-year-p (year)
  (if (zerop (mod year 4))
      (cond ((zerop (mod year 400))
             t)
            ((zerop (mod year 100))
             nil)
            (t t))
      nil))
