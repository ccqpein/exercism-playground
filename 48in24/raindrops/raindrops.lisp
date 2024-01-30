(defpackage :raindrops
  (:use :cl)
  (:export :convert))

(in-package :raindrops)

;; (defun convert (n)
;;   "Converts a number to a string of raindrop sounds."
;;   (let ((l (list (zerop (mod n 3)) (zerop (mod n 5)) (zerop (mod n 7)))))
;;     (if (every #'null l)
;;         (format nil "~a" n)
;;         (format nil "~{~:[~;Pling~]~:[~;Plang~]~:[~;Plong~]~}" l))))

(defun convert (n)
  "Converts a number to a string of raindrop sounds."
  (let ((l (mapcar #'zerop (list (mod n 3) (mod n 5) (mod n 7)))))
    (apply #'format nil (if (every #'null l)
                            `("~a" ,n)
                            `("~{~:[~;Pling~]~:[~;Plang~]~:[~;Plong~]~}" ,l)))))
