(defpackage :protein-translation
  (:use :cl)
  (:export :proteins
           :invalid-protein))

(in-package :protein-translation)

(defparameter *table*
  '((("AUG")        "Methionine")
    (("UUU" "UUC")	"Phenylalanine")
    (("UUA" "UUG")	"Leucine")
    (("UCU" "UCC" "UCA" "UCG")	"Serine")
    (("UAU" "UAC")	"Tyrosine")
    (("UGU" "UGC")	"Cysteine")
    (("UGG")        "Tryptophan")
    (("UAA" "UAG" "UGA") "STOP"))
  )

(define-condition invalid-protein () ())

(defun proteins (strand)
  (loop
    with cache = '()
    and result = '()
    for c across strand
    do (push c cache)
    ;;do (format t "~a~%" cache)
    if (= 3 (length cache))
      do (let ((a (concatenate 'string (reverse cache))))
           (loop for (p target) in *table*
                 if (member a p :test #'string=)
                   do (if (string= target "STOP")
                          (return-from proteins (reverse result))
                          (push target result))
                 ))
      and do (setf cache nil)
    finally (if (or (= 0 (length result)) (> (length cache) 0))
                (signal 'invalid-protein)
                (return (reverse result)))
    ))
