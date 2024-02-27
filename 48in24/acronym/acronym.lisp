(defpackage :acronym
  (:use :cl)
  (:export :acronym))

(in-package :acronym)

(defun acronym (str)
  "Returns the acronym for a noun of tech jargon."
  (let (x)
    (setf x (loop with aa = '()
                  for c in (concatenate 'list str)
                  if (and (char/= #\  c) (char/= #\- c))
                    do (push c aa)
                  else
                    collect (reverse aa) into bb
                    and do (setf aa '())
                  finally (progn (if aa (setf bb (append bb (list (reverse aa)))))
                                 (return bb))))
    (concatenate 'string (mapcar (lambda (w) (char-upcase (car w))) x))))
