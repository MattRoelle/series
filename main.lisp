(defpackage :series
  (:use :common-lisp)
  (:export
    :dimension-sequence
    :arithmetic?
    :geometric?
    :sum
    :finite-summation
    :infinite-summation))

(in-package :series)

(defun dimension-sequence (procedure min-n max-n)
  "generates a sequence with the given domain and function"
  (map 'list
       #'(lambda (n)
           (funcall procedure n))
       (loop
         :for i
         :from min-n
         :to max-n
         :collect i)))

(defun arithmetic? (seq)
  "determines if the given sequence is arithmetic"
  (cond
    ((<= (length seq) 3)
     nil)
    (t
     (if (= (- (nth 1 seq) (nth 0 seq)) (- (nth 2 seq) (nth 1 seq)))
       t
       nil))))

(defun geometric? (seq)
  "determines if the given sequence is geometric"
  (cond
    ((<= (length seq) 3)
     nil)
    (t
     (if (= (/ (nth 1 seq) (nth 0 seq)) (/ (nth 2 seq) (nth 1 seq)))
       t
       nil))))

(defun sum (series)
  "calculates the total sum of the given sequence"
  (apply '+ series))

(defun finite-summation (series n1 n2)
  "calculates the sum of a sub section of the given sequence"
  (apply '+ (subseq series n1 n2)))

(defun infinite-summation (procedure)
  "calculates the sum of an infinite sequence.
   series must be geometric and have a common ratio of -1 < r < 1"
  (let ((r (/
             (funcall procedure 1)
             (funcall procedure 0))))
    (cond
      ((and
            (< r 1)
            (> r -1)) 
       (/
         (funcall procedure 1)
         (- 1 r)))
      (t
       'infinity))))
