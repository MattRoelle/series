(defpackage :series
  (:use :common-lisp)
  (:export
    :dimension-sequence
    :arithmetic?
    :geometric?))

(in-package :series)

(defun dimension-sequence (procedure min-n max-n)
  (map 'list
       #'(lambda (n)
           (funcall procedure n))
       (loop
         :for i
         :from min-n
         :to max-n
         :collect i)))

(defun arithmetic? (seq)
  (cond
    ((<= (length seq) 3)
     nil)
    (t
     (if (= (- (nth 1 seq) (nth 0 seq)) (- (nth 2 seq) (nth 1 seq)))
       t
       nil))))

(defun geometric? (seq)
  (cond
    ((<= (length seq) 3)
     nil)
    (t
     (if (= (/ (nth 1 seq) (nth 0 seq)) (/ (nth 2 seq) (nth 1 seq)))
       t
       nil))))

(defun sum (series)
  (apply '+ series))

(defun finite-summation (series n1 n2)
  (apply '+ (subseq series n1 n2)))

(defun infinite-summation (procedure)
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
