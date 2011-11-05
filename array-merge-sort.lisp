(defpackage array-merge-sort
  (:use common-lisp)
  (:shadow :common-lisp sort)
  (:export sort))
(in-package :array-merge-sort)

(declaim (inline merge-arrays sort-impl))

(defun merge-arrays (array start1 end1 start2 end2 test key)
  (declare (fixnum start1 end1 start2 end2)
           (function test key)
           (simple-vector array))
  (labels ((less-equal-than (i1 i2)
             (not (funcall test (funcall key (aref array i2))
                           (funcall key (aref array i1)))))
           (less-than (i1 x)
             (funcall test (funcall key (aref array i1)) 
                      (funcall key x)))

           (merge1 (i1 e1 i2)
             (loop FOR i fixnum FROM i1 BELOW e1
                   WHILE (less-equal-than i i2)
                   FINALLY (return i)))

           (merge2 (i1 e1 i2 e2)
             (loop WITH x = (aref array i1)
                   FOR i fixnum FROM i2 BELOW e2
                   FOR k fixnum FROM i1 BELOW e1
                   WHILE (less-than i x)
               DO
               (rotatef (aref array i) (aref array k))
               FINALLY
               (return (values i k))))

           (recur (i1 e1 i2 e2)
  
             (let ((i1-mid (merge1 i1 e1 i2)))
               (when (>= i1-mid e1)
                 (return-from recur))
             
               (multiple-value-bind (i2-mid sorted-i) (merge2 i1-mid e1 i2 e2)
                 (unless (>= i2-mid e2)
                   (recur i2 i2-mid i2-mid e2))
                 (recur sorted-i e1 i2 e2)))))
    (declare (inline less-than less-equal-than))
    (recur start1 end1 start2 end2)
    array))
    
(declaim (ftype (function (vector function function) (values)) sort-impl))
(defun sort-impl (array test key)
  (declare (optimize (speed 3) (safety 2) (debug 2)))
  (labels ((recur (start end &aux (size (- end start)))
             (declare (optimize (speed 3) (safety 0) (debug 0))
                      (fixnum start end size)) 
             (unless (= 1 size)
               (let ((mid (+ start (ash size -1))))
                 (declare (fixnum mid))
                 (recur start mid)
                 (recur mid end)
                 (merge-arrays array start mid mid end test key))
               nil)))
    (when (plusp (length array))
      (recur 0 (length array)))
    (values)))

(defun sort (array test &key (key #'identity))
  (sort-impl array test key)
  array)
           
;(defparameter *b* (cl:sort (loop REPEAT (random 50) COLLECT (random 400)) #'<))
;(defparameter *c* (cl:sort (loop REPEAT (random 50) COLLECT (random 400)) #'<))

;(defparameter *e* (concatenate 'vector *b* *c*))

;(merge-arrays *e* 0 (length *b*) (length *b*) (length *e*) #'< #'identity)

