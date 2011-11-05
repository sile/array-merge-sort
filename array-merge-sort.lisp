(defpackage array-merge-sort
  (:use common-lisp)
  (:shadow :common-lisp sort)
  (:export sort))
(in-package :array-merge-sort)

(declaim (inline sort-impl block-swap)
         (inline merge-arrays))

(defun block-swap (array start1 end1 start2 end2)
;  (print (list :swap start1 end1 start2 end2))
  (loop FOR i fixnum FROM start2 BELOW end2
        FOR j fixnum FROM start1 BELOW end1
        DO
        (rotatef (aref array i) (aref array j))
        FINALLY
        (when (= i end2)
          (incf j))
;        (print (list :return j i))
        (return (values j i))))

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
             (declare (fixnum i2))
             (loop WITH x = (aref array i1)
                   FOR i fixnum FROM (1+ i2) BELOW e2
                   FOR k FROM (1+ i1) BELOW e1
                   WHILE (less-than i x)
               FINALLY
               (return i)))
           
           (impl (i1 e1 i2 e2)
             (let ((i1-mid (merge1 i1 e1 i2)))
               (when (< i1-mid e1)
                 (recur2 i1-mid e1 i2 e2))))
               
           (recur2 (i1 e1 i2 e2)
             (let ((i2-mid (merge2 i1 e1 i2 e2)))
               (recur i1 e1 i2 i2-mid e2)))

           (recur3 (i1 e1 i2 i2-2 e2)
             (let ((i2-mid (merge2 i1 e1 i2-2 e2)))
               (recur i1 e1 i2 i2-mid e2)))

           (recur (i1 e1 i2 i2-mid e2 &aux (p 0))
;             (print (list i1 e1 i2 i2-mid e2))
             (multiple-value-bind (b1 b2)
                                  (block-swap array i1 e1 i2 i2-mid)
               (declare (fixnum b1 b2 e1 e2 p))
;               (print (list b1 b2))
               (setf p (1- b2))
               (when (< b2 e2)
                 (let ((v (aref array (1- b2))))
                   (impl i2 b2 b2 e2)
                   (setf p (position v array :start (1- b2) :test #'eq))))

               (when (< b1 e1)
                 (recur3 b1 e1 i2 p e2)))))

    (declare (inline less-than less-equal-than merge1 merge2))
    (impl start1 end1 start2 end2)
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

