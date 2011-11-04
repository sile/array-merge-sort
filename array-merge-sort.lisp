(defpackage array-merge-sort
  (:use common-lisp)
  (:shadow :common-lisp sort)
  (:export sort))
(in-package :array-merge-sort)

(declaim (inline merge-lists sort-impl))

(defun merge-arrays (array start1 end1 start2 end2 test key)
  (labels ((less-equal-than (i1 i2)
             (not (funcall test (funcall key (aref array i2))
                           (funcall key (aref array i1)))))
           (less-than (i1 x)
             (funcall test (funcall key (aref array i1)) 
                      (funcall key x)))

           (merge1 (i1 e1 i2)
             (loop FOR i FROM i1 BELOW e1
                   WHILE (less-equal-than i i2)
                   FINALLY (return i)))

           (merge2 (i1 e1 i2 e2)
             (loop WITH x = (aref array i1)
                   FOR i FROM i2 BELOW e2
                   FOR c FROM 0
                   WHILE (and (< (+ c i1) e1)
                              (less-than i x))
               DO
               (rotatef (aref array i) (aref array (+ i1 c)))
               FINALLY
               (return (values i c))))

           (recur (i1 e1 i2 e2)
  (print array)
  (print (list :in i1 e1 i2 e2))
  
             (let ((i1-mid (merge1 i1 e1 i2)))
               (when (>= i1-mid e1)
                 (return-from recur i1-mid))
             
               (multiple-value-bind (i2-mid c) (merge2 i1-mid e1 i2 e2)
                 (when (>= i2-mid e2)
                   (return-from recur i1-mid))
                 
                 (let ((sorted-i (+ i1-mid c)))
                   (print (list :3 i1 sorted-i e1 i2 i2-mid e2))

                   (print (recur i2 i2-mid i2-mid e2))

                   (recur sorted-i e1 i2 e2)
                   #+C
                   (let ((end (recur i2 i2-mid i2-mid e2)))
                     (declare (ignore end))
                     (recur i1-mid e1 i2-mid e2)))))))
    (recur start1 end1 start2 end2)
    array))
                   
;(defparameter *b* (cl:sort (loop REPEAT (random 50) COLLECT (random 400)) #'<))
;(defparameter *c* (cl:sort (loop REPEAT (random 50) COLLECT (random 400)) #'<))

;(defparameter *e* (concatenate 'vector *b* *c*))

;(merge-arrays *e* 0 (length *b*) (length *b*) (length *e*) #'< #'identity)

#|
(defun merge-lists (head list1 list2 test key &aux (tail head))
  (macrolet ((less-equal-than (l1 l2)
               `(not (funcall test (funcall key (car ,l2)) (funcall key (car ,l1)))))
             (merge-one (l1 l2)
               `(unless (setf ,l1 (cdr (setf tail (setf (cdr tail) ,l1))))
                  (setf (cdr tail) ,l2)
                  (return (cdr head)))))
    (loop
     (if (less-equal-than list1 list2)
         (merge-one list1 list2)
       (merge-one list2 list1)))))

(declaim (ftype (function (list function function) list) sort-impl))
(defun sort-impl (list test key &aux (head (cons :head list)))
  (declare (optimize (speed 3) (safety 2) (debug 2))
           (dynamic-extent head))
  (labels ((recur (list size)
             (declare (optimize (speed 3) (safety 0) (debug 0))
                      (fixnum size)) 
             (if (= 1 size)
                 (values list (shiftf (cdr list) nil))
               (let ((half (ash size -1)))
                 (multiple-value-bind (list1 rest) (recur list half)
                   (multiple-value-bind (list2 rest) (recur rest (- size half))
                     (values (merge-lists head list1 list2 test key) rest)))))))
    (when list
      (values (recur list (length list))))))

(defun sort (list test &key (key #'identity) inline)
  (declare (ignore inline))
  (sort-impl list test key))

(define-compiler-macro sort (&whole form list test &key (key '#'identity) inline)
  (if inline
      `(sort-impl ,list ,test ,key)
    form))
|#