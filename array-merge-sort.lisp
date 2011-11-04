(defpackage array-merge-sort
  (:use common-lisp)
  (:shadow :common-lisp sort)
  (:export sort))
(in-package :array-merge-sort)

(declaim (inline merge-lists sort-impl))

(defun merge-arrays (array start1 end1 start2 end2 test key)
  (labels ((less-equal-than (i1 i2)
             (not (funcall test (funcall key (aref array i2)) (funcall key (aref array i1)))))
           (less-than (i1 i2)
             (funcall test (funcall key (aref array i1)) (funcall key (aref array i2))))
           (recur (i1 i2)
             (print (list array i1 i2))
             (loop FOR i = i1 THEN (1+ i)
                   WHILE (and (< i end1)
                              (less-equal-than i i2))
                   FINALLY
                   (setf i1 i))
             (when (>= i1 end1)
               (return-from recur))

             (print (list :l2 array i1 i2))
             (loop FOR i = i2 THEN (1+ i)
                   FOR c FROM 0
                   WHILE (and (< i end2)
                              (less-than i i1))
                   FINALLY
                   (print (list :l2-end i1 i2 i))
                   (loop FOR k1 = i1 THEN (1+ k1)
                         FOR k2 = i2 THEN (1+ k2)
                         WHILE (< k2 i)
                     DO
                     (incf i1)
                     (rotatef (aref array k1) (aref array k2)))
                   (when (>= i end2)
                     (return-from recur))
                   (let ((bk end1))
                     (setf end1 i)
                     (recur i2 i) ;; TODO: この後にマージを再開する必要がある。 交換したl1の最後の部分から
                   
                     (setf end1 bk)
                     (recur i1 start2))
                   )))
    (recur start1 start2)
    array))
                   
(defparameter *b* (sort (loop REPEAT (random 50) COLLECT (random 400)) #'<))
(defparameter *c* (sort (loop REPEAT (random 50) COLLECT (random 400)) #'<))

(defparameter *e* (concatenate 'vector *b* *c*))

(merge-arrays *e* 0 (length *b*) (length *b*) (length *e*) #'< #'identity)

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