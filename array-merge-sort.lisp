(defpackage array-merge-sort
  (:use common-lisp)
  (:shadow :common-lisp sort)
  (:export sort))
(in-package :array-merge-sort)

(declaim (inline sort-impl block-swap ins2)
         (inline merge-arrays)
         (notinline ins))

(defun block-swap (array start1 end1 start2 end2)
  (loop FOR i fixnum FROM start2 BELOW end2
        FOR j fixnum FROM start1 BELOW end1
        DO
        (rotatef (aref array i) (aref array j))))

(defun ins (array start mid end)
  (declare (fixnum start mid end)
           (simple-vector array)
           (optimize (speed 3) (safety 0) (debug 0)))
  (when (or (= start mid)
            (= mid end))
    (return-from ins array))

  (loop FOR i fixnum FROM start BELOW mid
        FOR j fixnum FROM mid BELOW end
        DO
        (rotatef (aref array i) (aref array j))
        FINALLY
        (cond ((= j end) 
               (return (ins array i mid end)))
              (t
               (return (ins array mid (the fixnum (1+ j)) end))))))

(defun ins2 (array start1 end1 start2 end2)
  (declare (fixnum start1 end1 start2 end2))
  (if (<= end1 start2)
      (block-swap array start1 end1 start2 end2)
    (ins array start1 start2 end2)))

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
             (declare (fixnum i2)
                      (ignorable e1))
             (loop WITH x = (aref array i1)
                   FOR i fixnum FROM (1+ i2) BELOW e2
                   FOR k FROM (1+ i1) BELOW e1
                   WHILE (less-than i x)
                FINALLY
               (return i)))
           
           (impl (i1 e1 i2 e2)
             (let ((i1-mid (merge1 i1 e1 i2)))
               (if (< i1-mid e1)
                 (recur3 i1-mid e1 i2 i2 e2)
                 e1)))
               
           (recur3 (i1 e1 i2 i2-2 e2)
             (let ((i2-mid (merge2 i1 e1 i2-2 e2)))
               (recur i1 e1 i2 i2-mid e2)))

           (recur (i1 e1 i2 i2-mid e2 &aux (p 0) (i1-mid (+ i1 (- i2-mid i2))))
             (declare (fixnum i1 e1 i2 i1-mid i2-mid e2 p))
             (ins2 array i1 i1-mid i2 i2-mid)

             (if (<= i1-mid i2)
                 (progn 
                   (setf p (1- i2-mid))
                   (when (< i2-mid e2)
                     (let ((v (aref array (1- i2-mid)))
                           (v2 (impl i2 i2-mid i2-mid e2)))
                       (declare (ignorable v v2))
                       (setf p (position v array :start (- v2 1) :test #'eq))
                       ;; (setf p (- v2 1))
                       ))
                   (recur i1-mid e1 i2 (1+ p) e2))

               (if (< i2-mid e2)
                   (impl i1-mid i2-mid i2-mid e2)
                 e1))))

    (declare (inline less-than less-equal-than impl merge1 merge2))
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

#|
(defparameter *a* (coerce (loop REPEAT 300000 COLLECT (random 10000000)) 'vector))

(defparameter *a* (coerce (loop FOR i FROM 0 BELOW 300000 COLLECT i) 'vector))

(defparameter *c* 0)
(let ((l1 (copy-seq *a*))
      (l2 (copy-seq *a*)))
  (setf *c* 0)
  (values
   (equalp
    (time 
     (sb-impl::stable-sort-simple-vector 
      l1 (lambda (x y) #+C (incf *c*) (< x y)) #'identity))
    
    (time
     (array-merge-sort:sort 
      l2 (lambda (x y) #+C (incf *c*) (< x y)))))
   *c*))

(array-merge-sort::merge-arrays
 #(5 6 7 1 2 3) 0 3 3 6 #'< #'identity)

(defparameter *b* (sort (coerce (loop REPEAT 3000 COLLECT (random 10000000)) 'vector) #'<))
(defparameter *d* (sort (coerce (loop REPEAT 3000 COLLECT (random 10000000)) 'vector) #'<))

(let ()
  (setf *c* 0)
  (values
   (equalp
    (time 
     (merge 'vector (copy-seq *b*) (copy-seq *d*)
            (lambda (x y) #+C (incf *c*) (< x y))))
    
    (time
     (array-merge-sort::merge-arrays 
      (concatenate 'vector *b* *d*)
      0 (length *b*) (length *b*) (+ (length *b*) (length *d*))
      (lambda (x y) #-C (incf *c*) (< x y)) #'identity)))
   *c*))
|#