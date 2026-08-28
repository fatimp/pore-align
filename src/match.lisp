(defpackage pore-align/match
  (:use #:cl)
  (:local-nicknames (#:util #:pore-align/util)
                    (#:ff   #:float-features))
  (:export #:match-descriptors))
(in-package :pore-align/match)

(cffi:define-foreign-library faiss
  (:unix  (:or "libfaiss_c.so"))
  (t (:default "libfaiss_c")))
(cffi:use-foreign-library faiss)

(defconstant +metric-l2+ 1)

(cffi:defcfun ("faiss_index_factory" index-factory) :int
  (index  :pointer)
  (d      :int)
  (descr  :string)
  ;; FIXME: enum
  (metric :int))

(cffi:defcfun ("faiss_Index_free" index-free) :void
  (index :pointer))

(cffi:defcfun ("faiss_Index_add" %index-add) :int
  (index :pointer)
  (n     :int64)
  (x     (:pointer :float)))

(cffi:defcfun ("faiss_Index_search" %index-search) :int
  (index   :pointer)
  (n       :int64)
  (x       (:pointer :float))
  (k       :int64)
  (dists   (:pointer :float))
  (indices (:pointer :int64)))

(cffi:defcfun ("faiss_get_last_error" last-error) :string)

(serapeum:-> new-index ((and (signed-byte 32) (integer 0)))
             (values sb-sys:system-area-pointer &optional))
(defun new-index (d)
  (cffi:with-foreign-object (pointer :pointer)
    (setf (cffi:mem-ref pointer :pointer)
          (cffi:null-pointer))
    (unless (zerop (index-factory pointer d "Flat" +metric-l2+))
      (error 'util:ffi-error
             :message (last-error)))
    (cffi:mem-ref pointer :pointer)))

(declaim (inline index-add))
(defun index-add (index n array)
  (unless (zerop (%index-add index n array))
    (error 'util:ffi-error
           :message (last-error))))

(declaim (inline index-search))
(defun index-search (index n x k dists indices)
  (unless (zerop (%index-search index n x k dists indices))
    (error 'util:ffi-error
           :message (last-error))))

(defmacro with-index ((index d) &body body)
  `(let ((,index (new-index ,d)))
     (unwind-protect
          (progn ,@body)
       (index-free ,index))))

(defmacro with-pointers-to-arrays (bindings &body body)
  (reduce
   (lambda (binding acc)
     (destructuring-bind (var array) binding
       `(cffi:with-pointer-to-vector-data (,var (sb-ext:array-storage-vector ,array))
          ,acc)))
    bindings
    :from-end t
    :initial-value `(progn ,@body)))

(deftype pair-array (type) `(simple-array ,type (* 2)))
(deftype dist-array    () '(pair-array single-float))
(deftype indices-array () '(pair-array (unsigned-byte 64)))

(serapeum:-> find-closest ((util:fixed-entries *)
                           (util:fixed-entries *))
             (values dist-array indices-array &optional))
(defun find-closest (s1 s2)
  (assert (= (array-dimension s1 1)
             (array-dimension s2 1)))
  (let* ((nqs (array-dimension s1 0))
         (dists   (make-array (list nqs 2) :element-type 'single-float))
         (indices (make-array (list nqs 2) :element-type '(unsigned-byte 64))))
    (with-index (index (array-dimension s1 1))
      (with-pointers-to-arrays ((s1-ptr      s1)
                                (s2-ptr      s2)
                                (dists-ptr   dists)
                                (indices-ptr indices))
        (index-add    index (array-dimension s2 0) s2-ptr)
        (index-search index (array-dimension s1 0) s1-ptr 2 dists-ptr indices-ptr)))
    (values dists indices)))

(serapeum:-> match-descriptors ((util:fixed-entries #.util:+descriptor-offset+)
                                (util:fixed-entries #.util:+descriptor-offset+)
                                (util:fixed-entries *)
                                (util:fixed-entries *)
                                &optional (single-float 0.0))
             (values list &optional))
(defun match-descriptors (c1 c2 d1 d2 &optional (c 1.2))
  "Find matches between two sets of descriptors. The parameter @c(C)
controls what we treat as a match. Bigger values result in a lesser
number of more stable matches. \\(C_i\\) is an array of keypoint
coordinates and \\(D_i\\) is an array of corresponding descriptors."
  (declare (optimize (speed 3)))
  (flet ((row (xs i)
           (let ((dim (array-dimension xs 1)))
             (make-array dim
                         :element-type 'single-float
                         :initial-contents (loop for j below dim collect
                                                 (aref xs i j))))))
    ;; FIND-CLOSEST returns squared distances
    (let (matches (c (expt c 2)))
      (multiple-value-bind (dists indices)
          (find-closest d1 d2)
        (loop for i below (array-dimension dists 0)
              for d1 = (aref dists i 0)
              for d2 = (aref dists i 1)
              when (< (* d1 c) d2) do
              (push (cons (row c1 i)
                          (row c2 (aref indices i 0)))
                    matches)))
      matches)))
