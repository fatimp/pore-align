(defpackage pore-align/match
  (:use #:cl)
  (:local-nicknames (#:util  #:pore-align/util)
                    (#:faiss #:cl-faiss))
  (:export #:match-descriptors))
(in-package :pore-align/match)

(deftype pair-array (type) `(simple-array ,type (* 2)))
(deftype dist-array    () '(pair-array single-float))
(deftype indices-array () '(pair-array (unsigned-byte 64)))

(serapeum:-> find-closest ((util:fixed-entries *)
                           (util:fixed-entries *))
             (values dist-array indices-array &optional))
(defun find-closest (s1 s2)
  (faiss:with-index (index (array-dimension s1 1) "Flat" :l2)
    (faiss:index-add    index s2)
    (faiss:index-search index s1 2)))

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
