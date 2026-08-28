(defpackage pore-align/pca
  (:use #:cl)
  (:local-nicknames (#:util #:pore-align/util)
                    (#:em   #:entzauberte-matrices))
  (:export #:fit-pca
           #:transform-pca
           #:invert-pca
           #:restore-descriptors))
(in-package :pore-align/pca)

(serapeum:-> means ((util:fixed-entries #.util:+descriptor-length+))
             (values (simple-array single-float (#.util:+descriptor-length+))
                     &optional))
(defun means (descriptors)
  (declare (optimize (speed 3)))
  (let ((accum (make-array #.util:+descriptor-length+
                           :element-type 'vector-sum:sum-state
                           :initial-element (vector-sum:sum-state 0.0))))
    (util:loop-array (descriptors (i j))
      (setf (aref accum j)
            (vector-sum:add (aref accum j)
                            (aref descriptors i j))))
    (let ((samples (float (array-dimension descriptors 0))))
      (map '(vector single-float)
           (lambda (x)
             (declare (type vector-sum:sum-state/single-float x))
             (/ (vector-sum:state-sum x) samples))
           accum))))

(serapeum:-> op-means ((util:fixed-entries #.util:+descriptor-length+)
                       (simple-array single-float (#.util:+descriptor-length+))
                       (member :sub :add))
             (values (util:fixed-entries #.util:+descriptor-length+)
                     &optional))
(defun op-means (descriptors means what)
  (declare (optimize (speed 3)))
  (let ((result (make-array (array-dimensions descriptors)
                            :element-type 'single-float)))
    (ecase what
      (:sub
       (util:loop-array (result (i j))
         (setf (aref result i j)
               (- (aref descriptors i j) (aref means j)))))
      (:add
       (util:loop-array (result (i j))
         (setf (aref result i j)
               (+ (aref descriptors i j) (aref means j))))))
    result))

(serapeum:-> fit-pca
             ((util:fixed-entries #.util:+descriptor-length+) (single-float 0.0 1.0))
             (values (util:fixed-entries #.util:+descriptor-length+)
                     (simple-array single-float (#.util:+descriptor-length+)) &optional))
(defun fit-pca (descriptors explained-variance)
  "Return a matrix which projects vectors from the descriptor space to
a PCA space of lesser dimensionality. The parameter
@c(EXPLAINED-VARIANCE) controls dimensionality of that space."
  (declare (optimize (speed 3)))
  ;; # samples >= # features
  (assert (>= (array-dimension descriptors 0)
              (array-dimension descriptors 1)))
  (let* ((means (means descriptors))
         (centered (op-means descriptors means :sub)))
    (multiple-value-bind (u s vt)
        (em:svd centered :compact t)
      (declare (ignore u)
               ;; TODO: figure out why I need this
               (type (simple-array single-float) s))
      (let* ((samples (array-dimension descriptors 0))
             ;; explained variance
             (ev (map '(vector single-float)
                      (lambda (x)
                        (/ (expt x 2) (1- samples)))
                      s))
             (total-ev (vector-sum:sum ev))
             (ev-ratio (map '(vector single-float)
                            (lambda (x) (/ x total-ev))
                            ev))
             (cum-ratio (vector-sum:scan ev-ratio))
             (n-components (1+ (or (position-if
                                    (lambda (x) (< x explained-variance))
                                    cum-ratio :from-end t)
                                   0)))
             (result (make-array (list n-components util:+descriptor-length+)
                                 :element-type 'single-float)))
        (util:loop-array (result (i j))
          (setf (aref result i j)
                (aref vt i j)))
        (values result means)))))

(serapeum:-> transform-pca
             ((util:fixed-entries #.util:+descriptor-length+)
              (util:fixed-entries #.util:+descriptor-length+)
              (simple-array single-float (#.util:+descriptor-length+)))
             (values (simple-array single-float (* *)) &optional))
(defun transform-pca (descriptors vt means)
  "Apply a transform to the PCA space to descriptors. @c(VT) and
@c(MEANS) are obtained from @c(FIT-PCA)."
  (let ((centered (op-means descriptors means :sub)))
    (em:mult centered vt :tb t)))

(serapeum:-> invert-pca
             ((util:fixed-entries *)
              (util:fixed-entries #.util:+descriptor-length+)
              (simple-array single-float (#.util:+descriptor-length+)))
             (values (util:fixed-entries #.util:+descriptor-length+) &optional))
(defun invert-pca (pca vt means)
  "This is a (lossy) inversion of @c(TRANSFORM-PCA), i.e. it converts
vectors in the PCA space back into the descriptor space."
  (op-means (em:mult pca vt) means :add))

(serapeum:-> restore-descriptors ((simple-array single-float (* *))
                                  (util:fixed-entries #.util:+descriptor-length+)
                                  (simple-array single-float (#.util:+descriptor-length+))
                                  (simple-array single-float (* *))
                                  (util:fixed-entries #.util:+descriptor-length+)
                                  (simple-array single-float (#.util:+descriptor-length+)))
             (values (simple-array single-float (* *))
                     (simple-array single-float (* *))
                     &optional))
(defun restore-descriptors (d1 vt1 means1 d2 vt2 means2)
  "Convert two sets of descriptors to the same PCA space (one with
bigger dimensionality)."
  (if (> (array-dimension d1 1)
         (array-dimension d2 1))
      (values d1 (transform-pca (invert-pca d2 vt2 means2) vt1 means1))
      (values    (transform-pca (invert-pca d1 vt1 means1) vt2 means2) d2)))
