(defpackage pore-align/transform
  (:use #:cl)
  (:local-nicknames (#:ff   #:float-features)
                    (#:em   #:entzauberte-matrices)
                    (#:util #:pore-align/util))
  (:export #:rigid-transform-fit
           #:ransac
           #:ransac-result
           #:ransac-result-transform
           #:ransac-result-error
           #:ransac-result-inliers
           #:affine-transform
           #:affine-rotation
           #:affine-translation
           #:affine-uniform-scaling))
(in-package :pore-align/transform)

(deftype coordinate       () '(simple-array single-float (3)))
(deftype affine-transform () '(simple-array single-float (4 4)))

(deftype fit-function ()
  '(function (list)
    (values affine-transform &optional)))

(alexandria:define-constant +flip-det+
    (make-array '(3 3)
                :element-type 'single-float
                :initial-contents '((1.0 0.0  0.0)
                                    (0.0 1.0  0.0)
                                    (0.0 0.0 -1.0)))
  :test #'equalp)

;; ======================
;; Transform constructors
;; ======================

(declaim (inline affine-rotation))
(defun affine-rotation (rot)
  "Convert a 3x3 rotation matrix to an affine transform."
  (let ((result (make-array '(4 4)
                            :element-type 'single-float
                            :initial-element 0.0)))
    (loop for i below 3 do
      (loop for j below 3 do
        (setf (aref result i j)
              (aref rot i j))))
    (setf (aref result 3 3) 1.0)
    result))

(declaim (inline affine-translation))
(defun affine-translation (trans)
  "Convert a translation vector to an affine transform."
  (let ((result (make-array '(4 4)
                            :element-type 'single-float
                            :initial-element 0.0)))
    (loop for i below 4 do
      (setf (aref result i i) 1.0))
    (loop for i below 3 do
          (setf (aref result i 3)
                (aref trans i)))
    result))

(declaim (inline affine-uniform-scaling))
(defun affine-uniform-scaling (s)
  "Convert uniform scaling to an affine transform"
  (let ((result (make-array '(4 4)
                            :element-type 'single-float
                            :initial-element 0.0)))
    (loop for i below 3 do
      (setf (aref result i i) s))
    (setf (aref result 3 3) 1.0)
    result))

;; ==============================
;; Determine centroid and scaling
;; ==============================

(serapeum:-> center (list) (values coordinate &optional))
(defun center (coords)
  (declare (optimize (speed 3)))
  (let ((length (length coords))
        (state (vector-sum:sum-state 0f0)))
    (labels ((%go (state-x state-y state-z coords)
               (if (null coords)
                   (make-array 3
                               :element-type 'single-float
                               :initial-contents
                               (list (/ (vector-sum:state-sum state-x) length)
                                     (/ (vector-sum:state-sum state-y) length)
                                     (/ (vector-sum:state-sum state-z) length)))
                   (let ((coord (car coords)))
                     (declare (type coordinate coord))
                     (%go (vector-sum:add state-x (aref coord 0))
                          (vector-sum:add state-y (aref coord 1))
                          (vector-sum:add state-z (aref coord 2))
                          (cdr coords))))))
      (%go state state state coords))))

(serapeum:-> uniform-scale (list coordinate)
             (values single-float &optional))
(defun uniform-scale (coords center)
  (declare (optimize (speed 3)))
  (let ((length (length coords))
        (state (vector-sum:sum-state 0f0)))
    (labels ((update (state coord idx)
               (vector-sum:add state (expt (- (aref coord  idx)
                                              (aref center idx))
                                           2)))
             (%go (state-x state-y state-z coords)
               (if (null coords)
                   (values (vector-sum:state-sum state-x)
                           (vector-sum:state-sum state-y)
                           (vector-sum:state-sum state-z))
                   (let ((coord (car coords)))
                     (declare (type coordinate coord))
                     (%go (update state-x coord 0)
                          (update state-y coord 1)
                          (update state-z coord 2)
                          (cdr coords))))))
      (declare (inline update))
      (multiple-value-bind (x y z)
          (%go state state state coords)
        (sqrt (/ (+ x y z) length))))))

;; ==========================================
;; Rigid transform + uniform scale (optional)
;; ==========================================

(serapeum:-> make-matrix (boolean list)
             (values (util:fixed-entries 3) coordinate single-float &optional))
(defun make-matrix (scalingp coords)
  "Convert a list of coordinates to a Nx3 matrix + 3-vector centroid +
scaling parameter. The scaling parameter is always 1.0 if @c(scalingp)
is @c(NIL)."
  (declare (optimize (speed 3)))
  (let* ((center (center coords))
         (scale  (if scalingp (uniform-scale coords center) 1.0))
         (length (length coords))
         (matrix (make-array (list length 3) :element-type 'single-float)))
    (loop for i below length
          for coord of-type coordinate in coords do
            (loop for j below 3
                  for diff = (- (aref coord j) (aref center j)) do
                    (setf (aref matrix i j)
                          ;; SIFT3D can return points with the same coordinate and
                          ;; different descriptors. This can result in entries in COORDS
                          ;; being all the same and SCALE = 0. This case will be later
                          ;; rejected by the RANSAC. Just do this check so we don't signal
                          ;; here.
                          (if (zerop scale) diff (/ diff scale)))))
    (values matrix center scale)))

(deftype rotation-constraint () '(or null (integer 0 2)))

(serapeum:-> maybe-constrain-rotation ((util:fixed-entries 3) rotation-constraint)
             (values (util:fixed-entries 3) &optional))
(defun maybe-constrain-rotation (m constraint)
  (declare (optimize (speed 3)))
  (if constraint
      (let ((res (make-array '(3 3) :element-type 'single-float)))
        (util:loop-array (res (i j))
          (setf (aref res i j)
                (cond
                  ((= i j constraint) 1.0)
                  ((or (= i constraint)
                       (= j constraint))
                   0.0)
                  (t (aref m i j)))))
        res)
      m))

(serapeum:-> %rigid-transform-fit (boolean rotation-constraint list)
             (values affine-transform &optional))
(defun %rigid-transform-fit (scalingp rotation-constraint matches)
  "Find a rigid transform which fits the matches. If @c(scalingp) is
@c(T), the transform also includes uniform scaling."
  (declare (optimize (speed 3)))
  (let ((q (mapcar #'car matches))
        (p (mapcar #'cdr matches)))
    (serapeum:mvlet ((q c1 s1 (make-matrix scalingp q))
                     (p c2 s2 (make-matrix scalingp p)))
      (multiple-value-bind (u s vt)
          (em:svd (maybe-constrain-rotation
                   (em:mult p q :ta t)
                   rotation-constraint))
        (declare (ignore s))
        (let* ((uvt (em:mult u vt))
               (rot (if (> (em:det uvt) 0) uvt
                        (em:@ u +flip-det+ vt))))
          (em:@ (affine-translation c2)
                (affine-uniform-scaling s2)
                (affine-rotation rot)
                (em:invert (affine-uniform-scaling s1))
                (em:invert (affine-translation c1))))))))

(serapeum:-> rigid-transform-fit (&optional boolean rotation-constraint)
             (values (serapeum:-> (list) (values affine-transform &optional)) &optional))
(defun rigid-transform-fit (&optional scalingp rotation-constraint)
  "Return a function which finds a rigid transform which fits the
matches. The parameter @c(rotation-constraint) controls what rotations
are allowed and can be @c(nil) or a number 0, 1 or 2. If it's @c(nil)
rotation is unconstrained. If it's a number, only rotation around that
axis is allowed. The parameter @c(scaling), if non-@c(nil), also
enables uniform scaling of the coordinates."
  (lambda (matches)
    (%rigid-transform-fit scalingp rotation-constraint matches)))

(serapeum:-> to-affine-vector (coordinate)
             (values (simple-array single-float (4)) &optional))
(declaim (inline to-affine-vector))
(defun to-affine-vector (v)
  (let ((av (make-array 4 :element-type 'single-float)))
    (replace av v)
    (setf (aref av 3) 1.0)
    av))

(serapeum:-> match-fit-error (affine-transform (cons coordinate coordinate))
             (values single-float &optional))
(defun match-fit-error (fit match)
  "Calculate an error of the fit for a pair of coordinates."
  (declare (optimize (speed 3)))
  (let ((v1 (to-affine-vector (car match)))
        (v2 (to-affine-vector (cdr match))))
    (em:norm
     (em:sub
      (em:column (em:mult fit (em:vector->column v1)) 0)
      v2))))

(serapeum:-> fit-error (affine-transform list)
             (values single-float &optional))
(defun fit-error (fit matches)
  "Calculate the total fit error."
  (declare (optimize (speed 3)))
  (sqrt
   (loop for (p1 . p2) in matches sum
         (let ((v1 (to-affine-vector p1))
               (v2 (to-affine-vector p2)))
           (let ((diff (em:sub (em:column
                                (em:mult fit (em:vector->column v1)) 0)
                               v2)))
             (em:dot diff diff)))
         single-float)))

;; ============
;; Ransac stuff
;; ============

(defun random-integers (k n)
  "Collect K random integer from 0 (inclusive) to N (exclusive)
without repetitions."
  (assert (>= n k))
  (labels ((%go (acc k)
             (if (zerop k) acc
                 (let ((x (random n)))
                   (if (find x acc :test #'=)
                       (%go acc k)
                       (%go (cons x acc) (1- k)))))))
    (%go nil k)))

(defun select-entries (list indices)
  "Select items with specific indices from a list."
  (let ((indices (sort (copy-list indices) #'<)))
    (labels ((%go (list indices acc i)
               (cond
                 ((null indices) acc)
                 ((null list)
                  (error "The end of LIST is reached"))
                 ((= i (car indices))
                  (%go (cdr list) (cdr indices) (cons (car list) acc) (1+ i)))
                 (t
                  (%go (cdr list) indices acc (1+ i))))))
      (%go list indices nil 0))))

(serapeum:defconstructor ransac-result
  (transform affine-transform)
  (error     (single-float 0f0))
  (inliers   alexandria:positive-fixnum))

(serapeum:-> ransac-iteration (fit-function list
                               alexandria:positive-fixnum
                               (single-float 0f0))
             (values (or ransac-result null) &optional))
(defun ransac-iteration (f matches k ε)
  "Perform one iteration of RANSAC fit, namely find a linear model ΒS
so that ΒS(XS) fits YS. K is the number of points to find an initial
fit. ε is a criterion for being an inlier, namely |Y - ΒS(X)| must be
less that ε."
  (declare (optimize (speed 3)))
  (let* ((length (length matches))
         (indices (random-integers k length))
         (subset (select-entries matches indices))
         (fit (funcall f subset))
         (inliers
           (loop for match in matches
                 for err = (match-fit-error fit match)
                 when (< err ε)
                   collect match)))
    ;; There are k inliers or more
    (when (nth (1- k) inliers)
      (let* ((fit (funcall f inliers))
             (err (fit-error fit inliers))
             (n (length inliers)))
        (ransac-result fit err n)))))

(serapeum:-> ransac (fit-function list &key
                     (:iterations  alexandria:positive-fixnum)
                     (:seed-points alexandria:positive-fixnum)
                     (:err         (single-float 0f0)))
             (values (or null ransac-result) &optional))
(defun ransac (f matches &key (iterations 500) (seed-points 15) (err 100f0))
    "Find an affine transform which transforms the first keypoint in
each pair of matches to the second keypoint. Keypoint parameters are
related to the RANSAC algorithm: @c(ITERATIONS) is the number of
iterations, @c(SEED-POINTS) is an initial number of points to make a
fit. A point is well-fit if \\(\\| y - Ax \\|\\) is less than @c(ERR),
(\\(A\\) is a candidate for the found fit). The function @c(F) fits a
subset of @c(MATCHES) and controls the type of transform (e.g. rigid,
translation, etc.)."
  (declare (optimize (speed 3)))
  (if (< (length matches) seed-points) nil
      (let ((results (loop repeat iterations
                           collect
                           (lparallel:future
                             (ransac-iteration f matches seed-points err)))))
        (reduce
         (lambda (acc result)
           (let ((result (lparallel:force result)))
             (if (or (not acc)
                     (and result (or (> (ransac-result-inliers result)
                                        (ransac-result-inliers acc))
                                     (and (= (ransac-result-inliers result)
                                             (ransac-result-inliers acc))
                                          (< (ransac-result-error result)
                                             (ransac-result-error acc))))))
                 result acc)))
         results :initial-value nil))))
