(defpackage pore-align/util
  (:use #:cl)
  (:export #:loop-array
           #:loop-ranges
           #:image
           #:histograms
           #:descriptor
           #:fixed-entries
           #:+descriptor-offset+
           #:+descriptor-length+
           #:transpose-3d
           #:interpolate
           #:cut-from-center
           #:image-offset
           #:image-offset-x
           #:image-offset-y
           #:image-offset-z
           #:generic-error
           #:internal-error
           #:ffi-error
           #:descriptor-error
           #:user-input-error
           #:io-error
           #:clamp))
(in-package :pore-align/util)

(defconstant +descriptor-offset+ 3)
(defconstant +descriptor-length+ (- 771 +descriptor-offset+))

(deftype image      (type) `(simple-array ,type 3))
(deftype histograms (type) `(simple-array ,type (* * * 256)))
(deftype fixed-entries (n) `(simple-array single-float (* ,n)))

;; Useful macros for iteration which supersede nested loops
(defmacro loop-array ((array indices &rest options) &body body)
  (car
   (reduce
    (lambda (entry acc)
      (destructuring-bind (d . idx)
          entry
        (if (and (member :parallel options)
                 (eq idx (car indices)))
            `((lparallel:pdotimes (,idx (array-dimension ,array ,d))
                (declare (type fixnum ,idx))
                ,@acc))
            `((dotimes (,idx (array-dimension ,array ,d))
                (declare (type fixnum ,idx))
                ,@acc)))))
    (loop for idx in indices
          for d from 0 by 1
          collect (cons d idx))
    :from-end t
    :initial-value body)))

(defmacro loop-ranges (specs &body body)
  (car
   (reduce
    (lambda (spec acc)
      (destructuring-bind (idx start end)
          spec
        `((loop for ,idx fixnum from ,start below ,end do
                ,@acc))))
    specs
    :from-end t
    :initial-value body)))

(serapeum:-> transpose-3d ((image single-float))
             (values (image single-float) &optional))
(defun transpose-3d (array)
  (declare (optimize (speed 3)))
  (let ((result (make-array (reverse (array-dimensions array))
                            :element-type 'single-float)))
    (loop-array (result (i j k))
     (setf (aref result i j k)
           (aref array k j i)))
    result))

;; Linear interpolation

(declaim (inline interp1d))
(defun interp1d (v0 v1 x)
  (+ v0 (* (- v1 v0) x)))

(serapeum:-> interpolate
             ((serapeum:-> (fixnum fixnum fixnum)
                           (values single-float &optional))
              real real real
              (real (0)) (real (0)) (real (0)))
             (values single-float &optional))
(declaim (inline interpolate))
(defun interpolate (f x y z divisor-x divisor-y divisor-z)
  "Interpolate F in the point (X/DIVISOR-X, Y/DIVISOR-Y, Z/DIVIZOR-Z)."
  (let ((divisor-x (float divisor-x))
        (divisor-y (float divisor-y))
        (divisor-z (float divisor-z)))
    (serapeum:mvlet ((qi ri (floor x divisor-x))
                     (qj rj (floor y divisor-y))
                     (qk rk (floor z divisor-z)))
      ;; For code formatting
      (flet ((id (x) x))
        (declare (inline id))
        (let* ((ri (/ ri divisor-x))
               (rj (/ rj divisor-y))
               (rk (/ rk divisor-z))

               (v000 (funcall f (id qi) (id qj) (id qk)))
               (v001 (funcall f (id qi) (id qj) (1+ qk)))
               (v010 (funcall f (id qi) (1+ qj) (id qk)))
               (v011 (funcall f (id qi) (1+ qj) (1+ qk)))
               (v100 (funcall f (1+ qi) (id qj) (id qk)))
               (v101 (funcall f (1+ qi) (id qj) (1+ qk)))
               (v110 (funcall f (1+ qi) (1+ qj) (id qk)))
               (v111 (funcall f (1+ qi) (1+ qj) (1+ qk)))

               (v00 (interp1d v000 v001 rk))
               (v01 (interp1d v010 v011 rk))
               (v10 (interp1d v100 v101 rk))
               (v11 (interp1d v110 v111 rk))

               (v0 (interp1d v00 v01 rj))
               (v1 (interp1d v10 v11 rj))

               (v (interp1d v0 v1 ri)))
          v)))))

(serapeum:defconstructor image-offset
  (x alexandria:non-negative-fixnum)
  (y alexandria:non-negative-fixnum)
  (z alexandria:non-negative-fixnum))

(serapeum:-> cut-from-center ((image (unsigned-byte 8)) alexandria:positive-fixnum)
             (values (image (unsigned-byte 8)) image-offset &optional))
(defun cut-from-center (array side)
  (declare (optimize (speed 3)))
  (let* ((h (array-dimension array 0))
         (w (array-dimension array 1))
         (d (array-dimension array 2))
         (%h (min h side))
         (%w (min w side))
         (%d (min d side))
         (off-x (floor (- h %h) 2))
         (off-y (floor (- w %w) 2))
         (off-z (floor (- d %d) 2))
         (result (make-array (list %h %w %d)
                             :element-type (array-element-type array))))
    (loop-array (result (i j k))
     (setf (aref result i j k)
           (aref array (+ i off-x) (+ j off-y) (+ k off-z))))
    (values result (image-offset off-x off-y off-z))))

(declaim (inline clamp))
(defun clamp (x min max)
  (min (max x min) max))

;; Where else to put this?
(define-condition generic-error (error)
  ((message :reader  error-message
            :initarg :message))
  (:documentation "Generic error which is explicitly signaled from pore-align"))

(define-condition internal-error (generic-error)
  ()
  (:documentation "Error which is not in any way related to the user input"))

(define-condition ffi-error (internal-error)
  ()
  (:report (lambda (c s)
             (format s "FFI error: ~a" (error-message c)))))

(define-condition descriptor-error (internal-error)
  ()
  (:report (lambda (c s)
             (format s "Descriptor error: ~a" (error-message c)))))

(define-condition user-input-error (generic-error)
  ()
  (:report (lambda (c s)
             (format s "User input error: ~a" (error-message c)))))

(define-condition io-error (generic-error)
  ()
  (:report (lambda (c s)
             (format s "I/O error: ~a" (error-message c)))))
