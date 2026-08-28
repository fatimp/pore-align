;; B. Rister, M. A. Horowitz and D. L. Rubin, "Volumetric Image
;; Registration From Invariant Keypoints," in IEEE Transactions on
;; Image Processing, vol. 26, no. 10, pp. 4900-4910, Oct. 2017. doi:
;; 10.1109/TIP.2017.2722689

(defpackage pore-align/sift3d
  (:use #:cl)
  (:local-nicknames (#:util #:pore-align/util))
  (:export #:descriptors))
(in-package :pore-align/sift3d)

;; Libraries
(cffi:define-foreign-library libsift3d
  (:unix (:or "libsift3D.so.2"))
  (t (:default "libsift3D")))

(cffi:use-foreign-library libsift3D)

;; ==========
;;   Images
;; ==========

(serapeum:defconstructor image
  (pointer t))

(cffi:defcfun (%make-image "sift3d_make_image") :pointer
  (nx :int)
  (ny :int)
  (nz :int)
  (nc :int))

(serapeum:-> make-image
             (unsigned-byte unsigned-byte unsigned-byte unsigned-byte)
             (values image &optional))
(defun make-image (nx ny nz nc)
  (let ((image (%make-image nx ny nz nc)))
    (when (cffi:null-pointer-p image)
      (error 'util:ffi-error :message "Cannot allocate an image"))
    (image image)))

(cffi:defcfun (%free-image "sift3d_free_image") :void
  (image :pointer))

(serapeum:-> free-image (image) (values &optional))
(defun free-image (image)
  (%free-image (image-pointer image))
  (values))

(cffi:defcfun (%image-data "sift3d_image_data") (:pointer :float)
  (image :pointer))

(serapeum:-> copy-to-image (image (simple-array single-float 3))
             (values &optional))
(defun copy-to-image (image array)
  (let ((data-ptr (%image-data (image-pointer image))))
    (loop for i below (array-total-size array) do
          (setf (cffi:mem-aref data-ptr :float i)
                (row-major-aref array i))))
  (values))

(defmacro with-image ((image array) &body body)
  (let ((array-sym (gensym "ARRAY")))
    `(let* ((,array-sym ,array)
            (,image (make-image (array-dimension ,array-sym 0)
                                (array-dimension ,array-sym 1)
                                (array-dimension ,array-sym 2)
                                1)))
       (unwind-protect
            (progn
              (copy-to-image ,image ,array-sym)
              ,@body)
         (free-image ,image)))))

;; =============
;;  Other types
;; =============

(macrolet ((define-ffi-type (type constructor destructor)
             (flet ((mk-symbol (str)
                      (intern (format nil str (symbol-name type)))))
               (let ((lowlevel-constructor-name (mk-symbol "%MAKE-~a"))
                     (lowlevel-destructor-name  (mk-symbol "%FREE-~a"))
                     (constructor-name (mk-symbol "MAKE-~a"))
                     (destructor-name  (mk-symbol "FREE-~a"))
                     (unwrap (mk-symbol "~a-POINTER"))
                     (macro-name (mk-symbol "WITH-~a")))
                 `(progn
                    (serapeum:defconstructor ,type
                      (pointer t))

                    (cffi:defcfun (,lowlevel-constructor-name ,constructor) :pointer)
                    (cffi:defcfun (,lowlevel-destructor-name ,destructor) :void
                      (obj :pointer))

                    (serapeum:-> ,constructor-name () (values ,type &optional))
                    (defun ,constructor-name ()
                      (let ((ptr (,lowlevel-constructor-name)))
                        (when (cffi:null-pointer-p ptr)
                          (error 'util:ffi-error :message
                                 ,(format nil "Cannot allocate a ~a" type)))
                        (,type ptr)))

                    (serapeum:-> ,destructor-name (,type) (values &optional))
                    (defun ,destructor-name (obj)
                      (,lowlevel-destructor-name (,unwrap obj))
                      (values))

                    (defmacro ,macro-name ((var) &body body)
                      `(let ((,var (,',constructor-name)))
                         (unwind-protect
                              (progn ,@body)
                           (,',destructor-name ,var)))))))))
  (define-ffi-type matrix "sift3d_make_mat_rm" "sift3d_free_mat_rm")
  (define-ffi-type detector "sift3d_make_detector" "sift3d_free_detector")
  (define-ffi-type keypoint-store "sift3d_make_keypoint_store" "sift3d_free_keypoint_store")
  (define-ffi-type descriptor-store "sift3d_make_descriptor_store" "sift3d_free_descriptor_store"))

;; ==========
;;  Matrices
;; ==========

(defconstant +type-double+ 0)
(defconstant +type-float+  1)
(defconstant +type-int+    2)

(cffi:defcfun (%matrix-data "sift3d_mat_rm_data") :pointer
  (obj :pointer))

(cffi:defcfun (%matrix-dimensions "sift3d_mat_rm_dimensions") :void
  (obj :pointer)
  (num-cols (:pointer :int))
  (num-rows (:pointer :int)))

(cffi:defcfun (%matrix-type "sift3d_mat_rm_type") :int
  (obj :pointer))

;; Type unsafe
(serapeum:-> matrix-data (matrix) (values t &optional))
(defun matrix-data (matrix)
  (%matrix-data (matrix-pointer matrix)))

(serapeum:-> matrix-type (matrix) (values unsigned-byte &optional))
(defun matrix-type (matrix)
  (%matrix-type (matrix-pointer matrix)))

(serapeum:-> matrix-dimensions (matrix) (values integer integer &optional))
(defun matrix-dimensions (matrix)
  (cffi:with-foreign-objects
      ((cols-ptr :int)
       (rows-ptr :int))
    (%matrix-dimensions (matrix-pointer matrix) cols-ptr rows-ptr)
    (values (cffi:mem-ref rows-ptr :int)
            (cffi:mem-ref cols-ptr :int))))

;; ===============
;; Keypoint store
;; ===============

(cffi:defcfun (%sort-keypoints-by-strength "sift3d_keypoint_store_sort_by_strength") :void
  (store :pointer)
  (limit :int))

(serapeum:-> sort-keypoints-by-strength (keypoint-store unsigned-byte)
             (values &optional))
(defun sort-keypoints-by-strength (store limit)
  (%sort-keypoints-by-strength (keypoint-store-pointer store) limit)
  (values))

;; =================
;; Descriptor store
;; =================

(cffi:defcfun (%desc-store-to-matrix "sift3d_descriptor_store_to_mat_rm") :int
  (store  :pointer)
  (matrix :pointer))

(serapeum:-> desc-store-to-matrix (descriptor-store matrix)
             (values &optional))
(defun desc-store-to-matrix (store matrix)
  (unless (zerop (%desc-store-to-matrix
                  (descriptor-store-pointer store)
                  (matrix-pointer matrix)))
    (error 'util:ffi-error :message "Cannot copy descriptors to a matrix"))
  (values))

;; =========
;; Detector
;; =========

(cffi:defcfun (%detect-keypoints "sift3d_detect_keypoints") :int
  (detector :pointer)
  (image    :pointer)
  (kp-store :pointer))

(serapeum:-> detect-keypoints (detector image keypoint-store)
             (values &optional))
(defun detect-keypoints (detector image store)
  (unless (zerop (%detect-keypoints
                  (detector-pointer detector)
                  (image-pointer image)
                  (keypoint-store-pointer store)))
    (error 'util:ffi-error :message "Cannot detect keypoints"))
  (values))

(cffi:defcfun (%extract-descriptors "sift3d_extract_descriptors") :int
  (detector   :pointer)
  (kp-store   :pointer)
  (desc-store :pointer))

(serapeum:-> extract-descriptors (detector keypoint-store descriptor-store)
             (values &optional))
(defun extract-descriptors (detector kp-store desc-store)
  (unless (zerop (%extract-descriptors
                  (detector-pointer detector)
                  (keypoint-store-pointer kp-store)
                  (descriptor-store-pointer desc-store)))
    (error 'util:ffi-error :message "Cannot extract descriptors"))
  (values))

;; =========================
;; The ultimate WITH- macro
;; =========================

;; Each binding is in the form (MACRO-NAME VAR &REST ARGS) where
;; MACRO-NAME is a part of the name after WITH-.
(defmacro with-sift3d-objects (bindings &body body)
  (car
   (reduce
    (lambda (binding acc)
      (destructuring-bind (var macro-name &rest args)
          binding
        (let ((macro-sym (intern (format nil "WITH-~a" macro-name))))
          `((,macro-sym (,var ,@args) ,@acc)))))
    bindings
    :from-end t
    :initial-value body)))

(serapeum:-> split-coords
             ((util:fixed-entries #.(+ util:+descriptor-offset+ util:+descriptor-length+)))
             (values (util:fixed-entries #.util:+descriptor-offset+)
                     (util:fixed-entries #.util:+descriptor-length+) &optional))
(defun split-coords (array)
  (declare (optimize (speed 3)))
  (let* ((length (array-dimension array 0))
         (coords (make-array (list length util:+descriptor-offset+)
                             :element-type 'single-float))
         (descr  (make-array (list length util:+descriptor-length+)
                             :element-type 'single-float)))
    (loop for i below length do
          (loop for j below util:+descriptor-offset+ do
                (setf (aref coords i j) (aref array i j)))
          (loop for j below util:+descriptor-length+ do
                (setf (aref descr  i j) (aref array i (+ j util:+descriptor-offset+)))))
    (values coords descr)))

(defconstant +max-keypoints+ 300000)

;; Now, high-level function for descriptor arrays
(serapeum:-> descriptors ((util:image single-float))
             (values (util:fixed-entries #.util:+descriptor-offset+)
                     (util:fixed-entries #.util:+descriptor-length+) &optional))
(defun descriptors (array)
  "Take an image (3D array of single-floats) and return an array of
keypoint coordinates and their descriptors."
  (with-sift3d-objects ((detector   detector)
                        ;; Here we call TRANSPOSE because Sift3D
                        ;; library accepts arrays in column-major
                        ;; order (but outputs normal row-major ordered
                        ;; arrays).
                        (image      image (util:transpose-3d array))
                        (kp-store   keypoint-store)
                        (desc-store descriptor-store)
                        (matrix     matrix))
    (detect-keypoints detector image kp-store)
    (sort-keypoints-by-strength kp-store +max-keypoints+)
    (extract-descriptors detector kp-store desc-store)
    (desc-store-to-matrix desc-store matrix)
    (multiple-value-bind (nrows ncols)
        (matrix-dimensions matrix)
      (unless (and (= (matrix-type matrix) +type-float+)
                   (= ncols (+ util:+descriptor-offset+ util:+descriptor-length+)))
        (error 'util:ffi-error :message "Got strange descriptors"))
      (let ((matrix-data (matrix-data matrix))
            (descriptors (make-array (list nrows ncols) :element-type 'single-float)))
        ;; A descriptor is a vector of 771 single float elements.
        ;; The first 3 elements are the keypoint's coordinate and
        ;; the rest are arbitrary numbers which form a metric
        ;; space [0, 1]^{768} with a Euclidean metric.
        ;; SIFT3D returns them as an array with length Nx771
        (loop for i below (array-total-size descriptors) do
              (setf (row-major-aref descriptors i)
                    (cffi:mem-aref matrix-data :float i)))
        (split-coords descriptors)))))
