(defpackage pore-align/descriptor
  (:use #:cl)
  (:local-nicknames (#:util   #:pore-align/util)
                    (#:sift3d #:pore-align/sift3d)
                    (#:pca    #:pore-align/pca)
                    (#:pre    #:pore-align/preprocessing)
                    (#:match  #:pore-align/match))
  (:export #:calculate-descriptor
           #:calculate-matches
           #:descriptor
           #:descriptor-coords
           #:descriptor-npoints
           #:descriptor-pca-descr
           #:descriptor-pca-trans
           #:descriptor-means))
(in-package :pore-align/descriptor)

(serapeum:defconstructor descriptor
  (coords    (util:fixed-entries #.util:+descriptor-offset+))
  (pca-descr (util:fixed-entries *))
  (pca-trans (util:fixed-entries #.util:+descriptor-length+))
  (means     (simple-array single-float (#.util:+descriptor-length+))))

(serapeum:-> descriptor-npoints (descriptor)
             (values alexandria:array-index &optional))
(defun descriptor-npoints (descriptor)
  (array-dimension (descriptor-coords descriptor) 0))

(serapeum:-> calculate-descriptor ((util:image (unsigned-byte 8)))
             (values descriptor &optional))
(defun calculate-descriptor (image)
  "Extract feature points and descriptors from an image"
  (declare (optimize (speed 3)))
  (multiple-value-bind (coords descr)
      (sift3d:descriptors (pre:clahe image))
    (if (< (array-dimension descr 0)
           (array-dimension descr 1))
        (error 'util:descriptor-error :message "Too small number of feature points")
        (multiple-value-bind (vt means)
            (pca:fit-pca descr 0.95)
          (let ((pca (pca:transform-pca descr vt means)))
            (descriptor coords pca vt means))))))

(serapeum:-> add-offsets!
    ((util:fixed-entries #.util:+descriptor-offset+) util:image-offset)
    (values &optional))
(defun add-offsets! (keypoints offset)
  (declare (optimize (speed 3)))
  (let ((x (float (util:image-offset-x offset)))
        (y (float (util:image-offset-y offset)))
        (z (float (util:image-offset-z offset))))
    (loop for i below (array-dimension keypoints 0) do
      (incf (aref keypoints i 0) x)
      (incf (aref keypoints i 1) y)
      (incf (aref keypoints i 2) z)))
  (values))

(deftype matches-fn ()
  '(function (descriptor descriptor
              (or util:image-offset null)
              (or util:image-offset null)
              (single-float 1.0))
    (values list alexandria:array-index &optional)))

(serapeum:-> calculate-matches (descriptor descriptor
                                (or util:image-offset null)
                                (or util:image-offset null)
                                (single-float 1.0))
             (values list &optional))
(defun calculate-matches (ref-descriptors src-descriptors ref-offset src-offset dist-ratio)
  (declare (optimize (speed 3)))
  (multiple-value-bind (ref-desc src-desc)
      (pca:restore-descriptors
       (descriptor-pca-descr ref-descriptors)
       (descriptor-pca-trans ref-descriptors)
       (descriptor-means     ref-descriptors)
       (descriptor-pca-descr src-descriptors)
       (descriptor-pca-trans src-descriptors)
       (descriptor-means     src-descriptors))
    (let ((ref-kp (descriptor-coords ref-descriptors))
          (src-kp (descriptor-coords src-descriptors)))
      (when ref-offset
        (add-offsets! ref-kp ref-offset))
      (when src-offset
        (add-offsets! src-kp src-offset))
      (match:match-descriptors ref-kp src-kp ref-desc src-desc dist-ratio))))
