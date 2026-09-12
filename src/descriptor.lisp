(defpackage pore-align/descriptor
  (:use #:cl)
  (:local-nicknames (#:util   #:pore-align/util)
                    (#:sift3d #:pore-align/sift3d)
                    (#:pca    #:pore-align/pca)
                    (#:pre    #:pore-align/preprocessing))
  (:export #:calculate-descriptor
           #:descriptor-fn
           #:descriptor
           #:descriptor-coords
           #:descriptor-pca-descr
           #:descriptor-pca-trans
           #:descriptor-means))
(in-package :pore-align/descriptor)

(serapeum:defconstructor descriptor
  (coords    (util:fixed-entries #.util:+descriptor-offset+))
  (pca-descr (util:fixed-entries *))
  (pca-trans (util:fixed-entries #.util:+descriptor-length+))
  (means     (simple-array single-float (#.util:+descriptor-length+))))

(deftype descriptor-fn ()
  '(function ((util:image (unsigned-byte 8))) (values descriptor &optional)))

(declaim (ftype descriptor-fn calculate-descriptor))
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
