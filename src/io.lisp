(defpackage pore-align/io
  (:use #:cl)
  (:local-nicknames (#:util #:pore-align/util)
                    (#:tiff #:cl-libtiff))
  (:export #:read-image
           #:write-image))
(in-package :pore-align/io)

;; Raw "format" (a big mispractice of our NII)
(serapeum:-> read-raw (pathname)
             (values (util:image (unsigned-byte 8)) &optional))
(defun read-raw (pathname)
  (let ((specification
         (with-open-file (input pathname)
           (uiop:with-safe-io-syntax (:package :cl)
             (read input nil)))))
    (unless (and (null (cddddr specification))
                 (cdddr specification))
      (error 'util:io-error
             :message "Raw format spec must be (FILENAME DIM-X DIM-Y DIM-Z)"))
    (let ((raw-pathname (merge-pathnames
                         (pathname (first specification))
                         pathname))
          (result (make-array (cdr specification)
                              :element-type '(unsigned-byte 8))))
      (with-open-file (input raw-pathname :element-type '(unsigned-byte 8))
        (unless (= (read-sequence (sb-ext:array-storage-vector result) input)
                   (array-total-size result))
          (error 'util:io-error
                 :message "The input file is too short")))
      result)))

(serapeum:-> write-raw ((util:image (unsigned-byte 8)) pathname)
             (values &optional))
(defun write-raw (array pathname)
  (with-open-file (output pathname
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    (write-sequence (sb-ext:array-storage-vector array) output))
  (values))

;; TIFF format

(serapeum:defconstructor dimensions
  (width  (unsigned-byte 32))
  (height (unsigned-byte 32))
  (depth  (unsigned-byte 32)))

(serapeum:-> image-dimensions (tiff:tiff-handle)
             (values (or dimensions null) &optional))
(declaim (inline image-dimensions))
(defun image-dimensions (tiff)
  (let ((width  (tiff:width  tiff))
        (height (tiff:height tiff)))
    (labels ((%go (n)
               (declare (type fixnum n))
               (if (and (= (tiff:width  tiff) width)
                        (= (tiff:height tiff) height))
                   (let ((m (1+ n))
                         (nextp (tiff:read-directory tiff)))
                     (if nextp (%go m) m)))))
      (let ((depth (%go 0)))
        (if depth (dimensions width height depth))))))

(serapeum:-> intensity ((unsigned-byte 32))
             (values (unsigned-byte 8) &optional))
(declaim (inline intensity))
(defun intensity (color)
  (multiple-value-bind (r g b)
      (tiff:split-rgba color)
    (nth-value
     0 (floor (+ r g b) 3))))

(serapeum:-> read-tiff ((or pathname string))
             (values (or (simple-array (unsigned-byte 8) 3) null) &optional))
(defun read-tiff (name)
  (declare (optimize (speed 3)))
  (tiff:with-open-tiff (tiff name :input)
    (let ((dimensions (image-dimensions tiff)))
      (declare (dynamic-extent dimensions))
      (when dimensions
        (let* ((depth  (dimensions-depth  dimensions))
               (width  (dimensions-width  dimensions))
               (height (dimensions-height dimensions))
               (image (make-array (list depth height width)
                                  :element-type '(unsigned-byte 8))))
          (tiff:set-directory tiff 0)
          (loop for i below depth
                for slice = (tiff:read-rgba-image-oriented
                             tiff width height :topleft)
                do
                   (loop with idx = (array-row-major-index image i 0 0)
                         for j below (array-total-size slice) do
                           (setf (row-major-aref image (+ idx j))
                                 (intensity
                                  (row-major-aref slice j))))
                   (tiff:read-directory tiff))
          image)))))


(serapeum:-> write-tiff ((simple-array (unsigned-byte 8) 3)
                         (or pathname string))
             (values &optional))
(defun write-tiff (image name)
  (declare (optimize (speed 3)))
  (tiff:with-open-tiff (tiff name :output-bigtiff)
    (let ((depth  (array-dimension image 0))
          (height (array-dimension image 1))
          (width  (array-dimension image 2)))
      (loop for i below depth do
        (setf (tiff:width  tiff) width
              (tiff:height tiff) height
              
              (tiff:bits-per-sample   tiff) 8
              (tiff:samples-per-pixel tiff) 1
              (tiff:photometric       tiff) :min-is-black)
        (let ((scanline (make-array (tiff:scanline-size tiff)
                                    :element-type '(unsigned-byte 8))))
          (loop for j below height
                for idx = (array-row-major-index image i j 0) do
                  (replace scanline (sb-ext:array-storage-vector image)
                           :start2 idx)
                  (tiff:write-scanline tiff scanline j 0)))
        (tiff:write-directory tiff))))
  (values))

(serapeum:-> just-read-tiff ((or pathname string))
             (values (simple-array (unsigned-byte 8) 3) &optional))
(defun just-read-tiff (name)
  (let ((image (read-tiff name)))
    (if image image
        (error 'util:io-error
               :message
               #.(concatenate 'string
                              "All images in a multipage TIFF file must be of the "
                              "same dimensionality")))))

;; General I/O facility

(declaim (inline check-element-type))
(defun check-element-type (array)
  (unless (equalp (array-element-type array) '(unsigned-byte 8))
    (error 'util:io-error
           :message "Input array must have dtype='uint8'"))
  array)

;; TODO: detection based on content rather on name
(serapeum:-> read-image
             ((or string pathname))
             (values (util:image (unsigned-byte 8)) &optional))
(defun read-image (pathname)
  (let* ((pathname (pathname pathname))
         (type (pathname-type pathname)))
    (check-element-type
     (cond
       ((string-equal type "npy")
        (numpy-npy:load-array pathname))
       ((string-equal type "s-exp")
        (read-raw pathname))
       ((or (string-equal type "tif")
            (string-equal type "tiff"))
        (just-read-tiff pathname))
       (t (error 'util:io-error
                 :message "Unsupported input format"))))))

(serapeum:-> write-image
             ((util:image (unsigned-byte 8)) (or string pathname))
             (values &optional))
(defun write-image (array pathname)
  (let* ((pathname (pathname pathname))
         (type (pathname-type pathname)))
    (cond
      ((string-equal type "npy")
       (numpy-npy:store-array array pathname))
      ((string-equal type "raw")
       (write-raw array pathname))
      ((or (string-equal type "tif")
           (string-equal type "tiff"))
       (write-tiff array pathname))
      (t (error 'util:io-error
                 :message "Unsupported output format")))))
