(defpackage pore-align/db
  (:use #:cl)
  (:local-nicknames (#:util #:pore-align/util)
                    ;; FIXME: for %assoc
                    (#:cmd  #:command-line-parse)
                    (#:dsc  #:pore-align/descriptor))
  (:export #:descriptors-cached
           #:matches-cached))
(in-package :pore-align/db)

(serapeum:-> matches-hash ((simple-array (unsigned-byte 8) (32))
                           (simple-array (unsigned-byte 8) (32))
                           (or util:image-offset null)
                           (or util:image-offset null)
                           (single-float 1.0))
             (values (simple-array (unsigned-byte 8) (32)) &optional))
(defun matches-hash (ref-hash src-hash ref-offset src-offset dist-ratio)
  (let ((digest (ironclad:make-digest 'ironclad:sha256)))
    (flet ((update-offset! (offset)
             (let ((buffer (make-array (* 3 4)
                                       :element-type '(unsigned-byte 8))))
               (setf (nibbles:ub32ref/le buffer 0)
                     (util:image-offset-x offset)
                     (nibbles:ub32ref/le buffer 4)
                     (util:image-offset-y offset)
                     (nibbles:ub32ref/le buffer 8)
                     (util:image-offset-z offset))
               (ironclad:update-digest digest buffer)))
           (update-float! (x)
             (let ((buffer (make-array 4 :element-type '(unsigned-byte 8))))
               (setf (nibbles:ieee-single-ref/le buffer 0) x)
               (ironclad:update-digest digest buffer))))
      ;; Store hashes of reference and source
      (ironclad:update-digest digest ref-hash)
      (ironclad:update-digest digest src-hash)
      ;; Store hash of reference offset (if any)
      (when ref-offset
        (update-offset! ref-offset))
      ;; Store hash of source offset (if any)
      (when src-offset
        (update-offset! src-offset))
      ;; Hash distance ratio
      (update-float! dist-ratio)
      (ironclad:produce-digest digest))))

(serapeum:-> image-hash ((util:image (unsigned-byte 8)))
             (values (simple-array (unsigned-byte 8) (32)) &optional))
(defun image-hash (array)
  (let ((digest (ironclad:make-digest 'ironclad:sha256)))
    ;; Update with array dimensions
    (ironclad:update-digest
     digest
     (let ((dim-vector (make-array (* 3 4) :element-type '(unsigned-byte 8))))
       (loop for dim in (array-dimensions array)
             for idx from 0 by 4 do
             (setf (nibbles:ub32ref/le dim-vector idx) dim))
       dim-vector))
    ;; Update with array data
    (ironclad:update-digest
     digest (sb-ext:array-storage-vector array))
    (ironclad:produce-digest digest)))

(defmethod conspack:encode-object append
    ((descriptor dsc:descriptor) &key &allow-other-keys)
  (list
   (cons :coords    (dsc:descriptor-coords    descriptor))
   (cons :pca-descr (dsc:descriptor-pca-descr descriptor))
   (cons :pca-trans (dsc:descriptor-pca-trans descriptor))
   (cons :means     (dsc:descriptor-means     descriptor))))

(defmethod conspack:decode-object-allocate
    ((class (eql 'dsc:descriptor)) alist &key &allow-other-keys)
  (dsc:descriptor
   (cmd:%assoc :coords    alist)
   (cmd:%assoc :pca-descr alist)
   (cmd:%assoc :pca-trans alist)
   (cmd:%assoc :means     alist)))

(defmethod conspack:decode-object-initialize progn
    ((object dsc:descriptor) class alist &key &allow-other-keys)
  (declare (ignore class alist))
  object)

(serapeum:-> encode-object (t)
             (values (simple-array (unsigned-byte 8) (*)) &optional))
(declaim (inline encode-object))
(defun encode-object (object)
  (let ((stream (fast-io:make-output-buffer)))
    (conspack:encode-to-buffer object stream)
    (fast-io:finish-output-buffer stream)))

(serapeum:-> encode-descriptor (dsc:descriptor)
             (values (simple-array (unsigned-byte 8) (*)) &optional))
(defun encode-descriptor (descriptor)
  (encode-object descriptor))

(serapeum:-> encode-matches (list)
             (values (simple-array (unsigned-byte 8) (*)) &optional))
(defun encode-matches (matches)
  (encode-object matches))

(serapeum:-> decode-object ((simple-array (unsigned-byte 8) (*)))
             (values t &optional))
(declaim (inline decode-object))
(defun decode-object (octets)
  (let ((stream (fast-io:make-input-buffer :vector octets)))
    (conspack:decode-value stream)))

(serapeum:-> decode-descriptor ((simple-array (unsigned-byte 8) (*)))
             (values dsc:descriptor &optional))
(defun decode-descriptor (octets)
  (decode-object octets))

(serapeum:-> decode-matches ((simple-array (unsigned-byte 8) (*)))
             (values list &optional))
(defun decode-matches (octets)
  (decode-object octets))

(serapeum:-> %descriptors-cached (lmdb:env (util:image (unsigned-byte 8)))
             (values dsc:descriptor &optional))
(defun %descriptors-cached (env array)
  (let* ((hash (image-hash array))
         (db (lmdb+:get-db "descriptors" :env env))
         (data (lmdb+:with-txn (:env env)
                 (lmdb+:get db hash))))
    ;; Descriptors are in the database, return them
    (if data (decode-descriptor data)
        (let ((descriptor (dsc:calculate-descriptors array)))
          (lmdb+:with-txn (:env env :write t)
            (lmdb+:put db hash (encode-descriptor descriptor)))
          descriptor))))

;; High-level function
(serapeum:-> descriptors-cached ((or pathname string)
                                 (util:image (unsigned-byte 8)))
             (values dsc:descriptor &optional))
(defun descriptors-cached (db-pathname array)
  "Calculate image descriptors using 3D SIFT and cache them in a
database. The next time the descriptors are calculated for this
particular array the results are read from the database. The database
uses SHA256 hash of the array as a key into the
database. @c(DB-PATHNAME) argument is a path to the database. This
function is a cached version of @c(CALCULATE-DESCRIPTORS).

Return @c(DESCRIPTOR) structure."
  (lmdb+:with-env (env (uiop:native-namestring db-pathname)
                       :if-does-not-exist :create
                       :max-dbs           2
                       :map-size          (* 64 (expt 2 30)))
    (%descriptors-cached env array)))

(serapeum:-> %descriptors-with-logging ((function () (values dsc:descriptor &optional))
                                        string)
             (values dsc:descriptor &optional))
(defun %descriptors-with-logging (f which)
  (let ((dsc (funcall f)))
    (log:info "Got ~d descriptors of the ~a image"
              (dsc:descriptor-npoints dsc)
              which)
    dsc))

(defmacro descriptors-with-logging (which &body body)
  `(%descriptors-with-logging
    (lambda ()
      ,@body)
    ,which))

(serapeum:-> matches-cached ((or string pathname)
                             (util:image (unsigned-byte 8))
                             (util:image (unsigned-byte 8))
                             (or util:image-offset null)
                             (or util:image-offset null)
                             (single-float 1.0))
             (values list &optional))
(defun matches-cached (db-pathname ref src ref-offset src-offset dist-ratio)
  "Find matched between descriptors and cache the result in the
database, so the next time the matches are needed the DB entry is
returned instead of running the full search.

Image offsets (when working with subregions) and the distance ratio is
encoded in the key as well.

When there are no matches in the database for this combination of
arguments, the required descriptors are also cached in the DB.

This is a caching version of @c(CALCULATE-MATCHES)."
  (let* ((ref-hash (image-hash ref))
         (src-hash (image-hash src))
         (hash     (matches-hash ref-hash src-hash ref-offset src-offset dist-ratio)))
    (lmdb+:with-env (env (uiop:native-namestring db-pathname)
                         :if-does-not-exist :create
                         :max-dbs           2
                         :map-size          (* 64 (expt 2 30)))
      (let* ((db   (lmdb+:get-db "matches" :env env))
             (data (lmdb+:with-txn (:env env)
                     (lmdb+:get db hash))))
        (if data (decode-matches data)
            (let* ((ref-descriptors
                     (descriptors-with-logging "reference"
                       (%descriptors-cached env ref)))
                   (src-descriptors
                     (descriptors-with-logging "source"
                       (%descriptors-cached env src)))
                   (matches (dsc:calculate-matches
                             ref-descriptors src-descriptors
                             ref-offset src-offset dist-ratio)))
              (lmdb+:with-txn (:env env :write t)
                (lmdb+:put db hash (encode-matches matches)))
              matches))))))
