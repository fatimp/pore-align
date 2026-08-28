(defpackage pore-align/db
  (:use #:cl)
  (:local-nicknames (#:util   #:pore-align/util)
                    (#:pca    #:pore-align/pca)
                    (#:pre    #:pore-align/preprocessing)
                    (#:sift3d #:pore-align/sift3d))
  (:export #:descriptors-cached))
(in-package :pore-align/db)

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

(serapeum:-> encode-object (t)
             (values (simple-array (unsigned-byte 8) (*)) &optional))
(defun encode-object (object)
  (let ((stream (fast-io:make-output-buffer)))
    (conspack:encode-to-buffer object stream)
    (fast-io:finish-output-buffer stream)))

(serapeum:-> decode-object ((simple-array (unsigned-byte 8) (*)))
             (values t &optional))
(defun decode-object (octets)
  (nth-value
   0 (conspack:decode octets)))

(serapeum:-> descriptors-cached
             ((util:image (unsigned-byte 8)) pathname)
             (values (util:fixed-entries #.util:+descriptor-offset+)
                     (util:fixed-entries *)
                     (util:fixed-entries #.util:+descriptor-length+)
                     (simple-array single-float (#.util:+descriptor-length+))
                     &optional))
(defun descriptors-cached (array db-pathname)
  "Calculate image descriptors using 3D SIFT and cache them in a
database. The next time the descriptors are calculated for this
particular array the results are read from the database. The database
uses SHA256 hash of the array as a key into the database. Unlike
@c(SOIL-ALIGN/SIFT3D:DESCRIPTORS) function, this function accepts an
(original) array of octets which is later converted to an array of
single floats using CLAHE algorithm. @c(DB-PATHNAME) argument is a
path to the database.

Return four values: Coordinates of keypoints, descriptors in the PCA
space, a transform from the descriptor space to the PCA space,
descriptor component means."
  (let ((hash (image-hash array)))
    (ensure-directories-exist db-pathname)
    (lmdb+:with-env (env (uiop:native-namestring db-pathname)
                         :if-does-not-exist :create
                         :map-size          (* 64 (expt 2 30)))
      (let ((db (lmdb+:get-db "descriptors" :env env)))
        (let ((data (lmdb+:with-txn (:env env)
                      (lmdb+:get db hash))))
          ;; Descriptors are in the database, return them
          (if data
              ;; TODO: Do it without intermediate list consing ;)
              (destructuring-bind (coord pca vt means)
                  (decode-object data)
                  (values coord pca vt means))
              ;; else
              (multiple-value-bind (coords descr)
                  (sift3d:descriptors (pre:clahe array))
                (if (< (array-dimension descr 0)
                       (array-dimension descr 1))
                    (error 'util:db-error :message "Too small number of feature points")
                    (multiple-value-bind (vt means)
                        (pca:fit-pca descr 0.95)
                      (let ((pca (pca:transform-pca descr vt means)))
                        (lmdb+:with-txn (:env env :write t)
                          (lmdb+:put
                           db hash
                           (encode-object
                            (list coords pca vt means))))
                        (values coords pca vt means)))))))))))
