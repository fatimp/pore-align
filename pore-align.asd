(defsystem :pore-align
  :name :pore-align
  :version "1.0.0"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "Align images of soil"
  :pathname "src"
  :serial t
  :class :package-inferred-system
  :depends-on (:serapeum
               :alexandria
               :float-features
               :cffi
               :entzauberte-matrices
               :log4cl
               :parse-float
               :command-line-parse
               :numpy-npy
               :nibbles
               :ironclad
               :lmdb
               :vector-sum
               :lparallel
               :fast-io
               :cl-conspack
               :cl-libtiff
               (:feature :freebsd :freebsd-sysctl)
               :pore-align/util
               :pore-align/preprocessing
               :pore-align/pca
               :pore-align/sift3d
               :pore-align/descriptor
               :pore-align/db
               :pore-align/match
               :pore-align/transform
               :pore-align/array-transform
               :pore-align/io
               :pore-align/cli)
  :in-order-to ((test-op (load-op "pore-align/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (funcall
                     (symbol-function
                      (intern (symbol-name '#:run-tests)
                              (find-package :pore-align/tests)))))
  :build-operation program-op
  :build-pathname "pore-align"
  :entry-point "pore-align/cli:main")

(defsystem :pore-align/tests
  :name :pore-align/tests
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :pathname "tests"
  :components ((:file "package")
               (:file "tests" :depends-on ("package")))
  :depends-on (:pore-align :fiveam :approx))

;; For qlot
(defsystem :pore-align/docs
    :depends-on (:pore-align :codex))

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c)
                   :executable t
                   :compression -1))
