(defpackage pore-align/tests
  (:use #:cl #:fiveam #:approx)
  (:local-nicknames (#:util #:pore-align/util)
                    (#:pca  #:pore-align/pca)
                    (#:tran #:pore-align/transform))
  (:export #:run-tests))
