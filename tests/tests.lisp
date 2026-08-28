(in-package :pore-align/tests)

(def-suite stuff :description "Different aspects of pore-align")

(defun run-tests ()
  (let ((lparallel:*kernel* (lparallel:make-kernel 4)))
    (unwind-protect
         (every #'identity
                (mapcar (lambda (suite)
                          (let ((status (run suite)))
                            (explain! status)
                            (results-status status)))
                        '(stuff)))
      (lparallel:end-kernel :wait t))))

(defun gen-descriptors (n m)
  "Make n descriptors with m independent variables."
  (lambda ()
    (let* ((n (if (functionp n) (funcall n) n))
           (m (if (functionp m) (funcall m) m))
           (result (make-array (list n util:+descriptor-length+)
                               :element-type 'single-float
                               :initial-element 0.0)))
      (loop for i below n do
        (loop for j below m do
          (setf (aref result i j) (random 1f0))))
      result)))

(defun gen-coord ()
  (lambda ()
    (make-array 3 :element-type 'single-float
                  :initial-contents
                  (loop repeat 3 collect (random 100.0)))))

(defun gen-pair (gen)
  (lambda ()
    (cons (funcall gen)
          (funcall gen))))

(in-suite stuff)

(test pca
  (for-all* ((samples (gen-integer :min 1000 :max 2000))
             (indep   (gen-integer :min 50   :max 150))
             (descr   (gen-descriptors samples indep)))
    (multiple-value-bind (trans means)
        (pca:fit-pca descr 0.99)
      (is (<= (array-dimension trans 0)
              (1+ indep)))
      (let* ((descr-pcad (pca:transform-pca descr trans means))
             (descr-back (pca:invert-pca descr-pcad trans means)))
        (is-true (array-approx-p descr descr-back :rtol 1f-1))))))

(test ransac
  (for-all ((inliers  (gen-list :length   (gen-integer :min 2000 :max 3000)
                                :elements (gen-coord)))
            (outliers (gen-list :length   (gen-integer :min 100 :max 500)
                                :elements (gen-pair (gen-coord))))
            (translation (gen-coord)))
    (let* ((inliers (mapcar
                     (lambda (v)
                       (cons v (map '(vector single-float) #'+
                                    v translation)))
                     inliers))
           (all-together (append inliers outliers))
           (transform (tran:ransac-result-transform
                       (tran:ransac (tran:rigid-transform-fit) all-together
                                    :iterations 400 :err 2.0))))
      (loop for i below 3 do
        (is-true
         (approxp (aref transform i 3)
                  (aref translation i)
                  :rtol 1f-2)))
      (loop for i below 3 do
        (loop for j below 3 do
          (let ((x (aref transform i j)))
            (if (= i j)
                (is (approxp x 1.0))
                (< (abs x) 1f-5))))))))
