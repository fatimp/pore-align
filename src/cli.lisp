(defpackage pore-align/cli
  (:use #:cl #:command-line-parse #:parse-float)
  (:local-nicknames (#:match  #:pore-align/match)
                    (#:util   #:pore-align/util)
                    (#:sift3d #:pore-align/sift3d)
                    (#:db     #:pore-align/db)
                    (#:io     #:pore-align/io)
                    (#:pca    #:pore-align/pca)
                    (#:trans  #:pore-align/transform)
                    (#:atrans #:pore-align/array-transform)
                    (#:em     #:entzauberte-matrices))
  (:export #:main))
(in-package :pore-align/cli)

(defparameter *dist-ratio-default*  1.1)
(defparameter *fit-error-default*   20.0)
(defparameter *ransac-iter-default* 20000)
(defparameter *background-default*  0)
(defparameter *seed-points-default* 15)

(alexandria:define-constant +db-pathname+
    #+unix
    #p"~/.local/share/pore-align/"
    #-unix
    (error "I don't know a suitable location where I can store the database.")
  :documentation "Path where the cache is stored"
  :test #'equalp)

(alexandria:define-constant +log-pathname+
    #+unix
    #p"~/.local/share/pore-align/log"
    #-unix
    (error "I don't know a suitable location where I can store the log file.")
  :documentation "Path where the log is stored"
  :test #'equalp)

(serapeum:-> get-db-pathname ()
             (values pathname &optional))
(defun get-db-pathname ()
  (let ((override (sb-posix:getenv "SOIL_ALIGN_DB")))
    (if override (pathname override) +db-pathname+)))

(defun parse-dist-ratio (string)
  (let ((x (ignore-errors (parse-float string))))
    (unless (and x (>= x 1))
      (error 'util:user-input-error :message "The distance ratio must be bigger than 1"))
    x))

(defun parse-constraint (string)
  (let ((ax (ignore-errors (parse-integer string))))
    (unless (and ax (<= 0 ax 2))
      (error 'util:user-input-error :message "The constraint can be 0, 1 or 2"))
    ax))

(defun parse-octet (string)
  (let ((n (ignore-errors (parse-integer string))))
    (unless (and n (<= 0 n 255))
      (error 'util:user-input-error
             :message "Background color must be an integer in the range 0..255"))
    n))

(defun parse-positive-integer (string)
  (let ((n (ignore-errors (parse-integer string))))
    (unless (and n (plusp n))
      (error 'util:user-input-error
             :message "Positive integer required"))
    n))

(declaim (inline default-number-of-threads-fallback))
(defun default-number-of-threads-fallback (signal-warn-p)
  (when signal-warn-p
    (log:warn
     #.(concatenate
        'string
        "Cannot get default number of threads and will use only 1. "
        "Use --threads to override this behavior.")))
  1)

(serapeum:-> default-number-of-threads (boolean)
             (values (integer 1) &optional))
(defun default-number-of-threads (signal-warn-p)
  (declare (ignorable signal-warn-p))
  #+freebsd
  (util:clamp (floor (freebsd-sysctl:sysctl-by-name "kern.smp.cores") 2) 1 10)
  #-freebsd
  (default-number-of-threads-fallback signal-warn-p))

(defparameter *parser*
  (seq
   (optional
    (flag   :verbose
            :short       #\v
            :long        "verbose"
            :description "Be verbose")
    (option :nthreads    "N"
            :long        "threads"
            :short       #\t
            :fn          #'parse-positive-integer
            :description (format nil "Number of threads to use (Default: ~d)"
                                 (default-number-of-threads nil)))
    (option :src-workspace "SIDE"
            :long        "src-workspace-side"
            :fn          #'parse-positive-integer
            :description #.(concatenate
                            'string
                            "Side of a workspace which is cut from center of the "
                            "source image. Has a precedence over -w option."))
    (option :ref-workspace "SIDE"
            :long        "ref-workspace-side"
            :fn          #'parse-positive-integer
            :description #.(concatenate
                            'string
                            "Side of a workspace which is cut from center of the "
                            "reference image. Has a precedence over -w option."))
    (option :seed-points "N"
            :short       #\p
            :long        "seed-points"
            :fn          #'parse-positive-integer
            :description (format
                          nil "Number of seed points for RANSAC (Default: ~d)"
                          *seed-points-default*))
    (option :workspace   "SIDE"
            :long        "workspace-side"
            :short       #\w
            :fn          #'parse-positive-integer
            :description "Side of a workspace which is cut from center of the input images")
    (option :transform-output "m.npy"
            :short       #\O
            :long        "transform-output"
            :description "Output file name for a transform matrix (.npy)")
    (option :output      "out.npy"
            :short       #\o
            :long        "output"
            :description "Output file name for a transformed image (.npy or .raw)")
    (option :constraint  "AXIS"
            :long        "rotation-constraint"
            :description "Constrain rotation to be around only one axis (0, 1 or 2)"
            :fn          #'parse-constraint)
    (flag   :scaling
            :short       #\s
            :long        "scaling"
            :description "Add uniform scaling to the model")
    (option :background  "COLOR"
            :long        "background-color"
            :short       #\b
            :description (format nil "Background color (Default: ~d)"
                                 *background-default*)
            :fn          #'parse-octet)
    (option :dist-ratio  "C"
            :long        "dist-ratio"
            :description (format
                          nil #.(concatenate
                                 'string
                                 "Minimal distance ratio between the closest and the "
                                 "second to closest neighbors (Default: ~f)")
                          *dist-ratio-default*)
            :fn          #'parse-dist-ratio)
    (option :fit-error   "E"
            :long        "fit-error"
            :description (format
                          nil #.(concatenate
                                 'string
                                 "The maximal allowed fit error to treat a sample as "
                                 "inlier (Default: ~f)")
                          *fit-error-default*)
            :fn          #'parse-float)
    (option :ransac-iter "M"
            :long        "ransac-iterations"
            :description (format nil "Number of RANSAC iterations (Default: ~d)"
                                 *ransac-iter-default*)
            :fn          #'parse-positive-integer))
   (argument :reference "reference")
   (argument :source    "source")))

(declaim (inline log-eval))
(defun log-eval (message f &rest args)
  (let ((value (apply f args)))
    (log:info message)
    value))

(serapeum:-> maybe-cut ((util:image (unsigned-byte 8))
                        (or null alexandria:positive-fixnum))
             (values (util:image (unsigned-byte 8))
                     alexandria:non-negative-fixnum
                     alexandria:non-negative-fixnum
                     alexandria:non-negative-fixnum
                     &optional))
(defun maybe-cut (array side)
  (if side
      (util:cut-from-center array side)
      (values array 0 0 0)))

(serapeum:-> add-offsets!
             (alexandria:non-negative-fixnum
              alexandria:non-negative-fixnum
              alexandria:non-negative-fixnum
              (util:fixed-entries #.util:+descriptor-offset+))
             (values (util:fixed-entries #.util:+descriptor-offset+) &optional))
(defun add-offsets! (off-x off-y off-z keypoints)
  (declare (optimize (speed 3)))
  (unless (= off-x off-y off-z 0)
    (loop for i below (array-dimension keypoints 0) do
          (incf (aref keypoints i 0) off-x)
          (incf (aref keypoints i 1) off-y)
          (incf (aref keypoints i 2) off-z)))
  keypoints)

(defun %main ()
  (let* ((args (parse-argv *parser*))
         (dist-ratio        (%assoc :dist-ratio       args *dist-ratio-default*))
         (fit-error         (%assoc :fit-error        args *fit-error-default*))
         (trans-image       (%assoc :output           args))
         (trans-matrix      (%assoc :transform-output args))
         (scalingp          (%assoc :scaling          args))
         (rot-constraint    (%assoc :constraint       args))
         (reference         (%assoc :reference        args))
         (source            (%assoc :source           args))
         (src-workspace (or (%assoc :src-workspace    args)
                            (%assoc :workspace        args)))
         (ref-workspace (or (%assoc :ref-workspace    args)
                            (%assoc :workspace        args)))
         (ransac-iter       (%assoc :ransac-iter      args *ransac-iter-default*))
         (seed-points       (%assoc :seed-points      args *seed-points-default*))
         (background        (%assoc :background       args *background-default*))
         (nthreads          (%assoc :nthreads         args))
         (nthreads          (or nthreads (default-number-of-threads t)))
         (db-pathname       (get-db-pathname)))
    (unless (or trans-image trans-matrix)
      (error 'util:user-input-error :message "No output selected"))
    (log:config (if (%assoc :verbose args) :info :warn))
    (log:config :daily +log-pathname+ :backup nil)
    (let* ((source    (io:read-image source))
           (reference (io:read-image reference))
           (ref-shape (array-dimensions reference)))
      (serapeum:mvlet ((source    sx sy sz (maybe-cut source    src-workspace))
                       (reference rx ry rz (maybe-cut reference ref-workspace)))
        ;; Run a full GC because uncut arrays may be really big, we need
        ;; to collect them now because later we will run foreign code
        ;; which allocates a lot.
        (sb-ext:gc :full t)
        (log:info "Starting")
        (log:info "Will use ~d threads" nthreads)
        (em:set-num-threads nthreads)
        (setq lparallel:*kernel* (lparallel:make-kernel nthreads))
        (serapeum:mvlet*
            ((ref-descriptors (log-eval "Got descriptors of the reference image"
                                        #'db:descriptors-cached reference
                                        db-pathname))
             (src-descriptors (log-eval "Got descriptors of the source image"
                                        #'db:descriptors-cached source
                                        db-pathname))
             ;; Convert descriptors in one PCA space
             (ref-desc src-desc (pca:restore-descriptors
                                 (db:descriptor-pca-descr ref-descriptors)
                                 (db:descriptor-pca-trans ref-descriptors)
                                 (db:descriptor-means     ref-descriptors)
                                 (db:descriptor-pca-descr src-descriptors)
                                 (db:descriptor-pca-trans src-descriptors)
                                 (db:descriptor-means     src-descriptors)))
             ;; Find matches between descriptors
             (ref-kp (db:descriptor-coords ref-descriptors))
             (src-kp (db:descriptor-coords src-descriptors))
             (matches (match:match-descriptors
                       (add-offsets! rx ry rz ref-kp)
                       (add-offsets! sx sy sz src-kp)
                       ref-desc src-desc dist-ratio)))
          (log:info "Found matches between images")
          ;; RANSAC is parallelized on the lisp side already
          (em:set-num-threads 1)
          (let ((fit (trans:ransac (trans:rigid-transform-fit scalingp rot-constraint)
                                   matches
                                   :seed-points seed-points
                                   :iterations  ransac-iter
                                   :err         fit-error)))
            (unless fit
              (log:info "Summary: ~d/~d descriptors, ~d matches"
                        (array-dimension src-kp 0)
                        (array-dimension ref-kp 0)
                        (length matches))
              (log:error "Consensus is not achieved")
              (uiop:quit 0))
            (log:info "Found a transform matrix")
            (when trans-matrix
              (numpy-npy:store-array (trans:ransac-result-transform fit) trans-matrix))
            (when trans-image
              (io:write-image
               (log-eval "Computed a transformed image"
                         #'atrans:apply-transform
                         (if src-workspace
                             ;; Load a bigger image once more
                             (io:read-image (%assoc :source args))
                             source)
                         (trans:ransac-result-transform fit) ref-shape
                         :background background)
               trans-image))
            (log:info #.(concatenate
                         'string
                         "Summary: ~d/~d descriptors, ~d independent parameters, "
                         "~d matches, ~d inliers, ~f fit error")
                      (array-dimension src-kp 0)
                      (array-dimension ref-kp 0)
                      (array-dimension ref-desc 1)
                      (length matches)
                      (trans:ransac-result-inliers fit)
                      (trans:ransac-result-error   fit))))))))

(defun handle-error (c)
  (princ c *error-output*)
  (terpri *error-output*)
  (typecase c
    (sb-sys:interactive-interrupt
     ;; Silently quit
     (uiop:quit 0))
    ((or cmd-line-parse-error
         (and util:generic-error (not util:internal-error)))
     (print-usage *parser* "pore-align")
     (uiop:quit 1))
    (error
     (sb-debug:backtrace 20 *error-output*)
     (uiop:quit 1))))

(defun main ()
  (sb-ext:disable-debugger)
  (handler-bind
      ((condition #'handle-error))
    (%main))
  (uiop:quit 0))
