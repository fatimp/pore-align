(defun do-all()
  (handler-case
      (asdf:load-system :pore-align/tests)
    (error ()
      (uiop:quit 1)))
  (uiop:quit
   (if (uiop:call-function "pore-align/tests:run-tests")
       0 1)))

(do-all)
