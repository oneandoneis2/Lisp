(defparameter *small* 1)
(defparameter *big* 100)
(defparameter *guess* 0)

(defun guess-my-number ()
  (setf *guess* (ash ( + *small* *big* ) -1))
  )

(defun smaller ()
  ( setf *big* ( 1- *guess*  ) )
  (guess-my-number)
  )

(defun bigger ()
  ( setf *small* ( 1+ *guess*  ) )
  (guess-my-number)
  )

(defun start-over ()
  (defparameter *small* 1)
  (defparameter *big* 100)
  (guess-my-number)
  )
