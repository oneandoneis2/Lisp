(defparameter *nodes*
  '(
    (living-room (You are in the living room. A wizard is snoring on the couch.) )
    (garden (You are in a lovely garden. There is a well in front of you.) )
    (attic (You are in the attic. There is a giant welding torch in the corner.) )
    )
  )

(defun describe-location (location nodes)
  (cadr (assoc location nodes))
  )
