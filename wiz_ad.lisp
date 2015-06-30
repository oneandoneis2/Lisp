(defparameter *nodes*
  '(
    (living-room (You are in the living room. A wizard is snoring on the couch.) )
    (garden (You are in a lovely garden. There is a well in front of you.) )
    (attic (You are in the attic. There is a giant welding torch in the corner.) )
    )
  )

(defparameter *edges*
  '(
    (living-room
      (garden west door)
      (attic upstairs ladder)
      )
    (garden
      (living-room east door)
      )
    (attic
      (living-room downstairs ladder)
      )
    )
  )

(defun describe-location (location nodes)
  (cadr (assoc location nodes))
  )

(defun describe-path (edge)
  `(There is a ,(caddr edge) going ,(cadr edge) from here.)
 )
