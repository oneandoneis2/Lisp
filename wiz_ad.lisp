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

(defparameter *objects*
  '(
    whiskey
    bucket
    frog
    chain
    )
 )

(defparameter *object-locations*
  '(
    (whiskey living-room)
    (bucket living-room)
    (frog garden)
    (chain garden)
    )
 )

(defun describe-location (location nodes)
  (cadr (assoc location nodes))
  )

(defun describe-path (edge)
  `(There is a ,(caddr edge) going ,(cadr edge) from here.)
 )

(defun describe-paths (location edges)
  (apply #'append
          (mapcar #'describe-path
                  (cdr (assoc location edges))
                  )
          )
  )

(defun objects-at (loc objs obj-locs)
  (labels (
           (at-loc-p (obj) (eq (cadr (assoc obj obj-locs)) loc) )
           )
    (remove-if-not #'at-loc-p objs)
   )
 )
