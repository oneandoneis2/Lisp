(defparameter *nodes*
  '(
    (living-room (you are in the living room. a wizard is snoring on the couch.))
    (garden (you are in a lovely garden. there is a well in front of you.))
    (attic (you are in the attic. there is a giant welding torch in the corner.))))

(defparameter *edges*
  '(
    (living-room
      (garden west door)
      (attic upstairs ladder))
    (garden
      (living-room east door))
    (attic
      (living-room downstairs ladder))
    )
  )

(defparameter *objects*
  '(
    whiskey
    bucket
    frog
    chain
    ))

(defparameter *object-locations*
  '(
    (whiskey living-room)
    (bucket living-room)
    (frog garden)
    (chain garden)
    ))

(defparameter *location* 'living-room)

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

(defun describe-paths (location edges)
  (apply #'append
         (mapcar (lambda (edge) `(There is a ,(caddr edge) going ,(cadr edge) from here.))
                 (cdr (assoc location edges)))))

(defun objects-at (loc objs obj-locs)
  (labels (
           (at-loc-p (obj) (eq (cadr (assoc obj obj-locs)) loc) ))
    (remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-locs)
  (labels (
           (describe-objs (obj) `(You see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-objs (objects-at loc objs obj-locs)))))

(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  (let ((next (find direction
                    ( cdr (assoc *location* *edges*) )
                    :key #'cadr)))
    (if next
      (progn (setf *location* (car next)) (look))
      '(You cannot go that way.))))

(defun pickup (object)
  (cond ((member object (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(You are now carrying the ,object))
        (t '(You cannot pick that up.))))

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(defun game-read()
  (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
    (flet ( (quote-it (x) (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pickup inventory))
(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
    (eval sexp)
    `(I do not know how to ,(car sexp))))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eql item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
  (princ (
          coerce (
                  tweak-text (
                              coerce (
                                      string-trim "() " (prin1-to-string lst))
                              'list)
                  t
                  nil) 'string))
  (fresh-line))

(defun have (object)
  (member object (cdr (inventory))))

(defparameter *chain-welded* nil)

(defun weld (subject object)
  (if (and (eq *location* 'attic)
           (eq subject 'chain)
           (eq object 'bucket)
           (have 'chain)
           (have 'bucket)
           (not *chain-welded*))
    (progn (setf *chain-welded* t)
           '(the chain is now welded to the bucket))
    '(you cannot weld like that.)))

(pushnew 'weld *allowed-commands*)

(defparameter *bucket-filled* nil)

(defun dunk (subject object)
  (if (and (eq *location* 'garden)
           (eq subject 'bucket)
           (eq object 'well)
           (have 'bucket)
           *chain-welded*)
    (progn (setf *bucket-filled* t)
           '(the bucket is now full of water))
    '(you cannot dunk like that)))

(pushnew 'dunk *allowed-commands*)
