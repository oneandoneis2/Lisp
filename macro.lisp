(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))
; (let ((foo (+ 2 3))) (* foo foo))
; (macroexpand '(let1 foo (+ 2 3) (* foo foo)))
; (LET ((FOO (+ 2 3))) (* FOO FOO))

(defun add (a b)
  (let1 x (+ a b)
        (format t "The sum is ~a" x)
        x))

(defmacro split (val yes no)
  `(let1 x ,val
         (if x
           (let ((head (car x))
                 (tail (cdr x)))
             ,yes)
           ,no)))

(defun my-length (lst)
  (labels ((f (lst acc)
              (split lst
                (f tail (1+ acc))
                acc)))
    (f lst 0)))
