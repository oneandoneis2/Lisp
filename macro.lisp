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

(defun my-length (lst)
  (labels ((f (lst acc)
              (if lst
                (f (cdr lst) (1+ acc))
                acc)))
    (f lst 0)))
