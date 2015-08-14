(defmacro return-if (from val)
  `(if ,val
    (return-from ,from ,val)))

(defun foo1 (args)
  (return-if foo1 (cdr (assoc 'foo args)))
  "Nope")

(defun foo (args)
  (if (cdr (assoc 'foo args))
    (return-from foo (cdr (assoc 'foo args))))
  "Nope")

(foo '((foo . "yes")))
(foo '((fo . "yes")))
