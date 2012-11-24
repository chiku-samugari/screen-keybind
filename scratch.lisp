(defmacro foo (arg &environment env)
  (multiple-value-bind (a b c)
    (sb-cltl2:variable-information arg env)
    `(progn
       (print ,a)
       (print ,b)
       (print ,c))))

(let ((x 10))
  (foo x))
