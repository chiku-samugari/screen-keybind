(defmacro foo (arg &environment env)
  (multiple-value-bind (a b c)
    (sb-cltl2:variable-information arg env)
    `(progn
       (print ,a)
       (print ,b)
       (print ,c))))

(let ((x 10))
  (foo x))

(defmacro foo (&rest args)
  (let ((forms (mapcar #`(print (string-downcase ,a0)) args)))
    `(progn
       ,@forms)))

(foo 'a 'b 'c)
(let ((x 'y))
  (foo x))

(defmacro bar (&rest args)
  (with-gensyms (forms)
    `(let ((,forms (mapcar #`(print (string-downcase ,a0)) ',args)))
       (cons 'progn ,forms))))

(bar 'a 'b 'c)
(let ((x '(a b c)))
  (bar

(defmacro alt-bar (&rest args)
  (with-gensyms (forms)
    `(let ((,forms (mapcar #`(print (string-downcase ,a0)) ',args)))
       `(progn ,@,forms))))

(alt-bar 'a 'b 'c)
