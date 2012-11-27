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

(defmacro alt-bar (&rest args)
  (with-gensyms (forms)
    `(let ((,forms (mapcar #`(print (string-downcase ,a0)) ',args)))
       `(progn ,@,forms))))

(alt-bar 'a 'b 'c)

(format t "~{~a~%~}"
        (remove-if-not (lambda (p) (search "SB-C" p :test #'equal))
                       (mapcar #'package-name (list-all-packages))))

(let ((s (make-string-output-stream)))
  (run-program "/bin/pwd" () :output s)
  (get-output-stream-string s)
  (close s))

(with-output-to-string (s)
  (run-program "/bin/pwd" () :output s))

(destructuring-bind (a &rest b)
  (list 1 2 3)
  (print a)
  (print b))

(quote [^i other])
(quote {snormal snormal [^i other]})

(chiku.genscreenrc:read-rcsrc "screenrc-src" "/home/chiku/.screen/keybind.screenrc")
