;;;; Some utilities I daily use.
;;; Author: chiku (Takehiko Nawata, samugari@is.s.u-tokyo.ac.jp)
(in-package :chiku.util)

(defun iota (num &key (start 0) (step 1))
  (do ((i start (+ i step))
       (acc '() (cons i acc))
       (c 0 (1+ c)))
    ((= c num) (nreverse acc))))

;;; From ``Practical Common Lisp''
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
             ,@body)))))

;;; From ``On Lisp''
(defmacro in (obj &rest choices)
  (with-gensyms (insym)
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c)) choices)))))

(defmacro inq (obj &rest choices)
  `(in ,obj ,@(mapcar #'(lambda (c) `',c) choices)))

(defmacro in-if (fn &rest choices)
  (with-gensyms (fn-var)
    `(let ((,fn-var ,fn))
       (or ,@(mapcar #'(lambda (c) `(funcall ,fn-var ,c)) choices)))))

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defun map0-n (fn n)
  (mapcar fn (iota (1+ n))))

(defun append1 (lst obj) (append lst (list obj)))

;;; From ``Let Over Lambda''
(defun |#`-reader| (strm c n)
  (declare (ignore c))
    `(lambda ,(map0-n (lambda (n) (intern (format nil "A~a" n))) (1- (or n 1)))
       ,(read (make-concatenated-stream (make-string-input-stream "`") strm)
              t nil t)))

(set-dispatch-macro-character #\# #\` #'|#`-reader|)

;;; Others
(defun concat-str (&rest strs)
  (apply #'concatenate (cons 'string strs)))

(defun drop (seq &optional (n 1)) (subseq seq n))

(defun position-list (item seq &key (key #'identity) (test #'eql))
  (labels ((rec (s acc)
                (let ((pos (position item s :key key :test test)))
                  (if pos
                    (rec (subseq s (1+ pos)) (cons (1+ pos) acc))
                    (nreverse (maplist #'(lambda (x) (reduce #'+ x)) acc))))))
    (rest (rec seq (list -1)))))

(defun group-headed (head-mark seq &key (mark-discard? nil) (key #'identity) (test #'eql))
  (let ((head-pos (position-list head-mark seq :key key :test test)))
    (values
      (mapcar
        (lambda (&rest args) (apply #'subseq seq args))
        (if mark-discard? (mapcar #'1+ head-pos) head-pos)
        (drop (append1 head-pos (length seq))))
      (subseq seq 0 (car head-pos)))))

(defun ITOA (num)
  (format nil "~a" num))
