;;;; Readers for reading more easier formats.
;;; Nov. 25th 2012, chiku
;;; This is the last step.
(in-package :chiku.genscreenrc)

(defmacro with-preserved-symbolcase (&body body)
  (with-gensyms (save-readtable-case)
    `(let ((,save-readtable-case (readtable-case *readtable*)))
       (unwind-protect
         (progn
           (setf (readtable-case *readtable*) :preserve)
           ,@body)
         (setf (readtable-case *readtable*) ,save-readtable-case)))))

(defun |[-reader| (strm c)
  (declare (ignore c))
  (destructuring-bind (key &rest cmdseq)
    (with-preserved-symbolcase
      (read-delimited-list #\] strm t))
    (list (resolve-string key)
          (multiple-value-bind (result top)
            (group-headed '! cmdseq :mark-discard? t)
            (if top (cons top result) result)))))

(set-macro-character #\[ #'|[-reader| nil)

(set-macro-character #\] (get-macro-character #\)))

(defun |{-reader| (strm c)
  (declare (ignore c))
  (destructuring-bind (start dst &rest key-cmdseqs)
    (with-preserved-symbolcase
      (read-delimited-list #\} strm t))
    `(multiple-keybinds t ',start ',dst ,@key-cmdseqs)))

(set-macro-character #\{ #'|{-reader| nil)

(set-macro-character #\} (get-macro-character #\) nil))
