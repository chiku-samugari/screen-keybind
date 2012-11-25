;;;; Readers for reading more easier formats.
;;; Nov. 25th 2012, chiku
;;; This is the last step.
(in-package :chiku.genscreenrc)

(defun |[-reader| (strm c)
  (declare (ignore c))
  (destructuring-bind (key &rest cmdseq)
    (read-delimited-list #\] strm t)
    (list key (multiple-value-bind (result top)
                (group-headed "!" cmdseq :mark-discard? t :key #'symbol-name :test #'string=)
                (cons top result)))))

(set-macro-character #\[ #'|[-reader| nil)

(set-macro-character #\] (get-macro-character #\)))

(defun |{-reader| (strm c)
  (declare (ignore c))
  (destructuring-bind (start dst &rest key-cmdseqs)
    (read-delimited-list #\} strm t)
    `(multiple-keybinds t ,start ,dst
       ,@key-cmdseqs)))

(set-macro-character #\{ #'|{-reader| nil)

(set-macro-character #\} (get-macro-character #\) nil))
