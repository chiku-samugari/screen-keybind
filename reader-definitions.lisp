;;;; Readers for reading more easier formats.
;;; Author : chiku (Takehiko Nawata, samugari@is.s.u-tokyo.ac.jp)
;;;
;;; Nov. 25th 2012, chiku
;;; This is the last step.

(in-package :chiku.screen-keybind)

(defmacro with-preserved-symbolcase (&body body)
  (with-gensyms (save-readtable-case)
    `(let ((,save-readtable-case (readtable-case *readtable*)))
       (unwind-protect
         (progn
           (setf (readtable-case *readtable*) :preserve)
           ,@body)
         (setf (readtable-case *readtable*) ,save-readtable-case)))))

(defun case-sensitive-resolve-string (strsrc)
  (if (symbolp strsrc)
    (symbol-name strsrc)
    (resolve-string strsrc)))

(defun |[-reader| (strm c)
  (declare (ignore c))
  (destructuring-bind (key &rest cmdseq)
    (with-preserved-symbolcase
      (read-delimited-list #\] strm t))
    (list (case-sensitive-resolve-string key)
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
