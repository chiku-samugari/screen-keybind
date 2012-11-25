;;;; Readers for reading more easier formats.
;;; Nov. 25th 2012, chiku
;;; This is the last step.
;{snormal snormal
;  [l focus right]
;  [s split -v ! focus right ! other ! focus left]
;}
;
;(multiple-keybinds t snromal snormal
;  (l ((focus right)))
;  (s ((split -v) (focus right) (other) (focus left))))
;
;[l focus right]
;(l ((focus right)))
;
;[s split -v ! focus right ! other ! focus left]
;(s ((split -v) (focus right) (other) (focus left)))

(defun |[-reader| (strm c)
  (declare (ignore c))
  (destructuring-bind (key &rest cmdseq)
    (read-delimited-list #\] strm t)
    (list key (multiple-value-bind (result top)
                (group-headed "!" cmdseq :mark-discard? t :key #'symbol-name :test #'string=)
                (cons top result)))))

(set-macro-character #\[ #'|[-reader| nil)

(set-macro-character #\] (get-macro-character #\)))

(quote [s split -v ! focus right ! other ! focus left])

(defun |{-reader| (strm c)
  (declare (ignore c))
  (destructuring-bind (start dst &rest key-cmdseqs)
    (read-delimited-list #\} strm t)
    `(multiple-keybinds t ,start ,dst
       ,@key-cmdseqs)))
    

(set-macro-character #\{ #'|{-reader| nil)

(set-macro-character #\} (get-macro-character #\) nil))

(quote {snormal snormal
         [l focus right]
         [s split -v ! focus right ! other ! focus left]
       })

;(with-open-file (*standard-output* ".screenrc" :direction :output :if-exists :supersede)
;{snormal snormal
;  [j focus down]
;  [k focus up]
;  [l focus right]
;  [h focus left]
;  ["C-i" focus next]
;  [s split ! focus down ! other ! focus up]
;  [v split -v ! focus right ! other ! focus left]})
