;;;; Read screenrc source
;;; Tiny program for reading screenrc source and generate
;;; screenrc from it.
(in-package :chiku.genscreenrc)

(defun read-rcsrc (src-filename &optional (output t))
  (if (eq output t)
    (load src-filename)
    (with-open-file (*standard-output* output :direction :output :if-exists :supersede)
      (load src-filename))))
