;;;; Read screenrc source
;;; Tiny program for reading screenrc source and generate
;;; screenrc from it.
(in-package :chiku.genscreenrc)

(defun read-rcsrc (src-filename &optional (output-filename ".screenrc"))
  (with-open-file (*standard-output* output-filename :direction :output :if-exists :supersede)
    (load src-filename)))
