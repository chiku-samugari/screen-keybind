;;;; Read screenrc source
;;; Tiny program for reading screenrc source and generate
;;; screenrc from it.
(in-package :chiku.genscreenrc)

(defun read-rcsrc (src-filename &optional (output t))
  (setf *require-insert* nil)
  (with-open-file (*standard-input* src-filename :if-does-not-exist :error)
    (if (eq output t)
      (progn
        (load *standard-input*)
        (dolist (state *require-insert*)
          (gen-insert-mode state)))
      (with-open-file (*standard-output* output :direction :output :if-exists :supersede)
        (load *standard-input*)
        (dolist (state *require-insert*)
          (gen-insert-mode state))))))
