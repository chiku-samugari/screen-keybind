;;;; Read screenrc source
;;; Tiny program for reading screenrc source and generate
;;; screenrc from it.
(in-package :chiku.screen-keybind)

(defun read-rcsrc (src-filename &optional (output t))
  (clr-imode-lst)
  (with-open-file (*standard-input* src-filename :if-does-not-exist :error)
    (if (eq output t)
      (progn
        (load *standard-input*)
        (dolist (imode (get-imode-lst))
          (define-insert-mode imode)))
      (with-open-file (*standard-output* output :direction :output :if-exists :supersede)
        (load *standard-input*)
        (dolist (imode (get-imode-lst))
          (define-insert-mode imode))))))
