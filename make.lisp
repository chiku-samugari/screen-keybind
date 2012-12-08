(in-package :cl-user)

(load "packages.lisp")

(load "main.lisp")

(load "insert-mode.lisp")

(load "reader-definitions.lisp")

(load "read-rcsource.lisp")

(defun main ()
  (let ((*package* (find-package :chiku.genscreenrc)))
    (aif (third *posix-argv*)
      (chiku.genscreenrc:read-rcsrc (second *posix-argv*) it)
      (chiku.genscreenrc:read-rcsrc (second *posix-argv*)))))

(save-lisp-and-die "screen-keybind" :toplevel 'main :executable t)
