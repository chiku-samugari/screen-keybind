(in-package :cl-user)

(require :sb-cltl2)

(load "packages.lisp")

(load "main.lisp")

(load "reader-definitions.lisp")

(load "insert-mode.lisp")

(load "read-rcsource.lisp")

(defun main ()
  (let ((*package* (find-package :chiku.genscreenrc)))
    (aif (third *posix-argv*)
      (chiku.genscreenrc:read-rcsrc (second *posix-argv*) it)
      (chiku.genscreenrc:read-rcsrc (second *posix-argv*)))))

(save-lisp-and-die "gen-screenrc" :toplevel 'main :executable t)
