;;;; Build screen-keybind
;;; Author : chiku (Takehiko Nawata, samugari@is.s.u-tokyo.ac.jp)

(in-package :cl-user)

(load "packages.lisp")

(load "main.lisp")

(load "insert-mode.lisp")

(load "reader-definitions.lisp")

(load "read-rcsource.lisp")

(defun main ()
  (let ((*package* (find-package :chiku.screen-keybind)))
    (aif (third *posix-argv*)
      (chiku.screen-keybind:read-rcsrc (second *posix-argv*) it)
      (chiku.screen-keybind:read-rcsrc (second *posix-argv*)))))

(save-lisp-and-die "screen-keybind" :toplevel 'main :executable t)
