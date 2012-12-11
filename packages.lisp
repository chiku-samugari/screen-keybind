;;;; Package definition
;;; Author : chiku (Takehiko Nawata, samugari@is.s.u-tokyo.ac.jp)

(in-package :cl-user)

(defpackage :chiku.util
  (:use :cl)
  (:export :with-gensyms :once-only :in :inq :in-if :aif :it :map0-n :append1  :|#`-reader|
           :concat-str :iota :drop :position-list :group-headed :itoa))

(defpackage :chiku.screen-keybind
  (:use :cl :chiku.util)
  (:export :read-rcsrc))
