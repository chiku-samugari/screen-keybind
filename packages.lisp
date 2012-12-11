;;;; Package definition
;;; Author : chiku (Takehiko Nawata, samugari@is.s.u-tokyo.ac.jp)

(in-package :cl-user)

(defpackage :chiku.screen-keybind
  (:use :cl :chiku.util)
  (:export :read-rcsrc))
