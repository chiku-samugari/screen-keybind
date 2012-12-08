;;;; Hold the control during the input.
;;; Dec. 01st 2012, chiku
;;; In order to keep snormal mode after the execution of a command
;;; that comes with input, we have to avoid going back to the raw
;;; mode during the input.
(in-package :chiku.screen-keybind)

(defun octal-desc (code)
  (format nil "\\~o" code))

(defun ctrl-code-p (code)
  (< code #b100000))

(defun use-charname-p (code)
  (or (ctrl-code-p code) (= #o40 code)))

(defun stuffmsg-string (code)
  (concat-str "stuff "
              (if (use-charname-p code)
                (char-name (code-char code))
                (string (code-char code)))))

(defun define-insert-mode (imode &optional
                               (finish-keys '(#\Return #\Newline))
                               (cancel-keys '(#\Bel)))
  (macrolet ((aux (cur goal print-mode msg)
               `(keybind-common t (octal-desc code)
                                ,cur ,goal
                                ((stuff (octal-desc code)))
                                (default-message ,print-mode ,msg))))
    (with-slots (mode primary-mode insert-mode next-mode) imode
      (dolist (code (iota #x80))
        (cond ((member (code-char code) finish-keys)
               (aux insert-mode next-mode next-mode ""))
              ((member (code-char code) cancel-keys)
               (aux insert-mode primary-mode primary-mode ""))
              (t (aux insert-mode (if (eq mode 'transient)
                                     next-mode insert-mode)
                      "insert"
                      (stuffmsg-string code))))))))
