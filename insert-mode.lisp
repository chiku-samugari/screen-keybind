;;;; Hold the control during the input.
;;; Dec. 01st 2012, chiku
;;; In order to keep snormal mode after the execution of a command
;;; that comes with input, we have to avoid going back to the raw
;;; mode during the input.
(in-package :chiku.genscreenrc)

(remove-if-not #'graphic-char-p (mapcar #'code-char (iota #x80)))

(mapcar #'code-char (iota #x80))

(mapcar (apapply (format t "~s(~:*~c)~10t~7b~10t~:*~o~%" (code-char a0) a0)) (iota #x80))

;;; Okay, I understand the situation.
(format t "~{bind \\~o eval 'command -c insert'~%~}~%" (iota #x80))

(dolist (octal-desc (mapcar (papply (format nil "\\~o"))
                            (remove-if (lambda (x) (inq (code-char x) #\ #\))
                                       (iota #x80))))
  (keybind-common t octal-desc 'insert 'insert ()))

(defun octal-desc (code)
  (format nil "\\~o" code))

(defun ctrl-code-p (code)
  (< code #b100000))

(ctrl-code-p (char-code #\space))
(ctrl-code-p (char-code #\Newline))
(ctrl-code-p (char-code #\!))

(defun use-charname-p (code)
  (or (ctrl-code-p code) (= #o40 code)))

(defun stuffmsg-string (code)
  (concat-str "stuff "
              (if (use-charname-p code)
                (char-name (code-char code))
                (string (code-char code)))))

(defun gen-insert-mode (istate &optional
                               (finish-keys '(#\Return #\Newline))
                               (cancel-keys '(#\Bel)))
  (macrolet ((aux (cur goal msg)
               `(keybind-common t (octal-desc code)
                                ,cur ,goal
                                ((stuff (octal-desc code)))
                                (default-message ,msg (stuffmsg-string code)))))
    (with-slots (mode primary-state insert-state next-state) istate
      (dolist (code (iota #x80))
        (cond ((member (code-char code) finish-keys) (aux insert-state next-state next-state))
              ((member (code-char code) cancel-keys)
               (aux insert-state primary-state primary-state))
              (t (aux insert-state (if (eq mode 'transient)
                                     next-state insert-state)
                      "insert")))))))

;;; #\Bel, #\Newline and #\Return : "command", others : "command"\40"-c"\40"screen-insert-layer1"
;(gen-insert-mode (make-common-istate 'layer '|screen| '|screen| 1))
;;; all : "command"
;(gen-insert-mode (make-common-istate 'transient '|screen| '|screen| 1))
;;; #\Bel : "command"
;;; others : "command"\40"-c"\40"screen-insert-transient2"
;(gen-insert-mode (make-common-istate 'transient '|screen| '|screen-insert-transient2| 1))
;;; #\Bel : "command"\40"-c"\40"snormal"
;;; #\Newline and #\Return : "command"\40"-c"\40"snormal-insert-layer2"
;;; others : "command"\40"-c"\40"screen-insert-layer1"
;(gen-insert-mode (make-common-istate 'layer 'snormal '|snormal-insert-layer2| 1))

;(with-open-file (*standard-output* "insertmode.screen" :direction :output :if-exists :supersede)
;  (gen-insert-mode (make-common-istate 'layer 'snormal 'snormal 1)))
