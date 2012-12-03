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

(defmacro gen-insert-mode (goal &optional (leave-keys '(#\Bel #\Return #\Newline)))
  (once-only (goal)
    (with-gensyms (code insert-state-name)
      `(let ((,insert-state-name (gen-insert-state-name (resolve-string ,goal))))
         (dolist (,code (remove-if (lambda (x) (inq (code-char x) ,@leave-keys))
                                   (iota #x80)))
           (keybind-common t (octal-desc ,code) ,insert-state-name ,insert-state-name
                           ((stuff (octal-desc ,code)))
                           (default-message "insert"
                                            (concat-str "stuff "
                                                        (if (or (ctrl-code-p ,code) (= #o40 ,code))
                                                          (char-name (code-char ,code))
                                                          (string (code-char ,code)))))))
         (dolist (,code (mapcar #'char-code ',leave-keys))
           (keybind-common t (octal-desc ,code) ,insert-state-name ,goal
                           ((stuff (octal-desc  ,code)))
                           (default-message (resolve-string ,goal)
                                            (concat-str "stuff "
                                                        (if (ctrl-code-p ,code)
                                                          (char-name (code-char ,code))
                                                          (string (code-char ,code)))))))))))

;(macroexpand-1 '(gen-insert-mode snormal))
;(with-open-file (*standard-output* "insertmode.screen" :direction :output :if-exists :supersede)
;  (gen-insert-mode 'snormal))
