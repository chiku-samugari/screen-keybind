;;;; Screenrc Generator
;;; Nov. 17th 2012, chiku
;;; I decided to implement a mode like vim's normal mode on screen. It requires
;;; me to write too much same codes and thus I decided to write a converter.
;;; There are 3 categories of commands in snormal mode.
;;;
;;;     +. RETAIN : retains in the same mode after the execution of the command.
;;;     +. LEAVE : leave for the state before any key is input to screen.
;;;     +. TRANSIT : move to a different mode after the execution of the command.
;;;
;;; In principal, all these 3 category can be seen as just a variation of TRANSIT
;;; because a RETAIN command move to the same command after the execution and a
;;; LEAVE command move to a non-screen mode, if we call that state as non-screen
;;; mode.
;;;
;;; In order to move from non-screen mode to another mode, we have to use ``bindkey''

;;; This time, I try reader macro. Let me decide the format of settings first.
;;; { and } are preserved for us!
{snormal snormal
  [l "'focus right'" ! "'echo \"[SNORMAL] (focus right)\"'"]
  [h "'focus '"  ! "'echo \"[SNORMAL] (focus right)\"'"]
}

{src-state dst-state
  [input-key list-of-outputs-before-transition ! list-of-outputs-after-transition]
}

(defun |#{-reader| (strm c arg)
  (declare (ignore c arg))
  (mapcon (lambda (x)
            (mapcar (lambda (y) (list (car x) y)) (cdr x)))
          (read-delimited-list #\} strm t)))

(set-dispatch-macro-character #\# #\{ #'|#{-reader|)

(set-macro-character #\} (get-macro-character #\) nil))

(quote #{p q r s})

;;; Nov. 18th 2012, chiku
;;; No, I first have to write a program. Reader macro will behave as a coverter
;;; that converts the input file (manually written) into the form that the program
;;; can handle.

;;; Nov. 22nd 2012, chiku
;;; Dirty function. I want to use symbols to provide pieces of screenrc settings.
;;; Some macros will be proveided as outermost interfaces.
;;; But I do not know if any capital letter is used in the setting. As far as I know,
;;; there are not. Thus, downcased SYMBOL-NAME is used. If some capital letter is needed
;;; then please use a string instead of a symbol : "Capitral-LETTEr-OPtioN".
;;;  I considered to utilize READTABLE-CASE slot of readtable, but concluded it is not
;;; useful in this case because it controls the reader -- it controls what happens on
;;; READ time not EVALUATION time.
;;;
;;;     (let ((*readtable* (copy-readtable)))
;;;       (setf (readtable-case *readtable*) :preserve)
;;;       (symbol-name 'foCus))
;;;     -> FOCUS
;;;
;;; This code clearly shows the reason of the rejection. If I want to solve the problem
;;; by modifying the READTABLE-CASE slot, (setf (readtable-case *readtable*) :preserve)
;;; must be evaluated (to say, executed) before READ any use of COMMAND-DESC macro. It is
;;; not comfortable situation because the COMMAND-DESC macro is used by myself in the
;;; implementation of one upper layer, not the outermost layer (that is exposed to the user).
;;; In such case, START-STATE-DESC function must be as follows:
;;;
;;;     (DEFUN START-STATE-DESC (STATE)
;;;       (COMMAND-DESC bind -c (SYMBOL-NAME STATE)))
;;;
;;; The paremeter STATE can be state or so, though.
;;;  The another reason of rejection is the treatment of positive sign symbol: +. Even if I
;;; accept the uncomfortable situation, +1 becomes 1 when it is read. I mean, the use of
;;; READTABLE-CASE slot only solves a portion of the problem. But this function can solve
;;; the situtation.
(defun resolve-string (string-designator)
  (cond ((symbolp string-designator) (string-downcase string-designator))
        ((characterp string-designator) (string string-designator))
        ((stringp string-designator) string-designator)
        (t (error "Malformed string-designator: ~s" string-designator))))

(resolve-string 'resize)

(defmacro spacing-join (&rest args)
  `(concat-str ,@(mapcan (lambda (x) `(" " ,x)) args)))

(defun |#`-reader| (strm c n)
  (declare (ignore c))
    `(lambda ,(map0-n (lambda (n) (intern (format nil "A~a" n))) (1- (or n 1)))
       ,(read (make-concatenated-stream (make-string-input-stream "`") strm)
              t nil t)))

(set-dispatch-macro-character #\# #\` #'|#`-reader|)

;;; I decided to implement this operator as a macro because it is one of the
;;; interface I use for the implementation of next layer, START-STATE-DESC and so on.
;;; SYMBOL-MACROLET!
;;;  Nov. 23rd 2012, chiku
;;;     (lambda (state)
;;;       (command-desc bind -c state))
;;; The above form is correctly handled. STATE is not quoted during the expansion of
;;; COMMAND-DESC macro. This is really good to write code pieces that use this macro,
;;; but when it comes to read, I guess it is not comfortable.
(defmacro command-desc (command &rest args &environment env)
  `(symbol-macrolet ,(mapcar #`(,a0 ',a0)
                         (remove-if-not
                           (lambda (sym)
                             (and (symbolp sym)
                                  (not (eq (sb-cltl2:variable-information sym env) :lexical))))
                           (cons command args)))
     (concat-str "'"
                 (resolve-string ,command)
                 (spacing-join
                   ,@(mapcar #`(resolve-string ,a0) args))
                 "'")))

(command-desc focus 'left up down (concat-str "a" "b") (intern "ABC"))
(command-desc 'focus 'left up down (concat-str "a" "b") (intern "ABC"))
(command-desc resize -l -v "+1")

(defun spacing-join-fn (&rest args)
  (if (cdr args)
    (apply #'concat-str (drop (mapcan (papply (list " ")) args)))
    (car args)))

(spacing-join-fn "ab" "cd" "ef")

(defun command-desc-fn (command &rest args)
  (concat-str "'"
              (resolve-string command)
              " "
              (apply #'spacing-join-fn (mapcar #'resolve-string args))
              "'"))

(command-desc-fn 'focus 'left 'up 'down (concat-str "a" "b") (intern "ABC"))
(command-desc-fn 'split '-v)

(defun start-state-desc (state)
  (command-desc bind -c state))

(start-state-desc 'snormal)

(defun dst-state-desc (state)
  (command-desc command -c state))

(dst-state-desc 'snormal)

(defun echo-command (msg)
  (command-desc echo (concat-str "\"" msg "\"")))

(echo-command "let me know")

(defun keybind-desc (key start dst before-lst after-lst)
  (spacing-join-fn
    (start-state-desc start)
    (resolve-string key)
    "eval"
    (apply #'spacing-join-fn (mapcar (lambda (x) (apply #'command-desc-fn x)) before-lst))
    (dst-state-desc dst)
    (apply #'spacing-join-fn (mapcar (lambda (x) (apply #'command-desc-fn x)) after-lst))))

(keybind-desc "s" 'snormal 'snormal '((split -v) (focus right) (other) (focus left))
              '((echo "\"[snormal] (split -v)\"")))

(defun commonplace-keybind-desc (key start dst command-lst &optional message)
  (keybind-desc key start dst command-lst
                (list
                  (if message
                    (list 'echo message)
                    (list 'echo
                          (concat-str "\"[" (resolve-string dst) "] ("
                                      (apply #'spacing-join-fn (mapcar #'resolve-string (car command-lst)))
                                      ")\""))))))

(commonplace-keybind-desc "k" 'snormal 'sinsert '((split -v) (focus right) (other) (focus left)))
(commonplace-keybind-desc "s" 'snormal 'sinsert '((split -v) (focus right) (other) (focus left)))

(defmacro keybindings (strm start dst &body key-command-msg-lst)
  (with-gensyms (key-com-msg key cmd msg)
    `(dolist (,key-com-msg ',key-command-msg-lst)
       (format ,strm "~%~a"
               (destructuring-bind (,key ,cmd &optional ,msg)
                 ,key-com-msg
                 (commonplace-keybind-desc ,key ',start ',dst (if (atom (car ,cmd))
                                                                 (list ,cmd),cmd)
                                           ,msg))))))

;(defmacro keybinding% (key start dst (&rest befores) (&rest afters))
;  `(spacing-join
;     (start-state-desc ',start)
;     (resolve-string ',key)
;     "eval"
;     ,@(mapcar #`(command-desc ,(car a0) ,@(cdr a0)) befores)
;     (dst-state-desc ',dst)
;     ,@(mapcar #`(command-desc ,(car a0) ,@(cdr a0)) afters)))
;
;(macroexpand-1 '(keybinding% j snormal snormal ((focus down))
;                             ((echo "\"snormal (focus down)\""))))
;(format t "~a" (keybinding% j snormal snormal ((focus down)) ((echo "\"snormal (focus down)\""))))
;(format t "~a" (keybinding% "J" snormal snormal ((focus down)) ((echo "\"snormal (focus down)\""))))
;
;(defmacro defkeybind (key start dst (&rest command) &optional message)
;  `(keybinding% ,key ,start ,dst
;                (,@(if (atom (car command))
;                    `(,command)
;                    command))
;                (,(if (null message)
;                    (echo-command
;                      (concat-str "[" (resolve-string dst) "] ("
;                                  (reduce (papply (concat-str
;                                                    (resolve-string _) " " (resolve-string _)))
;                                          (if (atom (car command))
;                                            command (car command))
;                                          :initial-value "")
;                                  ")"))
;                    (echo-command message)))))
;
;(format t "~a" (defkeybind j snormal snormal (focus down) "snormal (focus down)"))
;(format t "~a" (defkeybind j snormal snormal (focus down)))
;
;(defmacro keybindings (strm start dst &body key-command-msg-lst)
;  (with-gensyms (key-com-msg key cmd msg)
;    `(dolist (,key-com-msg ',key-command-msg-lst)
;       (format ,strm "~%~a"
;               (destructuring-bind (,key ,cmd &optional ,msg)
;                 ,key-com-msg
;                 (defkeybind ,key ,start ,dst ,cmd  ,msg))))))

(keybindings t snormal snormal
  (j (focus down))
  (k (focus up))
  (d (remove))
  (s ((split -v) (focus right) (other) (focus left))))
