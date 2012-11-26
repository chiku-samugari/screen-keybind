;;;; Screenrc Generator
;;; Nov. 27th 2012, chiku
;;; I start the second implementation that can treat all state transition. Let me
;;; call the initial state ``raw,'' the state after screen-escape key ``screen''
;;; and all other states ``X.'' Then, following 9 patterns are possible:
;;;
;;;     1. raw -> screen
;;;     2. screen -> X
;;;     3. X -> raw
;;;     4. raw -> X
;;;     5. X -> screen
;;;     6. screen -> raw
;;;     7. raw -> raw
;;;     8. screen -> screen
;;;     9. X -> X
;;;
;;; Please be aware that ``->'' intends not only a state transition but also
;;; a sequence of commands could be executed along to that state transition.
;;; The required command sequence is different from 1. to 9. as follows:
;;;
;;;     1. escape ^z
;;;         OR bindkey ^z eval 'command'
;;;         OR bindkey ^z eval 'focus up' 'command' 'echo "enter [screen]"'
;;;     2. bind r eval 'command -c X' 'echo "enter [X]"'
;;;     3. bind -c X s 'split' 'focus down' 'other' 'focus up' 'echo "leave [X]"'
;;;     4. bindkey ^w eval 'other' 'command -c X' 'echo "enter [X]"'
;;;     5. bind -c X k eval 'focus up' 'command' 'echo "enter [screen]"'
;;;     6. bind w windowlist -b
;;;     7. bindkey ^a eval 'focus up'
;;;     8. bind + eval 'resize +1' 'command'
;;;     9. bind -c X j eval 'focus down' 'command -c X'
;;;
;;; As you can see, we have to properly use 3 diffierent commands ``bind,''
;;; ``bind -c'' and ``bindkey.''
;;;
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
(in-package :chiku.genscreenrc)

(defun resolve-string (strsrc)
  (cond ((numberp strsrc) (itoa strsrc))
        ((symbolp strsrc) (string-downcase strsrc))
        ((characterp strsrc) (string strsrc))
        ((stringp strsrc) strsrc)
        (t (error "Malformed string source: ~s" strsrc))))

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

(defun start-state-desc (state)
  (if (eq state 'non-screen)
    "bindkey"
    (concat-str "bind -c " (resolve-string state))))

(defun dst-state-desc (state)
  (if (eq state 'non-screen)
    ""
    (command-desc command -c state)))

(defmacro keybind-desc (key start dst (&rest befores) (&rest afters))
  `(drop (spacing-join
           (start-state-desc ',start)
           (resolve-string ',key)
           "eval"
           ,@(mapcar #`(command-desc ,(car a0) ,@(cdr a0)) befores)
           (dst-state-desc ',dst)
           ,@(mapcar #`(command-desc ,(car a0) ,@(cdr a0)) afters))))

(defmacro keybind-common (key start dst (&rest commands) &optional message)
  `(keybind-desc ,key ,start ,dst ,commands
                (,(if (null message)
                    `(echo
                       (concat-str "\"[" (resolve-string ',dst) "] ("
                                   (format nil "~{~a~^ ~}" (mapcar #'resolve-string
                                                                   (car ',commands)))
                                   ")\""))
                    `(echo (concat-str "\"" ,message"\""))))))

;;; Nov. 25th 2012, chiku
;;; The latest version was not good to use. This is the outermost interface
;;; and thus we are pretty sure that immidiate descriptions are written.
(defun normalize-cmd-format (cmd)
  (cond ((null cmd) '())
        ((symbolp cmd) `((,cmd)))
        ((symbolp (car cmd)) `(,cmd))
        (t cmd)))

(defmacro multiple-keybinds (strm start dst &body key-command-msg-lst)
  (let ((forms
          (mapcar (lambda (key-com-msg)
                    (destructuring-bind (key cmd &optional msg)
                      key-com-msg
                      `(format ,strm "~a~%"
                               (keybind-common ,key ,start ,dst
                                               ,(normalize-cmd-format cmd)
                                               ,msg))))
                  key-command-msg-lst)))
    `(progn
       ,@forms)))
