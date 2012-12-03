;;;; Screen Keybind
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
;;;     3. bind -c X s eval 'split' 'focus down' 'other' 'focus up' 'echo "leave [X]"'
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

(defun spacing-join (&rest strs)
  (format nil "~{~a~^ ~}" strs))

(defun join/40 (&rest args)
  (format nil "~{~a~^\\40~}" args))

(join/40 "split" "-v")
(princ
  (apply #'spacing-join
         (mapcar (papply (apply #'join/40))
                 '(("split" "-v")
                   ("focus" "right")
                   ("other")
                   ("focus" "left")))))

(defun dqstringify (str)
  (concat-str "\"" str "\""))

(defmacro gen-escape-fn (prepend-char &rest chars)
  (with-gensyms (c)
    `(lambda (,c)
       (if (inq ,c ,@chars)
         (list ,prepend-char ,c)
         (list ,c)))))

(macroexpand-1 '(gen-escape-fn #\\ #\" #\# #\^))

(defmacro escape (prepend-char str &rest chars)
  (once-only (prepend-char)
    `(coerce (mapcan (gen-escape-fn ,prepend-char ,@chars)
                     (coerce ,str 'cons))
             'string)))

(macroexpand-1 '(escape #\\ "^^" #\" #\# #\^))

(defun make-dqstring (str)
  " Returns a string whose content is recognized identical to ``str'';{{{
    in the meaning of screen's double quoted string.
    Please be aware that ``str'' is a lisp object, string.
     Actually speaking, this function is a little strange. I mean,
    it should not exists. For example, if ^ is required to escaped
    by prepending a \, then it should be  done by the input side. Even
    if this function exists or not, the input side have to care about very
    similar point: which character should be escaped? If this function
    didn't exists, the input side must know which character must be
    escaped, and construct proper screen-dq-string. This function is
    only for reducing this annoying part. Thus, I have to do following
    2 things:
        1. DO NOT scented the existance of this function!
        2. Advertise NOTHING must be escaped. Just write the
           screen string contents, and wrap it by double quotes!
           The double quotes are for making it to Lisp string,
           not screen string.
  ";}}}
  (dqstringify
    (escape #\\ str #\" #\# #\^ #\\)))

(join/40 "split" "-v")
(princ
  (apply #'spacing-join
         (mapcar (papply (apply #'join/40))
                 (maptree #'make-dqstring
                 '(("split" "-v")
                   ("focus" "right")
                   ("other")
                   ("focus" "left"))))))

(defun make-evalarg (&rest command-components)
  (apply #'join/40 (mapcar #'make-dqstring command-components)))

(defun state-leave-desc (state)
  (case state
    (raw "bindkey")
    (screen "bind")
    (t (concat-str "bind -c " (resolve-string state)))))

(defun state-arrive-desc (state)
  (case state
    (raw "")
    (screen (make-evalarg "command"))
    (t (make-evalarg "command" "-c" (resolve-string state)))))

(princ (state-arrive-desc 'screen))
(princ (state-arrive-desc 'snormal))

;;; desc : string
;;; state : string-designator
;;; key : string or an integer number or a character. Symbol is NOT allowed.
;;;       How to treat octal numbers?

(defun process-evalarg-lst (evalarg-lst)
  (if (or (null evalarg-lst)
          (and (null (cdr evalarg-lst))
               (zerop (length (car evalarg-lst)))))
    ""
    (apply #'spacing-join evalarg-lst)))

(defun keybind-desc (key start-state goal-state prior-evalargs post-evalargs)
  (string-trim " "
               (spacing-join (state-leave-desc start-state)
                             (resolve-string key)
                             "eval"
                             (process-evalarg-lst prior-evalargs)
                             (state-arrive-desc goal-desc)
                             (process-evalarg-lst post-evalargs))))

(princ
  (keybind-desc #\t 'snormal 'snormal (list (make-evalarg "colon"))
                (list (make-evalarg "echo" "[screen-insert] (colon)"))))
;;; Another format of above one. These two does same thing.
(princ
  (keybind-desc #\t 'snormal 'snormal (list (make-evalarg "colon"))
                (list (make-evalarg"echo" "\"[screen-insert]\\40(colon)\""))))
;;; Dec. 3rd 2012, chiku
;;; As you can aware, one argument for eval command  is a non-quoted screen
;;; string. In screen-keybind project, it is called EVALARG. In non-quoted
;;; screen string, \40 works as a separator. Each component of a command is
;;; double-quoted screen string. In addition, a command sequence is
;;; space-separated commands. A command sequence is the argument for eval
;;; command.
;;;
;;;  You may feel that the application of MAKE-EVALARG function should be done
;;; by KEYBIND-DESC function. But it is wrong. In that approach, one EVALARG
;;; source is space-separated string. In general, we cannot know which space
;;; should be treated as separaters and which are not. In almost all cases, it
;;; is possible but bosering.  Here is an example:
;;;
;;;     "echo \"[screen-insert] (colon)\""
;;;
;;; We have to replace a spce by \40 if it was not inside a double quoted string.
;;; The important point is, there seems no worth to write that code. This
;;; KEYBIND-DESC will not be used by a human. On the more higher layer, the
;;; most comfortable format (at least, most comfortable and natural for me)
;;; is 2 level list format. Thus, another possible solution is to adopt that
;;; 2 leve list format on this level:
;;;
;;;     `(("echo" ,(make-dqstring "[screen-insert] (colon)")))
;;;         or
;;;     (list (list "echo" (make-dqstring "[screen-insert] (colon)")))
;;;
;;; I do not know if I should implement it on this layer. I think both of them
;;; are acceptable.

(keybind-desc #\C 'screen "X"
              (list (make-evalarg "split" "-v") (make-evalarg "focus" "right")
                    (make-evalarg "other") (make-evalarg "focus" "left"))
              (list (make-evalarg "echo" "enter [X]")))

(keybind-desc 'c 'screen "X"
              (list (make-evalarg "split" "-v") (make-evalarg "focus" "right")
                    (make-evalarg "other") (make-evalarg "focus" "left"))
              (list (make-evalarg "echo" "enter [X]")))

(keybind-desc "^v" 'screen "X"
              (list (make-evalarg "split" "-v") (make-evalarg "focus" "right")
                    (make-evalarg "other") (make-evalarg "focus" "left"))
              (list (make-evalarg "echo" "enter [X]")))

(keybind-desc "^s" "X" "X"
              (list (make-evalarg "split" "-v") (make-evalarg "focus" "right")
                    (make-evalarg "other") (make-evalarg "focus" "left"))
              (list (make-evalarg "echo" "enter [X]")))

(keybind-desc "v" "X" "X"
              (list (make-evalarg "split" "-v") (make-evalarg "focus" "right")
                    (make-evalarg "other") (make-evalarg "focus" "left"))
              ())

(keybind-desc "v" "X" "X"
              (list (make-evalarg "split" "-v") (make-evalarg "focus" "right")
                    (make-evalarg "other") (make-evalarg "focus" "left"))
              (list ""))

(defun take-lispcode (x)
  (cadr x))

;;; comitem : string
(defun comitem-stringify-form (comitem)
  (cond ((atom comitem) (resolve-string comitem))
        ;; This is required for really minor case. If we want to use
        ;; single lexical variable as acom item, then this is useful.
        ;; (identity x) is alternative way in such case, though.
        ((and (consp comitem) (eq (car comitem) :lisp))
         (take-lispcode comitem))
        (t comitem)))

(comitem-stringify-form 'left)
(comitem-stringify-form '(coerce (list #\l #\e #\f #\t) 'string))
(comitem-stringify-form '(:lisp (concatenate 'string (list #\c #\l))))

(defun com-evalargify-form (com)
  (if (eq (car com) :lisp)
    (take-lispcode com)
    `(make-evalarg ,@(mapcar #'comitem-stringify-form com))))

(com-evalargify-form '(focus left))
(com-evalargify-form '(focus (coerce (list #\l #\e #\f #\t) 'string)))
(com-evalargify-form '(:lisp (concatenate 'string "focus " (concatenate 'string "ri" "ght"))))

(defun comseq-construct-form (comseq)
  (if (eq (car comseq) :lisp)
    (take-lispcode comseq)
    `(list ,@(mapcar #'com-evalargify-form comseq))))

(comseq-construct-form '((split -v) (focus right) (other) (focus left)))
(comseq-construct-form '((split) (:lisp (string #\c))))
(comseq-construct-form '(:lisp (list "split -v" (concat-str "fo" "cus" " right"
                                                            (spacing-join " other" "focus" "left")))))

(defmacro keybind (strm key start goal (&rest prior-coms) (&rest post-coms))
  " key : a string or a character or an integer
    start,goal : a symbol. not evaluated.
  "
  `(format ,strm "~a~%"
           (keybind-desc ,key ',start ',goal
                         ,(comseq-construct-form prior-coms)
                         ,(comseq-construct-form post-coms))))

;;; COMSEQ-CONSTRUCT-FORM, COM-EVALARGIFY-FORM and COMITEM-STRINGIFY-FORM return
;;; a form that returns one string if evaluated. A function that returns a form
;;; is of course quite natural when we are writing macros. What I want to
;;; emphasize here is, do not use it in a function definition. It works in
;;; macro for constructing a code.

(keybind t "^c" raw screen ((focus up)) ())
(keybind t "^p" window-select window-select
         ((prev))
         ((echo (make-dqstring "[window-select] (prev)"))))
;;; Both of following two commands outputs [s] as an echo message:
;;;     1. eval "echo"\40"[s]"
;;;     2. eval "echo"\40"\"[s]\""
;;; In 1., [s] is the argument for echo command. The argument for echo command
;;; in 2. is "[s]".
;;;     alt-1. echo [s]
;;;     alt-2. echo "[s]"

;;; The :LISP form allows follwing thing. This example is not preferrable, though.
(let ((x "'focus down'")
      (y "-v"))
  (keybind nil "j" window-select window-select
           ((:lisp x) (focus up) (split (:lisp y)))
           ()))

(define-symbol-macro escape (format t "~&escape ~a~%" (resolve-string (read))))

(defparameter *hardstatus-string* "")

(defun message (msg &optional (show-state nil show-state-p)
                              (hardstatus-format *hardstatus-string*))
  (make-dqstring
    (concat-str
      (if show-state-p
        (concat-str "%{= }%?%E[" show-state  "]" msg "%:[raw]%?")
        msg)
      hardstatus-format)))

;;; Dec. 3rd 2012, chiku
;;; In order to output one ^ in the hardstatus line, a man should provide
;;; "\"\\^\\^\"" to screen-keybind, and screen-keybind will convert it into
;;; "\"\\\"\\\\\\^\\\\\\^\\\"\"". That user is responsible for providing the
;;; first one. This DEFAULT-MESSAGE function has same responsibility. This is
;;; the reason for why I write the escape process in this function and why
;;; I do not write such escape process for MESSAGE function.
(defun default-message (dst primary-command-desc)
  (message (if (not (zerop (length primary-command-desc)))
             (concat-str " (" (escape #\^ primary-command-desc #\^) ")")
             "")
           (resolve-string dst)))

(defun set-hardstatus-string (str)
  (progn (setf *hardstatus-string* str)
         (format t "~&hardstatus string ~a~%"
                 (default-message 'screen ""))))

(define-symbol-macro hardstatus-string
                     (set-hardstatus-string (read)))

(set-hardstatus-string "%030=%{B.} %{-}%-w%{=b Mw}%{+u}%{+s}%n %t%{-}%{-}%{-}%+w%{B.} %{-}%=%m/%d %02c")

(defmacro keybind-common (strm key start dst (&rest commands) &optional message)
  " message : any form that is evaluated into a string.
              A string literal is welcome, of course.
   "
  `(keybind ,strm ,key ,start ,dst
            ,commands
            (,(cond ((null message)
                     `(hardstatus string
                                  (:lisp (default-message
                                           ',dst
                                           (spacing-join
                                             ,@(mapcar #'comitem-stringify-form
                                                       (car commands)))))))
                    ((zerop (length message)) ())
                    (t `(hardstatus string (:lisp (message ,message))))))))

(keybind-common t "j" snormal snormal ((focus down)))
(keybind-common t "k" screen raw ((focus up)) "focus up")
(keybind-common t "v" screen raw ((split -v) (focus right) (other) (focus left)) "vertical split")
(keybind-common t "v" snormal snormal ((split -v) (focus right) (other) (focus left)))
(keybind-common t "s" snormal snormal ((split) (focus right) (other) (focus left)))
; a form generates the message
(keybind-common nil "l" snormal snormal ((focus right)) (coerce (coerce "focus right" 'list) 'string))
(keybind-common nil "l" snormal snormal ((focus right)) "")
(keybind-common nil "^z" raw screen ())
(keybind-common t "^" snormal-insert snormal-insert ((stuff ^)))

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
                      `(keybind-common ,strm ,key ,start ,dst
                                       ,(normalize-cmd-format cmd)
                                       ,msg)))
                  key-command-msg-lst)))
    `(progn
       ,@forms)))

(multiple-keybinds t snormal snormal
  ("k" (focus up))
  ("j" (focus down))
  ("^i" (focus next))
  ("s" ((split) (focus down) (other) (focus up)))
  ("v" ((split -v) (focus right) (other) (focus left)) "vertical split"))

(multiple-keybinds t screen raw
  ("^n" next)
  ("^p" prev)
  (0 (select 0))
  ("-" (select -)))
