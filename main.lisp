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

(defun dqstringify (str)
  (concat-str "\"" str "\""))

(defmacro gen-escape-fn (prepend-char &rest chars)
  (with-gensyms (c)
    `(lambda (,c)
       (if (inq ,c ,@chars)
         (list ,prepend-char ,c)
         (list ,c)))))

(defmacro escape (prepend-char str &rest chars)
  (once-only (prepend-char)
    `(coerce (mapcan (gen-escape-fn ,prepend-char ,@chars)
                     (coerce ,str 'cons))
             'string)))

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
    (if (zerop (length str))
      ""
      (escape #\\ str #\" #\# #\^ #\\))))

(defclass comitemseq ()
  ((command :initarg :command :reader command)
   (args :initarg :args :reader args)))

(defun make-comitemseq (command &rest args)
  (make-instance 'comitemseq :command command :args args))

(defgeneric make-evalarg (com&args)
  (:documentation "Return an evalarg by concatenating command and arguments."))

(defun evalargify (&rest comitems)
  (apply #'join/40 (mapcar #'make-dqstring comitems)))

(defmethod make-evalarg ((com&args comitemseq))
  (apply #'evalargify (command com&args) (args com&args)))

(defun state-leave-desc (state)
  (case state
    (|raw| "bindkey")
    (|screen| "bind")
    (t (concat-str "bind -c " (resolve-string state)))))

(defun state-arrive-desc (state)
  (case state
    (|raw| "")
    (|screen| (evalargify "command"))
    (t (evalargify "command" "-c" (resolve-string state)))))

(defun input-invoke-p (comitemseq)
  (with-slots (command args) comitemseq
    (macrolet ((regardless-of-args (&rest strs)
                 `(in-if (papply #'string= command) ,@strs))
               (if-completely-same (&rest str-lsts)
                 `(in-if (papply #'string= (make-evalarg comitemseq))
                         ,@(mapcar #`(evalargify ,@(if (consp a0) a0 (list a0))) str-lsts))))
      (cond ((or (regardless-of-args "colon" "windowlist")
                 (if-completely-same "stuff" "title" "paste" "process" "readreg"
                                     "select")
                 (and (string= "setenv" command) (= (length args) 2)))
             (cons 'layer 1))
            ((if-completely-same "password" "su" "setenv") (cons 'layer 2))
            ((if-completely-same "digraph") (cons 'transient 2))
            (t nil)))))

(defun gen-insert-state (primary-state mode n)
  (intern (format nil "~a-insert-~a~a" (resolve-string primary-state) (resolve-string mode) n)))

(defstruct (istate (:constructor make-istate (&key mode primary-state insert-state next-state))
                   (:constructor make-common-istate
                    (mode primary-state next-state n
                          &aux (insert-state (gen-insert-state primary-state mode n)))))
  mode primary-state insert-state next-state)

(let ((istate-lst ()))
  (defun add-insert-state (start-state goal-state mode n)
    (dotimes (i n)
      ;; name of a state should start from 1.
      (pushnew (make-istate :mode mode :primary-state start-state
                            :insert-state (gen-insert-state start-state mode (1+ i))
                            :next-state (if (= i (1- n))
                                          goal-state
                                          (gen-insert-state start-state mode (+ 2 i))))
               istate-lst
               :test #'equalp))
    (nth (1- n) istate-lst))
  (defun get-istate-lst () (copy-list istate-lst))
  (defun clr-istate-lst () (setf istate-lst nil)))

(defun next-state (start-state goal-state whole-evalarg-lst)
  (aif (and (not (eq goal-state '|raw|))
            (some #'input-invoke-p whole-evalarg-lst))
    (istate-insert-state (add-insert-state start-state goal-state (car it) (cdr it)))
    goal-state))

(defun process-comitemseq-lst (comitemseq-lst)
  (if (or (null comitemseq-lst)
          (and (null (cdr comitemseq-lst))
               (zerop (length (command (car comitemseq-lst))))))
    ""
    (apply #'spacing-join (mapcar #'make-evalarg comitemseq-lst))))

(defun keybind-desc (key start-state goal-state prior-commands post-commands)
  (string-trim " "
               (spacing-join (state-leave-desc start-state)
                             (resolve-string key)
                             "eval"
                             (process-comitemseq-lst prior-commands)
                             (state-arrive-desc
                               (next-state start-state goal-state
                                           (append prior-commands post-commands)))
                             (process-comitemseq-lst post-commands))))

(defun take-lispcode (x)
  (cadr x))

(defun comitem-stringify-form (comitem)
  (cond ((atom comitem) (resolve-string comitem))
        ((and (consp comitem) (eq (car comitem) :lisp))
         (take-lispcode comitem))
        (t comitem)))

(defun make-comitemseq-form (com)
  (if (eq (car com) :lisp)
    (take-lispcode com)
    `(make-comitemseq ,@(mapcar #'comitem-stringify-form com))))

(defun comseq-construct-form (comseq)
  (if (eq (car comseq) :lisp)
    (take-lispcode comseq)
    `(list ,@(mapcar #'make-comitemseq-form comseq))))

(defmacro keybind (strm key start goal (&rest prior-coms) (&rest post-coms))
  " key : a string or a character or an integer
    start,goal : evaluated.
  "
  `(format ,strm "~a~%"
           (keybind-desc ,key ,start ,goal
                         ,(comseq-construct-form prior-coms)
                         ,(comseq-construct-form post-coms))))

(define-symbol-macro escape (format t "~&escape ~a~%" (resolve-string (read))))

(defparameter *hardstatus-string* "")

(defun message (msg &optional (state-name nil state-name-p)
                              (hardstatus-format *hardstatus-string*))
  (make-dqstring
    (concat-str
      (if state-name-p
        (concat-str "%{= }%?%E[" state-name "]" msg "%:[raw]%?")
        msg)
      hardstatus-format)))

(defun default-message (dst primary-command-desc)
  (message (if (not (zerop (length primary-command-desc)))
             (concat-str " (" (escape #\^ primary-command-desc #\^) ")")
             "")
           (resolve-string dst)))

(defun set-hardstatus-string (str)
  (progn (setf *hardstatus-string* str)
         (format t "~&hardstatus string ~a~%"
                 (default-message '|screen| ""))))

(define-symbol-macro hardstatus-string
                     (set-hardstatus-string (read)))

(defmacro keybind-common (strm key start dst (&rest commands) &optional message)
  " message : any form that is evaluated into a string.
              A string literal is welcome, of course.
   "
  (once-only (dst)
    `(keybind ,strm ,key ,start ,dst
              ,commands
              (,(cond ((null message)
                       `(hardstatus string
                                    (:lisp (default-message
                                             ,dst
                                             (spacing-join
                                               ,@(mapcar #'comitem-stringify-form
                                                         (car commands)))))))
                      ((zerop (length message)) '(:lisp (make-comitemseq ""))) ; may be obsolete.
                      (t `(hardstatus string ,message)))))))

(defun normalize-cmd-format (cmd)
  (cond ((null cmd) '())
        ((symbolp cmd) `((,cmd)))
        ((symbolp (car cmd)) `(,cmd))
        (t cmd)))

(defmacro multiple-keybinds (strm start dst &body key-command-msg-lst)
  (once-only (strm start dst)
    (let ((forms
            (mapcar (lambda (key-com-msg)
                      (destructuring-bind (key cmd &optional msg)
                        key-com-msg
                        `(keybind-common ,strm ,key ,start ,dst
                                         ,(normalize-cmd-format cmd)
                                         ,msg)))
                    key-command-msg-lst)))
      `(progn
         ,@forms))))
