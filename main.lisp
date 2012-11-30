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

(defun seval-arg (str)
  " An argument for eval command of screen must be wrapped
    with quote."
  (concat-str "'" str "'"))

(defun sstringarg (str)
  " A string arguments for screen command must be double quoted."
  (concat-str "\"" str "\""))

(defun state-leave-desc (state)
  (case state
    (raw "bindkey")
    (screen "bind")
    (t (concat-str "bind -c " (resolve-string state)))))

(defun state-arrive-desc (state)
  (case state
    (raw "")
    (screen (seval-arg "command"))
    (t (seval-arg (concat-str "command -c " (resolve-string state))))))

(defun spacing-join (&rest strs)
  (format nil "~{~a~^ ~}" strs))

;;; desc : string
;;; state : string-designator
;;; key : string-designator or an integer number

;(defun keybind-desc (key start-state goal-state prior-comdescs post-comdescs)
;  (string-trim " "
;               (spacing-join (state-leave-desc start-state)
;                             (resolve-string key)
;                             "eval"
;                             (apply #'spacing-join (mapcar #'seval-arg prior-comdescs))
;                             (state-arrive-desc goal-state)
;                             (apply #'spacing-join (mapcar #'seval-arg post-comdescs)))))

(defun keybind-desc (key start-state goal-state prior-comdescs post-comdescs)
  (string-trim " "
               (spacing-join (state-leave-desc start-state)
                             (resolve-string key)
                             "eval"
                             (if (or (null prior-comdescs)
                                     (and (null (cdr prior-comdescs))
                                          (zerop (length (car prior-comdescs)))))
                               ""
                               (apply #'spacing-join (mapcar #'seval-arg prior-comdescs)))
                             (state-arrive-desc goal-state)
                             (if (or (null post-comdescs)
                                     (and (null (cdr post-comdescs))
                                          (zerop (length (car post-comdescs)))))
                               ""
                               (apply #'spacing-join (mapcar #'seval-arg post-comdescs))))))

(keybind-desc "^v" 'screen "X"
              (list "split -v" "focus right" "other" "focus left")
              (list "echo \"enter [X]\""))

(keybind-desc "v" "X" "X"
              (list "split -v" "focus right" "other" "focus left")
              (list "echo \"enter [X]\""))

(keybind-desc '^s "X" "X"
              (list "split" "focus down" "other" "focus up")
              (list "echo \"enter [X]\""))

(keybind-desc 'v "X" "X"
              (list "split -v" "focus right" "other" "focus left")
              ())

(keybind-desc 'v "X" "X"
              (list "split -v" "focus right" "other" "focus left")
              (list ""))

;;; Nov. 27th 2012, chiku
;;; Dangerous function. When you useit, please consider carefully if
;;; this judgement is really possible on macro expansion time.
;;; Generally speaking, macros that works as outermost interfaces can
;;; use to implemente with this function.
(defun quote-if-symbol (x)
  (if (symbolp x) `',x x))

(defun take-lispcode (x)
  (cadr x))

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

(defun com-stringify-form (com)
  (if (eq (car com) :lisp)
    (take-lispcode com)
    `(spacing-join ,@(mapcar #'comitem-stringify-form com))))

(com-stringify-form '(focus left))
(com-stringify-form '(focus (coerce (list #\l #\e #\f #\t) 'string)))
(com-stringify-form '(:lisp (concatenate 'string "focus " (concatenate 'string "ri" "ght"))))

(defun comseq-construct-form (comseq)
  (if (eq (car comseq) :lisp)
    (take-lispcode comseq)
    `(list ,@(mapcar #'com-stringify-form comseq))))

(comseq-construct-form '((split -v) (focus right) (other) (focus left)))
(comseq-construct-form '((split) (:lisp (string #\c))))
(comseq-construct-form '(:lisp (list "split -v" (concat-str "fo" "cus" " right"
                                                            (spacing-join " other" "focus" "left")))))

(defmacro keybind (strm key start goal (&rest prior-coms) (&rest post-coms))
  `(format ,strm "~a~%"
           (keybind-desc ,(quote-if-symbol key)
                         ,(quote-if-symbol start)
                         ,(quote-if-symbol goal)
                         ,(comseq-construct-form prior-coms)
                         ,(comseq-construct-form post-coms))))

;;; COMSEQ-CONSTRUCT-FORM, COM-STRINGIFY-FORM and COMITEM-STRINGIFY-FORM return
;;; a form that returns one string if evaluated. A function that returns a form
;;; is of course quite natural when we are writing macros. What I want to
;;; emphasize here is, do not use it in a function definition. It works in
;;; macro for constructing a code.

(keybind t ^c raw screen ((focus up)) ())
(keybind t ^p window-select window-select
         ((prev))
         ((echo (sstringarg "[window-select] (prev)"))))

(let ((x "focus down")
      (y "-v"))
  (keybind nil j window-select window-select
           ((:lisp x) (focus up) (split (:lisp y)))
           ()))

(defparameter *hardstatus-string* "")

(defun set-hardstatus-string (str)
  (progn (setf *hardstatus-string* str)
         (format t "~&hardstatus string ~a~%" str)))

(define-symbol-macro hardstatus-string
                     (set-hardstatus-string (read)))

(define-symbol-macro escape (format t "~&escape ~a~%" (resolve-string (read))))

(defun default-message (dst comdesc)
  (concat-str "%{= }%?%E[" (resolve-string dst) "]"
              (if (not (zerop (length comdesc)))
                (concat-str " (" comdesc ")")
                "")
              "%:[raw]%?" *hardstatus-string*))

(defmacro keybind-common (strm key start dst (&rest commands) &optional message)
  " message : any form that is evaluated into a string.
   A string literal is welcome, of course.
   "
  `(keybind ,strm ,key ,start ,dst
            ,commands
            (,(cond ((null message)
                     `(hardstatus string
                                  (:lisp (sstringarg (default-message
                                                       ',dst
                                                       ,(com-stringify-form (car commands)))))))
                    ((zerop (length message)) ())
                    (t `(hardstatus string (:lisp (sstringarg ,message))))))))

(keybind-common t j snormal snormal ((focus down)))
(keybind-common t k screen raw ((focus up)) "focus up")
(keybind-common t v screen raw ((split -v) (focus right) (other) (focus left)) "vertical split")
(keybind-common t v snormal snormal ((split -v) (focus right) (other) (focus left)))
(keybind-common t s snormal snormal ((split) (focus right) (other) (focus left)))
; a form generates the message
(keybind-common nil l snormal snormal ((focus right)) (coerce (coerce "focus right" 'list) 'string))
(keybind-common nil l snormal snormal ((focus right)) "")
(keybind-common nil ^z raw screen ())

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
  (k (focus up))
  (j (focus down))
  (^i (focus next))
  (s ((split) (focus down) (other) (focus up)))
  (v ((split -v) (focus right) (other) (focus left)) "vertical split"))

(multiple-keybinds t screen raw
  (^n next)
  (^p prev)
  (0 (select 0))
  (- (select -)))
