;;; -*- Mode: Lisp; Package: ESA -*-

;;;  (c) copyright 2005 by
;;;           Robert Strandh (strandh@labri.fr)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

;;; Emacs-Style Appication

(in-package :esa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Info pane, a pane that displays some information about another pane

(defclass info-pane (application-pane)
  ((master-pane :initarg :master-pane :reader master-pane))
  (:default-initargs
      :background +gray85+
      :scroll-bars nil
      :borders nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Minibuffer pane

(defvar *minimum-message-time* 1
  "The minimum number of seconds a minibuffer message will be
  displayed." )

(defclass minibuffer-pane (application-pane)
  ((message :initform nil
            :accessor message
            :documentation "An output record containing whatever
            message is supposed to be displayed in the
            minibuffer.")
   (message-time :initform 0
                 :accessor message-time
                 :documentation "The universal time at which the
                 current message was set."))
  (:default-initargs
   :scroll-bars nil
    :display-function 'display-minibuffer))

(defmethod stream-accept :before ((pane minibuffer-pane) type &rest args)
  (declare (ignore type args))
  (window-clear pane))

(defmethod stream-accept :around ((pane minibuffer-pane) type &rest args)
  (declare (ignore args))
  ;; FIXME: this isn't the friendliest way of indicating a parse
  ;; error: there's no feedback, unlike emacs' quite nice "[no
  ;; match]".
  (loop
   (handler-case 
       (return (call-next-method))
     (parse-error ()
       nil))))

(defmethod stream-accept ((pane minibuffer-pane) type &rest args
                          &key (view (stream-default-view pane))
                          &allow-other-keys)
  ;; default CLIM prompting is OK for now...
  (apply #'prompt-for-accept pane type view args)
  ;; but we need to turn some of ACCEPT-1 off.
  (apply #'accept-1-for-minibuffer pane type args))

;;; simpler version of McCLIM's internal operators of the same names:
;;; HANDLE-EMPTY-INPUT to make default processing work, EMPTY-INPUT-P
;;; and INVOKE-HANDLE-EMPTY-INPUT to support it.  We don't support
;;; recursive bouncing to see who most wants to handle the empty
;;; input, but that's OK, because we are always conceptually one-level
;;; deep in accept (even if sometimes we call ACCEPT recursively for
;;; e.g. command-names and arguments).
(defmacro handle-empty-input ((stream) input-form &body handler-forms)
  "see climi::handle-empty-input"
  (let ((input-cont (gensym "INPUT-CONT"))
        (handler-cont (gensym "HANDLER-CONT")))
    `(flet ((,input-cont ()
	      ,input-form)
	    (,handler-cont ()
	      ,@handler-forms))
       (declare (dynamic-extent #',input-cont #',handler-cont))
       (invoke-handle-empty-input ,stream #',input-cont #',handler-cont))))

;;; The code that signalled the error might have consumed the gesture, or
;;; not.
;;; XXX Actually, it would be a violation of the `accept' protocol to consume
;;; the gesture, but who knows what random accept methods are doing.
(defun empty-input-p
    (stream begin-scan-pointer activation-gestures delimiter-gestures)
  (let ((scan-pointer (stream-scan-pointer stream))
	(fill-pointer (fill-pointer (stream-input-buffer stream))))
    ;; activated?
    (cond ((and (eql begin-scan-pointer scan-pointer)
		(eql scan-pointer fill-pointer))
	   t)
	  ((or (eql begin-scan-pointer scan-pointer)
	       (eql begin-scan-pointer (1- scan-pointer)))
	   (let ((gesture 
                  (aref (stream-input-buffer stream) begin-scan-pointer)))
	     (and (characterp gesture)
                  (flet ((gesture-matches-p (g)
                           (if (characterp g)
                               (char= gesture g)
                               ;; FIXME: not quite portable --
                               ;; apparently
                               ;; EVENT-MATCHES-GESTURE-NAME-P need
                               ;; not work on raw characters
                               (event-matches-gesture-name-p gesture g))))
                    (or (some #'gesture-matches-p activation-gestures)
                        (some #'gesture-matches-p delimiter-gestures))))))
	  (t nil))))

(defun invoke-handle-empty-input
    (stream input-continuation handler-continuation)
  (unless (input-editing-stream-p stream)
    (return-from invoke-handle-empty-input (funcall input-continuation)))
  (let ((begin-scan-pointer (stream-scan-pointer stream))
	(activation-gestures *activation-gestures*)
	(delimiter-gestures *delimiter-gestures*))
    (block empty-input
      (handler-bind 
          ((parse-error
            #'(lambda (c)
                (when (empty-input-p stream begin-scan-pointer 
                                     activation-gestures delimiter-gestures)
                  (return-from empty-input nil)))))
	(return-from invoke-handle-empty-input (funcall input-continuation))))
    (funcall handler-continuation)))

(defun accept-1-for-minibuffer
    (stream type &key
     (view (stream-default-view stream))
     (default nil defaultp) (default-type nil default-type-p)
     provide-default insert-default (replace-input t)
     history active-p prompt prompt-mode display-default
     query-identifier (activation-gestures nil activationsp)
     (additional-activation-gestures nil additional-activations-p)
     (delimiter-gestures nil delimitersp)
     (additional-delimiter-gestures nil  additional-delimiters-p))
  (declare (ignore provide-default history active-p
		   prompt prompt-mode
		   display-default query-identifier))
  (when (and defaultp (not default-type-p))
    (error ":default specified without :default-type"))
  (when (and activationsp additional-activations-p)
    (error "only one of :activation-gestures or ~
            :additional-activation-gestures may be passed to accept."))
  (unless (or activationsp additional-activations-p *activation-gestures*)
    (setq activation-gestures *standard-activation-gestures*))
  (with-input-editing 
      ;; this is the main change from CLIM:ACCEPT-1 -- no sensitizer.
      (stream :input-sensitizer nil)
    ;; KLUDGE: no call to CLIMI::WITH-INPUT-POSITION here, but that's
    ;; OK because we are always going to create a new editing stream
    ;; for each call to accept/accept-1-for-minibuffer, so the default
    ;; default for the BUFFER-START argument to REPLACE-INPUT is
    ;; right.
    (when insert-default
      ;; Insert the default value to the input stream. It should
      ;; become fully keyboard-editable.
      (presentation-replace-input 
       stream default default-type view))
    (with-input-context (type)
        (object object-type event options)
        (with-activation-gestures ((if additional-activations-p
                                       additional-activation-gestures
                                       activation-gestures)
                                   :override activationsp)
          (with-delimiter-gestures ((if additional-delimiters-p
                                        additional-delimiter-gestures
                                        delimiter-gestures)
                                    :override delimitersp)
            (let ((accept-results nil))
              (climi::handle-empty-input (stream)
                  (setq accept-results
                        (multiple-value-list
                         (if defaultp
                             (funcall-presentation-generic-function
                              accept type stream view
                              :default default :default-type default-type)
                             (funcall-presentation-generic-function
                              accept type stream view))))
                ;; User entered activation or delimiter gesture
                ;; without any input.
                (if defaultp
                    (presentation-replace-input
                     stream default default-type view :rescan nil)
                    (simple-parse-error
                     "Empty input for type ~S with no supplied default"
                     type))
                (setq accept-results (list default default-type)))
              ;; Eat trailing activation gesture
              ;; XXX what about pointer gestures?
              ;; XXX and delimiter gestures?
              ;;
              ;; deleted check for *RECURSIVE-ACCEPT-P*
              (let ((ag (read-char-no-hang stream nil stream t)))
                (unless (or (null ag) (eq ag stream))
                  (unless (activation-gesture-p ag)
                    (unread-char ag stream))))
              (values (car accept-results) 
                      (if (cdr accept-results) (cadr accept-results) type)))))
      ;; A presentation was clicked on, or something.
      (t
       (when (and replace-input 
                  (getf options :echo t)
                  (not (stream-rescanning-p stream)))
         (presentation-replace-input 
          stream object object-type view :rescan nil))
       (values object object-type)))))

(defun display-minibuffer (frame pane)
  (declare (ignore frame))
  (when (message pane)
    (if (> (get-universal-time)
           (+ *minimum-message-time* (message-time pane)))
        (setf (message pane) nil)
        (replay-output-record (message pane) pane))))

(defmacro with-minibuffer-stream ((stream-symbol)
                                  &body body)
  "Bind `stream-symbol' to the minibuffer stream and evaluate
  `body'. This macro makes sure to setup the initial blanking of
  the minibuffer as well as taking care of for how long the
  message should be displayed."
  `(let ((,stream-symbol *standard-input*))
     (setf (message ,stream-symbol)
           (with-output-to-output-record (,stream-symbol)
             (window-clear ,stream-symbol)
             (setf (message-time ,stream-symbol) (get-universal-time))
             ,@body))))

(defun display-message (format-string &rest format-args)
  "Display a message in the minibuffer. Composes the string based
on the `format-string' and the `format-args'."
  (with-minibuffer-stream (minibuffer)
    (apply #'format minibuffer format-string format-args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; ESA pane mixin

(defclass esa-pane-mixin ()
  (;; allows a certain number of commands to have some minimal memory
   (previous-command :initform nil :accessor previous-command)
   (command-table :initarg :command-table :accessor command-table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; ESA frame mixin

(defclass esa-frame-mixin ()
  ((windows :accessor windows)
   (recordingp :initform nil :accessor recordingp)
   (executingp :initform nil :accessor executingp)
   (recorded-keys :initform '() :accessor recorded-keys)
   (remaining-keys :initform '() :accessor remaining-keys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Command processing

(defun find-gestures (gestures start-table)
  (loop with table = (find-command-table start-table)
	for (gesture . rest) on gestures
	for item = (find-keystroke-item  gesture table :errorp nil)
	while item
	do (if (eq (command-menu-item-type item) :command)
	       (return (if (null rest) item nil))
	       (setf table (command-menu-item-value item)))
	finally (return item)))

(defun find-gestures-with-inheritance (gestures start-table)
  (or (find-gestures gestures start-table)
      (some (lambda (table)
	      (find-gestures-with-inheritance gestures table))
	    (command-table-inherit-from
	     (find-command-table start-table)))))

;;; In Classic CLIM event-matches-gesture-name-p doesn't accept characters.
#+mcclim
(defun gesture-matches-gesture-name-p (gesture gesture-name)
  (event-matches-gesture-name-p gesture gesture-name))

#-mcclim
(defun gesture-matches-gesture-name-p (gesture gesture-name)
  (etypecase gesture
    (event
     (event-matches-gesture-name-p gesture gesture-name))
    (character
     (clim-internals::keyboard-event-matches-gesture-name-p gesture
							    gesture-name))))
(defparameter *current-gesture* nil)

(defparameter *meta-digit-table*
  (loop for i from 0 to 9
       collect (list :keyboard (digit-char i) (make-modifier-state :meta))))

(defun meta-digit (gesture)
  (position gesture *meta-digit-table*
	    :test #'gesture-matches-gesture-name-p))

(defun esa-read-gesture ()
  (unless (null (remaining-keys *application-frame*))
    (return-from esa-read-gesture
      (pop (remaining-keys *application-frame*))))
  (loop for gesture = (read-gesture :stream *standard-input*)
	until (or (characterp gesture)
		  (and (typep gesture 'keyboard-event)
		       (or (keyboard-event-character gesture)
			   (not (member (keyboard-event-key-name
					 gesture)
					'(:control-left :control-right
					  :shift-left :shift-right
					  :meta-left :meta-right
					  :super-left :super-right
					  :hyper-left :hyper-right
					  :shift-lock :caps-lock
					  :alt-left :alt-right))))))
	finally (progn (when (recordingp *application-frame*)
			 (push gesture (recorded-keys *application-frame*)))
		       (return gesture))))

(defun esa-unread-gesture (gesture stream)
  (cond ((recordingp *application-frame*)
	 (pop (recorded-keys *application-frame*))
	 (unread-gesture gesture :stream stream))
	((executingp *application-frame*)
	 (push gesture (remaining-keys *application-frame*)))
	(t 
	 (unread-gesture gesture :stream stream))))

(define-gesture-name universal-argument :keyboard (#\u :control))

(define-gesture-name meta-minus :keyboard (#\- :meta))

(defun read-numeric-argument (&key (stream *standard-input*))
  "Reads gestures returning two values: prefix-arg and whether prefix given.
Accepts: EITHER C-u, optionally followed by other C-u's, optionally followed
by a minus sign, optionally followed by decimal digits;
OR An optional M-minus, optionally followed by M-decimal-digits.
You cannot mix C-u and M-digits.
C-u gives a numarg of 4. Additional C-u's multiply by 4 (e.g. C-u C-u C-u = 64).
After C-u you can enter decimal digits, possibly preceded by a minus (but not
a plus) sign. C-u 3 4 = 34, C-u - 3 4 = -34. Note that C-u 3 - prints 3 '-'s.
M-1 M-2 = 12. M-- M-1 M-2 = -12. As a special case, C-u - and M-- = -1.
In the absence of a prefix arg returns 1 (and nil)."
  (let ((gesture (esa-read-gesture)))
    (cond ((gesture-matches-gesture-name-p
	    gesture 'universal-argument)
	   (let ((numarg 4))
	     (loop for gesture = (esa-read-gesture)
		   while (gesture-matches-gesture-name-p
			  gesture 'universal-argument)
		   do (setf numarg (* 4 numarg))
		   finally (esa-unread-gesture gesture stream))
	     (let ((gesture (esa-read-gesture))
		   (sign +1))
	       (when (and (characterp gesture)
			  (char= gesture #\-))
		 (setf gesture (esa-read-gesture)
		       sign -1))
	       (cond ((and (characterp gesture)
			   (digit-char-p gesture 10))
		      (setf numarg (digit-char-p gesture 10))
		      (loop for gesture = (esa-read-gesture)
			    while (and (characterp gesture)
				       (digit-char-p gesture 10))
			    do (setf numarg (+ (* 10 numarg)
					       (digit-char-p gesture 10)))
			    finally (esa-unread-gesture gesture stream)
				    (return (values (* numarg sign) t))))
		     (t
		      (esa-unread-gesture gesture stream)
		      (values (if (minusp sign) -1 numarg) t))))))
	  ((or (meta-digit gesture)
	       (gesture-matches-gesture-name-p
		gesture 'meta-minus))
	   (let ((numarg 0)
		 (sign +1))
	     (cond ((meta-digit gesture)
		    (setf numarg (meta-digit gesture)))
		   (t (setf sign -1)))
	     (loop for gesture = (esa-read-gesture)
		   while (meta-digit gesture)
		   do (setf numarg (+ (* 10 numarg) (meta-digit gesture)))
		   finally (esa-unread-gesture gesture stream)
			   (return (values (if (and (= sign -1) (= numarg 0))
					       -1
					       (* sign numarg))
					   t)))))
	  (t (esa-unread-gesture gesture stream)
	     (values 1 nil)))))

(defvar *numeric-argument-p* (list nil))

(defun substitute-numeric-argument-p (command numargp)
  (substitute numargp *numeric-argument-p* command :test #'eq))

(defun process-gestures-or-command (frame command-table)
  (with-input-context 
      ('menu-item)
      (object)
    (with-input-context 
        (`(command :command-table ,command-table))
        (object)
      (let ((gestures '()))
        (multiple-value-bind (numarg numargp)
            (read-numeric-argument :stream *standard-input*)
          (loop 
           (setf *current-gesture* (esa-read-gesture))
           (setf gestures 
                 (nconc gestures (list *current-gesture*)))
           (let ((item (find-gestures-with-inheritance gestures command-table)))
             (cond 
               ((not item)
                (beep) (return))
               ((eq (command-menu-item-type item) :command)
                (let ((command (command-menu-item-value item)))
                  (unless (consp command)
                    (setf command (list command)))
                  (setf command (substitute-numeric-argument-marker command numarg))
                  (setf command (substitute-numeric-argument-p command numargp))
                  (when (member *unsupplied-argument-marker* command :test #'eq)
                    (setq command
                          (funcall
                           *partial-command-parser*
                           (frame-command-table frame) 
                           (frame-standard-input frame) command 0)))
                  (execute-frame-command frame command)
                  (return)))
               (t nil))))))
      (command
       (execute-frame-command frame object)))
    (menu-item
     (let ((command (command-menu-item-value object)))
       (unless (listp command)
         (setq command (list command)))       
       (when (member *unsupplied-argument-marker* command :test #'eq)
         (setq command
               (funcall 
                *partial-command-parser*
                (frame-command-table frame) (frame-standard-input frame) 
                command 0)))
       (execute-frame-command frame command)))))

(defmethod redisplay-frame-panes :around ((frame esa-frame-mixin) &key force-p)
  (declare (ignore force-p))
  (when (null (remaining-keys frame))
    (setf (executingp frame) nil)
    (call-next-method)))

(defmethod execute-frame-command :after ((frame esa-frame-mixin) command)
  ;; FIXME: I'm not sure that we want to do this for commands sent
  ;; from other threads; we almost certainly don't want to do it twice
  ;; in such cases...
  ;;
  ;; FIXME: also, um, throwing away the arguments is likely to be bad.
  (setf (previous-command (car (windows frame)))
	(if (consp command)
	    (car command)
	    command)))

(defmethod execute-frame-command :around ((frame esa-frame-mixin) command)
  (call-next-method)
  (when (eq frame *application-frame*)
    (redisplay-frame-panes frame)))

(defgeneric find-applicable-command-table (frame))

(defmethod find-applicable-command-table ((frame esa-frame-mixin))
  (command-table (car (windows frame))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Top level

(defvar *extended-command-prompt*)

(defun esa-top-level (frame &key
                      (command-parser 'esa-command-parser)
                      ;; FIXME: maybe customize this?  Under what
                      ;; circumstances would it be used?  Maybe try
                      ;; turning the clim listener into an ESA?
                      (command-unparser 'command-line-command-unparser)
                      (partial-command-parser 'esa-partial-command-parser)
                      (prompt "Extended Command: "))
  (declare (ignore prompt))
  (with-slots (windows) frame
    (let ((*standard-output* (car windows))
	  (*standard-input* (frame-standard-input frame))
	  (*print-pretty* nil)
	  (*abort-gestures* `((:keyboard #\g ,(make-modifier-state :control))))
          (*command-parser* command-parser)
          (*command-unparser* command-unparser)
          (*partial-command-parser* partial-command-parser)
          (*extended-command-prompt* prompt)
          (*pointer-documentation-output*
           (frame-pointer-documentation-output frame)))
      (unless (eq (frame-state frame) :enabled)
	(enable-frame frame))
      (redisplay-frame-panes frame :force-p t)
      (loop
       do (restart-case
              (handler-case
                  (let ((command-table (find-applicable-command-table frame)))
                    ;; for presentation-to-command-translators,
                    ;; which are searched for in
                    ;; (frame-command-table *application-frame*)
                    (redisplay-frame-pane frame (frame-standard-input frame) :force-p t)
                    (setf (frame-command-table frame) command-table)
                    (process-gestures-or-command frame command-table))
                (abort-gesture () 
                  (display-message "Quit")
                  (redisplay-frame-panes frame)))
	   (return-to-esa () nil)
           (reset-esa ()
             ;; This restart is used to jump out of deadlocks where
             ;; ESA will otherwise run an error-inducing gesture again
             ;; and again.
             (setf (remaining-keys *application-frame*) nil)
             nil))))))

(defmacro simple-command-loop (command-table loop-condition end-clauses)
  (let ((gesture (gensym))
        (item (gensym))
        (command (gensym)))
    `(handler-case
	 (progn 
	   (redisplay-frame-panes *application-frame*)
	   (loop while ,loop-condition
		 as ,gesture = (esa-read-gesture)
		 as ,item = (find-gestures-with-inheritance (list ,gesture) ,command-table)
		 do (cond ((and ,item (eq (command-menu-item-type ,item) :command))
			   (setf *current-gesture* ,gesture)
			   (let ((,command (command-menu-item-value ,item)))
			     (unless (consp ,command)
			       (setf ,command (list ,command)))
			     (execute-frame-command *application-frame*
						    ,command)))
			  (t
			   (unread-gesture ,gesture)
			   ,@end-clauses))
		    (redisplay-frame-panes *application-frame*)))
       (abort-gesture ()
	 ,@end-clauses))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; command table manipulation

;;; Helper to avoid calling find-keystroke-item at load time. In Classic CLIM
;;; that function doesn't work if not connected to a port.

(defun compare-gestures (g1 g2)
  (and (eql (car g1) (car g2))
       (eql (apply #'make-modifier-state (cdr g1))
	    (apply #'make-modifier-state (cdr g2)))))

(defun find-gesture-item (table gesture)
  (map-over-command-table-keystrokes
     (lambda (name gest item)
       (declare (ignore name))
       (when (compare-gestures gesture gest)
	 (return-from find-gesture-item item)))
     table)
  nil)

#-mcclim
(defun ensure-subtable (table gesture)
  (let ((item (find-gesture-item table gesture)))
    (when (or (null item) (not (eq (command-menu-item-type item) :menu)))
      (let ((name (gensym)))
	(make-command-table name :errorp nil)
	(add-menu-item-to-command-table table (symbol-name name)
					:menu name
					:keystroke gesture)))
    (command-menu-item-value (find-gesture-item table gesture))))

#+mcclim
(defun ensure-subtable (table gesture)
  (let* ((event (make-instance
		'key-press-event
		:key-name nil
		:key-character (car gesture)
		:modifier-state (apply #'make-modifier-state (cdr gesture))))
	 (item (find-keystroke-item event table :errorp nil)))
    (when (or (null item) (not (eq (command-menu-item-type item) :menu)))
      (let ((name (gensym)))
	(make-command-table name :errorp nil)
	(add-menu-item-to-command-table table (symbol-name name)
					:menu name
					:keystroke gesture)))
    (command-menu-item-value
     (find-keystroke-item event table :errorp nil))))

(defun set-key (command table gestures)
  ;; WTF?
  #-(and)
  (unless (consp command)
    (setf command (list command)))
  (let ((gesture (car gestures)))
    (cond ((null (cdr gestures))
	   (add-keystroke-to-command-table
	    table gesture :command command :errorp nil)
	   (when (and (listp gesture)
		      (find :meta gesture))
             ;; KLUDGE: this is a workaround for poor McCLIM
             ;; behaviour; really this canonization should happen in
             ;; McCLIM's input layer.
	     (set-key command table
		      (list (list :escape)
			    (let ((esc-list (remove :meta gesture)))
			      (if (and (= (length esc-list) 2)
				       (find :shift esc-list))
				  (remove :shift esc-list)
				  esc-list))))))
	  (t (set-key command
		      (ensure-subtable table gesture)
		      (cdr gestures))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; standard key bindings 

;;; global

(define-command-table global-esa-table)

(define-command (com-quit :name t :command-table global-esa-table) ()
  "Exit.
First ask if modified buffers should be saved. If you decide not to save a modified buffer, you will be asked to confirm your decision to exit."
  (frame-exit *application-frame*))

(set-key 'com-quit 'global-esa-table '((#\x :control) (#\c :control)))

(define-command (com-extended-command
		 :command-table global-esa-table)
    ()
  "Prompt for a command name and arguments, then run it."
  (let ((item (handler-case
                  (accept
                   `(command :command-table ,(find-applicable-command-table *application-frame*))
                   ;; this gets erased immediately anyway
                   :prompt "" :prompt-mode :raw)
                ((or command-not-accessible command-not-present) ()
                  (beep)
                 (display-message "No such command")
                 (return-from com-extended-command nil)))))
    (execute-frame-command *application-frame* item)))

(set-key 'com-extended-command 'global-esa-table '((#\x :meta)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Help

(defgeneric help-stream (frame title))

(defmethod help-stream (frame title)
  (open-window-stream
   :label title
   :input-buffer (#+mcclim climi::frame-event-queue
			   #-mcclim silica:frame-input-buffer
			   *application-frame*)
   :width 400))

(defun read-gestures-for-help (command-table)
  (loop for gestures = (list (esa-read-gesture))
	  then (nconc gestures (list (esa-read-gesture)))
	for item = (find-gestures-with-inheritance gestures command-table)
	unless item
	  do (return (values nil gestures))
	when (eq (command-menu-item-type item) :command)
	  do (return (values (command-menu-item-value item)
			     gestures))))

(defun describe-key-briefly (pane)
  (let ((command-table (command-table pane)))
    (multiple-value-bind (command gestures)
	(read-gestures-for-help command-table)
      (when (consp command)
	(setf command (car command)))
      (display-message "~{~A ~}~:[is not bound~;runs the command ~:*~A~]"
		       (mapcar #'gesture-name gestures)
		       (or (command-line-name-for-command
			    command command-table :errorp nil)
			   command)))))

(defgeneric gesture-name (gesture))

(defmethod gesture-name ((char character))
  (if (and (graphic-char-p char)
           (not (char= char #\Space)))
      (string char)
      (or (char-name char)
          char)))

(defun translate-name-and-modifiers (key-name modifiers)
  (with-output-to-string (s)
      (loop for (modifier name) on (list
					;(+alt-key+ "A-")
					+hyper-key+ "H-"
					+super-key+ "s-"
					+meta-key+ "M-"
					+control-key+ "C-")
	      by #'cddr
	    when (plusp (logand modifier modifiers))
	      do (princ name s))
      (princ (if (typep key-name 'character)
		 (gesture-name key-name)
		 key-name) s)))

(defmethod gesture-name ((ev keyboard-event))
  (let ((key-name (keyboard-event-key-name ev))
	(modifiers (event-modifier-state ev)))
    (translate-name-and-modifiers key-name modifiers)))

(defmethod gesture-name ((gesture list))
  (cond ((eq (car gesture) :keyboard)
	 (translate-name-and-modifiers (second gesture) (third gesture)))
	;; punt on this for now
	(t nil)))

(defun find-keystrokes-for-command (command command-table)
  (let ((keystrokes '()))
    (labels ((helper (command command-table prefix)
	       (map-over-command-table-keystrokes
		#'(lambda (menu-name keystroke item)
		    (declare (ignore menu-name))
		    (cond ((and (eq (command-menu-item-type item) :command)
                                (listp (command-menu-item-value item))
				(eq (car (command-menu-item-value item)) command))
			   (push (cons keystroke prefix) keystrokes))
			  ((eq (command-menu-item-type item) :menu)
			   (helper command (command-menu-item-value item) (cons keystroke prefix)))
			  (t nil)))
		command-table)))
      (helper command command-table nil)
      keystrokes)))

(defun find-keystrokes-for-command-with-inheritance (command start-table)
  (let ((keystrokes '()))
    (labels  ((helper (table)
		(let ((keys (find-keystrokes-for-command command table)))
		  (when keys (push keys keystrokes))
		  (dolist (subtable (command-table-inherit-from
				     (find-command-table table)))
		    (helper subtable)))))
      (helper start-table))
    keystrokes))

(defun find-all-keystrokes-and-commands (command-table)
  (let ((results '()))
    (labels ((helper (command-table prefix)
	       (map-over-command-table-keystrokes
		#'(lambda (menu-name keystroke item)
		    (declare (ignore menu-name))
		    (cond ((eq (command-menu-item-type item) :command) 
			   (push (cons (cons keystroke prefix)
				       (command-menu-item-value item))
				 results))
			  ((eq (command-menu-item-type item) :menu)
			   (helper (command-menu-item-value item) (cons keystroke prefix)))
			  (t nil)))
		command-table)))
      (helper command-table nil)
      results)))

(defun find-all-keystrokes-and-commands-with-inheritance (start-table)
  (let ((results '()))
    (labels  ((helper (table)
		(let ((res (find-all-keystrokes-and-commands table)))
		  (when res  (setf results (nconc res results)))
		  (dolist (subtable (command-table-inherit-from
				     (find-command-table table)))
		    (helper subtable)))))
      (helper start-table))
    results))

(defun find-all-commands-and-keystrokes-with-inheritance (start-table)
  (let ((results '()))
    (map-over-command-table-commands
     (lambda (command)
       (let ((keys (find-keystrokes-for-command-with-inheritance command start-table)))
	 (push (cons command keys) results)))
     start-table
     :inherited t)
    results))

(defun sort-by-name (list)
  (sort list #'string< :key (lambda (item) 
                              (symbol-name (if (listp (cdr item)) 
                                               (cadr item) 
                                               (cdr item))))))

(defun sort-by-keystrokes (list)
  (sort list (lambda (a b)
	       (cond ((and (characterp a)
			   (characterp b))
		      (char< a b))
		     ((characterp a)
		      t)
		     ((characterp b)
		      nil)
		     (t (string< (symbol-name a)
				 (symbol-name b)))))
	:key (lambda (item) (second (first (first item))))))

(defun describe-bindings (stream command-table
			  &optional (sort-function #'sort-by-name))
  (formatting-table (stream)
    (loop for (keys . command)
	  in (funcall sort-function
		      (find-all-keystrokes-and-commands-with-inheritance
			   command-table))
          when (consp command) do (setq command (car command))
	  do (formatting-row (stream) 
	       (formatting-cell (stream :align-x :right)
		 (with-text-style (stream '(:sans-serif nil nil))
		   (present command
                            `(command-name :command-table ,command-table)
                            :stream stream)))
	       (formatting-cell (stream)
		 (with-drawing-options (stream :ink +dark-blue+
					       :text-style '(:fix nil nil))
		   (format stream "~&~{~A~^ ~}"
			   (mapcar #'gesture-name (reverse keys))))))
	  count command into length
	  finally (change-space-requirements stream
			 :height (* length (stream-line-height stream)))
		  (scroll-extent stream 0 0))))

(defun print-docstring-for-command (command-name command-table &optional (stream *standard-output*))
  "Print documentation for `command-name', which should 
   be a symbol bound to a function, to `stream'. If no 
   documentation can be found, this fact will be printed to the stream."
  (declare (ignore command-table))
  ;; This needs more regex magic. Also, it is only an interim
  ;; solution.
  (with-text-style (stream '(:sans-serif nil nil))
    (let* ((command-documentation (or (documentation command-name 'function)
                                     "This command is not documented."))
	   (first-newline (position #\Newline command-documentation))
	   (first-line (subseq command-documentation 0 first-newline)))
      ;; First line is special
      (format stream "~A~%" first-line)
      (when first-newline
	(let* ((rest (subseq command-documentation first-newline))
	       (paras (delete ""
			      (loop for start = 0 then (+ 2 end)
				    for end = (search '(#\Newline #\Newline) rest :start2 start)
				    collecting
				    (nsubstitute #\Space #\Newline (subseq rest start end))
				    while end)
			      :test #'string=)))
	  (dolist (para paras)
	    (terpri stream)
	    (let ((words (loop with length = (length para)
			       with index = 0
			       with start = 0
			       while (< index length)
			       do (loop until (>= index length)
					while (member (char para index) '(#\Space #\Tab))
					do (incf index))
				  (setf start index)
				  (loop until (>= index length)
					until (member (char para index) '(#\Space #\Tab))
					do (incf index))
			       until (= start index)
			       collecting (string-trim '(#\Space #\Tab #\Newline)
							(subseq para start index)))))
	      (loop with margin = (stream-text-margin stream)
		    with space-width = (stream-character-width stream #\Space)
		    with current-width = 0
		    for word in words
		    for word-width = (stream-string-width stream word)
		    when (> (+ word-width current-width)
				   margin)
		      do (terpri stream)
			 (setf current-width 0)
		    do (princ word stream)
		       (princ #\Space stream)
		       (incf current-width (+ word-width space-width))))
	    (terpri stream)))))))

(defun describe-command-binding-to-stream (gesture command &key 
                                           (command-table (find-applicable-command-table *application-frame*))
                                           (stream *standard-output*))
  "Describe `command' as invoked by `gesture' to `stream'."
  (let* ((command-name (if (listp command)
                           (first command)
                           command))        
         (command-args (if (listp command)
                           (rest command)))
         (real-command-table (or (command-accessible-in-command-table-p 
                                  command-name
                                  command-table)
                                 command-table)))
    (with-text-style (stream '(:sans-serif nil nil))
      (princ "The gesture " stream)
      (with-drawing-options (stream :ink +dark-blue+
				    :text-style '(:fix nil nil))
        (princ gesture stream))
      (princ " is bound to the command " stream)
      (if (command-present-in-command-table-p command-name real-command-table)
          (with-text-style (stream '(nil :bold nil))
	    (present command-name `(command-name :command-table ,command-table) :stream stream))
          (present command-name 'symbol :stream stream))
      (princ " in " stream)
      (present real-command-table 'command-table :stream stream)
      (format stream ".~%")
      (when command-args
        (apply #'format stream
	       "This binding invokes the command with these arguments: ~@{~A~^, ~}.~%"
	       (mapcar #'(lambda (arg)
                           (cond ((eq arg *unsupplied-argument-marker*)
                                  "unsupplied-argument")
                                 ((or (eq arg *numeric-argument-marker*)
                                      (eq arg *numeric-argument-p*))
                                  "numeric-argument")
                                 (t arg))) command-args)))
      (terpri stream)
      (print-docstring-for-command command-name command-table stream)
      (scroll-extent stream 0 0))))

(defun describe-command-to-stream
    (command-name &key 
     (command-table (find-applicable-command-table *application-frame*))
     (stream *standard-output*))
  "Describe `command' to `stream'."
  (let ((keystrokes (find-keystrokes-for-command-with-inheritance command-name command-table)))
    (with-text-style (stream '(:sans-serif nil nil))
      (with-text-style (stream '(nil :bold nil))
	(present command-name `(command-name :command-table ,command-table) :stream stream))
      (princ " calls the function " stream)
      (present command-name 'symbol :stream stream)
      (princ " and is accessible in " stream)
      (if (command-accessible-in-command-table-p command-name command-table)
          (present (command-accessible-in-command-table-p command-name command-table)
                   'command-table
                   :stream stream)
          (princ "an unknown command table" stream))
      
      (format stream ".~%")
      (when (plusp (length keystrokes))
        (princ "It is bound to " stream)
        (loop for gestures-list on (first keystrokes)
           do (with-drawing-options (stream :ink +dark-blue+
					    :text-style '(:fix nil nil))
                (format stream "~{~A~^ ~}"
                        (mapcar #'gesture-name (reverse (first gestures-list)))))
           when (not (null (rest gestures-list)))
           do (princ ", " stream))
        (terpri stream))
      (terpri stream)
      (print-docstring-for-command command-name command-table stream)
      (scroll-extent stream 0 0))))

;;; help commands

(define-command-table help-table)

(define-command (com-describe-key-briefly :name t :command-table help-table) ()
  "Prompt for a key and show the command it invokes."  
  (display-message "Describe key briefly:")
  (redisplay-frame-panes *application-frame*)
  (describe-key-briefly (car (windows *application-frame*))))

(set-key 'com-describe-key-briefly 'help-table '((#\h :control) (#\c)))

(define-command (com-where-is :name t :command-table help-table) ()
  "Prompt for a command name and show the key that invokes it."
  (let* ((command-table (command-table (car (windows *application-frame*))))
	 (command
	  (handler-case
	      (accept
	       `(command-name :command-table
			      ,command-table)
	       :prompt "Where is command")
	    (error () (progn (beep)
			     (display-message "No such command")
			     (return-from com-where-is nil)))))
	 (keystrokes (find-keystrokes-for-command-with-inheritance command command-table)))
    (display-message "~A is ~:[not on any key~;~:*on ~{~A~^, ~}~]"
		     (command-line-name-for-command command command-table)
		     (mapcar (lambda (keys)
			       (format nil "~{~A~^ ~}"
				       (mapcar #'gesture-name (reverse keys))))
			     (car keystrokes)))))

(set-key 'com-where-is 'help-table '((#\h :control) (#\w)))

(define-command (com-describe-bindings :name t :command-table help-table)
    ((sort-by-keystrokes 'boolean :prompt "Sort by keystrokes?"))
  "Show which keys invoke which commands.
Without a numeric prefix, sorts the list by command name. With a numeric prefix, sorts by key."
  (let ((stream (help-stream *application-frame* (format nil "Help: Describe Bindings")))
	 (command-table (find-applicable-command-table *application-frame*)))
    (describe-bindings stream command-table
		       (if sort-by-keystrokes
			   #'sort-by-keystrokes
			   #'sort-by-name))))

(set-key `(com-describe-bindings ,*numeric-argument-p*) 'help-table '((#\h :control) (#\b)))

(define-command (com-describe-key :name t :command-table help-table)
    ()
  "Display documentation for the command invoked by a given gesture sequence. 
When invoked, this command will wait for user input. If the user inputs a gesture 
sequence bound to a command available in the syntax of the current buffer,
documentation and other details will be displayed in a typeout pane."
  (let ((command-table (find-applicable-command-table *application-frame*)))
    (display-message "Describe Key:")
    (redisplay-frame-panes *application-frame*)
    (multiple-value-bind (command gestures)
        (read-gestures-for-help command-table)
      (let ((gesture-name (format nil "~{~A~#[~; ~; ~]~}"
                                  (mapcar #'gesture-name gestures))))
        (if command
            (let ((out-stream
		   (help-stream *application-frame*
				(format nil "~10THelp: Describe Key for ~A" gesture-name))))
              (describe-command-binding-to-stream gesture-name command
                                                  :command-table command-table
                                                  :stream out-stream))
            (display-message "Unbound gesture: ~A" gesture-name))))))

(set-key 'com-describe-key
         'help-table
         '((#\h :control) (#\k)))

(define-command (com-describe-command :name t :command-table help-table)
    ((command 'command-name :prompt "Describe command"))
  "Display documentation for the given command."
  (let* ((command-table (find-applicable-command-table *application-frame*))
	 (out-stream (help-stream *application-frame*
				  (format nil "~10THelp: Describe Command for ~A"
					  (command-line-name-for-command command
									 command-table
									 :errorp nil)))))
    (describe-command-to-stream command
                                :command-table command-table
                                :stream out-stream)))

(set-key `(com-describe-command ,*unsupplied-argument-marker*)
         'help-table
         '((#\h :control) (#\f)))

(define-presentation-to-command-translator describe-command
    (command-name com-describe-command help-table
                  :gesture :select
                  :documentation "Describe command")
    (object)
    (list object))

(define-command (com-apropos-command :name t :command-table help-table)
    ((words '(sequence string) :prompt "Search word(s)"))
  "Shows commands with documentation matching the search words.
Words are comma delimited. When more than two words are given, the documentation must match any two."
  ;; 23.8.6 "It is unspecified whether accept returns a list or a vector."
  (setf words (coerce words 'list))
  (when words
    (let* ((command-table (find-applicable-command-table *application-frame*))
	   (results (loop for (function . keys)
			  in (find-all-commands-and-keystrokes-with-inheritance
				  command-table)
			  when (consp function)
			    do (setq function (car function))
			  when (let ((documentation (or (documentation function 'function) ""))
				     (score 0))
				 (cond
				   ((> (length words) 1)
				    (loop for word in words
					  until (> score 1)
					  when (or
						 (search word (symbol-name function)
						       :test #'char-equal)
						 (search word documentation :test #'char-equal))
					    do (incf score)
					  finally (return (> score 1))))
				   (t (or
				       (search (first words) (symbol-name function)
					       :test #'char-equal)
				       (search (first words) documentation :test #'char-equal)))))
			    collect (cons function keys))))
      (if (null results)
	  (display-message "No results for ~{~A~^, ~}" words)
	  (let ((out-stream (help-stream *application-frame*
					 (format nil "~10THelp: Apropos ~{~A~^, ~}"
						 words))))
	    (loop for (command . keys) in results
		  for documentation = (or (documentation command 'function)
					  "Not documented.")
		  do (with-text-style (out-stream '(:sans-serif :bold nil))
		       (present command
				`(command-name :command-table ,command-table)
				:stream out-stream))
		     (with-drawing-options (out-stream :ink +dark-blue+
						       :text-style '(:fix nil nil))
		       (format out-stream "~30T~:[M-x ... RETURN~;~:*~{~A~^, ~}~]"
			       (mapcar (lambda (keystrokes)
					 (format nil "~{~A~^ ~}"
						 (mapcar #'gesture-name (reverse keystrokes))))
				       (car keys))))
		     (with-text-style (out-stream '(:sans-serif nil nil))
		       (format out-stream "~&~2T~A~%"
			       (subseq documentation 0 (position #\Newline documentation))))
		  count command into length
		  finally (change-space-requirements out-stream
				 :height (* length (stream-line-height out-stream)))
			  (scroll-extent out-stream 0 0)))))))

(set-key `(com-apropos-command ,*unsupplied-argument-marker*)
	 'help-table
	 '((#\h :control) (#\a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Keyboard macros

(define-command-table keyboard-macro-table)

(define-command (com-start-kbd-macro
		 :name t
		 :command-table keyboard-macro-table)
    ()
  "Start recording keys to define a keyboard macro.
Use C-x ) to finish recording the macro, and C-x e to run it."
  (setf (recordingp *application-frame*) t)
  (setf (recorded-keys *application-frame*) '()))

(set-key 'com-start-kbd-macro 'keyboard-macro-table '((#\x :control) #\())

(define-command (com-end-kbd-macro
		 :name t
		 :command-table keyboard-macro-table)
    ()
  "Finish recording keys that define a keyboard macro.
Use C-x ( to start recording a macro, and C-x e to run it."
  (setf (recordingp *application-frame*) nil)
  (setf (recorded-keys *application-frame*)
	;; this won't work if the command was invoked in any old way
	(reverse (cddr (recorded-keys *application-frame*)))))

(set-key 'com-end-kbd-macro 'keyboard-macro-table '((#\x :control) #\)))

(define-command (com-call-last-kbd-macro
		 :name t
		 :command-table keyboard-macro-table)
    ((count 'integer :prompt "How many times?"))
  "Run the last keyboard macro that was defined.
Use C-x ( to start and C-x ) to finish recording a keyboard macro."
  (setf (remaining-keys *application-frame*)
        (loop repeat count append (recorded-keys *application-frame*)))
  (setf (executingp *application-frame*) t))

(set-key `(com-call-last-kbd-macro ,*numeric-argument-marker*)
         'keyboard-macro-table '((#\x :control) #\e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; example application

(defclass example-info-pane (info-pane)
  ()
  (:default-initargs
      :height 20 :max-height 20 :min-height 20
      :display-function 'display-info
      :incremental-redisplay t))

(defun display-info (frame pane)
  (declare (ignore frame))
  (format pane "Pane name: ~s" (pane-name (master-pane pane))))

(defclass example-minibuffer-pane (minibuffer-pane)
  ()
  (:default-initargs
      :height 20 :max-height 20 :min-height 20))

(defclass example-pane (esa-pane-mixin application-pane)
  ((contents :initform "hello" :accessor contents)))

(define-application-frame example (standard-application-frame
				   esa-frame-mixin)
  ()
  (:panes
   (window (let* ((my-pane 
		(make-pane 'example-pane
			   :width 900 :height 400
			   :display-function 'display-my-pane
			   :command-table 'global-example-table))
	       (my-info-pane
		(make-pane 'example-info-pane
			   :master-pane my-pane
			   :width 900)))
	  (setf (windows *application-frame*) (list my-pane))
	  (vertically ()
	    (scrolling ()
	      my-pane)
	    my-info-pane)))
   (minibuffer (make-pane 'example-minibuffer-pane :width 900)))
  (:layouts
   (default
       (vertically (:scroll-bars nil)
	 window
	 minibuffer)))
  (:top-level (esa-top-level)))

(defun display-my-pane (frame pane)
  (declare (ignore frame))
  (princ (contents pane) *standard-output*))

(defun example (&key (width 900) (height 400))
  "Starts up the example application"
  (let ((frame (make-application-frame
		'example
		:width width :height height)))
    (run-frame-top-level frame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Commands and key bindings

(define-command-table global-example-table
    :inherit-from (global-esa-table keyboard-macro-table))


