(defpackage :qt-repl
  (:use :cl :qt)
  (:export #:start-gui-thread
           #:eval-in-gui-thread))

(in-package :qt-repl)

(named-readtables:in-readtable :qt)

(defvar *qapp*)
(defvar *notifier*)
(defvar *gui-thread*)
(defvar *executer*)

(defun message-handler ())
(defclass repl-notifier ()
  ((pending-form :accessor pending-form)
   (form-result :accessor form-result)
   (new-package :accessor new-package))
  (:metaclass qt-class)
  (:qt-superclass "QObject")
  (:signals ("formReady()")))

(defmethod initialize-instance :after ((repl-notifier repl-notifier)
                                       &key &allow-other-keys)
  (new repl-notifier))

(defun notifier-do-eval (notifier)
  (flet ((doit ()
           (setf (form-result notifier)
                 (multiple-value-list
                     (eval (pending-form notifier)))
                 (new-package notifier) *package*)))
    #-swank
    (doit)
    #+swank
    (let ((swank:*sldb-quit-restart* (find-restart 'abort)))
      (doit))))

(defclass repl-executer ()
  ((notifier :reader notifier :initarg :notifier))
  (:metaclass qt-class)
  (:qt-superclass "QObject")
  (:slots ("handleForm()" (lambda (this)
                            (notifier-do-eval (notifier this))))))

(defmethod initialize-instance :after ((repl-executer repl-executer)
                                       &key notifier &allow-other-keys)
  (assert notifier () "must specify notifier")
  (new repl-executer)
  (#_connect "QObject"
             notifier (QSIGNAL "formReady()")
             repl-executer (QSLOT "handleForm()")
             (#_BlockingQueuedConnection "Qt")))

(defun %eval-in-gui-thread (notifier form)
  (unwind-protect
       (progn
         (setf (pending-form notifier) form)
         (emit-signal notifier "formReady()")
         (cond ((slot-boundp notifier 'form-result)
                (setf *package* (new-package notifier))
                (apply #'values (form-result notifier)))
               (t
                ;; FIXME: this is perhaps poor substitute
                (format *debug-io* ";; Evaluation aborted~%")
                (values))))
    (slot-makunbound notifier 'pending-form)
    (slot-makunbound notifier 'form-result)))

(defmacro eval-in-gui-thread (&body body)
  `(%eval-in-gui-thread *notifier*
                        ',(if (rest body)
                              `(progn ,@body)
                              (first body))))

;; print settings sometimes becomes skewed in the new thread

(defparameter *globals*
  '(*debug-io* *query-io* *terminal-io* *standard-output*
    *standard-input* *error-output* *trace-output*
    *print-array* *print-base* *print-radix*
    *print-case* *print-circle* *print-escape*
    *print-gensym* *print-level* *print-length*
    *print-lines* *print-miser-width* *print-pretty*
    *print-readably* *print-right-margin*
    *package*))

(defun install-debug-io-message-handler ()
  (let ((output *debug-io*))
    (install-message-handler
     #'(lambda (message-type message)
         (format output "~&QT ~A: ~A~%"
                 (case message-type
                   (0 "DEBUG")
                   (1 "WARNING")
                   (2 "CRITICAL")
                   (3 "FATAL")
                   (t "UNKNOWN"))
                 message)))))

(defun start-gui-thread (&optional (install-repl-hook t))
  (unless (boundp '*gui-thread*)
    (ensure-smoke :qtcore)
    (ensure-smoke :qtgui)
    (setf *notifier* (make-instance 'repl-notifier)
          *gui-thread*
          (let ((global-values (mapcar #'symbol-value *globals*))
                (lock (bt:make-lock))
                (cv (bt:make-condition-variable))
                (ready nil))
            (bt:make-thread
             #'(lambda ()
                 (loop for var in *globals*
                       for value in global-values
                       do (setf (symbol-value var) value))
                 (setf *qapp* (make-qapplication)
                       *executer* (make-instance 'repl-executer
                                                 :notifier *notifier*))
                 (#_setQuitOnLastWindowClosed *qapp* nil)
                 (bt:with-lock-held (lock)
                   (setf ready t)
                   (bt:condition-notify cv))
                 (#_exec *qapp*)))
            (loop
              (bt:with-lock-held (lock)
                (when ready (return))
                (bt:condition-wait cv lock)))))
    (install-debug-io-message-handler)
    (when (and install-repl-hook (find-package "SWANK"))
      (let ((hooks (find-symbol "*SLIME-REPL-EVAL-HOOKS*" "SWANK")))
        (if hooks
            (push #'(lambda (form)
                      (%eval-in-gui-thread *notifier* form))
                  (symbol-value (find-symbol "*SLIME-REPL-EVAL-HOOKS*" "SWANK")))
            (warn "Cannot initialize *SLIME-REPL-EVAL-HOOKS*, use (eval-in-gui-thread ...) form."))
        (values)))))
