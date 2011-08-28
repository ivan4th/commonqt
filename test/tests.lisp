(in-package :qt-tests)

(named-readtables:in-readtable :qt)

(defmacro with-qapp (&body body)
  `(let ((qapp (ensure-qapplication)))
     (declare (ignorable qapp))
     ,@body))

;; CommonQt reader macro works by defining some new macros.
;; This is ok for ordinary compilation but with RT test
;; cases the macroexpansion is done when tests are run,
;; and the newly defined macros don't make it into fasls,
;; so loading compiled tests fails. We have to put the
;; test body into separate defun, sacrificing runtime
;; macro expanding.

(defmacro deftest/qt (name form &rest values)
  (alexandria:with-gensyms (func-name)
    `(progn
       (defun ,func-name () (with-qapp ,form))
       (deftest ,name (,func-name) ,@values))))

(let ((bad (cons nil nil)))
  (defun marshal-and-test (value type class-p test-fun &optional (unmarshal-type type) (other-value value))
    (cffi:with-foreign-object (stack-item 'qt::|union StackItem|)
      (let ((result bad)
            (<type> (or (qt::find-qtype type)
                        (error "no such type ~s" type)))
	    (<unmarshal-type> (qt::find-qtype unmarshal-type)))
        (assert (qt::can-marshal-p value <type>) ()
                "cannot marshal ~s as ~s" value type)
        (qt::marshal value <type>
                     stack-item
		     #'(lambda ()
			 (setf result
			       (funcall test-fun
					(qt::unmarshal
					 <unmarshal-type> stack-item)))))
        (qt::marshal other-value <type>
                    stack-item
                    #'(lambda ()
                        (qt::assign
                         value <type>
                         (if class-p
                             (cffi:foreign-slot-value stack-item 'qt::|union StackItem| 'qt::class)
                             stack-item))
                        (assert (equal result
                                       (funcall test-fun
                                                (qt::unmarshal
                                                 <unmarshal-type> stack-item))))))
        (assert (not (eq bad result)) () "marshalling continuation not invoked")
        result)))
  
  (defun remarshal (value type class-p &optional with-const-p (key #'identity) (other-value value))
    (let ((result (marshal-and-test value type class-p key)))
      (when with-const-p
	(let ((const-type (format nil "const ~A&" type)))
	  (marshal-and-test value
			    const-type
                            class-p
			    #'(lambda (v)
                                (setf ;; result (funcall key result)
                                 v (funcall key v))
				(assert (equal result v)
					() "remarshal: got ~s instead of ~s when marshalling using const ~A&"
					v result type)
                                nil)
			    type
                            other-value)))
      result)))

(defmacro define-marshalling-test (name type class-p with-const-p &rest values)
  `(deftest/qt ,name
       (values ,@(iter (for val in values)
                       (for rval in (reverse values))
                       (collect `(remarshal ',val ,type ,class-p ,with-const-p #'identity ',rval))))
     ,@values))

(defmacro define-marshalling-test/no-unmarshal
    (name type class-p key &rest values)
  `(deftest/qt ,name
       (values ,@(iter (for val in values)
                       (for rval in (reverse values))
                       (collect `(marshal-and-test ',val ,type ,class-p ,key ,type ',rval))))
     ,@values))

(define-marshalling-test/no-unmarshal test-qbytearray-marshalling
    "QByteArray" t (lambda (x) (#_data x))
  "" "abc" "qwerty uiop" #.(make-string 3 :initial-element (code-char 1093)))

(define-marshalling-test test-qvariant-marshalling
    "QVariant" t t
  "" 123 123.25d0 "zzz" #.(make-string 3 :initial-element (code-char 1093)))

(deftest/qt test-single-float-via-qvariant-marshalling
    (values (remarshal 0.0 "QVariant" t t)
            (remarshal 123.25 "QVariant" t t))
  0.0d0 123.25d0)

(deftest/qt test-qcolor-via-qvariant-marshalling
    (flet ((convert (c) (#_name c)))
      (values (remarshal (#_new QColor "#000000") "QVariant" t t #'convert)
              (remarshal (#_new QColor "#ffffff") "QVariant" t t #'convert)))
  "#000000" "#ffffff")

(deftest/qt test-qpixmap-via-qvariant-marshalling
    (flet ((convert (p) (cons (#_width p) (#_height p))))
      (values (remarshal (#_new QPixmap 142 100) "QVariant" t t #'convert)))
  (142 . 100))

(deftest/qt test-qpixmap-via-qvariant-marshalling
    (flet ((convert (p)
             (assert (qtypep p "QPixmap"))
             (cons (#_width p) (#_height p))))
      (values (remarshal (#_new QPixmap 142 100) "QVariant" t t #'convert)))
  (142 . 100))

(deftest/qt test-qicon-via-qvariant-marshalling
    (flet ((convert (icon)
             (assert (qtypep icon "QIcon"))
             (#_isNull icon)))
      (values (remarshal (#_new QIcon) "QVariant" t t #'convert)))
  t)

(define-marshalling-test test-qstring-marshalling
    "QString" t t
  "" "abc" "qwerty uiop" #.(make-string 3 :initial-element (code-char 1093)))

(define-marshalling-test test-qstringlist-marshalling
    "QStringList" t t
  () ("abc") ("" "abcd" "qqqq" "rrr") ("abc" "Def" "ghi"))

(define-marshalling-test test-qlistint-marshalling
    "QList<int>" t t
  () (42) (#x7fffffff 12345 678) (11 12))

(define-marshalling-test/no-unmarshal test-qlistbytearray-marshalling
    "QList<QByteArray>" t (lambda (x)
                            (iter (for y in x)
                                  (collect (#_data y))))
  () ("abc") ("" "abcd" "qqqq" "rrr") ("abc" "Def" "ghi"))

(define-marshalling-test test-qlistqvariant-marshalling
    "QList<QVariant>" t t
  () ("abc") ("" 123 "zzz" 456))

(deftest/qt test-qobjectlist-marshalling
    (let ((a (#_new QObject))
          (b (#_new QPushButton "Def"))
          (c (#_new QLabel "zzz")))
      (#_setObjectName a "Abc")
      (flet ((extract (list)
               (list
                (#_objectName (first list))
                (#_text (second list))
                (#_text (third list)))))
        (extract (remarshal (list a b c) "QList<QObject*>" t t))))
  ("Abc" "Def" "zzz"))

(deftest/qt test-object-children
    (let* ((a (#_new QObject))
           (b (#_new QObject a))
           (c (#_new QObject a)))
      (set-difference (list b c) (#_children a)))
  nil)

(deftest/qt test-item-model-stuff-marshalling
    (let ((model (#_new QStandardItemModel)))
      (#_appendRow model (list (#_new QStandardItem "01")
                               (#_new QStandardItem "bca")))
      (#_appendRow model (list (#_new QStandardItem "02")
                               (#_new QStandardItem "abc")))
      (#_appendRow model (list (#_new QStandardItem "03")
                               (#_new QStandardItem "bcq")))
      (values
        (iter (for item in (remarshal (list (#_new QStandardItem "zz")
                                            (#_new QStandardItem "rr"))
                                      "QList<QStandardItem*>" t))
              (collect (#_text item)))
        (iter (for i from 0 to 2)
              (collect (cons (#_data model (#_index model i 0))
                             (#_data model (#_index model i 1)))))
        (iter (for index in (#_match model (#_index model 0 1)
                                     (#_Qt::DisplayRole) "bc" -1))
              (collect (cons (#_row index) (#_column index))))
        (remarshal (#_match model (#_index model 0 1)
                            (#_Qt::DisplayRole) "bc" -1)
                   "QList<QModelIndex>" t nil ;; TBD: t t?
                   #'(lambda (l)
                       (iter (for idx in l)
                             (collect
                                 (cons (#_row idx) (#_column idx))))))))
  ("zz" "rr") (("01" . "bca") ("02" . "abc") ("03" . "bcq"))
  ((0 . 1) (2 . 1)) ((0 . 1) (2 . 1)))

(deftest/qt test-no-enum-confusion
    (let ((action (#_new QAction (null-qobject (find-qclass "QAction"))))
          (keys (list (#_new QKeySequence :|int| (#_Qt::Key_Backspace))
                      (#_new QKeySequence (#_QKeySequence::Back)))))
      (#_setShortcuts action keys)
      (iter (for shortcut in (#_shortcuts action))
            (collect (#_toString shortcut))))
  ("Backspace" "Alt+Left"))

(defclass sig-emitter ()
  ()
  (:metaclass qt-class)
  (:qt-superclass "QObject")
  (:signals ("noArgs()")
            ("oneArg(int)")
            ("twoArgs(int, QString)")))

(defmethod initialize-instance :after ((instance sig-emitter) &key parent)
  (if parent
      (new instance parent)
      (new instance)))

(defclass sig-receiver ()
  ((handler :accessor handler :initarg :handler))
  (:metaclass qt-class)
  (:qt-superclass "QObject")
  (:slots ("slotNoArgs()"
           (lambda (this &rest args)
             (apply (handler this) 'no-args args)))
          ("slotOneArg(int)"
           (lambda (this &rest args)
             (apply (handler this) 'one-arg args)))
          ("slotTwoArgs(int, QString)"
           (lambda (this &rest args)
             (apply (handler this) 'two-args args)))))

(defmethod initialize-instance :after ((instance sig-receiver) &key parent)
  (if parent
      (new instance parent)
      (new instance)))

(deftest/qt test-connect
    (let ((log '()))
      (flet ((note (&rest args)
               (push args log)))
        (let ((sender (make-instance 'sig-emitter))
              (receiver (make-instance 'sig-receiver :handler #'note)))
          (connect sender "noArgs()" receiver "slotNoArgs()")
          (connect sender (QSIGNAL "oneArg(int)") receiver "slotOneArg(int)")
          (connect sender "twoArgs(int, QString)"
                   receiver (QSLOT "slotTwoArgs(int, QString)"))
          (emit-signal sender "noArgs()")
          (emit-signal sender "oneArg(int)" 42)
          (emit-signal sender "oneArg(int)" 4242)
          (emit-signal sender "twoArgs(int, QString)" 42 "zzz")

          (disconnect sender "noArgs()" receiver "slotNoArgs()")
          (disconnect sender "twoArgs(int, QString)"
                      receiver (QSLOT "slotTwoArgs(int, QString)"))
          (emit-signal sender "noArgs()")
          (emit-signal sender "oneArg(int)" 123)
          (emit-signal sender "twoArgs(int, QString)" 12 "qqq")

          (disconnect sender (QSIGNAL "oneArg(int)")
                      receiver (QSLOT "slotOneArg(int)"))
          (emit-signal sender "noArgs()")
          (emit-signal sender "oneArg(int)" 456)
          (emit-signal sender "twoArgs(int, QString)" 34 "qqq")
          (reverse log))))
  ((no-args)
   (one-arg 42)
   (one-arg 4242)
   (two-args 42 "zzz")
   (one-arg 123)))

(deftest/qt test-dynamic-connect
    (let ((log '()))
      (let ((sender (make-instance 'sig-emitter))
            (receiver (#_new QObject)))
        (labels ((note (&rest args)
                   (push args log))
                 (no-args (this)
                   (assert (eq receiver this))
                   (note 'no-args))
                 (one-arg (n)
                   (note 'one-arg n))
                 (two-args (this n s)
                   (assert (eq receiver this))
                   (note 'two-args n s)))
          ;; we don't use lambdas for connections because we
          ;; want to break connections later
          (connect sender "noArgs()" receiver #'no-args)
          (connect sender (QSIGNAL "oneArg(int)") #'one-arg)
          (connect sender "twoArgs(int, QString)" receiver #'two-args)
          (emit-signal sender "noArgs()")
          (emit-signal sender "oneArg(int)" 42)
          (emit-signal sender "oneArg(int)" 4242)
          (emit-signal sender "twoArgs(int, QString)" 42 "zzz")

          (disconnect sender "noArgs()" receiver #'no-args)
          (emit-signal sender "noArgs()")
          (emit-signal sender "oneArg(int)" 123)
          (emit-signal sender "twoArgs(int, QString)" 12 "qqq")

          (#_delete receiver)
          (emit-signal sender "noArgs()")
          (emit-signal sender "oneArg(int)" 456)
          (emit-signal sender "twoArgs(int, QString)" 34 "qqq")

          (disconnect sender (QSIGNAL "oneArg(int)") #'one-arg)
          (emit-signal sender "noArgs()")
          (emit-signal sender "oneArg(int)" 789)
          (emit-signal sender "twoArgs(int, QString)" 56 "qqq")
          (reverse log))))
  ((no-args)
   (one-arg 42)
   (one-arg 4242)
   (two-args 42 "zzz")
   (one-arg 123)
   (two-args 12 "qqq")
   (one-arg 456)))

;; TBD: deconstify types when looking for marshaller/unmarshaller, remove (macro-generated) duplicate marshaller definitions

(deftest/qt window-geometry-using-qvariant-and-qbytarray
  ;; regression test for issue with with qbytearrays unmarshalled as strings
  (with-object (window (#_new QWidget))
    (with-object (sx (#_new QSettings "CommonQt test" "CommonQt test"))
      (#_setValue sx "geometry" (#_new QVariant (#_saveGeometry window)))
      (#_restoreGeometry window (#_toByteArray (#_value sx "geometry")))))
  t)

(defclass prop-class ()
  ((someprop :accessor someprop :initform "qwerty")
   (anotherprop :accessor anotherprop :initform 42)
   (color :accessor color :initform (#_new QColor "#fff")))
  (:metaclass qt-class)
  (:qt-superclass "QObject")
  (:properties ("someprop" "QString"
                           :read someprop
                           :write t
                           :notify "somepropChanged()")
               ("anotherprop" "int"
                              :read
                              (lambda (obj)
                                (anotherprop obj))
                              :write
                              (lambda (new-value obj)
                                (setf (anotherprop obj) new-value)))
               ("color" "QColor"
                        :read color
                        :notify "colorChanged()"))
  (:signals ("somepropChanged()")
            ("colorChanged()")))

(defmethod initialize-instance :after ((instance prop-class) &key parent)
  (if parent
      (new instance parent)
      (new instance)))

(deftest/qt test-properties
    (let ((obj (make-instance 'prop-class)))
      ;; TBD: notifications
      (let* ((v1 (#_property obj "someprop"))
             (v2 (#_property obj "anotherprop"))
             (mobj (#_metaObject obj))
             (p1 (#_property mobj (#_indexOfProperty mobj "someprop")))
             (p2 (#_property mobj (#_indexOfProperty mobj "anotherprop")))
             (p3 (#_property mobj (#_indexOfProperty mobj "color"))))
        (flet ((prop-info (prop)
                 (list (#_name prop)
                       (#_typeName prop)
                       (#_isReadable prop)
                       (#_isWritable prop)
                       (if (#_hasNotifySignal prop)
                           (#_signature (#_notifySignal prop))
                           nil))))
          (values v1 v2
                  (#_setProperty obj "someprop" "zzz")
                  (#_setProperty obj "anotherprop" 4242)
                  (someprop obj)
                  (anotherprop obj)
                  (#_property obj "someprop")
                  (#_property obj "anotherprop")
                  (#_name (#_property obj "color"))
                  (prop-info p1)
                  (prop-info p2)
                  (prop-info p3)))))
  "qwerty" 42 t t "zzz" 4242 "zzz" 4242 "#ffffff"
  ("someprop" "QString" t t "somepropChanged()")
  ("anotherprop" "int" t t nil)
  ("color" "QColor" t nil "colorChanged()"))
