(in-package :qt)
(named-readtables:in-readtable :qt)

(defmarshal (value place (:|QVariant| :|const QVariant&|) :around cont)
  (cond ((null place)
         (with-object (v (qvariant value))
           (funcall cont v)))
        ((eq place :keep)
         (funcall cont (qvariant value)))
        (t
         (#_operator= (%qobject
                       (with-cache () (qtype-class (find-qtype "QVariant")))
                       (place-pointer place))
                      value)
         (funcall cont (place-pointer place)))))

(defun qvariant-ptr-types ()
  (with-cache ()
    (iter (for (code . type) in (list (cons (#_QVariant::Color) "QColor")
                                      (cons (#_QVariant::Pixmap) "QPixmap")
                                      (cons (#_QVariant::Icon) "QIcon")))
          (collect (cons (primitive-value code)
                         (find-qclass type))))))

(defun qvariant (value)
  (etypecase value
    (string (#_new QVariant :|const QString&| value))
    (integer (#_new QVariant :|int| value))
    ((or single-float double-float) (#_new QVariant :|double| value))
    (boolean (#_new QVariant :|bool| value))
    (alexandria:proper-list
     (#_new QVariant :|const QList<QVariant>&| value)
     #+nil
     (let ((variants (mapcar #'qvariant value)))
       (unwind-protect
            (#_new QVariant :|const QList<QVariant>&| variants)
         (dolist (variant variants)
           (#_delete variant)))))
    (qobject
     (if (qtypep value "QVariant")
         value
         (iter (for (code . type) in (qvariant-ptr-types))
               (when (qtypep value type)
                 (return (#_new QVariant code (qobject-pointer value))))
               (finally (return value)))))))

(defun unvariant (variant &optional (type (find-qtype "QVariant")))
  (let* ((qobject (%qobject (qtype-class type) variant))
         (code (primitive-value (#_type qobject))))
    (case code
      (2 (#_toInt qobject))
      (10 (#_toString qobject))
      (6 (#_toDouble qobject))
      (1 (#_toBool qobject))
      (9 (#_toList qobject))
      (t
         (alexandria:if-let ((qclass (cdr (assoc code (qvariant-ptr-types)))))
           (%qobject qclass (#_constData qobject))
           qobject)))))

(define-marshalling-test (value :|QVariant|)
  (labels ((variantable-p (x)
             (typecase x
               ((or string integer single-float double-float boolean) t)
               (alexandria:proper-list
                (every #'variantable-p x))
               (qobject
                (iter (for (nil . type) in (qvariant-ptr-types))
                  (thereis (qtypep x type))))
               (t nil))))
    (variantable-p value)))
