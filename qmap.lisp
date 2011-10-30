(in-package :qt)

(defmarshal (value place (:|QMap<QString,QVariant>| :|const QMap<QString,QVariant>&|) :around cont :type list)
  (let ((qmap (sw_qmap_qstring_qvariant_new (place-pointer place))))
    (unwind-protect
         (progn
           (iter (for (k . v) in value)
                 (let ((qstr (sw_make_qstring (string k) (cffi:null-pointer))))
                   (with-object (variant (qvariant v))
                     (unwind-protect
                          (sw_qmap_qstring_qvariant_set
                           qmap
                           qstr
                           (qobject-pointer variant))
                       (sw_delete_qstring qstr)))))
           (funcall cont qmap))
      (when (must-delete-object-p place)
        (sw_qmap_qstring_qvariant_delete qmap)))))

(define-marshalling-test (value :|QMap<QString,QVariant>|)
  (and (alexandria:proper-list-p value)
       (every #'consp value)))

(def-unmarshal (value "QMap<QString,QVariant>" type)
  (let* ((result '())
         (*map-func* #'(lambda (k v)
                         (push (cons (qstring-pointer-to-lisp k)
                                     (unvariant v))
                               result))))
    (sw_qmap_qstring_qvariant_map value (cffi:callback map-callback))
    (nreverse result)))
