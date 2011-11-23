;;; -*- show-trailing-whitespace: t; indent-tabs-mode: nil -*-

;;; Copyright (c) 2009 David Lichteblau. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :qt)
(named-readtables:in-readtable :qt)

(defun resolve-cast (<from> <to>)
  (let* ((module (ldb-module <from>))
         (compatible-<to>
          (if (eql module (ldb-module <to>))
              <to>
              (find-qclass-in-module module (qclass-name <to>)))))
    (unless compatible-<to>
      (error "sorry, casting across modules in several steps not yet supported"))
    (values (data-castfn (data-ref module))
            compatible-<to>)))

(defun perform-cast (obj castfn <from> <to>)
  (cffi:foreign-funcall-pointer
   castfn
   ()
   :pointer (qobject-pointer obj)
   :short (unbash <from>)
   :short (unbash <to>)
   :pointer))

(defun %cast (obj <to>)
;;;   (let* ((from-class (qobject-class obj))
;;;          (module (ldb-module from-class))
;;;          (to-class
;;;           (if (eql module (ldb-module to-class))
;;;               to-class
;;;               (find-qclass-in-module module (qclass-name to-class)))))
;;;     (cffi:foreign-funcall-pointer
;;;      (data-castfn (data-ref module))
;;;      ()
;;;      :pointer (qobject-pointer obj)
;;;      :short (unbash from-class)
;;;      :short (unbash to-class)
;;;      :pointer))
  (let ((<from> (qobject-class obj)))
    (multiple-value-bind (fn <cto>)
        (resolve-cast <from> <to>)
      (perform-cast obj fn <from> <cto>))))

(defun marshal (value type stack-item &optional cont)
  "Marshal value using specified target TYPE into STACK-ITEM which should
  be a pointer to |union StackItem| structure. If CONT is non-NIL,
  call it and then delete any objects created during marshalling.
  If CONT is NIL, don't delete anything."
  (funcall (marshaller value type) value stack-item nil cont))

;; FIXME: trying to reuse the marshaller causes memory fault

(defun marshaller (obj <type>)
  (marshaller-2 (type-of obj)
                (when (typep obj 'qobject)
                  (qobject-class obj))
                <type>))

#+nil
(let ((marshaller-table (make-hash-table :test #'equal)))
  (defun marshaller (obj <type>)
    (let ((key (cons (type-of obj)
                     (when (typep obj 'qobject)
                       (qobject-class obj)))))
      (or (gethash key marshaller-table)
          (setf (gethash key marshaller-table)
                (marshaller-2 (type-of obj)
                              (when (typep obj 'qobject)
                                (qobject-class obj))
                              <type>))))))

(defun marshaller-2 (obj-type qobject-class <type>)
  (let* ((slot (qtype-stack-item-slot <type>))
         (set-thunk
          (macrolet
              ((si (slot)
                 `(cffi:foreign-slot-value si '|union StackItem| ',slot))
               (dispatching ((getter slot) &body body)
                 `(ecase ,slot
                    ,@ (mapcar (lambda (slot)
                                 `((,slot)
                                   (macrolet ((,getter () `(si ,',slot)))
                                     ,@body)))
                          '(ptr bool char uchar short ushort int
                            uint long ulong float double enum class)))))
            (case slot
              (bool  (lambda (val si) (setf (si bool) (if val 1 0))))
              (class (if qobject-class
                         (let ((<from> qobject-class))
                           (multiple-value-bind (castfn <to>)
                               (resolve-cast <from> (qtype-class <type>))
                             (lambda (val si)
                               (setf (si class)
                                     (perform-cast val castfn <from> <to>)))))
                         (lambda (val si)
                           (setf (si class)
                                 (if (typep val 'cffi:foreign-pointer)
                                     val
                                     (qobject-pointer val))))))
              (enum (cond ((subtypep obj-type 'integer)
                           (lambda (val si) (setf (si enum) val)))
                          ((subtypep obj-type 'enum)
                           (lambda (val si) (setf (si enum) (primitive-value val))))
                          (t (error "type mismatch: ~s for type slot ~s" obj-type slot))))
              (int (cond ((subtypep obj-type 'integer)
                          (lambda (val si) (setf (si int) val)))
                         ((subtypep obj-type 'enum)
                          (lambda (val si) (setf (si int) (primitive-value val))))
                         (t (error "type mismatch: ~s for type slot ~s" obj-type slot))))
              (uint (cond ((subtypep obj-type 'integer)
                           (lambda (val si) (setf (si uint) val)))
                          ((subtypep obj-type 'enum)
                           (lambda (val si) (setf (si uint) (primitive-value val))))
                          (t (error "type mismatch: ~s for type slot ~s" obj-type slot))))
              (float (lambda (val si) (setf (si float) (float val 1.0s0))))
              (double (lambda (val si) (setf (si double) (float val 1.0d0))))
              ;; that leaves:
              ;;   ptr char uchar short ushort long ulong
              (t
                 (dispatching (%si slot)
                              (lambda (val si)
                                (setf (%si) val)))))))
         (primary-cons
          (get (qtype-interned-name <type>) 'marshaller/primary))
         (around-cons
          (get (qtype-interned-name <type>) 'marshaller/around))
         (primary-type (car primary-cons))
         (primary-thunk (cdr primary-cons))
         (around-type (car around-cons))
         (around-thunk (cdr around-cons)))
    (cond
      ((and primary-thunk (subtypep obj-type primary-type))
       (assert (null around-thunk))
       (named-lambda marshal-primary-outer (value stack-item assignment-p cont)
         (if assignment-p
             (funcall primary-thunk value stack-item)
             (funcall set-thunk
                      (funcall primary-thunk value (cffi:null-pointer))
                      stack-item))
         (when cont (funcall cont))))
      ((and around-thunk (subtypep obj-type around-type))
       (named-lambda marshal-around-outer (value stack-item assignment-p cont)
         (funcall around-thunk
                  value
                  (cond (assignment-p stack-item)
                        ((null cont) :keep)
                        (t nil))
                  (named-lambda marshal-around-inner (new-value)
                    (unless assignment-p
                      (funcall set-thunk new-value stack-item))
                    (when cont (funcall cont))))))
      ((eq :pointer (qtype-kind <type>))
       (named-lambda marshal-default (value stack-item assignment-p cont)
         (declare (ignore assignment-p))
         (funcall set-thunk value stack-item)
         (when cont (funcall cont))))
      (t
       (named-lambda marshal-default (value stack-item assignment-p cont)
         (unless (and assignment-p (eq slot 'class))
           (funcall set-thunk value stack-item)
           (when cont (funcall cont))))))))

(defmacro defmarshal ((var place name &key around (type t)) &body body)
  (if (consp name)
      `(progn
         ,@(iter (for n1 in name)
                 (collect `(defmarshal (,var ,place ,n1 :around ,around :type ,type) ,@body))))
      (let ((function-name (intern (format nil "~a-~a" name 'marshaller))))
        (if around
            `(setf (get ',name 'marshaller/primary) nil
                   (get ',name 'marshaller/around)
                   (cons ',type (named-lambda ,function-name (,var ,place ,around) ,@body)))
            `(setf (get ',name 'marshaller/primary)
                   (cons ',type (named-lambda ,function-name (,var ,place) ,@body))
                   (get ',name 'marshaller/around) nil)))))

;; place values:
;; NIL     = delete the marshalled object after calling continuation
;; :KEEP   = keep the marshalled object after calling continuation
;; pointer = use assignment operation

(defun place-pointer (place)
  (case place
    ((nil :keep) (cffi:null-pointer))
    (t place)))

(defun must-delete-object-p (place)
  (or (null place)
      (cffi:null-pointer-p place)))

(defmarshal (value place (:|QString| :|const QString&|) :around cont :type string)
  (let ((qstring (sw_make_qstring value (place-pointer place))))
    (unwind-protect
         (funcall cont qstring)
      (when (must-delete-object-p place)
        (sw_delete_qstring qstring)))))

;;; Don't delete the string because it may be used afterwards by Qt
(defmarshal (value place :|QString*| :around cont :type string)
  (funcall cont (sw_make_qstring value (place-pointer place))))

(defmarshal (value place (:|const char*| |unsigned char*| |char*|) :around cont :type string)
  (let ((char* (cffi:foreign-string-alloc value)))
    (unwind-protect
         (funcall cont char*)
      (cond ((must-delete-object-p place)
             (cffi:foreign-free char*))
            ((not (eq :keep place))
             (warn "place specified for char*, possible memory leak"))))))

(defmarshal (value place (:|QByteArray| :|const QByteArray&|) :around cont :type string)
  (let ((qbytearray (sw_make_qbytearray value (place-pointer place))))
    (unwind-protect
         (funcall cont qbytearray)
      (when (must-delete-object-p place)
        (sw_delete_qbytearray qbytearray)))))
