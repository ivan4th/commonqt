(in-package :qt)
(named-readtables:in-readtable :qt)

(defun assign (value type ptr)
  "Use C++ assignment operator to place value at PTR"
  (let ((done-p nil))
    (funcall (marshaller value type) value ptr t
             #'(lambda ()
                 (setf done-p t)))
    (unless done-p
      ;; standard marshaller wasn't able to do assignment
      (#_operator= (%qobject (qtype-class type) ptr) value))))
