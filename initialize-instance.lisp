(in-package #:static-gfs)

(defun compute-initializer (class initkeys)
  (multiple-value-bind (applicable-methods certainty)
      (compute-applicable-methods-using-classes #'initialize-instance (list class))
    (if (can-optimize-initializer-p applicable-methods certainty)
        (compute-optimized-initializer class applicable-methods initkeys)
        (compute-fallback-initializer initkeys))))

(defun can-optimize-initializer-p (applicable-methods certainty)
  (and certainty
       (can-fake-emf-p
        applicable-methods
        (find-method #'initialize-instance nil
                     (list (find-class #+sbcl 'sb-pcl::slot-object
                                       #+clasp 't
                                       #-(or sbcl clasp) 'standard-object))
                     nil))))

(defun compute-optimized-initializer (class applicable-methods initkeys)
  (multiple-value-bind (state shinitializer-lambda)
      (compute-shinitializer class 't initkeys)
    (multiple-value-bind (params initargs)
        (keys-to-initargs initkeys)
      (values
       state
       `(lambda (instance ,@params)
          ,(wrap-in-fake-emf applicable-methods
                             `(funcall ,shinitializer-lambda
                                       instance ,@params)
                             `(list instance ,@Initargs)))))))

(defun compute-fallback-initializer (initkeys)
  (values
   :fallback-initialize-instance
   (fallback-initializer-form initkeys)))

(defun fallback-initializer-form (initkeys)
  (multiple-value-bind (params initargs) (keys-to-initargs initkeys)
    `(lambda (instance ,@params)
       (declare (notinline initialize-instance))
       (initialize-instance instance ,@initargs))))
