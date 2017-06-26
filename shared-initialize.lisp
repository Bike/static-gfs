(in-package #:static-gfs)

(defun compute-shinitializer (class slot-names initkeys)
  (multiple-value-bind (applicable-methods certainty)
      (compute-applicable-methods-using-classes
       #'shared-initialize
       (list class (find-class t)))
    (if (can-optimize-shinitializer-p applicable-methods certainty)
        (values :optimized-shared-initialize
                (compute-optimized-shinitializer
                 class applicable-methods slot-names initkeys))
        (values :fallback-shared-initialize
                (compute-fallback-shinitializer slot-names initkeys)))))

(defun can-optimize-shinitializer-p (applicable-methods certainty)
  (and certainty
       (can-fake-emf-p
        applicable-methods
        (find-method #'shared-initialize
                     nil
                     ;; primary method varies by impl.
                     (list (find-class #+sbcl 'sb-pcl::slot-object
                                       #+clasp 't
                                       #-(or sbcl clasp) 'standard-object)
                           (find-class 't))))))

(defun compute-fallback-shinitializer (slot-names initkeys)
  (multiple-value-bind (params initargs)
      (keys-to-initargs initkeys)
    `(lambda (instance ,@params)
       (declare (notinline shared-initialize))
       (shared-initialize instance ',slot-names ,@initargs))))

(defun compute-optimized-shinitializer (class applicable-methods slot-names initkeys)
  (multiple-value-bind (params initargs) (keys-to-initargs initkeys)
    `(lambda (instance ,@params)
       ,(wrap-in-fake-emf applicable-methods
                          (optimized-si-primary class slot-names
                                                initkeys 'instance params)
                          `(list instance ',slot-names ,@initargs)))))

(defun optimized-si-primary (class slot-names initkeys instance params)
  ;; TODO: Compile slot-boundp/value-using-class.
  ;; This could be done with a cell per call, but it might be nice to
  ;; have this body bind the cells and use them repeatedly.
  ;; for example (setf (slot-value-using-class ...) value)
  ;; could be compiled into ((lambda ...) slot-location value)
  ;; to avoid compiling a separate lambda for every single slotd.
  (multiple-value-bind (params slotds other-slotds)
      (keys-to-slotds class initkeys params)
    (flet ((initializing-slotd-p (slotd)
             (or (eq slot-names t)
                 (find (slot-definition-name slotd) slot-names))))
      `(progn
         ;; SHARED-INITIALIZE fills in slots with an initarg provided (the first loop),
         ;; and then ones that don't (the second)
         ,@(loop for param in params for slotd in slotds
                 when (initializing-slotd-p slotd)
                   collect `(setf (slot-value-using-class ,class ,instance ,slotd)
                                  ,param))
         ;; CLHS specifies that shared initialize's primary method does NOT fill in
         ;; from initfunctions any slots that are already filled in, e.g. by :before
         ,@(loop for slotd in other-slotds
                 for initfunction = (slot-definition-initfunction slotd)
                 when (and (initializing-slotd-p slotd) initfunction)
                   collect `(unless (slot-boundp-using-class ,class ,instance ,slotd)
                              (setf (slot-value-using-class ,class ,instance ,slotd)
                                  (funcall (the function
                                                ;; again, note that including a function
                                                ;; prevents this form from being dumped.
                                                ,(slot-definition-initfunction slotd))))))
         ,instance))))

(defun keys-to-slotds (class initkeys variables)
  ;; returns three variables:
  ;; a list of variables, a list of slotds, and another list of slotds.
  ;; The variables should be used to initialize the first slotds' slots.
  ;; the other slotds did not have a specified initarg.
  (let ((slotds (class-slots class))
        ret-variables ret-slotds)
    ;; Per 7.1.4 there can be multiple slots initialized by one initarg,
    ;; and the converse is also true. Thus this complicated... thing.
    (loop for key in initkeys
          for variable in variables
          ;; remove any slots we found an initarg for.
          do (setf slotds
                   (set-difference
                    slotds
                    (loop for slotd in slotds
                          when (find key (slot-definition-initargs slotd))
                            do (push variable ret-variables)
                               (push slotd ret-slotds)
                            and collect slotd))))
    (values ret-variables ret-slotds slotds)))
