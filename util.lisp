(in-package #:static-gfs)

(defun method-specializes-p (method arguments)
  ;; ARGUMENTS is a list of required arguments, and is as long
  ;; as the method's specializers.
  ;; as far as i can see this does not exist in MOP.
  ;; additionally, there's no protocol for specializers
  ;; other than classes or EQL specializers.
  ;; So we use subtypep return convention. FIXME?
  (loop with certain = t
        for arg in arguments
        for spec in (method-specializers method)
        do (typecase spec
             (class (unless (typep arg spec)
                      (return-from method-specializes-p
                        (values nil t))))
             (eql-specializer
              (unless (eql arg (eql-specializer-object spec))
                (return-from method-specializes-p
                  (values nil t))))
             (t (setf certain nil)))
        finally (return (if certain
                            (values t t)
                            (values nil nil)))))

(defun method-may-specialize-p (method arguments)
  ;; careful interpretation of the above.
  (multiple-value-bind (specializes certainty)
      (method-specializes-p method arguments)
    (or specializes (not certainty))))

;;; Like the above, but provide classes of arguments.
;;; (Like compute-applicable-methods-using-classes.)
;;; Except that, given the context of initialize-instance, if there's
;;; an EQL specializer we say we definitely don't specify on it
;;; (since the instance of the class is fresh).
(defun method-specializes-using-classes-p (method classes)
  (loop with certain = t
        for class in classes
        for spec in (method-specializers method)
        do (typecase spec
             ;; SUBTYPEP is always certain since these are classes.
             (class (cond ((subtypep class spec)
                           (return-from method-specializes-using-classes-p
                             (values t t)))
                          ((subtypep spec class)
                           ;; A given object of class CLASS might fit the
                           ;; method or might not.
                           (setf certain nil))))
             ;; see comment above
             (eql-specializer
              (return-from method-specializes-using-classes-p
                (values nil t)))
             (t (setf certain nil)))
        finally (return
                  (if certain
                      (values t t)
                      (values nil nil)))))

(defun method-may-specialize-using-classes-p (method classes)
  (multiple-value-bind (specializes certain)
      (method-specializes-using-classes-p method classes)
    (or specializes (not certain))))

;;; Given a &key argument list from a form, returns six values:
;;; 1) Canonical key args in order.
;;; 2) Canonical values for them in the corresponding order
;;; 3) A list of bindings for LET used by #2
;;; 4) Declarations for #3
;;; 5) Whether :allow-other-keys with a true value is in place.
;;; 6) success-p.
;;; Useful for eliminating calls with &key. Example output:
;;; (:foo 4 'bar 7) => (bar #:g1 :foo #:g2), ((#:g1 7) (#:g2 4)), nil, nil, T
;;; (:foo 1 :foo 2) => (:foo #:g3), ((#:g3 1) (#:g4 2)), (ignore #:g4), nil, T
;;; (a 1) => NIL, NIL, NIL, NIL, NIL, NIL ; non-constant keyword

;;; Originally I was going to have this reorder the key args, but per 7.1.4
;;; we need them in order for an obscure case.
(defun constant-kwargs (kwargs &optional env)
  (declare (ignore env)) ; constant-form-value :(
  (flet ((fail ()
           (return-from constant-kwargs
             (values nil nil nil nil nil nil))))
    (let (canon binds decls seen-aok aok-p)
      (loop
         (cond ((null kwargs)
                (return (let ((canon (nreverse canon)))
                          (values (mapcar #'car canon) (mapcar #'cdr canon)
                                  (nreverse binds) decls (and seen-aok aok-p)
                                  t))))
               ((and (consp kwargs)
                     (consp (cdr kwargs)))
                (destructuring-bind (key value &rest next) kwargs
                  (if (constantp key)
                      (let ((keyv (eval key)))
                        (unless (symbolp keyv) (fail))
                        (when (and (eq keyv :allow-other-keys)
                                   (not seen-aok))
                          (setf seen-aok t)
                          (if (constantp value)
                              (setf aok-p (eval value))
                              (fail)))
                        (let ((bind (gensym (symbol-name keyv))))
                          (push (list bind value) binds)
                          (if (find keyv canon :key #'car)
                              (push `(ignore ,bind) decls)
                              (push `(,keyv . ,bind) canon))))
                      (fail))
                  (setf kwargs next)))
               (t (fail)))))))

(defun keys-to-initargs (initkeys)
  ;; (:bar :baz) => (#:bar #:baz), (:bar #:bar :baz #:baz)
  ;; Intended for (lambda (#:bar #:baz) (foo :bar #:bar :baz #:baz))
  (loop for k in initkeys
        for param = (make-symbol (symbol-name k))
        collect param into params
        collect k into initargs
        collect param into initargs
        finally (return (values params initargs))))

(defmacro ensure-gethash (key hash-table &optional default)
  (let ((p (gensym "PRESENT-P"))
        (v (gensym "VALUE"))
        (k (gensym "KEY"))
        (h (gensym "HASH-TABLE")))
    `(let ((,k ,key) (,h ,hash-table))
       (multiple-value-bind (,v ,p) (gethash ,k ,h)
         (if ,p
             ,v
             (setf (gethash ,k ,h) ,default))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; I wanted to just use UPDATE-DEPENDENTS, but it turns out that it's not called
;;; on a class that has just had a superclass reinitialized, even though this
;;; changes the effective slots. Annoying.

;;; This is technically nonconformant because it defines a method that can apply
;;; to direct instances of standard-class.

;;; Also it sucks.

(defun add-slots-dependency (class cell function)
  (defmethod compute-slots :after ((class (eql class)))
    (funcall function cell)))

(defun remove-slots-dependency (class cell)
  (declare (ignore cell))
  (let ((method (find-method #'compute-slots '(:after)
                             (list (intern-eql-specializer class))
                             nil)))
    (when method
      (remove-method #'compute-slots method))))


