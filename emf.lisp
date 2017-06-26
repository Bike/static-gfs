(in-package #:static-gfs)

;;; We'd like to take a generic function, and, in the situation where it
;;; doesn't need to dispatch (all arguments are constant enough at runtime)
;;; we just call an effective method directly.

;;; But not just any effective method. An effective method where the primary
;;; method has been replaced by some optimized thing. The optimized thing
;;; is only correct if it gets the original arguments to the generic function,
;;; i.e. it is not called by CALL-NEXT-METHOD with arguments. Also the
;;; optimized thing does not care about next methods.

;;; Therefore an EMF is fakeable if there are no :around methods and the most
;;; specific primary method is the one we're replacing.

;;; If we knew whether methods called call-next-method in a complicated way,
;;; we could fake more EMFs.

;;; This is all for standard method combination only, if that wasn't clear.

(defun can-fake-emf-p (applicable-methods primary)
  (let ((seen-primary nil))
    (loop for method in applicable-methods
          for qualifiers = (method-qualifiers method)
          do (cond ((and (null qualifiers) (not seen-primary))
                    (unless (eq method primary)
                      (return nil)))
                   ((eq (first qualifiers) :around)
                    (return nil)))
          finally (return t))))

;;; Second value is whether we need the arguments as a list (i.e. args-name)
;;; KLUDGE, ideally we would just bind the list and then the compiler would
;;; eliminate it if it wasn't used. But that's tricky since consing can be
;;; considered a side effect.
(defun fake-emf (applicable-methods form args-name)
  ;; Assumes it's fakeable. How trusting.
  (let (need-args)
    (values
     `(progn
        ,@(loop for method in applicable-methods
                when (eq (first (method-qualifiers method)) :before)
                  collect `(invoke-method ,method ,args-name nil)
                  and do (setf need-args t))
        (multiple-value-prog1
            ,form
          ,@(loop for method in applicable-methods
                  when (eq (first (method-qualifiers method)) :after)
                    collect `(invoke-method ,method ,args-name nil)
                    and do (setf need-args t))))
     need-args)))

(defun wrap-in-fake-emf (applicable-methods form
                         fake-args-form &optional more-bindings)
  (let ((fake-args (gensym "FAKE-ARGS")))
    (multiple-value-bind (form need-fake-args)
        (fake-emf applicable-methods form fake-args)
      `(let (,@more-bindings
             ,@(if need-fake-args
                   `((,fake-args ,fake-args-form))))
         ,form))))

;;; per MOP, standard method functions should take args and next-methods
;;; as two normal arguments
(defmacro invoke-method (method args &optional next-methods)
  #+clasp
  `(apply (the function ,(method-function method)) ,args ,next-methods ,args)
  #-(or clasp)
  `(funcall (the function ,(method-function method)) ,args ,next-methods))
