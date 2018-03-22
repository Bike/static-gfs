(in-package #:static-gfs)

;;;; Compiler macros for static generic functions.

;;; in a separate file because they violate package locks
;;; and redefine system behavior, which can be a unstable if they're
;;; not perfectly correct.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ALLOCATE-INSTANCE

(define-compiler-macro allocate-instance (&whole form class &rest initargs)
  (if (constantp class)
      (let ((cell (gensym "ALLOCATOR-CELL")))
        `(let ((,cell (load-time-value (ensure-allocator ,class))))
           (funcall (the function (allocator-cell-function ,cell))
                    ,@initargs)))
      form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MAKE-INSTANCE

(define-compiler-macro make-instance (&whole form class
                                             &rest initargs &environment env)
  (if (constantp class)
      (let ((class (eval class)))
        (multiple-value-bind
              (canon-keys canon-values bindings decls aok-p success)
            (constant-kwargs initargs env)
          (if success
              (let ((cell (gensym "CONSTRUCTOR-CELL")))
                `(let ((,cell
                         ,(typecase class
                            #-named-constructor-cells
                            (symbol `(ensure-constructor (find-class ',class)
                                                         '(,@canon-keys)
                                                         ',aok-p))
                            ((or class #+named-constructor-cells symbol)
                             `(load-time-value
                               (ensure-constructor ',class
                                                   '(,@canon-keys)
                                                   ',aok-p)))
                            (t (return-from make-instance form)))))
                   (let (,@bindings)
                     (declare ,@decls)
                     (funcall (the function (constructor-cell-function ,cell))
                              ,@canon-values))))
            form)))
      ;; class is not constant, punt
      form))
