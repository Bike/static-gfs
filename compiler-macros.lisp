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
      (let ((class (eval class))
            (cell (gensym "ALLOCATOR-CELL")))
        `(let ((,cell (load-time-value (ensure-allocator ,class))))
           (funcall (the function (allocator-cell-function ,cell))
                    ,@initargs)))
      form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MAKE-INSTANCE

;;; CLHS 11.1.2.1.2 19 forbids the user from a make-instance method specializing
;;; on any type of symbol, so we can skip that method even if we can't optimize.

(define-compiler-macro make-instance (&whole form class
                                             &rest initargs &environment env)
  (if (constantp class)
      (let ((class (eval class)))
        (typecase class
          (symbol
           ;; this should descend back into this macro, but now with a class.
           (let ((class (find-class class nil env)))
             (if class
                 `(make-instance ,class ,@initargs)
                 form)))
          (class
           (multiple-value-bind
                 (canon-keys canon-values bindings decls aok-p success)
               (constant-kwargs initargs env)
             (if success
                 (let ((cell (gensym "CONSTRUCTOR-CELL")))
                   `(let ((,cell (load-time-value
                                  (ensure-constructor
                                   ,class
                                   '(,@canon-keys)
                                   ',aok-p))))
                      (let (,@bindings)
                        (declare ,@decls)
                        (funcall (the function (constructor-cell-function ,cell))
                                 ,@canon-values))))
                 form)))
          (t ;; dunno what it is, punt
           form)))
      ;; class is not constant, punt
      form))
