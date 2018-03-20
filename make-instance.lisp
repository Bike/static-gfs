(in-package #:static-gfs)

;;;; Optimizing static calls to MAKE-INSTANCE.

;;; Looks for calls with constant CLASS and constant keyword arguments
;;; (i.e., the keys are constant) and reduces them to non-keyword calls.
;;; Also elides runtime initialization argument checking.

;;; Constructor functions take arguments specified by the call.
;;; E.g.
#||

(defclass foo () ((bar :initarg :bar) (baz :initarg :baz))
  (:default-initargs :baz 7))
(make-instance 'foo :bar 3)
=> (funcall constructor 3)

||#

;;; So the constructor has to take care of default initargs.

;;; FIXME: Not thread safe.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cells

;;; SBCL has its "CTORS" as funcallable instances which can just be mutated directly.
;;; I think going through an accessor to get the function is fine though.
;;; We need the other slots for reacting to redefinitions.
(defstruct (constructor-cell
            (:constructor make-constructor-cell
                (class initkeys allow-other-keys-p &optional function)))
  (function (no-compile-fallback-constructor class initkeys)
   :type function)
  class initkeys
  ;; this boolean is true iff the call has :allow-other-keys true.
  ;; It's rare, but does mean we don't need to do initargs checking.
  allow-other-keys-p
  ;; STATE is only used, anyway, for :wait-finalize. The idea is that the updaters
  ;; (i.e. update-dependent) could be fine-grained and only work based on the state.
  ;; But we don't do that. Knowing the state can be nice for debug, though.
  (state :no-compile-fallback
   :type (member :no-compile-fallback
                 :bad-initargs :wait-finalize :fallback-make-instance
                 :fallback-initialize-instance
                 :fallback-shared-initialize :optimized-shared-initialize)))

(defmethod print-object ((o constructor-cell) s)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (o s :type t)
        (format s "(~a (~{~s~^ ~})~:[~; :ALLOW-OTHER-KEYS~]) state = ~a"
                (class-name (constructor-cell-class o))
                (constructor-cell-initkeys o)
                (constructor-cell-allow-other-keys-p o)
                (constructor-cell-state o)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tables

;;; This is a hash table where the keys are classes, and
;;; the values are hash tables where the keys are lists
;;; of aok-p plus initarg keys and the values are cells.
;;; Don't worry too much, there are accessors.

;;; FIXME: Should be weak-key.
(defvar *constructor-tables* (make-hash-table :test 'eq))

(declaim (inline class-constructor-table (setf class-constructor-table)))
(defun class-constructor-table (class)
  (gethash class *constructor-tables*))
(defun (setf class-constructor-table) (value class)
  (setf (gethash class *constructor-tables*) value))

(declaim (inline make-class-constructor-table))
(defun make-class-constructor-table ()
  ;; EQUAL since our keys are lists of initkeys.
  ;; FIXME: should be weak-value.
  (make-hash-table :test 'equal))

(declaim (inline ensure-class-constructor-table))
(defun ensure-class-constructor-table (class)
  (ensure-gethash class *constructor-tables*
                  (make-class-constructor-table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cell creation and updating.

(defun ensure-constructor (class initkeys aok-p)
  ;; grab an existing cell if there is one; otherwise make a new cell
  ;; and compute a constructor to put in it.
  (let ((key (list* aok-p initkeys))
        (table (ensure-class-constructor-table class)))
    (multiple-value-bind (cell present-p)
        (gethash key table)
      (if present-p
          cell
          ;; Originally this was a simple ensure-gethash.
          ;; However, the possibility of a compiler using this library is one
          ;; that should be supported. And if that's done, it's easy to run into
          ;; circularity issues: the compiler might try to optimize a make-instance,
          ;; thus calling itself, and then that compilation tries to do the same...
          ;; To avoid this we set the cell in place immediately, so that recursive
          ;; calls to ensure a constructor with this pattern will find something.
          ;; So it's important that make-constructor-cell (and setf gethash) can
          ;; not call ensure-constructor, possibly recursively.
          ;; This is what no-compile-fallback-constructor is for.
          (let ((cell (make-constructor-cell class initkeys aok-p)))
            (prog1 (setf (gethash key table) cell)
              (update-constructor-cell cell)
              (start-constructor-cell-updates cell)))))))

(defun start-constructor-cell-updates (cell)
  (add-dependent (constructor-cell-class cell) cell)
  (add-slots-dependency (constructor-cell-class cell) cell #'update-constructor-cell)
  (add-dependent #'make-instance cell)
  (add-dependent #'initialize-instance cell)
  (add-dependent #'shared-initialize cell)
  (add-dependent #'allocate-instance cell))

(defun update-constructor-cell (cell)
  (let ((class (constructor-cell-class cell)))
    (setf (values (constructor-cell-state cell)
                  (constructor-cell-function cell))
          (cond ((not (class-finalized-p class))
                 ;; If a class isn't finalized we can't do much of anything, since we
                 ;; don't know its slots and therefore its initargs.
                 ;; But a class has to be finalized before make-instance can complete,
                 ;; so we set up a constructor that finalizes and then computes.
                 ;; We also don't need to worry about class reinitialization.
                 (compute-finalizer cell))
                (t (compute-constructor-finalized cell))))))

(defun update-constructor-cell-finalized (cell)
  (setf (values (constructor-cell-state cell)
                (constructor-cell-function cell))
        (compute-constructor-finalized cell)))

(defun compute-constructor-finalized (cell)
  (let ((class (constructor-cell-class cell))
        (initkeys (constructor-cell-initkeys cell)))
    (multiple-value-bind (all-keys default-initargs default-initarg-bindings)
        (constructor-default-initargs class initkeys)
      (let ((invalid-keys
             (if (constructor-cell-allow-other-keys-p cell)
                 nil
                 (invalid-keys class all-keys)))
            (applicable-methods
              (compute-applicable-methods #'make-instance (list class))))
        (cond (invalid-keys
               (compute-bad-initargs-constructor class invalid-keys))
              ((can-optimize-constructor-p applicable-methods)
               (compute-optimized-constructor
                class applicable-methods all-keys initkeys
                default-initargs default-initarg-bindings))
              (t (compute-fallback-constructor class initkeys)))))))

(defun can-optimize-constructor-p (applicable-methods)
  (can-fake-emf-p applicable-methods
                  (find-method #'make-instance nil (list (find-class 'class)) nil)))

(defun constructor-default-initargs (class keys)
  ;; Takes the default initargs for keys not in KEYS.
  ;; Returns a list of KEYS + those keys, and then initargs suitable for calls,
  ;; and then bindings for those values (for avoiding multiple evaluation)
  (let ((initargs (remove-if (lambda (initarg) (find initarg keys))
                             (class-default-initargs class)
                             ;; A canonical initarg spec is (key form function)
                             :key #'first)))
    (loop for (key form function) in initargs
          for bind = (make-symbol (symbol-name key))
          do (push key keys)
          collect key into new-initargs
          collect bind into new-initargs
          collect `(,bind (funcall ,function)) into bindings
          finally (return (values keys new-initargs bindings)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Computers of cell functions to call.

(defun compute-optimized-constructor
    (class applicable-methods all-keys initkeys
     default-initargs default-initarg-bindings)
  (multiple-value-bind (new-state lambda-expression)
      (optimized-constructor-form
       class applicable-methods all-keys initkeys
       default-initargs default-initarg-bindings)
    (values new-state (compile nil lambda-expression))))

;; separate for debugging purposes.
(defun optimized-constructor-form
    (class applicable-methods initkeys all-keys
     default-initargs default-initarg-bindings)
  ;; The optimized primary method of make-instance is
  ;; (funcall initializer
  ;;          (allocate-instance ...defaulted-initargs...)
  ;;          ...defaulted-initvalues...)
  ;; The initializer does not know or care about defaulting,
  ;; which is why we pass compute-initializer all-keys instead of initkeys.
  ;; allocate-instance has its own cell caching mechanism
  ;; in allocate-instance.lsp.
  (multiple-value-bind (params initargs) (keys-to-initargs initkeys)
    (multiple-value-bind (state initializer-lambda)
        (compute-initializer class all-keys)
      (let ((default-initvalues
              (loop for (key value) on default-initargs by #'cddr
                    collect value)))
        (values
         state
         `(lambda (,@params)
            ,(wrap-in-fake-emf applicable-methods
                               `(funcall ,initializer-lambda
                                         (allocate-instance
                                          ,class ,@initargs ,@default-initargs)
                                        ,@params ,@default-initvalues)
                               `(list ,class ,@initargs ,@default-initargs)
                               default-initarg-bindings)))))))

(defun compute-fallback-constructor (class initkeys)
  (values :fallback-make-instance
          (compile nil (fallback-constructor-form class initkeys))))

(defun fallback-constructor-form (class initkeys)
  (let* ((params (mapcar (lambda (k) (make-symbol (symbol-name k))) initkeys))
         (kwargs (loop for key in initkeys for param in params
                       collect key collect param)))
    `(lambda (,@params)
       (declare (notinline make-instance))
       (make-instance ,class ,@kwargs))))

(defun compute-bad-initargs-constructor (class invalid-keys)
  (values
   :bad-initargs
   (lambda (&rest values)
     (declare (ignore values))
     (error "Unknown initialization arguments for ~s: ~s"
            (class-name class) invalid-keys))))

(defun compute-finalizer (cell)
  ;; This is somewhat unusual as functions go, because it triggers an update.
  ;; (We can't just force finalization when the class is redefined, because
  ;;  it might have added forward referenced superclasses or something.)
  (let ((class (constructor-cell-class cell)))
    (values
     :wait-finalize
     (lambda (&rest values)
       ;; some other call site or otherwise might have finalized, so don't repeat.
       (unless (class-finalized-p class)
         (finalize-inheritance class))
       ;; ditto lock comment above
       (update-constructor-cell-finalized cell)
       (apply (constructor-cell-function cell) values)))))

;;; For circularity purposes, it is important that this function not do anything
;;; complicated, including recursively. Immediately returning a closure is a
;;; good example of nothing complicated.
;;; The closure is only used temporarily, if at all, so it's ok that it sucks.
(defun no-compile-fallback-constructor (class initkeys)
  (lambda (&rest values)
    (declare (notinline make-instance))
    (apply #'make-instance
           class
           (loop for k in initkeys for v in values
                 collect k collect v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Updaters for redefinition.

;;; Don't do anything if the method is irrelevant.
;;; Methods change relatively rarely, but if it were to
;;; trigger updates in every cell in the system, it could go pretty bad.

;;; If we're in the :wait-finalize state, we don't need to do anything, because
;;; it updates itself.

;;; Otherwise, if a method is relevant, we pretty much always start over.
;;; This is because a method could have changed the valid initargs.
;;; This could be done in a more fine-grained way, but it's probably unimportant.

;;; FIXME?: Maybe we should only invalidate the cache here, and let the next
;;; cell call fix itself up?

(defun update-from-function (cell args)
  (destructuring-bind (key method &rest more) args
    (declare (ignore more))
    ;; args can be an initargs list, so we have to check the key.
    (when (and (find key '(cl:add-method cl:remove-method))
               (method-may-specialize-p
                method (list (constructor-cell-class cell))))
      (unless (eq (constructor-cell-state cell) :wait-finalize)
        (update-constructor-cell-finalized cell)))))

(defmethod update-dependent ((object (eql #'make-instance))
                             (cell constructor-cell)
                             &rest args)
  ;; I actually don't think a programmer can make an initarg valid by having it
  ;; as a &key to make-instance. But I'm not sure. Better safe than sorry.
  (destructuring-bind (&optional key method &rest more) args
    (declare (ignore more))
    (when (and (find key '(cl:add-method cl:remove-method))
               (method-may-specialize-p
                method (list (constructor-cell-class cell))))
      (unless (eq (constructor-cell-state cell) :wait-finalize)
        (update-constructor-cell-finalized cell)))))

(defmethod update-dependent ((object (eql #'initialize-instance))
                             (cell constructor-cell)
                             &rest args)
  (destructuring-bind (&optional key method &rest more) args
    (declare (ignore more))
    (when (and (find key '(cl:add-method cl:remove-method))
               (method-may-specialize-using-classes-p
                method (list (constructor-cell-class cell))))
      (unless (eq (constructor-cell-state cell) :wait-finalize)
        (update-constructor-cell-finalized cell)))))

(defmethod update-dependent ((object (eql #'shared-initialize))
                             (cell constructor-cell)
                             &rest args)
  (destructuring-bind (&optional key method &rest more) args
    (declare (ignore more))
    (when (and (find key '(cl:add-method cl:remove-method))
               (method-may-specialize-using-classes-p
                method (list (constructor-cell-class cell)
                             (find-class 't))))
      (unless (eq (constructor-cell-state cell) :wait-finalize)
        (update-constructor-cell-finalized cell)))))

(defmethod update-dependent ((object (eql #'allocate-instance))
                             (cell constructor-cell)
                             &rest args)
  (destructuring-bind (&optional key method &rest more) args
    (declare (ignore more))
    (when (and (find key '(cl:add-method cl:remove-method))
               (method-may-specialize-p
                method (list (constructor-cell-class cell))))
      (unless (eq (constructor-cell-state cell) :wait-finalize)
        (update-constructor-cell-finalized cell)))))

(defmethod update-dependent ((object class)
                             (dependent constructor-cell)
                             &rest args)
  ;; We're only registered as a dependent if the class is OUR class, so we
  ;; don't need to check any applicability.
  ;; We could check if our state is :wait-finalize but meh.
  (declare (ignore args))
  ;; class may no longer be initialized, start over.
  (update-constructor-cell dependent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Initialization argument checking.

;;; The runtime must have stuff for this, but it's not exposed in MOP and probably
;;; has to be reoriented to work without the values/signaling errors.

(defun invalid-keys (class keys)
  (let ((method-keys
         #+clasp
         (if (slot-boundp class 'clos::valid-initargs)
             (clos::class-valid-initargs class)
             (clos::precompute-valid-initarg-keywords class))
         #-(or clasp)
         (compute-method-initkeys class))
        (slots (class-slots class)))
    (if (eq method-keys t)
        nil
        (loop for key in keys
             unless (or
                     ;; First, is it a slot initarg somewhere?
                     (member key slots :test #'member :key #'slot-definition-initargs)
                     ;; Less likely, it could have been defined as valid by a method.
                     (member key method-keys)
                     ;; once in a blue moon
                     (eq key :allow-other-keys))
               collect key))))

#-(or clasp)
(defun compute-method-initkeys (class)
  (loop for method in (nconc (compute-applicable-methods
                              #'allocate-instance (list class))
                             (compute-applicable-methods
                              #'initialize-instance (list (class-prototype class)))
                             (compute-applicable-methods
                              #'shared-initialize (list (class-prototype class) t)))
        append (multiple-value-bind (keywords aok-p)
                   (function-keywords method)
                 (when aok-p (return t))
                 keywords)))
