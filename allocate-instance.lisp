(in-package #:static-gfs)

;;;; Optimizing static calls to ALLOCATE-INSTANCE.

;;; Looks for calls with constant CLASS.
;;; Since the CLHS says ALLOCATE-INSTANCE doesn't have to check initargs,
;;; and it seems that we can assume the class is finalized,
;;; this is much easier than MAKE-INSTANCE.

;;; Constructors take the same arguments as allocate-instance.
;;; Of course, they probably ignore them.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cells

(defstruct (allocator-cell
             (:constructor make-allocator-cell
                           (class &optional function)))
  (function (compute-fallback-allocate-instance class)
   :type function)
  class
  (state :fallback :type (member :optimized :fallback :built-in-class)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tables

;;; Keys are classes, values are cells. One cell per class.

;;; FIXME: should be weak key-or-value.
(defvar *allocator-cells* (make-hash-table :test 'eq))

(declaim (inline class-allocator-cell
                 (setf class-allocator-cell)))
(defun class-allocator-cell (class)
  (gethash class *allocator-cells*))
(defun (setf class-allocator-cell) (value class)
  (setf (gethash class *allocator-cells*) value))

(defmacro ensure-class-allocator-cell
    (class &optional default)
  `(ensure-gethash ,class *allocator-cells* ,default))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cell creation and updating.

(defun ensure-allocator (class)
  (multiple-value-bind (cell present-p)
      (class-allocator-cell class)
    (if present-p
        cell
        (let ((cell (make-allocator-cell class)))
          ;; See long comment in ensure-constructor for why we order like this.
          (prog1 (setf (class-allocator-cell class) cell)
            (update-allocator-cell cell)
            (start-allocator-cell-updates cell))))))

(defun start-allocator-cell-updates (cell)
  (add-dependent (allocator-cell-class cell) cell)
  (add-slots-dependency (allocator-cell-class cell) cell #'update-allocator-cell)
  (add-dependent #'allocate-instance cell))

(defun update-allocator-cell (cell)
  (setf (values (allocator-cell-state cell)
                (allocator-cell-function cell))
        (compute-allocator cell)))

(defun compute-allocator (cell)
  (let* ((class (allocator-cell-class cell))
         (applicable-methods
           (compute-applicable-methods #'allocate-instance (list class))))
    ;; There are three standard methods on allocate-instance.
    (typecase class
      (standard-class
       (if (can-optimize-allocate-instance-p
            applicable-methods (find-class 'standard-class))
           (values :optimized (compute-allocate-standard-instance class applicable-methods))
           (values :fallback (compute-fallback-allocate-instance class))))
      (funcallable-standard-class
       (if (can-optimize-allocate-instance-p
            applicable-methods (find-class 'funcallable-standard-class))
           (values :optimized
                   (compute-allocate-funcallable-standard-instance class applicable-methods))
           (values :fallback (compute-fallback-allocate-instance class))))
      (built-in-class
       (values :built-in-class (compute-allocate-built-in-instance class)))
      (t (values :fallback (compute-fallback-allocate-instance class))))))

(defun can-optimize-allocate-instance-p (applicable-methods base)
  (can-fake-emf-p applicable-methods
                  (find-method #'allocate-instance nil (list base) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Computers of cell functions.

;;; What ALLOCATE-INSTANCE does under the hood is necessarily magical, so we can't
;;; avoid implementation specific stuff of some kind here.

(defun compute-allocate-standard-instance (class applicable-methods)
  (let ((initargs (gensym "INITARGS")))
    (compile
     nil
     `(lambda (&rest ,initargs &key &allow-other-keys)
        (declare (ignorable ,initargs))
        ,(fake-emf applicable-methods
                   (allocate-standard-instance-form class)
                   initargs)))))

(defun allocate-standard-instance-form (class)
  #+clasp
  (let ((size (clos::class-size class)))
    `(core:allocate-new-instance ,class ,size))
  #-(or clasp)
  (progn (warn "Don't know how to optimize ALLOCATE-INSTANCE of a standard class!")
         `(funcall ,(compute-fallback-allocate-instance class))))

(defun compute-allocate-funcallable-standard-instance (class applicable-methods)
  (let ((initargs (gensym "INITARGS")))
    (compile
     nil
     `(lambda (&rest ,initargs &key &allow-other-keys)
        (declare (ignorable ,initargs))
        ,(fake-emf applicable-methods
                   (allocate-funcallable-standard-instance-form class)
                   initargs)))))

(defun allocate-funcallable-standard-instance-form (class)
  #+clasp
  (let ((size (clos::class-size class)))
    `(core:allocate-new-funcallable-instance ,class ,size))
  #-(or clasp)
  (progn (warn "Don't know how to optimize ALLOCATE-INSTANCE of a funcallable standard class!")
         `(funcall ,(compute-fallback-allocate-instance class))))

(defun compute-allocate-built-in-instance (class)
  (lambda (&key &allow-other-keys)
    (error "~s called on ~s, a ~s" 'allocate-instance class 'built-in-class)))

;;; For circularity purposes, it is important that this function not do anything
;;; complicated, including recursively. Immediately returning a closure is a
;;; good example of nothing complicated.
(defun compute-fallback-allocate-instance (class)
  (lambda (&rest initargs &key &allow-other-keys)
    (declare (notinline allocate-instance))
    (apply #'allocate-instance class initargs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Updaters for redefinition.

(defmethod update-dependent ((object (eql #'allocate-instance))
                             (dependent allocator-cell)
                             &rest args)
  (destructuring-bind (&optional key method &rest more) args
    (declare (ignore more))
    (case (allocator-cell-state dependent)
      ((:optimized)
       (when (and (find key '(cl:add-method cl:remove-method))
                  (method-may-specialize-p
                   method (list (allocator-cell-class dependent))))
         (update-allocator-cell dependent)))
      ((:fallback)
       (when (and (eq key 'cl:remove-method)
                  (method-may-specialize-p
                   method (list (allocator-cell-class dependent))))
         (update-allocator-cell dependent))))))

(defmethod update-dependent ((object class)
                             (dependent allocator-cell)
                             &rest initargs)
  (declare (ignore initargs))
  (update-allocator-cell dependent))
