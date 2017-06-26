(in-package #:static-gfs)

;;;; Utilities for debuggers staring at this system in confusion

;;; This system may be difficult to debug because both parts are difficult to debug.
;;; The first part, cells, is essentially a complicated cache and cache invalidation
;;; mechanism. Enough said.
;;; The second part is generating optimized compiled code at runtime - code, moreover,
;;; that deals with the guts of the metaobject protocol.
;;; Therefore I think the existence of this file should be helpful whenever my
;;; mistakes appear.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Constructor cells

;;; Get a cell, or NIL if there isn't one already.
;;; AOK-P is whether there was an :allow-other-keys true in the initargs.
(defun find-constructor-cell (class initkeys &optional aok-p)
  (multiple-value-bind (class-table present-p)
      (class-constructor-table class)
    (and present-p
         (multiple-value-bind (cell present-p)
             (gethash (list* aok-p initkeys) class-table)
           (and present-p cell)))))

;;; Forces a constructor cell into the :fallback-make-instance state, in which it
;;; just calls MAKE-INSTANCE as if the call had not been transformed at all.
;;; The cell will not stay that way if the system changes; see STOP-CELL-UPDATES
(defun force-constructor-cell-to-full-call (cell)
  (setf (values (constructor-cell-state cell)
                (constructor-cell-function cell))
        (compute-fallback-constructor
         (constructor-cell-class cell)
         (constructor-cell-initkeys cell)))
  cell)

;;; Removes a cell as a dependent of various system things. This means it will
;;; only change again if you force it to or restart updates with
;;; START-CONSTRUCTOR-CELL-UPDATES.
;;; It is possible to break things this way, if the function isn't a full call.
(defun stop-constructor-cell-updates (cell)
  (remove-dependent (constructor-cell-class cell) cell)
  (remove-dependent #'make-instance cell)
  (remove-dependent #'initialize-instance cell)
  (remove-dependent #'shared-initialize cell)
  (remove-dependent #'allocate-instance cell))

(defun map-all-constructor-cells (function)
  (maphash
   (lambda (class table)
     (declare (ignore class))
     (maphash
      (lambda (initargs cell)
        (declare (ignore initargs))
        (funcall function cell))
      table))
   *constructor-tables*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Allocator cells

(defun find-allocator-cell (class)
  (values (class-allocator-cell class)))

(defun force-allocator-cell-to-full-call (class)
  (setf (values (allocator-cell-state class)
                (allocator-cell-function class))
        (values :fallback (compute-fallback-allocate-instance class))))

(defun stop-allocator-cell-updates (cell)
  (remove-dependent (allocator-cell-class cell) cell)
  (remove-dependent #'allocate-instance cell))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Code generation

;;; Returns the lambda expression for the optimized MAKE-INSTANCE for a call,
;;; or NIL if it can't be done validly.
;;; Does ignore initarg checking, though.
(defun easy-optimized-constructor-form (class initkeys)
  (let ((applicable-methods
          (compute-applicable-methods #'make-instance (list class))))
    (and (can-optimize-constructor-p applicable-methods)
         (multiple-value-bind (all-keys default-initargs default-initarg-bindings)
             (constructor-default-initargs class initkeys)
           (optimized-constructor-form class applicable-methods initkeys all-keys
                                       default-initargs default-initarg-bindings)))))
