(defpackage #:static-gfs
  (:use #:cl #+clasp #:clos #+sbcl #:sb-mop)
  (:export #:ensure-constructor #:find-constructor-cell #:update-constructor-cell
           #:start-constructor-cell-updates #:stop-constructor-cell-updates)
  (:export #:force-constructor-cell-to-full-call)
  (:export #:map-all-constructor-cells)
  (:export #:ensure-allocator #:find-allocator-cell #:update-allocator-cell
           #:start-allocator-cell-updates #:stop-allocator-cell-updates)
  (:export #:force-allocator-cell-to-full-call)
  (:export #:easy-optimized-constructor-form))
