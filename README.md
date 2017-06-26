Static generic functions
========================

Some generic function calls are "static" in the sense that all of the arguments dispatched on are constant. How common this is varies by GF; it's almost all calls of `make-instance`, but almost none of `print-object`.

Anyway, if this is the case we can skip dispatch and call an effective method directly - possibly an optimized effective method at that. We still need to react to changes to the generic function. So we can set up static calls something like

    `(let ((cell (load-time-value
                   (ensure-cell ',arguments))))
       (funcall (cell-function cell) ,@arguments))

`ensure-cell` returns an object that stores, among other things, the optimized effective method or `cell-function`. The cell is additionally stored in an internal table, so that other call sites with the same arguments use the same cell.

The cell is set up as a dependent (in the MOP sense) of the generic function. When a method is added or removed, the cell function must be recomputed. Dependent maintenance allows this to happen pretty easily.

This is based on the same idea as SBCL's "ctor" functionality, but is intended to be portable (ideally, conformant, but alas) and comprehensible.

I have tested this but not extensively. Break it.

Interface
---------

There are compiler macros on `cl:allocate-instance` and `cl:make-instance`.

If you do not want a call inlined, just `(declare notinline)` to suppress the compiler-macroexpansion.

If you want more fine grained control, or more likely, debugging:

* `ensure-constructor` is the `ensure-cell` for `make-instance`. It takes components of the call it needs: a class, and the keywords from the initarg list. (Also whether there was `:allow-other-keys`, but that's very rarely needed). You can call it to ensure that a cell is present.
* `find-constructor-cell` is like `ensure-constructor` but doesn't make the cell if there isn't one already.
* `update-constructor-cell` causes its cell argument to be updated for the present situation, e.g. it recomputes the function.
* `force-constructor-cell-to-full-call` is intended for debugging, and does what it says: all optimization on the cell is inhibited. But it can still be recomputed, so watch that.
* `start-constructor-cell-updates` and `stop-constructor-cell-updates` respectively add or remove a cell as a dependent of things it needs to be a dependent of. The latter is for debugging.
* `map-all-constructor-cells` takes a function and applies it to all existing cells. You could do some of the former, for instance, to force the system into a stable state if something has gone wrong.
* Most of the above have an `-allocator-` version as well.
* `easy-optimized-constructor-form` returns the lambda expression that will be compiled into a constructor function for the given call type. Try it! It's huge! This is a debug interface also.

Fake effective method functions
-------------------------------

While it would be possible, with some effort, to have the `cell-function` just be a compiled form of the effective method, what's really helpful is to have methods be optimized for the constant arguments as well. This is supported here with the "fake EMF" functions in emf.lisp. Generally, they take a form representing the optimized primary method, and return a form like the effective method but with the primary method call replaced by the form. This lets us support `:after` methods and so on.

Unfortunately this is limited, because the Common Lisp `call\-next\-method` function can be provided arguments. In this case the optimized primary could be called with arguments not available at optimization time. Therefore we can't support `:around` methods (see TODO).

allocate-instance
-----------------

`allocate-instance` is set up as explained. Cells are dependents of `allocate-instance` and also of the class argument. The optimized primary method does some implementation-defined magic.

make-instance
-------------

`make-instance` is most of the code here. The main draw is that initarg validity checks can be done at optimization time rather than run time, which in practice seems to make `make-instance` two to seven times faster. Cells are dependents of what determine initargs, per CLHS 7.1.2 the class argument, `make-instance`, `initialize-instance`, `shared-initialize`, and `allocate-instance`. Initarg defaulting is also done at optimization time.

The optimized primary method of `make-instance` calls `allocate-instance` (relying on that function to be optimized with cells as well) and then implements the behavior of `initialize-instance` and `shared-initialize`. Those functions do not have static calls themselves, and it would be a bit wasteful to rely on cells (particularly, `shared-initialize` cells would differ by initargs, meaning that `initialize-instance` cells would as well even though all `initialize-instance` does is call `shared-initialize`).

Implementation specificity
--------------------------

Unfortunately, this cannot be done entirely implementation agnostically, but the exceptions are actually pretty minor:

* The package definition USEs wherever the implementation puts its MOP. (No closer-mop as I tried to implement this to be usable as a primitive extension.)
* Clasp currently has incompatible (and possibly MOP-nonconformant) lambda lists for method functions. But that may change soon.
* To save some duplication we can try to hook into the implementation's initarg checker. But we can do this portably if we have a lambda list parser.
* Implementations seem to vary on what standard methods specialize on. For example, in SBCL the primary standard methods for `shared-initialize` and `initialize-instance` are specialized on `sb-pcl::slot-object` rather than `standard-object`.
* The optimized body for `allocate-instance` is magic.

Overall, it should load and work on Clasp, and load on SBCL if you do something about the package lock. Porting doesn't seem hard, other than possibly the `allocate-instance` body.

TODO/not done
-------------

* Inline `slot-value-using-class` and `slot-boundp-using-class`. Probably with cells, but maybe shared. See comment in shared-initialize.lisp.
* We're thread unsafe in that redefining an instance maker/initializer at the same time a cell is updating would probably cause strange and terrible issues.
* Several hash tables should use weak pointers.
* Optimize other functions like this, if there are any.
* Possibly delay more. As is, we recompute functions immediately when any methods change or classes are reinitialized. We could delay recomputation to `make-instance` or whatever actually being called. I think this is what SBCL does. I don't like that it makes the first call to `make-instance` much slower, but repeatedly redefining a method during development could potentially cause slowdown our way. (We do this already, though, if the class is not finalized.)
