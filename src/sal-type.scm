;;
;; SAL 3.1, Copyright (C) 2006, 2011, SRI International.  All Rights Reserved.
;;
;; SAL is free software; you can redistribute it and/or 
;; modify it under the terms of the GNU General Public License 
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of 
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
;; GNU General Public License for more details. 
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software 
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. 
;;

(module sal-type
        (include "sal.sch")
        (include "iterators.sch")
        (include "sal-ast-table.sch")
        (import sal-environment sal-expression gmp-scheme sal-module 
                sal-expression sal-ast-instantiate sal-ast-eq sal-ast-list
                sal-ast-simplify sal-dnf sal-ast-for-each unique-names
                sal-ast-env sal-decls sal-bound sal-expr-evaluator sal-pp)
        (export (sal-type-name/type-param? type-name)
                (sal-type-name/definition type-name)
                (sal-type/expand-if-type-name type)
                (sal-type-name/recursive? type-name)
                (sal-function-type/arity type)
                (sal-type/finite? type)
                (sal-type/finite-core? type expr-evaluator)
                (sal-type/non-finiteness-reason)
                (sal-type/array? type)
                (sal-type/record? type)
                (sal-type/tuple? type)
                (sal-type/function? type)
                (sal-type/scalar? type)
                (sal-type/scalar-set? type)
                (sal-type/ring-set? type)
                (sal-type/boolean? type)
                (sal-type/predicate? type)
                (sal-type/for-each-used-type proc type)
                (sal-type/for-each proc type)
                (sal-type/find pred? ast)
                (sal-type/first-order? type)
                (sal-type/negative-occurrence? type1 type2)
                (sal-type/subtype-of? type super-type)
                (sal-type/bounded-subtype? type)
                (sal-type/number? type)
                (sal-type/real? type)
                (sal-type/integer? type)
                (sal-type/natural? type)                
                (sal-subtype/normalize type)
                (sal-subtype/immediate-super-type type)
                (sal-type->bounded-subtype type)
                (sal-number-subtype/bounded? type)
                (sal-type/cast type class)
                (sal-tuple-type/types type)
                (sal-record-type/fields type)
                (sal-record-type/sorted-fields type)
                (sal-field-list->sal-type-list field-list)
                (sal-scalar-type/elements type)
                (sal-function-type/domain type)
                (sal-function-type/domain-types type)
                (sal-function-type/range type)
                (sal-bounded-subtype/lower type)
                (sal-bounded-subtype/upper type)
                (sal-type->type-list type)
                (sign-iterator-error ast msg . args)
                (sal-type/make-iterator type)
                (sal-type/make-iterator-core type expr-evaluator)
                (sal-constructor/number-of-elements-core constructor-name expr-evaluator)
                (sal-constructor/number-of-elements constructor-name)
                (sal-constructor/number-of-elements-as-integer constructor-name)
                (sal-type/number-of-elements-core type expr-evaluator)
                (sal-type/number-of-elements type)
                (sal-type/number-of-elements-as-integer type)
                (sal-type/value-idx type value)
                (sal-type/super-type type)
                (sal-type/equivalent? type1 type2)
                (sal-type/equivalent-core? type1 type2 env)
                (sal-type/union type1 type2)
                (sal-type/union-core type1 type2 proc-child)
                (sal-tuple-type/element-type type pos)
                (sal-record-type/element-type type field-id)
                (make-sal-subrange lower upper . place-provider))
        )
                
(define-api (sal-type-name/type-param? (type-name <sal-type-name>))
  (ensure ((type-name <sal-type-name>))
    (instance-of? (slot-value type-name :decl) <sal-type-param-decl>)))

(define-generic (sal-type-decl/definition decl))
(define-method (sal-type-decl/definition (decl <sal-type-decl>))
  (slot-value decl :type))
(define-method (sal-type-decl/definition (decl <sal-type-param-decl>))
  #f)

(define-generic (sal-type-name/definition type-name)
  :doc "Returns the definition of a type name. Returns @code{#f} is the type is uninterpreted.")
(define-method (sal-type-name/definition (type-name <sal-type-name>))
  (sal-type-decl/definition (slot-value type-name :decl)))
;; Remark I do not memoize the following method because:
;;  1) It will create a mutual dependency (and infinite loop) between 
;;     sal-type-name/definition and sal-ast/equivalent?
;;  2) Type definitions are usually small, and it is not worth to cache them
(define-method (sal-type-name/definition (type-name <sal-qualified-type-name>))
  (let ((type-def (sal-type-decl/definition (slot-value type-name :decl))))
    (and type-def
         (sal-ast/instantiate type-def (slot-value type-name :actuals)))))

(define-generic (sal-type/expand-if-type-name type)
  :doc "Expand @code{type} if it is a type name.")
(define-method (sal-type/expand-if-type-name (type <sal-type>))
  type)
(define-method (sal-type/expand-if-type-name (type <sal-type-name>))
  (let ((body (sal-type-name/definition type)))
    (if body
      (sal-type/expand-if-type-name body)
      type)))

(define-api (sal-type-name/recursive? type)
  :doc "Returns @code{#t} if @code{type} is a recursive type."
  (ensure ((type <sal-type-name>))
    (sal-decl/recursive? (slot-value type :decl))))

(define-api (sal-function-type/arity type)
  :doc "Returns the arity of the function type @code{type}."
  [assert (type) (sal-type/function? type)]
  (let ((domain (sal-function-type/domain type)))
    (if (sal-type/domain-tuple? domain)
      (length (sal-tuple-type/types domain))
      1)))

(define-generic (finite-type? type expr-evaluator))

(define *non-finiteness-reason* '())

(define (sal-type/non-finiteness-reason)
  (if (null? *non-finiteness-reason*)
    ""
    (let* ((prefix "  Reason: ")
           (prefix-len (string-length prefix)))
      (let loop ((reasons (cdr *non-finiteness-reason*))
                 (result (string-append prefix (car *non-finiteness-reason*))))
        (if (null? reasons)
          result
          (loop (cdr reasons) (string-append result "\n" (make-string prefix-len) (car reasons))))))))

(define-api (sal-type/finite? type)
  :doc "Returns @code{#t} if @code{type} has a finite number of elements."
  :examples '((sal-type/finite? (sal/type "nat"))
              (sal-type/finite? (sal/type "[0..3]")))
  (sal-type/finite-core? type sal-expr/evaluate))

(define (sal-type/finite-core? type expr-evaluator)
  (set! *non-finiteness-reason* '())
  (finite-type? type expr-evaluator))

(define-method (finite-type? (type <sal-function-type>) (expr-evaluator <primitive>))
  (and (finite-type? (slot-value type :domain) expr-evaluator)
       (finite-type? (slot-value type :range) expr-evaluator)))

(define-method (finite-type? (type <sal-tuple-type>) (expr-evaluator <primitive>))
  (for-all (cut finite-type? <> expr-evaluator) (slot-value type :types)))

(define-method (finite-type? (type <sal-record-type>) (expr-evaluator <primitive>))
  (for-all (lambda (field)
             (finite-type? (slot-value field :type) expr-evaluator))
           (slot-value type :fields)))

(define-method (finite-type? (type <sal-state-type>) (expr-evaluator <primitive>))
  (finite-type? (sal-module/type (slot-value type :module)) expr-evaluator))

(define-method (finite-type? (type <sal-scalar-type>) (expr-evaluator <primitive>))
  #t)

(define-method (finite-type? (type <sal-data-type>) (expr-evaluator <primitive>))
  (for-all (lambda (constructor-name)
             (for-all (lambda (accessor-name)
                        (finite-type? (sal-name-expr/accessor-type accessor-name) expr-evaluator))
                      (sal-name-expr/constructor-accessors constructor-name)))
           (slot-value type :constructors)))

(define-method (finite-type? (type <sal-bounded-subtype>) (expr-evaluator <primitive>))
  #t)
        
(define (evaluate-sub-type-predicate! type expr-evaluator)
  (try 
   (set-slot-value! type :expr (expr-evaluator (slot-value type :expr)))
   (lambda (escape proc msg obj)
     (escape #unspecified))))

(define-method (finite-type? (type <sal-subtype>) (expr-evaluator <primitive>))
  (evaluate-sub-type-predicate! type expr-evaluator)
  (cond
   ((sal-type/integer? type)
    (cond 
     ((sal-number-subtype/bounded? type) #t)
     (else 
      (push! (format-with-location type "Type is not a bounded subtype of integer, or it was not possible to determine the bound.") 
             *non-finiteness-reason*)
      #f)))
   (else
    (finite-type? (sal-function-type/domain (sal-expr/type (slot-value type :expr))) expr-evaluator))))

(define-method (finite-type? (type <sal-type-name>) (expr-evaluator <primitive>))
  (cond
   ((sal-decl/recursive? (slot-value type :decl))
    (push! (format-with-location type "Type is recursive.") *non-finiteness-reason*)
    #f)
   (else
    (let ((def (sal-type-name/definition type)))
      (cond
       (def
        (finite-type? def expr-evaluator))
       (else
        (push! (format-with-location type "Type is anonymous (i.e., it does not have a body).") *non-finiteness-reason*)
        #f))))))

(define-generic (sal-type/kind? type class))
(define-method (sal-type/kind? (type <sal-type>) (class <primitive>))
  (instance-of? type class))
(define-method (sal-type/kind? (type <sal-type-name>) (class <primitive>))
  (cond
   ((sal-type-name/definition type) =>
    (cut sal-type/kind? <> class))
   (else
    (call-next-method))))
(define-method (sal-type/kind? (type <sal-subtype>) (class <primitive>))
  (or (instance-of? type class)
      (sal-type/kind? (sal-subtype/immediate-super-type type) class)))
 
(define-generic (sal-type/cast type class))
(define-method (sal-type/cast (type <sal-type>) (class <primitive>))
  (if (instance-of? type class)
    type
    (internal-error)))
(define-method (sal-type/cast (type <sal-type-name>) (class <primitive>))
  (cond
   ((instance-of? type class)
    type)
   ((sal-type-name/definition type) =>
    (cut sal-type/cast <> class))
   (else
    (internal-error))))
(define-method (sal-type/cast (type <sal-subtype>) (class <primitive>))
  (cond
   ((instance-of? type class)
    type)
   (else
    (sal-type/cast (sal-subtype/immediate-super-type type) class))))


(define-api (sal-type/array? (type <sal-type>))
  (sal-type/kind? type <sal-array-type>))
(define-api (sal-type/record? (type <sal-type>))
  (sal-type/kind? type <sal-record-type>))
(define-api (sal-type/tuple? (type <sal-type>))
  (sal-type/kind? type <sal-tuple-type>))
(define-api (sal-type/domain-tuple? (type <sal-type>))
  (sal-type/kind? type <sal-domain-tuple-type>))
(define-api (sal-type/function? (type <sal-type>))
  (sal-type/kind? type <sal-function-type>))
(define-api (sal-type/scalar? (type <sal-type>))
  (sal-type/kind? type <sal-scalar-type>))
(define-api (sal-type/scalar-set? type)
  (sal-type/kind? type <sal-scalar-set-type>))
(define-api (sal-type/ring-set? type)
  (sal-type/kind? type <sal-ring-set-type>))

(define-generic (sal-type/boolean? type)
  :doc "Returns @code{#t} if @code{type} is a boolean type.")
(define-method (sal-type/boolean? (type <sal-type>))
  (sal-type/equivalent? type (make-sal-builtin-name <sal-bool-type> type)))
(define-method (sal-type/boolean? (type <sal-bool-type>)) 
  #t)

(define-api (sal-type/predicate? (type <sal-type>))
  (and (sal-type/function? type)
       (sal-type/boolean? (sal-function-type/range type))))

(define-generic (sal-type/for-each-used-type proc type)
  :doc "@code{proc} is a procedure, and receives on argument. @code{proc} is called for each type directly used by the given type, where 'use' means: type names use their definition, subtypes use their immediate super types, tuples use their types, records use the types of each field, functions use the domain and range type, and datatypes use the types of each constructor argument.")
(define-method (sal-type/for-each-used-type (proc <primitive>) (type <sal-type>))
  ;; do nothing
  #unspecified)
(define-method (sal-type/for-each-used-type (proc <primitive>) (type <sal-type-name>))
  (let ((def (sal-type-name/definition type)))
    (when def
      (proc def))))
(define-method (sal-type/for-each-used-type (proc <primitive>) (type <sal-subtype>))
  (proc (sal-subtype/immediate-super-type type)))
(define-method (sal-type/for-each-used-type (proc <primitive>) (type <sal-tuple-type>))
  (for-each proc (slot-value type :types)))
(define-method (sal-type/for-each-used-type (proc <primitive>) (type <sal-record-type>))
  (for-each (lambda (field)
              (proc (slot-value field :type)))
            (slot-value type :fields)))
(define-method (sal-type/for-each-used-type (proc <primitive>) (type <sal-function-type>))
  (proc (slot-value type :domain))
  (proc (slot-value type :range)))
(define-method (sal-type/for-each-used-type (proc <primitive>) (type <sal-data-type>))
  (for-each (lambda (constructor)
              (let ((accessor-list (sal-name-expr/constructor-accessors constructor)))
                (for-each (lambda (accessor)
                            (proc (sal-name-expr/accessor-type accessor)))
                          accessor-list)))
            (slot-value type :constructors)))


;; auxiliary global variable used in sal-type/for-each
(define *sal-type-for-each-rec-types* #unspecified)

(define-api (sal-type/for-each proc type)
  :doc "Traverse the types used directly and indirectly by @code{type}. See @code{sal-type/for-each-used-type}."
  (dlet ((*sal-type-for-each-rec-types* '()))
    (let loop ((type type))
;; BD: misuse of the when macro --> different result on bigloo2.6 and bigloo2.8
      (when (proc type)
        (cond
         ((instance-of? type <sal-type-name>)
          (ensure ((type <sal-type-name>))
            (when (not (exists (cut sal-ast/equivalent? type <>) *sal-type-for-each-rec-types*))
              (dlet ((*sal-type-for-each-rec-types* (if (sal-type-name/recursive? type)
                                                      (cons type *sal-type-for-each-rec-types*)
                                                      *sal-type-for-each-rec-types*)))
                (sal-type/for-each-used-type loop type)))))
         (else
          (sal-type/for-each-used-type loop type)))))))
              
(define *sal-type/find* (for-each->find sal-type/for-each))

(define-api (sal-type/find pred? ast)
  :doc "Search for a used type which satisfies @code{pred?}. See @code{sal-type/for-each-used-type}."
  (*sal-type/find* pred? ast))

(define-api (sal-type/first-order? type)
  :doc "Returns @code{#t} if @code{type} is a first order type."
  (not (sal-type/find (lambda (n)
                        (instance-of? n <sal-function-type>))
                      type)))

(define-api (sal-type/negative-occurrence? type1 type2)
  :doc "Returns @code{#t} is @code{type1} has a negative occurrence in @code{type2}."
  (bind-exit (exit)
    (sal-type/for-each (lambda (t1)
                         (cond
                          ((instance-of? t1 <sal-function-type>)
                           (when (or (sal-type/find (lambda (t2) (sal-ast/equivalent? type1 t2))
                                                    (slot-value t1 :domain))
                                     (sal-type/negative-occurrence? type1 (slot-value t1 :range)))
                             (exit #t))                           
                           #f)
                          (else
                           #t)))
                       type2)
    #f))

(define-generic (sal-type/subtype-of? type super-type)
  :doc "Returns @code{#t} if @code{type} is a subtype of @code{super-type}.")
(define-method (sal-type/subtype-of? (type <sal-type>) (super-type <sal-type>))
  (sal-ast/equivalent? type super-type))
(define-method (sal-type/subtype-of? (type <sal-subtype>) (super-type <sal-type>))
  (trace 'type "checking if the subtype at ~a is a subtype of the type located at ~a" 
         (format-with-location type "") (format-with-location super-type ""))
  (or (call-next-method)
      (sal-type/subtype-of? (sal-subtype/immediate-super-type type) super-type)))
(define-method (sal-type/subtype-of? (type <sal-type-name>) (super-type <sal-type>))
  (trace 'type "checking if the type-name at ~a is a subtype of the type located at ~a" 
         (format-with-location type "") (format-with-location super-type ""))
  (or (call-next-method)
      (let ((def (sal-type-name/definition type)))
        (and def (sal-type/subtype-of? def super-type)))))

(define (sal-type/bounded-subtype? type)
  (let ((type (sal-type/expand-if-type-name type)))
    (or (instance-of? type <sal-bounded-subtype>)
        (and (sal-type/integer? type) (sal-number-subtype/bounded? type)))))

(define-generic (sal-type/number? type)
  :doc "Returns @code{#t} if @code{type} is a numeric type.")
(define-method (sal-type/number? (type <sal-number-type>)) #t)
(define-method (sal-type/number? (type <sal-type>))
  (sal-type/subtype-of? type (make-sal-builtin-name <sal-number-type> type)))

(define-api (sal-subtype/var-decl-and-body (type <sal-subtype>))
  (ensure ((type <sal-subtype>))
    (tlet* ((normalized-subtype <sal-subtype> (sal-subtype/normalize type))
            (pred <sal-lambda> (slot-value normalized-subtype :expr)))
      [assert (pred) (instance-of? pred <sal-lambda>)]
      [assert (pred) (= (length (slot-value pred :local-decls)) 1)]
      (let* ((var-decl (car (slot-value pred :local-decls)))
             (body (slot-value pred :expr))
             (dnf (sal-expr->dnf body)))
        (values var-decl dnf)))))
          
(define (min-aux curr-lower new-lower)
  (if curr-lower
    (make-sal-builtin-application <sal-min> #unspecified
                                  curr-lower new-lower)
    new-lower))

(define (max-aux curr-upper new-upper)
  (if curr-upper
    (make-sal-builtin-application <sal-max> #unspecified
                                  curr-upper new-upper)
    new-upper))

(define (sal-number-subtype/compute-bounds type)
  [assert (type) (sal-type/number? type)]
  (multiple-value-bind
      (var-decl body-dnf)
      (sal-subtype/var-decl-and-body (sal-type/expand-if-type-name type))
    ;; (breakpoint "bounds" (var-decl body-dnf) #t)
    (let ((lower #f)
          (upper #f))
      (sal-dnf/for-each-cube
       body-dnf
       (lambda (cube)
         (let* ((cube-lower #f)
                (cube-upper #f))
           (sal-cube/for-each-literal
            cube
            (lambda (literal)
              (cond 
               ((sal-expr/lower-bound literal var-decl)
                =>
                (lambda (lower)
                  (set! cube-lower (max-aux cube-lower lower))))
               ((sal-expr/upper-bound literal var-decl)
                =>
                (lambda (upper)
                  (set! cube-upper (min-aux cube-upper upper)))))))
           (unless (and cube-lower cube-upper)
             ;; it was not possible to compute the bounds
             (values #f #f))
           (set! lower (min-aux lower cube-lower))
           (set! upper (max-aux upper cube-upper)))))
      (values lower upper))))

(define-generic (sal-type->bounded-subtype type))

(define-method (sal-type->bounded-subtype (type <sal-type-name>))
  (trace 'type "converting to bounded subtype the type-name at ~a" (format-with-location type ""))
  (let ((body (sal-type-name/definition type)))
    (and body (sal-type->bounded-subtype body))))

(define-method (sal-type->bounded-subtype (type <sal-subtype>))
  (trace 'type "converting to bounded subtype the subtype at ~a" (format-with-location type ""))
  [assert (type) (sal-type/number? type)]
  (multiple-value-bind
      (lower upper)
      (sal-number-subtype/compute-bounds type)
    (and lower upper
         (tlet ((result <sal-bounded-subtype> (change-ast-class type <sal-bounded-subtype>)))
           (set-slot-value! result :lower lower)
           (set-slot-value! result :upper upper)
           result))))

(memoize-sal-ast-method (sal-type->bounded-subtype (type <sal-subtype>)))

(define-method (sal-type->bounded-subtype (type <sal-bounded-subtype>))
  type)

(define (sal-number-subtype/bounded? type)
  (sal-type->bounded-subtype type))

(define (sal-type/check-subtype-pred type pred?)
  (and (instance-of? type <sal-subtype>)
       (multiple-value-bind
           (var-decl body-dnf)
           (sal-subtype/var-decl-and-body type)
         (bind-exit (exit)
           (sal-dnf/for-each-cube
            body-dnf
            (lambda (cube)
              (unless (bind-exit (cube-exit)
                        (sal-cube/for-each-literal
                         cube
                         (lambda (literal)
                           (when (pred? var-decl literal)
                             (cube-exit #t))))
                        #f)
                (exit #f))))
           #t))))

(define (pred-of? var-decl literal proc)
  (and-instance-of? ((literal <sal-application>))
    (let ((fun (slot-value literal :fun))
          (arg-list (sal-application/force-argument-list literal 1)))
      (and (proc fun)
           (= (length arg-list) 1)
           (let ((arg (car arg-list)))
             (and-instance-of? ((arg <sal-name-expr>))
               (eq? var-decl (slot-value arg :decl))))))))
  
(define (real-pred? var-decl literal)
  (and-instance-of? ((literal <sal-real-pred>))
    (let ((arg (slot-value literal :arg)))
      (and-instance-of? ((arg <sal-name-expr>))
        (eq? var-decl (slot-value arg :decl))))))

(define (int-pred? var-decl literal)
  (and-instance-of? ((literal <sal-int-pred>))
    (let ((arg (slot-value literal :arg)))
      (and-instance-of? ((arg <sal-name-expr>))
        (eq? var-decl (slot-value arg :decl))))))

(define (nat-pred? var-decl literal)
  (cond
   ((sal-expr/lower-bound literal var-decl)
    => 
    (lambda (lower)
      (let ((lower (sal-ast/simplify lower)))
        (and-instance-of? ((lower <sal-numeral>))
          (>=mpq (slot-value lower :num) *mpq-zero*)))))
   (else
    #f)))

(define-generic (sal-type/real? type)
  :doc "Returns @code{#t} if @code{type} is the REAL type.")
(define-method (sal-type/real? (type <sal-real-type>)) #t)
(define-method (sal-type/real? (type <sal-type>))
  (trace 'type "checking if type at ~a is a real" (format-with-location type ""))
  (or (sal-type/subtype-of? type (make-sal-builtin-name <sal-real-type> type))
      (sal-type/check-subtype-pred (sal-type/expand-if-type-name type)
                                   real-pred?)))

(define-generic (sal-type/integer? type)
  :doc "Returns @code{#t} if @code{type} is the INTEGER type.")
(define-method (sal-type/integer? (type <sal-int-type>)) #t)
(define-method (sal-type/integer? (type <sal-type>))
  (trace 'type "checking if type at ~a is an integer" (format-with-location type ""))
  (or (sal-type/subtype-of? type (make-sal-builtin-name <sal-int-type> type))
      (sal-type/check-subtype-pred (sal-type/expand-if-type-name type)
                                   int-pred?)))

(define-generic (sal-type/natural? type)
  :doc "Returns @code{#t} if @code{type} is the NATURAL type.")
(define-method (sal-type/natural? (type <sal-nat-type>)) #t)
(define-method (sal-type/natural? (type <sal-type>))
  (trace 'type "checking if type at ~a is a natural" (format-with-location type ""))
  (or (sal-type/subtype-of? type (make-sal-builtin-name <sal-nat-type> type))
      (and (sal-type/integer? type)
           (sal-type/check-subtype-pred (sal-type/expand-if-type-name type)
                                        nat-pred?))))

(define-api (sal-tuple-type/types type)
  :doc "Return the list of types of a tuple type."
  [assert (type) (sal-type/tuple? type)]
  (slot-value (sal-type/cast type <sal-tuple-type>) :types))
  
(define-api (sal-record-type/fields type)
  :doc "Returns the list of fields of a record type."
  [assert (type) (sal-type/record? type)]
  (slot-value (sal-type/cast type <sal-record-type>) :fields))

(define-api (sal-record-type/sorted-fields record-type)
  :doc "Returns the list of fields of a record type sorted by name."
  (let ((fields (sal-record-type/fields record-type)))
    (sort fields (lambda (field1 field2)
                   (string<? (symbol->string (sal-identifier/name (slot-value field1 :id)))
                             (symbol->string (sal-identifier/name (slot-value field2 :id))))))))

(define-api (sal-field-list->sal-type-list field-list)
  :doc "Converts a list of fields into a list o types."
  (map (lambda (field)
         (ensure ((field <sal-field>))
           (slot-value field :type)))
       field-list))

(define-api (sal-scalar-type/elements type)
  :doc "Returns the scalar elements of a scalar type."
  :examples '((sal-scalar-type/elements (sal/type "bool")))
  [assert (type) (sal-type/scalar? type)]
  (tlet ((scalar-type <sal-scalar-type> (sal-type/cast type <sal-scalar-type>)))
    (slot-value scalar-type :scalar-elements)))

(define-api (sal-function-type/domain type)
  :doc "Returns the domain of the given function type. See @code{sal-function-type/range}."
  :examples '((sal-function-type/domain (sal/type "[nat -> bool]")))
  [assert (type) (sal-type/function? type)]
  (tlet ((func-type <sal-function-type> (sal-type/cast type <sal-function-type>)))
    (slot-value func-type :domain)))

(define-api (sal-function-type/domain-types type)
  :doc "Returns the domain of the given function type as a list of types (one for each argument)."
  :examples '((sal-function-type/domain-types (sal/type "[[nat, int] -> bool]")))
  (let ((domain (sal-function-type/domain type)))
    (cond
     ((= (sal-function-type/arity type) 1)
      (list domain))
     (else 
      (ensure ((domain <sal-tuple-type>))
        (slot-value domain :types))))))

(define-api (sal-function-type/range type)
  :doc "Returns the range of the given function type. See @code{sal-function-type/domain}."
  :examples '((sal-function-type/range (sal/type "[nat -> bool]")))
  [assert (type) (sal-type/function? type)]
  (tlet ((type <sal-function-type> (sal-type/cast type <sal-function-type>)))
    (slot-value type :range)))

(define-api (sal-bounded-subtype/lower type)
  :doc "Returns the lower bound of a bounded subtype."
  :examples '((sal-bounded-subtype/lower (sal/type "[0..2]")))
  [assert (type) (sal-type/bounded-subtype? type)]
  (tlet ((type <sal-bounded-subtype> (sal-type->bounded-subtype (sal-type/expand-if-type-name type))))
    (slot-value type :lower)))

(define-api (sal-bounded-subtype/upper type)
  :doc "Returns the upper bound of a bounded subtype."
  :examples '((sal-bounded-subtype/upper (sal/type "[0..2]")))
  [assert (type) (sal-type/bounded-subtype? type)]
  (tlet ((type <sal-bounded-subtype> (sal-type->bounded-subtype (sal-type/expand-if-type-name type))))
    (slot-value type :upper)))

(define-generic (sal-type->type-list type))

(define-method (sal-type->type-list (type <sal-domain-tuple-type>))
  (slot-value type :types))

(define-method (sal-type->type-list (type <sal-type>))
  (list type))

(register-app-error! 'type-iterator)

(define (sign-iterator-error ast msg . args)
  (error 'type-iterator (apply format-with-location ast msg args) #unspecified))

(define-api (sal-type/make-iterator type)
  :doc "Create an iterator for the given type. @code{type} must be a finite type."
  :examples '((begin
                (define it (sal-type/make-iterator (sal/type "[# idx : [0..2], flag : bool #]")))
                (iterator->list it)))
  (trace 'type "making iterator for type ~a" (format-with-location type ""))
  (sal-type/make-iterator-core type sal-expr/evaluate))

(define-generic (sal-type/make-iterator-core type expr-evaluator))

(define-method (sal-type/make-iterator-core (type <sal-type-name>) (expr-evaluator <primitive>))
  (unless (sal-type/finite-core? type expr-evaluator)
    (sign-iterator-error type "Type cannot be iterated, it was not possible to verify whether the type is finite or not."))
  (sal-type/make-iterator-core (sal-type-name/definition type) expr-evaluator))

(define (check-subrange lower upper place)
  (ensure ((lower <sal-numeral>)
           (upper <sal-numeral>))
;; bug: (unless #t ..) gives #f on biglo2.8 and #unspecified in 2.6
    (if (<=mpq (slot-value lower :num) (slot-value upper :num))
	#t
	(sign-source-error place "Invalid subrange, lower bound is greater than the upper bound."))))
  
(define (subrange-iterator type expr-evaluator)
  (ensure ((type <sal-bounded-subtype>))
    (let ((lower (expr-evaluator (slot-value type :lower)))
          (upper (expr-evaluator (slot-value type :upper))))
      (cond
       ((and (instance-of? lower <sal-numeral>)
             (instance-of? upper <sal-numeral>))
        (ensure ((lower <sal-numeral>)
                 (upper <sal-numeral>))
          (check-subrange lower upper type)
          (let ((curr (slot-value lower :num)))
            (make-iterator (<=mpq curr (slot-value upper :num))
                           (let ((result curr))
                             (set! curr (+mpq curr *mpq-one*))
                             (make-sal-numeral result type))
                           (set! curr (slot-value lower :num))))))
       (else
        (sign-iterator-error type "Subrange cannot be iterated, it was not possible to compute a numeric lower and/or upper bound."))))))

(define-method (sal-type/make-iterator-core (type <sal-subrange>) (expr-evaluator <primitive>))
  (subrange-iterator type expr-evaluator))

(define (mk-bounded-subtype-iterator type expr-evaluator)
  (iterator/filter 
   (lambda (e)
     (sal-expr/true? (expr-evaluator (sal-expr/apply (slot-value type :expr) e))))
   (subrange-iterator type expr-evaluator)))

(define-method (sal-type/make-iterator-core (type <sal-bounded-subtype>) (expr-evaluator <primitive>))
  (let ((new-type (update-ast-slots type :expr (expr-evaluator (slot-value type :expr)))))
    (mk-bounded-subtype-iterator new-type expr-evaluator)))

(define-method (sal-type/make-iterator-core (type <sal-subtype>) (expr-evaluator <primitive>))
  (let ((new-type (update-ast-slots type :expr (expr-evaluator (slot-value type :expr)))))
    (cond
     ((and (sal-type/integer? new-type) 
           (sal-type->bounded-subtype new-type)) 
      =>
      (lambda (bounded-subtype)
        (mk-bounded-subtype-iterator bounded-subtype expr-evaluator)))
     (else
      (iterator/filter
       (lambda (e)
;          (sal/pp e) (print "")
;          (sal/pp new-type) (print "")
;          (sal/pp (slot-value new-type :expr)) (print "")
;          (print "---------------")
         (sal-expr/true? (expr-evaluator (sal-expr/apply (slot-value new-type :expr) e))))
       (sal-type/make-iterator-core (sal-subtype/immediate-super-type new-type) expr-evaluator))))))
  
(define-method (sal-type/make-iterator-core (type <sal-scalar-type>) (expr-evaluator <primitive>))
  (make-list-iterator (slot-value type :scalar-elements)))

(define (make-tuple-type-iterator type expr-evaluator tuple-literal-class)
  (apply iterator/product 
         (lambda elems 
           (make-ast-instance tuple-literal-class type
                              :exprs elems))
         (map (cut sal-type/make-iterator-core <> expr-evaluator) (slot-value type :types))))

(define-method (sal-type/make-iterator-core (type <sal-tuple-type>) (expr-evaluator <primitive>))
  (make-tuple-type-iterator type expr-evaluator <sal-tuple-literal>))

(define-method (sal-type/make-iterator-core (type <sal-domain-tuple-type>) (expr-evaluator <primitive>))
  (make-tuple-type-iterator type expr-evaluator <sal-arg-tuple-literal>))

(define-method (sal-type/make-iterator-core (type <sal-record-type>) (expr-evaluator <primitive>))
  (let ((ids (map (lambda (field)
                    (ensure ((field <sal-field>))
                      (slot-value field :id)))
                  (slot-value type :fields))))
    (apply iterator/product
           (lambda values
             (make-ast-instance <sal-record-literal> type
                                :entries (map (lambda (id val)
                                                (make-ast-instance <sal-record-entry> type
                                                                   :id id
                                                                   :expr val))
                                              ids values)))
           (map (lambda (field)
                  (ensure ((field <sal-field>))
                    (sal-type/make-iterator-core (slot-value field :type) expr-evaluator)))
                (slot-value type :fields)))))

(define (sal-function-type/make-argument-list type)
  [assert (type) (sal-type/function? type)]
  (let* ((domain (sal-function-type/domain type))
         (type-list (sal-type->type-list domain)))
    (sal-type-list->sal-decl-list type-list)))

(define-method (sal-type/make-iterator-core (type <sal-function-type>) (expr-evaluator <primitive>))
  (let* ((arg-type-iterator (sal-type/make-iterator-core (slot-value type :domain) expr-evaluator))
         (args (iterator->list arg-type-iterator))
         (num-args (length args))
         (it-list (generate-list (lambda (idx) (sal-type/make-iterator-core (slot-value type :range) expr-evaluator)) num-args))
         (power-it (apply iterator/product list it-list))
         (var-decl-list (sal-function-type/make-argument-list type))
         (var-decl-name-exprs (sal-decl-list->sal-name-expr-list var-decl-list)))
    (iterator/map
     (lambda (elems)
       [assert (elems) (= num-args (length elems))]
       [assert (elems) (not (null? elems))]
       (let ((fun-body (let loop ((elems elems)
                                  (args args))
                         (if (null? (cdr elems))
                           (car elems)
                           (let* ((curr-elem (car elems))
                                  (curr-arg (car args))
                                  (condition (if (null? (cdr var-decl-name-exprs)) ;; only one argument
                                               (make-sal-equality (car var-decl-name-exprs) curr-arg)
                                               (let ((eq-list (map (lambda (expr1 expr2) 
                                                                     (make-sal-builtin-application <sal-eq> expr1
                                                                                                   expr1 expr2))
                                                                   var-decl-name-exprs 
                                                                   (slot-value curr-arg :exprs))))
                                                 (apply make-sal-builtin-application <sal-and> type
                                                        eq-list)))))
                             (make-sal-cond+ condition 
                                             curr-elem
                                             (loop (cdr elems) (cdr args))
                                             type))))))
         (make-ast-instance <sal-lambda> type
                            :local-decls var-decl-list
                            :expr fun-body)))
     power-it)))

(define-method (sal-type/make-iterator-core (type <sal-array-type>) (expr-evaluator <primitive>))
  (let ((aux-iterator (call-next-method)))
    (iterator/map (lambda (elem)
                    (change-ast-class elem <sal-array-literal>))
                  aux-iterator)))

(define (make-constructor-iterator constructor-name expr-evaluator)
  (let* ((accessor-list (sal-name-expr/constructor-accessors constructor-name))
         (accessor-type-list (map sal-name-expr/accessor-type accessor-list)))
    (if (null? accessor-list)
      (make-singleton-iterator constructor-name)
      (apply iterator/product 
             (lambda elems
                      (make-ast-instance <sal-constructor-application> constructor-name
                                         :fun constructor-name
                                         :arg (apply make-application-argument elems)))
             (map (cut sal-type/make-iterator-core <> expr-evaluator) accessor-type-list)))))
    

(define-method (sal-type/make-iterator-core (type <sal-data-type>) (expr-evaluator <primitive>))
  (iterator/append* (map (cut make-constructor-iterator <> expr-evaluator) (slot-value type :constructors))))

(define-generic (sal-type/value-idx type value)
  :doc "Returns the index of a value in the given type. The index is the position of the value in the builtin order for the given type. @code{type} must be a finite type. @code{value} must be an element of the type @code{type}."
  :examples '((sal-type/value-idx (sal/type "[2..5]") (sal/expr "3"))
              (sal-type/value-idx (sal/type "[2..5]") (sal/expr "2"))
              (sal-type/value-idx (sal/type "[2..5]") (sal/expr "4"))
              (sal-type/value-idx (sal/type "[bool, bool]") (sal/expr "(false, true)"))))

(define-method (sal-type/value-idx (type <sal-type>) (value <sal-expr>))
  (try
   (let ((it (sal-type/make-iterator type))
         (env (make-empty-env))
         (idx 0))
     (bind-exit (exit)
       (iterator/for-each (lambda (t-value)
                            (let ((equal-value? (sal-expr/equal-values? t-value value env)))
                              (cond
                               ((eq? equal-value? #t)
                                (exit idx))
                               ((eq? equal-value? #f)
                                (set! idx (+ idx 1)))
                               (else
                                (exit #f)))))
                          it)
       #f))
   (catch 'type-iterator
          (lambda (_)
            #f))))

(define-method (sal-type/value-idx (type <sal-subrange>) (value <sal-expr>))
  (let ((lower (sal-expr/evaluate (slot-value type :lower)))
        (upper (sal-expr/evaluate (slot-value type :upper)))
        (value (sal-expr/evaluate value)))
    (and-instance-of? ((lower <sal-numeral>)
                       (upper <sal-numeral>)
                       (value <sal-numeral>))
      (check-subrange lower upper type)
;; BD: (unless #t .. ) returns #f in bigloo2.8 and #unspecified in bigloo2.6
;; this is a bug here since it's embedded inside and
      (if (and (>=mpq (slot-value value :num) (slot-value lower :num))
	       (<=mpq (slot-value value :num) (slot-value upper :num)))
	  #t
	  (sign-source-error value "Type error, expression is not a member of type at ~a." (format-with-location type "")))
      (mpq->integer (-mpq (slot-value value :num) (slot-value lower :num))))))
      
(define-method (sal-type/value-idx (type <sal-scalar-type>) (value <sal-expr>))
  (let ((value (sal-expr/evaluate value)))
    (and-instance-of? ((value <sal-name-expr>))
      (sal-name-expr/scalar-element? value)
      (sal-scalar-element/idx value))))

(define-method (sal-type/value-idx (type <sal-type-name>) (value <sal-expr>))
  (let ((definition (sal-type-name/definition type)))
    (and definition
         (sal-type/value-idx definition value))))

(define-generic (sal-type/number-of-elements-core type expr-evaluator))

(memoize-sal-ast-method (sal-type/number-of-elements-core (type <sal-type>) (expr-evaluator <primitive>)))

(define-method (sal-type/number-of-elements-core (type <sal-type-name>) (expr-evaluator <primitive>))
  [assert (type) (sal-type/finite? type)]
  (sal-type/number-of-elements-core (sal-type-name/definition type) expr-evaluator))

(define (sign-unknown-number-of-elements)
  (error 'unknown-number-of-elements 'unknown-number-of-elements 'unknown-number-of-elements))

(define (bounded-subtype-number-of-elements type expr-evaluator)
  (let ((lower (expr-evaluator (slot-value type :lower)))
        (upper (expr-evaluator (slot-value type :upper))))
    (or
     (and-instance-of? ((lower <sal-numeral>)
                        (upper <sal-numeral>))
       (+mpq (-mpq (slot-value upper :num) (slot-value lower :num)) *mpq-one*))
     (sign-unknown-number-of-elements))))

(define-method (sal-type/number-of-elements-core (type <sal-subrange>) (expr-evaluator <primitive>))
  (bounded-subtype-number-of-elements type expr-evaluator))

(define-method (sal-type/number-of-elements-core (type <sal-scalar-type>) (expr-evaluator <primitive>))
  (integer->mpq (length (slot-value type :scalar-elements))))

(define-method (sal-type/number-of-elements-core (type <sal-tuple-type>) (expr-evaluator <primitive>))
  (fold-left (lambda (curr type)
               (*mpq curr (sal-type/number-of-elements-core type expr-evaluator)))
             *mpq-one*
             (slot-value type :types)))

(define-method (sal-type/number-of-elements-core (type <sal-record-type>) (expr-evaluator <primitive>))
  (fold-left (lambda (curr field)
               (ensure ((field <sal-field>))
                 (*mpq curr (sal-type/number-of-elements-core (slot-value field :type) expr-evaluator))))
             *mpq-one*
             (slot-value type :fields)))

(define-method (sal-type/number-of-elements-core (type <sal-function-type>) (expr-evaluator <primitive>))
  (mpq/exp (sal-type/number-of-elements-core (slot-value type :range) expr-evaluator)
           (sal-type/number-of-elements-core (slot-value type :domain) expr-evaluator)))

(define (sal-constructor/number-of-elements-core constructor-name expr-evaluator)
  (let* ((accessor-list (sal-name-expr/constructor-accessors constructor-name))
         (accessor-type-list (map sal-name-expr/accessor-type accessor-list)))
    (fold-left (lambda (curr type)
                 (*mpq curr (sal-type/number-of-elements-core type expr-evaluator)))
               *mpq-one*
               accessor-type-list)))

(define (num-elems->int n place-provider)
  (unless (and (mpq? n) (<mpq n *mpq-max-int*))
    (sign-sal-error 'sal-type-number-of-elements place-provider "The number of elements in the given type is unknown or infinite." '()))
  (mpq->integer n))

(define (sal-constructor/number-of-elements constructor-name)
  (sal-constructor/number-of-elements-core constructor-name sal-expr/evaluate))

(define (sal-constructor/number-of-elements-as-integer constructor-name)
  (num-elems->int (sal-constructor/number-of-elements constructor-name) constructor-name))
                 
(define-method (sal-type/number-of-elements-core (type <sal-data-type>) (expr-evaluator <primitive>))
  (fold-left (lambda (curr constructor)
               (+mpq curr (sal-constructor/number-of-elements-core constructor expr-evaluator)))
             *mpq-zero*
             (slot-value type :constructors)))

(define-method (sal-type/number-of-elements-core (type <sal-type>) (expr-evaluator <primitive>))
  ;; Compute the number of elements using a type iterator.
  ;; this function can be quite expensive.
  ;; See function sal-type/make-iterator-core
  (integer->mpq (iterator/number-of-elements (sal-type/make-iterator-core type expr-evaluator))))

(define-api (sal-type/number-of-elements type)
  :doc "Returns the number of elements in the given type. The result is a GMP big number."
  (if (sal-type/finite? type)
    (try
     (sal-type/number-of-elements-core type sal-expr/evaluate)
     (lambda (escape proc msg obj)
       (if (app-error? proc)
         (escape 'unknown-number-of-elements)
         (error proc msg obj))))
    'infinite))

(define-api (sal-type/number-of-elements-as-integer type)
  :doc "Returns the number of elements in the given type. The result is a Scheme integer, if the result does fit in a Scheme integer, then an error is thrown."
  (num-elems->int (sal-type/number-of-elements type) type))

(define-generic (sal-type/super-type type)
  :doc "Returns the most generic super type of the given type.")

(define-method (sal-type/super-type :around (type <sal-type>))
  (trace 'trace "computing the super-type of ~a" (format-with-location type ""))
  (call-next-method))

(define-method (sal-type/super-type (type <sal-type>))
  type)

(define-method (sal-type/super-type (type <sal-tuple-type>))
  (update-ast-slots type :types (conservative-map-1 sal-type/super-type (slot-value type :types))))

(define-method (sal-type/super-type (type <sal-record-type>))
  (update-ast-slots type :fields (conservative-map-1
                                  (lambda (field)
                                    (ensure ((field <sal-field>))
                                      (update-ast-slots field :type (sal-type/super-type (slot-value field :type)))))
                                  (slot-value type :fields))))

(define-method (sal-type/super-type (type <sal-function-type>))
  (update-ast-slots type :range (sal-type/super-type (slot-value type :range))))

(define-api (sal-subtype/immediate-super-type type)
  :doc "Returns the immediate super type of the given type."
  [assert (type) (instance-of? type <sal-subtype>)]
  (let ((predicate-type (sal-expr/type (slot-value type :expr))))
    (unless (sal-type/predicate? predicate-type)
      (sign-source-error type "Invalid subtype."))
    (sal-function-type/domain predicate-type)))

(define-method (sal-type/super-type (type <sal-subtype>))
  (sal-type/super-type (sal-subtype/immediate-super-type type)))

(define-method (sal-type/super-type (type <sal-scalar-set-type>))
  type)

(define-method (sal-type/super-type (type <sal-ring-set-type>))
  type)
                 
(define-method (sal-type/super-type (type <sal-type-name>))
  (let ((def (sal-type-name/definition type)))
    (if def
      (sal-type/super-type def)
      type)))

(define-api (sal-subtype/normalize type)
  [assert (type) (instance-of? type <sal-subtype>)]
  (ensure ((type <sal-subtype>))
    (let* ((expr-list '())
           (arg-var (gen-unique-name 'arg))
           (arg-var-decl (make-ast-instance <sal-var-decl> type
                                          :id (make-sal-identifier type arg-var)))
           (arg-name-expr (make-sal-name-expr arg-var-decl type))
           (super-type (let loop ((subtype type))
                         (cond
                          ((instance-of? subtype <sal-subtype>)
                           (ensure ((subtype <sal-subtype>))
                             (let* ((expr (slot-value subtype :expr))
                                    (new-expr (sal-expr/apply expr arg-name-expr)))
                               (push! new-expr expr-list)
                               (loop (sal-subtype/immediate-super-type subtype)))))
                          ((instance-of? subtype <sal-type-name>)
                           (let ((def (sal-type-name/definition subtype)))
                             (if def
                               (loop def)
                               subtype)))
                          (else
                           subtype)))))
      (ensure ((arg-var-decl <sal-var-decl>))
        (set-slot-value! arg-var-decl :type super-type)
        (make-ast-instance <sal-subtype> type
                           :expr (make-ast-instance <sal-lambda> type
                                                    :local-decls (list arg-var-decl)
                                                    :expr (apply make-sal-and expr-list)))))))

(define-generic (sal-type/equivalent? type1 type2)
  :doc "Returns @code{#t} if @code{type1} and @code{type2} are equivalent.")

(define-method (sal-type/equivalent? :around (type1 <sal-type>) (type2 <sal-type>))
  (or (sal-ast/equivalent? type1 type2)
      (call-next-method)))

(define-method (sal-type/equivalent? (type1 <sal-type>) (type2 <sal-type>))
  (let ((stype1 (sal-type/super-type type1))
        (stype2 (sal-type/super-type type2)))
    (sal-type/equivalent-core? stype1 stype2 (make-empty-env))))
  
(define-generic (sal-type/equivalent-core? type1 type2 env))

(define-method (sal-type/equivalent-core? (ast1 <sal-ast>) (ast2 <sal-ast>) (env <primitive>))
  (sal-ast/equivalent-core? ast1 ast2 env))

(define (sal-type-eq-default type1 type2 env)
  (and (eq? (class-of type1) (class-of type2))
       (sal-ast/compare-children type1 type2 env sal-type/equivalent-core?)))

(define-method (sal-type/equivalent-core? (type1 <sal-type>) (type2 <sal-type>) (env <primitive>))
  (sal-type-eq-default type1 type2 env))

;; avoid the distinction between tuple-types and domain-tuple-types
(define-method (sal-type/equivalent-core? (type1 <sal-tuple-type>) (type2 <sal-tuple-type>) (env <primitive>))
  (sal-ast/compare-children type1 type2 env sal-type/equivalent-core?))

;; avoid the distinction between array-type and function-type
(define-method (sal-type/equivalent-core? (type1 <sal-function-type>) (type2 <sal-function-type>) (env <primitive>))
  (sal-ast/compare-children type1 type2 env sal-type/equivalent-core?))

(define-method (sal-type/equivalent-core? (ast1 <sal-type-name>) (ast2 <sal-type>) (env <primitive>))
  (let ((def (sal-type-name/definition ast1)))
    (and def (sal-type/equivalent-core? ast2 def env))))

(define-method (sal-type/equivalent-core? (ast1 <sal-type>) (ast2 <sal-type-name>) (env <primitive>))
  (let ((def (sal-type-name/definition ast2)))
    (and def (sal-type/equivalent-core? ast1 def env))))

(define-method (sal-type/equivalent-core? (ast1 <sal-type-name>) (ast2 <sal-type-name>) (env <primitive>))
  (let ((def1 (sal-type-name/definition ast1))
        (def2 (sal-type-name/definition ast2)))
    (if (and def1 def2)
      (sal-type/equivalent-core? def1 def2 env)
      (eq? (slot-value ast1 :decl) (slot-value ast2 :decl)))))

(define-method (sal-type/equivalent-core? (type1 <sal-field>) (type2 <sal-field>) (env <primitive>))
  (sal-type-eq-default type1 type2 env))

(define-method (sal-type/equivalent-core? (expr1 <sal-expr>) (expr2 <sal-expr>) (env <primitive>))
  (or
   (sal-ast/equivalent-core? expr1 expr2 env)
   (sal-ast/equivalent-core? (sal-ast/simplify-core expr1 env) (sal-ast/simplify-core expr2 env) env)
   (sal-ast/equivalent-core? (sal-ast/eager-simplify-core expr1 env) (sal-ast/eager-simplify-core expr2 env) env)))

(define-api (sal-type/union type1 type2)
  :doc "Returns the union of types @code{type1} and @code{type2}."
  :examples '((sal-type/union (sal/type "[0..3]") (sal/type "[1..5]")))
  (trace 'type "computing the union of the types located at ~a and ~a" (format-with-location type1 "") (format-with-location type2 ""))
  (sal-type/union-core type1 type2 sal-type/union))

(define-generic (sal-type/union-core type1 type2 proc-child))

(define-method (sal-type/union-core :around (type1 <sal-type>) (type2 <sal-type>) (proc-child <primitive>))
;   (print "type1: ")
;   (sal/pp type1)
;   (print "\ntype2: ")
;   (sal/pp type2)
;   (print "\nequivalent: " (sal-type/equivalent? type1 type2))
  (cond
   ((sal-ast/equivalent? type1 type2)
    type1)
   ((sal-type/subtype-of? type1 type2)
    type2)
   ((sal-type/subtype-of? type2 type1)
    type1)
   (else
    (call-next-method))))

(define-method (sal-type/union-core (type1 <sal-type>) (type2 <sal-type>) (proc-child <primitive>))
  (sign-source-error type1 "Failed to compute the union of two types. This type is incompatible with the type located at ~a" (format-with-location type2 "")))

(define-method (sal-type/union-core (type1 <sal-tuple-type>) (type2 <sal-tuple-type>) (proc-child <primitive>))
  [assert (type1 type2) (= (length (slot-value type1 :types)) (length (slot-value type2 :types)))]
  (update-ast-slots type1
                    :types (conservative-map-2 proc-child
                                               (slot-value type1 :types) (slot-value type2 :types))))

(define-method (sal-type/union-core (type1 <sal-record-type>) (type2 <sal-record-type>) (proc-child <primitive>))
  [assert (type1 type2) (= (length (slot-value type1 :fields)) (length (slot-value type2 :fields)))]
  (update-ast-slots type1
                    :fields (conservative-map-2
                             (lambda (field1 field2)
                               (ensure ((field1 <sal-field>)
                                        (field2 <sal-field>))
                                 [assert (field1 field2) (eq? (sal-field/name field1) (sal-field/name field2))]
                                 (update-ast-slots field1 
                                                   :type (proc-child (slot-value field1 :type) (slot-value field2 :type)))))
                             (slot-value type1 :fields)
                             (slot-value type2 :fields))))

(define-method (sal-type/union-core (type1 <sal-function-type>) (type2 <sal-function-type>) (proc-child <primitive>))
  ;; the argument types must be equal! I'm not forcing this constraint
  (update-ast-slots type1
                    :range (proc-child (slot-value type1 :range) (slot-value type2 :range))))

(define-method (sal-type/union-core (type1 <sal-data-type>) (type2 <sal-data-type>) (proc-child <primitive>))
  [assert (type1 type2) (sal-ast/equivalent? type1 type2)]
  (unreachable-code))

(define-method (sal-type/union-core (type1 <sal-scalar-type>) (type2 <sal-scalar-type>) (proc-child <primitive>))
  [assert (type1 type2) (sal-ast/equivalent? type1 type2)]
  #unspecified)
;;  (unreachable-code))

(define-method (sal-type/union-core (type1 <sal-subtype>) (type2 <sal-type>) (proc-child <primitive>))
  ;; approximate
  (proc-child (sal-subtype/immediate-super-type type1) type2))

(define-method (sal-type/union-core (type1 <sal-type>) (type2 <sal-subtype>) (proc-child <primitive>))
  ;; approximate
  (proc-child type1 (sal-subtype/immediate-super-type type2)))

(define-method (sal-type/union-core (type1 <sal-subtype>) (type2 <sal-subtype>) (proc-child <primitive>))
  ;; (breakpoint "union" (type1 type2 proc-child) #t)
  ;; The subtypes need to be normalized, because we want to be able to handle
  ;; examples such as:
  ;; union of "{x : [1..11] | x /= 4}" and "[7 20]"
  (let* ((ntype1 (sal-subtype/normalize type1))
         (ntype2 (sal-subtype/normalize type2))
         (expr1 (slot-value ntype1 :expr))
         (expr2 (slot-value ntype2 :expr))
         (expr1-type (sal-expr/type expr1))
         (expr2-type (sal-expr/type expr2)))
    (unless (sal-type/equivalent? expr1-type expr2-type)
      (sign-source-error type1 "Type is not compatible with the type located at: ~a." (format-with-location type2 ""))) 
    (let* ((domain (sal-function-type/domain expr1-type))
           (arg-var (gen-unique-name 'arg))
           (arg-var-decl (make-ast-instance <sal-var-decl> type1
                                            :id (make-sal-identifier type1 arg-var)
                                            :type domain))
           (arg-name-expr (make-sal-name-expr arg-var-decl type1))
           (app1 (sal-expr/apply expr1 arg-name-expr))
           (app2 (sal-expr/apply expr2 arg-name-expr)))
      (make-ast-instance <sal-subtype> type1
                         :expr (make-ast-instance <sal-lambda> type1
                                                  :local-decls (list arg-var-decl)
                                                  :expr (make-sal-or app1 app2))))))

(define-method (sal-type/union-core (type1 <sal-state-type>) (type2 <sal-type>) (proc-child <primitive>))
  (proc-child (sal-module/type (slot-value type1 :module)) type2))

(define-method (sal-type/union-core (type1 <sal-type>) (type2 <sal-state-type>) (proc-child <primitive>))
  (proc-child type1 (sal-module/type (slot-value type2 :module))))

(define-method (sal-type/union-core (type1 <sal-type-name>) (type2 <sal-type-name>) (proc-child <primitive>))
  (let ((def1 (sal-type-name/definition type1))
        (def2 (sal-type-name/definition type2)))
    (cond
     ((and def1 def2)
      (proc-child def1 def2))
     (else
      (unreachable-code)))))

(define-method (sal-type/union-core (type1 <sal-type-name>) (type2 <sal-type>) (proc-child <primitive>))
  (let ((def (sal-type-name/definition type1)))
    (cond
     (def
      (proc-child def type2))
     (else
      (unreachable-code)))))

(define-method (sal-type/union-core (type1 <sal-type>) (type2 <sal-type-name>) (proc-child <primitive>))
  (proc-child type2 type1))

(define-method (sal-type/union-core (type1 <sal-subtype>) (type2 <sal-type-name>) (proc-child <primitive>))
  (proc-child type2 type1))

(define (sal-tuple-type/element-type type pos)
  (sal-tuple/element (sal-type/cast type <sal-tuple-type>) :types pos))

(define (sal-record-type/element-type type field-id)
  (sal-record/element (sal-type/cast type <sal-record-type>) :fields :type field-id))

(define-api (make-sal-subrange lower upper . place-provider)
  :doc "Creates a subrange type."
  :examples '((make-sal-subrange (sal/expr "0") (sal/expr "100")))
  (let* ((place-provider (optional-arg place-provider lower))
         (int-type-name (make-sal-builtin-name <sal-int-type> place-provider))
         (arg (gen-unique-name 'arg))
         (arg-id (make-sal-identifier place-provider arg))
         (arg-var-decl (make-ast-instance <sal-var-decl> place-provider
                                          :id arg-id
                                          :type int-type-name))
         (arg-name-expr (make-sal-name-expr arg-var-decl place-provider))
         (ge-lower (make-sal-builtin-application <sal-ge> place-provider
                                                 arg-name-expr lower))
         (le-upper (make-sal-builtin-application <sal-le> place-provider
                                                 arg-name-expr upper))
         (and-expr (make-sal-builtin-application <sal-and> place-provider
                                                 ge-lower le-upper))
         (predicate (make-ast-instance <sal-lambda> place-provider
                                       :local-decls (list arg-var-decl)
                                       :expr and-expr)))
    (make-ast-instance <sal-subrange> place-provider
                       :expr predicate
                       :lower lower
                       :upper upper)))         

         


