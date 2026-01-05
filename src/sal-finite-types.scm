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

(module sal-finite-types
        (include "sal.sch")
        (include "sal-ast-table.sch")
        (import gmp-scheme sal-expression sal-type sal-expr-evaluator sal-ast-eq iterators)
        (export (sal-type/finite-rep-num-bits type)
                (sal-type/finite-rep-num-elems type)
                (sal-bounded-subtype/normalized-bounds type)
                (sal-bounded-subtype/normalized-lower-bound type)
                (sal-bounded-subtype/normalized-upper-bound type)
                (sal-constructor/finite-rep-num-bits type)
                (evaluate-subrange-bound bound)
                (sal-data-type/finite-rep-num-tag-bits type)
                (sal-type/union-finite-rep type1 type2)
                (sal-finite-types/set-subrange-maximum-upper-bound! max)
                (sal-finite-types/set-subrange-minimum-lower-bound! min)
                (sal-finite-types/set-maximum-function-domain-size! max)
                (sal-finite-types/maximum-function-domain-size))
        )

(define *sal-subrange-minimum-lower-bound* (make-mpq "-2147483648"))
(define *sal-subrange-maximum-upper-bound* (make-mpq "2147483647"))
(define *sal-maximum-function-domain-size* 1048576)

(define (sal-finite-types/set-subrange-maximum-upper-bound! max)
  (let ((new-max (object->mpq max)))
    (unless (>mpq new-max *sal-subrange-minimum-lower-bound*)
      (sign-error "Invalid new subrange maximum upper bound threshold, it must be greater than the current minimum lower bound threshold."))
    (set! *sal-subrange-maximum-upper-bound* new-max)))

(define (sal-finite-types/set-subrange-minimum-lower-bound! min)
  (let ((new-min (object->mpq min)))
    (unless (<mpq new-min *sal-subrange-maximum-upper-bound*)
      (sign-error "Invalid new subrange minimum lower bound threshold, it must be less than the current maximum upper bound threshold."))
    (set! *sal-subrange-minimum-lower-bound* new-min)))

(define (sal-finite-types/set-maximum-function-domain-size! max)
  (set! *sal-maximum-function-domain-size* (object->mpq max)))

(define (sal-finite-types/maximum-function-domain-size)
  *sal-maximum-function-domain-size*)

(define (sal-type/finite-rep-num-elems type)
  (sal-type/number-of-elements-as-integer type))

(register-app-error! 'finite-type-representation)

(define (sign-finite-type-representation-error ast msg . args)
  (error 'finite-type-representation (apply format-with-location ast msg args) #unspecified))

(define-generic (sal-type/finite-rep-num-bits type))

(memoize-sal-ast-method (sal-type/finite-rep-num-bits (type <sal-type>)))

(define-method (sal-type/finite-rep-num-bits (type <sal-scalar-type>))
  (let ((num-elems (integer->mpq (length (slot-value type :scalar-elements)))))
    (mpq/num-bits num-elems)))

(define-method (sal-type/finite-rep-num-bits (type <sal-bool-type>))
  1)

(define (normalize-bounds lower-val upper-val)
  (multiple-value-bind
      (low up)
      (cond
       ((<mpq upper-val *mpq-zero*)
        [assert (lower-val) (<mpq lower-val *mpq-zero*)]
        (values lower-val (-mpq (mpq/absolute lower-val) *mpq-one*)))
       ((>=mpq lower-val *mpq-zero*)
        [assert (upper-val) (>=mpq upper-val *mpq-zero*)]
        (values *mpq-zero* upper-val))
       ((<mpq lower-val *mpq-zero*)
        [assert (upper-val) (>=mpq upper-val *mpq-zero*)]
        (cond
         ((<=mpq (mpq/absolute lower-val) upper-val)
          (values (-mpq (-mpq *mpq-zero* upper-val) *mpq-one*) upper-val))
         (else
          [assert (lower-val upper-val) (>mpq (mpq/absolute lower-val) upper-val)]
          (values lower-val (-mpq (mpq/absolute lower-val) *mpq-one*)))))
       (else
        (internal-error)))
    (let ((num-bits (mpq/num-bits (+mpq (-mpq up low) *mpq-one*))))
      (if (<mpq low *mpq-zero*)
        (let ((power (mpq/exp *mpq-two* (object->mpq (- num-bits 1)))))
          (values (-mpq *mpq-zero* power) (-mpq power *mpq-one*)))
        (let ((power (mpq/exp *mpq-two* (object->mpq num-bits))))
          (values *mpq-zero* (-mpq power *mpq-one*)))))))

(define (evaluate-subrange-bound bound)
  (try
   (sal-expr/evaluate bound)
   (catch 'expr-evaluator
          (lambda (msg)
            (sign-finite-type-representation-error bound "Finite type representation cannot be generated, bound failed to be evaluated, reason: ~a" msg)))))

(define (sal-bounded-subtype/normalized-bounds type)
  (let* ((lower (evaluate-subrange-bound (slot-value type :lower)))
         (upper (evaluate-subrange-bound (slot-value type :upper)))
         (lower-val (slot-value lower :num))
         (upper-val (slot-value upper :num)))
    (unless (<=mpq lower-val upper-val)
      (sign-source-error type "Invalid subrange, lower bound is greater than the upper bound."))
    (when (<mpq lower-val *sal-subrange-minimum-lower-bound*)
      (sign-finite-type-representation-error type "The subrange lower bound (~a) is below the minimum allowed (~a). Please rewrite your specification or increase the threshold. The threshold can be increased using the statement:\n\n  (sal-finite-types/set-subrange-minimum-lower-bound! \"<new-threshold>\")\n\nThis statement should be included in your `.salrc' file in your home directory." (mpq->string lower-val) (mpq->string *sal-subrange-minimum-lower-bound*)))
    (when (>mpq upper-val *sal-subrange-maximum-upper-bound*)
      (sign-finite-type-representation-error type "The subrange upper bound (~a) is above the maximum allowed (~a). Please rewrite your specification or increase the threshold. The threshold can be increased using the statement:\n\n  (sal-finite-types/set-subrange-maximum-upper-bound! \"<new-threshold>\")\n\nThis statement should be included in your `.salrc' file in your home directory." (mpq->string upper-val) (mpq->string *sal-subrange-maximum-upper-bound*)))
    (normalize-bounds lower-val upper-val)))

(define (sal-bounded-subtype/finite-rep-num-bits type)
  (multiple-value-bind
      (normalized-lower-val normalized-upper-val)
      (sal-bounded-subtype/normalized-bounds type)
    (mpq/num-bits (+mpq (-mpq normalized-upper-val normalized-lower-val) *mpq-one*))))

(define (sal-bounded-subtype/normalized-lower-bound type)
  (multiple-value-bind 
      (normalized-lower-bound normalized-upper-bound)
      (sal-bounded-subtype/normalized-bounds type)
    normalized-lower-bound))

(define (sal-bounded-subtype/normalized-upper-bound type)
  (multiple-value-bind
      (normalized-lower-bound normalized-upper-bound)
      (sal-bounded-subtype/normalized-bounds type)
    normalized-upper-bound))

(define-method (sal-type/finite-rep-num-bits (type <sal-bounded-subtype>))
  (sal-bounded-subtype/finite-rep-num-bits type))

(define-method (sal-type/finite-rep-num-bits (type <sal-subtype>))
  (cond
   ((and (sal-type/integer? type) 
         (sal-type->bounded-subtype type)) =>
    sal-bounded-subtype/finite-rep-num-bits)
   (else
    (let ((super-type (sal-subtype/immediate-super-type type)))
      (sal-type/finite-rep-num-bits super-type)))))

(define-method (sal-type/finite-rep-num-bits (type <sal-type-name>))
  (unless (sal-type/finite? type)
    (sign-finite-type-representation-error type "Finite type representation cannot be generated, type is not finite.\n~a" 
                                           (sal-type/non-finiteness-reason)))
  (let ((definition (sal-type-name/definition type)))
    (if definition
       (sal-type/finite-rep-num-bits definition)
       (unreachable-code))))

(define (compute-sum lst proc)
  (fold-left (lambda (val elem)
               (+ val (proc elem)))
             0
             lst))

(define-method (sal-type/finite-rep-num-bits (type <sal-tuple-type>))
  (compute-sum (slot-value type :types) sal-type/finite-rep-num-bits))

(define-method (sal-type/finite-rep-num-bits (type <sal-record-type>))
  (compute-sum (slot-value type :fields) (compose sal-type/finite-rep-num-bits (cut slot-value <> :type))))

(define-method (sal-type/finite-rep-num-bits (type <sal-state-type>))
  (sign-unsupported-feature type "Cannot compute finite representation for state type."))

(define-method (sal-type/finite-rep-num-bits (type <sal-function-type>))
  (try
   (let ((domain-num-elems (sal-type/number-of-elements-as-integer (slot-value type :domain))) 
         (range-num-bits (sal-type/finite-rep-num-bits (slot-value type :range))))
     (when (> domain-num-elems *sal-maximum-function-domain-size*)
       (sign-finite-type-representation-error type "Finite type representation cannot be generated, the domain of the function (array) is too big. Please rewrite your specification or increase the threshold."))
     (* range-num-bits domain-num-elems))
   (catch 'sal-type-number-of-elements
          (lambda (msg)
            (sign-unsupported-feature type "Function (array) type cannot be converted to a boolean representation because it was not possible to compute the number of elements in the domain (reason: ~a)" msg)))))

(define (sal-constructor/finite-rep-num-bits type)
  (let ((type (sal-name-expr/type type)))
    (if (sal-type/function? type)
      (sal-type/finite-rep-num-bits (sal-function-type/domain type))
      0)))

(define (sal-data-type/finite-rep-num-tag-bits type)
  (mpq/num-bits (length (slot-value type :constructors))))

(define-method (sal-type/finite-rep-num-bits (type <sal-data-type>))
  (let* ((constructors (slot-value type :constructors))
         (num-tag-bits (sal-data-type/finite-rep-num-tag-bits type))
         (num-body-bits (list-max sal-constructor/finite-rep-num-bits constructors)))
    (+ num-tag-bits num-body-bits)))

(define-generic (sal-type/union-finite-rep type1 type2))

(define-method (sal-type/union-finite-rep :around (type1 <sal-type>) (type2 <sal-type>))
  ;; (breakpoint "sal-type/union-finite-rep" (type1 type2) #t)
  (cond
   ((sal-ast/equivalent? type1 type2)
    type1)
   ((sal-type/subtype-of? type1 type2)
    type2)
   ((sal-type/subtype-of? type2 type1)
    type1)
   (else
    (call-next-method))))

(define-method (sal-type/union-finite-rep (type1 <sal-ast>) (type2 <sal-ast>))
  (sal-type/union-core type1 type2 sal-type/union-finite-rep))

(define (sal-bounded-subtype/union-finite-rep type1 type2)
  (let* ((lower1 (evaluate-subrange-bound (slot-value type1 :lower)))
         (upper1 (evaluate-subrange-bound (slot-value type1 :upper)))
         (lower2 (evaluate-subrange-bound (slot-value type2 :lower)))
         (upper2 (evaluate-subrange-bound (slot-value type2 :upper)))
         (new-lower (update-ast-slots lower1 :num (mpq/min (slot-value lower1 :num) (slot-value lower2 :num))))
         (new-upper (update-ast-slots upper1 :num (mpq/max (slot-value upper1 :num) (slot-value upper2 :num)))))
    (make-sal-subrange new-lower new-upper)))
   
(define-method (sal-type/union-finite-rep (type1 <sal-bounded-subtype>) (type2 <sal-bounded-subtype>))
  (sal-bounded-subtype/union-finite-rep type1 type2))

(define-method (sal-type/union-finite-rep (type1 <sal-subtype>) (type2 <sal-subtype>))
  (cond
   ((and (sal-type/integer? type1) 
         (sal-type/integer? type2))
    (let ((b1 (sal-type->bounded-subtype type1))
          (b2 (sal-type->bounded-subtype type2)))
      (if (and b1 b2)
        (sal-bounded-subtype/union-finite-rep b1 b2)
        (call-next-method))))
   (else
    (call-next-method))))

