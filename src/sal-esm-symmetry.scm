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

(module sal-esm-symmetry
        (include "sal.sch")
        (import sal-esm-util sal2scm-core sal2scm sal-ast-table sal-type
                sal-expression sal-ast-env sal2scm-runtime
                gmp-scheme state-entry-channel sal-ast-eq)
        (export (make-pre-symmetry-layout-context)
                (sal-type/esm-symmetric-memory-layout type scm-ctx pre-ctx)
                (pre-symmetry-layout-context->symmetry-layout-context pre-ctx)
                (symmetry-layout-context/show ctx)
                (symmetry-layout-context/make-symmetry-constraint-vector ctx)
                (symmetry-layout-context/layout-definition-vector ctx)
                (symmetry-constraint-vector/normalized-value constraint-vector type-idx val)
                (symmetry-constraint-vector/normalize! constraint-vector type-idx val)
                (symmetry-constraint-vector/show constraint-vector)
                (symmetry-constraint-vector/fill! vect)
                (symmetry-constraint-vector/reset! vect)
                (mk-esm-sym-atom-layout type-idx num-bits)
                (esm-sym-atom-layout? obj)
                (esm-sym-atom-layout/type-idx layout)
                (esm-sym-atom-layout/num-bits layout)
                (inline esm-sym-atom-value->channel! value layout constraint-vect channel)
                (inline channel->esm-sym-atom-value! channel layout)
                (mk-esm-sym-mpq-layout type-idx num-bits)
                (esm-sym-mpq-layout? obj)
                (esm-sym-mpq-layout/type-idx layout)
                (esm-sym-mpq-layout/num-bits layout)
                (inline esm-sym-mpq-value->channel! value layout constraint-vect channel)
                (inline channel->esm-sym-mpq-value! channel layout)
                (mk-esm-scalar-set-array-layout type-idx body)
                (esm-scalar-set-array-layout? obj)
                (esm-scalar-set-array-layout/type-idx layout)
                (esm-scalar-set-array-layout/body layout)
                (mk-esm-ring-set-array-layout type-idx body)
                (esm-ring-set-array-layout? obj)
                (esm-ring-set-array-layout/type-idx layout)
                (esm-ring-set-array-layout/body layout) 
                (esm/normalize-symmetric-object value layout layout-def-vect constraint-vect)
                (esm-scalar-set-array->channel! value layout layout-def-vect constraint-vect proc-value)
                (esm-ring-set-array->channel! value layout layout-def-vect constraint-vect proc-value)
                (sal-type/symmetry-friendliness type)
                )
        )

(define-record-type pre-symmetry-layout-context
  (mk-pre-symmetry-layout-context type-name-table next-type-name-idx sym-type-table next-sym-type-idx)
  pre-symmetry-layout-context?
  (type-name-table pre/type-name-table)
  (next-type-name-idx pre/next-type-name-idx pre/set-next-type-name-idx!)
  (sym-type-table pre/sym-type-table)
  (next-sym-type-idx pre/next-sym-type-idx pre/set-next-sym-type-idx!))

(define-record-type symmetry-layout-context
  (mk-symmetry-layout-context layout-definition-vector sym-type-info-vector)
  symmetry-layout-context?
  (layout-definition-vector symmetry-layout-context/layout-definition-vector)
  (sym-type-info-vector symmetry-layout-context/symmetric-type-info-vector))

(define (pre-symmetry-layout-context/contains-symmetric-types? pre-ctx)
  (> (pre/next-sym-type-idx pre-ctx) 0))

(define (pre-symmetry-layout-context->symmetry-layout-context pre-ctx)
  (let* ((num-type-names (pre/next-type-name-idx pre-ctx))
         (num-sym-types (pre/next-sym-type-idx pre-ctx))
         (layout-definition-vector (make-vector num-type-names #unspecified))
         (sym-type-info-vector (make-vector num-sym-types #unspecified)))
    (sal-ast-table/for-each (lambda (_ info)
                              (let ((idx (car info))
                                    (layout (cdr info)))
                                (vector-set! layout-definition-vector (esm-ref-layout/definition-idx idx) layout)))
                            (pre/type-name-table pre-ctx))
    (sal-ast-table/for-each (lambda (_ info)
                              (let ((idx (car info))
                                    (data (cdr info)))
                                (vector-set! sym-type-info-vector idx data)))
                            (pre/sym-type-table pre-ctx))
    (mk-symmetry-layout-context layout-definition-vector sym-type-info-vector)))

(define (symmetry-layout-context/show ctx)
  (print "Layout definitions:")
  (let ((idx 0))
    (vector/for-each (lambda (info)
                       (print idx " |-> " info)
                       (set! idx (+ idx 1)))
                     (symmetry-layout-context/layout-definition-vector ctx)))
  (print "Symmetric types information:")
  (let ((idx 0))
    (vector/for-each (lambda (data)
                       (print idx " |-> " data)
                       (set! idx (+ idx 1)))
                     (symmetry-layout-context/symmetric-type-info-vector ctx))))

(define-inline (symmetry-layout-context/layout-definition ctx idx)
  (vector-ref (symmetry-layout-context/layout-definition-vector ctx) idx))

(define-record-type scalar-set-constraint
  (mk-scalar-set-constraint next-available-value permutation inv-permutation)
  scalar-set-constraint?
  (next-available-value scalar-set-constraint/next-available-value scalar-set-constraint/set-next-available-value!)
  (permutation scalar-set-constraint/permutation)
  (inv-permutation scalar-set-constraint/inv-permutation))

(define-record-type ring-set-constraint
  (mk-ring-set-constraint delta size)
  ring-set-constraint?
  (delta ring-set-constraint/delta ring-set-constraint/set-delta!)
  (size ring-set-constraint/size))

(define (symmetry-layout-context/make-symmetry-constraint-vector ctx)
  (let* ((sym-type-info-vector (symmetry-layout-context/symmetric-type-info-vector ctx))
         (num-sym-types (vector-length sym-type-info-vector))
         (result (make-vector num-sym-types)))
    (let loop ((idx 0))
      (when (< idx num-sym-types)
        (let ((data (vector-ref sym-type-info-vector idx)))
          (if (eq? (car data) 'scalar-set)
            (vector-set! result idx (mk-scalar-set-constraint 0 (make-vector (cdr data)) (make-vector (cdr data))))
            (vector-set! result idx (mk-ring-set-constraint #unspecified (cdr data)))))
        (loop (+ idx 1))))
    (symmetry-constraint-vector/reset! result)
    result))

(define (symmetry-constraint-vector/reset! vect)
  (let ((n (vector-length vect)))
    (let loop ((i 0))
      (when (< i n)
        (let ((constraint (vector-ref vect i)))
          (cond
           ;; RESET SCALARSET CONSTRAINTS
           ((scalar-set-constraint? constraint)
            (scalar-set-constraint/set-next-available-value! constraint 0)
            (vector-fill! (scalar-set-constraint/permutation constraint) 'undef)
            (vector-fill! (scalar-set-constraint/inv-permutation constraint) 'undef))
           ;; RESET RINGSET CONSTRAINTS
           (else
            [assert (constraint) (ring-set-constraint? constraint)]
            (ring-set-constraint/set-delta! constraint 'undef))))
        (loop (+ i 1))))))

(define (symmetry-constraint-vector/fill! vect)
  (let ((n (vector-length vect)))
    (let loop ((i 0))
      (when (< i n)
        (let ((constraint (vector-ref vect i)))
          (cond
           ;; FILL SCALARSET CONSTRAINTS
           ((scalar-set-constraint? constraint)
            (let* ((permutation-vect (scalar-set-constraint/permutation constraint))
                   (inv-permutation-vect (scalar-set-constraint/inv-permutation constraint))
                   (permutation-vect-size (vector-length permutation-vect))
                   (next-available-idx (scalar-set-constraint/next-available-value constraint)))
              (when (< next-available-idx permutation-vect-size)
                (let inner-loop ((j 0))
                  (when (< j permutation-vect-size)
                    (when (eq? (vector-ref permutation-vect j) 'undef)
                      (vector-set! permutation-vect j next-available-idx)
                      (vector-set! inv-permutation-vect next-available-idx j)
                      (set! next-available-idx (+ next-available-idx 1)))
                    (inner-loop (+ j 1)))))
              [assert (next-available-idx permutation-vect-size) (= next-available-idx permutation-vect-size)]
              (scalar-set-constraint/set-next-available-value! constraint permutation-vect-size)))
           ;; FILL RINGSET CONSTRAINTS
           (else
            [assert (constraint) (ring-set-constraint? constraint)]
            (when (eq? (ring-set-constraint/delta constraint) 'undef)
              (ring-set-constraint/set-delta! constraint 0)))))
        (loop (+ i 1))))))

(define-inline (rotation-normal val delta size)
  (if (>= val delta)
    (- val delta)
    (- size (- delta val))))

(define (symmetry-constraint-vector/normalized-value constraint-vector type-idx val)
  (let ((constraint (vector-ref constraint-vector type-idx)))
    (cond
     ;; GET NORMALIZED SCALARSET VALUE IF AVAILABLE
     ((scalar-set-constraint? constraint)
      (let* ((next-available-value (scalar-set-constraint/next-available-value constraint))
             (permutation (scalar-set-constraint/permutation constraint))
             (normalized-value (vector-ref permutation val)))
        normalized-value))
     ;; GET NORMALIZED RINGSET VALUE IF AVAILABLE
     (else
      [assert (constraint) (ring-set-constraint? constraint)]
      (let ((delta (ring-set-constraint/delta constraint)))
      (if (eq? delta 'undef)
        'undef
        (rotation-normal val delta (ring-set-constraint/size constraint))))))))

(define (symmetry-constraint-vector/normalize! constraint-vector type-idx val)
  (let ((constraint (vector-ref constraint-vector type-idx)))
    (cond
     ;; NORMALIZE SCALARSET VALUE
     ((scalar-set-constraint? constraint)
      (let* ((next-available-value (scalar-set-constraint/next-available-value constraint))
             (permutation (scalar-set-constraint/permutation constraint))
             (inv-permutation (scalar-set-constraint/inv-permutation constraint))
             (normalized-value (vector-ref permutation val)))
        (cond
         ((eq? normalized-value 'undef)
          (vector-set! permutation val next-available-value)
          (vector-set! inv-permutation next-available-value val)
          (scalar-set-constraint/set-next-available-value! constraint (+ next-available-value 1))
          next-available-value)
         (else
          normalized-value))))
     ;; NORMALIZE RINGSET VALUE
     (else
      [assert (constraint) (ring-set-constraint? constraint)]
      (let ((delta (ring-set-constraint/delta constraint)))
      (cond 
       ((eq? delta 'undef)
        (ring-set-constraint/set-delta! constraint val)
        0)
       (else  
        ;; ROTATE RINGSET ELEMENT
        (rotation-normal val delta (ring-set-constraint/size constraint)))))))))

(define (permutation-vector->list vect)
  (map (lambda (val)
         (if (eq? val 'undef) 'x  val))
       (vector->list vect)))

(define (symmetry-constraint-vector/show constraint-vector)
  (let ((n (vector-length constraint-vector)))
    (let loop ((i 0))
      (when (< i n)
        (let ((constraint (vector-ref constraint-vector i)))
          (cond
           ;; NORMALIZE SCALARSET VALUE
           ((scalar-set-constraint? constraint)
            (print i " |-> " 
                   (permutation-vector->list (scalar-set-constraint/permutation constraint))
                   ", "
                   (permutation-vector->list (scalar-set-constraint/inv-permutation constraint))))
           (else
            [assert (constraint) (ring-set-constraint? constraint)]
            (let ((delta (ring-set-constraint/delta constraint)))
              (print i " |-> " (if (eq? delta 'undef) 'x delta) " # " (ring-set-constraint/size constraint))))))
        (loop (+ i 1))))))

(define (make-pre-symmetry-layout-context)
  (mk-pre-symmetry-layout-context (make-sal-ast-table) 0 (make-sal-ast-table) 0))

(define-generic (sal-type/esm-symmetric-memory-layout type scm-ctx pre-ctx))

(define-method (sal-type/esm-symmetric-memory-layout (type <sal-type-name>) (scm-ctx <sal-scm-context>) (pre-ctx <primitive>))
  (let ((type-name-table (pre/type-name-table pre-ctx))
        (next-type-name-idx (pre/next-type-name-idx pre-ctx))
        (recursive? (sal-type-name/recursive? type)))

    (cond
     ((sal-ast-table/get type-name-table type) =>
      (lambda (entry)
        (let ((info (cdr entry)))
          (car info))))
     (else
      (let ((new-info (cons (mk-esm-ref-layout next-type-name-idx recursive?) #unspecified)))
        (sal-ast-table/put! type-name-table type new-info)
        (pre/set-next-type-name-idx! pre-ctx (+ next-type-name-idx 1))
        (let* ((defintion (sal-type-name/definition type))
               (_ [assert (defintion) defintion])
               (def-layout (sal-type/esm-symmetric-memory-layout defintion scm-ctx pre-ctx)))
          ;; (print next-type-name-idx " ... " def-layout)
          (set-cdr! new-info def-layout))
        (car new-info))))))

(define-method (sal-type/esm-symmetric-memory-layout (type <sal-tuple-type>) (scm-ctx <sal-scm-context>) (pre-ctx <primitive>))
  (apply vector
         (map (cut sal-type/esm-symmetric-memory-layout <> scm-ctx pre-ctx)
              (slot-value type :types))))

(define-method (sal-type/esm-symmetric-memory-layout (type <sal-record-type>) (scm-ctx <sal-scm-context>) (pre-ctx <primitive>))
  (apply vector
         (map (lambda (field)
                (sal-type/esm-symmetric-memory-layout (slot-value field :type) scm-ctx pre-ctx))
              (sal-record-type/sorted-fields type))))

(define-method (sal-type/esm-symmetric-memory-layout (type <sal-scalar-type>) (scm-ctx <sal-scm-context>) (pre-ctx <primitive>))
  (mk-esm-atom-layout (sal-scalar-type/num-bits type) 0))

(define-method (sal-type/esm-symmetric-memory-layout (type <sal-bool-type>) (scm-ctx <sal-scm-context>) (pre-ctx <primitive>))
  'boolean)

(define-method (sal-type/esm-symmetric-memory-layout (type <sal-bounded-subtype>) (scm-ctx <sal-scm-context>) (pre-ctx <primitive>))
  (multiple-value-bind
      (num-bits lower)
      (sal-bounded-subtype/num-bits-and-lower type scm-ctx)
    (if (slot-value scm-ctx :gmp?)
      (if num-bits
        (mk-esm-mpq-layout num-bits lower)
        'gmp-infinite)
      (if num-bits
        (mk-esm-atom-layout num-bits lower)
        'infinite))))

(define-method (sal-type/esm-symmetric-memory-layout (type <sal-number-type>) (scm-ctx <sal-scm-context>) (pre-ctx <primitive>))
  (if (slot-value scm-ctx :gmp?)
    'gmp-infinite
    'infinite))

(define-method (sal-type/esm-symmetric-memory-layout (type <sal-subtype>) (scm-ctx <sal-scm-context>) (pre-ctx <primitive>))
  (sal-type/esm-symmetric-memory-layout (sal-subtype/immediate-super-type type) scm-ctx pre-ctx))

(define-generic (sal-symmetric-type/layout-tag type))
(define-method (sal-symmetric-type/layout-tag (type <sal-scalar-set-type>))
  `scalar-set)
(define-method (sal-symmetric-type/layout-tag (type <sal-ring-set-type>))
  `ring-set)

(define (register-symmetric-type! pre-ctx type)
  (let ((next-sym-type-idx (pre/next-sym-type-idx pre-ctx))
        (sym-type-table (pre/sym-type-table pre-ctx)))
    (cond
     ((sal-ast-table/get sym-type-table type) =>
      (lambda (entry)
        (let ((info (cdr entry)))
          (car info))))
     (else
      (let ((new-info (cons* next-sym-type-idx (sal-symmetric-type/layout-tag type) (sal-type/number-of-elements-as-integer type))))
        (sal-ast-table/put! sym-type-table type new-info)
        (pre/set-next-sym-type-idx! pre-ctx (+ next-sym-type-idx 1))
        (car new-info))))))

(define-method (sal-type/esm-symmetric-memory-layout (type <sal-symmetric-type>) (scm-ctx <sal-scm-context>) (pre-ctx <primitive>))
  (let ((sym-type-idx (register-symmetric-type! pre-ctx type))
        (num-bits (sal-bounded-subtype/num-bits-and-lower type scm-ctx)))
    (if (slot-value scm-ctx :gmp?)
      (mk-esm-sym-mpq-layout sym-type-idx num-bits)
      (mk-esm-sym-atom-layout sym-type-idx num-bits))))

(define-method (sal-type/esm-symmetric-memory-layout (type <sal-data-type>) (scm-ctx <sal-scm-context>) (pre-ctx <primitive>))
  (mk-esm-datatype-layout (sal-data-type/tag-num-bits type)
                          (apply vector (map (cut gen-constructor-symmetry-layout <> scm-ctx pre-ctx)
                                             (slot-value type :constructors)))))

(define (gen-constructor-symmetry-layout constructor sym-ctx pre-ctx)
  (let ((type (sal-name-expr/type constructor)))
    (if (sal-type/function? type)
      (let* ((domain (sal-function-type/domain type))
             (layout (sal-type/esm-symmetric-memory-layout domain sym-ctx pre-ctx)))
        (if (sal-type/tuple? domain)
          (vector->list layout)
          (list layout)))
      '())))

(define-generic (sal-domain/make-array-layout type body pre-ctx))
(define-method (sal-domain/make-array-layout (type <sal-scalar-set-type>) (body <primitive>) (pre-ctx <primitive>))
  (let ((idx (register-symmetric-type! pre-ctx type)))
    (mk-esm-scalar-set-array-layout idx body)))
(define-method (sal-domain/make-array-layout (type <sal-ring-set-type>) (body <primitive>) (pre-ctx <primitive>))
  (let ((idx (register-symmetric-type! pre-ctx type)))
    (mk-esm-ring-set-array-layout idx body)))
(define-method (sal-domain/make-array-layout (type <sal-type>) (body <primitive>) (pre-ctx <primitive>))
  body)

(define-method (sal-type/esm-symmetric-memory-layout (type <sal-function-type>) (scm-ctx <sal-scm-context>) (pre-ctx <primitive>))
  (let ((domain (sal-type/expand-if-type-name (slot-value type :domain))))
    (let* ((range-data (sal-type/esm-symmetric-memory-layout (slot-value type :range) scm-ctx pre-ctx))
           (domain-size (sal-type/number-of-elements-as-integer type))
           (layout-body (make-vector domain-size range-data)))
      (sal-domain/make-array-layout domain layout-body pre-ctx))))

(define-record-type esm-sym-atom-layout
  (mk-esm-sym-atom-layout type-idx num-bits)
  esm-sym-atom-layout?
  (type-idx esm-sym-atom-layout/type-idx)
  (num-bits esm-sym-atom-layout/num-bits))

(define-record-type esm-sym-mpq-layout
  (mk-esm-sym-mpq-layout type-idx num-bits)
  esm-sym-mpq-layout?
  (type-idx esm-sym-mpq-layout/type-idx)
  (num-bits esm-sym-mpq-layout/num-bits))

(define-record-type esm-scalar-set-array-layout
  (mk-esm-scalar-set-array-layout type-idx body)
  esm-scalar-set-array-layout?
  (type-idx esm-scalar-set-array-layout/type-idx)
  (body esm-scalar-set-array-layout/body))

(define-record-type esm-ring-set-array-layout
  (mk-esm-ring-set-array-layout type-idx body)
  esm-ring-set-array-layout?
  (type-idx esm-ring-set-array-layout/type-idx)
  (body esm-ring-set-array-layout/body))

(define-inline (esm-sym-atom-value->channel! value layout constraint-vect channel)
  (let* ((type-idx (esm-sym-atom-layout/type-idx layout))
         (num-bits (esm-sym-atom-layout/num-bits layout))
         (value (if (number? value) value 0))
         (normalized-value (symmetry-constraint-vector/normalize! constraint-vect type-idx value)))
    (sec/add-num! channel normalized-value num-bits)))

(define-inline (channel->esm-sym-atom-value! channel layout)
  (let ((num-bits (esm-sym-atom-layout/num-bits layout)))
    (sec/read-num! channel num-bits)))

(define-inline (esm-sym-mpq-value->channel! value layout constraint-vect channel)
  (let* ((type-idx (esm-sym-mpq-layout/type-idx layout))
         (num-bits (esm-sym-mpq-layout/num-bits layout))
         (value (if (mpq? value) (mpq->integer value) 0))
         (normalized-value (symmetry-constraint-vector/normalize! constraint-vect type-idx value)))
    (sec/add-num! channel normalized-value num-bits)))

(define-inline (channel->esm-sym-mpq-value! channel layout)
  (let ((num-bits (esm-sym-mpq-layout/num-bits layout)))
    (integer->mpq (sec/read-num! channel num-bits))))

(define (symmetric-var-vector->input-channel! var-vector first-idx layout symmetry-layout-context constraint-vect channel)
  (let loop ((value-vector var-vector)
             (layout layout)
             (first-idx first-idx))
    (let ((size (vector-length layout)))
      (let inner-loop ((idx 0))
        (when (< idx size)
          (let ((curr-value (vector-ref value-vector (+ idx first-idx)))
                (curr-layout (vector-ref layout idx)))
            ;; BOOLEANS
            ((eq? curr-layout 'boolean)
             [assert (curr-value) (or (boolean? curr-value) (eq? curr-value 'not-assigned))]
             (sec/add-bit! channel curr-value))
            ;; FINITE INTEGERS
            ((esm-atom-layout? curr-layout)
             [assert (curr-value) (or (number? curr-value) (eq? curr-value 'not-assigned))]
             (let ((lower (esm-atom-layout/lower curr-layout))
                   (num-bits (esm-atom-layout/num-bits curr-layout)))
               #unspecified))
            ))))))

(define-inline (int-cmp? val1 val2)
  (cond
   ((<fx val1 val2) -1)
   ((=fx val1 val2)  0)
   (else             1)))

(define-inline (vect-cmp? val1 val2 layout cmp?)
  [assert (val1) (vector? val1)]
  [assert (val2) (vector? val2)]
  (let ((n (vector-length val1)))
    (let loop ((idx 0))
      (if (>= idx n)
        0
        (case (cmp? (vector-ref val1 idx)
                    (vector-ref val2 idx)
                    (vector-ref layout idx))
          ((-1) -1)
          ((1)   1)
          ((0)  (loop (+ idx 1))))))))

(define-inline (symmetric-atom-cmp-core? val1 val2 type-idx layout constraint-vect)
  (let ((norm-val1 (symmetry-constraint-vector/normalized-value constraint-vect type-idx val1))
        (norm-val2 (symmetry-constraint-vector/normalized-value constraint-vect type-idx val2)))
    (cond 
     ((and (number? norm-val1) (number? norm-val2))
      (int-cmp? val1 val2)) ;; PRECISE
     ((number? norm-val1)
      [assert (norm-val2) (eq? norm-val2 'undef)]
      -1)                   ;; PRECISE
     ((number? norm-val2)
      [assert (norm-val1) (eq? norm-val1 'undef)]
      1)                    ;; PRECISE
     (else 
      (int-cmp? val1 val2)))))      ;; APPROXIMATION
  
(define-inline (symmetric-atom-cmp? val1 val2 layout constraint-vect)
  (symmetric-atom-cmp-core? val1 val2 (esm-sym-atom-layout/type-idx layout) layout constraint-vect))

(define-inline (symmetric-mpq-cmp? val1 val2 layout constraint-vect)
  (symmetric-atom-cmp-core? (esm/force-integer val1)
                            (esm/force-integer val2)
                            (esm-sym-mpq-layout/type-idx layout) 
                            layout constraint-vect))

(define-inline (scalar-set-array-cmp? val1 val2 layout constraint-vect cmp?)
  (let* ((type-idx (esm-scalar-set-array-layout/type-idx layout))
         (layout-vect (esm-scalar-set-array-layout/body layout))
         (constraint (vector-ref constraint-vect type-idx))
         (permutation (scalar-set-constraint/permutation constraint))
         (inv-permutation (scalar-set-constraint/inv-permutation constraint))
         (n (vector-length inv-permutation)))
    [assert (val1) (vector? val1)]
    [assert (val2) (vector? val2)]
    (let loop ((i 0))
      (if (>= i n)
        0 ;; PRECISE RESULT...
        (let ((j (vector-ref inv-permutation i)))
          (cond
           ((eq? j 'undef)
            ;; USING APPROXIMATION... compare all slots that are undef
            (let inner-loop ((k 0))
              (cond
               ((>= k n)
                0) 
               ((eq? (vector-ref permutation k) 'undef)
                (case (cmp? (vector-ref val1 k)
                            (vector-ref val2 k)
                            (vector-ref layout-vect k))
                  ((-1) -1)
                  ((1)   1)
                  ((0)   (inner-loop (+ k 1)))))
               (else
                [assert (permutation) (number? (vector-ref permutation k))]
                ;; Ignore this slot it was compared in the ounter loop
                (inner-loop (+ k 1))))))
           (else
            ;; PRECISE...
            (case (cmp? (vector-ref val1 j)
                        (vector-ref val2 j)
                        (vector-ref layout-vect j))
              ((-1) -1)
              ((1)   1)
              ((0)  (loop (+ i 1)))))))))))
  

(define-inline (ring-set-array-cmp? val1 val2 layout constraint-vect cmp?)
  (let* ((type-idx (esm-ring-set-array-layout/type-idx layout))
         (layout-vect (esm-ring-set-array-layout/body layout))
         (constraint (vector-ref constraint-vect type-idx))
         (delta (ring-set-constraint/delta constraint))
         (size (ring-set-constraint/size constraint)))
    (cond
     ((eq? delta 'undef)
      ;; IMPRECISE
      (vect-cmp? val1 val2 layout-vect cmp?))
     (else 
      ;; PRECISE
      (let loop ((i 0)
                 (idx delta))
        (if (>= i size)
          0
          (case (cmp? (vector-ref val1 idx)
                      (vector-ref val2 idx)
                      (vector-ref layout-vect idx))
            ((-1) -1)
            ((1)   1)
            ((0)  (loop (+ i 1)
                        (if (= idx (- size 1))
                          0
                          (+ idx 1)))))))))))

(define (list-cmp? val-list1 val-list2 layout-list cmp?)
  (cond
   ((null? val-list1)
    [assert (val-list2) (null? val-list2)]
    0)
   (else
    (case (cmp? (car val-list1) (car val-list2) (car layout-list))
      ((-1) -1)
      ((1)   1)
      ((0) (list-cmp? (cdr val-list1) (cdr val-list2) (cdr layout-list) cmp?))))))

(define-inline (datatype-cmp? val1 val2 layout cmp?)
  (case (int-cmp? (car val1) (car val2))
    ((-1) -1)
    ((1)   1)
    ((0) (let* ((tag-idx (car val1))
                (constructor-layout (vector-ref (esm-datatype-layout/constructor-vect layout) tag-idx)))
           (list-cmp? (cdr val1) (cdr val2) constructor-layout cmp?)))))

(define (sal-scm/cmp? val1 val2 layout layout-def-vect constraint-vect)
  (let cmp? ((val1 val1)
             (val2 val2)
             (layout layout))
    ;; (print "val1: " val1)
    ;; (print "val2: " val2)
    ;; (print "layout: " layout)
    (cond
     ;; BOOLEANS
     ((eq? layout 'boolean)
      [assert (val1 val2) (and (boolean? val1) (boolean? val2))]
      (cond 
       ((and (not val1) val2) -1)
       ((eq? val1 val2)        0)
       (else                   1)))
     ((esm-ref-layout? layout)
      (let ((def-layout (vector-ref layout-def-vect (esm-ref-layout/definition-idx layout))))
        (cmp? val1 val2 def-layout)))
     ;; INTEGERS
     ((or (esm-atom-layout? layout)
          (eq? layout 'infinite))
      [assert (val1 val2) (and (number? val1) (number? val2))]
      (int-cmp? val1 val2))
     ;; SCALAR-SETS and RING-SETS
     ((esm-sym-atom-layout? layout)
      (symmetric-atom-cmp? val1 val2 layout constraint-vect))
     ((esm-sym-mpq-layout? layout)
      (symmetric-mpq-cmp? val1 val2 layout constraint-vect))
     ;; RECORDS, TUPLES, ARRAYS
     ((vector? layout)
      (vect-cmp? val1 val2 layout cmp?))
     ;; SCALAR SET ARRAY
     ((esm-scalar-set-array-layout? layout)
      (scalar-set-array-cmp? val1 val2 layout constraint-vect cmp?))
     ;; RING SET ARRAY
     ((esm-ring-set-array-layout? layout)
      (ring-set-array-cmp? val1 val2 layout constraint-vect cmp?))
     ;; DATATYPES
     ((esm-datatype-layout? layout)
      (datatype-cmp? val1 val2 layout cmp?))
     ;; GMP NUMBERS
     ((or (esm-mpq-layout? layout)
          (eq? layout 'gmp-infinite))
      [assert (val1 val2) (and (mpq? val1) (mpq? val2))]
      (cond 
       ((<mpq val1 val2)  -1)
       ((=mpq val1 val2)   0)
       (else               1)))
     (else
      (internal-error)))))


(define (esm-scalar-set-array-pick-min value layout-vect permutation layout-def-vect constraint-vect)
  (let ((min #f)
        (n (vector-length layout-vect)))
    (let loop ((i 0))
      (when (< i n)
        (when (and (eq? (vector-ref permutation i) 'undef)
                   (or (not min)
                       (<fx (sal-scm/cmp? (vector-ref value i) (vector-ref value min)
                                          (vector-ref layout-vect i) layout-def-vect
                                          constraint-vect)
                            0)))
          (set! min i))
        (loop (+ i 1))))
    [assert (min value layout-def-vect permutation layout-vect constraint-vect) min]
    min))

(define (no-undef-in-vect? permutation)
  (vector/for-each (lambda (i)
                     [assert (i) (not (eq? i 'undef))]
                     #unspecified)
                   permutation)
  #t)

(define (has-undef-in-vect? permutation)
  (exists (cut eq? <> 'undef) (vector->list permutation)))

(define (num-undef permutation)
  (vector/fold (lambda (curr val)
                 (if (eq? val 'undef) (+ curr 1) curr))
               0
               permutation))

(define (esm-scalar-set-array->channel! value layout layout-def-vect constraint-vect proc-value)
  [assert (layout) (esm-scalar-set-array-layout? layout)]
  (let* ((type-idx (esm-scalar-set-array-layout/type-idx layout))
         (layout-vect (esm-scalar-set-array-layout/body layout))
         (constraint (vector-ref constraint-vect type-idx))
         (permutation (scalar-set-constraint/permutation constraint))
         (inv-permutation (scalar-set-constraint/inv-permutation constraint))
         (n (vector-length inv-permutation)))
    (cond
     ((eq? value 'not-assigned)
      (proc-value value layout-vect))
     (else
      [assert (value) (vector? value)]
      (let loop ((i 0))
        (when (< i n)
          (let ((j (vector-ref inv-permutation i)))
            (when (eq? j 'undef)
              (let ((min (esm-scalar-set-array-pick-min value layout-vect permutation 
                                                        layout-def-vect constraint-vect)))
                (symmetry-constraint-vector/normalize! constraint-vect type-idx min)
                [assert (j i inv-permutation) (not (eq? (vector-ref inv-permutation i) 'undef))]
                (set! j min)))
            (proc-value (vector-ref value j) (vector-ref layout-vect j))
            (loop (+ i 1)))))))))

(define-inline (next-ring-set-idx idx size)
  (let ((idx (+ idx 1)))
    (if (= idx size) 
      0
      idx)))

(define (esm-ring-set-array-pick-min value layout-vect layout-def-vect constraint-vect)
  ;; (print value " layout-vect: " layout-vect " layout-def-vect " layout-def-vect)
  (let ((n (vector-length layout-vect))
        (min 0)
        (layout (vector-ref layout-vect 0)))
    (let loop ((i 1))
      (when (< i n)
        (case (sal-scm/cmp? (vector-ref value i) (vector-ref value min)
                            layout layout-def-vect constraint-vect)
          ((-1) (set! min i))
          ((0) 
           ;; compare the successors
           ;; of min and i 
           (let inner-loop ((j 1)
                            (i1 (next-ring-set-idx i n))
                            (i2 (next-ring-set-idx min n)))
             (when (< j n)
               (case (sal-scm/cmp? (vector-ref value i1) (vector-ref value i2)
                                   layout layout-def-vect constraint-vect)
                 ((-1) (set! min i))
                 ((0) (inner-loop (+ j 1)
                                  (next-ring-set-idx i1 n)
                                  (next-ring-set-idx i2 n))))))))
        (loop (+ i 1))))
    min))
    

(define (esm-ring-set-array->channel! value layout layout-def-vect constraint-vect proc-value)
  (let* ((type-idx (esm-ring-set-array-layout/type-idx layout))
         (layout-vect (esm-ring-set-array-layout/body layout))
         (constraint (vector-ref constraint-vect type-idx))
         (delta (ring-set-constraint/delta constraint))
         (size (ring-set-constraint/size constraint)))
    (cond
     ((eq? value 'not-assigned)
      (proc-value value layout-vect))
     (else
      [assert (value) (vector? value)]
      (when (eq? delta 'undef)
        (let ((min (esm-ring-set-array-pick-min value layout-vect
                                                layout-def-vect constraint-vect)))
          ;; set constraint
          (symmetry-constraint-vector/normalize! constraint-vect type-idx min)))
      (let ((delta (ring-set-constraint/delta constraint)))
        [assert (delta) (not (eq? delta 'undef))]
        (let loop ((i 0)
                   (idx delta))
          (when (< i size)
            (proc-value (vector-ref value idx) (vector-ref layout-vect idx))
            (loop (+ i 1)
                  (next-ring-set-idx idx size)))))))))


(define (esm/normalize-symmetric-object value layout layout-def-vect constraint-vect)
  (let proc-value ((value value)
                   (layout layout))
    (cond
     ;; BOOLEANS, INTEGERS, GMP NUMBERS, INFINITE DATA 
     ((or (eq? layout 'boolean)
          (esm-atom-layout? layout)
          (esm-mpq-layout? layout)
          (eq? layout 'infinite)
          (eq? layout 'gmp-infinite))
      value)
     ;; TUPLES, RECORDS, AND ARRAYS
     ((vector? layout)
      [assert (value) (vector? value)]
      (let* ((value value)
             (size (vector-length layout))
             (new-value (make-vector size)))
        (let inner-loop ((i 0))
          (when (< i size)
            (vector-set! new-value i (proc-value (vector-ref value i) (vector-ref layout i)))
            (inner-loop (+ i 1))))
        new-value))
     ;; SCALAR SET AND RING SET ATOMS
     ((esm-sym-atom-layout? layout)
      (let ((type-idx (esm-sym-atom-layout/type-idx layout)))
        (symmetry-constraint-vector/normalize! constraint-vect type-idx value)))
     ;; SCALAR SET AND RING SET ATOMS WHEN GMP IS USED
     ((esm-sym-mpq-layout? layout)
      (let ((type-idx (esm-sym-mpq-layout/type-idx layout)))
        (integer->mpq (symmetry-constraint-vector/normalize! constraint-vect type-idx (mpq->integer value)))))
     ;; REFERENCE
     ((esm-ref-layout? layout)
      (proc-value value (vector-ref layout-def-vect (esm-ref-layout/definition-idx layout))))
     ;; DATATYPE
     ((esm-datatype-layout? layout)
      (if (null? (cdr value))
        value ;; constant constructor
        (cons (car value)
              (map proc-value (cdr value) (vector-ref (esm-datatype-layout/constructor-vect layout) (car value))))))
     ;; SCALAR SET ARRAY
     ((esm-scalar-set-array-layout? layout)
      (normalize-sym-array value layout layout-def-vect constraint-vect proc-value 
                           esm-scalar-set-array->channel!))
     ;; RING SET ARRAY
     ((esm-ring-set-array-layout? layout)
      (normalize-sym-array value layout layout-def-vect constraint-vect proc-value 
                           esm-ring-set-array->channel!))
     (else
      (internal-error)))))


(define (normalize-sym-array value layout layout-def-vect constraint-vect proc-value to-channel-proc)
  (let* ((size (vector-length value))
         (new-value (make-vector size))
         (i 0)
         (fake-proc-value (lambda (child-value child-layout)
                            (vector-set! new-value i (proc-value child-value child-layout))
                            (set! i (+ i 1)))))
    (to-channel-proc value layout layout-def-vect constraint-vect fake-proc-value)
    new-value))

(define (sal-type/symmetry-friendliness type)
  (sal-type/symmetry-friendliness-core type '()))
        
;; We compute the following level of friendliness
;; - lvl 0 (max friendliness)
;;    Type does not contain symmetric elements; 
;;    or, type does not contain an array indexed by symmetric elements
;;
;; - lvl 1
;;    Type contains an array indexed by symmetric elements, but
;;    the range does not contain symmetric elements      
;;
;; - lvl 2 (min friednliness)
;;    Type contains an array indexed by symmetric elements, and
;;    the range also contains symmetric elements.
;;
(define-generic (sal-type/symmetry-friendliness-core type rec-types))

(define-method (sal-type/symmetry-friendliness-core (type <sal-type>) (rec-types <primitive>))
  0)
(define-method (sal-type/symmetry-friendliness-core (type <sal-subtype>) (rec-types <primitive>))
  (sal-type/symmetry-friendliness-core (sal-subtype/immediate-super-type type) rec-types))
(define-method (sal-type/symmetry-friendliness-core (type <sal-tuple-type>) (rec-types <primitive>))
  (fold-left (lambda (result child-type)
               (max result (sal-type/symmetry-friendliness-core child-type rec-types)))
             0
             (slot-value type :types)))
(define-method (sal-type/symmetry-friendliness-core (type <sal-record-type>) (rec-types <primitive>))
  (fold-left (lambda (result field)
               (max result (sal-type/symmetry-friendliness-core (slot-value field :type) rec-types)))
             0
             (slot-value type :fields)))
(define-method (sal-type/symmetry-friendliness-core (type <sal-type-name>) (rec-types <primitive>))
  (if (not (exists (cut sal-ast/equivalent? type <>) rec-types))
    (let ((new-rec-types (if (sal-type-name/recursive? type)
                           (cons type rec-types)
                           rec-types))    
          (definition (sal-type-name/definition type)))
      (if definition
        (sal-type/symmetry-friendliness-core definition new-rec-types)
        0))
    0))
(define-method (sal-type/symmetry-friendliness-core (type <sal-data-type>) (rec-types <primitive>))
  (fold-left (lambda (result constructor)
               (max result (sal-constructor/symmetry-friendliness constructor rec-types)))
             0
             (slot-value type :constructors)))
(define (sal-constructor/symmetry-friendliness constructor rec-types)
  (let ((type (sal-name-expr/type constructor)))
    (if (sal-type/function? type)
      (sal-type/symmetry-friendliness-core (sal-function-type/domain type) rec-types)
      0)))
(define-method (sal-type/symmetry-friendliness-core (type <sal-function-type>) (rec-types <primitive>))
  (let* ((domain (slot-value type :domain))
         (range (slot-value type :range))
         (domain-contains-symmetric? (sal-type/uses-symmetric-type? domain))
         (range-contains-symmetric? (sal-type/uses-symmetric-type? range))
         (domain-lvl (sal-type/symmetry-friendliness-core domain rec-types))
         (range-lvl (sal-type/symmetry-friendliness-core range rec-types)))
    (max range-lvl
         domain-lvl
         (cond
          ((and domain-contains-symmetric? range-contains-symmetric?) 2)
          (domain-contains-symmetric? 
           [assert (range-contains-symmetric?) (not range-contains-symmetric?)]
           1)
          (else
           0)))))

(define-generic (sal-type/uses-symmetric-type? type))
(define-method (sal-type/uses-symmetric-type? (type <sal-type>))
  #f)
(define-method (sal-type/uses-symmetric-type? (type <sal-tuple-type>))
  (exists sal-type/uses-symmetric-type? (slot-value type :types)))
(define-method (sal-type/uses-symmetric-type? (type <sal-record-type>))
  (exists (lambda (field)
            (sal-type/uses-symmetric-type? (slot-value field :type)))
          (slot-value type :fields)))
(define-method (sal-type/uses-symmetric-type? (type <sal-subtype>))
  (sal-subtype/immediate-super-type type))
(define-method (sal-type/uses-symmetric-type? (type <sal-function-type>))
  (or (sal-type/uses-symmetric-type? (slot-value type :domain))
      (sal-type/uses-symmetric-type? (slot-value type :range))))
(define-method (sal-type/uses-symmetric-type? (type <sal-type-name>))
  (let ((definition (sal-type-name/definition type)))
    (and definition
         (sal-type/uses-symmetric-type? definition))))
(define-method (sal-type/uses-symmetric-type? (type <sal-scalar-set-type>))
  #t)
(define-method (sal-type/uses-symmetric-type? (type <sal-ring-set-type>))
  #t)
(define-method (sal-type/uses-symmetric-type? (type <sal-data-type>))
  (exists sal-constructor/uses-symmetric-type? (slot-value type :constructors)))
(define (sal-constructor/uses-symmetric-type? constructor)
  (let ((type (sal-name-expr/type constructor)))
    (if (sal-type/function? type)
      (sal-type/uses-symmetric-type? (sal-function-type/domain type))
      #f)))
  
