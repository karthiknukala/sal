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

(module sal-finite-expressions
        (include "sal.sch")
        (include "sal-ast-table.sch")
        (include "fast-hash-table.sch")
        (import sal-type sal-expression sal-finite-types gmp-scheme queue sal-environment sal-ast-attributes
                unique-names sal-ast-eq sal-ast-env symbol-table sal-module sal-expr-evaluator iterators
                sal-ast-simplify sal-decls sal-flat-modules sal-ast-copy runtime sal-ast-expand sal-pp
                symbol-set sal-component-info sal-flat-support)
        (export (sal-circuit-generator/set-maximum-constant-in-multiplication! max)
                (sal-ast->boolean-ast ast)
                (sal-expr->boolean-expr-core expr env aux-decl-queue)
                (sal-expr->boolean-expr expr env)
                (sal-type/finite-rep-membership-expr type env expr)
                (sal-type/convert-to source-type target-type bit-list)
                (sal-assertion->boolean-assertion-core assertion env)
                (sal-state-expr->boolean-state-expr expr env bool-flat-module)
                (bit-list->sal-value type bit-value-list)
                (gen-name-sequencer-proc)
                (sal-var-decl-list->sal-bool-var-decl-list var-decl-list unique-name-proc)
                (force-number-bit-list-size bit-list desired-num-bits signed? place-source)
                (force-bit-list-size bit-list desired-num-bits place-source))
        )

;--------------------------------------------------------------------
;
;  This file contains code that transforms SAL expressions in
;  a list of SAL boolean expressions. 
;  Of course, this transformation can only be applied over expressions
;  that use finite types.
;
;-------------------------------------------------------------------- 

(define *sal-circuit-generator-max-mul-constant* (make-mpq "1024"))

(define (sal-circuit-generator/set-maximum-constant-in-multiplication! max)
  (set! *sal-circuit-generator-max-mul-constant* (object->mpq max)))

;-----------------------------------------------------------------
;
;  SAL bit-list conversion
;
;  Two equivalent types may be represented by lists of boolean
;  expressions of different size.
;
;  For instance:
;  The type (tuple (subrange 0 7) (subrange 0 3))
;  is represented by a list of 5 boolean expressions, 3 bits for the first
;  and 2 bits for the second element in the tuple.
;  This type is compatible with the type (tuple (subrange 0 15) (subrange 0 15))
;  which is represented by a list of 8 boolean expressions. Thus, 
;  the function sal-type/convert-to performs the necessary adjustments.
;
;-----------------------------------------------------------------

(define-generic (sal-type/convert-to source-type target-type bit-list))

(define-method (sal-type/convert-to :around (source-type <sal-type>) (target-type <sal-type>) (bit-list <primitive>))
  (cond
   ((sal-ast/equivalent? source-type target-type)
    bit-list)
   (else
    (call-next-method))))

(define-inline (lower-bound bounded-type)
  [assert (bounded-type) (sal-type/bounded-subtype? bounded-type)]
  (evaluate-subrange-bound (sal-bounded-subtype/lower bounded-type)))
  
(define-inline (upper-bound bounded-type)
  [assert (bounded-type) (sal-type/bounded-subtype? bounded-type)]
  (evaluate-subrange-bound (sal-bounded-subtype/upper bounded-type)))

(define (sal-bounded-subtype/signed-number? type)
  ;; (breakpoint "sal-bounded-subtype/signed-number?" (type) #t)
  (tlet ((lower <sal-numeral> (lower-bound type)))
    (<mpq (slot-value lower :num) *mpq-zero*)))

(define-inline (add-sign-bit bit-list place-source)
  (append bit-list (list (make-sal-false place-source))))

(define-inline (invert-bit-list bit-list)
  (map make-sal-not+ bit-list))

(define (inc-bit-list bit-list aux-decl-queue)
  (let* ((place-source (car bit-list))
         (carry (make-sal-true place-source)))
    (let ((result-list
           (let loop ((bit-list bit-list)
                      (carry carry))
             (if (null? bit-list)
               '()
               (cons (make-sal-xor+ (car bit-list) carry)
                     (loop (cdr bit-list)
                           (make-bool-expr-alias! (make-sal-and+ (car bit-list) carry)
                                                  aux-decl-queue
                                                  'carry)))))))
      [assert (result-list bit-list) (= (length result-list) (length bit-list))]
      result-list)))

(define (two-complement-bit-list bit-list aux-decl-queue)
  (let ((result-list (if (null? bit-list)
                       '()
                       (inc-bit-list (invert-bit-list bit-list) aux-decl-queue))))
    [assert (result-list bit-list) (= (length result-list) (length bit-list))]
    result-list))

(define (abs-bit-list bit-list aux-decl-queue)
  (let* ((sign (make-bool-expr-alias! (bit-list-sign bit-list) aux-decl-queue))
         (place-provider sign)
         (pos-body-list (add-sign-bit (bit-list-body bit-list) place-provider))
         (neg-body-list (two-complement-bit-list bit-list aux-decl-queue))
         (_ [assert (pos-body-list neg-body-list) (= (length pos-body-list) (length neg-body-list))])
         (result-list (map (lambda (pos-bit neg-bit)
                             (make-bool-expr-alias! (make-sal-cond+ sign neg-bit pos-bit place-provider)
                                                    aux-decl-queue))
                           pos-body-list
                           neg-body-list)))
    [assert (bit-list result-list) (= (length result-list) (length bit-list))]
    result-list))

(define (invert-number-type type)
  (tlet* ((lower <sal-numeral> (lower-bound type))
          (upper <sal-numeral> (upper-bound type))
          (new-lower <sal-numeral> (update-ast-slots lower :num (-mpq *mpq-zero* (slot-value upper :num))))
          (new-upper <sal-numeral> (update-ast-slots upper :num (-mpq *mpq-zero* (slot-value lower :num)))))
    (make-sal-subrange new-lower new-upper)))

(define (invert-number-bit-list-and-type bit-list type aux-decl-queue)
  (if (null? bit-list) ;; zero, so I don't need to invert
    (values bit-list type)
    (let* ((signed? (sal-bounded-subtype/signed-number? type))
           (place-source type)
           (bit-list (if signed? 
                       bit-list
                       (add-sign-bit bit-list place-source)))
           (new-bit-list (two-complement-bit-list bit-list aux-decl-queue))
           (new-type (invert-number-type type))
           (num-bits (sal-type/finite-rep-num-bits new-type)))
      ;; (breakpoint "invert-number-bit-list-and-type" (bit-list new-bit-list new-type num-bits bit-list type aux-decl-queue) #t)
      (values (force-number-bit-list-size new-bit-list num-bits #t place-source)
              new-type))))

(define (bit-list-sign bit-list)
  (list-last-element bit-list))

(define (bit-list-body bit-list)
  [assert (bit-list) (not (null? bit-list))]
  (let loop ((bit-list bit-list))
    (if (null? (cdr bit-list))
      '()
      (cons (car bit-list) (loop (cdr bit-list))))))

(define (force-number-bit-list-size bit-list desired-num-bits signed? place-source)
  (if (= desired-num-bits 0)
    '()
    (let ((len (length bit-list)))
      (cond 
       ((= len desired-num-bits)
        bit-list)
       ((< len desired-num-bits)
        (let* ((bit-to-add (if signed? 
                             (bit-list-sign bit-list) 
                             (make-sal-false place-source)))
               (bits-to-append (generate-list (lambda (i) bit-to-add) (- desired-num-bits len))))
          (append bit-list bits-to-append)))
       ((and (> len desired-num-bits) (not signed?))
        (list-head bit-list desired-num-bits))
       (else
        [assert (len desired-num-bits signed?) (and (> len desired-num-bits) signed?)]
        [assert (desired-num-bits) (>= desired-num-bits 1)] 
        (append (list-head bit-list (- desired-num-bits 1)) (list (bit-list-sign bit-list))))))))

(define (force-bit-list-size bit-list desired-num-bits place-source)
  (force-number-bit-list-size bit-list desired-num-bits #f place-source))

(define (convert-number-bit-list source-type target-type bit-list)
  [assert (source-type bit-list) (= (sal-type/finite-rep-num-bits source-type) (length bit-list))]
  (if (sal-ast/equivalent? source-type target-type)
    bit-list
    (let* ((bounded-source-type (sal-type->bounded-subtype source-type))
           (bounded-target-type (sal-type->bounded-subtype target-type))
           (_ [assert (source-type target-type bounded-target-type bounded-source-type bit-list) 
                      (and bounded-source-type bounded-target-type)])
           (source-signed? (sal-bounded-subtype/signed-number? bounded-source-type))
           (target-signed? (sal-bounded-subtype/signed-number? bounded-target-type))
           (result-len (sal-type/finite-rep-num-bits bounded-target-type))
           (place-source (if (null? bit-list) bounded-source-type (car bit-list))))
      ;;[assert (source-type target-type) (imply source-signed? target-signed?)]
      (cond 
       ((eq? source-signed? target-signed?)
        (force-number-bit-list-size bit-list result-len source-signed? place-source))
       ((and source-signed? (not target-signed?))
        (force-number-bit-list-size (bit-list-body bit-list) result-len #f place-source))
       (else
        [assert (bounded-source-type bounded-target-type) (and (not source-signed?) target-signed?)]
        (force-number-bit-list-size (add-sign-bit bit-list place-source) result-len #t place-source))))))

(define-method (sal-type/convert-to (source-type <sal-bounded-subtype>) (target-type <sal-bounded-subtype>) (bit-list <primitive>))
  (convert-number-bit-list source-type target-type bit-list))

(define (convert-list source-type-list target-type-list bit-list)
  [assert (source-type-list target-type-list) (= (length source-type-list) (length target-type-list))]
  (let ((result (make-queue)))
    (let loop ((source-type-list source-type-list)
               (target-type-list target-type-list)
               (bit-list bit-list))
      (if (null? source-type-list)
        (queue->list result)
        (let* ((curr-source-type (car source-type-list))
               (curr-num-bits (sal-type/finite-rep-num-bits curr-source-type))
               (curr-bit-list (list-head bit-list curr-num-bits))
               (rest-bit-list (list-tail bit-list curr-num-bits))
               (curr-target-type (car target-type-list))
               (new-curr-bit-list (sal-type/convert-to curr-source-type curr-target-type curr-bit-list)))
          (queue/append! result new-curr-bit-list)
          (loop (cdr source-type-list)
                (cdr target-type-list)
                rest-bit-list))))))

(define-method (sal-type/convert-to (source-type <sal-tuple-type>) (target-type <sal-tuple-type>) (bit-list <primitive>))
  (convert-list (slot-value source-type :types) (slot-value target-type :types) bit-list))

(define-method (sal-type/convert-to (source-type <sal-record-type>) (target-type <sal-record-type>) (bit-list <primitive>))
  (convert-list (sal-field-list->sal-type-list (slot-value source-type :fields))
                (sal-field-list->sal-type-list (slot-value target-type :fields))
                bit-list))

(define-method (sal-type/convert-to (source-type <sal-type>) (target-type <sal-type>) (bit-list <primitive>))
  (sign-unsupported-feature source-type "Failed to convert finite representation of this type to one of the type located at ~a."
                            (format-with-location target-type "")))

(define (type-definition type)
  (let ((definition (sal-type-name/definition type)))
    (unless definition
      (sign-unsupported-feature type "Failed to convert finite representation, source type does not have an interpretation."))
    definition))

(define-method (sal-type/convert-to (source-type <sal-type-name>) (target-type <sal-type>) (bit-list <primitive>))
  (sal-type/convert-to (type-definition source-type) target-type bit-list))

(define-method (sal-type/convert-to (source-type <sal-type>) (target-type <sal-type-name>) (bit-list <primitive>))
  (sal-type/convert-to source-type (type-definition target-type) bit-list))

(define-method (sal-type/convert-to (source-type <sal-subtype>) (target-type <sal-type-name>) (bit-list <primitive>))
  (sal-type/convert-to source-type (type-definition target-type) bit-list))

(define-method (sal-type/convert-to (source-type <sal-function-type>) (target-type <sal-function-type>) (bit-list <primitive>))
  (try
   (let* ((source-domain (slot-value source-type :domain))
          (source-range (slot-value source-type :range))
          (source-num-domain-elems (sal-type/number-of-elements-as-integer source-domain))
          (source-num-range-bits (sal-type/finite-rep-num-bits source-range))
          (source-total-num-bits (* source-num-domain-elems source-num-range-bits))
          (target-domain (slot-value target-type :domain))
          (target-range (slot-value target-type :range))
          (target-num-domain-elems (sal-type/number-of-elements-as-integer target-domain)))
     (unless (= source-num-domain-elems target-num-domain-elems)
       (sign-source-error source-type "Failed to convert finite representation of this type to the type located at ~a. Reason: these types are incompatible. In the SAL language the domain of equivalent function (and array) types must be equal."))
     [assert (bit-list source-type source-total-num-bits) (= (length bit-list) source-total-num-bits)]
     (let ((result (make-queue)))
       (let loop ((i 0)
                  (bit-list bit-list))
         (if (< i source-num-domain-elems)
           (let* ((curr-bit-list (list-head bit-list source-num-range-bits))
                  (rest-bit-list (list-tail bit-list source-num-range-bits))
                  (new-curr-bit-list (sal-type/convert-to source-range target-range curr-bit-list)))
             (queue/append! result new-curr-bit-list)
             (loop (+ i 1) rest-bit-list))
           (queue->list result)))))
   (catch* '(expr-evaluator type-iterator)
           (lambda (_ msg)
             (sign-unsupported-feature source-type "Failed to convert this type with the one located at: ~a. The conversion failed because it was not possible to compute the number of elements in the domain (reason: ~a)" (format-with-location target-type "") msg)))))

(define-method (sal-type/convert-to (source-type <sal-subtype>) (target-type <sal-type>) (bit-list <primitive>))
  (sal-type/convert-to (sal-subtype/immediate-super-type source-type) target-type bit-list))

(define-method (sal-type/convert-to (source-type <sal-type>) (target-type <sal-subtype>) (bit-list <primitive>))
  (sal-type/convert-to source-type (sal-subtype/immediate-super-type target-type) bit-list))

(define-method (sal-type/convert-to (source-type <sal-subtype>) (target-type <sal-subtype>) (bit-list <primitive>))
  (cond
   ((and (sal-type/integer? source-type) 
         (sal-type/integer? target-type))
    (let ((b1 (sal-type->bounded-subtype source-type))
          (b2 (sal-type->bounded-subtype target-type)))
      (if (and b1 b2)
        (convert-number-bit-list b1 b2 bit-list)
        (sign-unsupported-feature source-type "Failed to convert finite boolean representation, from this type to the one located at ~a. Reason: one (or both) of the types is not finite."
                                  (format-with-location target-type "")))))
   (else
    (sal-type/convert-to (sal-type/super-type source-type) (sal-type/super-type target-type) bit-list))))

;-----------------------------------------------------------------
;
;  Type membership expressions
;
;  When an element of type is represented by a list of boolean
;  expressions, some combinations are invalid. For instance.
;  The type (subrange 0 5) is represented by 3 bits, where
;  two combinations are invalid (the ones that represent the 
;  values 6 and 7).
;  This unused (invalid) combinations may create problems, when
;  we try, for instance, prove a property by induction. Since,
;  a counterexample for the induction step may use this invalid
;  combinations.
;
;  The function sal-type/finite-rep-membership-expr generates
;  a boolean expression that is true iff a list of boolean
;  expression is a valid.
;  
;-----------------------------------------------------------------

(define-generic (sal-type/finite-rep-membership-expr type env expr))

(define-method (sal-type/finite-rep-membership-expr (type <sal-type>) (env <primitive>) (expr <sal-expr>))
  (sign-unsupported-feature type "Failed to generate type membership condition, reason: nonsupported type."))

(define-method (sal-type/finite-rep-membership-expr (type <sal-type-name>) (env <primitive>) (expr <sal-expr>))
  (let ((definition (sal-type-name/definition type)))
    (if definition
      (sal-type/finite-rep-membership-expr definition env expr)
      ;; always a member
      (make-sal-true expr))))

 (define (bits-to-shift mpq)
  (bind-exit (exit)
    (let loop ((mpq mpq))
      (cond
       ((<=mpq mpq *mpq-zero*)
        (exit #f))
       ((=mpq mpq *mpq-one*)
        0)
       ((=mpq (%mpq mpq *mpq-two*) *mpq-zero*)
        (+ (loop (div-mpq mpq *mpq-two*)) 1))
       (else
        (exit #f))))))

(define-inline (power-of-two? val)
  (bits-to-shift (object->mpq val)))

(define-method (sal-type/finite-rep-membership-expr (type <sal-scalar-type>) (env <primitive>) (expr <sal-expr>))
  (let* ((place-source expr)
         (num-scalar-elems (length (slot-value type :scalar-elements))))
    (if (power-of-two? num-scalar-elems)
      (make-sal-true type)
      (let* ((max-bit-list (number->bit-list (- num-scalar-elems 1) place-source))
             (aux-decl-queue (make-queue))
             (expr-bit-list (sal-expr->boolean-expr-core expr env aux-decl-queue))
             (body-expr (absolute-le expr-bit-list max-bit-list place-source aux-decl-queue)))
        (make-sal-let-expr (queue->list aux-decl-queue) body-expr)))))

(define-method (sal-type/finite-rep-membership-expr (type <sal-subtype>) (env <primitive>) (expr <sal-expr>))
  (let* ((pred (slot-value type :expr))
         (super-type (sal-subtype/immediate-super-type type))
         (cond-expr (sal-ast/simplify (sal-expr/lift pred expr (make-empty-env) 0)))
         (boolean-cond-expr (sal-expr->boolean-expr cond-expr env))
         (super-type-expr (sal-type/finite-rep-membership-expr super-type env expr)))
    (make-sal-and+ super-type-expr boolean-cond-expr)))

(define-method (sal-type/finite-rep-membership-expr (type <sal-tuple-type>) (env <primitive>) (expr <sal-expr>))
  (let ((conditions '())
        (place-source expr))
    (let loop ((type-list (slot-value type :types))
               (idx 1))
      (unless (null? type-list)
        (let ((selection (make-ast-instance <sal-tuple-selection> place-source
                                            :target expr
                                            :idx (make-sal-numeral idx place-source))))
          (push! (sal-type/finite-rep-membership-expr (car type-list) env selection) 
                 conditions)
          (loop (cdr type-list) (+ idx 1)))))
    (make-sal-and+* conditions place-source)))
                        
(define-method (sal-type/finite-rep-membership-expr (type <sal-record-type>) (env <primitive>) (expr <sal-expr>))
  (let ((conditions '())
        (place-source expr))
    (let loop ((field-list (slot-value type :fields)))
      (unless (null? field-list)
        (tlet* ((curr-field <sal-field> (car field-list))
                (selection <sal-record-selection> (make-ast-instance <sal-record-selection> place-source
                                                                     :target expr
                                                                     :idx (slot-value curr-field :id))))
          (push! (sal-type/finite-rep-membership-expr (slot-value curr-field :type) env selection)
                 conditions)
          (loop (cdr field-list)))))
    (make-sal-and+* conditions place-source)))

(define-method (sal-type/finite-rep-membership-expr (type <sal-function-type>) (env <primitive>) (expr <sal-expr>))
  (try
   (let* ((domain (slot-value type :domain))
          (range (slot-value type :range))
          (conditions '())
          (place-source expr)
          (it (sal-type/make-iterator domain)))
     (iterator/for-each (lambda (domain-value)
                          (let ((selection (sal-expr/apply expr domain-value)))
                            (push! (sal-type/finite-rep-membership-expr range env selection)
                                   conditions)))
                        it)
     (make-sal-and+* conditions place-source))
   (catch 'type-iterator
          (lambda (msg)
            (sign-unsupported-feature type "Failed to generate type membership condition, reason: ~a" msg)))))

(define-method (sal-type/finite-rep-membership-expr (type <sal-data-type>) (env <primitive>) (expr <sal-expr>))
  (let ((constructors (slot-value type :constructors))
        (place-source type)
        (alternatives '()))
    (for-each (lambda (constructor-name)
                (let* ((accessors (sal-name-expr/constructor-accessors constructor-name))
                       (recognizer (sal-name-expr/constructor-recognizer constructor-name))
                       ; (_ (breakpoint "membership datatype" (accessors recognizer type constructor-name expr) #t))
                       (recognizes? (sal-ast/simplify (sal-expr->boolean-expr (sal-expr/apply recognizer expr) env)))
                       (conditions '()))
                  (unless (sal-expr/false? recognizes?)
                    (for-each (lambda (accessor-name)
                                (let ((elem (sal-expr/apply accessor-name expr))
                                      (accessor-type (sal-name-expr/accessor-type accessor-name)))
                                  (push! (sal-type/finite-rep-membership-expr accessor-type env elem)
                                         conditions)))
                              accessors)
                    (push! (make-sal-and+* (cons recognizes? conditions) place-source)
                           alternatives))))
              constructors)
    (make-sal-or+* alternatives place-source)))

;-----------------------------------------------------------------
;
;  SAL expression -> SAL boolean expression
;
;  Converts a SAL expression in a list of boolean SAL expressions.
; 
;  the aux-decl-queue can be used to store auxiliary let declarations
;  that will be automatically placed somewhere. 
; 
;  The function sal-expr->boolean-expr-core returns two values:
;   - a list of boolean expressions
;   - a type which provides the interpretation for the list of boolean
;     expressions.
;
;-----------------------------------------------------------------

(define-generic (sal-expr->boolean-expr-core expr env aux-decl-queue))

;; This wrapper will automatically create a let expression to hold all auxiliary declarations generated
;; by sal-expr->boolean-expr-core. 
(define (sal-expr->boolean-expr expr env)
  [assert (expr) (sal-type/boolean? (sal-expr/type expr))]
  (let* ((aux-decl-queue (make-queue))
         (new-expr (car (sal-expr->boolean-expr-core expr env aux-decl-queue))))
    (make-sal-let-expr (queue->list aux-decl-queue) new-expr)))

(define-method (sal-expr->boolean-expr-core (expr <sal-expr>) (env <primitive>) (aux-decl-queue <primitive>))
  (sign-source-error expr "Failed to convert expression to boolean finite representation, this kind of expression is not supported by the translator."))

;; Convert a list of variable declarations in a list of boolean variable declarations.
;; For instance, the decl list:
;; x::(subrange 0 3) y::(tuple bool bool bool)
;; is mapped to
;; x!1::bool x!2::bool y!1::bool y!2::bool y!3::bool
;;
;; Two values are returned:
;; - a list of boolean declarations
;; - a list of pairs (bit-list, type), this list should be used to update the environment.
;;   this list has the same size of the original var-decl list. It shows how the variables
;;   were mapped
;;   For the example above this list will contain the pairs:
;;    ((x!1 x!2) (subrange 0 3))  ((y!1 y!2 y!3) (tuple bool bool bool))
(define (sal-var-decl-list->sal-bool-var-decl-list var-decl-list unique-name-proc)
  (let ((new-decls-queue (make-queue))
        (pairs-queue (make-queue)))
    (let loop ((var-decl-list var-decl-list))
      (if (null? var-decl-list)
        (values (queue->list new-decls-queue)
                (queue->list pairs-queue))
        (tlet ((curr-decl <sal-var-decl> (car var-decl-list)))
          (let* ((curr-id (slot-value curr-decl :id))
                 (curr-name (sal-identifier/name curr-id))
                 (curr-type (slot-value curr-decl :type))
                 (curr-num-bits (sal-type/finite-rep-num-bits curr-type))
                 (new-bit-queue (make-queue))) ;; I use this queue to build the pair (bit-list, type)
            (let inner-loop ((i 0))
              (when (< i curr-num-bits)
                (let* ((new-bit-name (unique-name-proc curr-name))
                       (new-bit-decl (copy-ast curr-decl
                                               :id (make-sal-identifier curr-id new-bit-name)
                                               :type (make-sal-builtin-name <sal-bool-type> curr-type)))
                       (new-bit-name (make-sal-name-expr new-bit-decl)))
                  (queue/insert! new-decls-queue new-bit-decl)
                  (queue/insert! new-bit-queue new-bit-name)
                  (inner-loop (+ i 1)))))
            (let ((new-pair (cons (queue->list new-bit-queue) curr-type)))
              (queue/insert! pairs-queue new-pair)
              (loop (cdr var-decl-list)))))))))

;; var-decl-list -> membership-expr
;;
;; Generate a condition that is true if and only if a boolean representation of var-decl-list elements 
;; is valid. The environment should contain a mapping from var-decl to a pair (bit-list, type).
;; these pairs are produced by the previous function.
;; For instance, the var-decl-list
;;   x::(subrange 0 2) y::(subrange 0 5)
;;   with env (x -> ((x!1 x!2) (subrange 0 2))) (y -> ((y!1 y!2 y!3) (subrange 0 5)))
;; will produce the condition
;;  
;;  (x!1 x!2) >= 0x00 AND (x!1 x!2) <= 0x10 AND (y!1 y!2 y!3) >= 0x00 (y!1 y!2 y!3) <= 0x101
;; Remark: The operators >= and <= used here, are `macros' for their boolean representation
;;
(define (var-decl-list->membership-expr var-decl-list env)
  (apply make-sal-and+ (map (lambda (var-decl)
                              (let* ((name-expr (make-sal-name-expr var-decl))
                                     (expr (sal-type/finite-rep-membership-expr (slot-value var-decl :type) env name-expr)))
                                (sal-ast/simplify expr)))
                            var-decl-list)))

;; This function implements a template for for-all and exists 
;; mk-body receives two arguments membership-expr and body-expr
;; for <sal-for-all-expr> it must return  (or (not membership-expr) body-expr)
;; for <sal-exists-expr> it must return (and membership-expr body-expr)
(define (sal-quantified-expr>boolean-quantified-expr expr env aux-decl-queue mk-body)
  (let ((local-decls (slot-value expr :local-decls)))
    (multiple-value-bind
        (new-local-decls bit-list-type-list)
        (sal-var-decl-list->sal-bool-var-decl-list local-decls gen-unique-name)
      (let* ((env (update-env* env local-decls bit-list-type-list))
             (membership-expr (var-decl-list->membership-expr local-decls env))
             (pre-new-nested-expr (car (sal-expr->boolean-expr-core (slot-value expr :expr) env aux-decl-queue)))
             (new-nested-expr (mk-body membership-expr pre-new-nested-expr))
             ;; aux-decl-queue contains auxiliary let decls... I have to remove the ones that reference
             ;; new-local-decls... and use them to create a let-expr
             (removed-let-decls (remove-dependent-aux-let-decls! aux-decl-queue new-local-decls))
             (new-expr (make-sal-let-expr removed-let-decls new-nested-expr))
             (result-expr (copy-ast expr
                                    :local-decls new-local-decls
                                    :expr new-expr)))
        (values (list result-expr)
                (sal-expr/type expr))))))

(define-method (sal-expr->boolean-expr-core (expr <sal-exists-expr>) (env <primitive>) (aux-decl-queue <primitive>))
  (sal-quantified-expr>boolean-quantified-expr expr env aux-decl-queue
                                               (lambda (membership-expr body-expr)
                                                 (make-sal-and+ membership-expr body-expr))))

(define-method (sal-expr->boolean-expr-core (expr <sal-for-all-expr>) (env <primitive>) (aux-decl-queue <primitive>))
  (sal-quantified-expr>boolean-quantified-expr expr env aux-decl-queue
                                               (lambda (membership-expr body-expr)
                                                 (make-sal-or+ (make-sal-not+ membership-expr) body-expr))))

(define (check-if-finite-number-type type)
  (unless (sal-type/bounded-subtype? type)
    (sign-source-error type "Failed to generate circuit for arithmetic operation, type is not finite.")))

(define (number->bit-list num place-source)
  (let ((num (object->mpq num)))
    (unless (mpq/integer? num)
      (sign-error "This tool does not support rational numbers: ~a." (mpq->string num)))
    (let ((bit-list (let loop ((val num))
                      (if (=mpq val *mpq-zero*)
                        '()
                        (let ((curr-bit (if (=mpq (%mpq val *mpq-two*) *mpq-zero*)
                                          (make-sal-false place-source)
                                          (make-sal-true place-source)))
                              (rest (loop (div-mpq val *mpq-two*))))
                          (cons curr-bit rest))))))
      bit-list)))

(define (sal-numeral/gen-circuit numeral aux-decl-queue)
  (let* ((value (slot-value numeral :num))
         (signed? (<mpq value *mpq-zero*))
         (abs-value (mpq/absolute value))
         (place-source numeral)
         (bit-list (number->bit-list abs-value place-source))
         (lower (make-sal-numeral *mpq-zero* place-source))
         (upper (make-sal-numeral abs-value place-source))
         (result-type (make-sal-subrange lower upper))
         (num-bits (sal-type/finite-rep-num-bits result-type))
         (result-bit-list (force-number-bit-list-size bit-list num-bits #f numeral)))
    (if signed?
      (invert-number-bit-list-and-type result-bit-list result-type aux-decl-queue)
      (values result-bit-list result-type))))

(define-method (sal-expr->boolean-expr-core (expr <sal-numeral>) (env <primitive>) (aux-decl-queue <primitive>))
  (sal-numeral/gen-circuit expr aux-decl-queue))

(define-method (sal-expr->boolean-expr-core (constructor <sal-constructor>) (env <primitive>) (aux-decl-queue <primitive>))
  (unless (null? (sal-name-expr/constructor-accessors constructor))
    (sign-unsupported-feature constructor "Failed to convert finite representation, invalid use of non-constant datatype constructor."))
  (let* ((idx (sal-constructor/idx constructor))
         (bit-list (number->bit-list idx constructor))
         (result-type (sal-type/expand-if-type-name (sal-expr/type constructor)))
         (num-bits (sal-type/finite-rep-num-bits result-type))
         (result-bit-list (force-bit-list-size bit-list num-bits constructor)))
    (values result-bit-list result-type)))

(define-method (sal-expr->boolean-expr-core (scalar <sal-scalar>) (env <primitive>) (aux-decl-queue <primitive>))
  (let* ((idx (sal-scalar-element/idx scalar))
         (bit-list (number->bit-list idx scalar))
         (result-type (sal-expr/type scalar))
         (num-bits (sal-type/finite-rep-num-bits result-type))
         (result-bit-list (force-number-bit-list-size bit-list num-bits #f scalar)))
    (values result-bit-list result-type)))

(define-method (sal-expr->boolean-expr-core (expr <sal-name-expr>) (env <primitive>) (aux-decl-queue <primitive>))
  (cond
   ((lookup-env (slot-value expr :decl) env) =>
    (lambda (bit-list-and-type)
      (values (car bit-list-and-type) (cdr bit-list-and-type))))
   ((sal-name-expr/definition expr) =>
    (lambda (definition)
      (sal-expr->boolean-expr-core definition env aux-decl-queue)))
   (else
;     (let ((decl (slot-value expr :decl)))
;       (cond
;        ((instance-of? decl <sal-constant-decl>)
    ;; (breakpoint "name-expr" (expr env) #t)
    (sign-unsupported-feature expr "Failed to convert name expression `~a' to a finite boolean representation. In the current version, sal-bmc/sal-smc do not support uninterpreted constants." (sal-name-ref/name expr)))))

(define-method (sal-expr->boolean-expr-core (expr <sal-next-operator>) (env <primitive>) (aux-decl-queue <primitive>))
  (multiple-value-bind
      (name-expr-bit-list name-expr-type)
      (sal-expr->boolean-expr-core (slot-value expr :name-expr) env aux-decl-queue)
    (let ((new-bit-list (map (lambda (bit)
                              (copy-ast expr
                                        :name-expr bit))
                             name-expr-bit-list)))
      (values new-bit-list name-expr-type))))

(define-method (sal-expr->boolean-expr-core (expr <sal-propositional-application>) (env <primitive>) (aux-decl-queue <primitive>))
  (sal-flattener/keep-boolean-application expr (cut sal-expr->boolean-expr-core <> env aux-decl-queue)))

(define-method (sal-expr->boolean-expr-core (expr <sal-temporal-application>) (env <primitive>) (aux-decl-queue <primitive>))
  (sal-flattener/keep-boolean-application expr (cut sal-expr->boolean-expr-core <> env aux-decl-queue)))

(define (sal-eq/gen-circuit-core type1 type2 bit-list1 bit-list2 eq-proc combine-proc)
  (let* ((result-type (sal-type/union-finite-rep type1 type2))
         (bit-list1 (sal-type/convert-to type1 result-type bit-list1))
         (bit-list2 (sal-type/convert-to type2 result-type bit-list2)))
    [assert (type1 type2 result-type bit-list1 bit-list2) (= (length bit-list1) (length bit-list2))]
    (values
     (list (combine-proc (map (lambda (bit1 bit2)
                                (eq-proc bit1 bit2))
                              bit-list1
                              bit-list2)))
     (make-sal-builtin-name <sal-bool-type> type1))))
          
(define (sal-eq->boolean-expr-core expr env eq-proc combine-proc aux-decl-queue)
  ;; (sal/pp expr) (print "")
  (multiple-value-bind 
      (arg1 arg2)
      (sal-binary-application/arguments expr)
    (multiple-value-bind
        (arg1-bit-list arg1-type)
        (sal-expr->boolean-expr-core arg1 env aux-decl-queue)
      (multiple-value-bind
          (arg2-bit-list arg2-type)
          (sal-expr->boolean-expr-core arg2 env aux-decl-queue)
        (let* ((result-type (sal-type/union-finite-rep arg1-type arg2-type))
               (bit-list1 (sal-type/convert-to arg1-type result-type arg1-bit-list))
               (bit-list2 (sal-type/convert-to arg2-type result-type arg2-bit-list)))
          [assert (arg1-type arg2-type result-type bit-list1 bit-list2) (= (length bit-list1) (length bit-list2))]
          (values
           (list (apply combine-proc (map (lambda (bit1 bit2)
                                            (eq-proc bit1 bit2))
                                          bit-list1
                                          bit-list2)))
           (make-sal-builtin-name <sal-bool-type> arg1-type)))))))

(define-method (sal-expr->boolean-expr-core (expr <sal-eq>) (env <primitive>) (aux-decl-queue <primitive>))
  (let ((place-source expr))
    (sal-eq->boolean-expr-core expr env
                               (lambda (arg1 arg2)
                                 (copy-ast expr
                                           :arg (make-application-argument arg1 arg2)))
                               (lambda args
                                 (make-sal-and+* args place-source))
                               aux-decl-queue)))
  
(define-method (sal-expr->boolean-expr-core (expr <sal-assignment>) (env <primitive>) (aux-decl-queue <primitive>))
  (multiple-value-bind
      (lhs rhs)
      (sal-binary-application/arguments expr)
    (cond
     ((sal-lhs/ground? lhs)
      (multiple-value-bind
          (lhs-bit-list lhs-type)
          (sal-expr->boolean-expr-core lhs env aux-decl-queue)
        (multiple-value-bind
            (rhs-bit-list rhs-type)
            (sal-expr->boolean-expr-core rhs env aux-decl-queue)
          (let* ((result-type (sal-type/union-finite-rep lhs-type rhs-type))
                 (_ [sal-assert "assignment1" (lhs lhs-type result-type lhs-bit-list) 
                                (= (sal-type/finite-rep-num-bits lhs-type) (length lhs-bit-list))))
                 (__ [sal-assert "assignment2" (rhs rhs-type result-type rhs-bit-list) 
                                 (= (sal-type/finite-rep-num-bits rhs-type) (length rhs-bit-list))))
                 (comp-list (let loop ((lhs-bit-list (sal-type/convert-to lhs-type result-type lhs-bit-list))
                                       (rhs-bit-list (sal-type/convert-to rhs-type result-type rhs-bit-list)))
                              [assert (lhs rhs lhs-type rhs-type env aux-decl-queue lhs-bit-list rhs-bit-list) 
                                      (= (length lhs-bit-list) (length rhs-bit-list))]
                              (if (null? lhs-bit-list)
                                '()
                                (let ((curr-lhs-bit (car lhs-bit-list))
                                      (curr-rhs-bit (car rhs-bit-list))
                                      (rest-list (loop (cdr lhs-bit-list) (cdr rhs-bit-list))))
                                  ;; if curr-lhs-bit is not a var-name or a next, then it can be ignored.
                                  ;; REMARK: this is unsound if the code is not type correct!!!
                                  ;; example: x' = x + 1, where x is a (subrange 0 3)
                                  ;; The boolean representation produced will make x' = 0 if x = 3
                                  (if (or (instance-of? curr-lhs-bit <sal-next-operator>)
                                          (and (instance-of? curr-lhs-bit <sal-name-expr>)
                                               (sal-name-ref/state? curr-lhs-bit)))
                                    (cons (copy-ast expr
                                                    :arg (make-application-argument curr-lhs-bit curr-rhs-bit))
                                          rest-list)
                                    rest-list)))))
                 ; (_ (breakpoint "sal-assignment to bool" (comp-list lhs-bit-list rhs-bit-list expr) (null? comp-list)))
                 (place-source expr)
                 (result-expr (make-sal-and+* comp-list place-source)))
            (values (list result-expr) (make-sal-builtin-name <sal-bool-type> expr))))))
     (else
      ;; the lhs is not ground... so, I convert the assignment to a definition-expression, and call 
      ;; sal-expr->boolean-expr-core again
      (let ((def-expr (make-ast-instance <sal-definition-expression> expr
                                         :lhs-list (list lhs)
                                         :expr (change-ast-class expr <sal-eq>))))
        (sal-expr->boolean-expr-core def-expr env aux-decl-queue))))))

(define-method (sal-expr->boolean-expr-core (expr <sal-diseq>) (env <primitive>) (aux-decl-queue <primitive>))
  (sal-eq->boolean-expr-core expr env
                             (lambda (arg1 arg2)
                               (copy-ast expr
                                         :arg (make-application-argument arg1 arg2)))
                             (lambda args
                               (if (null? args)
                                 (make-sal-false expr)
                                 (apply make-sal-or+ args)))
                             aux-decl-queue))

(define-method (sal-expr->boolean-expr-core (expr <sal-conditional>) (env <primitive>) (aux-decl-queue <primitive>))
  (multiple-value-bind
      (cond-bit-list cond-type)
      (sal-expr->boolean-expr-core (slot-value expr :cond-expr) env aux-decl-queue)
    (multiple-value-bind
        (then-bit-list then-type)
        (sal-expr->boolean-expr-core (slot-value expr :then-expr) env aux-decl-queue)
      (multiple-value-bind
          (else-bit-list else-type)
          (sal-expr->boolean-expr-core (slot-value expr :else-expr) env aux-decl-queue)
        (let* ((place-provider expr)
               (result-type (sal-type/union-finite-rep then-type else-type))
               (new-then-bit-list (sal-type/convert-to then-type result-type then-bit-list))
               (new-else-bit-list (sal-type/convert-to else-type result-type else-bit-list))
               (cond-expr (make-bool-expr-alias! (car cond-bit-list)
                                            aux-decl-queue
                                            'cond)))
          [assert (expr cond-type) (sal-type/boolean? cond-type)]
          [assert (expr new-then-bit-list new-else-bit-list) (= (length new-then-bit-list) (length new-else-bit-list))]
          (let ((result-bit-list (map (lambda (then-bit else-bit)
                                        (make-sal-cond+ cond-expr then-bit else-bit place-provider))
                                      new-then-bit-list
                                      new-else-bit-list)))
            (with-output-to-trace 'finite-expr 
                                  (trace 'finite-expr "converting conditional")
                                  (sal/pp expr)
                                  (print "")
                                  (print "result =")
                                  (sal/pp result-bit-list)
                                  (print "")
                                  (trace 'finite-expr "======================"))
            (values result-bit-list
                    result-type)))))))

(define-method (sal-expr->boolean-expr-core (expr <sal-tuple-literal>) (env <primitive>) (aux-decl-queue <primitive>))
  (let ((result-bits (make-queue))
        (result-types (make-queue)))
    (for-each (lambda (curr-expr)
                (multiple-value-bind
                    (expr-bit-list expr-type)
                    (sal-expr->boolean-expr-core curr-expr env aux-decl-queue)
                  (queue/append! result-bits expr-bit-list)
                  (queue/insert! result-types expr-type)))
              (slot-value expr :exprs))
    (values (queue->list result-bits) 
            (make-ast-instance <sal-tuple-type> expr
                               :types (queue->list result-types)))))

(define-method (sal-expr->boolean-expr-core (expr <sal-record-literal>) (env <primitive>) (aux-decl-queue <primitive>))
  (let ((result-bits (make-queue))
        (result-fields (make-queue)))
    (for-each (lambda (entry)
                (ensure ((entry <sal-record-entry>))
                  (multiple-value-bind
                      (entry-bit-list entry-type)
                      (sal-expr->boolean-expr-core (slot-value entry :expr) env aux-decl-queue)
                    (queue/append! result-bits entry-bit-list)
                    (queue/insert! result-fields (make-ast-instance <sal-field> entry
                                                                    :id (slot-value entry :id)
                                                                    :type entry-type)))))
              (slot-value expr :entries))
    (values (queue->list result-bits)
            (make-ast-instance <sal-record-type> expr
                               :fields (queue->list result-fields)))))

(define-method (sal-expr->boolean-expr-core (expr <sal-lambda>) (env <primitive>) (aux-decl-queue <primitive>))
  (try
   (let* ((fun-type (sal-expr/type expr))
          (domain (sal-function-type/domain fun-type))
          (it (sal-type/make-iterator domain))
          (range-bit-lists (make-queue))
          (range-types (make-queue))
          (computed-range-type #f))
;    (sal/pp expr) (print "")
     (iterator/for-each (lambda (domain-value)
                          (let ((range-value (sal-ast/simplify (sal-expr/apply expr domain-value))))
;                             (sal/pp domain-value)
;                             (display " -> ")
;                             (sal/pp range-value)
;                             (print "")
                            (multiple-value-bind
                                (range-value-bit-list range-value-type)
                                (sal-expr->boolean-expr-core range-value env aux-decl-queue)
                              (set! computed-range-type (if computed-range-type
                                                          (sal-type/union-finite-rep computed-range-type range-value-type)
                                                          range-value-type))
                              (queue/insert! range-bit-lists range-value-bit-list)
                              (queue/insert! range-types range-value-type))))
                        it)
;    (print "---------------")
     (let ((result-bits (make-queue))
           (result-type (make-ast-instance <sal-function-type> expr
                                           :domain domain 
                                           :range computed-range-type)))
       (for-each (lambda (range-bit-list range-type)
                   (let ((new-range-bit-list (sal-type/convert-to range-type computed-range-type range-bit-list)))
                     (for-each (lambda (bit)
                                 (queue/insert! result-bits (sal-ast/simplify bit)))
                               new-range-bit-list)))
                 (queue->list range-bit-lists)
                 (queue->list range-types))
       (values (queue->list result-bits)
               result-type)))
   (catch 'type-iterator
          (lambda (msg)
            (sign-unsupported-feature expr "Failed to convert function (or array) to finite boolean representation, reason: ~a" msg)))))

(define-method (sal-expr->boolean-expr-core (expr <sal-array-literal>) (env <primitive>) (aux-decl-queue <primitive>))
  (multiple-value-bind
      (bit-list type)
      (call-next-method)
    (values bit-list (quick-change-class! type <sal-array-type>))))

(define-method (sal-expr->boolean-expr-core (expr <sal-tuple-selection>) (env <primitive>) (aux-decl-queue <primitive>))
  (multiple-value-bind
      (tuple-bit-list tuple-type)
      (sal-expr->boolean-expr-core (slot-value expr :target) env aux-decl-queue)
    (let ((pos (- (sal-tuple-position->integer (slot-value expr :idx)) 1))
          (type-list (sal-tuple-type/types tuple-type)))
      (let loop ((i 0)
                 (type-list type-list)
                 (bit-list tuple-bit-list))
        (when (null? type-list)
          (sign-source-error expr "Invalid tuple selection."))
        (let* ((curr-type (car type-list))
               (curr-num-bits (sal-type/finite-rep-num-bits curr-type)))
          (if (= i pos)
            (values (list-head bit-list curr-num-bits) curr-type)
            (loop (+ i 1) (cdr type-list) (list-tail bit-list curr-num-bits))))))))

(define-method (sal-expr->boolean-expr-core (expr <sal-record-selection>) (env <primitive>) (aux-decl-queue <primitive>))
  (multiple-value-bind
      (record-bit-list record-type)
      (sal-expr->boolean-expr-core (slot-value expr :target) env aux-decl-queue)
    (let ((field-name (slot-value (slot-value expr :idx) :name))
          (field-list (sal-record-type/fields record-type)))
      (let loop ((field-list field-list)
                 (bit-list record-bit-list))
        (when (null? field-list)
          (sign-source-error expr "Invalid record selection."))
        (tlet* ((curr-field <sal-field> (car field-list))
                (curr-type <sal-type> (slot-value curr-field :type))
                (curr-num-bits <primitive> (sal-type/finite-rep-num-bits curr-type))
                (curr-id <sal-identifier> (slot-value curr-field :id))
                (curr-field-name <primitive> (slot-value curr-id :name)))
          (if (eq? curr-field-name field-name)
            (values (list-head bit-list curr-num-bits) curr-type)
            (loop (cdr field-list) (list-tail bit-list curr-num-bits))))))))
      
(define (evaluate-or-false expr)
  (try
   (sal-expr/evaluate expr)
   (catch 'expr-evaluator
          (lambda (_)
            #f))))

(define (sal-function-application->boolean-expr-core expr env aux-decl-queue)
  (ensure ((expr <sal-application>))
    (try
     (multiple-value-bind
         (function-bit-list function-type)
         (sal-expr->boolean-expr-core (slot-value expr :fun) env aux-decl-queue)
       (let* ((domain (sal-function-type/domain function-type))
              (range (sal-function-type/range function-type))
              (range-num-bits (sal-type/finite-rep-num-bits range))
              (it (sal-type/make-iterator domain))
              (arg (evaluate-or-false (slot-value expr :arg))))
         (unless (iterator/has-next? it)
           (sign-source-error (slot-value expr :fun) "Invalid function (or array), the domain is empty."))
         (if (and arg (sal-expr/first-order-value? arg))
           (let ((value-idx (sal-type/value-idx domain arg)))
             (if (not value-idx)
		 (sign-unsupported-feature expr "Failed to convert function application (array selection). The function/array does not have a finite domain, or the argument is not in the domain of the function/array.")
               (let ((bit-list (list-head (list-tail function-bit-list (* value-idx range-num-bits)) range-num-bits)))
                 (values bit-list range))))
           ;; generate expensive representation
           (multiple-value-bind 
               (arg-bit-list arg-type)
               (sal-expr->boolean-expr-core (slot-value expr :arg) env aux-decl-queue)
             (let ((bit-list (list-head function-bit-list range-num-bits)))
               (set! function-bit-list (list-tail function-bit-list range-num-bits))
               (iterator/next! it)
               (iterator/for-each (lambda (domain-value)
                                    (multiple-value-bind
                                        (domain-value-bit-list domain-value-type)
                                        (sal-expr->boolean-expr-core domain-value env aux-decl-queue)
                                      (let* ((union-type (sal-type/union-finite-rep arg-type domain-value-type))
                                             (new-arg-bit-list (sal-type/convert-to arg-type union-type arg-bit-list))
                                             (new-domain-value-bit-list (sal-type/convert-to domain-value-type union-type domain-value-bit-list))
                                             (place-provider expr)
                                             (condition (make-bool-expr-alias! (make-sal-and+* (map (lambda (new-arg-bit new-domain-bit)
                                                                                                      (make-sal-equality+ new-arg-bit new-domain-bit))
                                                                                                    new-arg-bit-list
                                                                                                    new-domain-value-bit-list)
                                                                                               place-provider)
                                                                               aux-decl-queue
                                                                               'cond))
                                             (next-bit-list (list-head function-bit-list range-num-bits))
                                             (new-bit-list (map (lambda (curr-bit next-bit)
                                                                  (make-sal-cond+ condition next-bit curr-bit expr))
                                                                bit-list
                                                                next-bit-list)))
                                        (set! bit-list new-bit-list)
                                        (set! function-bit-list (list-tail function-bit-list range-num-bits)))))
                                  it)
               (values bit-list range))))))
     (catch 'type-iterator
            (lambda (msg)
              (sign-unsupported-feature expr "Failed to convert function application (or array selection) to finite boolean representation, reason: ~a" msg))))))

(define-method (sal-expr->boolean-expr-core (expr <sal-array-selection>) (env <primitive>) (aux-decl-queue <primitive>))
  (sal-function-application->boolean-expr-core expr env aux-decl-queue))

(define-method (sal-expr->boolean-expr-core (expr <sal-debug-application>) (env <primitive>) (aux-decl-queue <primitive>))
  (sal-expr->boolean-expr-core (list-last-element (sal-application/argument-list expr)) env aux-decl-queue))

(define-method (sal-expr->boolean-expr-core (expr <sal-constructor-application>) (env <primitive>) (aux-decl-queue <primitive>))
  (let* ((constructor (slot-value expr :fun))
         (idx (sal-constructor/idx constructor))
         (constructor-type (sal-expr/type constructor))
         (domain (sal-function-type/domain constructor-type))
         (data-type (sal-type/expand-if-type-name (sal-function-type/range constructor-type)))
         (num-tag-bits (sal-data-type/finite-rep-num-tag-bits data-type))
         (num-bits (sal-type/finite-rep-num-bits data-type))
         (idx-bit-list (number->bit-list idx expr))
         (tag-bit-list (force-bit-list-size idx-bit-list num-tag-bits expr)))
    (multiple-value-bind
        (arg-bit-list arg-type)
        (sal-expr->boolean-expr-core (slot-value expr :arg) env aux-decl-queue)
      (let* ((body-bit-list (sal-type/convert-to arg-type domain arg-bit-list))
             (tag-and-body-bit-list (append tag-bit-list body-bit-list))
             (result-bit-list (force-bit-list-size tag-and-body-bit-list num-bits expr)))
        (values result-bit-list data-type)))))

(define-method (sal-expr->boolean-expr-core (expr <sal-recognizer-application>) (env <primitive>) (aux-decl-queue <primitive>))
  (let* ((place-provider expr)
         (recognizer (slot-value expr :fun))
         (recognizer-idx (sal-recognizer/idx recognizer))
         (data-type (sal-type/expand-if-type-name (sal-function-type/domain (sal-expr/type recognizer))))
         (num-tag-bits (sal-data-type/finite-rep-num-tag-bits data-type))
         (idx-bit-list (number->bit-list recognizer-idx expr))
         (tag-bit-list (force-bit-list-size idx-bit-list num-tag-bits expr)))
    (multiple-value-bind
        (arg-bit-list arg-type)
        (sal-expr->boolean-expr-core (slot-value expr :arg) env aux-decl-queue)
      [assert (arg-type data-type) (sal-type/equivalent? arg-type data-type)]
      [assert (arg-type data-type) (= (sal-type/finite-rep-num-bits arg-type)
                                      (sal-type/finite-rep-num-bits data-type))]
      (let* ((arg-tag-bits (list-head arg-bit-list num-tag-bits))
             (result-bit (make-sal-and+* (map make-sal-equality+ arg-tag-bits tag-bit-list) place-provider))
             (result-type (make-sal-builtin-name <sal-bool-type> expr)))
        (values (list result-bit) result-type)))))

(define-method (sal-expr->boolean-expr-core (expr <sal-accessor-application>) (env <primitive>) (aux-decl-queue <primitive>))
  (let* ((accessor (slot-value expr :fun))
         (accessor-list (sal-name-expr/accessor-other-accessors accessor))
         (accessor-type (sal-expr/type accessor))
         (data-type (sal-type/expand-if-type-name (sal-function-type/domain accessor-type)))
         (num-tag-bits (sal-data-type/finite-rep-num-tag-bits data-type))
         (num-bits (sal-type/finite-rep-num-bits data-type))
         (result-type (sal-function-type/range accessor-type))
         (result-num-bits (sal-type/finite-rep-num-bits result-type)))
    (multiple-value-bind
        (arg-bit-list arg-type)
        (sal-expr->boolean-expr-core (slot-value expr :arg) env aux-decl-queue)
      [assert (arg-type data-type) (sal-type/equivalent? arg-type data-type)]
      [assert (arg-type data-type) (= (sal-type/finite-rep-num-bits arg-type)
                                      (sal-type/finite-rep-num-bits data-type))]
      (let ((body-bit-list (list-tail arg-bit-list num-tag-bits)))
        (let loop ((body-bit-list body-bit-list)
                   (accessor-list accessor-list))
          (when (null? accessor-list)
            (sign-source-error expr "Invalid accessor use."))
          (let ((curr-accessor (car accessor-list)))
            (if (sal-ast/equivalent? curr-accessor accessor)
              (values (list-head body-bit-list result-num-bits)
                      result-type)
              (let* ((curr-accessor-elem-type (sal-name-expr/accessor-type curr-accessor))
                     (curr-num-bits (sal-type/finite-rep-num-bits curr-accessor-elem-type)))
                (loop (list-tail body-bit-list curr-num-bits)
                      (cdr accessor-list))))))))))

(define-method (sal-expr->boolean-expr-core (expr <sal-application>) (env <primitive>) (aux-decl-queue <primitive>))
  (let ((fun (slot-value expr :fun)))
    (if (and (instance-of? fun <sal-name-expr>)
             (instance-of? (slot-value fun :decl) <sal-auxiliary-decl>))
      (let ((new-expr (sal-ast/simplify (sal-expr/apply (slot-value (slot-value fun :decl) :value) (slot-value expr :arg)))))
        (sal-expr->boolean-expr-core new-expr env aux-decl-queue))
      (sal-function-application->boolean-expr-core expr env aux-decl-queue))))

(define-method (sal-expr->boolean-expr-core (expr <sal-definition-expression>) (env <primitive>) (aux-decl-queue <primitive>))
  (let ((defined-lhs-list (slot-value expr :lhs-list)))
    (unless (= (length defined-lhs-list) 1)
      ;; I should check this restriction...
      (sign-unsupported-feature expr "Defined expression contains more than one defined left-hand-side, so it cannot be converted to finite boolean representation."))
    (multiple-value-bind
        (lhs-bit-list lhs-type)
        (sal-expr->boolean-expr-core (car defined-lhs-list) env aux-decl-queue)
      (multiple-value-bind
          (expr-bit-list expr-type)
          (sal-expr->boolean-expr-core (slot-value expr :expr) env aux-decl-queue)
        [assert (expr-type) (sal-type/boolean? expr-type)]
        [assert (expr-bit-list) (= (length expr-bit-list) 1)]
        (let ((result-bit (copy-ast expr
                                    :lhs-list lhs-bit-list
                                    :expr (car expr-bit-list))))
          (values (list result-bit) expr-type))))))

;; Let expressions 
;; I convert the let-decls and insert them in the aux-decl-queue.
;; I update the environment with the let-decls, and the result is
;; the result of the body of the let-expression. 
(define-method (sal-expr->boolean-expr-core (expr <sal-let-expr>) (env <primitive>) (aux-decl-queue <primitive>))
  (let* ((local-decls (slot-value expr :local-decls))
         (bit-list-type-pairs (map (lambda (local-decl)
                                     (ensure ((local-decl <sal-let-decl>))
                                       (multiple-value-bind
                                           (curr-bit-list curr-type)
                                           (sal-expr->boolean-expr-core (slot-value local-decl :value) env aux-decl-queue)
                                         (let ((new-bits (map (lambda (curr-bit)
                                                                (if (or (instance-of? curr-bit <sal-ast-leaf>)
                                                                        (instance-of? curr-bit <sal-next-operator>))
                                                                  curr-bit
                                                                  (let* ((new-name (gen-unique-name (sal-decl/name local-decl)))
                                                                         (new-id (copy-ast (slot-value local-decl :id) 
                                                                                           :name new-name))
                                                                         (new-decl (copy-ast local-decl
                                                                                             :id new-id
                                                                                             :type (make-sal-builtin-name 
                                                                                                    <sal-bool-type>
                                                                                                    (slot-value local-decl :type))
                                                                                             :value curr-bit))
                                                                         (new-bit (make-sal-name-expr new-decl)))
                                                                    (queue/insert! aux-decl-queue new-decl)
                                                                    new-bit)))
                                                              curr-bit-list)))
                                           (cons new-bits curr-type)))))
                                   local-decls))
         (env (update-env* env local-decls bit-list-type-pairs)))
    (sal-expr->boolean-expr-core (slot-value expr :expr) env aux-decl-queue)))

(define-method (sal-expr->boolean-expr-core (expr <sal-tuple-update>) (env <primitive>) (aux-decl-queue <primitive>))
  (multiple-value-bind
      (tuple-bit-list tuple-type)
      (sal-expr->boolean-expr-core (slot-value expr :target) env aux-decl-queue)
    (multiple-value-bind
        (new-value-bit-list new-value-type)
        (sal-expr->boolean-expr-core (slot-value expr :new-value) env aux-decl-queue)
      (let ((pos (- (sal-tuple-position->integer (slot-value expr :idx)) 1))
            (type-list (sal-tuple-type/types tuple-type))
            (result-type-queue (make-queue))
            (result-bit-queue (make-queue)))
        (let loop ((i 0)
                   (type-list type-list)
                   (bit-list tuple-bit-list))
          (when (null? type-list)
            (sign-source-error expr "Invalid tuple update."))
          (let* ((curr-type (car type-list))
                 (curr-num-bits (sal-type/finite-rep-num-bits curr-type))
                 (rest-bit-list (list-tail bit-list curr-num-bits))
                 (rest-type-list (cdr type-list)))
            (if (= i pos)
              (let* ((new-curr-type (sal-type/union-finite-rep curr-type new-value-type))
                     (new-value-bit-list (sal-type/convert-to new-value-type new-curr-type new-value-bit-list)))
                (queue/insert! result-type-queue new-curr-type)
                (queue/append! result-type-queue rest-type-list)
                (queue/append! result-bit-queue new-value-bit-list)
                (queue/append! result-bit-queue rest-bit-list))
              (let ((curr-bit-list (list-head bit-list curr-num-bits)))
                (queue/insert! result-type-queue curr-type)
                (queue/append! result-bit-queue curr-bit-list)
                (loop (+ i 1) rest-type-list rest-bit-list)))))
        (let ((result-type (make-ast-instance <sal-tuple-type> expr
                                              :types (queue->list result-type-queue)))
              (result-bit-list (queue->list result-bit-queue)))
          [assert (result-bit-list result-type) (= (length result-bit-list) (sal-type/finite-rep-num-bits result-type))]
          (values result-bit-list result-type))))))

(define-method (sal-expr->boolean-expr-core (expr <sal-record-update>) (env <primitive>) (aux-decl-queue <primitive>))
  (multiple-value-bind
      (record-bit-list record-type)
      (sal-expr->boolean-expr-core (slot-value expr :target) env aux-decl-queue)
    (multiple-value-bind
        (new-value-bit-list new-value-type)
        (sal-expr->boolean-expr-core (slot-value expr :new-value) env aux-decl-queue)
      (let ((field-name (slot-value (slot-value expr :idx) :name))
            (field-list (sal-record-type/fields record-type))
            (result-bit-queue (make-queue))
            (result-field-queue (make-queue)))
        (let loop ((field-list field-list)
                   (bit-list record-bit-list))
          (when (null? field-list)
            (sign-source-error expr "Invalid record update."))
          (tlet ((curr-field <sal-field> (car field-list)))
            (let* ((curr-type (slot-value curr-field :type))
                   (curr-num-bits (sal-type/finite-rep-num-bits curr-type))
                   (curr-field-name (slot-value (slot-value curr-field :id) :name))
                   (rest-bit-list (list-tail bit-list curr-num-bits))
                   (rest-field-list (cdr field-list)))
              (if (eq? field-name curr-field-name)
                (let* ((new-curr-type (sal-type/union-finite-rep curr-type new-value-type))
                       (new-curr-field (copy-ast curr-field :type new-curr-type))
                       (new-value-bit-list (sal-type/convert-to new-value-type new-curr-type new-value-bit-list)))
                  (queue/insert! result-field-queue new-curr-field)
                  (queue/append! result-field-queue rest-field-list)
                  (queue/append! result-bit-queue new-value-bit-list)
                  (queue/append! result-bit-queue rest-bit-list))
                (let ((curr-bit-list (list-head bit-list curr-num-bits)))
                  (queue/insert! result-field-queue curr-field)
                  (queue/append! result-bit-queue curr-bit-list)
                  (loop rest-field-list rest-bit-list))))))
        (let ((result-type (make-ast-instance <sal-record-type> expr
                                              :fields (queue->list result-field-queue)))
              (result-bit-list (queue->list result-bit-queue)))
          [assert (result-bit-list result-type) (= (length result-bit-list) (sal-type/finite-rep-num-bits result-type))]
          (values result-bit-list result-type))))))

(define-method (sal-expr->boolean-expr-core (expr <sal-array-update>) (env <primitive>) (aux-decl-queue <primitive>))
  (try
   (multiple-value-bind
       (array-bit-list array-type)
       (sal-expr->boolean-expr-core (slot-value expr :target) env aux-decl-queue)
     (multiple-value-bind
         (new-value-bit-list new-value-type)
         (sal-expr->boolean-expr-core (slot-value expr :new-value) env aux-decl-queue)
       (let* ((domain (sal-function-type/domain array-type))
              (range (sal-function-type/range array-type))
              (range-num-bits (sal-type/finite-rep-num-bits range))
              (new-range (sal-type/union-finite-rep range new-value-type))
              (result-type (copy-ast (sal-type/cast array-type <sal-array-type>) :range new-range))
              (it (sal-type/make-iterator domain))
              (idx (evaluate-or-false (slot-value expr :idx)))
              (new-value-bit-list (map (cut make-bool-expr-alias! <> aux-decl-queue 'new-val)
                                       (sal-type/convert-to new-value-type new-range new-value-bit-list)))
              (result-bit-queue (make-queue)))
         (unless (iterator/has-next? it)
           (sign-source-error (slot-value expr :target) "Invalid array, the domain is empty."))
         (if (and idx (sal-expr/first-order-value? idx))
           (let ((found? #f))
             (iterator/for-each (lambda (domain-value)
                                  ;; I cannot use env here, the env I have is incompatible with sal-expr/equal-values?
                                  ;; Remark: sal-expr/equal-values? possible results: #t, #f, 'dont-known
                                  (let ((equal-value? (sal-expr/equal-values? domain-value idx (make-empty-env))))
                                    (cond
                                     ((eq? equal-value? #t)
                                      (queue/append! result-bit-queue new-value-bit-list)
                                      (set! found? #t))
                                     ((eq? equal-value? #f)
                                      (let* ((curr-bit-list (list-head array-bit-list range-num-bits))
                                             (new-curr-bit-list (sal-type/convert-to range new-range curr-bit-list)))
                                        (queue/append! result-bit-queue new-curr-bit-list)))
                                     (else
                                      (sign-unsupported-feature expr "Failed to convert array update. The equality test failed. Please contact support."))))
                                  (set! array-bit-list (list-tail array-bit-list range-num-bits)))
                                it)
             (unless found?
               (sign-source-error expr "The index is not in the domain of the array.")))
           (multiple-value-bind
               (idx-bit-list idx-type)
               (sal-expr->boolean-expr-core (slot-value expr :idx) env aux-decl-queue)
             (iterator/for-each (lambda (domain-value)
                                  (multiple-value-bind
                                      (domain-value-bit-list domain-value-type)
                                      (sal-expr->boolean-expr-core domain-value env aux-decl-queue)
                                    (let* ((union-type (sal-type/union-finite-rep idx-type domain-value-type))
                                           (idx-bit-list (sal-type/convert-to idx-type union-type idx-bit-list))
                                           (new-domain-value-bit-list (sal-type/convert-to domain-value-type union-type domain-value-bit-list))
                                           (place-provider expr)
                                           (condition (make-bool-expr-alias! (make-sal-and+* (map (lambda (new-idx-bit new-domain-bit)
                                                                                                    (make-sal-equality+ new-idx-bit new-domain-bit))
                                                                                                  idx-bit-list
                                                                                                  new-domain-value-bit-list)
                                                                                             place-provider)
                                                                             aux-decl-queue
                                                                             'cond))
                                           (curr-bit-list (list-head array-bit-list range-num-bits))
                                           (new-curr-bit-list (sal-type/convert-to range new-range curr-bit-list))
                                           (new-bit-list (map (lambda (curr-bit new-bit)
                                                                (make-sal-cond+ condition new-bit curr-bit place-provider))
                                                              new-curr-bit-list
                                                              new-value-bit-list)))
                                      (queue/append! result-bit-queue new-bit-list)
                                      (set! array-bit-list (list-tail array-bit-list range-num-bits)))))
                                it)))
         (let ((result-bit-list (queue->list result-bit-queue)))
           (values result-bit-list result-type)))))
   (catch 'type-iterator
          (lambda (msg)
            (sign-unsupported-feature expr "Failed to convert array update to finite boolean representation, reason: ~a" msg)))))

;; The generation of membership expressions may generate calls to sal-real-pred and sal-int-pred...
;; this calls must be ignored.
(define (ignore-app app)
  (values (list (make-sal-true app))
          (make-sal-builtin-name <sal-bool-type> app)))

;; I need to ignore real-pred and int-pred, because of sal-type/finite-rep-membership-expr...
(define-method (sal-expr->boolean-expr-core (expr <sal-real-pred>) (env <primitive>) (aux-decl-queue <primitive>))
  (ignore-app expr))
(define-method (sal-expr->boolean-expr-core (expr <sal-int-pred>) (env <primitive>) (aux-decl-queue <primitive>))
  (ignore-app expr))

;--------------------------------------------------------------------------
;
; Support for Arithmetical operations
;
;--------------------------------------------------------------------------

(define (adder-result-type type1 type2)
  (tlet* ((type1 <sal-bounded-subtype> (sal-type->bounded-subtype type1))
          (type2 <sal-bounded-subtype> (sal-type->bounded-subtype type2))
          (lower1 <sal-numeral> (lower-bound type1))
          (upper1 <sal-numeral> (upper-bound type1))
          (lower2 <sal-numeral> (lower-bound type2))
          (upper2 <sal-numeral> (upper-bound type2))
          (new-lower <sal-numeral> (update-ast-slots lower1 :num (+mpq (slot-value lower1 :num) (slot-value lower2 :num))))
          (new-upper <sal-numeral> (update-ast-slots upper1 :num (+mpq (slot-value upper1 :num) (slot-value upper2 :num)))))
    (make-sal-subrange new-lower new-upper)))

(define (gen-adder-compatible-number-bit-lists type1 type2 bit-list1 bit-list2)
  (let* ((result-type (adder-result-type type1 type2))  
         ;; Important: in a previous version bit-list1 and bit-list2 were converted to an element of result-type,
         ;; but this is wrong, since the result-type may not contain all elements of type1 and type2.
         ;; Example: type1       : [10 .. 13]
         ;;          type2       : [-3 .. 2]
         ;;          result-type : [7 .. 15]
         ;; type is contained in result-type, but type2 is not. So the convertion was wrong. We fixed
         ;; this problem by performing the union of type1, type2 and result-type.
         (tmp-type (sal-type/union-finite-rep type1 result-type))
         (union-type (sal-type/union-finite-rep type2 tmp-type))
         ;; (_ (breakpoint "gen-adder-compatible-number-bit-lists" (type1 type2 bit-list1 bit-list2 result-type tmp-type union-type) #t))
         (bit-list1 (convert-number-bit-list type1 union-type bit-list1))
         (bit-list2 (convert-number-bit-list type2 union-type bit-list2)))
    (values bit-list1 bit-list2)))

(define (absolute-add bit-list1 bit-list2 place-source aux-decl-queue)
  [assert (bit-list1 bit-list2) (= (length bit-list1) (length bit-list2))]
  (let ((result-bit-queue (make-queue)))
    (let loop ((curr-bit-list1 bit-list1)
               (curr-bit-list2 bit-list2)
               (carry-in (make-sal-false place-source)))
      (cond
       ((null? curr-bit-list1)
        ;; (queue/insert! result-bit-queue carry-in)
        [assert (bit-list1 result-bit-queue) (= (length bit-list1) (queue/length result-bit-queue))]
        (queue->list result-bit-queue))
       (else
        (let* ((curr-bit1 (car curr-bit-list1))
               (curr-bit2 (car curr-bit-list2))
               (result-bit (make-sal-xor+ (make-sal-xor+ curr-bit1 curr-bit2)
                                          carry-in))
               (carry-out (make-bool-expr-alias! (make-sal-or+ (make-sal-and+ curr-bit1 curr-bit2)
                                                               (make-sal-and+ curr-bit1 carry-in)
                                                               (make-sal-and+ curr-bit2 carry-in))
                                                 aux-decl-queue
                                                 'carry)))
          (queue/insert! result-bit-queue result-bit)
          (loop (cdr curr-bit-list1) (cdr curr-bit-list2) carry-out)))))))

(define (sal-add/gen-circuit-core type1 type2 bit-list1 bit-list2 aux-decl-queue)
  ;; (breakpoint "sal-add/gen-circuit-core-1" (type1 type2 bit-list1 bit-list2) #t)
  (multiple-value-bind
      (bit-list1 bit-list2)
      (gen-adder-compatible-number-bit-lists type1 type2 bit-list1 bit-list2)
    (let* ((result-type (adder-result-type type1 type2))
           ;; (_ (breakpoint "sal-add/gen-circuit-core" (type1 type2 bit-list1 bit-list2 aux-decl-queue) #t))
           (num-bits (sal-type/finite-rep-num-bits result-type))
           (place-source type1)
           (signed? (sal-bounded-subtype/signed-number? result-type))
           (result-bit-list (force-number-bit-list-size (absolute-add bit-list1 bit-list2 place-source aux-decl-queue) num-bits signed? place-source)))
      ;; (result-bit-list (list-head (absolute-add bit-list1 bit-list2 place-source aux-decl-queue) num-bits)))
      [sal-assert "sal-add/gen-circuit-core-2" (type1 type2 bit-list1 bit-list2 result-type result-bit-list num-bits)
                  (= (sal-type/finite-rep-num-bits result-type) (length result-bit-list))]
      (values result-bit-list
              result-type))))

(define-method (sal-expr->boolean-expr-core (app <sal-add>) (env <primitive>) (aux-decl-queue <primitive>))
  (let ((arg-list (sal-application/argument-list app)))
    [sal-assert "adder circuit generator" (arg-list) (>= (length arg-list) 2)]
    (multiple-value-bind
        (bit-list1 type1)
        (sal-expr->boolean-expr-core (car arg-list) env aux-decl-queue)
      (check-if-finite-number-type type1)
      (let loop ((bit-list bit-list1)
                 (type type1)
                 (arg-list (cdr arg-list)))
        (if (null? arg-list)
          (values bit-list type)
          (let ((curr-arg (car arg-list)))
            (multiple-value-bind
                (curr-bit-list curr-type)
                (sal-expr->boolean-expr-core curr-arg env aux-decl-queue)
              (check-if-finite-number-type curr-type)
              (multiple-value-bind
                  (new-bit-list new-type)
                  (sal-add/gen-circuit-core type curr-type bit-list curr-bit-list aux-decl-queue)
                (loop new-bit-list new-type (cdr arg-list))))))))))

(define (gen-binary-arith-circuit app env proc aux-decl-queue)
  (multiple-value-bind
      (arg1 arg2)
      (sal-binary-application/arguments app)
    (multiple-value-bind
        (bit-list1 type1)
        (sal-expr->boolean-expr-core arg1 env aux-decl-queue)
      (check-if-finite-number-type type1)
      (multiple-value-bind
          (bit-list2 type2)
          (sal-expr->boolean-expr-core arg2 env aux-decl-queue)
        (check-if-finite-number-type type2)
        (proc type1 type2 bit-list1 bit-list2)))))

(define-method (sal-expr->boolean-expr-core (app <sal-sub>) (env <primitive>) (aux-decl-queue <primitive>))
  (gen-binary-arith-circuit 
   app env
   (lambda (type1 type2 bit-list1 bit-list2)
     (multiple-value-bind
         (new-bit-list2 new-type2)
         (invert-number-bit-list-and-type bit-list2 type2 aux-decl-queue)
       ;; (breakpoint "sal-expr->boolean-expr-core" (type1 type2 bit-list1 bit-list2 new-bit-list2 new-type2 app) #t)
       (sal-add/gen-circuit-core type1 new-type2 bit-list1 new-bit-list2 aux-decl-queue)))
   aux-decl-queue))

(define (lt-bit bit1 bit2)
  (make-sal-and+ (make-sal-not+ bit1) bit2))

(define (le-bit bit1 bit2)
  (make-sal-or+ (make-sal-not+ bit1) bit2))

;; This function is a template used to implement absolute-lt and absolute-le.
;; These two functions are almost the same, the only difference is how the first
;; bit is compared. 
(define (compare-number-bit-lists bit-list1 bit-list2 compare-first aux-decl-queue)
  [assert (bit-list1 bit-list2) (= (length bit-list1) (length bit-list2))]
  [assert (bit-list1) (not (null? bit-list1))]
  (let loop ((bit-list1 bit-list1)
             (bit-list2 bit-list2)
             (result #f))
    (if (null? bit-list1)
      result
      (let* ((bit1 (make-bool-expr-alias! (car bit-list1) aux-decl-queue))
             (bit2 (make-bool-expr-alias! (car bit-list2) aux-decl-queue))
             (new-result (if result
                           (make-sal-or+ (lt-bit bit1 bit2)
                                         (make-sal-and+ (le-bit bit1 bit2) ;; (make-sal-equality+ bit1 bit2)
                                                        result))
                           (compare-first bit1 bit2))))
        (loop (cdr bit-list1) (cdr bit-list2) new-result)))))

(define (absolute-lt bit-list1 bit-list2 place-source aux-decl-queue)
  [assert (bit-list1 bit-list2) (= (length bit-list1) (length bit-list2))]
  (if (null? bit-list1) 
    (make-sal-false place-source) ;; zero < zero  if false
    (compare-number-bit-lists bit-list1 bit-list2 lt-bit aux-decl-queue)))

(define (absolute-le bit-list1 bit-list2 place-source aux-decl-queue)
  [assert (bit-list1 bit-list2) (= (length bit-list1) (length bit-list2))]
  (if (null? bit-list1) 
    (make-sal-true place-source) ;; zero <= zero  if false
    (compare-number-bit-lists bit-list1 bit-list2 le-bit aux-decl-queue)))

(define (gen-compatible-number-bit-lists type1 type2 bit-list1 bit-list2)
  (let* ((result-type (sal-type/union-finite-rep type1 type2))
         (bit-list1 (convert-number-bit-list type1 result-type bit-list1))
         (bit-list2 (convert-number-bit-list type2 result-type bit-list2)))
    (values bit-list1 bit-list2)))

;; this is a template for generating the circuits of <sal-lt> and <sal-le>
(define (gen-lt-circuit-core type1 type2 bit-list1 bit-list2 absolute-lt aux-decl-queue)
  (multiple-value-bind
      (bit-list1 bit-list2)
      (gen-compatible-number-bit-lists type1 type2 bit-list1 bit-list2)
    (let* ((place-source type1)
           (result-bit (cond
                        ((and (not (sal-bounded-subtype/signed-number? type1))
                              (not (sal-bounded-subtype/signed-number? type2)))
                         (absolute-lt bit-list1 bit-list2 place-source aux-decl-queue))
                        (else
                         (let ((sign1 (make-bool-expr-alias! (bit-list-sign bit-list1) aux-decl-queue))
                               (sign2 (make-bool-expr-alias! (bit-list-sign bit-list2) aux-decl-queue))
                               (body1 (bit-list-body bit-list1))
                               (body2 (bit-list-body bit-list2)))
                           (make-sal-or+
                            (make-sal-and+ sign1 (make-sal-not+ sign2))
                            (make-sal-and+ (make-sal-equality+ sign1 sign2)
                                           (absolute-lt body1 body2 place-source aux-decl-queue)))))))
           (result-type (make-sal-builtin-name <sal-bool-type> result-bit)))
      (values (list result-bit)
              result-type))))

(define-method (sal-expr->boolean-expr-core (app <sal-lt>) (env <primitive>) (aux-decl-queue <primitive>))
  (gen-binary-arith-circuit 
   app env
   (lambda (type1 type2 bit-list1 bit-list2)
     (gen-lt-circuit-core type1 type2 bit-list1 bit-list2 absolute-lt aux-decl-queue))
   aux-decl-queue))

(define-method (sal-expr->boolean-expr-core (app <sal-le>) (env <primitive>) (aux-decl-queue <primitive>))
  (gen-binary-arith-circuit 
   app env
   (lambda (type1 type2 bit-list1 bit-list2)
     (gen-lt-circuit-core type1 type2 bit-list1 bit-list2 absolute-le aux-decl-queue))
   aux-decl-queue))

;; I handle <sal-ge> by using the simplifier. The simplifier will rewrite a <sal-ge> as a <sal-lt>
(define-method (sal-expr->boolean-expr-core (app <sal-ge>) (env <primitive>) (aux-decl-queue <primitive>))
  (sal-expr->boolean-expr-core (sal-ast/simplify app) env aux-decl-queue))

;; I handle <sal-gt> by using the simplifier. The simplifier will rewrite a <sal-gt> as a <sal-le>
(define-method (sal-expr->boolean-expr-core (app <sal-gt>) (env <primitive>) (aux-decl-queue <primitive>))
  (sal-expr->boolean-expr-core (sal-ast/simplify app) env aux-decl-queue))

;;--------------------------
;;
;; Integer multiplication
;;
;;--------------------------

(define (double-bounds type)
  (tlet* ((type <sal-bounded-subtype> (sal-type->bounded-subtype type))
          (lower <sal-numeral> (lower-bound type))
          (upper <sal-numeral> (upper-bound type))
          (new-lower <sal-numeral> (update-ast-slots lower :num (*mpq (slot-value lower :num) *mpq-two*)))
          (new-upper <sal-numeral> (update-ast-slots upper :num (*mpq (slot-value upper :num) *mpq-two*))))
    (make-sal-subrange new-lower new-upper)))

(define (sal-unsigned-mult-expr->boolean-expr arg1-type arg2-type arg1-bit-list arg2-bit-list aux-decl-queue place-provider)
  (multiple-value-bind
      (tmp-bit-list tmp-type)
      (sal-numeral/gen-circuit (make-sal-numeral 0 place-provider) aux-decl-queue)
  (multiple-value-bind
      (small-bit-list arg-bit-list arg-type)
      (if (<= (length arg1-bit-list) (length arg2-bit-list))
        (values arg1-bit-list arg2-bit-list arg2-type)
        (values arg2-bit-list arg1-bit-list arg1-type))
  (let ((len (length small-bit-list))
        (zero-bit (make-sal-false place-provider)))
    (let loop ((i 0)
               (result-bit-list tmp-bit-list)
               (result-type tmp-type)
               (small-bit-list small-bit-list)
               (arg-bit-list arg-bit-list)
               (arg-type arg-type))
      (if (>= i len)
        (values result-bit-list result-type)
        (multiple-value-bind 
            (adder-result-bit-list adder-result-type)
            (sal-add/gen-circuit-core result-type arg-type result-bit-list arg-bit-list aux-decl-queue)
          (let* ((adder-result-num-bits (length adder-result-bit-list))
                 (compatible-result-bit-list (force-number-bit-list-size result-bit-list adder-result-num-bits #f place-provider))
                 (curr-bit (make-bool-expr-alias! (car small-bit-list) aux-decl-queue))
                 (new-result-bit-list (map (lambda (adder-result-bit compatible-result-bit)
                                             (let ((new-bit (make-sal-cond+ curr-bit
                                                                            adder-result-bit
                                                                            compatible-result-bit
                                                                            place-provider)))
                                               (make-bool-expr-alias! new-bit aux-decl-queue)))
                                           adder-result-bit-list
                                           compatible-result-bit-list))
                 (new-result-type adder-result-type)
                 (new-arg-bit-list (cons zero-bit arg-bit-list))
                 (new-arg-type (double-bounds arg-type)))
            (loop (+ i 1) 
                  new-result-bit-list new-result-type
                  (cdr small-bit-list)
                  new-arg-bit-list new-arg-type)))))))))

(define (remove-sign type bit-list aux-decl-queue)
  (if (sal-bounded-subtype/signed-number? type)
    (let ((sign (make-bool-expr-alias! (bit-list-sign bit-list) aux-decl-queue))
          (tmp-bit-list (abs-bit-list bit-list aux-decl-queue)))
      (tlet ((lower <sal-numeral> (lower-bound type))
             (upper <sal-numeral> (upper-bound type)))
        (let* ((new-lower (update-ast-slots lower :num *mpq-zero*))
               (new-upper (update-ast-slots upper :num (mpq/max (mpq/absolute (slot-value lower :num)) (mpq/absolute (slot-value upper :num)))))
               (new-type (make-sal-subrange new-lower new-upper))
               (place-provider sign)
               (new-bit-list (force-number-bit-list-size tmp-bit-list (sal-type/finite-rep-num-bits new-type) #f place-provider)))
          (values new-type new-bit-list sign))))
    (values type bit-list (make-sal-false type))))

(define (merge-pos-neg pos-bit-list neg-bit-list sign-bit aux-decl-queue place-provider)
  (let* ((new-pos-bit-list (map (lambda (pos-bit-expr)
                                  (make-bool-expr-alias! pos-bit-expr aux-decl-queue))
                                pos-bit-list))
         (not-zero-bit (make-sal-or+* new-pos-bit-list place-provider))
         (new-sign-bit (make-sal-and+ not-zero-bit sign-bit)))
    (map (lambda (pos-bit neg-bit)
           (make-bool-expr-alias! (make-sal-cond+ new-sign-bit neg-bit pos-bit place-provider)
                                  aux-decl-queue))
         new-pos-bit-list
         neg-bit-list)))

(define (sal-mult-expr->boolean-expr arg1-type arg2-type arg1-bit-list arg2-bit-list aux-decl-queue place-provider)
  (cond
   ((and (not (sal-bounded-subtype/signed-number? arg1-type))
         (not (sal-bounded-subtype/signed-number? arg2-type)))
    (sal-unsigned-mult-expr->boolean-expr arg1-type arg2-type arg1-bit-list arg2-bit-list aux-decl-queue place-provider))
   (else
    (multiple-value-bind
        (arg1-u-type arg1-u-bit-list arg1-sign-bit)
        (remove-sign arg1-type arg1-bit-list aux-decl-queue)
    (multiple-value-bind
        (arg2-u-type arg2-u-bit-list arg2-sign-bit)
        (remove-sign arg2-type arg2-bit-list aux-decl-queue)
      ;; (breakpoint "mult" (arg1-u-type arg2-u-type arg1-u-bit-list arg2-u-bit-list 
      ;;                     arg1-sign-bit arg2-sign-bit arg1-bit-list arg2-bit-list aux-decl-queue) #t)
    (multiple-value-bind
        (u-result-bit-list u-result-type)
        (sal-unsigned-mult-expr->boolean-expr arg1-u-type arg2-u-type arg1-u-bit-list arg2-u-bit-list aux-decl-queue place-provider)
      ;; (breakpoint "mult2" (u-result-type u-result-bit-list aux-decl-queue) #t)
      (let* ((upper (upper-bound u-result-type))
             (lower (update-ast-slots upper :num (-mpq *mpq-zero* (slot-value upper :num))))
             (result-type (make-sal-subrange lower upper))
             (num-result-bits (sal-type/finite-rep-num-bits result-type))
             (neg-upper (update-ast-slots upper :num *mpq-zero*))
             (neg-type (make-sal-subrange lower neg-upper))
             (result-sign-bit (make-bool-expr-alias! (make-sal-xor+ arg1-sign-bit arg2-sign-bit) aux-decl-queue))
             (tmp-neg-result-bit-list (append (two-complement-bit-list u-result-bit-list aux-decl-queue) (list (make-sal-true place-provider))))
             (neg-result-bit-list (force-number-bit-list-size tmp-neg-result-bit-list num-result-bits #t place-provider))
             (pos-result-bit-list (force-number-bit-list-size u-result-bit-list num-result-bits #f place-provider))
             (_ [assert (neg-result-bit-list pos-result-bit-list) (= (length neg-result-bit-list) (length pos-result-bit-list))])
             )
        (values (merge-pos-neg pos-result-bit-list neg-result-bit-list result-sign-bit aux-decl-queue place-provider)
                result-type))))))))

(define-method (sal-expr->boolean-expr-core (app <sal-mul>) (env <primitive>) (aux-decl-queue <primitive>))
  (let ((arg-list (sal-application/argument-list app))
        (place-provider app))
    [assert (arg-list) (not (null? arg-list))]
    (multiple-value-bind
        (first-arg-bit-list first-arg-type)
        (sal-expr->boolean-expr-core (car arg-list) env aux-decl-queue)
    (let loop ((arg-list (cdr arg-list))
               (result-bit-list first-arg-bit-list)
               (result-type first-arg-type))
      (if (null? arg-list)
        (values result-bit-list result-type)
        (multiple-value-bind
            (curr-arg-bit-list curr-arg-type)
            (sal-expr->boolean-expr-core (car arg-list) env aux-decl-queue)
        (multiple-value-bind
            (new-result-bit-list new-result-type)
            (sal-mult-expr->boolean-expr result-type curr-arg-type result-bit-list curr-arg-bit-list aux-decl-queue place-provider)
          (loop (cdr arg-list)
                new-result-bit-list
                new-result-type))))))))

;;--------------------------
;;
;; Integer division
;;
;;--------------------------

(define (absolute-idiv n-bit-list d-bit-list aux-decl-queue place-provider)
  [assert (n-bit-list d-bit-list) (= (length n-bit-list) (length d-bit-list))]
  (let* ((num-bits (length n-bit-list))
         (B (add-sign-bit d-bit-list place-provider))
         (B-two-complement (two-complement-bit-list B aux-decl-queue))
         (P (force-number-bit-list-size (number->bit-list 0 place-provider) (+ num-bits 1) #f place-provider))
         (A n-bit-list))
    [assert (B B-two-complement) (= (length B) (length B-two-complement))]
    (let loop ((i 0)
               (P P)
               (A A))
      [assert (P B) (= (length B) (length P))]
      [assert (P A) (= (length P) (+ (length A) 1))]
      (cond
       ((< i num-bits)
        ;; P1          = P[num-bits-1] P[num-bits - 1] ... P[0] A[num-bits - 1]
        ;; P1-lt-B-bit = P1 < B
        ;; new-A       = A[num-bits-2] ... A[0] (not P1-lt-B-bit)
        ;; P2          = P1 - B
        ;; new-P       = if P1-lt-B-bit then P1 else P2 fi
        (let* ((A-most-significative-bit (make-bool-expr-alias! (list-last-element A)
                                                                aux-decl-queue))
               (P1 (cons A-most-significative-bit (list-head P num-bits))) ;; P,A <<< shift
               (P1-lt-B-bit (make-bool-expr-alias! (absolute-lt P1 B place-provider aux-decl-queue)
                                                   aux-decl-queue
                                                   'p-lt-b))
               (new-A (cons (make-sal-not+ P1-lt-B-bit) (list-head A (- num-bits 1))))
               (P2 (absolute-add P1 B-two-complement place-provider aux-decl-queue))
               (_ [assert (P1 P2) (= (length P1) (length P2))])
               (new-P (map (lambda (P1-bit P2-bit)
                             (make-bool-expr-alias! (make-sal-cond+ P1-lt-B-bit P1-bit P2-bit)
                                                    aux-decl-queue))
                           P1 P2)))
          (loop (+ i 1)
                new-P
                new-A)))
       (else
        ;; A contains the quotient, and P the remainder
        (values A (list-head P num-bits)))))))

(define (idiv-result-type type1 type2)
  (tlet* ((type1 <sal-bounded-subtype> (sal-type->bounded-subtype type1))
          (type2 <sal-bounded-subtype> (sal-type->bounded-subtype type2))
          (lower1 <sal-numeral> (lower-bound type1))
          (upper1 <sal-numeral> (upper-bound type1))
          (lower2 <sal-numeral> (lower-bound type2))
          (upper2 <sal-numeral> (upper-bound type2)))
    (let* ((abs-max (mpq/max (mpq/max (mpq/absolute (slot-value lower1 :num))
                                      (mpq/absolute (slot-value lower2 :num)))
                             (mpq/max (mpq/absolute (slot-value upper1 :num))
                                      (mpq/absolute (slot-value upper2 :num)))))
           (contains-neg? (or (<mpq (slot-value lower1 :num) *mpq-zero*)
                              (<mpq (slot-value lower2 :num) *mpq-zero*)))
           (new-lower (update-ast-slots lower1 :num (if contains-neg? (-mpq *mpq-zero* abs-max) *mpq-zero*)))
           (new-upper (update-ast-slots upper1 :num abs-max)))
      (make-sal-subrange new-lower new-upper))))

(define (sal-idiv-expr->boolean-expr-core app env aux-decl-queue)
  (multiple-value-bind
      (arg1 arg2)
      (sal-binary-application/arguments app)
    (multiple-value-bind
        (arg1-bit-list arg1-type)
        (sal-expr->boolean-expr-core arg1 env aux-decl-queue)
      (multiple-value-bind
          (arg2-bit-list arg2-type)
          (sal-expr->boolean-expr-core arg2 env aux-decl-queue)
        (let* ((result-type (idiv-result-type arg1-type arg2-type))
               (num-result-bits (sal-type/finite-rep-num-bits result-type))
               (arg1-bit-list (convert-number-bit-list arg1-type result-type arg1-bit-list))
               (arg2-bit-list (convert-number-bit-list arg2-type result-type arg2-bit-list))
               (place-provider app))
          ;; (breakpoint "sal-idiv-expr->boolean-expr-core-1" (result-type arg1-type arg2-type arg1-bit-list arg2-bit-list num-result-bits) #t)
          (cond 
           ((not (sal-bounded-subtype/signed-number? result-type))
            (multiple-value-bind
                (quotient remainder)
                (absolute-idiv arg1-bit-list arg2-bit-list aux-decl-queue place-provider)
              (values quotient remainder result-type)))
           (else
            (let* ((arg1-sign-bit (bit-list-sign arg1-bit-list))
                   (arg2-sign-bit (bit-list-sign arg2-bit-list))
                   (div-sign-bit (make-bool-expr-alias! (make-sal-xor+ arg1-sign-bit arg2-sign-bit) aux-decl-queue))
                   (mod-sign-bit arg1-sign-bit)
                   (u-arg1-bit-list (abs-bit-list arg1-bit-list aux-decl-queue))
                   (u-arg2-bit-list (abs-bit-list arg2-bit-list aux-decl-queue)))
              ;; (breakpoint "sal-idiv-expr->boolean-expr-core-2" (arg1-sign-bit arg2-sign-bit div-sign-bit mod-sign-bit u-arg1-bit-list u-arg2-bit-list) #t)
              [assert (u-arg1-bit-list u-arg2-bit-list) (= (length u-arg1-bit-list) (length u-arg2-bit-list))]
              (multiple-value-bind
                  (u-quotient u-remainder)
                  (absolute-idiv u-arg1-bit-list u-arg2-bit-list aux-decl-queue place-provider)
                [assert (u-quotient u-arg1-bit-list) (= (length u-arg1-bit-list) (length u-quotient))]
                [assert (u-remainder u-arg1-bit-list) (= (length u-arg1-bit-list) (length u-remainder))]
                (let* ((tmp-neg-quotient (append (two-complement-bit-list u-quotient aux-decl-queue) (list (make-sal-true place-provider))))
                       (tmp-neg-remainder (append (two-complement-bit-list u-remainder aux-decl-queue) (list (make-sal-true place-provider))))
                       (neg-quotient (force-number-bit-list-size tmp-neg-quotient num-result-bits #t place-provider))
                       (neg-remainder (force-number-bit-list-size tmp-neg-remainder num-result-bits #t place-provider))
                       (pos-quotient (force-number-bit-list-size u-quotient num-result-bits #f place-provider))
                       (pos-remainder (force-number-bit-list-size u-remainder num-result-bits #f place-provider))
                       (_ [assert (pos-remainder neg-remainder) (= (length pos-remainder) (length neg-remainder))])
                       (__ [assert (pos-quotient neg-quotient) (= (length pos-quotient) (length neg-quotient))])
                       (quotient (merge-pos-neg pos-quotient neg-quotient div-sign-bit aux-decl-queue place-provider))
                       (remainder (merge-pos-neg pos-remainder neg-remainder mod-sign-bit aux-decl-queue place-provider)))
                  (values quotient remainder result-type)))))))))))

(define-method (sal-expr->boolean-expr-core (app <sal-idiv>) (env <primitive>) (aux-decl-queue <primitive>))
  (multiple-value-bind
      (quotient remainder result-type)
      (sal-idiv-expr->boolean-expr-core app env aux-decl-queue)
    (values quotient result-type)))

(define-method (sal-expr->boolean-expr-core (app <sal-div>) (env <primitive>) (aux-decl-queue <primitive>))
  (warning-message "Using integer division interpretation. Use the operator 'div' instead of '/' to avoid this warning message.")
  (multiple-value-bind
      (quotient remainder result-type)
      (sal-idiv-expr->boolean-expr-core app env aux-decl-queue)
    (values quotient result-type)))

(define-method (sal-expr->boolean-expr-core (app <sal-mod>) (env <primitive>) (aux-decl-queue <primitive>))
  (multiple-value-bind
      (quotient remainder result-type)
      (sal-idiv-expr->boolean-expr-core app env aux-decl-queue)
    (values remainder result-type)))

;----------------------------------------------------------------
;
; bit-list->sal-value
;
; This function converts a list of boolean expression in a SAL value
; of a specific type. This function is used to produce counter-examples.
;
;----------------------------------------------------------------

(define-generic (bit-list->sal-value type bit-list))

(define (bit-list->unsigned-number bit-list)
  (let loop ((bit-list (reverse bit-list))
             (result *mpq-zero*))
    (if (null? bit-list)
      result
      (let* ((curr-bit (car bit-list))
             (new-result (if (sal-expr/true? curr-bit)
                           (+mpq (*mpq result *mpq-two*) *mpq-one*)
                           (+mpq (*mpq result *mpq-two*) *mpq-zero*))))
        (loop (cdr bit-list) new-result)))))
        
(define (bit-list->number bit-list signed?)
  (if signed?
    (let* ((sign (bit-list-sign bit-list)))
      (if (sal-expr/true? sign)
        (let ((number (bit-list->unsigned-number (invert-bit-list (bit-list-body bit-list)))))
          (-mpq (-mpq *mpq-zero* number) *mpq-one*))
        (bit-list->unsigned-number (bit-list-body bit-list))))
    (bit-list->unsigned-number bit-list)))

(define-method (bit-list->sal-value (type <sal-type>) (bit-list <primitive>))
  (sign-unsupported-feature type "Failed to generate a SAL value representation from a list of bits. This type is not supported."))

(define-method (bit-list->sal-value (type <sal-bounded-subtype>) (bit-list <primitive>))
  (make-ast-instance <sal-numeral> type
                     :num (bit-list->number bit-list (sal-bounded-subtype/signed-number? type))))

(define-method (bit-list->sal-value (type <sal-scalar-type>) (bit-list <primitive>))
  (let ((idx (mpq->integer (bit-list->unsigned-number bit-list)))
        (scalar-elements (slot-value type :scalar-elements)))
    (if (< idx (length scalar-elements))
      (list-ref scalar-elements idx)
      ;; This condition may happen for choice variables ... so this is not a real problem...
      (car scalar-elements))))

(define-macro (bit-list->tuple-value tuple-literal-class)
  `(let ((type-list (slot-value type :types))
         (expr-queue (make-queue)))
     (let loop ((type-list type-list)
                (bit-list bit-list))
       (if (null? type-list)
         (make-ast-instance ,tuple-literal-class type
                            :exprs (queue->list expr-queue))
         (let* ((curr-type (car type-list))
                (curr-num-bits (sal-type/finite-rep-num-bits curr-type))
                (curr-bit-list (list-head bit-list curr-num-bits))
                (rest-bit-list (list-tail bit-list curr-num-bits))
                (curr-expr (bit-list->sal-value curr-type curr-bit-list)))
           (queue/insert! expr-queue curr-expr)
           (loop (cdr type-list) rest-bit-list))))))
  
(define-method (bit-list->sal-value (type <sal-tuple-type>) (bit-list <primitive>))
  (bit-list->tuple-value <sal-tuple-literal>))

(define-method (bit-list->sal-value (type <sal-domain-tuple-type>) (bit-list <primitive>))
  (bit-list->tuple-value <sal-arg-tuple-literal>))

(define-method (bit-list->sal-value (type <sal-record-type>) (bit-list <primitive>))
  (let ((field-list (slot-value type :fields))
        (entry-queue (make-queue)))
    (let loop ((field-list field-list)
               (bit-list bit-list))
      (if (null? field-list)
        (make-ast-instance <sal-record-literal> type
                           :entries (queue->list entry-queue))
        (tlet ((curr-field <sal-field> (car field-list)))
          (let* ((curr-type (slot-value curr-field :type))
                 (curr-num-bits (sal-type/finite-rep-num-bits curr-type))
                 (curr-bit-list (list-head bit-list curr-num-bits))
                 (rest-bit-list (list-tail bit-list curr-num-bits))
                 (curr-expr (bit-list->sal-value curr-type curr-bit-list))
                 (curr-entry (make-ast-instance <sal-record-entry> curr-field
                                                :id (slot-value curr-field :id)
                                                :expr curr-expr)))
            (queue/insert! entry-queue curr-entry)
            (loop (cdr field-list) rest-bit-list)))))))

(define-method (bit-list->sal-value (type <sal-type-name>) (bit-list <primitive>))
  (let ((definition (sal-type-name/definition type)))
    (unless definition
      (sign-unsupported-feature type "Failed to convert back to SAL representation, source type does not have an interpretation."))
    (bit-list->sal-value definition bit-list)))

(define-method (bit-list->sal-value (type <sal-subtype>) (bit-list <primitive>))
  (cond
   ((and (sal-type/integer? type)
         (sal-type->bounded-subtype type))
    =>
    (lambda (bounded-subtype)
      (bit-list->sal-value bounded-subtype bit-list)))
   (else
    (bit-list->sal-value (sal-subtype/immediate-super-type type) bit-list))))

(define-method (bit-list->sal-value (type <sal-data-type>) (bit-list <primitive>))
  (let* ((num-tag-bits (sal-data-type/finite-rep-num-tag-bits type))
         (tag-bit-list (list-head bit-list num-tag-bits))
         (body-bit-list (list-tail bit-list num-tag-bits))
         (tag-idx (mpq->integer (bit-list->unsigned-number tag-bit-list)))
         (constructors (slot-value type :constructors))
         (constructor (if (< tag-idx (length constructors))
                        (list-ref constructors tag-idx)
                        ;; this condition may happen for choice variables...
                        (car constructors))))
    (if (sal-name-expr/constant-constructor? constructor)
      constructor
      (let* ((constructor-type (sal-expr/type constructor))
             (domain-type (sal-function-type/domain constructor-type))
             (body-expr (bit-list->sal-value domain-type body-bit-list)))
        (make-ast-instance <sal-constructor-application> type
                           :fun constructor
                           :arg body-expr)))))

(define-method (bit-list->sal-value (type <sal-function-type>) (bit-list <primitive>))
  (try
   (let* ((domain (slot-value type :domain))
          (range (slot-value type :range))
          (num-range-bits (sal-type/finite-rep-num-bits range))
          (it (sal-type/make-iterator domain))
          (alternatives-queue (make-queue)))
     (iterator/for-each (lambda (domain-value)
                          (let* ((curr-bit-list (list-head bit-list num-range-bits))
                                 (rest-bit-list (list-tail bit-list num-range-bits))
                                 (range-value (bit-list->sal-value range curr-bit-list)))
                            (set! bit-list rest-bit-list)
                            (queue/insert! alternatives-queue (cons domain-value range-value))))
                        it)
     (alt-pair-list->lambda (queue->list alternatives-queue) type))
   (catch 'type-iterator
          (lambda (msg)
            (sign-unsupported-feature type "Failed bit list representation to SAL function (array), reason: ~a" msg)))))

(define-method (bit-list->sal-value (type <sal-array-type>) (bit-list <primitive>))
  (change-ast-class (call-next-method) <sal-array-literal>))

;-----------------------------------------------------------------
;
; SAL AST -> Boolean SAL AST
;
; sal-ast->boolean-ast is just a wrapper for sal-expr->boolean-expr-core
;
;-----------------------------------------------------------------

(define-generic (sal-ast->boolean-ast ast))

(define-method (sal-ast->boolean-ast (ast <sal-ast>))
  (sign-unsupported-feature ast "This abstract syntax tree node cannot be converted to a finite boolean representation."))

(define-method (sal-ast->boolean-ast (expr <sal-expr>))
  (sal-expr->boolean-expr expr (make-empty-env)))

;; return a procedure that receives a prefix and returns a new symbol
;; Examples:
;;  (define proc (gen-name-sequencer-proc))
;;  (proc 'tst)
;;    ==> tst!1
;;  (proc 'tst)
;;    ==> tst!2
;;  (proc 'tst)
;;    ==> tst!3
;;  (proc 'foo)
;;    ==> foo!1
;;  (proc 'foo)
;;    ==> foo!2
;;  (proc 'tst)
;;    ==> tst!1  <<<<<< IMPORTANT
(define (gen-name-sequencer-proc)
  (let ((curr-var-name #unspecified)
        (next-idx 1))
    (lambda (var-name)
      (cond
       ((eq? var-name curr-var-name)
        (let ((result (symbol-append var-name '! (string->symbol (integer->string next-idx)))))
          (set! next-idx (+ next-idx 1))
          result))
       (else
        (set! curr-var-name var-name)
        (set! next-idx 2)
        (symbol-append var-name '!1))))))

(define (convert-state-variables source-module target-module)
  (let ((state-vars (sal-module/state-variables source-module))
        (target-var-info (slot-value target-module :var-trace-info))
        ;; use a special purpose name generator for state variables...
        (unique-name-proc (gen-name-sequencer-proc)))
    (multiple-value-bind
        (new-state-vars bit-list-type-list)
        (sal-var-decl-list->sal-bool-var-decl-list state-vars unique-name-proc)
      [assert (state-vars bit-list-type-list) (= (length state-vars) (length bit-list-type-list))]
      (sal-module/set-state-variables! target-module new-state-vars)
      ;; store traceability information
      (for-each (lambda (state-var-decl bit-list-type)
                  (eq-hash-table/put! target-var-info
                                      state-var-decl
                                      (map (lambda (bit)
                                             (slot-value bit :decl))
                                           (car bit-list-type))))
                state-vars
                bit-list-type-list)
      (let ((env (update-env* (make-empty-env) state-vars bit-list-type-list)))
        env))))

;; It rules out garbage. Example:
;; The subrange [0,2] uses two bits to be represented, and
;; the combination (true, true) is `garbage'.
(define (gen-valid-state-expr flat-module env)
  (verbose-message 3 "  generating is-state and is-input predicates...")
  (let* ((input-vars (filter (lambda (state-var-decl)
                              (and (instance-of? state-var-decl <sal-input-state-var-decl>)
                                   (not (instance-of? state-var-decl <sal-choice-input-state-var-decl>))))
                             (slot-value flat-module :state-vars)))
         (defined-var-set (sal-module/defined-variables flat-module))
         (latched-vars (filter (lambda (state-var-decl)
                                 (and (not (instance-of? state-var-decl <sal-input-state-var-decl>))
                                      (not (eq-hash-table/contains? defined-var-set state-var-decl))))
                               (slot-value flat-module :state-vars))))
    (values (if (null? input-vars)
              (make-sal-true flat-module)
              (var-decl-list->membership-expr input-vars env))
            (if (null? latched-vars)
              (make-sal-true flat-module)
              (var-decl-list->membership-expr latched-vars env)))))

(define (convert-component-info component-info env)
  (let* ((tmp-queue (make-queue))
         (proc (lambda (datum)
                 (let* ((datum (sal-ast/simplify datum))
                        (new-data-list (sal-expr->boolean-expr-core datum env tmp-queue)))
                   [assert (tmp-queue) (queue/empty? tmp-queue)]
                   new-data-list))))
    (sal-component-info/convert-data component-info proc)))

(define-method (sal-ast->boolean-ast (flat-module <sal-flat-module>))
  (status-message :generating-boolean-flat-module)
  (verbose-message 1 "converting flat module to boolean flat module...")
  (display-runtime 2 "  flat module -> boolean flat module conversion time: ~a secs"
    (lambda ()
      (verbose-message 3 "  converting state variables...")
      (let* ((aux-decl-queue (make-queue))
             (result-module (make-ast-instance <sal-boolean-flat-module> flat-module
                                               :original-module flat-module
                                               :var-trace-info (make-eq-hash-table)))
             (env (convert-state-variables flat-module result-module))
             (component-info (slot-value flat-module :component-info)))
        (when (sal-component-info/contains-multi-component? component-info)
          (sign-source-error flat-module "Failed to convert flat module to boolean representation. Please expand the quantifiers and try again."))
        (multiple-value-bind
            (extra-valid-input-expr extra-valid-state-expr)
            (gen-valid-state-expr flat-module env)
          (verbose-message 3 "  converting initialization section...")
          (set-slot-value! result-module :initialization (sal-expr->boolean-expr (slot-value flat-module :initialization) env))
          (verbose-message 3 "  converting definition section...")
          (set-slot-value! result-module :definition (sal-expr->boolean-expr (slot-value flat-module :definition) env))
          (verbose-message 3 "  converting transition section...")
          (set-slot-value! result-module :transition (sal-expr->boolean-expr (slot-value flat-module :transition) env))
          (set-slot-value! result-module :skip (sal-expr->boolean-expr (slot-value flat-module :skip) env))
          (verbose-message 3 "  converting is-input and is-state predicates...")
          (set-slot-value! result-module :valid-input-expr (make-sal-and+ 
                                                            (sal-expr->boolean-expr (slot-value flat-module :valid-input-expr) env)
                                                            extra-valid-input-expr))
          
          (set-slot-value! result-module :valid-state-expr (make-sal-and+
                                                            (sal-expr->boolean-expr (slot-value flat-module :valid-state-expr) env)
                                                            extra-valid-state-expr))
          ;; ******** FIX ME **********
          (set-slot-value! result-module :valid-constant-expr (make-sal-true flat-module))
          (set-slot-value! result-module :component-info (convert-component-info component-info env))
          result-module)))
    :boolean-flat-module-time))

(define-method (sal-ast->boolean-ast (assertion <sal-assertion-expr>))
  (sal-assertion->boolean-assertion-core assertion (make-empty-env)))

(define-generic (sal-assertion->boolean-assertion-core assertion env))

(define-method (sal-assertion->boolean-assertion-core (ast <sal-ast>) (env <primitive>))
  (sal-ast/map ast env sal-assertion->boolean-assertion-core))

;; Cached env for sal-state-expr->boolean-state-expr
(define *cached-new-env* #unspecified)
(define *cached-new-env-module* #unspecified)
(define *cached-new-env-given-env* #unspecified)

(define (sal-state-expr->boolean-state-expr expr env bool-flat-module)
  (let* ((place-provider bool-flat-module)
         (new-env (if (and (eq? *cached-new-env-module* bool-flat-module)
                           (or (empty-env? env)
                               (eq? env *cached-new-env-given-env*)))
                    *cached-new-env*
                    (let* ((original-module (sal-derived-flat-module/original-flat-module bool-flat-module))
                           (original-bool-module (sal-derived-flat-module/original-boolean-flat-module bool-flat-module))
                           (state-vars (slot-value original-module :state-vars))
                           (traceability-table (slot-value original-bool-module :var-trace-info)))
                      (set! *cached-new-env-given-env* env)
                      (set! *cached-new-env-module* bool-flat-module)
                      (set! *cached-new-env* 
                            (fold-left (lambda (new-env state-var-decl)
                                         (let* ((name (sal-decl/name state-var-decl))
                                                (bit-decls (let ((entry (eq-hash-table/get traceability-table state-var-decl)))
                                                             [assert (entry traceability-table state-var-decl) (and entry (pair? entry))]
                                                             (cdr entry)))
                                                (bit-list (map (lambda (decl)
                                                                 (make-sal-name-expr decl place-provider))
                                                               bit-decls))
                                                (env-entry (cons bit-list (slot-value state-var-decl :type))))
                                           (update-env new-env state-var-decl env-entry)))
                                       env
                                       state-vars))
                      *cached-new-env*))))
    (sal-expr->boolean-expr expr new-env)))
    
(define-method (sal-assertion->boolean-assertion-core (ast <sal-module-models>) (env <primitive>))
  (let ((module (slot-value ast :module)))
    (unless (instance-of? module <sal-flat-module>)
      (sign-error "You have to flat the modules in an assertion before converting to a boolean representation."))
    (let* ((new-module (sal-ast->boolean-ast module)) ;; it is safe to ignore the environment here...
           (new-property (begin
                           (status-message :generating-boolean-property)
                           (verbose-message 1 "converting property to boolean property...")
                           (display-runtime 2 "  property -> boolean property conversion time: ~a secs"
                             (lambda ()
                               (sal-ast/expand-names (sal-state-expr->boolean-state-expr (slot-value ast :expr) env new-module)
                                                     (make-empty-env)))
                             :boolean-property-time))))
      (update-ast-slots ast
                        :module new-module
                        :expr new-property))))
