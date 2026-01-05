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

(module sat-generic-context-result
        (include "sal.sch")
        (include "fast-hash-table.sch")
        (import sal-ast-simplify sal-expression sal-ast-copy sal-ast-for-each
                sal-ast-env queue sal-decls sal-pp sat-generic-context gmp-scheme sal-type
                sal-finite-expressions sal-ast-table sal-ast-list)
        (export <sat-generic-context-result>
                (sat-generic-context-result/cleanup! ctx-result))
        )

(define-class <sat-generic-context-result> () (:sat-context :result-constraint-list))

;; -------------------------------------------------------
;; This file contains a collection of function to "cleanup"
;; the result produced by a decision procedure
;;
;; - recovery of the scalar values eliminated by sat-context procedures
;; - simplification + ground substitution
;; - elimination of auxiliary variables
;; - recovery of the constraints (lambda) eliminated by sat-context procedures
;; -------------------------------------------------------

(define *empty-env* (make-empty-env))

(define-generic (sal-ast/sat-ground? ast env))
(define-method (sal-ast/sat-ground? (ast <sal-ast>) (env <primitive>))
  (sal-ast/for-all-children (cut sal-ast/sat-ground? <> env) ast))
(define-method (sal-ast/sat-ground? (ast <sal-name-expr>) (env <primitive>))
  (or (lookup-env (slot-value ast :decl) env) ;; it is a local variable
      (sal-name-ref/builtin? ast)
      (instance-of? (slot-value ast :decl) <sat-global-decl>))) ;; it is an uninterpreted constant...
(define-method (sal-ast/sat-ground? (ast <sal-lambda>) (env <primitive>))
  (let ((local-decls (slot-value ast :local-decls)))
    (sal-ast/sat-ground? (slot-value ast :expr) (update-env* env local-decls (map (lambda (_) #t) local-decls)))))
(define-method (sal-ast/sat-ground? (ast <sal-scalar>) (env <primitive>)) #t)
(define-method (sal-ast/sat-ground? (ast <sal-numeral>) (env <primitive>)) #t)
(define-method (sal-ast/sat-ground? (ast <sal-application>) (env <primitive>))
  (and (sal-ast/sat-ground? (slot-value ast :fun) env)
       (for-all (cut sal-ast/sat-ground? <> env) (sal-application/argument-list ast))))

(define (can-eq eq)
  (multiple-value-bind
      (lhs rhs)
      (sal-binary-application/arguments eq)
    (cond
     ((not (instance-of? lhs <sal-name-expr>))
      (values rhs lhs))
     ((instance-of? lhs <sal-scalar>)
      (values rhs lhs))
     ((and (instance-of? (slot-value lhs :decl) <sat-global-decl>)
           (instance-of? rhs <sal-name-expr>))
      (values rhs lhs))
     ;; if both sides are name-exprs then give priority to aux-vars
     ((and (instance-of? rhs <sal-name-expr>) 
           (instance-of? (slot-value rhs :decl) <sat-aux-decl>))
      (values rhs lhs))
     (else
      (values lhs rhs)))))

(define (populate-assignment-table! constraint-list assignment-table)
  (let ((new-constraint-queue (make-queue)))
    (for-each (lambda (expr)
                (if (instance-of? expr <sal-eq>)
                  (multiple-value-bind
                      (lhs rhs)
                      (can-eq expr)
                    (cond
                     ((and (instance-of? lhs <sal-name-expr>)
;                            (begin (breakpoint "assign" (lhs rhs expr assignment-table sal-ast/sat-ground?)
;                                               (sal-ast/find (lambda (n) (and (instance-of? n <sal-name-expr>)
;                                                                              (eq? (sal-name-ref/name n)
;                                                                                   'trash)))
;                                                             expr))
;                                   #t)
                           (sal-ast/sat-ground? rhs *empty-env*)
                           (not (eq-hash-table/get assignment-table (slot-value lhs :decl))) ;; it is not already in the table...
                           (not (sal-ast/contains-reference? rhs (slot-value lhs :decl))))
                      (eq-hash-table/put! assignment-table (slot-value lhs :decl) rhs))
                     (else
                      (queue/insert! new-constraint-queue expr))))
                  (queue/insert! new-constraint-queue expr)))
              constraint-list)
    (queue->list new-constraint-queue)))

(define (remove-aux-vars-from-assignment-table! assignment-table)
  (let ((decls-to-remove '()))
    (eq-hash-table/for-each-key (lambda (decl)
                                  (when (instance-of? decl <sat-aux-decl>)
                                    (push! decl decls-to-remove)))
                                assignment-table)
    (for-each (cut eq-hash-table/delete! assignment-table <>) decls-to-remove)))

(define-generic (sal-ast/apply-assignment ast env assignment-table))
(define-method (sal-ast/apply-assignment (ast <sal-ast>) (env <primitive>) (assignment-table <primitive>))
  (sal-ast/map ast env (lambda (child-ast new-env) (sal-ast/apply-assignment child-ast new-env assignment-table))))
(define-method (sal-ast/apply-assignment (ast <sal-name-expr>) (env <primitive>) (assignment-table <primitive>))
  (cond
   ((eq-hash-table/get assignment-table (slot-value ast :decl)) =>
    cdr)
   (else
    (sal-name-ref/map ast env))))

;; use the assignment table to simply constraints...
(define (sal-constraint/apply-assignment! constraint assignment-table constraint-queue)
  (let* ((tmp-constraint (sal-ast/apply-assignment constraint *empty-env* assignment-table))
         (new-constraint (if (eq? constraint tmp-constraint)
                           tmp-constraint
                           (let ((new-constraint (sal-ast/simplify-without-converting-inequalities tmp-constraint)))
                             (trace 'sat-generic-context-result "Simplifying constraint using assignment\nbefore: ~a\nafter: ~a"
                                    (sal-ast->list constraint)
                                    (sal-ast->list new-constraint))
                             new-constraint))))
    (when (sal-expr/false? new-constraint)
      ;; (print "old-constraint: " (sal-ast->list constraint) ", new-constraint: " (sal-ast->list new-constraint))
      (sign-error "An incompleteness was detected in the decision procedure (or a bug in the translator). The counterexample produced by the decision procedure is not valid."))
    (unless (sal-expr/true? new-constraint)
      (queue/insert! constraint-queue new-constraint))))

(define (remove-duplicate-constraints constraint-list)
  (let ((new-constraint-queue (make-queue))
        (already-found (make-sal-ast-table)))
    (for-each (lambda (constraint)
                (unless (sal-ast-table/get already-found constraint)
                  (queue/insert! new-constraint-queue constraint)
                  (sal-ast-table/put! already-found constraint #unspecified)))
              constraint-list)
    (queue->list new-constraint-queue)))

(define (constraint-list/collect-and-apply-assignments constraint-list)
  (let ((assignment-table (make-eq-hash-table)))
    (let loop ((constraint-list constraint-list))
      ;; (display-constraint-list constraint-list)
      (let* ((size-before (eq-hash-table/size assignment-table))
             (constraint-list (populate-assignment-table! constraint-list assignment-table))
             (size-after (eq-hash-table/size assignment-table)))
        (cond
         ((= size-before size-after)
          ;; (display-constraint-list constraint-list)
          ;; (display-assignment-table assignment-table)
          (remove-aux-vars-from-assignment-table! assignment-table)
          (values assignment-table (remove-duplicate-constraints constraint-list)))
         (else
          (let ((new-constraint-queue (make-queue)))
            (for-each (lambda (expr)
                        (sal-constraint/apply-assignment! expr assignment-table new-constraint-queue))
                      constraint-list)
            (loop (queue->list new-constraint-queue)))))))))

;; collect auxiliary variables that can be eliminated by substitution...
(define (collect-aux-substitutions! constraint-list)
  (let ((subst-table (make-eq-hash-table))
        (new-constraint-queue (make-queue)))
    (for-each (lambda (expr)
                (if (and (instance-of? expr <sal-eq>)
                         (not (instance-of? expr <sal-cyclic-eq>)))
                  (multiple-value-bind
                      (lhs rhs)
                      (can-eq expr)
;                     (breakpoint "subst" (lhs rhs) (and (instance-of? lhs <sal-name-expr>)
;                                                        (eq? (sal-name-ref/name lhs) 'ite_aux!52)))
;                     (breakpoint "subst" (lhs rhs) (and (instance-of? rhs <sal-name-expr>)
;                                                        (eq? (sal-name-ref/name rhs) 'ite_aux!52)))
                    (cond
                     ((and (instance-of? lhs <sal-name-expr>)
                           (instance-of? (slot-value lhs :decl) <sat-aux-decl>)
                           (not (eq-hash-table/get subst-table (slot-value lhs :decl))) ;; it is not already in the table
                           (not (sal-ast/contains-reference? rhs (slot-value lhs :decl))))
                      (eq-hash-table/put! subst-table (slot-value lhs :decl) rhs))
                     (else
                      (queue/insert! new-constraint-queue expr))))
                  (queue/insert! new-constraint-queue expr)))
              constraint-list)
    ;; prepare substitution environment
    (let ((pseudo-assignment-table (make-eq-hash-table)))
      (let inner-loop ()
        (let ((vars-to-remove-from-table '()))
          (eq-hash-table/for-each (lambda (decl expr)
                                    (unless (sal-ast/contains-reference-in-table? expr subst-table)
                                      (eq-hash-table/put! pseudo-assignment-table decl expr)
                                      (push! decl vars-to-remove-from-table)))
                                  subst-table)
          (for-each (lambda (to-remove)
                      (eq-hash-table/delete! subst-table to-remove))
                    vars-to-remove-from-table)
          ;; apply the substitution over the remaining variables
          (eq-hash-table/for-each-entry (lambda (entry)
                                          (let* ((expr (cdr entry))
                                                 (new-expr (sal-ast/apply-assignment expr *empty-env* pseudo-assignment-table)))
                                            (unless (eq? expr new-expr)
                                              (set-cdr! entry (sal-ast/simplify-without-converting-inequalities new-expr)))))
                                        subst-table)
          (unless (null? vars-to-remove-from-table)
            (inner-loop))))
      ;; return remaining elements of subst-table to the constraint-list
      (eq-hash-table/for-each (lambda (decl expr)
                                (let ((equality (quick-change-class! 
                                                 (make-sal-equality (make-sal-name-expr decl expr)
                                                                    expr)
                                                 <sal-cyclic-eq>)))
                                  (queue/insert! new-constraint-queue equality)))
                              subst-table)
      (values (queue->list new-constraint-queue) pseudo-assignment-table))))

(define (constraint-list/perform-aux-substitutions! constraint-list)
  (let loop ((constraint-list constraint-list))
    ;; (print "====================================")
    ;; (display-constraint-list constraint-list)
    (multiple-value-bind
        (constraint-list pseudo-assignment-table)
        (collect-aux-substitutions! constraint-list)
      (if (= (eq-hash-table/size pseudo-assignment-table) 0)
        constraint-list
        (let ((new-constraint-queue (make-queue)))
          (for-each (lambda (expr)
                      (sal-constraint/apply-assignment! expr pseudo-assignment-table new-constraint-queue))
                    constraint-list)
          (loop (queue->list new-constraint-queue)))))))

(define-generic (expected-type ast))
(define-method (expected-type (ast <sal-application>))
  (sal-function-type/range (sal-expr/type (slot-value ast :fun))))
;   (let* ((fun (slot-value ast :fun))
;          (ty (sal-expr/type fun)))
;     (if (sal-type/function? ty)
;       (sal-function-type/range ty)
;       #f)))
(define-method (expected-type (ast <sal-expr>))
  (sal-expr/type ast))
(define-method (expected-type (ast <sal-update-expr>))
  #f)
;; (define-method (expected-type (ast <sal-lambda>))
;;  #f)

(define-generic (sal-ast/recover-scalars-from-integers ast expected-type))
(define-method (sal-ast/recover-scalars-from-integers (ast <sal-name-expr>) (expected-type <primitive>)) ast)
(define-method (sal-ast/recover-scalars-from-integers (ast <sal-true>) (expected-type <primitive>)) ast)
(define-method (sal-ast/recover-scalars-from-integers (ast <sal-false>) (expected-type <primitive>))  ast)
(define-method (sal-ast/recover-scalars-from-integers (ast <sal-numeral>) (expected-type <primitive>)) ast)
(define-method (sal-ast/recover-scalars-from-integers (ast <sal-numeral>) (expected-type <sal-type>))
  (if (sal-type/scalar? expected-type)
    (let ((elements (sal-scalar-type/elements expected-type))
          (idx (mpq->integer (slot-value ast :num))))
      (if (< idx (length elements))
        (list-ref elements idx)
        (sign-error "A bug was found when decoding the counterexample. Please contact support.")))
    ast))
;; (define-method (sal-ast/recover-scalars-from-integers (ast <sal-lambda>) (expected-type <primitive>))
  ;; the scalars in the body of the lambdas were not converted to integers...
;;  ast)

(define (sal-builtin-app/recover-scalars-from-interger-core ast)
  ;; some builtin applications can handle arbitrary number of arguments...
  (copy-ast ast
            :fun (sal-ast/recover-scalars-from-integers (slot-value ast :fun) #f)
            :arg (apply make-application-argument (map (cut sal-ast/recover-scalars-from-integers <> #f) (sal-application/argument-list ast)))))
(define-method (sal-ast/recover-scalars-from-integers (ast <sal-arith-application>) (expected-type <primitive>))
  (sal-builtin-app/recover-scalars-from-interger-core ast))
(define-method (sal-ast/recover-scalars-from-integers (ast <sal-propositional-application>) (expected-type <primitive>))
  (sal-builtin-app/recover-scalars-from-interger-core ast))
(define-method (sal-ast/recover-scalars-from-integers (ast <sal-application>) (expected-type <primitive>))
  (let* ((fun (slot-value ast :fun))
         (new-fun (sal-ast/recover-scalars-from-integers fun #f))
         (argument-list (sal-application/argument-list ast))
         (domain-types (sal-function-type/domain-types (sal-expr/type new-fun)))
         (_ [sal-assert "recover-scalars-from-integers" (ast expected-type argument-list domain-types fun) (= (length argument-list)
                                                                                                              (length domain-types))])
         (new-argument-list (map (lambda (arg expected-arg-type)
                                   (sal-ast/recover-scalars-from-integers arg expected-arg-type))
                                 argument-list
                                 domain-types)))
    (copy-ast ast 
              :fun new-fun
              :arg (apply make-application-argument new-argument-list))))
(define-method (sal-ast/recover-scalars-from-integers (ast <sal-function-update>) (expected-type <primitive>))
  (let* ((target (slot-value ast :target))
         (new-target (sal-ast/recover-scalars-from-integers target #f))
         (idx (slot-value ast :idx))
         (domain-type (car (sal-function-type/domain-types (sal-expr/type new-target))))
         (new-idx (sal-ast/recover-scalars-from-integers idx domain-type))
         (val (slot-value ast :new-value))
         (new-val (sal-ast/recover-scalars-from-integers val (sal-function-type/range (sal-expr/type new-target)))))
    (copy-ast ast
              :target new-target
              :idx new-idx
              :new-value new-val)))
(define-method (sal-ast/recover-scalars-from-integers (ast <sal-array-update>) (expected-arg-type <primitive>))
  (let* ((target (slot-value ast :target))
         (new-target (sal-ast/recover-scalars-from-integers target #f))
         (new-target-type (sal-expr/type new-target))
         (domain (sal-function-type/domain new-target-type))
         (range (sal-function-type/range new-target-type)))
    (copy-ast ast 
              :target new-target
              :idx (sal-ast/recover-scalars-from-integers (slot-value ast :idx) domain)
              :new-value (sal-ast/recover-scalars-from-integers (slot-value ast :new-value) range))))
(define (sal-eq/recover-scalars-from-integers-core ast)
  (multiple-value-bind
      (arg1 arg2)
      (sal-binary-application/arguments ast)
    (let* ((t1 (expected-type arg1))
           (t2 (expected-type arg2))
           (expected-type (cond
                           ((and t1 (sal-type/scalar? t1)) t1)
                           ((and t2 (sal-type/scalar? t2)) t2)
                           (else
                            (or t1 t2))))
           (new-arg1 (sal-ast/recover-scalars-from-integers arg1 expected-type))
           (new-arg2 (sal-ast/recover-scalars-from-integers arg2 expected-type)))
      (copy-ast ast
                :arg (make-application-argument new-arg1 new-arg2)))))
(define-method (sal-ast/recover-scalars-from-integers (ast <sal-eq>) (expected-type <primitive>))
  (sal-eq/recover-scalars-from-integers-core ast))
(define-method (sal-ast/recover-scalars-from-integers (ast <sal-diseq>) (expected-type <primitive>))
  (sal-eq/recover-scalars-from-integers-core ast))

(define (constraint-list/recover-scalars-from-intergers ctx constraint-list)
  (let ((scalar->int-trace-info (slot-value ctx :scalar->int-trace-info)))
    ;; restore declarations
    (eq-hash-table/for-each (lambda (decl type)
                              (set-slot-value! decl :type type))
                            scalar->int-trace-info)
    (let ((new-constraint-queue (make-queue)))
      (for-each (lambda (expr)
                  ;; counterexample may contain inequality constrains associated with
                  ;; scalars... so I must remove them
                  (unless (and (instance-of? expr <sal-inequality>) ;; is an inequality
                               ;; and contain a reference to a scalar->int var
                               (sal-ast/find (lambda (n) (and (instance-of? n <sal-name-expr>)
                                                              (eq-hash-table/get scalar->int-trace-info (slot-value n :decl))))
                                             expr))
                    (queue/insert! new-constraint-queue (sal-ast/recover-scalars-from-integers expr #f))))
                constraint-list)
      (queue->list new-constraint-queue))))


(define (collect-scalar-bit-assignments constraint-list)
  (let ((new-constraint-queue (make-queue))
        (scalar-bit-assignments (make-eq-hash-table)))
    (for-each (lambda (expr)
                (cond
                 ((instance-of? expr <sal-eq>)
                  (multiple-value-bind
                      (lhs rhs)
                      (sal-binary-application/arguments expr)
                    (cond
                     ((and (instance-of? lhs <sal-name-expr>)
                           (instance-of? (slot-value lhs :decl) <sat-scalar-bit-decl>))
                      [assert (rhs) (or (instance-of? rhs <sal-true>) (instance-of? rhs <sal-false>))]
                      (eq-hash-table/put! scalar-bit-assignments (slot-value lhs :decl) rhs))
                     (else
                      (queue/insert! new-constraint-queue expr)))))
                 (else
                  (queue/insert! new-constraint-queue expr))))
              constraint-list)
    (values (queue->list new-constraint-queue) scalar-bit-assignments)))
                      

(define (constraint-list/recover-scalars-from-booleans ctx constraint-list)
  (multiple-value-bind
      (constraint-list scalar-bit-assignments)
      (collect-scalar-bit-assignments constraint-list)
    (let ((scalar->bool-trace-info (slot-value ctx :scalar->bool-trace-info)))
      (eq-hash-table/for-each (lambda (scalar-decl bit-decl-list)
                                (let* ((bit-list (map (lambda (bit-decl)
                                                        (cond
                                                         ((eq-hash-table/get scalar-bit-assignments bit-decl) =>
                                                          cdr)
                                                         (else
                                                          ;; it is a don't care
                                                          (make-sal-false bit-decl))))
                                                      bit-decl-list))
                                       (scalar-value (bit-list->sal-value (slot-value scalar-decl :type) bit-list))
                                       (scalar-constraint (make-sal-equality (make-sal-name-expr scalar-decl)
                                                                             scalar-value)))
                                  (push! scalar-constraint constraint-list)))
                              scalar->bool-trace-info)
      constraint-list)))

(define (constraint-list/recover-scalars ctx constraint-list)
  (let ((tmp-constraint-list (constraint-list/recover-scalars-from-intergers ctx constraint-list)))
    (constraint-list/recover-scalars-from-booleans ctx tmp-constraint-list)))

(define (display-constraint-list constraint-list)
  (print "constraints:")
  (for-each (lambda (expr)
              (sal/pp expr)
              (print ""))
            constraint-list)
  (print "----------------------"))

(define (display-assignment-table assignment-table)
  (print "assignment table: ")
  (eq-hash-table/for-each (lambda (decl expr)
                            (display* (sal-decl/name decl) " |-> ")
                            (sal/pp expr)
                            (print ""))
                          assignment-table)
  (print "---------------------------"))

(define-generic (sat-generic-context-result/cleanup! ctx-result))
(define-method (sat-generic-context-result/cleanup! (ctx-result <sat-generic-context-result>))
  (let ((ctx (slot-value ctx-result :sat-context))
        (constraint-list (slot-value ctx-result :result-constraint-list)))
    ;; (display-constraint-list constraint-list)
    (verbose-message 4 "    simplifying result produced by the decision procedure...")
    ;;      recover scalars
    (let* ((constraint-list-with-scalars (constraint-list/recover-scalars ctx constraint-list)) 
           ;; restore constraints which were eliminated
           (complete-constraint-list (append constraint-list-with-scalars (queue->list (slot-value ctx :eliminated-constraint-queue))))
           (constraint-list-without-aux (constraint-list/perform-aux-substitutions! complete-constraint-list)))
      ;; (display-constraint-list constraint-list-without-aux)
      (constraint-list/collect-and-apply-assignments constraint-list-without-aux))))

