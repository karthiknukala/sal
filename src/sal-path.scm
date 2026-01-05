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

(module sal-path
        (include "sal.sch")
        (include "fast-hash-table.sch")
        (import queue sal-expression sal-expr-evaluator
                sal-ast-env sal-module sal-ast-simplify)
        (export <sal-path>
                <sal-concrete-path>
                <sal-cyclic-path>
                <sal-pseudo-cyclic-path>
                <sal-step>
                (sal-path/length sal-path)
                (sal-path/step-data sal-path idx)
                (sal-path/state-expr sal-path idx . include-choice-vars?)
                (sal-step-info/copy-vars! source-step target-step)
                (sal-path/cut path expr)
                (sal-path/cut-after path expr num-steps)
                (sal-path->cyclic-path path final-expr)
                (step-info->state-expr step-info place-provider . include-choice-vars?)
                (sal-path/evaluate-expr-at expr path idx))
        )

(define-class <sal-path> () (:flat-module :step-info-list :auxiliary-decls :global-constraint-list)
  :doc "Represents an execution path. It is used to represent counterexamples and execution traces in the simulator. The object is a path for the module referenced by slot @code{:flat-module}. @code{:step-info-list} is a list of @{<sal-step>} objects. @code{:auxiliary-decls} is a list of auxiliary declarations (this slot is used in the symbolic counterexamples produced by sal-inf-bmc). @code{:global-constraint-list} is a list of contraints associated with path (this slot is used the symbolic counterexamples produced by sal-inf-bmc).")
(define-class <sal-step> () (:assignment-table :constraint-list :transition-step)
  :doc "Represents an execution step in a SAL path. See @code{<sal-path>}. @code{:assignment-table} contains a mapping @code{eq-hash-table} from state variable declarations to SAL expressions. @code{:constraint-list} contains symbolic constraints (it is used by sal-inf-bmc). @code{:transition-step} contains information about which transition was executed (see @code{<sal-transition-step>}).")
(define-class <sal-concrete-path> (<sal-path>) ()
  :doc "Subclass of @code{<sal-path>} which represents a concrete (non-symbolic) path.")
(define-class <sal-cyclic-path> (<sal-concrete-path>) (:cycle-step-info-list)
  :doc "Represents a cyclic path. A cyclic path is represented using two list of steps, the first list @code{:step-info-list} contains the cycle prefix, and @code{:cycle-step-info-list} contains the cycle. @code{:cycle-step-info-list} is also a list of @code{<sal-step>} objects.")
(define-class <sal-pseudo-cyclic-path> (<sal-concrete-path>) ()
  :doc "Represents a cyclic path where SAL was not capable to decide in which step the cycle begins. So the cycle is represented by a single list of steps.")

(define-generic (sal-path/length sal-path)
  :doc "Returns the length of a SAL path.")
(define-method (sal-path/length (sal-path <sal-path>))
  (length (slot-value sal-path :step-info-list)))
(define-method (sal-path/length (sal-path <sal-cyclic-path>))
  'infinite)

(define-generic (sal-path/step-data sal-path idx)
  :doc "Returns the @code{idx}-th step in the path. The result is @code{<sal-step>} object.")
(define-method (sal-path/step-data (sal-path <sal-path>) (idx <primitive>))
  (let ((step-list (slot-value sal-path :step-info-list)))
    (unless (< idx (length step-list))
      (sign-error "There isn't a state with index ~a. Path has only ~a states." idx (length step-list)))
    (list-ref step-list idx)))
(define-method (sal-path/step-data (sal-path <sal-cyclic-path>) (idx <primitive>))
  (let ((prefix-step-list (slot-value sal-path :step-info-list))
        (cycle-step-list (slot-value sal-path :cycle-step-info-list)))
    (cond
     ((< idx (length prefix-step-list))
      (list-ref prefix-step-list idx))
     (else
      (let* ((r-idx (- idx (- (length prefix-step-list) 1)))   ;; prefix and cycle contain an extra state
             (_ [assert (r-idx) (>= r-idx 0)])
             (n-idx (remainder r-idx (- (length cycle-step-list) 1))))
        (list-ref cycle-step-list n-idx))))))

(define-api (sal-path/state-expr sal-path idx . include-choices?)
  :doc "Returns a SAL expression representing the @code{idx}-th step in the path. If @code{include-choices?} is true, then choice variables are included. Choice variables are auxiliary state variables introduced by SAL to track which transition was executed in each step."
  (let ((include-choices? (optional-arg include-choices? #t)))
    (sal-path/state-expr-core sal-path idx include-choices?)))
  
(define-generic (sal-path/state-expr-core sal-path idx include-choices?)) 
(define-method (sal-path/state-expr-core (sal-path <sal-path>) (idx <primitive>) (include-choices? <primitive>))
  (sign-error "Only concrete path (non-symbolic) can be handled by sal-path/state-expr."))
(define-method (sal-path/state-expr-core (sal-path <sal-concrete-path>) (idx <primitive>) (include-choices? <primitive>))
  (step-info->state-expr (sal-path/step-data sal-path idx) (slot-value sal-path :flat-module) include-choices?))

(define (step-info->state-expr step-info place-provider . include-choice-vars?)
  (let ((include-choice-vars? (optional-arg include-choice-vars? #t))
        (tmp-list '()))
    (eq-hash-table/for-each (lambda (var-decl value)
                              (when (or (not (instance-of? var-decl <sal-choice-input-state-var-decl>))
                                        include-choice-vars?)
                                (push! (cons var-decl value) tmp-list)))
                            (slot-value step-info :assignment-table))
    ;; I sort the list to avoid non deterministic behavior
    (let* ((sorted-tmp-list (sort tmp-list (lambda (e1 e2)
                                             (symbol<? (sal-decl/name (car e1)) (sal-decl/name (car e2))))))
           (c-list (map (lambda (pair)
                          (make-sal-equality (make-sal-name-expr (car pair)) (cdr pair)))
                        sorted-tmp-list))
           (c-list (append! c-list (slot-value step-info :constraint-list))))
      (make-sal-and+* c-list place-provider))))

;; --------------------------------------------------------------------
;; Copy the variables in 'source' to 'target'
;;
;; This procedure is useful when gluing paths together.
;; --------------------------------------------------------------------
(define-api (sal-step-info/copy-vars! source-step target-step)
  :doc "Copy the variables in the assignment table of the step @code{source-step} to the assignment table of the step @code{target-step}. This procedure is useful for gluing paths together."
  (let ((target-assignment-table (slot-value target-step :assignment-table))
        (source-assignment-table (slot-value source-step :assignment-table)))
    (eq-hash-table/for-each (lambda (var-decl value)
                              (eq-hash-table/put! target-assignment-table var-decl value))
                            source-assignment-table)))

;; --------------------------------------------------------------------
;; sal-path/cut
;;
;; Cut a path in the first state which satisfies a given expression.
;; This function is useful to simplify paths produced by sal-bmc where
;; the bad state is in the middle of the path.
;;
;; --------------------------------------------------------------------
(define-generic (sal-path/cut path expr)
  :doc "Cut a path at the first state that satisfies the given SAL expression. This function is used to simplify paths produced by sal-bmc where the bad state is in the middle of the path.")
(define-method (sal-path/cut (path <sal-path>) (expr <sal-expr>))
  (let ((result-step-queue (make-queue))
        (flat-module (slot-value path :flat-module))
        (step-list (slot-value path :step-info-list)))
    (move-to-queue-until! flat-module result-step-queue step-list expr)
    (copy-instance path
                   :step-info-list (queue->list result-step-queue))))

;; similar to sal-path/cut, but it cuts the path in the first step satisfying expr after num-steps...
(define-generic (sal-path/cut-after path expr num-steps)
  :doc "Similar to @code{sal-path/cut}, but the expression @code{expr} is not checked in the first @code{num-steps} steps.")
(define-method (sal-path/cut-after (path <sal-path>) (expr <sal-expr>) (num-steps <primitive>))
  (let* ((result-step-queue (make-queue))
         (flat-module (slot-value path :flat-module))
         (step-list (slot-value path :step-info-list))
         (step-list-part1 (list-head step-list num-steps))
         (step-list-part2 (list-tail step-list num-steps)))
    (queue/append! result-step-queue step-list-part1)
    (move-to-queue-until! flat-module result-step-queue step-list-part2 expr)
    (copy-instance path
                   :step-info-list (queue->list result-step-queue))))

(define (move-to-queue-until! module queue step-list expr)
  (bind-exit (exit)
    (for-each (lambda (step)
                (queue/insert! queue step)
                (let ((result (sal-step/satisfies? step module expr)))
                  (when result ;; cut the path at this point, since expr is true
                    (exit #unspecified))))
              step-list)))

(define-api (sal-step/evaluate-expr-at step flat-module expr)
  :doc "Evaluate the expression @code{expr} in the step @code{step}. @code{expr} may reference variables in the module @code{flat-module}, and @code{step} must be a step of a path for the module @code{flat-module}."
  (let ((env (make-empty-env))
        (state-vars (slot-value flat-module :state-vars))
        (assignment-table (slot-value step :assignment-table)))
    ;; create environment with the values in the <sal-step> object
    (for-each (lambda (state-var-decl)
                (cond 
                 ((eq-hash-table/get assignment-table state-var-decl) => 
                  (lambda (entry)
                    (set! env (update-env env state-var-decl (cdr entry)))))))
              state-vars)
    (sal-expr/evaluate-core expr env 0)))

(define-api (sal-step/satisfies? step flat-module expr)
  :doc "Returns @code{true} if @code{expr} evaluates to @code{true} in the given step. Returns @code{#f} otherwise or if the evaluation fails. See @code{sal-step/evaluate-expr-at}."
  (try
   (sal-expr/true? (sal-step/evaluate-expr-at step flat-module expr))
   (catch 'expr-evaluator
          (lambda (_) #f))))

(define-generic (sal-path/evaluate-expr-at path expr idx)
  :doc "Evaluate the expression @code{expr} in the @code{idx}-th step of the path @code{path}.")

(define-method (sal-path/evaluate-expr-at (path <sal-path>) (expr <sal-expr>) (step-idx <primitive>))
  (let ((flat-module (slot-value path :flat-module))
        (step (sal-path/step-data path step-idx)))
    (sal-step/evaluate-expr-at step flat-module expr)))

(define-method (sal-path/evaluate-expr-at (path <sal-path>) (expr <primitive>) (step-idx <primitive>))
  (if (string? expr)
    (let* ((flat-module (slot-value path :flat-module))
           (expr-ast (sal-module/state-expr-string->ast flat-module expr)))
      (sal-path/evaluate-expr-at path expr-ast step-idx))
    (call-next-method)))

;; --------------------------------------------------------------------
;; sal-path->cyclic-path
;;
;; Convert a path in two sections (prefix, cycle) which define a
;; cyclic path.
;;
;; --------------------------------------------------------------------
(define (sal-step/equal? step1 step2 flat-module)
  (let ((assignment-table1 (slot-value step1 :assignment-table))
        (assignment-table2 (slot-value step2 :assignment-table))
        (defined-variables (sal-module/defined-variables flat-module))
        (env (make-empty-env)))
    (bind-exit (exit)
      (eq-hash-table/for-each (lambda (var-decl value1)
                                (unless (or (instance-of? var-decl <sal-input-state-var-decl>)
                                            (eq-hash-table/contains? defined-variables var-decl)
                                            (let ((value2 (cond 
                                                           ((eq-hash-table/get assignment-table2 var-decl) => cdr)
                                                           (else #f))))
                                              (and value2
                                                   (sal-expr/equal-values? value1 value2 env))))
                                  (exit #f)))
                              assignment-table1)
      #t)))

;; return a prefix of step-list up to step s. If step s is not in step-list, then return #f
(define (sal-step-list/prefix step-list target-step flat-module)
  (let ((result-step-queue (make-queue)))
    (bind-exit (exit)
      (for-each (lambda (step)
                  (queue/insert! result-step-queue step)
                  (when (sal-step/equal? step target-step flat-module)
                    (exit (queue->list result-step-queue))))
                step-list)
      #f)))

(define-generic (sal-path->cyclic-path path final-expr))
(define-method (sal-path->cyclic-path (path <sal-path>) (final-expr <sal-expr>))
  path)
(define-method (sal-path->cyclic-path (path <sal-concrete-path>) (final-expr <sal-expr>))
  ;; (breakpoint "cyclic-path" (path final-expr) #t)
  (let ((result-step-queue (make-queue))
        (flat-module (slot-value path :flat-module))
        (step-list (slot-value path :step-info-list)))
    (let loop ((step-list step-list))
      (cond
       ((or (null? step-list)
            (null? (cdr step-list)))
        #f) ;; the path is not cyclic
       ((and (sal-step/satisfies? (car step-list) flat-module final-expr) ;; is final state
             (sal-step-list/prefix (cdr step-list) (car step-list) flat-module)) ;; there is a cycle
        =>
        (lambda (cycle-step-list)
          (queue/insert! result-step-queue (car step-list))
          ;; (breakpoint "cyclic-path2" (path final-expr cycle-step-list result-step-queue step-list) #t)
          (make-instance <sal-cyclic-path>
                         :global-constraint-list (slot-value path :global-constraint-list)
                         :auxiliary-decls (slot-value path :auxiliary-decls)
                         :step-info-list (queue->list result-step-queue)
                         :cycle-step-info-list (cons (car step-list) cycle-step-list)
                         :flat-module flat-module)))
       (else
        (queue/insert! result-step-queue (car step-list))
        (loop (cdr step-list)))))))
                         
        
      
