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

(module sal-smc-context 
        (include "sal.sch")
        (include "sal-ast-table.sch")
        (import sal-context queue fast-hash-table sal-smc sal-smc-core
                sal-flat-module-to-bdd-fsm sal-assertion sal-api sal-derived-path bdd
                sal-bdd-cluster sal2bdd sal-path-pp sal-bdd-fsm)
        (export (sal-context/check-assertions-using-smc context only-invariants? build-counter-examples?))
        )

(define-generic (x-g-property? expr))

(define-method (x-g-property? (expr <sal-expr>))
  #f)

(define-method (x-g-property? (expr <sal-ltl-x>))
  (x-g-property? (slot-value expr :arg)))

(define-method (x-g-property? (expr <sal-ctl-ax>))
  (x-g-property? (slot-value expr :arg)))

(define-method (x-g-property? (expr <sal-ltl-g>))
  (not (sal-expr/contains-temporal-operators? (slot-value expr :arg))))

(define-method (x-g-property? (expr <sal-ctl-ag>))
  (not (sal-expr/contains-temporal-operators? (slot-value expr :arg))))

(define-generic (x-g-property-body expr))

(define-method (x-g-property-body (expr <sal-expr>))
  (internal-error))

(define-method (x-g-property-body (expr <sal-ltl-x>))
  (multiple-value-bind
      (body num-x)
      (x-g-property-body (slot-value expr :arg))
    (values body (+ num-x 1))))

(define-method (x-g-property-body (expr <sal-ctl-ax>))
  (multiple-value-bind
      (body num-x)
      (x-g-property-body (slot-value expr :arg))
    (values body (+ num-x 1))))

(define-method (x-g-property-body (expr <sal-ltl-g>))
  [assert (expr) (x-g-property? expr)]
  (values (slot-value expr :arg) 0))

(define-method (x-g-property-body (expr <sal-ctl-ag>))
  [assert (expr) (x-g-property? expr)]
  (values (slot-value expr :arg) 0))

(define (build-trace-ce layered-reachable-states target-set)
  (let ((result (make-queue)))
    (let loop ((layered-reachable-states layered-reachable-states))
      [assert (layered-reachable-states) (not (null? layered-reachable-states))]
      (let* ((curr (car layered-reachable-states))
             (curr-target (bdd/and curr target-set)))
        (cond
         ((bdd/false? curr-target)
          (queue/insert! result curr)
          (loop (cdr layered-reachable-states)))
         (else
          (queue/insert! result curr-target)
          (reverse (queue->list result))))))))

(define (build-x-n-trace fsm num-xs)
  (let ((initial-states (sal-bdd-fsm/initial-states fsm)))
    (let loop ((curr initial-states)
               (trace (list initial-states))
               (num-xs num-xs))
      (if (> num-xs 0)
        (let* ((new-curr (sal-bdd-fsm/image fsm curr))
               (new-trace (cons new-curr trace)))
          (loop new-curr
                new-trace
                (- num-xs 1)))
        trace))))

(define (make-x-n-suffix fsm source-set target-set x-reachable-states)
  (let ((m (sal-bdd-fsm/manager fsm)))
    (let loop ((curr-set target-set)
               (states-in-trace (bdd/false m))
               (trace '()))
      (let* ((focus (bdd/diff (bdd/and curr-set x-reachable-states) states-in-trace))
             (source-subset (bdd/and focus source-set)))
        (cond 
         ((bdd/false? focus)
          #f)
         ((not (bdd/false? source-subset))
          (cons source-subset trace))
         (else
          (let* ((focus-choice (sal-bdd-fsm/peek-state fsm focus))
                 (new-trace (cons focus-choice trace))
                 (new-states-in-trace (bdd/or states-in-trace focus-choice))
                 (new-curr-set (sal-bdd-fsm/pre-image-with-choices fsm focus-choice)))
            (loop new-curr-set
                  new-states-in-trace
                  new-trace))))))))

(define (build-x-n-trace-ce fsm num-xs target-set x-reachable-states)
  (let* ((prefix (build-x-n-trace fsm num-xs))
         (source-set (car prefix))
         (suffix (make-x-n-suffix fsm 
                                  source-set
                                  target-set
                                  x-reachable-states)))
    (let loop ((curr (car suffix))
               (prefix (cdr prefix))
               (result suffix))
      (if (null? prefix)
        result
        (let* ((curr-prefix (car prefix))
               (new-curr (bdd/and (sal-bdd-fsm/pre-image-with-choices fsm curr) curr-prefix)))
          (loop new-curr
                (cdr prefix)
                (cons new-curr result)))))))
                
(define (display-ce decl build-counter-examples? path)
  (when build-counter-examples?
    (print "\nCounterexample for '" (sal-decl/name decl) "' located at " (format-with-location decl "") ":")
    (sal-path/pp path)))

(define (compute-x-n-reachable-states fsm num-xs reachable-states x-reachability-cache)
  (verbose-message 1 "computing the set of reachable states after ~a step(s)." num-xs)
  (let ((x-rs-queue (cond
                     ((eq-hash-table/get x-reachability-cache fsm) =>
                      cdr)
                     (else
                      (let ((result (make-queue reachable-states)))
                        (eq-hash-table/put! x-reachability-cache fsm result)
                        result)))))
    (let loop ((len (queue/length x-rs-queue)))
      (when (<= len num-xs)
        (let ((last (queue/rear x-rs-queue)))
          (verbose-message 2 "  computing the X^~a reachable-states." len)
          (queue/insert! x-rs-queue (sal-bdd-fsm/image fsm last))
          (loop (+ len 1)))))
    (list-ref (queue->list x-rs-queue) num-xs)))

(define (sal-context/check-assertions-using-smc context only-invariants? build-counter-examples?)
  (let* ((declarations (sal-context/declarations context))
         (bool-flat-module-cache (make-sal-ast-table))
         (fsm-cache (make-eq-hash-table))
         (x-reachability-cache (make-eq-hash-table))
         (result-queue (make-queue)))
    (unless (null? (sal-context/params context))
      (sign-source-error context "Full context verification can only be used to check non-parametric contexts."))
    (for-each (lambda (decl)
                (when (instance-of? decl <sal-assertion-decl>)
                  (let* ((place-provider decl)
                         (assertion-name (make-ast-instance <sal-qualified-assertion-name> place-provider
                                                            :decl decl :context-ref context :actuals '()))
                         (assertion-def (sal-assertion-name/definition assertion-name)))
                    (cond
                     ((not (instance-of? assertion-def <sal-module-models>))
                      (warning-message "Only module models assertions are supported by smc. Ignoring the assertion '~a' located at ~a." (sal-decl/name decl) (format-with-location decl "")))
                     ((or (not only-invariants?) (sal-module-models/invariant? assertion-def))
                      (verbose-message 1 "\nChecking assertion '~a' located at ~a." (sal-decl/name decl) (format-with-location decl ""))
                      (let* ((module (slot-value assertion-def :module))
                             (property (slot-value assertion-def :expr))
                             (bool-flat-module (cond
                                                ((sal-ast-table/get bool-flat-module-cache module) => 
                                                 cdr)
                                                (else 
                                                 (let ((bool-flat-module (make-boolean-flat-module module)))
                                                   (sal-ast-table/put! bool-flat-module-cache module bool-flat-module)
                                                   bool-flat-module)))))
                        (cond
                         ((or (sal-module-models/invariant? assertion-def)
                              (x-g-property? property))
                          (let* ((fsm (cond
                                       ((eq-hash-table/get fsm-cache bool-flat-module) =>
                                        cdr)
                                       (else
                                        (let ((fsm (sal-flat-module->sal-bdd-fsm bool-flat-module)))
                                          (eq-hash-table/put! fsm-cache bool-flat-module fsm)
                                          fsm))))
                                 (layered-reachable-states (sal-bdd-fsm/layered-reachable-states fsm))
                                 (reachable-states (sal-bdd-fsm/reachable-states fsm)))
                            (multiple-value-bind
                                (body-expr num-xs)
                                (x-g-property-body property)
                              (let* ((bool-body-expr (make-boolean-state-expression-core body-expr bool-flat-module))
                                     (body-bdd-expr (sal-expr->bdd bool-body-expr fsm))
                                     (x-reachable-states (compute-x-n-reachable-states fsm num-xs reachable-states x-reachability-cache))
                                     (invalid-states (bdd/and x-reachable-states (bdd/not body-bdd-expr))))
                                (cond
                                 ((bdd/false? invalid-states)
                                  (verbose-message 1 "Proved.")
                                  (queue/insert! result-queue (cons decl #f)))
                                 (else
                                  (verbose-message 1 "Invalid.")
                                  (let* ((ce-trace (if (= num-xs 0)
                                                     (build-trace-ce layered-reachable-states invalid-states)
                                                     (build-x-n-trace-ce fsm num-xs invalid-states x-reachable-states)))
                                         (ce-path (if build-counter-examples?
                                                    (sal-derived-path->original-path (sal-bdd-fsm/make-path fsm ce-trace (= num-xs 0)))
                                                    #t)))
                                    (display-ce decl build-counter-examples? ce-path)
                                    (queue/insert! result-queue (cons decl ce-path)))))))))
                         (else
                          (try 
                           (let* ((bool-property (make-boolean-state-expression-core property bool-flat-module))
                                  (flat-assertion-def-tmp1 (copy-ast assertion-def
                                                                    :module bool-flat-module
                                                                    :expr bool-property))
                                  (flat-assertion-def-tmp2 (sal-module-models/create-ltl-monitor flat-assertion-def-tmp1 #t))
                                  (flat-assertion-def (sal/slice flat-assertion-def-tmp2))
                                  (fsm (sal-flat-module->sal-bdd-fsm (slot-value flat-assertion-def :module))))
                             (multiple-value-bind
                                 (valid? ce-path)
                                 (cond
                                  ((sal-module-models/invariant? flat-assertion-def)
                                   (sal-module-models/smc-invariant-core flat-assertion-def fsm))
                                  ((sal-module-models/accepting? flat-assertion-def)
                                   (sal-module-models/smc-accepting flat-assertion-def fsm))
                                  (else
                                   (internal-error)))
                               (cond
                                (valid?
                                 (verbose-message 1 "Proved.")
                                 (queue/insert! result-queue (cons decl #f)))
                                (else
                                 (verbose-message 1 "Invalid.")
                                 (let ((ce-path (if build-counter-examples?
                                                  (sal-derived-path->original-path ce-path)
                                                  #t)))
                                   (display-ce decl build-counter-examples? ce-path)
                                   (queue/insert! result-queue (cons decl ce-path)))))))
                           (catch 'ctl->ltl-failure
                                  (lambda (_)
                                    (warning-message "Only LTL properties are supported by smc. Ignoring the assertion '~a' located at ~a." 
                                                     (sal-decl/name decl)
                                                     (format-with-location decl "")))))))))))))
              declarations)
    (queue->list result-queue)))
