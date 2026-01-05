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

(module sal-flat-module-to-bdd-fsm
        (include "sal.sch")
        (include "fast-hash-table.sch")
        (import sal-ast-env bdd sal-ast-for-each queue runtime 
                front-end sal-module ordering sal2bdd sal-bdd-context
                sal-bdd-fsm sal-bdd-cluster)
        (export (sal-flat-module->sal-bdd-fsm flat-module)
                (sal-bdd-fsm/exec-pre-search-thunk-enabling-bdd-var-reordering-when-active fsm thunk)
                (sal-bdd-fsm/reorder! fsm)
                ;; configuration options
                (sal-bdd-fsm/set-reordering-method! id)
                (sal-bdd-fsm/set-static-order! id)
                (sal-bdd-fsm/enable-dynamic-reordering! flag)
                (sal-bdd-fsm/enable-dynamic-reordering-definitions! flag)
                (sal-bdd-fsm/enable-dynamic-reordering-before-search! flag)
                (sal-bdd-fsm/enable-next-curr-var-grouping! flag)
                (sal-bdd-fsm/set-input-order-file-name! file-name)
                (sal-bdd-fsm/set-output-order-file-name! file-name)
                (sal-bdd-fsm/set-num-forced-reorders n)
                (sal-bdd-fsm/set-sift-max-var! n)
                (sal-bdd-fsm/set-sift-max-swap! n)
                (sal-bdd-fsm/set-monolithic! flag))
        )
        
;; -------------------------------------------------------------------------
;;
;; Build BDD transition relation
;;
;; -------------------------------------------------------------------------
(define *sal-bdd-fsm-reordering-method* 'bdd-reorder-sift)
(define *sal-bdd-fsm-static-order* 'min-supp)
(define *sal-bdd-fsm-group-next-curr?* #t)
(define *sal-bdd-fsm-dynamic-reorder?* #f)
(define *sal-bdd-fsm-dynamic-reorder-definitions?* #t)
(define *sal-bdd-fsm-dynamic-reorder-before-search?* #f)
(define *sal-bdd-fsm-sift-max-swap* #f)
(define *sal-bdd-fsm-sift-max-var* #f)
(define *sal-bdd-fsm-monolithic?* #f)
(define *sal-bdd-fsm-num-reorderings* 1)
(define *sal-bdd-fsm-input-order-file-name* #f)
(define *sal-bdd-fsm-output-order-file-name* #f)

(define *reordering-method-doc* "Set the BDD dynamic reordering strategy. Available strategies: none, random, random-pivot, sift, sift-converge, symm-sift, symm-sift-conv, window2, window3, window4, window2-conv, window3-conv, window4-conv, group-sift, group-sift-conv, annealing, genetic, linear, linear-converge, lazy-sift, exact (default: sift).")
(define *static-order-doc*  "Set the strategy to compute the static initial BDD variable order. Available strategies: none, simple, random, min-supp, min-supp2, min-comm, min-comm2, user (default: min-supp).")
(define *num-forced-reorders-doc* "Set the number of forced variable reorderings to be performed after the construction of the transition relation (default: 1).")

(define (mk-toggle-doc-from-suffix suffix)
  (string-append "Enable/Disable " suffix))
(define *reordering-doc-suffix* "dynamic reordering of BDD variables (default: disabled).")
(define *reordering-def-doc-suffix* "dynamic reordering of variables when building BDDs representing the definition section (default: enabled). This option is ignored when --enable-dynamic-reorder or --enable-build-dyn-reorder are used.")
(define *reordering-bs-doc-suffix*  "dynamic reordering of BDD variables only before starting the fixpoint computations, that is, when building BDDs representing the transition relation, initial states, properties (default: disabled). This option is ignored when --enable-dynamic-reorder is used.")
(define *group-next-curr-doc-suffix*  "group of next-present variables (default: enabled).")
(define *sift-max-var-doc* "Set the maximum number of variables that will be sifted for each invocation of sifting. This option is in effect only if the reordering method is sift.")
(define *sift-max-swap-doc*  "Set the maximum number of swaps that will be attempted for each invocation of sifting. This option is in effect only if the reordering method is sift.")
(define *monolithic-doc* "Build a monolithic transition relation.")

(define-api (sal-bdd-fsm/set-reordering-method! (id symbol?))
  :doc *reordering-method-doc*
  (set! *sal-bdd-fsm-reordering-method* id))

(define-api (sal-bdd-fsm/enable-dynamic-reordering! (flag boolean?))
  :doc (mk-toggle-doc-from-suffix *reordering-doc-suffix*)
  (set! *sal-bdd-fsm-dynamic-reorder?* flag))

(define-api (sal-bdd-fsm/enable-dynamic-reordering-definitions! (flag boolean?))
  :doc (mk-toggle-doc-from-suffix *reordering-def-doc-suffix*)
  (set! *sal-bdd-fsm-dynamic-reorder-definitions?* flag))

(define-api (sal-bdd-fsm/enable-dynamic-reordering-before-search! (flag boolean?))
  :doc (mk-toggle-doc-from-suffix *reordering-bs-doc-suffix*)
  (set! *sal-bdd-fsm-dynamic-reorder-before-search?* flag))

(define-api (sal-bdd-fsm/enable-next-curr-var-grouping! (flag boolean?))
  :doc (mk-toggle-doc-from-suffix *group-next-curr-doc-suffix*)
  (set! *sal-bdd-fsm-group-next-curr?* flag))

(define-api (sal-bdd-fsm/set-static-order! (id symbol?))
  :doc *static-order-doc*
  (unless (memq id '(none simple random min-supp min-supp2 min-comm min-comm2 user))
    (sign-invalid-arg "Unknown static order strategy: `~a'." id))
  (set! *sal-bdd-fsm-static-order* id))

(define-api (sal-bdd-fsm/set-input-order-file-name! file-name)
  :doc "Set the file name which contains a user-defined BDD variable order. The static variable order method is marked as user-define."
  (unless (file-exists? file-name)
    (sign-error "File `~a' does not exist." file-name))
  (set! *sal-bdd-fsm-input-order-file-name* file-name)
  (set! *sal-bdd-fsm-static-order* 'user))

(define-api (sal-bdd-fsm/set-output-order-file-name! file-name)
  :doc "Set the file which will be used to store the BDD variable order."
  (set! *sal-bdd-fsm-output-order-file-name* file-name))

(define-api (sal-bdd-fsm/set-num-forced-reorders n)
  :doc *num-forced-reorders-doc*
  (set! *sal-bdd-fsm-num-reorderings* n))

(define-api (sal-bdd-fsm/set-sift-max-var! n)
  :doc *sift-max-var-doc*
  (set! *sal-bdd-fsm-sift-max-var* n))

(define-api (sal-bdd-fsm/set-sift-max-swap! n)
  :doc *sift-max-swap-doc*
  (set! *sal-bdd-fsm-sift-max-swap* n))

(define-api (sal-bdd-fsm/set-monolithic! (flag boolean?))
  :doc *monolithic-doc*
  (set! *sal-bdd-fsm-monolithic?* flag))

(front-end/add-full-option! 
 "BDD Interface" 
 "-r <name>" 
 "--reorder-method=<name>"
 *reordering-method-doc*
 (lambda (arg) 
   (let ((strategy-id (string->symbol (string-append "bdd-reorder-" arg))))
     (sal-bdd-fsm/set-reordering-method! strategy-id))))

(front-end/add-toggle-option! 
 "BDD Interface" 
 "dynamic-reorder" 
 *reordering-doc-suffix*
 (lambda (flag)
   (set! *sal-bdd-fsm-dynamic-reorder?* flag)))

(front-end/add-toggle-option!
 "BDD Interface"
 "def-dyn-reorder"
 *reordering-def-doc-suffix*
 (lambda (flag)
   (set! *sal-bdd-fsm-dynamic-reorder-definitions?* flag)))

(front-end/add-toggle-option!
 "BDD Interface"
 "bs-dyn-reorder"
 *reordering-bs-doc-suffix*
 (lambda (flag)
   (set! *sal-bdd-fsm-dynamic-reorder-before-search?* flag)))

(front-end/add-toggle-option! 
 "BDD Interface" 
 "var-group" 
 *group-next-curr-doc-suffix*
 (lambda (flag)
   (set! *sal-bdd-fsm-group-next-curr?* flag)))

(front-end/add-simple-option!
 "BDD Interface" 
 "--static-order=<name>"
 *static-order-doc*
 (lambda (name)
   (sal-bdd-fsm/set-static-order! (object->symbol name))))

(front-end/add-full-option!
 "BDD Interface"
 "-io <file-name>"
 "--input-order=<file-name>"
 "Load the BDD variable order from the given file."
 (lambda (file-name)
   (sal-bdd-fsm/set-input-order-file-name! file-name)))

(front-end/add-full-option!
 "BDD Interface"
 "-oo <file-name>"
 "--output-order=<file-name>"
 "Save the BDD variable order to the given file. The order is saved after the construction of the transition relation."
 (lambda (file-name)
   (sal-bdd-fsm/set-output-order-file-name! file-name)))

(front-end/add-simple-option! 
 "BDD Interface" 
  "--dreorder-threshold=<num>"
  "Set the threshold for the first dynamic reordering. The threshold is in number of nodes and is in effect only if dynamic reordering is enabled (default: 200000)."
  (front-end-adapter/nz-nat-arg 
  (lambda (arg)
    (bdd/set-dynamic-reorder-initial-threshold arg))))

(bdd/set-dynamic-reorder-initial-threshold 100000)
(bdd/set-dynamic-reorder-threshold-increment 200000)

;; REMARK:
;; The following option was removed since CUDD was producing strange behavior
;;

; (front-end/add-simple-option! 
;  "BDD Interface" 
;  "--dreorder-inc-threshold=<num>"
;  "Set the threshold increment for the next dynamic reordering step. The threshold is in number of nodes and is in effect only if dynamic reordering is enabled (default: 200000)."
;  (front-end-adapter/nz-nat-arg 
;   (lambda (arg)
;     (bdd/set-dynamic-reorder-threshold-increment arg))))

(front-end/add-simple-option!
 "BDD Interface" 
 "--num-reorders=<num>" 
 *num-forced-reorders-doc*
 (front-end-adapter/nat-arg
  (lambda (arg)
    (set! *sal-bdd-fsm-num-reorderings* arg))))

(front-end/add-simple-option! 
 "BDD Interface" 
 "--sift-max-var=<num>"
 *sift-max-var-doc*
 (front-end-adapter/nz-nat-arg 
  (lambda (arg)
    (set! *sal-bdd-fsm-sift-max-var* arg))))

(front-end/add-simple-option! 
 "BDD Interface" 
 "--sift-max-swap=<num>"
 *sift-max-swap-doc*
 (front-end-adapter/nz-nat-arg 
  (lambda (arg)
    (set! *sal-bdd-fsm-sift-max-swap* arg))))

(front-end/add-full-option! 
 "BDD Interface" 
 "-m"
 "--monolithic"
 *monolithic-doc*
 (lambda ()
   (set! *sal-bdd-fsm-monolithic?* #t)))

(define (sal-bdd-fsm/exec-pre-search-thunk-enabling-bdd-var-reordering-when-active fsm thunk)
  (let ((m (sal-bdd-context/manager fsm)))
    (when *sal-bdd-fsm-dynamic-reorder-before-search?*
      (bdd/enable-dynamic-reordering! m *sal-bdd-fsm-reordering-method*))
    (unwind-protect
     (thunk)
     (unless *sal-bdd-fsm-dynamic-reorder?*
       (bdd/disable-dynamic-reordering! m)))))
  
(define (report-causal-cycle remaining)
  ;; in the current version, I do not allow causal cycles in the definition section...
  ;; causal cycles can be handled, if we map auxiliary variables to full state variables...
  (sign-error "The BDD translator found a causal cycle in the following variables: ~a"
              (map (lambda (pair)
                     (format-with-location (car pair) ""))
                   remaining)))

(define-generic (sal-bdd-fsm/reorder! fsm))

(define-method (sal-bdd-fsm/reorder! (fsm <sal-bdd-fsm>))
  (let ((manager (sal-bdd-fsm/manager fsm)))
    (status-message :smc-num-bdd-nodes-before-reordering (bdd/num-nodes manager))
    (bdd/reorder! manager *sal-bdd-fsm-reordering-method*)
    (status-message :smc-num-bdd-nodes-after-reordering (bdd/num-nodes manager))))

(define (fill-env-with-implicit-lets! alist fsm)
  (let ((bdd-manager (slot-value fsm :bdd-manager)))
    (let loop ((alist alist) ;; association list from decl --> expression (the expression defines the declaration)
               (remaining '()) ;; association list of decl that cannot be processed yet, because they may depend on the value of other decls
               (changed? #f)) ;; true, if at least one decl was processed in the current iteration
      (if (null? alist)
        (cond
         ((null? remaining)
          ;; return
          #unspecified)
         ((not changed?)
          [assert (remaining) (not (null? remaining))]
          (report-causal-cycle remaining))
         (else
          ;; it is necessary to perform another pass, since there are definitions to process...
          (loop (reverse! remaining)  '() #f)))
        (let* ((curr-pair (car alist))
               (decl (car curr-pair))
               (ast (cdr curr-pair)))
          (try
           (let ((bdd (sal-expr->bdd-core ast (make-empty-env) fsm)))
             (sal-bdd-context/add-curr-var! fsm decl bdd)
             (sal-bdd-context/add-next-var! fsm decl (sal-bdd-fsm/map-curr->next fsm bdd))
             (trace 'bdd-trans "expr ~a was converted with success" (sal-ast->list ast))
             (loop (cdr alist) remaining #t))
           (catch 'internal-bdd-translation-error
                  (lambda (msg)
                    (trace 'bdd-trans "failed to convert ~a" (sal-ast->list ast))
                    (loop (cdr alist) (cons curr-pair remaining) changed?)))))))))

(define (state-vars->bdd-vars flat-module bdd-manager)
  (status-message :smc-creating-state-bdd-vars)
  (verbose-message 2 "  creating BDD variables...")
  (let* ((var-ordering (case *sal-bdd-fsm-static-order*
                         ((simple) (var-order/simple flat-module))
                         ((random) (var-order/random flat-module))
                         ((min-supp) (var-order/min-support flat-module #f))
                         ((min-supp2) (var-order/min-support flat-module #t))
                         ((min-comm) (var-order/min-comm flat-module #f))
                         ((min-comm2) (var-order/min-comm flat-module #t))
                         ((user) 
                          (unless *sal-bdd-fsm-input-order-file-name*
                            (sign-error "The BDD variable order file was not specified."))
                          (var-order/from-file flat-module *sal-bdd-fsm-input-order-file-name*))
                         (else
                          (var-order/default flat-module))))
         (_ [sal-assert "order" (var-ordering flat-module) (= (length var-ordering) (length (var-order/default flat-module)))])
         ;; (_ (print "order: " var-ordering))
         (curr-vars (make-eq-hash-table))
         (next-vars (make-eq-hash-table))
         (choice-indices (make-queue))
         (input-indices (make-queue))
         (curr-indices (make-queue))
         (next-indices (make-queue))
         (next-var-id 0)
         (mk-bdd-var (lambda ()
                       (let ((result next-var-id))
                         (bdd/ith-var bdd-manager next-var-id)
                         (set! next-var-id (+ next-var-id 1))
                         result))))
    (for-each (lambda (var-name)
                (let ((curr-var-id (mk-bdd-var))
                      (next-var-id (mk-bdd-var))
                      (state-var-decl (sal-module/lookup-var flat-module var-name)))
                  (when *sal-bdd-fsm-group-next-curr?*
                    (bdd/group-vars! bdd-manager curr-var-id 2))
                  (queue/insert! curr-indices curr-var-id)
                  (queue/insert! next-indices next-var-id)
                  (eq-hash-table/put! curr-vars state-var-decl (bdd/ith-var bdd-manager curr-var-id))
                  (eq-hash-table/put! next-vars state-var-decl (bdd/ith-var bdd-manager next-var-id))
                  (when (instance-of? state-var-decl <sal-choice-input-state-var-decl>)
                    (queue/insert! choice-indices curr-var-id))
                  (cond
                   ((instance-of? state-var-decl <sal-input-state-var-decl>)
                    (bdd/set-input-var! bdd-manager curr-var-id)
                    (bdd/set-input-var! bdd-manager next-var-id)
                    (queue/insert! input-indices curr-var-id))
                   (else
                    (bdd/set-curr-var! bdd-manager curr-var-id)
                    (bdd/set-next-var! bdd-manager next-var-id)))))
              var-ordering)
    (values curr-vars 
            next-vars
            (queue->list input-indices)
            (queue->list curr-indices)
            (queue->list next-indices)
            (queue->list choice-indices))))

(define (set-manager-options! bdd-manager)
  (cond
   (*sal-bdd-fsm-dynamic-reorder?*
    (verbose-message 3 "    BDD reordering strategy: ~a" *sal-bdd-fsm-reordering-method*)
    (bdd/enable-dynamic-reordering! bdd-manager *sal-bdd-fsm-reordering-method*))
   (else
    (bdd/disable-dynamic-reordering! bdd-manager)))
  (when *sal-bdd-fsm-sift-max-swap*
    (bdd/set-sift-max-swap! bdd-manager *sal-bdd-fsm-sift-max-swap*))
  (when *sal-bdd-fsm-sift-max-var*
    (bdd/set-sift-max-var! bdd-manager *sal-bdd-fsm-sift-max-var*)))

(define (display-trans-rel-info cluster)
  (let ((size (sal-bdd-cluster/size cluster))
        (num-clusters (sal-bdd-cluster/num-clusters cluster)))
  (status-message :smc-transition-relation-info size num-clusters)
  (verbose-message 3 "  transition relation - size: ~a (nodes), number of clusters: ~a" 
                   size
                   num-clusters)))

(define (compress-clusters! fsm)
  (unless *sal-bdd-fsm-monolithic?*
    (display-trans-rel-info (slot-value fsm :transition-relation-cluster))
    (status-message :smc-compressing-bdd-clusters)
    (verbose-message 2 "  compressing BDD clusters...")
    (display-runtime 3 "    cluster compression time: ~a secs"
      (lambda ()
        (set-slot-value! fsm :transition-relation-cluster (sal-bdd-cluster/compress 
                                                           (sal-bdd-cluster/push-tiny-bdds
                                                            (slot-value fsm :transition-relation-cluster))))
        (display-trans-rel-info (slot-value fsm :transition-relation-cluster))
        )
      :compressing-bdd-cluster-time)))
  
(define (reorder-transition-relation! fsm)
  (force-gc!)
  (let loop ((i 0))
    (when (< i *sal-bdd-fsm-num-reorderings*)
      (status-message :smc-reordering-bdd-variables)
      (verbose-message 2 "  reordering BDD variables...")
      (sal-bdd-fsm/reorder! fsm)
      (compress-clusters! fsm)
      (loop (+ i 1)))))

(define (report-fsm-statistics fsm)
  (verbose-message 3 "  statistics:")
  (verbose-message 3 "    number of nodes in initial states: ~a" (bdd/size (slot-value fsm :initial-states)))
  (verbose-message 3 "    number of nodes in transition relation: ~a" (sal-bdd-cluster/size (slot-value fsm :transition-relation-cluster)))
  (unless *sal-bdd-fsm-monolithic?*
    (verbose-message 3 "    transition relation detailed information:")
    (sal-bdd-cluster/display-info (slot-value fsm :transition-relation-cluster) 6)))

(define (check-if-inputs-are-not-used-in-next-operator flat-module)
  (let ((state-vars (slot-value flat-module :state-vars)))
    (sal-ast/for-each (lambda (ast)
                        (when (instance-of? ast <sal-next-operator>)
                          (let ((name-expr (slot-value ast :name-expr)))
                            (when (instance-of? (slot-value name-expr :decl) <sal-input-state-var-decl>)
                              (sign-unsupported-feature ast "In the SAL symbolic model checker, it is not allowed to use the next value of an input variable.")))))
                      flat-module)))

(define (sal-flat-module->sal-bdd-fsm-core flat-module)
  ;; I'm supportting next value of input variables
  ;; (check-if-inputs-are-not-used-in-next-operator flat-module)
  (let* ((definition (slot-value flat-module :definition))
         (state-vars (slot-value flat-module :state-vars))
         (implicit-lets (make-queue))
         (bdd-manager (make-bdd-manager)))
    (set-manager-options! bdd-manager)
    (collect-implicit-lets definition implicit-lets)
    (let ((implicit-let-table (make-eq-hash-table)))
      (for-each (lambda (decl-ast-pair)
                  (eq-hash-table/put! implicit-let-table (car decl-ast-pair) #f))
                (queue->list implicit-lets))
      (multiple-value-bind
          (curr-vars next-vars input-indices curr-indices next-indices choice-indices)
          (state-vars->bdd-vars flat-module bdd-manager)
        [assert (curr-indices next-indices) (= (length curr-indices) (length next-indices))]
        (status-message :smc-num-bdd-vars (bdd/num-vars bdd-manager))
        (verbose-message 2 "  number of BDD variables: ~a" (bdd/num-vars bdd-manager))
        (let* ((new-fsm (make-instance <sal-bdd-fsm> 
                                       :bdd-manager bdd-manager
                                       :flat-module flat-module
                                       :curr-vars curr-vars
                                       :next-vars next-vars
                                       :num-choice-vars (length choice-indices)
                                       :restriction (bdd/true bdd-manager) ;; default - no restriction
                                       :curr-var-array (indices-list->bdd-var-array bdd-manager curr-indices)
                                       :next-var-array (indices-list->bdd-var-array bdd-manager next-indices)
                                       :curr-indices curr-indices
                                       :next-indices next-indices
                                       :no-choice-var-array (indices-list->bdd-var-array bdd-manager (difference curr-indices choice-indices))
                                       :curr-cube (indices-list->cube bdd-manager curr-indices)
                                       :next-cube (indices-list->cube bdd-manager next-indices)
                                       :input-cube (indices-list->cube bdd-manager input-indices)
                                       :choice-cube (indices-list->cube bdd-manager choice-indices)
                                       :num-vars (length curr-indices)
                                       :tmp-bdd-var-list '()))
               (env (make-empty-env)))
          (status-message :smc-creating-definition-section)
          (verbose-message 2 "  creating definition section BDDs...")
          (when (or *sal-bdd-fsm-dynamic-reorder-definitions?* *sal-bdd-fsm-dynamic-reorder-before-search?*)
            (bdd/enable-dynamic-reordering! bdd-manager *sal-bdd-fsm-reordering-method*))
          (fill-env-with-implicit-lets! (queue->list implicit-lets) new-fsm)
          (status-message :smc-creating-valid-state-predicates)
          (verbose-message 2 "  creating valid state predicate BDDs...")
          (set-slot-value! new-fsm :valid-input (sal-expr->bdd-core (slot-value flat-module :valid-input-expr) env new-fsm))
          (set-slot-value! new-fsm :valid-latch (sal-expr->bdd-core (slot-value flat-module :valid-state-expr) env new-fsm))
          (unless (or *sal-bdd-fsm-dynamic-reorder?* *sal-bdd-fsm-dynamic-reorder-before-search?*)
            (bdd/disable-dynamic-reordering! bdd-manager))
          (status-message :smc-creating-initial-states)
          (verbose-message 2 "  creating BDD: set of initial states...")
          ;; I have to filter invalid initial states...`
          (let ((initial-states (sal-bdd-fsm/filter-invalid-state-rep
                                 new-fsm
                                 (sal-expr->bdd-core (slot-value flat-module :initialization) env new-fsm))))
            (set-slot-value! new-fsm :initial-states initial-states))
          (when (bdd/false? (slot-value new-fsm :initial-states))
            (warning-message "The set of initial states is empty."))
          (status-message :smc-creating-transition-relation)
          (verbose-message 2 "  creating BDD: transition relation...")
          (let ((transition-relation-cluster (if *sal-bdd-fsm-monolithic?*
                                               (let ((transition-relation (sal-expr->bdd-core (slot-value flat-module :transition) env new-fsm)))
                                                 (bdd->sal-bdd-monolithic-cluster new-fsm transition-relation))
                                               (sal-expr->cluster (slot-value flat-module :transition) env new-fsm))))
            (set-slot-value! new-fsm :transition-relation-cluster (sal-bdd-cluster/push-tiny-bdds transition-relation-cluster)))
	  ;;
	  ;; BD: this looks like a BUG. The flags don't have the intended effect,
	  ;; 
          (if (or *sal-bdd-fsm-dynamic-reorder?* *sal-bdd-fsm-dynamic-reorder-before-search?*)
            (compress-clusters! new-fsm)
            (reorder-transition-relation! new-fsm))
          (force-gc!)
          (report-fsm-statistics new-fsm)
          (when *sal-bdd-fsm-output-order-file-name*
            (var-order/save new-fsm *sal-bdd-fsm-output-order-file-name*))
          (unless *sal-bdd-fsm-dynamic-reorder?*
            (bdd/disable-dynamic-reordering! bdd-manager))
          new-fsm)))))
        
(define-api (sal-flat-module->sal-bdd-fsm flat-module)
  :doc "Convert a boolean flat module in a finite state machine encoded using BDDs."
  (status-message :generating-module-bdd-representation)
  (verbose-message 1 "converting flat module to BDD representation (initial states, and transition relation)...")
  (display-runtime 2 "  flat-module -> BDD conversion time: ~a secs"
    (lambda () 
      (sal-flat-module->sal-bdd-fsm-core flat-module))
    :module-bdd-representation-time))



        

