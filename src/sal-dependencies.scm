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

(module sal-dependencies
        (include "sal.sch")
        (include "fast-hash-table.sch")
        (include "sal-ast-table.sch")
        (import graph sal-module wttree symbol-set sal-expression sal-decls sal-ast-for-each
                queue)
        (export (sal-flat-module/make-dependency-graph flat-module)
                (sal-flat-module/make-transition-dependency-graph flat-module)
                (sal-flat-module/make-transition-explicit-dependency-graph flat-module)
                (sal-flat-module/make-transition-next-curr-dependency-graph flat-module)
                (dependency-graph/collapse-curr-next-nodes! graph flat-module)
                (dependency-graph/delete-defined-variables! graph flat-module)
                (dependency-graph-node/var-decl node)
                (dependency-graph-node/current? node)
                (dependency-graph-node/next? node)
                (dependency-graph/dump graph))
;;;        (eval (export-all)))
)

(define-record-type node-info
  (make-node-info current? var-decl next-curr-node)
  node-info?
  (current? node-info/current?)
  (var-decl node-info/var-decl)
  (next-curr-node node-info/next-curr-node node-info/set-next-curr-node!))

(define (dependency-graph-node/var-decl node)
  (node-info/var-decl (graph-node/data node)))

(define (dependency-graph-node/current? node)
  (node-info/current? (graph-node/data node)))

(define (dependency-graph-node/next? node)
  (not (dependency-graph-node/current? node)))

(define (make-dependency-graph flat-module)
  (let ((state-vars (sal-module/state-variables flat-module))
        (graph (make-graph)))
    (for-each (lambda (state-var-decl)
                (unless (instance-of? state-var-decl <sal-choice-input-state-var-decl>)
                  (let* ((curr-info (make-node-info #t state-var-decl #unspecified))
                         (next-info (make-node-info #f state-var-decl #unspecified))
                         (curr-node (graph/make-node! graph curr-info))
                         (next-node (graph/make-node! graph next-info)))
                    (node-info/set-next-curr-node! curr-info next-node)
                    (node-info/set-next-curr-node! next-info curr-node)
                    (set-slot-value! state-var-decl :curr-node curr-node)
                    (set-slot-value! state-var-decl :next-node next-node))))
              state-vars)
    graph))
                       
(define (dependency-graph/display graph)
  (graph/display graph :node-labeler (lambda (node)
                                       (let* ((info (graph-node/data node))
                                              (label (to-string (sal-decl/name (node-info/var-decl info)))))
                                         (if (node-info/current? info)
                                           label
                                           (string-append label "'"))))))

(define *set-of-support-cache* #unspecified)
(define *implicit-dependencies-chache* #unspecified)

(define (reset-dep-caches!)
  (set! *set-of-support-cache* (make-sal-ast-table))
  (set! *implicit-dependencies-chache* (make-sal-ast-table)))

;; set of state variables used by an abstract syntax tree 
(define-generic (set-of-support-core ast))
(define-method (set-of-support-core (ast <sal-ast>))
  (sal-ast/fold-children (lambda (curr-support child)
                           (let ((child-support (set-of-support-core child)))
                             (if curr-support
                               (if child-support
                                 (wt-tree/union curr-support child-support)
                                 curr-support)
                               child-support)))
                         #f
                         ast))
(define-method (set-of-support-core (ast <sal-type>))
  #f)
(define-method (set-of-support-core (ast <sal-module>))
  #f)
(define (relevant-state-var? ast)
  (and (sal-name-ref/state? ast)
       (not (instance-of? (slot-value ast :decl) <sal-choice-input-state-var-decl>))))
(define-method (set-of-support-core (ast <sal-name-expr>))
  (cond
   ((relevant-state-var? ast)
    (wt-tree/add (make-graph-node-tree) (slot-value (slot-value ast :decl) :curr-node) #unspecified))
   ((sal-name-ref/let-decl? ast)
    ;; I don't need to visit the same let-del several times
    (cond 
     ((sal-ast-table/get *set-of-support-cache* ast) =>
      cdr)
     (else
      (let ((result (set-of-support-core (sal-name-expr/definition ast))))
        (sal-ast-table/put! *set-of-support-cache* ast result)
        result))))
   (else
    #f)))
(define-method (set-of-support-core (ast <sal-next-operator>))
  (let ((name-expr (slot-value ast :name-expr)))
    (if (relevant-state-var? name-expr)
      (wt-tree/add (make-graph-node-tree) (slot-value (slot-value name-expr :decl) :next-node) #unspecified)
      #f)))


;; implicit dependencies are mainly introduced by guarded-commands 
;; that is
;; a = 1 --> x' = x + y
;; "x'" depends on "x" and "y", and implicitly on "a"
(define-generic (implicit-dependencies-core ast))
(define-method (implicit-dependencies-core (ast <sal-ast>))
  (set-of-support-core ast))
(define (implicit-dependencies-core-from-list ast-list)
  (fold-left (lambda (curr-dependencies ast)
               (let ((ast-dependencies (implicit-dependencies-core ast)))
                 (if curr-dependencies
                   (if ast-dependencies
                     (wt-tree/union curr-dependencies ast-dependencies)
                     curr-dependencies)
                   ast-dependencies)))
             #f
             ast-list))
(define-method (implicit-dependencies-core (ast <sal-and>))
  (implicit-dependencies-core-from-list (sal-application/argument-list ast)))
(define-method (implicit-dependencies-core (ast <sal-or>))
  (implicit-dependencies-core-from-list (sal-application/argument-list ast)))
;; ****** IMPORTANT ******
;; (define-method (implicit-dependencies-core (ast <sal-choice>))
;; In a previous version, I assumed that a choice does not propagate dependencies, since there is always an alternative...
;; But this is not true, all alternatives of a choice can be false, if the module is in a "local" deadlock.
;; For instance, in the peterson's mutual exclusion example a "local" deadlock happens when a process is trying
;; to enter the critical section but it is not its turn. Remark: there isn't a global deadlock in this example.
;;  #f)
(define-method (implicit-dependencies-core (ast <sal-multi-choice-expr>))
  ;; A choice does not propagate dependencies, since there is always an alternative
  #f)
(define-method (implicit-dependencies-core (ast <sal-quantified-expr>))
  (implicit-dependencies-core (slot-value ast :expr)))
(define-method (implicit-dependencies-core (ast <sal-let-expr>))
  (implicit-dependencies-core (slot-value ast :expr)))
(define-method (implicit-dependencies-core (ast <sal-assignment>))
  #f)
(define-method (implicit-dependencies-core (ast <sal-definition-expression>))
  #f)
(define-method (implicit-dependencies-core (ast <sal-name-expr>))
  (cond
   ((sal-name-ref/let-decl? ast)
    ;; I don't need to visit the same let-del several times
    (cond 
     ((sal-ast-table/get *implicit-dependencies-chache* ast) =>
      cdr)
     (else
      (let ((result (implicit-dependencies-core (sal-name-expr/definition ast))))
        (sal-ast-table/put! *implicit-dependencies-chache* ast result)
        result))))
   (else
    (set-of-support-core ast))))

;; a wrapper for implicit-dependencies-core
;; it returns a set of entries. An entry is a current or next variable name
(define (implicit-dependencies ast)
  (let ((result (implicit-dependencies-core ast)))
    (or result
        (make-graph-node-tree))))

;; a wrapper for set-of-support-core
(define (set-of-support ast)
  (let ((result (set-of-support-core ast)))
    (or result
        (make-graph-node-tree))))

(define-class <dependency-info> () (:graph       ;; reference to the dependency graph
                                    :edge-table  ;; reference to a hashtable of the edges in :graph
                                    :context     ;; implicit dependencies (tree of nodes)
                                    :next-vars   ;; hash-table of state-var-decls used in next-operators
                                    :region      ;; region (initialization, definition, transition) been processed
                                    :collect-implicit? ;; collect implicit dependencies
                                    :ignore-next-dependencies?  ;; ignore dependencies to next variables

                                    ))    

(define (make-next-variable-table ast)
  (let ((result (make-eq-hash-table)))
    (sal-ast/for-each (lambda (curr)
                        (when (instance-of? curr <sal-next-operator>)
                          (eq-hash-table/put! result (slot-value (slot-value curr :name-expr) :decl) #unspecified)))
                      ast)
    result))

(define (sal-flat-module/make-dependency-graph-core flat-module process-initialization? collect-implicit? ignore-next-dependencies?)
  (verbose-message 3 "    collecting state variables dependencies...")
  (reset-dep-caches!)
  (let* ((graph (make-dependency-graph flat-module))
         (edge-table (make-graph-edge-table))
         (dep-info (make-instance <dependency-info> 
                                  :graph graph
                                  :edge-table edge-table
                                  :next-vars (make-next-variable-table (slot-value flat-module :transition))
                                  :context (make-graph-node-tree)
                                  :collect-implicit? collect-implicit?
                                  :ignore-next-dependencies? ignore-next-dependencies?)))
    (when process-initialization?
      (set-slot-value! dep-info :region 'initialization)
      (collect-dependencies (slot-value flat-module :initialization) dep-info))
    (set-slot-value! dep-info :region 'definition)
    (collect-dependencies (slot-value flat-module :definition) dep-info)
    (set-slot-value! dep-info :region 'transition)
    (collect-dependencies (slot-value flat-module :transition) dep-info)
    (reset-dep-caches!)
    (values graph edge-table)))

(define (sal-flat-module/make-dependency-graph flat-module)
  (sal-flat-module/make-dependency-graph-core flat-module #t #t #f))

(define (sal-flat-module/make-transition-dependency-graph flat-module)
  (sal-flat-module/make-dependency-graph-core flat-module #f #t #f))

(define (sal-flat-module/make-transition-explicit-dependency-graph flat-module)
  (sal-flat-module/make-dependency-graph-core flat-module #f #f #f))

(define (sal-flat-module/make-transition-next-curr-dependency-graph flat-module)
  (sal-flat-module/make-dependency-graph-core flat-module #f #t #t))

(define (dependency-graph/collapse-curr-next-nodes! graph flat-module)
  (let ((state-vars (slot-value flat-module :state-vars)))
    (for-each (lambda (state-var-decl)
                (let ((curr-node (slot-value state-var-decl :curr-node))
                      (next-node (slot-value state-var-decl :next-node)))
                  (when (and curr-node next-node)
                    (graph/collapse-nodes! graph curr-node next-node)
                    (set-slot-value! state-var-decl :next-node #f))))
              state-vars)
    (graph/delete-parallel-edges! graph)))

(define (dependency-graph/delete-defined-variables! graph flat-module)
  (let* ((defined-variables (sal-module/defined-variables flat-module))
         (edges-table (make-graph-edge-table-for graph))
         (reduce! (lambda (node)
                    (graph-node/for-each-in-edge
                     (lambda (in-edge)
                       (graph-node/for-each-out-edge
                        (lambda (out-edge)
                          (let ((source (graph-edge/source in-edge))
                                (target (graph-edge/target out-edge)))
                            (unless (graph-edge-table/contains? edges-table source target)
                              (let ((new-edge (graph/make-edge! graph source target)))
                                (graph-edge-table/insert! edges-table new-edge)))))
                        node))
                     node)
                    (graph-node/for-each-in-out-edge (lambda (edge)
                                                       (graph-edge-table/delete! edges-table edge))
                                                     node)
                    (graph/delete-node! graph node))))
    (eq-hash-table/for-each (lambda (decl _)
                              (let* ((curr-node (slot-value decl :curr-node))
                                     (next-node (slot-value decl :next-node)))
                                (when curr-node
                                  (reduce! curr-node)
                                  (set-slot-value! decl :curr-node #f))
                                (when next-node
                                  (reduce! next-node)
                                  (set-slot-value! decl :next-node #f))))
                            defined-variables)))

(define (dependency-graph/dump graph)
  (print "Dependency Graph")
  (graph/for-each-edge (lambda (edge)
                         (let* ((source (graph-edge/source edge))
                                (target (graph-edge/target edge))
                                (source-info (graph-node/data source))
                                (target-info (graph-node/data target))
                                (source-name (sal-decl/name (node-info/var-decl source-info)))
                                (target-name (sal-decl/name (node-info/var-decl target-info))))
                           (print source-name (if (node-info/current? source-info) "" "'")
                                  " |-> "
                                  target-name (if (node-info/current? target-info) "" "'"))))
                       graph)
  (print "--------------------------------------"))
                             
(define-generic (collect-dependencies ast dep-info))

(define-method (collect-dependencies (ast <sal-ast>) (dep-info <dependency-info>))
  #unspecified)

(define-method (collect-dependencies (ast <sal-let-expr>) (dep-info <dependency-info>))
  (collect-dependencies (slot-value ast :expr) dep-info))

(define (collect-dependencies-from-list ast-list dep)
  (for-each (cut collect-dependencies <> dep) ast-list))

(define-method (collect-dependencies (ast <sal-and>) (dep-info <dependency-info>))
  ;; Save context info. Remark: I don't need any kind of proctection (e.g. unwind-protect), since
  ;; dep-info is a temporary object.
  (let* ((context (slot-value dep-info :context))
         (new-context (if (slot-value dep-info :collect-implicit?)
                        (wt-tree/union context (implicit-dependencies ast))
                        context)))
    (set-slot-value! dep-info :context new-context)
    (collect-dependencies-from-list (sal-application/argument-list ast) dep-info)
    (set-slot-value! dep-info :context context)))

(define-method (collect-dependencies (ast <sal-or>) (dep-info <dependency-info>))
  (collect-dependencies-from-list (sal-application/argument-list ast) dep-info))

(define-method (collect-dependencies (ast <sal-quantified-expr>) (dep-info <dependency-info>))
  (collect-dependencies (slot-value ast :expr) dep-info))

(define-method (collect-dependencies (ast <sal-name-expr>) (dep-info <dependency-info>))
  (when (sal-name-ref/let-decl? ast)
    ;; Remark: I need to visit the same let-decl several times, since the context may be different (i.e., (slot-value dep :context))
    (collect-dependencies (sal-name-expr/definition ast) dep-info)))

(define (register-dependencies! state-var-decl used-node-tree dep-info)
  (let* ((region (slot-value dep-info :region))
         (graph (slot-value dep-info :graph))
         (edge-table (slot-value dep-info :edge-table))
         (ignore-next-dependencies? (slot-value dep-info :ignore-next-dependencies?))
         (add-edge! (lambda (source-node target-node)
                      (when (or (not ignore-next-dependencies?)
                                (node-info/current? (graph-node/data target-node)))
                        (unless (graph-edge-table/contains? edge-table source-node target-node)
                          (let ((new-edge (graph/make-edge! graph source-node target-node)))
                            (graph-edge-table/insert! edge-table new-edge)))))))
    (case region
      ((initialization)
       (let ((source-node (slot-value state-var-decl :curr-node)))
         (wt-tree/for-each (lambda (target-node _)
                             [assert (target-node) (node-info/current? (graph-node/data target-node))]
                             (add-edge! source-node target-node))
                           used-node-tree)))
      ((transition)
       (let ((source-node (slot-value state-var-decl :next-node)))
         (wt-tree/for-each (lambda (target-node _)
                             (add-edge! source-node target-node))
                           used-node-tree)))
      ((definition)
       (let* ((source-node (slot-value state-var-decl :curr-node))
              (next-vars (slot-value dep-info :next-vars))
              (next-used? (eq-hash-table/contains? next-vars state-var-decl))
              (next-source-node (slot-value state-var-decl :next-node)))
         (wt-tree/for-each (lambda (target-node _)
                             [assert (target-node) (node-info/current? (graph-node/data target-node))]
                             (add-edge! source-node target-node)
                             (when next-used?
                               (let ((next-target-node (node-info/next-curr-node (graph-node/data target-node))))
                                 [assert (next-target-node) (not (node-info/current? (graph-node/data next-target-node)))]
                                 (add-edge! next-source-node next-target-node))))
                           used-node-tree)))
      (else
       ;; (breakpoint "dep register" (region dep-info state-var-decl used-node-tree) #t)
       (internal-error)))))

;; When the quantifiers are not expanded, and we convert to a boolean representation.
;; The LHS of an assignment of definition may not be a standard LHS, but a conditional.
;; Example:
;;   OUPUT arr : ARRAY [0..3] OF BOOLEAN
;;   ...
;;   FORALL(i:[0..3]): arr'[i] = ...
;;
;;   The assignment above is translated into:
;;
;;   FORALL (i1,i2:BOOLEAN):
;;     (IF NOT i1 OR NOT i2 THEN arr!0'
;;      ELSIF NOT i1 OR i2 THEN  arr!1'
;;      ELSIF i1 OR NOT i2 THEN  arr!2'
;;      ELSE  arr!3' ENDIF) = ...
;;  
;;
;; The following procedure collect all target variables in a LHS
(define (collect-lhs-targets ast)
  (let ((result (make-queue)))
    (collect-lhs-targets-core! ast result)
    (queue->list result)))

(define-generic (collect-lhs-targets-core! ast result-queue))
(define-method (collect-lhs-targets-core! (ast <sal-ast>) (result-queue <primitive>))
  ;; do nothing
  #unspecified) 
(define-method (collect-lhs-targets-core! (ast <sal-expr>) (result-queue <primitive>))
  (sal-ast/for-each-children (cut collect-lhs-targets-core! <> result-queue) ast))
(define-method (collect-lhs-targets-core! (ast <sal-name-expr>) (result-queue <primitive>))
  (let ((decl (slot-value (sal-lhs/name-expr ast) :decl)))
    (when (instance-of? decl <sal-state-var-decl>)
      (queue/insert! result-queue decl))))


(define-method (collect-dependencies (ast <sal-assignment>) (dep-info <dependency-info>))
  (multiple-value-bind
      (lhs rhs)
      (sal-binary-application/arguments ast)
    (let ((lhs-var-decl-list (collect-lhs-targets lhs))
          (explicit-dep (set-of-support rhs))
          (implicit-dep (slot-value dep-info :context)))
      (for-each (lambda (lhs-var-decl)
                  [assert (lhs-var-decl) (instance-of? lhs-var-decl <sal-state-var-decl>)]
                  (register-dependencies! lhs-var-decl explicit-dep dep-info)
                  (register-dependencies! lhs-var-decl implicit-dep dep-info))
                lhs-var-decl-list))))

(define-method (collect-dependencies (ast <sal-definition-expression>) (dep-info <dependency-info>))
  (let ((lhs-list (slot-value ast :lhs-list))
        (explicit-dep (set-of-support (slot-value ast :expr)))
        (implicit-dep (slot-value dep-info :context)))
    (for-each (lambda (lhs)
                (let ((lhs-var-decl-list (collect-lhs-targets lhs)))
                  (for-each (lambda (lhs-var-decl)
                              [assert (lhs-var-decl) (instance-of? lhs-var-decl <sal-state-var-decl>)]
                              (register-dependencies! lhs-var-decl explicit-dep dep-info)
                              (register-dependencies! lhs-var-decl implicit-dep dep-info))
                            lhs-var-decl-list)))
              lhs-list)))
