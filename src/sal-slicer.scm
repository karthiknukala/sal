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

(module sal-slicer
        (include "sal.sch")
        (include "fast-hash-table.sch")
        (import sal-ast-for-each sal-environment sal-type sal-expression sal-ast-env queue 
                sal-ast-copy sal-decls sal-ast-simplify sal-assertion sal-module runtime sal-pp graph 
                sal-dependencies sal-component-info)
        (export (sal-ast/slice ast info env)
                (sal-module-models/slice ast)
                (sal-module/slice-for flat-module expr)
                (sal-sliced-flat-module->flat-module module))
        )

(define-generic (sal-ast/slice ast relevant-vars env))

(define (non-sliced-lhs? lhs relevant-vars)
  (let ((lhs-var-decl (slot-value (sal-lhs/name-expr lhs) :decl)))
    (eq-hash-table/contains? relevant-vars lhs-var-decl)))

(define-method (sal-ast/slice (ast <sal-definition-expression>) (relevant-vars <primitive>) (env <primitive>))
  (let* ((lhs-list (slot-value ast :lhs-list)))
    (cond
     ((exists (cut non-sliced-lhs? <> relevant-vars) lhs-list)
      [assert (lhs-list) (for-all (cut non-sliced-lhs? <> relevant-vars) lhs-list)]
      (sal-ast/substitute ast env))
     (else
      (make-sal-true ast)))))

(define-method (sal-ast/slice (ast <sal-assignment>) (relevant-vars <primitive>) (env <primitive>))
  (let ((lhs (sal-binary-application/arg1 ast)))
    (if (non-sliced-lhs? lhs relevant-vars)
      (sal-ast/substitute ast env)
      (make-sal-true ast))))

(define-method (sal-ast/slice (ast <sal-ast>) (relevant-vars <primitive>) (env <primitive>))
  ;; the slicer only works if the following invariant is true...
  ;; sal-ast/flat-modules guarantee this invariant... if this is not true... I should check that function
  [assert (ast) (not (sal-ast/find (lambda (n) (instance-of? n <sal-assignment>)) ast))] 
  (sal-ast/substitute ast env))

(define-method (sal-ast/slice (ast <sal-quantified-expr>) (relevant-vars <primitive>) (env <primitive>))
  (update-ast-slots ast :expr (sal-ast/slice (slot-value ast :expr) relevant-vars env)))

(define-method (sal-ast/slice (ast <sal-and>) (relevant-vars <primitive>) (env <primitive>))
  (let* ((args (sal-application/argument-list ast))
         (new-args (conservative-map-1 (cut sal-ast/slice <> relevant-vars env) args)))
    (if (eq? args new-args)
      ast
      (apply make-simplified-sal-builtin-application <sal-and> ast
             new-args))))

(define-method (sal-ast/slice (ast <sal-choice>) (relevant-vars <primitive>) (env <primitive>))
  (let* ((args (sal-application/argument-list ast))
         (new-args (conservative-map-1 (cut sal-ast/slice <> relevant-vars env) args)))
    (if (eq? args new-args)
      ast
      (copy-ast ast 
                :arg (apply make-application-argument new-args)))))

(define-method (sal-ast/slice (ast <sal-let-decl>) (relevant-vars <primitive>) (env <primitive>))
  (update-ast-slots ast :value (sal-ast/slice (slot-value ast :value) relevant-vars env)))

(define-method (sal-ast/slice (ast <sal-let-expr>) (relevant-vars <primitive>) (env <primitive>))
  (let* ((local-decls (slot-value ast :local-decls))
         (new-local-decls (conservative-map-1 (cut sal-ast/slice <> relevant-vars env) local-decls))
         (env (update-env* env local-decls new-local-decls))
         (new-expr (sal-ast/slice (slot-value ast :expr) relevant-vars env))
         (new-ast (update-ast-slots ast
                                    :local-decls new-local-decls
                                    :expr new-expr)))
    (sal-let-expr/local-simplify new-ast)))
    
(define (sal-flat-module/used-variables flat-module)
  (let* ((used-vars (make-eq-hash-table))
         (collect-used-vars! (lambda (ast)
                               (sal-ast/for-each (lambda (ast)
                                                   (when (and (instance-of? ast <sal-name-expr>)
                                                              (sal-name-ref/state? ast))
                                                     (eq-hash-table/put! used-vars (slot-value ast :decl) #unspecified)))
                                                 ast))))
    (collect-used-vars! (slot-value flat-module :initialization))
    (collect-used-vars! (slot-value flat-module :transition))
    (collect-used-vars! (slot-value flat-module :definition))
    (collect-used-vars! (slot-value flat-module :skip))
    used-vars))

(define (filter-non-used-state-vars state-vars used-vars)
  (let ((new-state-var-queue (make-queue)))
    (for-each (lambda (state-var-decl)
                (when (or (instance-of? state-var-decl <sal-choice-input-state-var-decl>)
                          (eq-hash-table/contains? used-vars state-var-decl))
                  (queue/insert! new-state-var-queue state-var-decl)))
              state-vars)
    (queue->list new-state-var-queue)))

(define (filter-non-used-data component-info used-vars)
  (sal-component-info/convert-data component-info
                                   (lambda (lhs)
                                     (let ((lhs-var-name (sal-lhs/name-expr lhs)))
                                       (if (eq-hash-table/contains? used-vars (slot-value lhs-var-name :decl))
                                         (list lhs)
                                         '()))))) ;; remove the datum

(define (sal-flat-module/remove-non-used-vars! flat-module)
  (let* ((used-vars (sal-flat-module/used-variables flat-module))
         (new-state-vars (filter-non-used-state-vars (slot-value flat-module :state-vars) used-vars))
         (new-component-info (filter-non-used-data (slot-value flat-module :component-info) used-vars)))
    (set-slot-value! flat-module :state-vars new-state-vars)
    (set-slot-value! flat-module :component-info new-component-info)
    (let ((valid-input-expr (remove-non-used-var-from-valid-expr (slot-value flat-module :valid-input-expr) used-vars))
          (valid-state-expr (remove-non-used-var-from-valid-expr (slot-value flat-module :valid-state-expr) used-vars)))
      ;; Remark: I don't need to rewrite :valid-constant-expr since it does not contain references to state variables
      (set-slot-value! flat-module :valid-input-expr valid-input-expr)
      (set-slot-value! flat-module :valid-state-expr valid-state-expr))))
  
(define (remove-non-used-var-from-valid-expr ast used-vars)
  (sal-ast/simplify (remove-non-used-var-from-valid-expr-core ast used-vars #t)))

(define-generic (remove-non-used-var-from-valid-expr-core ast used-vars polarity))
(define-method (remove-non-used-var-from-valid-expr-core (ast <sal-ast>) (used-vars <primitive>) (polarity <primitive>))
  ;; (breakpoint "remove" (ast) #t)
  (sign-unsupported-feature ast "Slicer failed to remove unused variables from valid-state expressions. Main cause: unexpected abstract syntax tree node. Please contact support."))
(define-method (remove-non-used-var-from-valid-expr-core :around (ast <sal-ast>) (used-vars <primitive>) (polarity <primitive>))
  (cond
   ((sal-ast/find (lambda (n)
                    (and (instance-of? n <sal-name-expr>)
                         (sal-name-ref/state? n)
                         (eq-hash-table/contains? used-vars (slot-value n :decl))))
                  ast)
    ;; ast contains used state variables... then continue to apply the transformation
    (call-next-method))
   (polarity
    (make-sal-true ast))
   (else
    (make-sal-false ast))))
(define-method (remove-non-used-var-from-valid-expr-core (ast <sal-arg-tuple-literal>) (used-vars <primitive>) (polarity <primitive>))
  (update-ast-slots ast
                    :exprs (conservative-map-1 (cut remove-non-used-var-from-valid-expr-core <> used-vars polarity) (slot-value ast :exprs))))
(define (remove-non-used-var-from-valid-expr-core/app app used-vars polarity)
  (update-ast-slots app
                    :arg (remove-non-used-var-from-valid-expr-core (slot-value app :arg) used-vars polarity)))
(define-method (remove-non-used-var-from-valid-expr-core (ast <sal-not>) (used-vars <primitive>) (polarity <primitive>))
  (remove-non-used-var-from-valid-expr-core/app ast used-vars (not polarity)))
(define-method (remove-non-used-var-from-valid-expr-core (ast <sal-and>) (used-vars <primitive>) (polarity <primitive>))
  (remove-non-used-var-from-valid-expr-core/app ast used-vars polarity))
(define-method (remove-non-used-var-from-valid-expr-core (ast <sal-or>) (used-vars <primitive>) (polarity <primitive>))
  (remove-non-used-var-from-valid-expr-core/app ast used-vars polarity))
(define-method (remove-non-used-var-from-valid-expr-core (ast <sal-quantified-expr>) (used-vars <primitive>) (polarity <primitive>))
  (update-ast-slots ast
                    :expr (remove-non-used-var-from-valid-expr-core (slot-value ast :expr) used-vars polarity)))
(define-method (remove-non-used-var-from-valid-expr-core (ast <sal-let-expr>) (used-vars <primitive>) (polarity <primitive>))
  ;; I don't believe there are let-expression in :input-valid-expr and :state-valid-expr
  ;; I just want to be safe...
  ;; Remark: The references to let-decls will be expanded...
  (remove-non-used-var-from-valid-expr-core (slot-value ast :expr) used-vars polarity))
(define-method (remove-non-used-var-from-valid-expr-core (ast <sal-true>) (used-vars <primitive>) (polarity <primitive>))
  ast)
(define-method (remove-non-used-var-from-valid-expr-core (ast <sal-false>) (used-vars <primitive>) (polarity <primitive>))
  ast)
(define-method (remove-non-used-var-from-valid-expr-core (ast <sal-name-expr>) (used-vars <primitive>) (polarity <primitive>))
  (cond
   ((sal-name-ref/let-decl? ast)
    (remove-non-used-var-from-valid-expr-core (sal-name-expr/definition ast) used-vars polarity))
   ((sal-name-ref/state? ast)
    ast)
   (else
    (call-next-method))))

(define (sal-module/slice-core flat-module relevant-vars env new-class)
  (let ((new-flat-module (change-class flat-module new-class)))
    (set-slot-value! new-flat-module :original-module flat-module)
    (verbose-message 2 "  removing irrelevant parts of the specification...")
    (set-slot-value! new-flat-module :initialization (sal-ast/slice (slot-value flat-module :initialization) relevant-vars env))
    (set-slot-value! new-flat-module :definition (sal-ast/slice (slot-value flat-module :definition) relevant-vars env))
    (set-slot-value! new-flat-module :transition (sal-ast/slice (slot-value flat-module :transition) relevant-vars env))
    (set-slot-value! new-flat-module :skip (sal-ast/slice (slot-value flat-module :skip) relevant-vars env))
    new-flat-module))

(define-method (sal-ast/slice (flat-module <sal-flat-module>) (relevant-vars <primitive>) (env <primitive>))
  (sal-module/slice-core flat-module relevant-vars env <sal-sliced-flat-module>))

(define-method (sal-ast/slice (flat-module <sal-boolean-flat-module>) (relevant-vars <primitive>) (env <primitive>))
  (sal-module/slice-core flat-module relevant-vars env <sal-sliced-boolean-flat-module>))

(define-method (sal-ast/slice (flat-module <sal-simple-data-flat-module>) (relevant-vars <primitive>) (env <primitive>))
  (sal-module/slice-core flat-module relevant-vars env <sal-sliced-simple-data-flat-module>))

(define-api (sal-module/slice-for flat-module expr)
  (status-message :slicing-module)
  (verbose-message 1 "slicing the module...")
  (display-runtime 2 "  slicer time: ~a secs"
    (lambda ()
      (unless (instance-of? flat-module <sal-flat-module>)
        (sign-unsupported-feature flat-module "Only flat modules can be sliced. Please use the flattener before calling this function."))
      (let* ((dep-graph (sal-flat-module/make-dependency-graph flat-module))
             (reached (make-node-mapping dep-graph #f))
             (relevant-variables (make-eq-hash-table)))
        (dependency-graph/collapse-curr-next-nodes! dep-graph flat-module)
        (sal-ast/for-each (lambda (ast)
                            (when (and (instance-of? ast <sal-name-expr>)
                                       (sal-name-ref/state? ast))
                              [assert (ast) (slot-value (slot-value ast :decl) :curr-node)]
                              (graph/dfs! dep-graph (slot-value (slot-value ast :decl) :curr-node) reached)))
                          expr)
        (graph/for-each-node (lambda (node)
                               (when (node-mapping/ref reached node)
                                 (let ((state-var-decl (dependency-graph-node/var-decl node)))
                                   (eq-hash-table/put! relevant-variables state-var-decl #unspecified))))
                             dep-graph)
        (let ((new-flat-module (sal-ast/slice flat-module relevant-variables (make-empty-env))))
          (sal-flat-module/remove-non-used-vars! new-flat-module)
          (let* ((new-expr (sal-module/rebind-expr flat-module expr new-flat-module))
                 (num-vars-before (length (slot-value flat-module :state-vars)))
                 (num-vars-after (length (slot-value new-flat-module :state-vars)))
                 (num-removed-variables (- num-vars-before num-vars-after)))
            (status-message :slicer-result num-vars-before num-vars-after)
            (verbose-message 2 "  original number of variables: ~a" num-vars-before)
            (verbose-message 2 "  number of removed variables: ~a" num-removed-variables)
            (verbose-message 2 "  removed variables: ~a" (map sal-decl/name (difference (slot-value flat-module :state-vars) (slot-value new-flat-module :state-vars))))
            (values new-flat-module new-expr)))))
    :slicing-time))

(define-api (sal-module-models/slice (ast <sal-module-models>))
  :doc "Removes state variables that are irrelevant to prove the property in the given assertion. See @code{<sal-module-models>}."
  (multiple-value-bind
      (new-module new-expr)
      (sal-module/slice-for (slot-value ast :module) (slot-value ast :expr))
    (update-ast-slots ast
                      :module new-module
                      :expr new-expr)))

(define-generic (sal-sliced-flat-module->flat-module module))

(define-method (sal-sliced-flat-module->flat-module (module <sal-flat-module>))
  module)

(define-method (sal-sliced-flat-module->flat-module (module <sal-sliced-flat-module>))
  (sal-sliced-flat-module->flat-module (slot-value module :original-module)))
