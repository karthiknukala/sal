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

(module sal-esmc
        (include "sal.sch")
        (import sal-esm-engine sal-api sal-esm sal-assertion 
                sal-esm-options-support sal2scm-core sal-ast-env sal-ast-for-each
                fast-hash-table sal-module sal-esm-support runtime
                sal-esmc-bfs sal-esmc-dfs sal-esm-options sal-esm-random-simulation
                sal-esm-guided-simulation sal-esm-reflexivity sal-esmc-guided sal-decls
                sal-esmc-ndfs)
        (export (sal-esmc/verify assertion))
        )

(define-generic (sal-esmc/verify assertion))

(define-method (sal-esmc/verify (assertion <primitive>))
  [assert (assertion) (string? assertion)]
  (sal-esmc/verify (sal/assertion-name assertion)))

(define-method (sal-esmc/verify (assertion <sal-qualified-assertion-name>))
  (sal-esmc/verify (sal->esm assertion)))
  
(define-method (sal-esmc/verify (assertion <sal-module-models>))
  (let* ((assertion1 (sal-module-models/convert-to-ltl assertion))
         (assertion2 (if (instance-of? (slot-value assertion1 :module) <sal-esm-module>) 
                       assertion1
                       (sal->esm assertion1)))
         (assertion (sal-module-models/ltl->ba assertion2))
         (module (sal-esm/apply-module-transformations (slot-value assertion :module)))
         (property (slot-value assertion :expr))
         (ctx (esm-options->verification-context))
         (esm-engine (make-sal-esm-engine module ctx (sal-ast/collect-used-state-var-decls property))))
    (sal-esmc/verify-core property esm-engine)))

;; generate a predicate to check the property on the current states
(define (gen-check-prop-curr-proc ctx prop-body)
  (let ((check-prop-code (sal->scm prop-body ctx (make-empty-env))))
    (sal-esm-engine-scm-context/add-definition! ctx `(lambda () ,check-prop-code) 'check)))
  
;; generate a predicate to check the property on the next states
(define (gen-check-prop-next-proc engine ctx prop-body)
  ;; check if prop-body doesn't reference defined variables nor input variables
  (bind-exit (exit)
    (let* ((esm-module (slot-value engine :esm-module))
           (defined-var-table (sal-module/defined-variables esm-module)))
      (sal-ast/for-each (lambda (ast)
                          (when (and (instance-of? ast <sal-name-expr>)
                                     (or (instance-of? (slot-value ast :decl) <sal-input-state-var-decl>)
                                         (and (instance-of? (slot-value ast :decl) <sal-state-var-decl>)
                                              (eq-hash-table/get defined-var-table (slot-value ast :decl)))))
                            (if (null? (slot-value engine :input-vars))
                              (exit '(quote use-definitions))
                              (exit '(quote use-inputs)))))
                        prop-body)
      (let ((old-curr-var-idx-table (slot-value ctx :curr-var-idx-table)))
        (unwind-protect
         (begin
           ;; small hack to obtain a predicate that test the property on the successor states
           (set-slot-value! ctx :curr-var-idx-table (slot-value ctx :next-var-idx-table))
           (let ((check-prop-code (sal->scm prop-body ctx (make-empty-env))))
             (sal-esm-engine-scm-context/add-definition! ctx `(lambda () ,check-prop-code) 'check)))
         (set-slot-value! ctx :curr-var-idx-table old-curr-var-idx-table))))))

(define-generic (sal-esmc/verify-core property engine))
    
(define-method (sal-esmc/verify-core (property <sal-ltl-g>) (engine <sal-esm-engine>))
  (when (and (memq *esmc-traversal-strategy* '(guided guided-cacheless))
             (eq? *esm-state-weight-function* #f))
    (sign-error "Weight function was not defined. Use option -w"))
  (let* ((ctx (slot-value engine :sal-scm-context))
         (prop-body (slot-value property :arg))
         (module (slot-value engine :esm-module))
         (check-prop-curr-proc (gen-check-prop-curr-proc ctx prop-body))
         (check-prop-next-proc (gen-check-prop-next-proc engine ctx prop-body))
         (curr-weight-proc (if (memq *esmc-traversal-strategy* '(guided guided-cacheless))
                             (sal-esm/convert-user-function module *esm-state-weight-function* ctx #f)
                             #unspecified))
         (next-weight-proc (if (memq *esmc-traversal-strategy* '(guided guided-cacheless))
                             (sal-esm/convert-user-function module *esm-state-weight-function* ctx #t)
                             #unspecified)))
    (sal-esm-engine-scm-context/compile-code! ctx)
    (let* ((check-prop-curr-proc (eval check-prop-curr-proc))
           (check-prop-next-proc (eval check-prop-next-proc))
           (checking-mode (cond 
                           ((procedure? check-prop-next-proc) 'optimized)
                           ((eq? check-prop-next-proc 'use-inputs) 'all-input-combinations)
                           (else 'one-input-combination)))
           (check-prop-next-proc (if (eq? checking-mode 'optimized) check-prop-next-proc #unspecified)))
      (display-runtime 2 "  verification time: ~a secs"
        (lambda ()
          (case *esmc-traversal-strategy*
            ((bfs)
             (sal-esmc/bfs-check-invariant engine checking-mode check-prop-curr-proc check-prop-next-proc))
            ((idfs)
             (sal-esmc/idfs-check-invariant engine checking-mode check-prop-curr-proc check-prop-next-proc))
            ((cacheless)
             (sal-esmc/cacheless-check-invariant engine check-prop-curr-proc))
            ((guided-cacheless)
             (sal-esmc/guided-cacheless-check-invariant engine checking-mode check-prop-curr-proc 
                                                        (eval curr-weight-proc) (eval next-weight-proc)))
            ((guided)
             (sal-esmc/guided-check-invariant engine checking-mode check-prop-curr-proc check-prop-next-proc 
                                              (eval curr-weight-proc) (eval next-weight-proc)))
            (else
             (sal-esmc/dfs-check-invariant engine checking-mode check-prop-curr-proc check-prop-next-proc))))
        :esm-verification-time))))

(define-method (sal-esmc/verify-core (property <sal-accepting>) (engine <sal-esm-engine>))
  (sign-error "Not Implemented.")
  (let* ((ctx (slot-value engine :sal-scm-context))
         (accepting-condition (slot-value property :arg))
         (module (slot-value engine :esm-module))
         (accepting-proc (gen-check-prop-curr-proc ctx accepting-condition)))
    (sal-esm-engine-scm-context/compile-code! ctx)
    (let* ((accepting-proc (eval accepting-proc)))
      (display-runtime 2 "  verification time: ~a secs"
        (lambda ()
          (case *esmc-traversal-strategy*
            ((idfs)
             (sal-esmc/nidfs-check-liveness engine accepting-proc))
            (else
             (unless (eq? *esmc-traversal-strategy* 'dfs)
               (warning-message "The state traversal strategy `~a' is not available for checking liveness properties. Using nested depth-first search."))
             (sal-esmc/ndfs-check-liveness engine accepting-proc))))
        :esm-verification-time))))

  
