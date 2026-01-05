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

(module simple-abstraction
        (include "sal.sch")
        (include "fast-hash-table.sch")
        (import sal-ast-copy sal-ast-env sal-expression runtime)
        (export (sal-ast/simple-abstraction ast vars-to-abstract)
                (sal-flat-module/simple-abstraction flat-module vars-to-abstract))
        )

(define-generic (sal-ast/simple-abstraction ast vars-to-abstract))

(define *empty-env* (make-empty-env))

(define-method (sal-ast/simple-abstraction (ast <sal-ast>) (vars-to-abstract <primitive>))
  (sal-ast/map ast *empty-env* 
               (lambda (child-ast new-env) 
                 [assert (new-env) (eq? new-env *empty-env*)]
                 (sal-ast/simple-abstraction child-ast vars-to-abstract))))

(define-method (sal-ast/simple-abstraction (ast <sal-flat-module>) (vars-to-abstract <primitive>))
  (status-message :applying-simple-abstraction)
  (verbose-message 1 "abstracting flat module...")
  (display-runtime 2 "  abstraction time: ~a secs"
    (lambda ()
      (sal-flat-module/simple-abstraction ast vars-to-abstract))
    :applying-simple-abstraction))

(define-method (sal-ast/simple-abstraction (ast <sal-module-models>) (vars-to-abstract <primitive>))
  (let* ((module (slot-value ast :module))
         (state-vars (slot-value module :state-vars)))
    (unless (instance-of? module <sal-flat-module>)
      (sign-error "You have to flatten the modules in an assertion, before using the abstractor."))
    (multiple-value-bind
        (new-module vars-to-abstract-table)
        (sal-ast/simple-abstraction module vars-to-abstract)
      (let* ((new-state-vars (slot-value new-module :state-vars))
             (env (update-env* (make-empty-env) state-vars new-state-vars))
             (new-property (sal-expr/simple-abstraction (slot-value ast :expr) env vars-to-abstract-table)))
        (update-ast-slots ast
                          :module new-module
                          :expr new-property)))))

(define (sal-flat-module/simple-abstraction flat-module vars-to-abstract)
  (let ((vars-to-abstract-tmp-table (make-eq-hash-table))
        (vars-to-abstract-table (make-eq-hash-table)))
    (for-each (cut eq-hash-table/put! vars-to-abstract-tmp-table <> #unspecified) vars-to-abstract)
    (let* ((state-vars (slot-value flat-module :state-vars))
           (new-state-vars (map (lambda (var-decl)
                                  (cond
                                   ((eq-hash-table/contains? vars-to-abstract-tmp-table (sal-decl/name var-decl))
                                    (when (instance-of? var-decl <sal-input-state-var-decl>)
                                      (sign-source-error var-decl "Failed to abstract the variable `~a', variable is already an input." 
                                                         (sal-decl/name var-decl)))
                                    (verbose-message 5 "    abstracting variable: ~a" (sal-decl/name var-decl))
                                    (eq-hash-table/put! vars-to-abstract-table var-decl #unspecified)
                                    (change-ast-class var-decl <sal-input-state-var-decl>))
                                   (else
                                    var-decl)))
                                state-vars))
           (env (update-env* (make-empty-env) state-vars new-state-vars)))
      (verbose-message 2 "  number of abstracted variables: ~a" (eq-hash-table/size vars-to-abstract-table))
      (if (= (eq-hash-table/size vars-to-abstract-table) 0)
        (values flat-module vars-to-abstract-table)
        (values (copy-ast flat-module
                          
                          :state-vars new-state-vars
                          :definition (sal-expr/simple-abstraction (slot-value flat-module :definition) env vars-to-abstract-table)
                          :initialization (sal-expr/simple-abstraction (slot-value flat-module :initialization) env vars-to-abstract-table)
                          :transition (sal-expr/simple-abstraction (slot-value flat-module :transition) env vars-to-abstract-table)
                          :skip (sal-expr/simple-abstraction (slot-value flat-module :skip) env vars-to-abstract-table)
                          :component-info (sal-ast/substitute (slot-value flat-module :component-info) env)
                          :valid-input-expr (sal-expr/simple-abstraction (slot-value flat-module :valid-input-expr) env vars-to-abstract-table)
                          :valid-state-expr (sal-expr/simple-abstraction (slot-value flat-module :valid-state-expr) env vars-to-abstract-table)
                          ;; FIX ME: valid-constant-expr does not contain references to state variables
                          :valid-constant-expr (sal-expr/simple-abstraction (slot-value flat-module :valid-constant-expr) env vars-to-abstract-table))
                vars-to-abstract-table)))))
  
(define-generic (sal-expr/simple-abstraction ast env vars-to-abstract-table))

(define-method (sal-expr/simple-abstraction (ast <sal-ast>) (env <primitive>) (vars-to-abstract-table <primitive>))
  (sal-ast/map ast env (lambda (child-ast new-env) (sal-expr/simple-abstraction child-ast new-env vars-to-abstract-table))))

(define (abstract? lhs vars-to-abstract-table)
  (eq-hash-table/contains? vars-to-abstract-table (slot-value (sal-lhs/name-expr lhs) :decl)))

(define-method (sal-expr/simple-abstraction (ast <sal-assignment>) (env <primitive>) (vars-to-abstract-table <primitive>))
  (let ((lhs (sal-binary-application/arg1 ast)))
    (if (abstract? lhs vars-to-abstract-table)
      (make-sal-true ast)
      (call-next-method))))

(define-method (sal-expr/simple-abstraction (ast <sal-definition-expression>) (env <primitive>) (vars-to-abstract-table <primitive>))
  (let* ((lhs-list (slot-value ast :lhs-list)))
    (cond
     ((exists (lambda (lhs) (not (abstract? lhs vars-to-abstract-table))) lhs-list)
      (and-let* ((failed-lhs (find (cut abstract? <> vars-to-abstract-table) lhs-list)))
                (warning-message "Failed to abstract the variable `~a', because of nondeterministic assignment located at ~a."
                                 (sal-name-ref/name (sal-lhs/name-expr failed-lhs))
                                 (format-with-location failed-lhs "")))
      (call-next-method))
     (else
      (make-sal-true ast)))))


                
