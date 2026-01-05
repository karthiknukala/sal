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

(module sal-ast-copy
        (include "sal.sch")
        (include "fast-hash-table.sch")
        (import symbol-table sal-ast-env sal-module sal-ast-support)
        (export (sal-ast/map ast env proc)
                (sal-ast/map-list ast-lst env proc)
                (sal-name-ref/map ast env)
                (sal-base-module/map ast env proc)
                (sal-flat-module/map ast env proc)
                (sal-derived-flat-module/map flat-module env proc)
                (sal-ast/substitute ast env)
                (sal-ast/deep-copy ast)
                (sal-ast/deep-copy-core ast env))
        )

;; Return a new @code{<sal-ast>}, where each children
;; was mapped using @code{proc}. @code{proc} receives two
;; arguments: a child, and a new (possibly equal) environment.
;; @code{env} is a mapping from @code{<sal-decl>} to 
;; @code{<sal-ast>}. @code{env} is used to store substitutions,
;; and information to update the @code{:decl} slots of 
;; @{<sal-name-ref> instances.
(define-generic (sal-ast/map ast env proc)
  :doc "Return a new @code{<sal-ast>}, where each children
was mapped using @code{proc}. @code{proc} receives two
arguments: a child, and a new (possibly equal) environment.
@code{env} is a mapping from @code{<sal-decl>} to 
@code{<sal-ast>}. @code{env} is used to store substitutions,
and information to update the @code{:decl} slots of 
@code{<sal-name-ref>} instances.")

(define-macro (sal-ast/map-slot ast slot-id env proc)
  `(,proc (slot-value ,ast ,slot-id) ,env))

(define-macro (sal-ast/map-optional-slot ast slot-id env proc)
  `(let ((val (slot-value ,ast ,slot-id)))
     (and val
          (,proc val ,env))))

(define (sal-ast/map-list ast-lst env proc) 
  (conservative-map-1 (lambda (ast)
                        (proc ast env))
                      ast-lst))

(define-macro (sal-ast/map-list-slot ast slot-id env proc)
  `(sal-ast/map-list (slot-value ,ast ,slot-id) ,env ,proc))

(define (sal-name-ref/map name-ref env)
  (cond
   ((lookup-env (sal-name-ref/decl name-ref) env) =>
    (lambda (new-ref)
      (if (instance-of? new-ref <sal-decl>)
        (update-ast-slots name-ref :decl new-ref)
        new-ref)))
   (else
    name-ref)))

(define (sal-module/map-core module env proc proc-body)
  (let* ((state-vars (slot-value module :state-vars))
         (new-state-vars (sal-ast/map-list state-vars env proc))
         (new-env (update-env* env state-vars new-state-vars)))
    (proc-body new-state-vars new-env)))

(define (sal-base-module/map base-module env proc)
  (sal-module/map-core 
   base-module env proc
   (lambda (new-state-vars new-env)
     (update-ast-slots base-module
                       :state-vars new-state-vars
                       :definitions (sal-ast/map-list-slot base-module :definitions new-env proc)
                       :initialization-definitions (sal-ast/map-list-slot base-module :initialization-definitions new-env proc)
                       :transition-definitions (sal-ast/map-list-slot base-module :transition-definitions new-env proc)
                       :initialization-command-section (sal-ast/map-optional-slot base-module :initialization-command-section new-env proc)
                       :transition-command-section (sal-ast/map-optional-slot base-module :transition-command-section new-env proc)))))

(define (sal-flat-module/map flat-module env proc)
  (sal-module/map-core 
   flat-module env proc
   (lambda (new-state-vars new-env)
     (update-ast-slots flat-module
                       :state-vars new-state-vars
                       :definition (sal-ast/map-slot flat-module :definition new-env proc)
                       :initialization (sal-ast/map-slot flat-module :initialization new-env proc)
                       :transition (sal-ast/map-slot flat-module :transition new-env proc)
                       :skip (sal-ast/map-slot flat-module :skip new-env proc)
                       :component-info (sal-ast/map-slot flat-module :component-info new-env proc)
                       :valid-input-expr (sal-ast/map-slot flat-module :valid-input-expr new-env proc)
                       :valid-state-expr (sal-ast/map-slot flat-module :valid-state-expr new-env proc)
                       :valid-constant-expr (sal-ast/map-slot flat-module :valid-constant-expr new-env proc)
                       ))))

(define (sal-esm-module/map esm-module env proc)
  (sal-module/map-core 
   esm-module env proc
   (lambda (new-state-vars new-env)
     (update-ast-slots esm-module
                       :state-vars new-state-vars
                       :definition (and (slot-value esm-module :definition)
                                        (sal-ast/map-slot esm-module :definition new-env proc))
                       :initialization (and (slot-value esm-module :initialization)
                                            (sal-ast/map-slot esm-module :initialization new-env proc))
                       :transition (and (slot-value esm-module :transition)
                                        (sal-ast/map-slot esm-module :transition new-env proc))))))

(define (sal-derived-flat-module/map flat-module env proc)
  (let* ((old-state-vars (slot-value flat-module :state-vars))
         (result (sal-flat-module/map flat-module env proc))
         (new-state-vars (slot-value result :state-vars)))
    (if (eq? old-state-vars new-state-vars)
      result
      (let* ((var-trace-info (slot-value result :var-trace-info))
             (mapping (update-env* (make-empty-env) old-state-vars new-state-vars))
             (new-var-trace-info (make-eq-hash-table)))
        (eq-hash-table/for-each (lambda (original-decl old-var-decl-list)
                                  (eq-hash-table/put! new-var-trace-info
                                                      original-decl
                                                      (map (lambda (old-var-decl)
                                                             (cond ((lookup-env old-var-decl mapping) => identity)
                                                                   (else (internal-error))))
                                                           old-var-decl-list)))
                                var-trace-info)
        (copy-ast result
                  :var-trace-info new-var-trace-info)))))

(define-macro (map-body . slot-def-list)
  `(update-ast-slots ast
                     ,@(let loop ((slot-def-list slot-def-list))
                         (if (null? slot-def-list)
                           '()
                           (let ((slot-def (car slot-def-list))
                                 (rest (loop (cdr slot-def-list))))
                             (cond
                              ((keyword? slot-def)
                               (cons* slot-def `(sal-ast/map-slot ast ,slot-def env proc) rest))
                              ((optional-slot-def? slot-def)
                               (cons* (car slot-def) `(sal-ast/map-optional-slot ast ,(car slot-def) env proc) rest))
                              ((list-slot-def? slot-def)
                               (cons* (car slot-def) `(sal-ast/map-list-slot ast ,(car slot-def) env proc) rest))
                              (else
                               (error 'map-body "Invalid slot definition." #unspecified))))))))

(define-macro (map-method-core class . body)
  `(define-method (sal-ast/map (ast ,class) (env <primitive>) (proc <primitive>))
     ,@body))

(define-macro (map-method class . slot-def-list)
  `(map-method-core ,class (map-body ,@slot-def-list)))

(define-macro (map-local-binds-ast-core class decl-slot-id body-slot-id map-body-proc)
  `(map-method-core ,class
     (let* ((local-decls (slot-value ast ,decl-slot-id))
            (new-local-decls (sal-ast/map-list local-decls env proc))
            (env (update-env* env local-decls new-local-decls)))
       (update-ast-slots ast
                         ,decl-slot-id new-local-decls 
                         ,body-slot-id (,map-body-proc ast ,body-slot-id env proc)))))

(define-macro (map-local-binds-ast class decl-slot-id body-slot-id)
  `(map-local-binds-ast-core ,class ,decl-slot-id ,body-slot-id sal-ast/map-slot))
    
(map-method <sal-identifier>)
(map-method <sal-decl> :id)
(map-method <sal-typed-decl> :id :type)
(map-method <sal-const-decl> :id :type (:value optional))
(map-method-core <sal-context> (sign-error "It is not possible to map contexts."))
(map-method-core <sal-top-decl> (sign-error "It is not possible to map top level declaration \"~a\" at \"~a\"."
                                               (sal-decl/name ast)
                                               (sal-decl/name (sal-ast/context ast))))
(map-local-binds-ast <sal-parametric-module> :local-decls :module)
(map-method-core <sal-name-ref>
  (sal-name-ref/map ast env))
(map-method <sal-qualified-name-expr> (:actuals list))
(map-method <sal-qualified-type-name> (:actuals list))
(map-method <sal-qualified-module-name> (:actuals list))
(map-method <sal-qualified-assertion-name> (:actuals list))
(map-method <sal-numeral>)
(map-method <sal-string-expr>)
(map-method <sal-application> :fun :arg)
(map-local-binds-ast <sal-local-binds-expr> :local-decls :expr)
(map-method <sal-record-literal> (:entries list))
(map-method <sal-tuple-literal> (:exprs list))
(map-method <sal-record-entry> :id :expr)
(map-method <sal-simple-selection> :target :idx)
(map-method <sal-update-expr> :target :idx :new-value)
(map-method <sal-conditional> :cond-expr :then-expr :else-expr)
(map-method <sal-next-operator> :name-expr)
(map-method <sal-mod-init> :module)
(map-method <sal-mod-trans> :module)
(map-method <sal-pre-operator> :expr)
(map-method <sal-function-type> :domain :range)
(map-method <sal-tuple-type> (:types list))
(map-method <sal-record-type> (:fields list))
(map-method <sal-field> :id :type)
(map-method <sal-state-type> :module)
(map-method <sal-subtype> :expr)
(map-method <sal-bounded-subtype> :expr :lower :upper)
(map-method <sal-scalar-type> (:scalar-elements list))
(map-method <sal-data-type> (:constructors list))
(map-method <sal-definition-expression> (:lhs-list list) :expr)
(map-method <sal-simple-definition> :lhs :rhs)
(map-local-binds-ast-core <sal-for-all-definition> :local-decls :definitions sal-ast/map-list-slot)
(map-method <sal-command-section> (:commands list) (:else-command optional))
(map-method <sal-guarded-command> :guard (:assignments list))
(map-method <sal-labeled-command> :label :command)
(map-method <sal-else-command> (:assignments list))
(map-local-binds-ast <sal-multi-command> :local-decls :command)
(map-method <sal-module-composition> :module1 :module2)
(map-local-binds-ast <sal-multi-composition> :local-decls :module)
(map-method <sal-org-module> (:identifiers list) :module)
(map-local-binds-ast <sal-with-module> :new-state-vars :module)
(map-method <sal-renaming> (:renames list) :module)
(map-method <sal-rename> :from-name :to-expr)
(map-method <sal-module-instance> :module-name (:actuals list))
(map-method-core <sal-base-module>
  (sal-base-module/map ast env proc))
(map-method-core <sal-flat-module>
  (sal-flat-module/map ast env proc))
(map-method-core <sal-derived-flat-module>
  (sal-derived-flat-module/map ast env proc))
(map-method <sal-base-component-info> (:input-data list) (:output-data list) (:owned-data list))
(map-method <sal-composite-component-info> (:components list))
(map-local-binds-ast <sal-multi-component-info> :local-decls :component)
(map-method-core <sal-module-models>
  (let* ((module (slot-value ast :module))
         (new-module (proc module env))
         (env (update-env* env (sal-module/state-variables module) (sal-module/state-variables new-module))))
    (update-ast-slots ast
                      :module new-module
                      :expr (sal-ast/map-slot ast :expr env proc))))
(map-method <sal-module-implements> :module1 :module2)
(map-method <sal-assertion-proposition> (:assertion-exprs list))
(map-method-core <sal-esm-module>
  (sal-esm-module/map ast env proc))
(map-local-binds-ast <sal-esm-new-binds-statement> :local-decls :statement)
(map-method <sal-esm-composition-statement> (:statements list))
(map-method <sal-esm-case> :expr (:case-entries list))
(map-method <sal-esm-case-entry> :value :statement)
(map-method <sal-esm-when-undefined> :lhs :statement)
(map-method <sal-esm-guard> :expr)
(map-method <sal-esm-assignment> :lhs :rhs)

(define-api (sal-ast/substitute (ast <sal-ast>) env)
  :doc "Apply the substitutions specified by @code{env}. A new @code{<sal-ast>} instance is returned."
  (if (empty-env? env)
    ast
    (sal-ast/substitute-core ast env)))

(define (sal-ast/substitute-core ast env)
  (sal-ast/map ast env sal-ast/substitute-core))

(define (sal-ast/deep-copy ast)
  (sal-ast/deep-copy-core ast (make-empty-env)))

(define-generic (sal-ast/deep-copy-core ast env))

(define-method (sal-ast/deep-copy-core (ast <sal-ast>) (env <primitive>))
  (sal-ast/map ast env sal-ast/deep-copy-core))

(define-method (sal-ast/deep-copy-core (ast <sal-ast-leaf>) (env <primitive>))
  (copy-ast ast))
  
