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

(module sal-ast-eq
        (include "sal.sch")
        (import sal-ast-env gmp-scheme sal-type sal-module)
        (export (sal-ast/compare-children ast1 ast2 env pred?)
                (sal-ast/compare-list? ast1-lst ast2-lst env pred?)
                (sal-ast/equivalent? ast1 ast2)
                (sal-ast/equivalent-core? ast1 ast2 env)
                (sal-name-expr/equivalent? ast1 ast2 env))
        )

;; Compare the children of @code{ast1} and @code{ast2} using the binary 
;; predicate @code{pred?}. Actually, @code{pred?} receives three arguments: 
;; @code{child-of-ast1}, @code{child-of-ast2}, and a new (possibly equal) environment.
;; @code{ast1} and @code{ast2} must be instances of the same class.
;; @code{env} is a mapping from @code{<sal-decl>} to @code{<sal-decl>}.
(define-generic (sal-ast/compare-children ast1 ast2 env pred?))

(define-macro (sal-ast/compare-slot? ast1 ast2 slot-id env pred?)
  `(pred? (slot-value ,ast1 ,slot-id) (slot-value ,ast2 ,slot-id) ,env))

(define-macro (sal-ast/compare-optional-slot? ast1 ast2 slot-id env pred?)
  `(let ((%val1 (slot-value ,ast1 ,slot-id))
         (%val2 (slot-value ,ast2 ,slot-id)))
     (cond
      ((and %val1 %val2)
       (,pred? %val1 %val2 ,env))
      (else
       (eq? %val1 %val2)))))

(define (sal-ast/compare-list? ast1-lst ast2-lst env pred?)
  (for-all (lambda (a1 a2) (pred? a1 a2 env)) ast1-lst ast2-lst))

(define-macro (sal-ast/compare-list-slot? ast1 ast2 slot-id env pred?)
  `(sal-ast/compare-list? (slot-value ,ast1 ,slot-id) (slot-value ,ast2 ,slot-id) ,env ,pred?))

(define-macro (sal-local-binds-ast/compare-children-core ast1 ast2 env pred? decl-slot-id body-slot-id body-pred?)
  `(let* ((%ast1 ast1)
          (%ast2 ast2)
          (%local-decls1 (slot-value %ast1 ,decl-slot-id))
          (%local-decls2 (slot-value %ast2 ,decl-slot-id)))
     (and (sal-ast/compare-list? %local-decls1 %local-decls2 ,env ,pred?)
          (let ((%env (update-env* ,env %local-decls1 %local-decls2)))
            (,body-pred? %ast1 %ast2 ,body-slot-id %env ,pred?)))))

(define-macro (sal-local-binds-ast/compare-children ast1 ast2 env pred? decl-slot-id body-slot-id)
  `(sal-local-binds-ast/compare-children-core 
    ,ast1 ,ast2 ,env ,pred? ,decl-slot-id ,body-slot-id sal-ast/compare-slot?))

(define (sal-for-all-definition/compare-children ast1 ast2 env pred?)
  (sal-local-binds-ast/compare-children-core 
   ast1 ast2 env pred? :local-decls :definitions sal-ast/compare-list-slot?))

(define (sal-base-module/compare-chilren-core ast1 ast2 env pred? body-pred?)
  (let* ((state-vars1 (slot-value ast1 :state-vars))
         (state-vars2 (slot-value ast2 :state-vars)))
    (and (sal-ast/compare-list? state-vars1 state-vars2 env pred?)
         (let ((env (update-env* env state-vars1 state-vars2)))
           (body-pred? env)))))

(define (sal-module/compare-children-core ast1 ast2 env pred? compare-body-proc)
  (let ((state-vars1 (slot-value ast1 :state-vars))
        (state-vars2 (slot-value ast2 :state-vars)))
    (and (sal-ast/compare-list? state-vars1 state-vars2 env pred?)
         (let ((new-env (update-env* env state-vars1 state-vars2)))
           (compare-body-proc new-env)))))

(define (sal-base-module/compare-chilren ast1 ast2 env pred?)
  (sal-module/compare-children-core 
   ast1 ast2 env pred?
   (lambda (new-env)
     (and (sal-ast/compare-list-slot? ast1 ast2 :definitions new-env pred?)
          (sal-ast/compare-list-slot? ast1 ast2 :initialization-definitions new-env pred?)
          (sal-ast/compare-list-slot? ast1 ast2 :transition-definitions new-env pred?)
          (sal-ast/compare-optional-slot? ast1 ast2 :initialization-command-section new-env pred?)
          (sal-ast/compare-optional-slot? ast1 ast2 :transition-command-section new-env pred?)))))

(define (sal-flat-module/compare-children ast1 ast2 env pred?)
  (sal-module/compare-children-core 
   ast1 ast2 env pred? 
   (lambda (new-env)
     (and (sal-ast/compare-slot? ast1 ast2 :definition env pred?)
          (sal-ast/compare-slot? ast1 ast2 :initialization env pred?)
          (sal-ast/compare-slot? ast1 ast2 :transition env pred?)
          (sal-ast/compare-slot? ast1 ast2 :skip env pred?)
          (sal-ast/compare-slot? ast1 ast2 :valid-input-expr new-env pred?)
          (sal-ast/compare-slot? ast1 ast2 :valid-state-expr new-env pred?)
          (sal-ast/compare-slot? ast1 ast2 :valid-constant-expr new-env pred?)))))

(define-macro (compare-esm-slot? ast1 ast2 slot-name env pred?)
  `(let ((%ast1 ,ast1)
         (%ast2 ,ast2))
     (and
      (eq? (obj->boolean (slot-value %ast1 ,slot-name))
           (obj->boolean (slot-value %ast2 ,slot-name)))
      (sal-ast/compare-slot? %ast1 %ast2 ,slot-name ,env ,pred?))))

(define (sal-esm-module/compare-children ast1 ast2 env pred?)
  (sal-module/compare-children-core 
   ast1 ast2 env pred? 
   (lambda (new-env)
     (and (compare-esm-slot? ast1 ast2 :definition env pred?)
          (compare-esm-slot? ast1 ast2 :initialization env pred?)
          (compare-esm-slot? ast1 ast2 :transition env pred?)))))

(define-macro (compare-body . slot-def-list)
  `(and ,@(map (lambda (slot-def)
                 (cond
                  ((keyword? slot-def)
                   `(sal-ast/compare-slot? ast1 ast2 ,slot-def env pred?))
                  ((optional-slot-def? slot-def)
                   `(sal-ast/compare-optional-slot? ast1 ast2 ,(car slot-def) env pred?))
                  ((list-slot-def? slot-def)
                   `(sal-ast/compare-list-slot? ast1 ast2 ,(car slot-def) env pred?))
                  (else
                   (error 'compare-body "Invalid slot definition." #unspecified))))
               slot-def-list)))

(define-macro (compare-method-core class . body)
  `(define-method (sal-ast/compare-children (ast1 ,class) (ast2 ,class) (env <primitive>) (pred? <primitive>))
     ,@body))

(define-macro (compare-method class . slot-def-list)
  `(compare-method-core ,class (compare-body ,@slot-def-list)))

(compare-method <sal-decl> :id)
(compare-method <sal-typed-decl> :id :type)
(compare-method <sal-const-decl> :id :type (:value optional))
(compare-method-core <sal-context> (sign-error "It is not possible to compare contexts."))
(compare-method-core <sal-top-decl> (sign-error "It is not possible to compare top level declarations."))
(compare-method-core <sal-parametric-module>
  (sal-local-binds-ast/compare-children ast1 ast2 env pred? :local-decls :module))
(compare-method <sal-qualified-name-expr> (:actuals list))
(compare-method <sal-qualified-type-name> (:actuals list))
(compare-method <sal-qualified-module-name> (:actuals list))
(compare-method <sal-qualified-assertion-name> (:actuals list))
(compare-method <sal-application> :fun :arg)
(compare-method-core <sal-local-binds-expr>
  (sal-local-binds-ast/compare-children ast1 ast2 env pred? :local-decls :expr))
(compare-method <sal-tuple-literal> (:exprs list))
(compare-method <sal-record-literal> (:entries list))
(compare-method <sal-record-entry> :id :expr)
(compare-method <sal-simple-selection> :target :idx)
(compare-method <sal-update-expr> :target :idx :new-value)
(compare-method <sal-conditional> :cond-expr :then-expr :else-expr)
(compare-method <sal-next-operator> :name-expr)
(compare-method <sal-mod-init> :module)
(compare-method <sal-mod-trans> :module)
(compare-method <sal-pre-operator> :expr)
(compare-method <sal-function-type> :domain :range)
(compare-method <sal-tuple-type> (:types list))
(compare-method <sal-record-type> (:fields list))
(compare-method <sal-field> :id :type)
(compare-method <sal-state-type> :module)
(compare-method <sal-subtype> :expr)
(compare-method <sal-subrange> :lower :upper)
(compare-method <sal-scalar-type> (:scalar-elements list))
(compare-method <sal-data-type> (:constructors list))
(compare-method <sal-simple-definition> :lhs :rhs)
(compare-method <sal-definition-expression> (:lhs-list list) :expr)
(compare-method-core <sal-for-all-definition> 
  (sal-for-all-definition/compare-children ast1 ast2 env pred?))
(compare-method <sal-command-section> (:commands list) (:else-command optional))
(compare-method <sal-guarded-command> :guard (:assignments list))
(compare-method <sal-labeled-command> :label :command)
(compare-method <sal-else-command> (:assignments list))
(compare-method-core <sal-multi-command>
  (sal-local-binds-ast/compare-children ast1 ast2 env pred? :local-decls :command))
(compare-method <sal-module-composition> :module1 :module2)
(compare-method-core <sal-multi-composition>
  (sal-local-binds-ast/compare-children ast1 ast2 env pred? :local-decls :module))
(compare-method <sal-org-module> (:identifiers list) :module)
(compare-method-core <sal-with-module>
  (sal-local-binds-ast/compare-children ast1 ast2 env pred? :new-state-vars :module))
(compare-method <sal-renaming> (:renames list) :module)
(compare-method <sal-rename> :from-name :to-expr)
(compare-method <sal-module-instance> :module-name (:actuals list))
(compare-method-core <sal-base-module>
  (sal-base-module/compare-chilren ast1 ast2 env pred?))
(compare-method-core <sal-flat-module>
  (sal-flat-module/compare-children ast1 ast2 env pred?))
(compare-method-core <sal-module-models> 
  (let ((module1 (slot-value ast1 :module))
        (module2 (slot-value ast2 :module)))
    (and
     (pred? module1 module2 env)
     (let ((env (update-env* env (sal-module/state-variables module1) (sal-module/state-variables module2))))
       (sal-ast/compare-slot? ast1 ast2 :expr env pred?)))))
(compare-method <sal-module-implements> :module1 :module2)
(compare-method <sal-assertion-proposition> (:assertion-exprs list))
(compare-method-core <sal-esm-module>
  (sal-esm-module/compare-children ast1 ast2 env pred?))
(compare-method-core <sal-esm-new-binds-statement>
  (sal-local-binds-ast/compare-children ast1 ast2 env pred? :local-decls :statement))
(compare-method <sal-esm-composition-statement> (:statements list))
(compare-method <sal-esm-case> :expr (:case-entries list))
(compare-method <sal-esm-case-entry> :value :statement)
(compare-method <sal-esm-when-undefined> :lhs :statement)
(compare-method <sal-esm-guard> :expr)
(compare-method <sal-esm-assignment> :lhs :rhs)

(define-api (sal-ast/equivalent? (ast1 <sal-ast>) (ast2 <sal-ast>))
  :doc "Return true when @code{ast1} is alpha equivalent to @code{ast2}."
  :examples '((let ((n1 (sal/expr "(lambda (x::nat) (+ x 1))"))
                    (n2 (sal/expr "(lambda (y::nat) (+ y 1))")))
                (sal-ast/equivalent? n1 n2)))
  (sal-ast/equivalent-core? ast1 ast2 (make-empty-env)))


;; Return true when @code{ast1} is alpha equivalent to @code{ast2}, 
;; assuming the environemnt @code{env}. @code{env} is a mapping from
;; @code{<sal-decl>} to @code{<sal-decl>}.
(define-generic (sal-ast/equivalent-core? ast1 ast2 env))

(define-method (sal-ast/equivalent-core? :around (ast1 <sal-ast>) (ast2 <sal-ast>) (env <primitive>))
  (or (eq? ast1 ast2)
      (call-next-method)))

(define-method (sal-ast/equivalent-core? (ast1 <sal-ast>) (ast2 <sal-ast>) (env <primitive>))
  (and (eq? (class-of ast1) (class-of ast2))
       (sal-ast/compare-children ast1 ast2 env sal-ast/equivalent-core?)))

(define-method (sal-ast/equivalent-core? (ast1 <sal-typed-decl>) (ast2 <sal-typed-decl>) (env <primitive>))
  ;; I should not to compare the identifiers, since I'm perfoming alpha equivalence
  (sal-ast/equivalent-core? (slot-value ast1 :type) (slot-value ast2 :type) env))

(define-method (sal-ast/equivalent-core? (ast1 <sal-const-decl>) (ast2 <sal-const-decl>) (env <primitive>))
  (and (call-next-method)
       (sal-ast/equivalent-core? (slot-value ast1 :value) (slot-value ast2 :value) env)))

(define-method (sal-ast/equivalent-core? (ast1 <sal-identifier>) (ast2 <sal-identifier>) (env <primitive>))
  (eq? (slot-value ast1 :name) (slot-value ast2 :name)))

(define-generic (sal-name-ref/equivalent? ast1 ast2 env))
(define-method (sal-name-ref/equivalent? (ast1 <sal-name-ref>) (ast2 <sal-name-ref>) (env <primitive>))
  (eq? (sal-name-ref/decl ast1) (sal-name-ref/decl ast2)))
(define-method (sal-name-ref/equivalent? (ast1 <sal-qualified-name-ref>) (ast2 <sal-qualified-name-ref>) (env <primitive>))
  (and (eq? (sal-name-ref/decl ast1) (sal-name-ref/decl ast2))
       (eq? (sal-qualified-name-ref/context-ref ast1) (sal-qualified-name-ref/context-ref ast2))
       (sal-ast/compare-children ast1 ast2 env sal-ast/equivalent-core?)))

(define-method (sal-ast/equivalent-core? (ast1 <sal-name-ref>) (ast2 <sal-name-ref>) (env <primitive>))
  (sal-name-ref/equivalent? ast1 ast2 env))

(define (sal-name-expr/equivalent? ast1 ast2 env)
  (let ((decl1 (slot-value ast1 :decl))
        (decl2 (slot-value ast2 :decl)))
    (or 
     (eq? decl1 decl2)
     (cond 
      ((lookup-env decl1 env) =>
       (lambda (to-decl)
         (eq? to-decl decl2)))
      (else
       #f)))))

(define-method (sal-ast/equivalent-core? (ast1 <sal-name-expr>) (ast2 <sal-name-expr>) (env <primitive>))
  (sal-name-expr/equivalent? ast1 ast2 env))

(define-method (sal-ast/equivalent-core? (ast1 <sal-qualified-name-expr>) (ast2 <sal-qualified-name-expr>) (env <primitive>))
  (sal-name-ref/equivalent? ast1 ast2 env))

(define-method (sal-ast/equivalent-core? (ast1 <sal-numeral>) (ast2 <sal-numeral>) (env <primitive>))
  (=mpq (slot-value ast1 :num) (slot-value ast2 :num)))

(define-method (sal-ast/equivalent-core? (ast1 <sal-string-expr>) (ast2 <sal-string-expr>) (env <primitive>))
  (equal? (slot-value ast1 :string) (slot-value ast2 :string)))

(define-method (sal-ast/equivalent-core? (ast1 <sal-type-name>) (ast2 <sal-type>) (env <primitive>))
  (let ((def (sal-type-name/definition ast1)))
    (and def (sal-ast/equivalent-core? ast2 def env))))

(define-method (sal-ast/equivalent-core? (ast1 <sal-type>) (ast2 <sal-type-name>) (env <primitive>))
  (let ((def (sal-type-name/definition ast2)))
    (and def (sal-ast/equivalent-core? ast1 def env))))

(define-method (sal-ast/equivalent-core? (ast1 <sal-type-name>) (ast2 <sal-type-name>) (env <primitive>))
  (or (sal-name-ref/equivalent? ast1 ast2 env)
      (let ((def1 (sal-type-name/definition ast1))
            (def2 (sal-type-name/definition ast2)))
        (if (and def1 def2)
          (sal-ast/equivalent-core? def1 def2 env)
          (eq? (slot-value ast1 :decl) (slot-value ast2 :decl))))))

