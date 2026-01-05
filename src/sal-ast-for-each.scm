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

(module sal-ast-for-each
        (include "sal.sch")
        (import symbol-table queue)
        (export (sal-ast/for-each-children proc ast)
                (sal-ast/for-all-children proc ast)
                (sal-ast/exists-children proc ast)
                (sal-ast/fold-children proc init ast)
                (sal-ast/for-each proc ast)
                (sal-ast/for-each+ proc-before ast proc-after)
                (sal-ast/fold proc initial ast)
                (sal-ast/find proc ast)
                (sal-new-binds-ast/for-each-new-bind proc ast)
                (sal-new-binds-ast/fold-new-binds proc initial ast)
                (sal-new-binds-ast/exists-new-bind proc ast))
        )
;; Apply @code{proc} over each child of @code{ast}.
;; Examples: (sal-ast/for-each-children sal/pp (sal/expr "(mk-tuple 2 3)"))
(define-generic (sal-ast/for-each-children proc ast))

(define-method (sal-ast/for-each-children :around (proc <primitive>) (ast <sal-qualified-name-ref>))
  (call-next-method))

(define *for-all-children* (for-each->for-all sal-ast/for-each-children))

(define-api (sal-ast/for-all-children (pred? procedure?) (ast <sal-ast>))
  :doc "Return true when all children of @code{ast} satisfy the predicate @code{pred?}."
  :examples '((sal-ast/for-all-children (lambda (child) 
                                          (= (sal-ast/size child) 1))
                                        (sal/expr "(mk-tuple 2 3)")))
  (*for-all-children* pred? ast))

(define *exists-children* (for-each->find sal-ast/for-each-children))

(define-api (sal-ast/exists-children (pred? procedure?) (ast <sal-ast>))
  (*exists-children* pred? ast))

(define *fold-children* (for-each->fold sal-ast/for-each-children))

(define (sal-ast/fold-children proc init ast)
  (*fold-children* proc init ast))


;;
;; BD: fixed bug in sal-ast/for-each? 
;; bigloo2.6 and bigloo2.8 do not expand when in the same way:
;;
;; in bigloo2.6 (when '#f 'a) returns #unspecified
;; in bigloo2.8 (when '#f 'a) returns #f
;;
;; the proc passed to for-each may be defined using a (when (....))
;; (cf. utility.scm)
;; so sal-ast/for-each must not do any test on (proc a)
;;
(define (sal-ast/for-each proc ast)
  (let loop ((ast ast))
    (proc ast)
    (sal-ast/for-each-children loop ast)))
;;    (when (proc ast)    
;;	  (sal-ast/for-each-children loop ast))))

(define (sal-ast/for-each+ proc-before ast proc-after)
  (let loop ((ast ast))
    (when (proc-before ast)
      (sal-ast/for-each-children loop ast)
      (proc-after ast))))

(define *fold* (for-each->fold sal-ast/for-each))

(define (sal-ast/fold proc initial ast)
  (*fold* proc initial ast))

(define *find* (for-each->find sal-ast/for-each))

(define (sal-ast/find proc ast)
  (*find* proc ast))
                                  
(define-macro (traverse . slot-def-list)
  `(begin ,@(map (lambda (slot-def)
                   (cond 
                    ((keyword? slot-def)
                     `(proc (slot-value ast ,slot-def)))
                    ((optional-slot-def? slot-def)
                     `(when (slot-value ast ,(car slot-def))
                        (proc (slot-value ast ,(car slot-def)))))
                    ((list-slot-def? slot-def)
                     `(for-each proc (slot-value ast ,(car slot-def))))
                    ((queue-slot-def? slot-def)
                     `(for-each proc (queue->list (slot-value ast ,(car slot-def)))))
                    (else
                     (error 'traverse "Invalid slot definition." #unspecified))))
                 slot-def-list)))

(define-macro (for-each-method class . slot-def-list)
  `(define-method (sal-ast/for-each-children (proc <primitive>) (ast ,class))
     (traverse ,@slot-def-list)))

(for-each-method <sal-ast>)
(for-each-method <sal-decl> :id)
(for-each-method <sal-typed-decl> :id :type)
(for-each-method <sal-const-decl> :id :type (:value optional))
(for-each-method <sal-type-decl> :id (:type optional))
(for-each-method <sal-constant-decl> :id :type (:value optional))
(for-each-method <sal-constructor-decl> :id :type (:accessors list))
(for-each-method <sal-module-decl> :id :parametric-module)
(for-each-method <sal-parametric-module> (:local-decls list) :module)
(for-each-method <sal-context-name-decl> :id (:actuals list))
(for-each-method <sal-assertion-decl> :id :assertion-expr)
(for-each-method <sal-context> :id (:params list) (:declarations queue))
(for-each-method <sal-qualified-name-expr> (:actuals list))
(for-each-method <sal-qualified-type-name> (:actuals list))
(for-each-method <sal-qualified-module-name> (:actuals list))
(for-each-method <sal-qualified-assertion-name> (:actuals list))
(for-each-method <sal-tuple-literal> (:exprs list))
(for-each-method <sal-application> :fun :arg)
(for-each-method <sal-local-binds-expr> (:local-decls list) :expr)
(for-each-method <sal-record-literal> (:entries list))
(for-each-method <sal-record-entry> :id :expr)
(for-each-method <sal-simple-selection> :target :idx)
(for-each-method <sal-update-expr> :target :idx :new-value)
(for-each-method <sal-conditional> :cond-expr :then-expr :else-expr)
(for-each-method <sal-next-operator> :name-expr)
(for-each-method <sal-mod-init> :module)
(for-each-method <sal-mod-trans> :module)
(for-each-method <sal-pre-operator> :expr)
(for-each-method <sal-function-type> :domain :range)
(for-each-method <sal-tuple-type> (:types list))
(for-each-method <sal-record-type> (:fields list))
(for-each-method <sal-field> :id :type)
(for-each-method <sal-state-type> :module)
(for-each-method <sal-subtype> :expr)
(for-each-method <sal-scalar-type> (:scalar-elements list))
(for-each-method <sal-data-type> (:constructors list))
(for-each-method <sal-definition-expression> (:lhs-list list) :expr)
(for-each-method <sal-simple-definition> :lhs :rhs)
(for-each-method <sal-for-all-definition> (:local-decls list) (:definitions list))
(for-each-method <sal-command-section> (:commands list) (:else-command optional))
(for-each-method <sal-guarded-command> :guard (:assignments list))
(for-each-method <sal-labeled-command> :label :command)
(for-each-method <sal-multi-command> (:local-decls list) :command)
(for-each-method <sal-else-command> (:assignments list))
(for-each-method <sal-module-composition> :module1 :module2)
(for-each-method <sal-multi-composition> (:local-decls list) :module)
(for-each-method <sal-org-module> (:identifiers list) :module)
(for-each-method <sal-with-module> (:new-state-vars list) :module)
(for-each-method <sal-renaming> (:renames list) :module)
(for-each-method <sal-rename> :from-name :to-expr)
(for-each-method <sal-module-instance> :module-name (:actuals list))
(for-each-method <sal-base-module> (:state-vars list)
                 (:definitions list) (:initialization-definitions list) (:initialization-command-section optional)
                 (:transition-definitions list) (:transition-command-section optional))
(for-each-method <sal-flat-module> 
                 (:state-vars list) :definition :initialization :transition :skip :component-info 
                 :valid-input-expr :valid-state-expr :valid-constant-expr)
(for-each-method <sal-base-component-info> (:input-data list) (:output-data list) (:owned-data list))
(for-each-method <sal-composite-component-info> (:components list))
(for-each-method <sal-multi-component-info> (:local-decls list) :component)
(for-each-method <sal-module-models> :module :expr)
(for-each-method <sal-module-implements> :module1 :module2)
(for-each-method <sal-assertion-proposition> (:assertion-exprs list))
(for-each-method <sal-esm-module> (:state-vars list) (:definition optional) (:initialization optional) (:transition optional))
(for-each-method <sal-esm-new-binds-statement> (:local-decls list) :statement)
(for-each-method <sal-esm-case> :expr (:case-entries list))
(for-each-method <sal-esm-case-entry> :value :statement)
(for-each-method <sal-esm-when-undefined> :lhs :statement)
(for-each-method <sal-esm-composition-statement> (:statements list))
(for-each-method <sal-esm-guard> :expr)
(for-each-method <sal-esm-assignment> :lhs :rhs)
                 

(define-generic (sal-new-binds-ast/for-each-new-bind proc ast))

(define-method (sal-new-binds-ast/for-each-new-bind (proc <primitive>) (ast <sal-local-binds-ast>))
  (for-each proc (sal-local-binds-ast/local-decls ast)))

(define-method (sal-new-binds-ast/for-each-new-bind (proc <primitive>) (ast <sal-base-module>))
  (for-each proc (slot-value ast :state-vars)))

(define-method (sal-new-binds-ast/for-each-new-bind (proc <primitive>) (ast <sal-flat-module>))
  (for-each proc (slot-value ast :state-vars)))

(define-method (sal-new-binds-ast/for-each-new-bind (proc <primitive>) (ast <sal-with-module>))
  (for-each proc (slot-value ast :new-state-vars)))

(define *fold-new-binds* (for-each->fold sal-new-binds-ast/for-each-new-bind))

(define (sal-new-binds-ast/fold-new-binds proc initial ast)
  (*fold-new-binds* proc initial ast))

(define *exists-new-bind* (for-each->exists sal-new-binds-ast/for-each-new-bind))

(define (sal-new-binds-ast/exists-new-bind proc ast)
  (*exists-new-bind* proc ast))
