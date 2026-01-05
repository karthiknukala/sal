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

(module sal-ast
        (include "utility.sch")
        (include "scmobj.sch")
        (include "api.sch")
        (export <sal-ast> <sal-ast-leaf> <sal-identifier> <sal-new-binds-ast> <sal-local-binds-ast>
                <sal-decl>  <sal-typed-decl> <sal-const-decl> <sal-auxiliary-decl>
                <sal-context> <sal-type-param-decl> <sal-var-decl> <sal-var-param-decl> <sal-state-var-decl> 
                <sal-input-state-var-decl> <sal-choice-input-state-var-decl> <sal-output-state-var-decl> 
                <sal-global-state-var-decl> <sal-local-state-var-decl> <sal-idx-var-decl> <sal-let-decl>
                <sal-top-decl> <sal-recursive-decl> <sal-type-decl> <sal-module-decl> <sal-parametric-module> 
                <sal-constant-decl> <sal-implicit-decl> <sal-constructor-decl> 
                <sal-recognizer-decl> <sal-accessor-decl> <sal-scalar-element-decl>
                <sal-context-name-decl> <sal-assertion-decl> <sal-name-ref> <sal-qualified-name-ref> 
                ;; exprs
                <sal-expr> <sal-simple-expr> <sal-application> <sal-builtin-application>
                <sal-infix-application> <sal-in> <sal-binary-application> <sal-unary-application> 
                <sal-constructor-application> <sal-recognizer-application> 
                <sal-accessor-application>  <sal-eq> <sal-assignment>
                <sal-diseq> <sal-propositional-application> <sal-binary-propositional-application> 
                <sal-unary-propositional-application>
                <sal-iff> <sal-xor> <sal-and> <sal-or> <sal-choice> <sal-not> <sal-implies> <sal-temporal-application>
                <sal-ltl-application> <sal-unary-ltl-application> <sal-binary-ltl-application> <sal-ltl-x> 
                <sal-ltl-g> <sal-ltl-f>
                <sal-ltl-u> <sal-ltl-r> <sal-ltl-w> <sal-ltl-m> <sal-ctl-application>
                <sal-unary-ctl-application> <sal-binary-ctl-application>
                <sal-ctl-ax> <sal-ctl-ex> <sal-ctl-ag> <sal-ctl-eg> <sal-ctl-ar> <sal-ctl-er> <sal-ctl-af> 
                <sal-ctl-ef> <sal-ctl-au> <sal-ctl-eu> 
                <sal-accepting> <sal-weak-accepting>
                <sal-arith-application> <sal-arith-op> <sal-infix-arith-op> <sal-binary-arith-application>
                <sal-binary-arith-op> <sal-binary-infix-arith-op>
                <sal-add> <sal-sub> <sal-mul> <sal-div> <sal-idiv> <sal-mod>
                <sal-max> <sal-min> <sal-arith-relation> <sal-inequality> <sal-lt> <sal-gt> <sal-ge> <sal-le>
                <sal-real-pred> <sal-int-pred>
                <sal-definition-expression> <sal-numeral> <sal-name-expr>
                <sal-var-param-name-expr> <sal-qualified-name-expr> <sal-true> <sal-false> 
                <sal-scalar> <sal-constructor> <sal-accessor> <sal-recognizer>
                <sal-local-binds-expr> <sal-lambda> <sal-set-pred-expr> 
                <sal-set-list-expr> <sal-quantified-expr> <sal-for-all-expr> <sal-exists-expr> <sal-multi-choice-expr>
                <sal-let-expr> <sal-collection-literal> <sal-array-literal> <sal-tuple-literal> <sal-arg-tuple-literal> 
                <sal-record-literal> <sal-state-record-literal> <sal-record-entry> <sal-selection>
                <sal-array-selection> <sal-simple-selection> <sal-tuple-selection> <sal-record-selection> <sal-update-expr> 
                <sal-function-update> <sal-array-update>
                <sal-record-update> <sal-tuple-update> <sal-conditional> 
                <sal-next-operator> <sal-string-expr> <sal-mod-init> <sal-mod-trans>
                <sal-pre-operator> <sal-ring-application> <sal-ring-pre> <sal-ring-succ>
                <sal-debug-application> <sal-debug-print> <sal-debug-expr>
                ;; types
                <sal-type> <sal-type-name> <sal-type-param-name> <sal-any-type> <sal-bool-type> <sal-number-type> 
                <sal-real-type> <sal-int-type> <sal-nat-type>
                <sal-string-type> <sal-qualified-type-name>  
                <sal-function-type> <sal-tuple-type> <sal-domain-tuple-type>
                <sal-record-type> <sal-field> <sal-array-type> <sal-state-type> <sal-subtype> <sal-subrange> 
                <sal-bounded-subtype>
                <sal-type-def> <sal-scalar-type> <sal-data-type> <sal-symmetric-type> <sal-scalar-set-type> 
                <sal-ring-set-type>
                ;; modules
                <sal-definition> <sal-simple-definition> <sal-simple-selection-definition> <sal-for-all-definition> 
                <sal-command-section> <sal-command>
                <sal-guarded-command> <sal-labeled-command> <sal-multi-command> <sal-else-command> 
                <sal-module> <sal-module-composition>
                <sal-non-base-module> <sal-new-output> <sal-org-module> <sal-hiding> <sal-with-module>
                <sal-asynch-composition> <sal-synch-composition> <sal-observer> <sal-multi-composition> 
                <sal-multi-asynch-composition>
                <sal-multi-synch-composition> <sal-renaming> <sal-rename> 
                <sal-module-name> <sal-qualified-module-name> 
                <sal-module-instance> <sal-base-module> <sal-flat-module> <sal-derived-flat-module> 
                <sal-simple-data-flat-module> 
                <sal-boolean-flat-module>  <sal-sliced-flat-module> <sal-sliced-boolean-flat-module>
                <sal-sliced-simple-data-flat-module>
                <sal-component-info> <sal-base-component-info> <sal-multi-component-info> <sal-composite-component-info>
                <sal-data-flat-esm-module>
                ;; assertions
                <sal-assertion-expr> <sal-module-models> <sal-module-implements>
                <sal-assertion-proposition> 
                <sal-qualified-assertion-name>
                <sal-trace-info> <sal-else-trace-info> <sal-labeled-trace-info> <sal-multi-trace-info>
                <sal-multi-choice-trace-info> <sal-multi-sequence-trace-info> <sal-choice-trace-info>
                <sal-sequence-trace-info> <sal-multi-command-choice-trace-info> <sal-module-instance-trace-info>
                <sal-nested-trace-info> <sal-nested-list-trace-info> 
                <sal-transition-trace-info>
                ;; Explicit state module representation
                <sal-esm-component> <sal-esm-statement> <sal-esm-composition-statement> <sal-esm-choice> 
                <sal-esm-case> <sal-esm-case-entry> <sal-esm-when-undefined>
                <sal-esm-seq> <sal-esm-new-binds-statement> <sal-esm-multi-seq> <sal-esm-multi-choice>
                <sal-esm-leaf> <sal-esm-guard> <sal-esm-assignment> <sal-esm-choice-assignment> <sal-esm-module>
                <sal-esm-monitor-seq>
                ))

;; Basic
(define-class <sal-ast> () (:place :context :hash :internal-idx)
  :doc "Abstract class used to model SAL abstract syntaxt tree nodes. The slot @code{:place} contains a SAL place object which stores line and column numbers, @code{:context} contains a reference to the SAL context object which owns this node, @code{:hash} caches the node's hash code, @code{:internal-idx} stores an unique index used to create mapping from nodes to values, and implement sets of nodes.")

(define-class <sal-ast-leaf> (<sal-ast>) ()
  :doc "The superclass of all leaves of an abstract syntax tree.")

(define-class <sal-identifier> (<sal-ast-leaf>) (:name)
  :doc "Models a SAL identifier. The slot @code{:name} contains a symbol.")

(define-class <sal-new-binds-ast> (<sal-ast>) ())

(define-class <sal-local-binds-ast> (<sal-new-binds-ast>) ())

;; Declarations

(define-class <sal-decl> (<sal-ast>) (:id)
  :doc "The superclass of all SAL declarations. The slot @code{:id} contains a reference to a SAL identifier.")

(define-class <sal-typed-decl> (<sal-decl>) (:type)
  :doc "A typed declaration is a superclass for all classes which associate a type with an identifier. The slot @code{:type} contains a reference to a SAL type.")

(define-class <sal-const-decl> (<sal-typed-decl>) (:value)
  :doc "A superclass for constant declarations. Examples: let declarations and global constants. The slot @code{:value} contains a reference to a SAL expression.")

(define-class <sal-auxiliary-decl> (<sal-const-decl>) ()
  :doc "An auxiliary declaration which was internally generated by SAL.")

(define-class <sal-context> (<sal-decl>) (:params :declarations :constant-declarations :type-declarations :module-declarations :assertion-declarations :context-name-declarations :sal-env :importers :internal-actuals :file-name)
  :doc "A SAL context. @code{:params} contains a list of context parameters. @code{:declarations} contains a list of all context declarations (i.e., constants, types, modules, assertions, and context names). @code{:constant-declarations}, @code{:type-declarations}, @code{:module-declarations}, @code{:assertion-declarations}, @code{:context-name-declarations} are symbol tables (symbol to declaration). @code{:sal-env} reference to the SAL environment object which owns this context. @code{:importers} are used to handle external IMPORT statements. @code{:internal-actuals} auxiliary slot used to help the instantiation of context declarations. @code{:file-name} contains the name of the SAL context file.")

(define-class <sal-type-param-decl> (<sal-decl>) ()
  :doc "A context type parameter.")

(define-class <sal-var-decl> (<sal-typed-decl>) ()
  :doc "A SAL variable declaration.")

(define-class <sal-var-param-decl> (<sal-var-decl>) ())

(define-class <sal-idx-var-decl> (<sal-var-decl>) ())

;; the extra two slots are used to compute the dependency graph
(define-class <sal-state-var-decl> (<sal-var-decl>) (:curr-node :next-node)) 

(define-class <sal-input-state-var-decl> (<sal-state-var-decl>) ())

(define-class <sal-choice-input-state-var-decl> (<sal-input-state-var-decl>) ())

(define-class <sal-output-state-var-decl> (<sal-state-var-decl>) ())

(define-class <sal-global-state-var-decl> (<sal-state-var-decl>) ())

(define-class <sal-local-state-var-decl> (<sal-state-var-decl>) ())

(define-class <sal-let-decl> (<sal-const-decl>) ()
  :doc "A let declaration.")

(define-class <sal-top-decl> (<sal-decl>) ())

(define-class <sal-recursive-decl> (<sal-top-decl>) ())

(define-class <sal-type-decl> (<sal-recursive-decl>) (:type))

(define-class <sal-module-decl> (<sal-top-decl>) (:parametric-module))

(define-class <sal-parametric-module> (<sal-local-binds-ast>) (:local-decls :module))

(define-class <sal-constant-decl> (<sal-const-decl> <sal-recursive-decl>) (:memoize?))

(define-class <sal-implicit-decl> (<sal-constant-decl>) ())

(define-class <sal-constructor-decl> (<sal-implicit-decl>) (:accessors :recognizer-decl :data-type-decl))

(define-class <sal-recognizer-decl> (<sal-implicit-decl>) (:constructor-decl))

(define-class <sal-accessor-decl> (<sal-implicit-decl>) (:constructor-decl))

(define-class <sal-scalar-element-decl> (<sal-implicit-decl>) (:scalar-type-decl))

(define-class <sal-context-name-decl> (<sal-top-decl>) (:context-ref :actuals))

(define-class <sal-assertion-decl> (<sal-top-decl>) (:kind :assertion-expr))

;; Names
(define-class <sal-name-ref> (<sal-ast-leaf>) ())

(define-class <sal-qualified-name-ref> (<sal-name-ref>) ())

;; Expressions
(define-class <sal-expr> (<sal-ast>) (:type))

(define-class <sal-simple-expr> (<sal-expr>) ())

(define-class <sal-application> (<sal-expr>) (:fun :arg))

(define-class <sal-infix-application> (<sal-application>) ())

(define-class <sal-in> (<sal-application>) ())

(define-class <sal-binary-application> (<sal-application>) ())

(define-class <sal-binary-infix-application> (<sal-binary-application> <sal-infix-application>) ())

(define-class <sal-unary-application> (<sal-application>) ())

(define-class <sal-builtin-application> (<sal-application>) ())

(define-class <sal-constructor-application> (<sal-application> <sal-builtin-application>) ())

(define-class <sal-recognizer-application> (<sal-unary-application> <sal-builtin-application>) ())

(define-class <sal-accessor-application> (<sal-unary-application> <sal-builtin-application>) ())

(define-class <sal-eq> (<sal-binary-infix-application> <sal-builtin-application>) ())

(define-class <sal-assignment> (<sal-eq>) ())

(define-class <sal-diseq> (<sal-binary-infix-application> <sal-builtin-application>) ())

(define-class <sal-propositional-application> (<sal-application> <sal-builtin-application>) ())

(define-class <sal-binary-propositional-application> (<sal-propositional-application> <sal-binary-infix-application>) ())

(define-class <sal-unary-propositional-application> (<sal-propositional-application> <sal-unary-application>) ())

(define-class <sal-iff> (<sal-binary-propositional-application> <sal-eq>) ())

(define-class <sal-xor> (<sal-binary-propositional-application> <sal-diseq>) ())

(define-class <sal-and> (<sal-propositional-application> <sal-infix-application>) ())

(define-class <sal-or> (<sal-propositional-application> <sal-infix-application>) ())

(define-class <sal-choice> (<sal-or>) ())

(define-class <sal-not> (<sal-unary-propositional-application>) ())

(define-class <sal-implies> (<sal-binary-propositional-application>) ())

(define-class <sal-temporal-application> (<sal-application> <sal-builtin-application>) ())

(define-class <sal-ltl-application> (<sal-temporal-application>) ())

(define-class <sal-unary-ltl-application> (<sal-ltl-application> <sal-unary-application>) ())

(define-class <sal-binary-ltl-application> (<sal-ltl-application> <sal-binary-application>) ())

(define-class <sal-ltl-x> (<sal-unary-ltl-application>) ())

(define-class <sal-ltl-g> (<sal-unary-ltl-application>) ())

(define-class <sal-ltl-f> (<sal-unary-ltl-application>) ())

(define-class <sal-ltl-u> (<sal-binary-ltl-application>) ())

(define-class <sal-ltl-r> (<sal-binary-ltl-application>) ())

(define-class <sal-ltl-w> (<sal-binary-ltl-application>) ())

(define-class <sal-ltl-m> (<sal-binary-ltl-application>) ())

(define-class <sal-ctl-application> (<sal-temporal-application>) ())

(define-class <sal-unary-ctl-application> (<sal-ctl-application> <sal-unary-application>) ())

(define-class <sal-binary-ctl-application> (<sal-ctl-application> <sal-binary-application>) ())

(define-class <sal-ctl-ax> (<sal-unary-ctl-application>) ())

(define-class <sal-ctl-ex> (<sal-unary-ctl-application>) ())

(define-class <sal-ctl-ag> (<sal-unary-ctl-application>) ())

(define-class <sal-ctl-eg> (<sal-unary-ctl-application>) ())

(define-class <sal-ctl-af> (<sal-unary-ctl-application>) ())

(define-class <sal-ctl-ef> (<sal-unary-ctl-application>) ())

(define-class <sal-ctl-au> (<sal-binary-ctl-application>) ())

(define-class <sal-ctl-eu> (<sal-binary-ctl-application>) ())

(define-class <sal-ctl-ar> (<sal-binary-ctl-application>) ())

(define-class <sal-ctl-er> (<sal-binary-ctl-application>) ())

(define-class <sal-accepting> (<sal-temporal-application> <sal-unary-application>) ())

(define-class <sal-weak-accepting> (<sal-accepting>) ())

(define-class <sal-arith-application> (<sal-application> <sal-builtin-application>) ())

(define-class <sal-binary-arith-application> (<sal-arith-application> <sal-binary-application>) ())

(define-class <sal-arith-op> (<sal-arith-application>) ())

(define-class <sal-infix-arith-op> (<sal-arith-op> <sal-infix-application>) ())

(define-class <sal-binary-arith-op> (<sal-arith-op> <sal-binary-arith-application>) ())

(define-class <sal-binary-infix-arith-op> (<sal-binary-arith-op> <sal-infix-arith-op>) ())

(define-class <sal-add> (<sal-infix-arith-op>) ())

(define-class <sal-sub> (<sal-binary-infix-arith-op>) ())

(define-class <sal-mul> (<sal-infix-arith-op>) ())

(define-class <sal-div> (<sal-binary-infix-arith-op>) ())

(define-class <sal-idiv> (<sal-binary-infix-arith-op>) ())

(define-class <sal-mod> (<sal-binary-infix-arith-op>) ())

(define-class <sal-max> (<sal-binary-arith-op>) ())

(define-class <sal-min> (<sal-binary-arith-op>) ())

(define-class <sal-arith-relation> (<sal-binary-arith-application> <sal-infix-application>) ())

(define-class <sal-inequality> (<sal-arith-relation>) ())

(define-class <sal-lt> (<sal-inequality>) ())

(define-class <sal-gt> (<sal-inequality>) ())

(define-class <sal-ge> (<sal-inequality>) ())

(define-class <sal-le> (<sal-inequality>) ())

(define-class <sal-real-pred> (<sal-unary-application> <sal-arith-application>) ())

(define-class <sal-int-pred> (<sal-unary-application> <sal-arith-application>) ())

(define-class <sal-definition-expression> (<sal-expr>) (:lhs-list :expr))

(define-class <sal-numeral> (<sal-expr> <sal-ast-leaf> <sal-simple-expr>) (:num))

(define-class <sal-name-expr> (<sal-expr> <sal-name-ref> <sal-simple-expr>) (:decl))

(define-class <sal-var-param-name-expr> (<sal-name-expr>) ())

(define-class <sal-qualified-name-expr> (<sal-name-expr> <sal-qualified-name-ref>) (:context-ref :actuals))

(define-class <sal-scalar> (<sal-qualified-name-expr>) ())

(define-class <sal-true> (<sal-scalar>) ())

(define-class <sal-false> (<sal-scalar>) ())

(define-class <sal-constructor> (<sal-qualified-name-expr>) ())

(define-class <sal-accessor> (<sal-qualified-name-expr>) ())

(define-class <sal-recognizer> (<sal-qualified-name-expr>) ())

(define-class <sal-local-binds-expr> (<sal-local-binds-ast> <sal-expr>) (:local-decls :expr))

(define-class <sal-lambda> (<sal-local-binds-expr>) ())

(define-class <sal-set-pred-expr> (<sal-lambda>) ())

(define-class <sal-set-list-expr> (<sal-set-pred-expr>) ())

(define-class <sal-quantified-expr> (<sal-local-binds-expr>) ())

(define-class <sal-for-all-expr> (<sal-quantified-expr>) ())

(define-class <sal-exists-expr> (<sal-quantified-expr>) ())

(define-class <sal-multi-choice-expr> (<sal-exists-expr>) ())

(define-class <sal-let-expr> (<sal-local-binds-expr>) ())

(define-class <sal-collection-literal> (<sal-expr>) ())

(define-class <sal-array-literal> (<sal-lambda> <sal-collection-literal>) ())

(define-class <sal-tuple-literal> (<sal-collection-literal>) (:exprs))

(define-class <sal-arg-tuple-literal> (<sal-tuple-literal>) ())

(define-class <sal-record-literal> (<sal-collection-literal>) (:entries))

(define-class <sal-state-record-literal> (<sal-record-literal>) ())

(define-class <sal-record-entry> (<sal-ast>) (:id :expr))

(define-class <sal-selection> (<sal-expr>) ())

(define-class <sal-array-selection> (<sal-application> <sal-selection>) ())

(define-class <sal-simple-selection> (<sal-selection>) (:target :idx))

(define-class <sal-tuple-selection> (<sal-simple-selection>) ())

(define-class <sal-record-selection> (<sal-simple-selection>) ())

(define-class <sal-update-expr> (<sal-expr>) (:target :idx :new-value))

(define-class <sal-function-update> (<sal-update-expr>) ())

(define-class <sal-array-update> (<sal-function-update>) ())

(define-class <sal-record-update> (<sal-update-expr>) ())

(define-class <sal-tuple-update> (<sal-update-expr>) ())

(define-class <sal-conditional> (<sal-expr>) (:cond-expr :then-expr :else-expr))

(define-class <sal-next-operator> (<sal-expr> <sal-simple-expr>) (:name-expr))

(define-class <sal-string-expr> (<sal-expr> <sal-ast-leaf> <sal-simple-expr>) (:string))

(define-class <sal-mod-init> (<sal-expr>) (:module))

(define-class <sal-mod-trans> (<sal-expr>) (:module))

(define-class <sal-ring-application> (<sal-unary-application>) ())

(define-class <sal-ring-pre> (<sal-ring-application>) ())

(define-class <sal-ring-succ> (<sal-ring-application>) ())

(define-class <sal-debug-application> (<sal-application>) ())

(define-class <sal-debug-print> (<sal-debug-application>) ())

(define-class  <sal-debug-expr> (<sal-debug-application>) ())

;; Used in the generation of counterexamples...
(define-class <sal-pre-operator> (<sal-expr>) (:expr))

;; Types
(define-class <sal-type> (<sal-ast>) ())

(define-class <sal-type-name> (<sal-type> <sal-name-ref>) (:decl))

(define-class <sal-type-param-name> (<sal-type-name>) ())

(define-class <sal-qualified-type-name> (<sal-type-name> <sal-qualified-name-ref>) (:context-ref :actuals))

(define-class <sal-any-type> (<sal-qualified-type-name>) ())

(define-class <sal-bool-type> (<sal-qualified-type-name>) ())

(define-class <sal-number-type> (<sal-qualified-type-name>) ())

(define-class <sal-real-type> (<sal-number-type>) ())

(define-class <sal-int-type> (<sal-real-type>) ())

(define-class <sal-nat-type> (<sal-int-type>) ())

(define-class <sal-string-type> (<sal-qualified-type-name>) ())

(define-class <sal-function-type> (<sal-type>) (:domain :range))

(define-class <sal-array-type> (<sal-function-type>) ())

(define-class <sal-tuple-type> (<sal-type>) (:types))

(define-class <sal-domain-tuple-type> (<sal-tuple-type>) ())

(define-class <sal-record-type> (<sal-type>) (:fields))

(define-class <sal-field> (<sal-ast>) (:id :type))

(define-class <sal-state-type> (<sal-type>) (:module))

(define-class <sal-subtype> (<sal-type>) (:expr))

(define-class <sal-bounded-subtype> (<sal-subtype>) (:lower :upper))

(define-class <sal-subrange> (<sal-bounded-subtype>) ())

(define-class <sal-symmetric-type> (<sal-subrange>) ())

(define-class <sal-scalar-set-type> (<sal-symmetric-type>) ())

(define-class <sal-ring-set-type> (<sal-symmetric-type>) ())

;; Type defs

(define-class <sal-type-def> (<sal-type>) ())

(define-class <sal-scalar-type> (<sal-type-def>) (:scalar-elements))

(define-class <sal-data-type> (<sal-type-def>) (:constructors))

;; Commands

(define-class <sal-definition> (<sal-ast>) ())

(define-class <sal-simple-definition> (<sal-definition>) (:lhs :rhs))

(define-class <sal-simple-selection-definition> (<sal-simple-definition>) ())

(define-class <sal-for-all-definition> (<sal-local-binds-ast> <sal-definition>) (:local-decls :definitions))

(define-class <sal-command-section> (<sal-ast>) (:commands :else-command))

(define-class <sal-command> (<sal-ast>) ())

(define-class <sal-guarded-command> (<sal-command>) (:guard :assignments))

(define-class <sal-labeled-command> (<sal-command>) (:label :command))

(define-class <sal-multi-command> (<sal-local-binds-ast> <sal-command>) (:local-decls :command))

(define-class <sal-else-command> (<sal-command>) (:assignments))

;; Modules
(define-class <sal-module> (<sal-ast>) (:state-vars :state-vars-table))

(define-class <sal-non-base-module> (<sal-module>) ())

(define-class <sal-module-composition> (<sal-non-base-module>) (:module1 :module2))

(define-class <sal-asynch-composition> (<sal-module-composition>) ())

(define-class <sal-synch-composition> (<sal-module-composition>) ())

(define-class <sal-observer> (<sal-synch-composition>) ())

(define-class <sal-multi-composition> (<sal-local-binds-ast> <sal-non-base-module>) (:local-decls :module))

(define-class <sal-multi-asynch-composition> (<sal-multi-composition>) ())

(define-class <sal-multi-synch-composition> (<sal-multi-composition>) ())

(define-class <sal-renaming> (<sal-non-base-module>) (:renames :module))

(define-class <sal-rename> (<sal-ast>) (:from-name :to-expr))

(define-class <sal-org-module> (<sal-non-base-module>) (:identifiers :module))

(define-class <sal-hiding> (<sal-org-module>) ())

(define-class <sal-new-output> (<sal-org-module>) ())

(define-class <sal-with-module> (<sal-new-binds-ast> <sal-non-base-module>) (:new-state-vars :module))

(define-class <sal-module-name> (<sal-name-ref>) (:decl))

(define-class <sal-qualified-module-name> (<sal-module-name> <sal-qualified-name-ref>) (:context-ref :actuals))

(define-class <sal-module-instance> (<sal-non-base-module>) (:module-name :actuals))

(define-class <sal-base-module> (<sal-new-binds-ast> <sal-module>) (:definitions
                                                                    :initialization-definitions 
                                                                    :initialization-command-section 
                                                                    :transition-definitions
                                                                    :transition-command-section))

(define-class <sal-flat-module> (<sal-new-binds-ast> <sal-module>) (:definition 
                                                                    :initialization 
                                                                    :transition 
                                                                    :skip
                                                                    :transition-trace-info
                                                                    :choice-vars ;; temporary slot used in the flat module procedure
                                                                    :component-info
                                                                    :valid-input-expr ;; expression that specifies what is a valid input
                                                                    :valid-state-expr ;; expression that specifies what is a valid state (latches)
                                                                    :valid-constant-expr)) ;; expression that specifies what is a valid global cnst.

(define-class <sal-component-info> (<sal-ast>) (:data)) ;; :data is an auxiliary field used in some algorithms 
(define-class <sal-base-component-info> (<sal-component-info>) (:input-data :output-data :owned-data))
(define-class <sal-multi-component-info> (<sal-component-info> <sal-local-binds-ast>) (:local-decls :component))
(define-class <sal-composite-component-info> (<sal-component-info>) (:components))

(define-class <sal-derived-flat-module> (<sal-flat-module>) (:original-module
                                                             ;; A variable in the original module can be mapped to one or more variables in
                                                             ;; the derived module. The slot :var-trace-info store this mapping.
                                                             :var-trace-info ;; eq-hash-table (original-decl -> decl-list)
                                                             ;; When a flat-module is converted, some global declarations may be also mapped.
                                                             ;; They are stored in the slot :const-trace-info.
                                                             ;; The slot :const-trace-info is a mapping sal-ast-table from 
                                                             ;;  qualified-name-ref -> decl-list
                                                             ;; We used qualified-name-ref to be able to store the actuals
                                                             ;; in the mapping.
                                                             :const-trace-info 
                                                             ))

(define-class <sal-sliced-flat-module> (<sal-derived-flat-module>) ())

(define-class <sal-simple-data-flat-module> (<sal-derived-flat-module>) ())

(define-class <sal-sliced-simple-data-flat-module> (<sal-sliced-flat-module> <sal-simple-data-flat-module>) ())

(define-class <sal-boolean-flat-module> (<sal-derived-flat-module>) ())

(define-class <sal-sliced-boolean-flat-module> (<sal-sliced-flat-module> <sal-boolean-flat-module>) ())

;; Assertion Expr

(define-class <sal-assertion-expr> (<sal-expr>) ())

(define-class <sal-module-models> (<sal-assertion-expr>) (:module :expr))

(define-class <sal-module-implements> (<sal-assertion-expr>) (:module1 :module2))

(define-class <sal-assertion-proposition> (<sal-assertion-expr>) (:op :assertion-exprs))

(define-class <sal-qualified-assertion-name> (<sal-qualified-name-ref> <sal-assertion-expr>) (:decl :context-ref :actuals))

;; Traceability
(define-class <sal-trace-info> (<sal-ast>) ())

(define-class <sal-transition-trace-info> (<sal-trace-info>) ())

(define-class <sal-else-trace-info> (<sal-trace-info>) ())

(define-class <sal-nested-trace-info> (<sal-trace-info>) (:info))

(define-class <sal-module-instance-trace-info> (<sal-nested-trace-info>) ())

(define-class <sal-labeled-trace-info> (<sal-nested-trace-info>) (:label))

(define-class <sal-multi-trace-info> (<sal-nested-trace-info>) ())

(define-class <sal-multi-choice-trace-info> (<sal-multi-trace-info>) (:choice-var-names :original-var-names))

(define-class <sal-multi-command-choice-trace-info> (<sal-multi-choice-trace-info>) ())

(define-class <sal-multi-sequence-trace-info> (<sal-multi-trace-info>) (:idx-var-name))

(define-class <sal-nested-list-trace-info> (<sal-trace-info>) (:info-list))

(define-class <sal-choice-trace-info> (<sal-nested-list-trace-info>) (:choice-var-name))

(define-class <sal-sequence-trace-info> (<sal-nested-list-trace-info>) ())


;; Explicit state module representation
(define-class <sal-esm-component> (<sal-ast>) ())


;; The slot :num-rules caches the result of the method sal-esm/num-alternatives
(define-class <sal-esm-statement> (<sal-esm-component>) (:num-alternatives)) 

(define-class <sal-esm-composition-statement> (<sal-esm-statement>) (:statements))
(define-class <sal-esm-choice> (<sal-esm-composition-statement>) ())
(define-class <sal-esm-seq> (<sal-esm-composition-statement>) ())
(define-class <sal-esm-monitor-seq> (<sal-esm-seq>) ())

(define-class <sal-esm-case> (<sal-esm-statement>) (:expr :case-entries))
(define-class <sal-esm-case-entry> (<sal-esm-component>) (:value :statement))

(define-class <sal-esm-when-undefined> (<sal-esm-statement>) (:lhs :statement))

(define-class <sal-esm-new-binds-statement> (<sal-esm-statement>) (:local-decls :statement))
(define-class <sal-esm-multi-seq> (<sal-esm-new-binds-statement>) ())
(define-class <sal-esm-multi-choice> (<sal-esm-new-binds-statement>) ())

;; The slot :no-delay? contains #t when the esm-leaf cannot be delayed because the
;; value of an used variable is not available yet.
(define-class <sal-esm-leaf> (<sal-esm-statement>) (:dependencies :no-delay?))
(define-class <sal-esm-guard> (<sal-esm-leaf>) (:expr))
(define-class <sal-esm-assignment> (<sal-esm-leaf>) (:lhs :rhs))
(define-class <sal-esm-choice-assignment> (<sal-esm-assignment>) ())

(define-class <sal-esm-module> (<sal-esm-component> <sal-module>) (:initialization
                                                                   :transition
                                                                   :definition
                                                                   :transition-trace-info
                                                                   :choice-vars ;; temporary slot used in the esm module construction procedure
                                                                   ))

(define-class <sal-data-flat-esm-module> (<sal-esm-module>) (:original-module
                                                             :var-trace-info))
