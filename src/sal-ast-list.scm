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

(module sal-ast-list
        (include "sal.sch")
        (import symbol-table gmp-scheme queue sal-ast-support sal-context sal-expression)
        (export (sal-ast->list ast))
        )

;; converts a SAL ast in a list, this function is only used for debugging purposes.
(define-generic (sal-ast->list ast))

(define-macro (mk-list . slots)
  (let ((mk-body (lambda (slots)
                   `(list ,@(map (lambda (slot)
                                   (if (keyword? slot)
                                     `(sal-ast->list (slot-value ast ,slot))
                                     slot))
                                 slots)))))
    (if (symbol? (car slots))
      `(cons (quote ,(car slots)) ,(mk-body (cdr slots)))
      (mk-body slots))))

(define-macro (mk-slot-list tag slot)
  `(cons (quote ,tag) (map sal-ast->list
                           (slot-value ast ,slot))))

(define-macro (mk-tag-less-list slot)
  `(let ((list (slot-value ast ,slot)))
     (if list
       (map sal-ast->list list)
       #unspecified)))

(define (table->list table)
  (map (lambda (pair)
         (sal-ast->list (cdr pair)))
       (symbol-table->alist table)))

(define-method (sal-ast->list (ast <primitive>))
  (if (mpq? ast)
    (mpq->string ast)
    ast))

(define-method (sal-ast->list (ast <sal-ast>))
  (class-name-of (class-of ast)))

(define-method (sal-ast->list (ast <sal-identifier>))
  (sal-identifier/name ast))

(define-method (sal-ast->list (ast <sal-numeral>))
  (string->symbol (mpq->string (slot-value ast :num))))

(define-method (sal-ast->list (ast <sal-type-param-decl>))
  (sal-ast->list (slot-value ast :id)))

(define (make-var-decl ast)
  `(,(object->symbol (sal-ast->list (slot-value ast :id))) 
    ,(object->symbol (sal-ast->list (slot-value ast :type)))))

(define-method (sal-ast->list (ast <sal-typed-decl>))
  (make-var-decl ast))

(define-method (sal-ast->list (ast <sal-input-state-var-decl>))
  (mk-list input :id :type))
                              
(define-method (sal-ast->list (ast <sal-output-state-var-decl>))
  (mk-list output :id :type))

(define-method (sal-ast->list (ast <sal-global-state-var-decl>))
  (mk-list global :id :type))

(define-method (sal-ast->list (ast <sal-local-state-var-decl>))
  (mk-list local :id :type))

(define-method (sal-ast->list (ast <sal-const-decl>))
  `(,(sal-ast->list (slot-value ast :id))
    ,(sal-ast->list (slot-value ast :type))
    ,(sal-ast->list (slot-value ast :value))))

(define-method (sal-ast->list (ast <sal-context>))
  (cons* 'context 
         (sal-ast->list (slot-value ast :id))
         (mk-tag-less-list :params)
         (map sal-ast->list (queue->list (slot-value ast :declarations)))))

(define-method (sal-ast->list (ast <sal-type-decl>))
  (if (slot-value ast :type)
    (mk-list define-type :id :type)
    (mk-list define-type :id)))

(define-method (sal-ast->list (ast <sal-constant-decl>))
  (if (slot-value ast :value)
    (mk-list define (make-var-decl ast) :value)
    (mk-list define (make-var-decl ast))))

(define-method (sal-ast->list (ast <sal-constructor-decl>))
  (mk-list define (make-var-decl ast) (mk-slot-list accessors :accessors)))

(define-method (sal-ast->list (ast <sal-module-decl>))
  (mk-list define-module :id :parametric-module))

(define-method (sal-ast->list (ast <sal-parametric-module>))
  (mk-list module-template (mk-slot-list args :local-decls) :module))

(define-method (sal-ast->list (ast <sal-assertion-decl>))
  (list (slot-value ast :kind)
        (sal-ast->list (slot-value ast :id))
        (sal-ast->list (slot-value ast :assertion-expr))))

(define-method (sal-ast->list (ast <sal-name-ref>))
  (sal-name-ref/name ast))

(define-method (sal-ast->list (ast <sal-qualified-name-ref>))
  (cond 
   ((sal-name-ref/builtin? ast)
    (sal-name-ref/name ast))
   (else
    (mk-list @ (sal-name-ref/name ast)
               (mk-list (sal-ast/context-name (sal-qualified-name-ref/context-ref ast))
                        (let ((actuals (sal-qualified-name-ref/actuals ast)))
                          (if actuals
                            (map sal-ast->list actuals)
                            #unspecified)))))))

(define-method (sal-ast->list (ast <sal-application>))
  (let ((arg (slot-value ast :arg)))
    (cons (sal-ast->list (slot-value ast :fun))
          (if (instance-of? arg <sal-arg-tuple-literal>)
            (map sal-ast->list (slot-value arg :exprs))
            (list (sal-ast->list arg))))))

(define-method (sal-ast->list (ast <sal-definition-expression>))
  (sal-ast->list (slot-value ast :expr)))

(define-method (sal-ast->list (ast <sal-in>))
  (mk-list in :arg :fun))

(define-method (sal-ast->list (ast <sal-lambda>))
  (mk-list lambda (mk-tag-less-list :local-decls) :expr))

(define-method (sal-ast->list (ast <sal-set-pred-expr>))
  (mk-list set-pred (mk-tag-less-list :local-decls) :expr))

(define-method (sal-ast->list (ast <sal-for-all-expr>))
  (mk-list for-all (mk-tag-less-list :local-decls) :expr))

(define-method (sal-ast->list (ast <sal-exists-expr>))
  (mk-list exists (mk-tag-less-list :local-decls) :expr))

(define-method (sal-ast->list (ast <sal-let-expr>))
  (mk-list let (mk-tag-less-list :local-decls) :expr))

(define-method (sal-ast->list (ast <sal-array-literal>))
  (mk-list mk-array (mk-tag-less-list :local-decls) :expr))

(define-method (sal-ast->list (ast <sal-tuple-literal>))
  (mk-slot-list mk-tuple :exprs))

(define-method (sal-ast->list (ast <sal-record-literal>))
  (mk-slot-list mk-record :entries))

(define-method (sal-ast->list (ast <sal-record-entry>))
  (mk-list :id :expr))

(define-method (sal-ast->list (ast <sal-tuple-selection>))
  (mk-list tuple-selection :target :idx))

(define-method (sal-ast->list (ast <sal-record-selection>))
  (mk-list record-selection :target :idx))

(define-method (sal-ast->list (ast <sal-array-selection>))
  (mk-list array-selection :fun :arg))

(define-method (sal-ast->list (ast <sal-array-update>))
  (mk-list array-update :target :idx :new-value))

(define-method (sal-ast->list (ast <sal-tuple-update>))
  (mk-list tuple-update :target :idx :new-value))

(define-method (sal-ast->list (ast <sal-record-update>))
  (mk-list record-update :target :idx :new-value))

(define-method (sal-ast->list (ast <sal-conditional>))
  (mk-list if :cond-expr :then-expr :else-expr))

(define-method (sal-ast->list (ast <sal-next-operator>))
  (mk-list next (sal-name-ref/name (slot-value ast :name-expr))))

(define-method (sal-ast->list (ast <sal-string-expr>))
  (slot-value ast :string))

(define-method (sal-ast->list (ast <sal-mod-init>))
  (mk-list init :module))

(define-method (sal-ast->list (ast <sal-mod-trans>))
  (mk-list trans :module))

(define-method (sal-ast->list (ast <sal-function-type>))
  (mk-list -> :domain :range))

(define-method (sal-ast->list (ast <sal-tuple-type>))
  (mk-slot-list tuple :types))

(define-method (sal-ast->list (ast <sal-record-type>))
  (mk-slot-list record :fields))

(define-method (sal-ast->list (ast <sal-field>))
  (make-var-decl ast))

(define-method (sal-ast->list (ast <sal-array-type>))
  (mk-list array :domain :range))

(define-method (sal-ast->list (ast <sal-state-type>))
  (mk-list state :module))

(define-method (sal-ast->list (ast <sal-subtype>))
  (mk-list subtype :expr))

(define-method (sal-ast->list (ast <sal-bounded-subtype>))
  (mk-list bounded-subtype :expr :lower :upper))

(define-method (sal-ast->list (ast <sal-subrange>))
  (mk-list subrange :lower :upper))

(define-method (sal-ast->list (ast <sal-scalar-type>))
  (mk-slot-list scalar-type :scalar-elements))

(define-method (sal-ast->list (ast <sal-data-type>))
  (mk-slot-list data-type :constructors))

(define-method (sal-ast->list (ast <sal-simple-definition>))
  (mk-list = :lhs :rhs))

(define-method (sal-ast->list (ast <sal-simple-selection-definition>))
  (mk-list in :lhs :rhs))

(define-method (sal-ast->list (ast <sal-for-all-definition>))
  (mk-list for-all-definition 
           (mk-tag-less-list :local-decls) 
           (mk-tag-less-list :definitions)))

(define-method (sal-ast->list (ast <sal-command-section>))
  (mk-list some-commands 
           (mk-tag-less-list :commands)
           (mk-list else :else-command)))

(define-method (sal-ast->list (ast <sal-guarded-command>))
  (list 'guarded-command
        (sal-ast->list (slot-value ast :guard))
        (map sal-ast->list (slot-value ast :assignments))))

(define-method (sal-ast->list (ast <sal-labeled-command>))
  (mk-list label :label :command))

(define-method (sal-ast->list (ast <sal-multi-command>))
  (mk-list multi-command
           (mk-tag-less-list :local-decls) 
           :command))

(define-method (sal-ast->list (ast <sal-else-command>))
  (mk-slot-list else-command :assignments))

(define-method (sal-ast->list (ast <sal-base-module>))
  (mk-list base-module 
           (mk-tag-less-list :state-vars)
           (mk-slot-list definitions :definitions)
           (mk-slot-list initialization :initialization-definitions)
           :initialization-command-section
           (mk-slot-list transition :transition-definitions)
           :transition-command-section))

(define-method (sal-ast->list (ast <sal-flat-module>))
  (mk-list flat-module
           (mk-tag-less-list :state-vars)
           (mk-list definition :definition)
           (mk-list initialization :initialization)
           (mk-list transition :transition)
           (mk-list skip :skip)
           (mk-list valid-input :valid-input-expr)
           (mk-list valid-state :valid-state-expr)
           (mk-list valid-constant :valid-constant-expr)))

(define-method (sal-ast->list (ast <sal-module-instance>))
  (cons (sal-ast->list (slot-value ast :module-name))
        (map sal-ast->list (slot-value ast :actuals))))

(define-method (sal-ast->list (ast <sal-rename>))
  (mk-list :from-name :to-expr))

(define-method (sal-ast->list (ast <sal-renaming>))
  (mk-list renaming (mk-slot-list renames :renames) :module))

(define-method (sal-ast->list (ast <sal-asynch-composition>))
  (mk-list asynch :module1 :module2))

(define-method (sal-ast->list (ast <sal-synch-composition>))
  (mk-list synch :module1 :module2))

(define-method (sal-ast->list (ast <sal-observer>))
  (mk-list observer :module1 :module2))

(define-method (sal-ast->list (ast <sal-multi-asynch-composition>))
  (mk-list multi-asynch (mk-tag-less-list :local-decls) :module))

(define-method (sal-ast->list (ast <sal-multi-synch-composition>))
  (mk-list multi-synch (mk-tag-less-list :local-decls) :module))

(define-method (sal-ast->list (ast <sal-hiding>))
  (mk-list hiding (mk-tag-less-list :identifiers) :module))

(define-method (sal-ast->list (ast <sal-new-output>))
  (mk-list new-output (mk-tag-less-list :identifiers) :module))

(define-method (sal-ast->list (ast <sal-with-module>))
  (mk-list with (mk-tag-less-list :new-state-vars) :module))

(define-method (sal-ast->list (ast <sal-module-models>))
  (mk-list module-models :module :expr))

(define-method (sal-ast->list (ast <sal-module-implements>))
  (mk-list module-implements :module1 :module2))

(define-method (sal-ast->list (ast <sal-assertion-proposition>))
  (mk-list :op (mk-slot-list arguments :assertion-exprs)))

(define-method (sal-ast->list (ast <sal-labeled-trace-info>))
  (mk-list labeled-trace-info :label :info))

(define-method (sal-ast->list (ast <sal-module-instance-trace-info>))
  (mk-list module-instance-trace-info :info))

(define-method (sal-ast->list (ast <sal-multi-choice-trace-info>))
  (mk-list multi-choice-info (mk-tag-less-list :choice-var-names) (mk-tag-less-list :original-var-names) :info))

(define-method (sal-ast->list (ast <sal-multi-command-choice-trace-info>))
  (mk-list multi-command-choice-info (mk-tag-less-list :choice-var-names) (mk-tag-less-list :original-var-names) :info))

(define-method (sal-ast->list (ast <sal-multi-sequence-trace-info>))
  (mk-list multi-sequence-info :info))

(define-method (sal-ast->list (ast <sal-choice-trace-info>))
  (mk-list choice-trace-info :choice-var-name (mk-tag-less-list :info-list)))

(define-method (sal-ast->list (ast <sal-sequence-trace-info>))
  (mk-slot-list sequence-trace-info :info-list))





           











