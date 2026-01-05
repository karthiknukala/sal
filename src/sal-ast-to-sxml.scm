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

(module sal-ast-to-sxml
        (include "sal.sch")
        (import symbol-table sxml-package gmp-scheme sal-parser-utility sal-context
                queue sal-expression sal-type sal-sxml-support)
        (export (sal-ast->xml ast)
                (sal-ast->sxml ast))
        )

(define (sal-ast->xml ast)
  (let ((sxml (sal-ast->sxml ast)))
    (sxml->xml (sal/replace-builtin-names-with-official-names sxml))))

(define-generic (sal-ast->sxml ast))

(define (process-child value)
  (cond
   ((symbol? value)
    value)
   ((mpq? value)
    (mpq->string value))
   (else
    (sal-ast->sxml value))))

(define (set-place! sxml ast)
  (let ((place (slot-value ast :place)))
    (when place
      (sxml/set-attribute! sxml 'PLACE (sal-place->string place)))))

(define-macro (mk-node tag . slots)
  `(let ((%result (make-simple-sxml-node ,tag
                                         (list ,@(map (lambda (slot-id)
                                                        (cond
                                                         ((keyword? slot-id)
                                                          `(process-child (slot-value ast ,slot-id)))
                                                         (else
                                                          slot-id)))
                                                      slots)))))
     (set-place! %result ast)
     %result))

(define (set-place-using-list! sxml ast-list)
  (when (not (null? ast-list))
    (let ((first-place (slot-value (car ast-list) :place))
          (last-place (slot-value (car (last-pair ast-list)) :place)))
      (when (and first-place last-place)
        (let ((place (make-sal-place-from-places first-place last-place)))
          (sxml/set-attribute! sxml 'PLACE (sal-place->string place)))))))

(define (mk-node-from-list tag ast-list . place-source)
  (let ((result (make-simple-sxml-node tag 
                                       (map (lambda (ast)
                                              (process-child ast))
                                            ast-list))))
    (if (null? place-source)
      (set-place-using-list! result ast-list)
      (set-place! result (car place-source)))
    result))

(define-method (sal-ast->sxml (ast <sal-identifier>))
  (mk-node 'IDENTIFIER :name))

(define-method (sal-ast->sxml (ast <sal-numeral>))
  (mk-node 'NUMERAL :num))

(define-method (sal-ast->sxml (ast <sal-var-decl>))
  (mk-node 'VARDECL :id :type))

(define-method (sal-ast->sxml (ast <sal-idx-var-decl>))
  (mk-node 'INDEXVARDECL :id :type))

(define-method (sal-ast->sxml (ast <sal-let-decl>))
  (mk-node 'LETDECLARATION :id :type :value))

(define-method (sal-ast->sxml (ast <sal-type-decl>))
  (if (slot-value ast :type)
    (mk-node 'TYPEDECLARATION :id :type)
    (mk-node 'TYPEDECLARATION :id)))

(define-method (sal-ast->sxml (ast <sal-constant-decl>))
  (if (slot-value ast :value)
    (mk-node 'CONSTANTDECLARATION :id :type :value)
    (mk-node 'CONSTANTDECLARATION :id :type)))

(define-method (sal-ast->sxml (ast <sal-assertion-decl>))
  (mk-node 'ASSERTIONDECLARATION :id (mk-node 'ASSERTIONFORM :kind) :assertion-expr))

(define-method (sal-ast->sxml (ast <sal-module-decl>))
  (mk-node 'MODULEDECLARATION :id
           (mk-node-from-list 'VARDECLS (slot-value (slot-value ast :parametric-module) :local-decls))
           :parametric-module))

(define-method (sal-ast->sxml (ast <sal-parametric-module>))
  (process-child (slot-value ast :module)))

(define (make-sxml-name tag ast)
  (mk-node tag (sal-name-ref/name ast)))

(define-method (sal-ast->sxml (ast <sal-name-expr>))
  (make-sxml-name 'NAMEEXPR ast))

(define-method (sal-ast->sxml (ast <sal-type-name>))
  (make-sxml-name 'TYPENAME ast))

(define-method (sal-ast->sxml (ast <sal-module-name>))
  (make-sxml-name 'MODULENAME ast))

(define (make-sxml-qualified-name tag simple-tag ast)
  (cond
   ((sal-name-ref/builtin? ast)
    (make-sxml-name simple-tag ast))
   (else
    (mk-node tag
             (mk-node 'IDENTIFIER (sal-name-ref/name ast))
             (cond
              ((null? (slot-value ast :actuals))
               (mk-node 'CONTEXTNAME 
                        (mk-node 'IDENTIFIER (sal-context/name (slot-value ast :context-ref)))))
              (else
               (mk-node 'CONTEXTNAME 
                        (mk-node 'IDENTIFIER (sal-context/name (slot-value ast :context-ref)))
                        (mk-node-from-list 'ACTUALPARAMETERS
                                           (slot-value ast :actuals)))))))))

(define-method (sal-ast->sxml (ast <sal-qualified-name-expr>))
  (make-sxml-qualified-name 'QUALIFIEDNAMEEXPR 'NAMEEXPR ast))

(define-method (sal-ast->sxml (ast <sal-qualified-type-name>))
  (make-sxml-qualified-name 'QUALIFIEDTYPENAME 'TYPENAME ast))
                    
(define-method (sal-ast->sxml (ast <sal-qualified-module-name>))
  (make-sxml-qualified-name 'QUALIFIEDMODULENAME 'MODULENAME ast))

(define (make-sxml-context-parameters ast)
  (cond
   ((null? (slot-value ast :params))
    (mk-node 'PARAMETERS))
   (else
    (mk-node-from-list 'PARAMETERS (slot-value ast :params)))))

(define-method (sal-ast->sxml (ast <sal-context>))
  (mk-node 'CONTEXT 
           :id 
           (make-sxml-context-parameters ast)
           (mk-node-from-list 'CONTEXTBODY (queue->list (slot-value ast :declarations)))))

(define-method (sal-ast->sxml (ast <sal-type-param-decl>))
  (mk-node 'TYPEDECL :id))

(define (make-sxml-accessor accessor-name)
  (let* ((accessor-decl (slot-value accessor-name :decl))
         (accessor-id (slot-value accessor-decl :id))
         (accessor-type (sal-function-type/range (slot-value accessor-decl :type)))
         (sxml (make-simple-sxml-node 'ACCESSOR
                                      (list (process-child accessor-id)
                                            (process-child accessor-type)))))
    (set-place! sxml accessor-decl)
    sxml))

(define (make-sxml-constructors constructor-list)
  (map (lambda (constructor-name)
         (let* ((constructor-decl (slot-value constructor-name :decl))
                (constructor-id (slot-value constructor-decl :id))
                (sxml (make-simple-sxml-node 'CONSTRUCTOR
                                             (cons (process-child constructor-id)
                                                   (map make-sxml-accessor (slot-value constructor-decl :accessors))))))
           (set-place! sxml constructor-decl)
           sxml))
       constructor-list))

(define-method (sal-ast->sxml (ast <sal-data-type>))
  (let ((sxml (make-simple-sxml-node 'DATATYPE
                                     (make-sxml-constructors (slot-value ast :constructors)))))
    (set-place! sxml ast)
    sxml))

(define (make-sxml-scalar-elements scalar-name-list)
  (map (lambda (scalar-name)
         (let ((sxml (make-simple-sxml-node 'SCALARELEMENT
                                            (list (sal-name-ref/name scalar-name)))))
           (set-place! sxml scalar-name)
           sxml))
       scalar-name-list))
           
(define-method (sal-ast->sxml (ast <sal-scalar-type>))
  (let ((sxml (make-simple-sxml-node 'SCALARTYPE
                                     (make-sxml-scalar-elements (slot-value ast :scalar-elements)))))
    (set-place! sxml ast)
    sxml))
    

(define-method (sal-ast->sxml (ast <sal-function-type>))
  (mk-node 'FUNCTIONTYPE :domain :range))

(define-method (sal-ast->sxml (ast <sal-tuple-type>))
  (mk-node-from-list 'TUPLETYPE (slot-value ast :types) ast))

(define-method (sal-ast->sxml (ast <sal-record-type>))
  (mk-node-from-list 'RECORDTYPE (slot-value ast :fields) ast))

(define-method (sal-ast->sxml (ast <sal-field>))
  (mk-node 'FIELDDECLARATION :id :type))

(define-method (sal-ast->sxml (ast <sal-subtype>))
  (mk-node 'SUBTYPE :expr))

(define-method (sal-ast->sxml (ast <sal-state-type>))
  (mk-node 'STATETYPE :module))

(define-method (sal-ast->sxml (ast <sal-array-type>))
  (mk-node 'ARRAYTYPE :domain :range))

(define-method (sal-ast->sxml (ast <sal-subrange>))
  (mk-node 'SUBRANGE :lower :upper))

(define-method (sal-ast->sxml (ast <sal-next-operator>))
  (mk-node 'NEXTOPERATOR :name-expr))

(define-method (sal-ast->sxml (ast <sal-application>))
  (mk-node 'APPLICATION :fun :arg))

(define (make-sxml-binary-app-from-nary-op op ast)
  (let ((exprs (sal-application/argument-list ast)))
    [assert (exprs) (not (null? exprs))]
    (let loop ((result (car exprs))
               (rest (cdr exprs)))
      (if (null? rest)
        result
        (let* ((curr (car rest))
               (new-result (mk-node 'APPLICATION
                                    (mk-node 'NAMEEXPR op)
                                    (mk-node-from-list 'TUPLELITERAL
                                                       (list result
                                                             curr)))))
          (loop new-result (cdr rest)))))))

(define-method (sal-ast->sxml (ast <sal-and>))
  (make-sxml-binary-app-from-nary-op 'and ast))

(define-method (sal-ast->sxml (ast <sal-or>))
  (make-sxml-binary-app-from-nary-op 'or ast))

(define-method (sal-ast->sxml (ast <sal-add>))
  (make-sxml-binary-app-from-nary-op '+ ast))

(define-method (sal-ast->sxml (ast <sal-mul>))
  (make-sxml-binary-app-from-nary-op '- ast))

(define-method (sal-ast->sxml (ast <sal-array-selection>))
  (mk-node 'ARRAYSELECTION :fun :arg))

(define-method (sal-ast->sxml (ast <sal-record-selection>))
  (mk-node 'RECORDSELECTION :target :idx))

(define-method (sal-ast->sxml (ast <sal-tuple-selection>))
  (mk-node 'TUPLESELECTION :target :idx))

(define-method (sal-ast->sxml (ast <sal-tuple-literal>))
  (mk-node-from-list 'TUPLELITERAL (slot-value ast :exprs) ast))

(define-method (sal-ast->sxml (ast <sal-record-literal>))
  (mk-node-from-list 'RECORDLITERAL (slot-value ast :entries) ast))

(define-method (sal-ast->sxml (ast <sal-record-entry>))
  (mk-node 'RECORDENTRY :id :expr))

(define-method (sal-ast->sxml (ast <sal-array-literal>))
  (let ((var-decl (car (slot-value ast :local-decls))))
    (mk-node 'ARRAYLITERAL
             (mk-node 'INDEXVARDECL
                      (process-child (slot-value var-decl :id))
                      (process-child (slot-value var-decl :type)))
             :expr)))

(define-method (sal-ast->sxml (ast <sal-lambda>))
  (mk-node 'LAMBDAABSTRACTION
           (mk-node-from-list 'VARDECLS (slot-value ast :local-decls))
           :expr))

(define (make-sxml-quantified-expr quantifier ast)
  (mk-node 'QUANTIFIEDEXPRESSION
           (mk-node 'QUANTIFIER quantifier)
           (mk-node-from-list 'VARDECLS (slot-value ast :local-decls))
           :expr))

(define-method (sal-ast->sxml (ast <sal-exists-expr>))
  (make-sxml-quantified-expr 'EXISTS ast))

(define-method (sal-ast->sxml (ast <sal-for-all-expr>))
  (make-sxml-quantified-expr 'FORALL ast))

(define-method (sal-ast->sxml (ast <sal-set-pred-expr>))
  (let ((var-decl (car (slot-value ast :local-decls))))
    (mk-node 'SETPREDEXPRESSION
             (slot-value var-decl :id)
             (slot-value var-decl :type)
             :expr)))

(define-method (sal-ast->sxml (ast <sal-conditional>))
  (mk-node 'CONDITIONAL :cond-expr :then-expr :else-expr))

(define-method (sal-ast->sxml (ast <sal-mod-init>))
  (mk-node 'MODINIT :module))

(define-method (sal-ast->sxml (ast <sal-mod-trans>))
  (mk-node 'MODTRANS :module))

(define-method (sal-ast->sxml (ast <sal-string-expr>))
  (mk-node 'STRINGEXPR :string))

(define-method (sal-ast->sxml (ast <sal-let-decl>))
  (mk-node 'LETDECLARATION :id :type :value))

(define-method (sal-ast->sxml (ast <sal-let-expr>))
  (mk-node 'LETEXPRESSION
           (mk-node-from-list 'LETDECLARATIONS (slot-value ast :local-decls))
           :expr))

(define-method (sal-ast->sxml (ast <sal-input-state-var-decl>))
  (mk-node 'INPUTDECL (process-child (change-ast-class ast <sal-var-decl>))))

(define-method (sal-ast->sxml (ast <sal-output-state-var-decl>))
  (mk-node 'OUTPUTDECL (process-child (change-ast-class ast <sal-var-decl>))))

(define-method (sal-ast->sxml (ast <sal-local-state-var-decl>))
  (mk-node 'LOCALDECL (process-child (change-ast-class ast <sal-var-decl>))))

(define-method (sal-ast->sxml (ast <sal-global-state-var-decl>))
  (mk-node 'GLOBALDECL (process-child (change-ast-class ast <sal-var-decl>))))

(define (make-sxml-from-cmd-section tag def-list cmd-section)
  (if (and (null? def-list) (not cmd-section))
    '()
    (list (mk-node-from-list tag (append def-list
                                         (if cmd-section
                                           (list cmd-section)
                                           '()))))))
     
(define-method (sal-ast->sxml (ast <sal-base-module>))
  (let ((sxml (make-simple-sxml-node 
               'BASEMODULE
               (append (map sal-ast->sxml (slot-value ast :state-vars))
                       (if (null? (slot-value ast :definitions))
                         '()
                         (mk-node-from-list 'DEFDECL (slot-value ast :definitions)))
                       (make-sxml-from-cmd-section 'INITDECL 
                                                   (slot-value ast :initialization-definitions)
                                                   (slot-value ast :initialization-command-section))
                       (make-sxml-from-cmd-section 'TRANSDECL
                                                   (slot-value ast :transition-definitions)
                                                   (slot-value ast :transition-command-section))))))
    (set-place! sxml ast)
    sxml))

(define-method (sal-ast->sxml (ast <sal-simple-definition>))
  (mk-node 'SIMPLEDEFINITION :lhs (mk-node 'RHSEXPRESSION :rhs)))

(define-method (sal-ast->sxml (ast <sal-simple-selection-definition>))
  (mk-node 'SIMPLEDEFINITION :lhs (mk-node 'RHSSELECTION) :rhs))

(define-method (sal-ast->sxml (ast <sal-for-all-definition>))
  (let ((sxml (make-simple-sxml-node
               'FORALLDEFINITION
               (append (mk-node-from-list 'VARDECLS (slot-value ast :local-decls))
                       (map process-child (slot-value ast :definitions))))))
    (set-place! sxml ast)
    sxml))

(define-method (sal-ast->sxml (ast <sal-labeled-command>))
  (mk-node 'LABELEDCOMMAND 
           (mk-node 'LABEL (sal-identifier/name (slot-value ast :label)))
           :command))

(define-method (sal-ast->sxml (ast <sal-guarded-command>))
  (mk-node 'GUARDEDCOMMAND 
           (mk-node 'GUARD :guard)
           (mk-node-from-list 'ASSIGNMENTS (slot-value ast :assignments))))

(define-method (sal-ast->sxml (ast <sal-else-command>))
  (mk-node 'ELSECOMMAND
           (mk-node-from-list 'ASSIGNMENTS (slot-value ast :assignments))))

(define-method (sal-ast->sxml (ast <sal-multi-command>))
  (mk-node 'MULTICOMMAND
           (mk-node-from-list 'VARDECLS (slot-value ast :local-decls))
           :command))

(define-method (sal-ast->sxml (ast <sal-command-section>))
  (mk-node-from-list 'SOMECOMMANDS
                     (append (slot-value ast :commands)
                             (if (slot-value ast :else-command)
                               (list (slot-value ast :else-command))
                               '()))))

(define-method (sal-ast->sxml (ast <sal-synch-composition>))
  (mk-node 'SYNCHRONOUSCOMPOSITION :module1 :module2))

(define-method (sal-ast->sxml (ast <sal-asynch-composition>))
  (mk-node 'ASYNCHRONOUSCOMPOSITION :module1 :module2))

(define (make-sxml-multi-composition tag ast)
  (let ((var-decl (car (slot-value ast :local-decls))))
    (mk-node tag
             (mk-node 'INDEXVARDECL
                      (process-child (slot-value var-decl :id))
                      (process-child (slot-value var-decl :type)))
             :module)))

(define-method (sal-ast->sxml (ast <sal-multi-synch-composition>))
  (make-sxml-multi-composition 'MULTISYNCHRONOUS ast))

(define-method (sal-ast->sxml (ast <sal-multi-asynch-composition>))
  (make-sxml-multi-composition 'MULTIASYNCHRONOUS ast))

(define-method (sal-ast->sxml (ast <sal-observer>))
  (make-sxml-multi-composition 'OBSERVEMODULE ast))

(define (make-sxml-for-module-org tag ast)
  (mk-node tag
           (mk-node-from-list 'IDENTIFIERS (slot-value ast :identifiers))
           :module))

(define-method (sal-ast->sxml (ast <sal-hiding>))
  (make-sxml-for-module-org 'HIDING ast))

(define-method (sal-ast->sxml (ast <sal-new-output>))
  (make-sxml-for-module-org 'NEWOUTPUT ast))

(define-method (sal-ast->sxml (ast <sal-rename>))
  (mk-node 'RENAME 
           (mk-node 'NAMEEXPR
                    (sal-identifier/name (slot-value ast :from-name)))
           :to-expr))

(define-method (sal-ast->sxml (ast <sal-renaming>))
  (mk-node 'RENAMING
           (mk-node-from-list 'RENAMES (slot-value ast :renames))
           :module))

(define-method (sal-ast->sxml (ast <sal-with-module>))
  (mk-node 'WITHMODULE
           (mk-node-from-list 'NEWVARDECLS (slot-value ast :new-state-vars))
           :module))

(define-method (sal-ast->sxml (ast <sal-module-instance>))
  (mk-node 'MODULEINSTANCE
           :module-name
           (mk-node-from-list 'MODULEACTUALS (slot-value ast :actuals))))

(define-macro (make-sxml-update-expr ast sel-class)
  `(mk-node 'UPDATEEXPRESSION
            :target
            (process-child (make-ast-instance ,sel-class ,ast
                                              :target (slot-value ,ast :target)
                                              :idx (slot-value ,ast :idx)))
            :new-value))

(define-method (sal-ast->sxml (ast <sal-record-update>))
  (make-sxml-update-expr ast <sal-record-selection>))
                             
(define-method (sal-ast->sxml (ast <sal-tuple-update>))
  (make-sxml-update-expr ast <sal-tuple-selection>))
                             
(define-method (sal-ast->sxml (ast <sal-array-update>))
  (mk-node 'UPDATEEXPRESSION
           :target
           (process-child (make-ast-instance <sal-array-selection> ast
                                             :fun (slot-value ast :target)
                                             :arg (slot-value ast :idx)))
           :new-value))

(define-method (sal-ast->sxml (ast <sal-module-models>))
  (mk-node 'MODULEMODELS :module :expr))

(define-method (sal-ast->sxml (ast <sal-module-implements>))
  (mk-node 'MODULEIMPLEMENTS :module :expr))

(define-method (sal-ast->sxml (ast <sal-assertion-proposition>))
  (let ((sxml (make-simple-sxml-node 'ASSERTIONPROPOSITION 
                                     (cons (mk-node 'ASSERTIONOPERATOR :op)
                                           (map process-child (slot-value ast :assertion-exprs))))))
    (set-place! sxml ast)
    sxml))

(define (make-sxml-quantified-assertion quantifier ast)
  (mk-node 'QUANTIFIEDASSERTION
           (mk-node 'QUANTIFIER quantifier)
           (mk-node-from-list 'VARDECLS (slot-value ast :local-decls))
           :assertion-expr))

(define-method (sal-ast->sxml (ast <sal-ast>))
  (sign-unsupported-feature ast "This SAL AST cannot be converted to SXML."))
