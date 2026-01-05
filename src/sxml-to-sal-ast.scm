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

(module sxml-to-sal-ast
        (include "sal.sch")
        (include "sxml-package.sch")
        (import queue symbol-table symbol-set sal-context sal-environment queue sal-ast-attributes
                gmp-scheme trace sal-importer sal-sxml-support sal-module sal-type sal-type-checker
                sal-ast-list sal-expression unique-names sal-decls runtime)
        (export (convert-context! sxml context)
                (convert-top-expr context sxml-expr)
                (convert-top-type context sxml-type)
                (convert-top-module context sxml-module)
                (convert-top-assertion-name context sxml)
                (convert-expr context sym-tab sxml)
                (process-import context sxml-import-def))
        )

(define (sxml/place sxml)
  (sxml/attribute sxml 'PLACE))

(define-macro (make-ast-node class ctx sxml . svsv)
  `(make-instance ,class 
                  :context ,ctx
                  :place (sxml/place ,sxml)
                  ,@svsv))

(define (make-identifier-node ctx sxml name)
  (make-ast-node <sal-identifier> ctx sxml :name name))

(define (sign-redeclaration-of sxml name)
  (sign-source-error sxml "Redeclaration of \"~a\"." name))

(define (check-if-already-defined-constant context sxml name)
  (when (symbol-table/lookup (slot-value context :constant-declarations) name)
    (sign-redeclaration-of sxml name)))

(define (external-name context kind name node-with-place)
  (let ((found? #f)
        (place-info (sxml/place node-with-place)))
    (for-each (lambda (importer)
                (let ((result (importer kind name context place-info)))
                  (when result
                    (if found?
                      (let ((ctx1 (sal-context/name (sal-qualified-name-ref/defined-at found?)))
                            (ctx2 (sal-context/name (sal-qualified-name-ref/defined-at result))))
                        (unless (eq? ctx1 ctx2)
                          (sign-source-error node-with-place "Ambiguous name reference, \"~a\" is defined at ~a and ~a." 
                                             name ctx1 ctx2)))
                      (set! found? result)))))
              (queue->list (slot-value context :importers)))
    found?))

(define (add-constant-declaration context name decl)
  (symbol-table/add! (slot-value context :constant-declarations) name decl)
  (queue/insert! (slot-value context :declarations) decl))

(define (update-symbol-table-with-decl sym-tab decl)
  [assert (decl) (instance-of? decl <sal-decl>)]
  (symbol-table/add sym-tab (sal-decl/name decl) decl))

(define (update-symbol-table-with-decl-list sym-tab decl-list)
  (fold-left update-symbol-table-with-decl sym-tab decl-list))

(define (convert-identifier context sxml)
  [assert (sxml) (sxml/tag-equals? sxml 'IDENTIFIER)]
  (make-ast-node <sal-identifier> context sxml :name (sxml/first-child sxml)))

(define (set-context-name-attribute! sxml ctx-name)
  (sxml/traverse (lambda (n)
                   (when (sxml-node? n)
                     (sxml/set-attribute! n 'CONTEXT-NAME ctx-name)))
                 sxml))

(define (convert-context! sxml context)
  ;; (sxml/pp sxml)
  (sxml/match-or-fail sxml
    ((CONTEXT (as (IDENTIFIER ?name) ?id) 
              ?parameters
              (CONTEXTBODY . ?declaration-list))
     (verbose-message 1 "creating abstract syntax tree for context \"~a\"..." name)
     (display-runtime 2 "  ast generation time: ~a secs"
       (lambda ()
         (set-slot-value! context :place (sxml/place sxml))
         (set-slot-value! context :id (convert-identifier context id))
         (trace 'context "Importing context ~a" (sal-decl/name context))
         (set-context-name-attribute! sxml (sal-decl/name context))
         (convert-parameters context parameters)
         (convert-context-body context declaration-list))
       :sxml-to-ast-time))))

(define (convert-top-expr context sxml-expr)
  (convert-expr context (make-symbol-table) sxml-expr))

(define (convert-top-type context sxml-type)
  (convert-type context (make-symbol-table) sxml-type))

(define (convert-top-module context sxml-module)
  (convert-module context (make-symbol-table) sxml-module))

(define (convert-top-assertion-name context sxml)
  (case (sxml/tag sxml)
    ((QUALIFIEDASSERTIONNAME)
     (convert-qualified-assertion-name context (make-symbol-table) sxml))
    (else
     (internal-error))))

(define (add-ctx-param! context param)
  (set-slot-value! context :params (append (slot-value context :params) (list param))))

(define (convert-parameters context parameters)
  (sxml/match-or-fail parameters
    ((PARAMETERS)
     ;; do nothing
     #unspecified)
    ((PARAMETERS . ?param-list)
     (convert-param-decl-list context param-list)))
  (sal-context/initialize-internal-actuals! context)
  ;; convert types in variable declarations
  (for-each (lambda (decl)
              (when (instance-of? decl <sal-var-decl>)
                (let ((var-decl decl))
                  (set-slot-value! var-decl :type (convert-top-type context (slot-value var-decl :type))))))
            (slot-value context :params)))

(define (convert-context-body context declaration-list)
  (for-each (cut convert-declaration context <>) declaration-list))

(define (convert-param-decl-list context decl-list)
  (for-each (lambda (decl)
              (sxml/match-or-fail decl
                ((TYPEDECL ?id)
                 (let ((new-ast (make-ast-node <sal-type-param-decl> context decl 
                                               :id (convert-identifier context id))))
                   (add-ctx-param! context new-ast)))
                ((VARDECL . ?-)
                 (add-ctx-param! context (convert-var-param-decl context (make-symbol-table) decl)))))
            decl-list))
    
(define (convert-var-decl context sym-tab var-decl)
  (sxml/match-or-fail var-decl
    ((VARDECL ?id ?type)
     (make-ast-node <sal-var-decl>
                    context var-decl
                    :id (convert-identifier context id)
                    :type (convert-type context sym-tab type)))))

(define (convert-var-param-decl context sym-tab var-decl)
  (sxml/match-or-fail var-decl
    ((VARDECL ?id ?type)
     (make-ast-node <sal-var-param-decl>
                    context var-decl
                    :id (convert-identifier context id)
                    :type type)))) ;; do not convert type here!

(define (convert-state-var-decl context sym-tab var-decl state-var-class)
  (sxml/match-or-fail var-decl
    ((VARDECL (as (IDENTIFIER ?name) ?id) ?type)
;   In the current implementation, I'm not implementing the default `running' variable
;      (when (eq? name (sal/basemodule-running-var-name))
;        (sign-source-error var-decl "Invalid state variable declaration, `~a' is a builtin local variable of every SAL base module."
;                           (sal/basemodule-running-var-name)))
     (make-ast-node state-var-class
                    context var-decl
                    :id (convert-identifier context id)
                    :type (convert-type context sym-tab type)))))
  
(define (convert-idx-var-decl context sym-tab var-decl)
  (quick-change-class! (convert-var-decl context sym-tab var-decl) <sal-idx-var-decl>))

(define (convert-let-decl context sym-tab var-decl)
  (sxml/match-or-fail var-decl
    ((LETDECLARATION ?id ?type ?expr)
     (make-ast-node <sal-let-decl>
                    context var-decl
                    :id (convert-identifier context id)
                    :type (convert-type context sym-tab type)
                    :value (convert-expr context sym-tab expr)))))

(define (convert-type context sym-tab type)
  (case (sxml/tag type)
    ((TYPENAME)
     (convert-type-name context sym-tab type))
    ((QUALIFIEDTYPENAME)
     (convert-qualified-type-name context sym-tab type))
    ((TUPLETYPE)
     (convert-tuple-type context sym-tab type <sal-tuple-type>))
    ((RECORDTYPE)
     (convert-record-type context sym-tab type))
    ((ARRAYTYPE)
     (convert-array-type context sym-tab type))
    ((FUNCTIONTYPE)
     (convert-function-type context sym-tab type))
    ((SUBTYPE)
     (convert-subtype context sym-tab type))
    ((SUBRANGE)
     (convert-subrange context sym-tab type))
    ((STATETYPE)
     (convert-state-type context sym-tab type))
    ((VARDECL)
     (sign-unsupported-feature type "SALenv does not support dependent types."))
    (else
     (sign-source-error type "Invalid abstract syntax tree, tag is ~a." (sxml/tag type)))))

(define (convert-domain-type context sym-tab type)
  (case (sxml/tag type)
    ((TUPLETYPE)
     (convert-tuple-type context sym-tab type <sal-domain-tuple-type>))
    (else
     (convert-type context sym-tab type))))

(define (convert-type-def context type-decl type-def)
  (case (sxml/tag type-def)
    ((SCALARTYPE)
     (convert-scalar-type context type-decl type-def))
    ((SCALARSET)
     (convert-scalar-set-type context type-decl type-def))
    ((RINGSET)
     (convert-ring-set-type context type-decl type-def))
    ((DATATYPE)
     (convert-data-type context type-decl type-def))
    (else
     (convert-type context (make-symbol-table) type-def))))

(define (create-internal-name-ref context sxml decl)
  (make-ast-node (sal-decl/name-class decl) context sxml
                 :decl decl
                 :context-ref context
                 :actuals (slot-value context :internal-actuals)))
                 
(define (create-builtin-name-ref context sxml class decl)
  (make-ast-node class context sxml
                 :decl decl
                 :context-ref (sal-env/prelude (sal-ast/sal-env context))
                 :actuals '()))

(define (convert-type-name context sym-tab type)
  (sxml/match-or-fail type
    ((TYPENAME ?name)
     (cond 
      ((sal-context/type-declaration context name)
       =>
       (lambda (decl)
         (create-internal-name-ref context type decl)))
      ((sal-context/param context name) =>
       (lambda (decl)
         (unless (instance-of? decl <sal-type-param-decl>)
           (sign-source-error type "Context parameter `~a' is not a type name." name))
         (make-ast-node <sal-type-param-name> context type
                        :decl decl)))
      ((external-name context 'type name type) =>
       identity)
      ((sal-env/builtin-type-decl (sal-ast/sal-env context) name) =>
        (lambda (decl)
          (create-builtin-name-ref context type (sal-decl/name-class decl) decl)))
      (else 
       (sign-source-error type "Unknown type \"~a\"." name))))))

(define (convert-qualified-name-ref context sym-tab type get-decl-proc kind-str)
  (sxml/match-or-fail type
    ((?- (as (IDENTIFIER ?name) ?id) ?ctx-name-node)
     (let* ((ctx-name-decl (convert-context-name context sym-tab ctx-name-node))
            (ctx-ref (slot-value ctx-name-decl :context-ref))
            (actuals (slot-value ctx-name-decl :actuals)))
       (cond
        ((get-decl-proc ctx-ref name) =>
         (lambda (decl)
           (make-ast-node (sal-decl/name-class decl) context type 
                          :decl decl
                          :context-ref ctx-ref
                          :actuals actuals)))
        (else 
         (sign-source-error type "Unknown ~a \"~a\" in the context \"~a\"." kind-str name (sal-context/name ctx-ref))))))))

(define (convert-qualified-type-name context sym-tab sxml)
  (convert-qualified-name-ref context sym-tab sxml sal-context/type-declaration "type"))

(define (convert-qualified-name-expr context sym-tab sxml)
  (convert-qualified-name-ref context sym-tab sxml sal-context/constant-declaration "constant"))

(define (convert-qualified-module-name context sym-tab sxml)
  (convert-qualified-name-ref context sym-tab sxml sal-context/module-declaration "module"))

(define (convert-qualified-assertion-name context sym-tab sxml)
  (convert-qualified-name-ref context sym-tab sxml sal-context/assertion-declaration "assertion"))

(define (convert-tuple-type context sym-tab type tuple-type-class)
  (sxml/match-or-fail type
    ((TUPLETYPE . ?type-list)
     (make-ast-node tuple-type-class context type 
                    :types (map (cut convert-type context sym-tab <>) type-list)))))

(define (convert-record-type context sym-tab type)
  (sxml/match-or-fail type
    ((RECORDTYPE . ?field-list)
     (let ((field-list (sort field-list 
                             (lambda (field1 field2)
                               (sxml/match-or-fail field1
                                 ((FIELDDECLARATION (IDENTIFIER ?id1) ?-)
                                  (sxml/match-or-fail field2
                                    ((FIELDDECLARATION (IDENTIFIER ?id2) ?-)
                                     (string<? (symbol->string id1)
                                               (symbol->string id2))))))))))
       (make-ast-node <sal-record-type> context type
                      :fields (map (cut convert-field context sym-tab <>) field-list))))))

(define (convert-field context sym-tab field)
  (sxml/match-or-fail field
    ((FIELDDECLARATION ?id ?type)
     (make-ast-node <sal-field> context field
                    :id (convert-identifier context id)
                    :type (convert-type context sym-tab type)))))

(define (convert-array-type context sym-tab type)
  (sxml/match-or-fail type
    ((ARRAYTYPE ?idx-type ?elem-type)
     (make-ast-node <sal-array-type> context type
                    :domain (convert-type context sym-tab idx-type)
                    :range (convert-type context sym-tab elem-type)))))

(define (convert-function-type context sym-tab type)
  (sxml/match-or-fail type
    ((FUNCTIONTYPE ?arg-type ?return-type)
     (make-ast-node <sal-function-type> context type
                    :domain (convert-domain-type context sym-tab arg-type)
                    :range (convert-type context sym-tab return-type)))))

(define (convert-subrange context sym-tab type)
  (sxml/match-or-fail type
    ((SUBRANGE ?lower ?upper)
     (let* ((lower (convert-expr context sym-tab lower))
            (upper (convert-expr context sym-tab upper))
            (place-provider (make-ast-node <sal-ast> context type)))
       (make-sal-subrange lower upper place-provider)))))

(define (convert-subtype context sym-tab type)
  (sxml/match-or-fail type
    ((SUBTYPE ?expr)
     (make-ast-node <sal-subtype> context type
                    :expr (convert-expr context sym-tab expr)))))

(define (convert-state-type context sym-tab type)
  (sxml/match-or-fail type
    ((STATETYPE ?module)
     (let ((module (convert-module context sym-tab module)))
       (sal-module/type module)))))

(define (mk-subrange-with-n-elements expr context place-provider)
  (let* ((lower (make-sal-numeral 0 place-provider))
         (one (make-sal-numeral 1 place-provider))
         (upper (make-sal-builtin-application <sal-sub> place-provider
                                              (convert-expr context (make-symbol-table) expr)
                                              one)))
    (make-sal-subrange lower upper one)))

(define (convert-scalar-set-type context type-decl type-def)
  (sxml/match-or-fail type-def
    ((SCALARSET ?expr)
     (let ((place-provider (make-ast-node <sal-ast> context type-def)))
       (quick-change-class! (mk-subrange-with-n-elements expr context place-provider)
                            <sal-scalar-set-type>)))))

(define (convert-ring-set-type context type-decl type-def)
  (sxml/match-or-fail type-def
    ((RINGSET ?expr)
     (let ((place-provider (make-ast-node <sal-ast> context type-def)))
       (quick-change-class! (mk-subrange-with-n-elements expr context place-provider)
                            <sal-ring-set-type>)))))

(define (convert-scalar-type context scalar-type-decl scalar-type)
  (sxml/match-or-fail scalar-type
    ((SCALARTYPE . ?scalar-element-list)
     (make-ast-node <sal-scalar-type> context scalar-type
                    :scalar-elements (map (cut convert-scalar-element context scalar-type-decl <>) scalar-element-list)))))

(define (convert-scalar-element context scalar-type-decl scalar-element)
  (sxml/match-or-fail scalar-element
    ((SCALARELEMENT ?name)
     (check-if-already-defined-constant context scalar-element name)
     (let* ((id-ast (make-identifier-node context scalar-element name))
            (implicit-decl (make-ast-node <sal-scalar-element-decl> context scalar-element
                                 :id id-ast
                                 :type (create-internal-name-ref context scalar-element scalar-type-decl)
                                 :scalar-type-decl scalar-type-decl)))
       (sxml-symbol-attribute->ast-attribute! scalar-element 'name-class implicit-decl :name-class)
       (add-constant-declaration context name implicit-decl)
       (create-internal-name-ref context scalar-element implicit-decl)))))

(define (convert-data-type context data-type-decl data-type)
  (sxml/match-or-fail data-type
    ((DATATYPE . ?constructor-list)
     (make-ast-node <sal-data-type> context data-type
                    :constructors (map (cut convert-constructor context data-type-decl <>) constructor-list)))))

(define (convert-constructor context data-type-decl constructor)
  (sxml/match-or-fail constructor
    ((CONSTRUCTOR (as (IDENTIFIER ?name) ?id) . ?accessor-list)
     (let ((recognizer-name (symbol-append name '?)))
       (check-if-already-defined-constant context constructor name)
       (check-if-already-defined-constant context constructor recognizer-name)
       (let* ((id-constructor-ast (convert-identifier context id))
              (id-recognizer-ast (make-identifier-node context id recognizer-name))
              (data-type-type-name (create-internal-name-ref context constructor data-type-decl))
              (constructor-implicit-decl (make-ast-node <sal-constructor-decl> context constructor
                                            :id id-constructor-ast
                                            :data-type-decl data-type-decl)))
         (sxml-symbol-attribute->ast-attribute! constructor 'name-class constructor-implicit-decl :name-class)
         (let* ((result (create-internal-name-ref context constructor constructor-implicit-decl))
                (recognizer-implicit-decl (make-ast-node <sal-recognizer-decl> context constructor
                                                         :id id-recognizer-ast
                                                         :type (make-ast-node <sal-function-type> context constructor
                                                                              :domain data-type-type-name
                                                                              :range (make-sal-builtin-name <sal-bool-type> id-recognizer-ast))
                                                         :constructor-decl constructor-implicit-decl))
                (pre-accessor-list (map (lambda (accessor)
                                          (multiple-value-bind
                                              (accessor-name accessor-type)
                                              (convert-accessor context constructor-implicit-decl accessor)
                                            (cons accessor-name accessor-type)))
                                        accessor-list))
                (accessor-names (map car pre-accessor-list))
                (accessor-types (map cdr pre-accessor-list)))
           (sxml-symbol-attribute->ast-attribute! constructor 'recognizer-name-class recognizer-implicit-decl :name-class)
           (set-slot-value! constructor-implicit-decl :accessors accessor-names)
           (set-slot-value! constructor-implicit-decl :type
                            (if (null? accessor-types)
                              data-type-type-name
                              (make-ast-node <sal-function-type> context constructor
                                             :domain (cond 
                                                      ((null? (cdr accessor-types))
                                                       (car accessor-types))
                                                      (else (make-ast-node <sal-domain-tuple-type> context constructor
                                                                           :types accessor-types)))
                                             :range data-type-type-name)))
           (set-slot-value! constructor-implicit-decl :recognizer-decl recognizer-implicit-decl)
           (add-constant-declaration context name constructor-implicit-decl)
           (add-constant-declaration context recognizer-name recognizer-implicit-decl)
           result))))))

(define (convert-accessor context constructor-decl accessor)
  (sxml/match-or-fail accessor
    ((ACCESSOR (as (IDENTIFIER ?name) ?id) ?type)
     (check-if-already-defined-constant context accessor name)
     (let* ((type-ast (convert-type context (make-symbol-table) type))
            (id-ast (convert-identifier context id))
            (data-type-decl (slot-value constructor-decl :data-type-decl))
            (implicit-decl (make-ast-node <sal-accessor-decl> context accessor
                                          :id id-ast
                                          :type (make-ast-node <sal-function-type> context accessor
                                                               :domain (create-internal-name-ref context accessor data-type-decl)
                                                               :range type-ast)
                                          :constructor-decl constructor-decl)))
       (sxml-symbol-attribute->ast-attribute! accessor 'name-class implicit-decl :name-class)
       (add-constant-declaration context name implicit-decl)
       (let ((result (create-internal-name-ref context accessor implicit-decl)))
         (values result type-ast))))))

(define (context-name/actuals sxml)
  (sxml/match-or-fail sxml
    ((CONTEXTNAME ?- (ACTUALPARAMETERS . ?actual-list))
     actual-list)
    ((CONTEXTNAME . ?-)
     '())))

(define (convert-actual context sym-tab param actual)
  (if (instance-of? param <sal-type-param-decl>)
    (convert-type context sym-tab actual)
    (convert-expr context sym-tab actual)))

(define (convert-context-name context sym-tab sxml)
  (trace 'context "Converting CONTEXTNAME at ~a" (format-with-location sxml ""))
  (sxml/match-or-fail sxml
    ((CONTEXTNAME (as (IDENTIFIER ?name) ?id) . ?-)
     (let ((actual-list (context-name/actuals sxml)))
       (cond 
        ((and (null? actual-list)
              (sal-context/context-name-declaration context name)) =>
              identity)
        (else
         (let* ((ctx-ref (sal-env/context (sal-ast/sal-env context) name))
                (param-list (sal-context/params ctx-ref))
                (_ (unless (= (length param-list) (length actual-list))
                     (sign-source-error sxml "Invalid context name `~a', wrong number of actual parameters." name)))
                (result (make-ast-node <sal-context-name-decl>  context sxml
                                       :context-ref ctx-ref
                                       :actuals (map (lambda (param actual)
                                                       (convert-actual context sym-tab param actual))
                                                     param-list
                                                     actual-list))))
           (sal-ast/check-number-of-actuals result)
           result)))))))

(define (convert-declaration context sxml)
  (case (sxml/tag sxml)
    ((IMPORT)
     (process-import context sxml))
    ((CONSTANTDECLARATION)
     (convert-constant-declaration context sxml))
    ((TYPEDECLARATION)
     (convert-type-declaration context sxml))
    ((CONTEXTDECLARATION)
     (convert-context-name-declaration context sxml))
    ((MODULEDECLARATION)
     (convert-module-declaration context sxml))
    ((ASSERTIONDECLARATION)
     (convert-assertion-declaration context sxml))))

(define (process-import context sxml)
  (trace 'context "Processing IMPORT DECLARATION at ~a" (format-with-location sxml ""))
  (sxml/match-or-fail sxml
    ((IMPORT ?context-ref)
     (let ((new-importer (process-context-ref context (make-symbol-table) context-ref)))
       (queue/push! (slot-value context :importers) new-importer)))))

(define (process-context-ref context sym-tab sxml)
  (sxml/match-or-fail sxml
    ((CONTEXTNAME . ?-)
     (let ((ctx-name-decl (convert-context-name context sym-tab sxml)))
       (make-context-name-importer (slot-value ctx-name-decl :context-ref)
                                   (slot-value ctx-name-decl :actuals))))
    ((IMPORTRENAME (RENAMEDECLS . ?rename-list) ?ctx-ref)
     (let ((rename-list (map (sxml/match-lambda-or-fail
                              ((RENAMEDECL (IDENTIFIER ?id1) (IDENTIFIER ?id2))
                               (cons id1 id2)))
                             rename-list)))
       (make-rename-importer rename-list (process-context-ref context sym-tab ctx-ref))))
    ((ADDPREFIX (IDENTIFIER ?prefix) ?ctx-ref)
     (make-add-prefix-importer prefix (process-context-ref context sym-tab ctx-ref)))
    ((ADDSUFFIX (IDENTIFIER ?suffix) ?ctx-ref)
     (make-add-suffix-importer suffix (process-context-ref context sym-tab ctx-ref)))))

(define (sxml-attribute->ast-attribute! sxml attribute ast attr-keyword)
  (cond
   ((sxml/attribute sxml attribute) =>
    (lambda (value)
      (sal-ast/set-attribute! ast attr-keyword value)))))

(define (sxml-symbol-attribute->ast-attribute! sxml attribute ast attr-keyword)
  (cond
   ((sxml/attribute sxml attribute) =>
    (lambda (value)
      (try
       (let ((obj (eval (to-symbol value))))
         (sal-ast/set-attribute! ast attr-keyword obj))
       (lambda (escape proc msg obj)
         (sign-source-error sxml "Unknown symbol name ~a." value)))))))

(define (convert-constant-declaration context sxml)
  (trace 'context "Processing CONSTANTDECLARATION at ~a" (format-with-location sxml ""))
  (sxml/match-or-fail sxml
    ((CONSTANTDECLARATION (as (IDENTIFIER ?name) ?id) ?type . ?expr)
     (check-if-already-defined-constant context sxml name)
     (let ((result (make-ast-node <sal-constant-decl> context sxml
                                  :id (convert-identifier context id)
                                  :type (convert-type context (make-symbol-table) type)))
           (expr (if (null? expr) #f (car expr))))
       (symbol-table/add! (slot-value context :constant-declarations) name result)
       (queue/insert! (slot-value context :declarations) result)
       (when expr
         (set-slot-value! result :value (convert-expr context (make-symbol-table) expr)))
       (sxml-attribute->ast-attribute! sxml 'inline result :inline?)
       (sxml-symbol-attribute->ast-attribute! sxml 'app-class result :app-class)
       (sxml-symbol-attribute->ast-attribute! sxml 'name-class result :name-class)
       result))))

(define (convert-type-declaration context sxml)
  (trace 'context "Processing TYPEDECLARATION at ~a" (format-with-location sxml ""))
  (sxml/match-or-fail sxml
    ((TYPEDECLARATION (as (IDENTIFIER ?name) ?id) . ?type)
     (when (symbol-table/lookup (slot-value context :type-declarations) name)
       (sign-redeclaration-of sxml name))
     (let ((result (make-ast-node <sal-type-decl> context sxml
                                  :id (convert-identifier context id)))
           (type (if (null? type) #f (car type))))
       (sxml-symbol-attribute->ast-attribute! sxml 'name-class result :name-class)
       (symbol-table/add! (slot-value context :type-declarations) name result)
       (queue/insert! (slot-value context :declarations) result)
       (when type
         (set-slot-value! result :type (convert-type-def context result type)))))))

(define (convert-context-name-declaration context sxml)
  (trace 'context "Processing CONTEXTDECLARATION at ~a" (format-with-location sxml ""))
  (sxml/match-or-fail sxml
    ((CONTEXTDECLARATION (as (IDENTIFIER ?name) ?id) ?contextname-node)
     (when (symbol-table/lookup (slot-value context :context-name-declarations) name)
       (sign-redeclaration-of sxml name))
     (let ((result (convert-context-name context (make-symbol-table) contextname-node)))
       (set-slot-value! result :id (convert-identifier context id))
       (symbol-table/add! (slot-value context :context-name-declarations) name result)
       (queue/insert! (slot-value context :declarations) result)))))
 
(define (convert-module-declaration context sxml) 
  (trace 'context "Processing MODULEDECLARATION at ~a" (format-with-location sxml ""))
  (sxml/match-or-fail sxml
    ((MODULEDECLARATION (as (IDENTIFIER ?name) ?id) (VARDECLS . ?var-decl-list) ?module)
     (when (symbol-table/lookup (slot-value context :module-declarations) name)
       (sign-redeclaration-of sxml name))
     (let* ((var-decl-ast-list (convert-var-decl-list context (make-symbol-table) var-decl-list))
            (parametric-module (make-ast-node <sal-parametric-module> context sxml
                                              :local-decls var-decl-ast-list
                                              :module (convert-module context 
                                                                      (update-symbol-table-with-decl-list (make-symbol-table) var-decl-ast-list)
                                                                      module)))
            (result (make-ast-node <sal-module-decl> context sxml
                                   :id (convert-identifier context id)
                                   :parametric-module parametric-module)))
       (symbol-table/add! (slot-value context :module-declarations) name result)
       (queue/insert! (slot-value context :declarations) result)))))

(define (convert-assertion-declaration context sxml)
  (trace 'context "Processing ASSERTIONDECLARATION at ~a" (format-with-location sxml ""))
  (sxml/match-or-fail sxml
    ((ASSERTIONDECLARATION (as (IDENTIFIER ?name) ?id) (ASSERTIONFORM ?kind) ?assertion-expr)
     (when (symbol-table/lookup (slot-value context :assertion-declarations) name)
       (sign-redeclaration-of sxml name))
     (let ((result (make-ast-node <sal-assertion-decl> context sxml
                                  :id (convert-identifier context id)
                                  :kind kind
                                  :assertion-expr (convert-assertion-expr context (make-symbol-table) assertion-expr))))
       (symbol-table/add! (slot-value context :assertion-declarations) name result)
       (queue/insert! (slot-value context :declarations) result)))))

(define (convert-expr context sym-tab sxml)
  (case (sxml/tag sxml)
    ((NAMEEXPR)
     (convert-name-expr context sym-tab sxml))
    ((QUALIFIEDNAMEEXPR)
     (convert-qualified-name-expr context sym-tab sxml))
    ((NEXTOPERATOR)
     (convert-next-operator context sym-tab sxml))
    ((NUMERAL)
     (convert-numeral context sxml))
    ((STRINGEXPR)
     (convert-string-expr context sxml))
    ((APPLICATION)
     (convert-application context sym-tab sxml))
    ((ARRAYSELECTION)
     (convert-array-selection context sym-tab sxml))
    ((RECORDSELECTION)
     (convert-record-selection context sym-tab sxml))
    ((TUPLESELECTION)
     (convert-tuple-selection context sym-tab sxml))
    ((UPDATEEXPRESSION)
     (convert-update-expression context sym-tab sxml))
    ((LAMBDAABSTRACTION FORALLEXPRESSION EXISTSEXPRESSION)
     (convert-multi-binded-expression context sym-tab sxml))
    ((LETEXPRESSION)
     (convert-let-expression context sym-tab sxml))
    ((SETLISTEXPRESSION)
     (convert-set-list-expression context sym-tab sxml))
    ((ARRAYLITERAL SETPREDEXPRESSION)
     (convert-single-binded-expr context sym-tab sxml))
    ((RECORDLITERAL)
     (convert-record-literal context sym-tab sxml))
    ((TUPLELITERAL)
     (convert-tuple-literal context sym-tab sxml <sal-tuple-literal>))
    ((CONDITIONAL)
     (convert-conditional context sym-tab sxml))
    ((MODINIT)
     (convert-mod-init context sym-tab sxml))
    ((MODTRANS)
     (convert-mod-trans context sym-tab sxml))))

(define (convert-arg-expr context sym-tab sxml)
  (case (sxml/tag sxml)
    ((TUPLELITERAL)
     (convert-tuple-literal context sym-tab sxml <sal-arg-tuple-literal>))
    (else
     (convert-expr context sym-tab sxml))))

(define (convert-name-expr context sym-tab sxml)    
  (sxml/match-or-fail sxml
    ((NAMEEXPR ?name)
     (cond
      ((symbol-table/lookup sym-tab name) =>
       (lambda (decl)
         (make-ast-node <sal-name-expr> context sxml :decl decl)))
      ((sal-context/constant-declaration context name) =>
       (lambda (decl)
         (create-internal-name-ref context sxml decl)))
      ((sal-context/param context name) =>
       (lambda (decl)
         (unless (instance-of? decl <sal-var-decl>)
           (sign-source-error sxml "Context parameter `~a' is not a variable declaration." name))
         (make-ast-node <sal-var-param-name-expr> context sxml
                        :decl decl)))
      ((external-name context 'constant name sxml) =>
       identity)
      ((sal-env/builtin-constant-decl (sal-ast/sal-env context) name) =>
       (lambda (decl)
         (create-builtin-name-ref context sxml (sal-decl/name-class decl) decl)))
      (else 
       (sign-source-error sxml "Unknown variable \"~a\"." name))))))

(define (convert-next-operator context sym-tab sxml)    
  (sxml/match-or-fail sxml
    ((NEXTOPERATOR ?name-expr)
     (make-ast-node <sal-next-operator> context sxml
                    :name-expr (convert-name-expr context sym-tab name-expr)))))

(define (convert-numeral context sxml)
  (sxml/match-or-fail sxml
    ((NUMERAL ?num)
     (make-ast-node <sal-numeral> context sxml
                    :num (make-mpq num)))))

(define (convert-string-expr context sxml)
  (sxml/match-or-fail sxml
    ((STRINGEXPR ?str)
     (make-ast-node <sal-string-expr> context sxml
                    :string str))))

(define (convert-application context sym-tab sxml)
  (sxml/match-or-fail sxml
    ((APPLICATION (NAMEEXPR in) (TUPLELITERAL ?arg ?fun))
     (make-ast-node <sal-in> context sxml
                    :fun (convert-expr context sym-tab fun)
                    :arg (convert-expr context sym-tab arg)))
    ((APPLICATION ?fun ?arg)
     (let ((fun-ast (convert-expr context sym-tab fun))
           (arg-ast (convert-arg-expr context sym-tab arg)))
       (make-ast-node (sal-expr/application-class fun-ast) context sxml
                      :fun fun-ast
                      :arg arg-ast)))))

(define (convert-array-selection context sym-tab sxml)
  (sxml/match-or-fail sxml
    ((ARRAYSELECTION ?col ?idx)
     (make-ast-node <sal-array-selection> context sxml
                    :fun (convert-expr context sym-tab col)
                    :arg (convert-expr context sym-tab idx)))))

(define (convert-record-selection context sym-tab sxml)
  (sxml/match-or-fail sxml
    ((RECORDSELECTION ?col ?id)
     (make-ast-node <sal-record-selection> context sxml
                    :target (convert-expr context sym-tab col)
                    :idx (convert-identifier context id)))))

(define (convert-tuple-selection context sym-tab sxml)
  (sxml/match-or-fail sxml
    ((TUPLESELECTION ?col ?pos)
     (make-ast-node <sal-tuple-selection> context sxml
                    :target (convert-expr context sym-tab col)
                    :idx (convert-numeral context pos)))))

(define (convert-update-index context sym-tab idx)
  (sxml/match-or-fail idx
    ((IDENTIFIER ?-) 
     (convert-identifier context idx))
    ((TUPLELITERAL . ?-)
     (convert-arg-expr context sym-tab idx))
    (?-
     (convert-expr context sym-tab idx))))

(define (create-update-expr sel-tag context sxml target idx new-value)
  (make-ast-node (case sel-tag
                   ((ARRAYSELECTION) <sal-array-update>)
                   ((RECORDSELECTION) <sal-record-update>)
                   ((TUPLESELECTION) <sal-tuple-update>)
                   ((APPLICATION) <sal-function-update>)
                   (else (internal-error)))
                 context sxml
                 :target target
                 :idx idx
                 :new-value new-value))

(define (convert-simple-update-expression context sym-tab sxml)
  [assert (sxml) (sal/simple-update-expression? sxml)]
  (sxml/match-or-fail sxml
    ((UPDATEEXPRESSION ?target (as (?- ?- ?idx) ?selection) ?new-value)
     [assert (selection) (sxml/tag-equals*? selection '(ARRAYSELECTION TUPLESELECTION RECORDSELECTION APPLICATION))]
     (create-update-expr (sxml/tag selection) context sxml 
                         (convert-expr context sym-tab target)
                         (convert-update-index context sym-tab idx)
                         (convert-expr context sym-tab new-value)))))

(define (sxml-name-expr? sxml)
  (sxml/match sxml
    ((or (NAMEEXPR . ?-)
         (QUALIFIEDNAMEEXPR . ?-)) #t)))

(define (convert-update-expression context sym-tab sxml)
  ;; (print "convert-update-expression:" ) (sxml/pp sxml)
  (if (sal/simple-update-expression? sxml)
    (convert-simple-update-expression context sym-tab sxml)
    (sxml/match-or-fail sxml
      ((UPDATEEXPRESSION ?target ?pos ?value)
       (let* ((simple-target? (sxml-name-expr? target))
              (target-ast (convert-expr context sym-tab target))
              (aux-var (if (not simple-target?) (gen-unique-name 'col)))
              (new-decl-ast (and (not simple-target?)
                                 (make-ast-node <sal-let-decl> context sxml
                                                :id (make-identifier-node context sxml aux-var)
                                                :type (begin
                                                        (sal-ast/type-check target-ast) ;; it is not safe to call sal-expr/type, before type checking
                                                        (sal-expr/type target-ast))
                                                :value target-ast)))
              (new-sym-tab (if new-decl-ast
                             (update-symbol-table-with-decl sym-tab new-decl-ast)
                             sym-tab))
              (new-target (if simple-target?
                            target
                            (template->sxml (NAMEEXPR (<== target) ,aux-var))))
              (new-target-ast (if simple-target?
                                target-ast
                                (convert-expr context new-sym-tab new-target))))
         (multiple-value-bind
             (last-selection new-pos)
             (let loop ((pos pos))
               (sxml/match-or-fail pos
                 (((or ARRAYSELECTION RECORDSELECTION TUPLESELECTION APPLICATION) ?c ?idx)
                  (if (sxml/equal? c target)
                    (let ((last-selection (make-sxml-node-based-on pos
                                                                   (list new-target idx))))
                      (values last-selection last-selection))
                    (multiple-value-bind
                        (last-selection new-pos)
                        (loop c)
                      (values last-selection (make-sxml-node-based-on pos
                                                                      (list new-pos idx))))))))
           (let* ((new-value-ast (convert-update-expression context
                                                            new-sym-tab
                                                            (make-sxml-node-based-on sxml
                                                                                     (list last-selection new-pos value))))
                  (last-selection-idx (sxml/match-or-fail last-selection
                                        ((?- ?- ?idx) idx)))
                  (last-selection-idx-ast (convert-update-index context new-sym-tab last-selection-idx))
                  (new-update-expr-ast  (create-update-expr (sxml/tag last-selection) context sxml
                                                            new-target-ast
                                                            last-selection-idx-ast
                                                            new-value-ast)))
             (if (eq? target new-target)
               new-update-expr-ast
               (make-ast-node <sal-let-expr> context sxml
                              :local-decls (list new-decl-ast)
                              :expr new-update-expr-ast)))))))))

(define (convert-var-decl-list context sym-tab var-decl-list)
  (map (cut convert-var-decl context sym-tab <>) var-decl-list))

(define (convert-multi-binded-expression context sym-tab sxml)
  (sxml/match-or-fail sxml
    ((?- (VARDECLS . ?var-decl-list) ?expr)
     (let ((var-decl-ast-list (convert-var-decl-list context sym-tab var-decl-list)))
       (make-ast-node (case (sxml/tag sxml)
                        ((LAMBDAABSTRACTION) <sal-lambda>)
                        ((FORALLEXPRESSION) <sal-for-all-expr>)
                        ((EXISTSEXPRESSION) <sal-exists-expr>)
                        (else (internal-error)))
                      context sxml
                      :local-decls var-decl-ast-list
                      :expr (convert-expr context (update-symbol-table-with-decl-list sym-tab var-decl-ast-list) expr))))))

(define (convert-let-expression context sym-tab sxml)
  (sxml/match-or-fail sxml
    ((LETEXPRESSION (LETDECLARATIONS . ?let-decl-list) ?expr)
     (let ((let-decl-ast-list (map (cut convert-let-decl context sym-tab <>) let-decl-list)))
       (make-ast-node <sal-let-expr> context sxml
                      :local-decls let-decl-ast-list
                      :expr (convert-expr context (update-symbol-table-with-decl-list sym-tab let-decl-ast-list) expr))))))

(define (convert-single-binded-expr context sym-tab sxml)
  (sxml/match-or-fail sxml
    ((?- ?var-decl ?expr)
     (let ((var-decl-ast (convert-var-decl context sym-tab var-decl)))
       (when (sxml/tag-equals? sxml 'ARRAYLITERAL)
         (quick-change-class! var-decl-ast <sal-idx-var-decl>))
       (make-ast-node (case (sxml/tag sxml)
                        ((ARRAYLITERAL) <sal-array-literal>)
                        ((SETPREDEXPRESSION) <sal-set-pred-expr>)
                        (else (internal-error)))
                      context sxml
                      :local-decls (list var-decl-ast)
                      :expr (convert-expr context (update-symbol-table-with-decl sym-tab var-decl-ast) expr))))))

(define (convert-set-list-expression context sym-tab sxml)
  (sxml/match-or-fail sxml
    ((SETLISTEXPRESSION . ?expr-list)
     (when (null? expr-list)
       (sign-source-error sxml "Invalid empty set list expression."))
     (let* ((exprs (map (cut convert-expr context sym-tab <>) expr-list))
            (expr-types (map (lambda (expr)
                               (sal-ast/type-check expr) ;; it is not safe to call sal-expr/type before type-checking
                               (sal-expr/type expr)) exprs)))
       (check-set-list-expression-arguments sxml exprs)
       (let* ((result-type (fold-left sal-type/union (car expr-types) (cdr expr-types)))
              (aux-var-name (gen-unique-name 'aux))
              (aux-var-id (make-identifier-node context sxml aux-var-name))
              (aux-var-decl (make-ast-node <sal-var-decl> context sxml
                                           :id aux-var-id
                                           :type result-type))
              (aux-var-name (make-ast-node <sal-name-expr> context sxml
                                           :decl aux-var-decl))
              (body-expr (apply make-sal-or (map (lambda (expr)
                                                   (make-sal-builtin-application <sal-eq> expr
                                                                                 aux-var-name
                                                                                 expr))
                                                 exprs))))
         (make-ast-node <sal-set-list-expr> context sxml
                        :local-decls (list aux-var-decl)
                        :expr body-expr))))))

(define (convert-record-literal context sym-tab sxml)
  (sxml/match-or-fail sxml
    ((RECORDLITERAL . ?entry-list)
     (let ((entry-list (sort entry-list
                             (lambda (entry1 entry2)
                               (sxml/match-or-fail entry1
                                 ((RECORDENTRY (IDENTIFIER ?id1) ?-)
                                  (sxml/match-or-fail entry2
                                    ((RECORDENTRY (IDENTIFIER ?id2) ?-)
                                     (string<? (symbol->string id1)
                                               (symbol->string id2))))))))))
       (make-ast-node <sal-record-literal> context sxml
                      :entries (map (cut convert-entry context sym-tab <>) entry-list))))))

(define (convert-entry context sym-tab sxml)
  (sxml/match-or-fail sxml
    ((RECORDENTRY ?id ?expr)
     (make-ast-node <sal-record-entry> context sxml
                    :id (convert-identifier context id)
                    :expr (convert-expr context sym-tab expr)))))

(define (convert-tuple-literal context sym-tab sxml tuple-literal-class)
  (sxml/match-or-fail sxml
    ((TUPLELITERAL . ?expr-list)
     (make-ast-node tuple-literal-class context sxml
                    :exprs (map (cut convert-expr context sym-tab <>) expr-list)))))

(define (convert-conditional context sym-tab sxml)
  (sxml/match-or-fail sxml
    ((CONDITIONAL ?cond-expr ?then-expr ?else-expr)
     (make-ast-node <sal-conditional> context sxml
                    :cond-expr (convert-expr context sym-tab cond-expr)
                    :then-expr (convert-expr context sym-tab then-expr)
                    :else-expr (convert-expr context sym-tab else-expr)))))

(define (convert-mod-init context sym-tab sxml)
  (sxml/match-or-fail sxml
    ((MODINIT ?module)
     (make-ast-node <sal-mod-init> context sxml
                    :module (convert-module context sym-tab module)))))

(define (convert-mod-trans context sym-tab sxml)
  (sxml/match-or-fail sxml
    ((MODTRANS ?module)
     (make-ast-node <sal-mod-trans> context sxml
                    :module (convert-module context sym-tab module)))))

(define (convert-module context sym-tab sxml)
  (case (sxml/tag sxml)
    ((BASEMODULE)
     (convert-base-module context sym-tab sxml))
    ((ASYNCHRONOUSCOMPOSITION SYNCHRONOUSCOMPOSITION OBSERVEMODULE)
     (convert-composition context sym-tab sxml))
    ((MULTISYNCHRONOUS MULTIASYNCHRONOUS)
     (convert-multi-composition context sym-tab sxml))
    ((HIDING NEWOUTPUT)
     (convert-nested-module context sym-tab sxml))
    ((RENAMING)
     (convert-renaming context sym-tab sxml))
    ((WITHMODULE)
     (convert-with-module context sym-tab sxml))
    ((MODULEINSTANCE)
     (convert-module-instance context sym-tab sxml))
    (else
     [sal-assert "convert-module" (sxml) #f]
     (internal-error))))

(define (convert-state-var-decl-list-into-table! context sym-tab var-decl-list state-var-class defined-names queue)
  (for-each (lambda (var-decl)
              (sxml/match-or-fail var-decl
                ((VARDECL (IDENTIFIER ?name) ?-)
                 (when (symbol-set/member? name defined-names)
                   (sign-redeclaration-of var-decl name))
                 (symbol-set/add! defined-names name)
                 (queue/insert! queue (convert-state-var-decl context sym-tab var-decl state-var-class)))))
            var-decl-list))

(define (convert-state-variable-declarations context sym-tab decl-list)
  (let* ((result (make-queue))
         (defined-names (make-symbol-set)))
    (for-each 
     (sxml/match-lambda
      ((INPUTDECL . ?var-decl-list)
       (convert-state-var-decl-list-into-table! context sym-tab var-decl-list <sal-input-state-var-decl> defined-names result))
      ((OUTPUTDECL . ?var-decl-list)
       (convert-state-var-decl-list-into-table! context sym-tab var-decl-list <sal-output-state-var-decl> defined-names result))
      ((GLOBALDECL . ?var-decl-list)
       (convert-state-var-decl-list-into-table! context sym-tab var-decl-list <sal-global-state-var-decl> defined-names result))
      ((LOCALDECL . ?var-decl-list)
       (convert-state-var-decl-list-into-table! context sym-tab var-decl-list <sal-local-state-var-decl> defined-names result)))
     decl-list)
    (queue->list result)))

(define (make-running-var-decl place-provider)
  (let* ((id (make-sal-identifier place-provider (sal/basemodule-running-var-name)))
         (bool-type (make-sal-builtin-name <sal-bool-type> place-provider)))
    (make-ast-instance <sal-local-state-var-decl> place-provider
                       :id id
                       :type bool-type)))
         
(define (collect-base-module-decl decl-tag decl-list)
  (let ((result (make-queue)))
    (for-each
     (sxml/match-lambda
      (((? (cut eq? <> decl-tag)) . ?def-list)
       (queue/append! result def-list)))
     decl-list)
    (queue->list result)))

(define (convert-base-module context sym-tab sxml)
  (sxml/match-or-fail sxml
    ((BASEMODULE . ?decl-list)
     (let* ((state-vars (convert-state-variable-declarations context sym-tab decl-list))
            (place-provider (make-ast-node <sal-ast> context sxml))
            ;; In the current implementation, I do not support `running' as a default builtin variable
            ;; (running-var-decl (make-running-var-decl place-provider))
            (state-vars-with-running state-vars) ;; (cons running-var-decl state-vars))
            (new-sym-tab (update-symbol-table-with-decl-list sym-tab state-vars-with-running))
            (definition-body (collect-base-module-decl 'DEFDECL decl-list))
            (initialization-body (collect-base-module-decl 'INITDECL decl-list))
            (transition-body (collect-base-module-decl 'TRANSDECL decl-list))
            (definitions (convert-def-list context new-sym-tab definition-body)))
       (multiple-value-bind
           (initialization-definitions initialization-command-section)
           (convert-def-or-cmd-list context new-sym-tab initialization-body)
         (multiple-value-bind
             (transition-definitions transition-command-section)
             (convert-def-or-cmd-list context new-sym-tab transition-body)
           (unless definitions
             (set! definitions '()))
           (unless initialization-definitions
             (set! initialization-definitions '()))
           (unless transition-definitions
             (set! transition-definitions '()))
           (make-ast-node <sal-base-module> context sxml
                          :state-vars state-vars-with-running
                          :definitions definitions
                          :initialization-definitions initialization-definitions
                          :initialization-command-section initialization-command-section
                          :transition-definitions transition-definitions
                          :transition-command-section transition-command-section)))))))

(define (convert-def-list context sym-tab def-list)
  (map (cut convert-definition context sym-tab <>) def-list))

(define (convert-assignment-list context sym-tab def-list)
  (map (cut convert-definition context sym-tab <>) def-list))

(define (convert-definition context sym-tab definition)
  (case (sxml/tag definition)
    ((SIMPLEDEFINITION)
     (convert-simple-definition context sym-tab definition))
    ((FORALLDEFINITION)
     (convert-for-all-definition context sym-tab definition))
    (else
     (internal-error))))

(define (convert-simple-definition context sym-tab definition)
  (sxml/match-or-fail definition
    ((SIMPLEDEFINITION ?lhs (as (?- ?rhs) ?rhs-expr))
     (make-ast-node 
      (case (sxml/tag rhs-expr)
        ((RHSEXPRESSION) <sal-simple-definition>)
        ((RHSSELECTION) <sal-simple-selection-definition>)
        (else (internal-error)))
      context definition
      :lhs (convert-expr context sym-tab lhs)
      :rhs (convert-expr context sym-tab rhs)))))

(define (convert-for-all-definition context sym-tab definition)
  (sxml/match-or-fail definition
    ((FORALLDEFINITION (VARDECLS . ?var-decl-list) . ?def-list)
     (let* ((var-decl-ast-list (convert-var-decl-list context sym-tab var-decl-list))
            (new-sym-tab (update-symbol-table-with-decl-list sym-tab var-decl-ast-list))
            (def-ast-list (convert-def-list context new-sym-tab def-list)))
       (make-ast-node <sal-for-all-definition> context definition
                      :local-decls var-decl-ast-list
                      :definitions def-ast-list)))))

(define (convert-def-or-cmd-list context sym-tab def-or-cmd-list)
  (let ((cmd-queue (make-queue))
        (def-queue (make-queue))
        (cmd-place-provider #f))
    (for-each (lambda (curr-def-or-cmd)
                (cond
                 ((sxml/tag-equals? curr-def-or-cmd 'SOMECOMMANDS)
                  (unless cmd-place-provider
                    (set! cmd-place-provider curr-def-or-cmd))
                  (queue/append! cmd-queue (sxml/children curr-def-or-cmd)))
                 (else
                  (queue/insert! def-queue curr-def-or-cmd))))
              def-or-cmd-list)
    (values (convert-def-list context sym-tab (queue->list def-queue))
            (convert-cmd-list context sym-tab (queue->list cmd-queue) cmd-place-provider))))

(define-generic (sal-command/else? cmd))
(define-method (sal-command/else? (cmd <sal-command>)) #f)
(define-method (sal-command/else? (cmd <sal-labeled-command>)) (sal-command/else? (slot-value cmd :command)))
(define-method (sal-command/else? (cmd <sal-else-command>)) #t)
  
(define (convert-cmd-list context sym-tab cmd-list place-provider)
  (and (not (null? cmd-list))
       (let loop ((cmd-list cmd-list)
                  (else-cmd #f)
                  (cmd-queue (make-queue)))
         (if (null? cmd-list)
           (make-ast-node <sal-command-section> context place-provider
                          :commands (queue->list cmd-queue)
                          :else-command else-cmd)
           (let* ((new-cmd (convert-command context sym-tab (car cmd-list)))
                  (new-else-cmd? (sal-command/else? new-cmd)))
             (when (and else-cmd new-else-cmd?)
               (sign-source-error (car cmd-list) "A guarded command section must contain at most one ELSE command."))
             (loop (cdr cmd-list)
                   (if new-else-cmd? new-cmd else-cmd)
                   (if new-else-cmd? cmd-queue (queue/insert! cmd-queue new-cmd))))))))
  
(define (convert-command context sym-tab command)
  (case (sxml/tag command)
    ((GUARDEDCOMMAND)
     (convert-guarded-command context sym-tab command))
    ((LABELEDCOMMAND LABELEDELSECOMMAND)
     (convert-labeled-command context sym-tab command))
    ((MULTICOMMAND)
     (convert-multi-command context sym-tab command))
    ((ELSECOMMAND)
     (convert-else-command context sym-tab command))
    (else
     [assert (command) #f]
     (internal-error))))

(define (convert-guarded-command context sym-tab command)
  (sxml/match-or-fail command
    ((GUARDEDCOMMAND (GUARD ?expr) (ASSIGNMENTS . ?def-list))
     (make-ast-node <sal-guarded-command> context command
                    :guard (convert-expr context sym-tab expr)
                    :assignments (convert-assignment-list context sym-tab def-list)))))

(define (convert-labeled-command context sym-tab command)
  (sxml/match-or-fail command
    (((or LABELEDCOMMAND LABELEDELSECOMMAND) (as (LABEL ?label-name) ?label) ?nested-command)
     (make-ast-node <sal-labeled-command> context command
                    :label (make-identifier-node context label label-name)
                    :command (convert-command context sym-tab nested-command)))))

(define (convert-multi-command context sym-tab command)
  (sxml/match-or-fail command
    ((MULTICOMMAND (VARDECLS . ?var-decl-list) ?nested-command)
     (let* ((var-decl-ast-list (convert-var-decl-list context sym-tab var-decl-list))
            (new-sym-tab (update-symbol-table-with-decl-list sym-tab var-decl-ast-list)))
       (make-ast-node <sal-multi-command> context command
                      :local-decls var-decl-ast-list
                      :command (convert-command context new-sym-tab nested-command))))))

(define (convert-else-command context sym-tab command)
  (let ((def-list (sxml/match-or-fail command
                    ((ELSECOMMAND (ASSIGNMENTS . ?def-list))
                     def-list)
                    ((ELSECOMMAND)
                     '()))))
    (make-ast-node <sal-else-command> context command
                   :assignments (convert-assignment-list context sym-tab def-list))))
     
(define (convert-composition context sym-tab sxml)
  (sxml/match-or-fail sxml
    ((?- ?mod1 ?mod2)
     (let* ((mod1-ast (convert-module context sym-tab mod1))
            (mod2-ast (convert-module context sym-tab mod2))
            (class (case (sxml/tag sxml)
                     ((SYNCHRONOUSCOMPOSITION) <sal-synch-composition>)
                     ((ASYNCHRONOUSCOMPOSITION) <sal-asynch-composition>)
                     ((OBSERVEMODULE) <sal-observer>)
                     (else (internal-error))))
            (new-module (make-ast-node class context sxml
                                       :module1 mod1-ast
                                       :module2 mod2-ast)))
       (sal-module/update-interface! new-module)
       new-module))))

(define (convert-nested-module context sym-tab sxml)
  (sxml/match-or-fail sxml
    ((?- (IDENTIFIERS . ?id-list) ?mod)
     (let* ((mod-ast (convert-module context sym-tab mod))
            (id-name-list (map (sxml/match-lambda-or-fail 
                                ((as (IDENTIFIER ?name) ?id)
                                 (make-identifier-node context id name)))
                               id-list))
            (class (case (sxml/tag sxml)
                     ((NEWOUTPUT) <sal-new-output>)
                     ((HIDING) <sal-hiding>)
                     (else (internal-error))))
            (new-module (make-ast-node class context sxml
                                       :identifiers id-name-list
                                       :module mod-ast)))
       (sal-module/update-interface! new-module)
       new-module))))

(define (check-rename-list rename-list mod)
  (for-each (sxml/match-lambda-or-fail
             ((RENAME ?from ?to)
              (unless (sxml/tag-equals? from 'NAMEEXPR)
                (sign-source-error from "Invalid rename, the `from' variable must be a name expression, that is, an identifier."))))
            rename-list))

(define (process-simple-rename-target context child-state-vars implicit-declarations from-name to-expr)
  (sxml/match-or-fail to-expr
    ((NAMEEXPR ?to-name)
     (cond 
      ((sal-decl-list/lookup child-state-vars from-name) =>
       (lambda (decl)
         (let ((new-var-decl (copy-ast decl 
                                       :context context
                                       :place (sxml/place to-expr)
                                       :id (make-identifier-node context to-expr to-name))))
           (queue/insert! implicit-declarations new-var-decl)
           (make-ast-node <sal-name-expr> context to-expr
                          :decl new-var-decl))))))))

(define (convert-renaming context sym-tab sxml)
  (sxml/match-or-fail sxml
    ((RENAMING (RENAMES . ?rename-list) ?mod)
     (let ((mod-ast (convert-module context sym-tab mod)))
       (check-rename-list rename-list mod-ast)
       (let* ((state-vars (sal-module/state-variables mod-ast))
              (renamed-var-list (map (sxml/match-lambda-or-fail 
                                      ((RENAME (NAMEEXPR ?name) ?-)
                                       name))
                                     rename-list))
              (implicit-declarations (make-queue))
              (rename-var-ast-list 
               (map 
                (sxml/match-lambda-or-fail
                 ((RENAME (as (NAMEEXPR ?from-name) ?from-name-expr) ?to-expr)
                  (let ((to-ast (sxml/match to-expr
                                  ((NAMEEXPR ?to-name)
                                   (cond
                                    ((symbol-table/lookup sym-tab to-name) =>
                                     (lambda (decl)
                                       (make-ast-node <sal-name-expr> context to-expr
                                                      :decl decl)))
                                    (else 
                                     (process-simple-rename-target context state-vars implicit-declarations from-name to-expr))))
                                  (?- (convert-expr context sym-tab to-expr)))))
                    (make-ast-node <sal-rename> context sxml
                                   :from-name (make-identifier-node context from-name-expr from-name)
                                   :to-expr to-ast))))
                rename-list))
              (rename-ast (make-ast-node <sal-renaming> context sxml
                                         :renames rename-var-ast-list
                                         :module mod-ast)))
         (sal-module/update-interface! rename-ast)
         (if (not (queue/empty? implicit-declarations))
           ;; create a with module to store the implicit declarations
           (let ((with-module (make-ast-node <sal-with-module> context sxml
                                             :new-state-vars (queue->list implicit-declarations)
                                             :module rename-ast)))
             (sal-module/update-interface! with-module)
             with-module)
           rename-ast))))))

(define (convert-module-name context sym-tab sxml)
  (sxml/match-or-fail sxml
    ((MODULENAME ?name)
     (cond
      ((sal-context/module-declaration context name) =>
       (lambda (decl)
         (create-internal-name-ref context sxml decl)))
      ((external-name context 'module name sxml) =>
       identity)
      (else
       (sign-source-error sxml "Unknown module \"~a\"." name))))
    ((QUALIFIEDMODULENAME . ?-)
     (convert-qualified-module-name context sym-tab sxml))))

(define (convert-module-instance context sym-tab sxml)
  (sxml/match-or-fail sxml
    ((MODULEINSTANCE ?module-name (MODULEACTUALS . ?expr-list))
     (let* ((module-name-ast (convert-module-name context sym-tab module-name))
            (actuals (map (cut convert-expr context sym-tab <>) expr-list))
            (new-module (make-ast-node <sal-module-instance> context sxml
                                       :module-name module-name-ast
                                       :actuals actuals)))
       (sal-ast/check-number-of-actuals new-module)
       (sal-module/update-interface! new-module)
       new-module))))

(define (convert-with-module context sym-tab sxml)
  (sxml/match-or-fail sxml
    ((WITHMODULE (NEWVARDECLS . ?decl-list) ?mod)
     (let* ((new-state-vars (convert-state-variable-declarations context sym-tab decl-list))
            (new-sym-tab (update-symbol-table-with-decl-list sym-tab new-state-vars))
            (mod-ast (convert-module context new-sym-tab mod))
            (new-module (make-ast-node <sal-with-module> context sxml
                                         :new-state-vars new-state-vars
                                         :module mod-ast)))
       (sal-module/update-interface! new-module)
       new-module))))
                     
(define (convert-multi-composition context sym-tab sxml)
  (sxml/match-or-fail sxml
    ((?- ?var-decl ?mod)
     (let* ((var-decl-ast (convert-idx-var-decl context sym-tab var-decl))
            (new-sym-tab (update-symbol-table-with-decl sym-tab var-decl-ast))
            (class (case (sxml/tag sxml)
                     ((MULTIASYNCHRONOUS) <sal-multi-asynch-composition>)
                     ((MULTISYNCHRONOUS) <sal-multi-synch-composition>)
                     (else (internal-error))))
            (mod-ast (convert-module context new-sym-tab mod))
            (new-module (make-ast-node class context sxml
                                       :local-decls (list var-decl-ast)
                                       :module mod-ast)))
       (sal-module/update-interface! new-module)
       new-module))))

(define (convert-assertion-expr context sym-tab sxml)
  (case (sxml/tag sxml)
    ((MODULEMODELS)
     (convert-module-models context sym-tab sxml))
    ((MODULEIMPLEMENTS)
     (convert-module-implements context sym-tab sxml))
    ((ASSERTIONPROPOSITION)
     (convert-assertion-proposition context sym-tab sxml))
    (else
     (convert-expr context sym-tab sxml))))
  
(define (convert-module-models context sym-tab sxml)
  (sxml/match-or-fail sxml
    ((MODULEMODELS ?mod ?expr)
     (let* ((mod-ast (convert-module context sym-tab mod))
            (new-sym-tab (symbol-table/union sym-tab (sal-module/state-variables-table mod-ast)))
            (expr-ast (convert-expr context new-sym-tab expr)))
       (make-ast-node <sal-module-models> context sxml
                      :module mod-ast
                      :expr expr-ast)))))

(define (convert-module-implements context sym-tab sxml)
  (sxml/match-or-fail sxml
    ((MODULEIMPLEMENTS ?mod1 ?mod2)
     (make-ast-node <sal-module-implements> context sxml
                    :module1 (convert-module context sym-tab mod1)
                    :module2 (convert-module context sym-tab mod2)))))

(define (convert-assertion-proposition context sym-tab sxml)
  (sxml/match-or-fail sxml
    ((ASSERTIONPROPOSITION (ASSERTIONOPERATOR ?op) . ?assertion-expr-list)
     (make-ast-node <sal-assertion-proposition> context sxml
                    :op op
                    :assertion-exprs (map (cut convert-assertion-expr context sym-tab <>) assertion-expr-list)))))

