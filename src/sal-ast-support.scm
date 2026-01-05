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

(module sal-ast-support
        (import collect-info)
        (include "scmobj.sch")
        (include "utility.sch")
        (include "api.sch")
        (include "fast-hash-table.sch")
        (include "sal-ast-support.macros")
        (import sal-ast symbol-table sal-ast-for-each gmp-scheme sal-module sal-error 
                sal-context sal-environment queue sal-type unique-names sal-ast-attributes
                sal-decls)
        (export (sal-local-binds-ast/local-decls ast)
                (make-sal-name-expr decl . place-provider)
                (sal-decl/attribute decl attr-id)
                (sal-decl/name-class decl)
                (sal-decl/app-class decl)
                (sal-name-ref/decl-attribute expr attr-id)
                (sal-expr/application-class expr)
                *empty-place-source*
                (sal-ast/reset-cache! ast)
                (sal-ast/internal-idx ast)
                (change-ast-class ast class)
                (sal-ast-list? obj)
                (sal-expr-list? obj)
                (sal-type-list? obj)
                (sal-decl-list? obj)
                (sal-ast/copy-place-information! target place-source)
                (make-sal-identifier place-source name)
                (sal-ast/place ast)
                (sal-ast/size ast)
                (sal-ast/context ast)
                (sal-ast/sal-env ast)
                (sal-decl/name ast)
                (sal-field/name ast)
                (sal-record-entry/name ast)
                (sal-ast/context-name ast)
                (sal-ast/context-file-name ast)
                (sal-identifier/name ast)
                (sal-name-ref/decl name-ref)
                (sal-qualified-name-ref/context-ref qualified-name-ref)
                (sal-qualified-name-ref/actuals qualified-name-ref)
                (sal-name-ref/id ast)
                (sal-name-ref/name ast)
                (sal-name-ref/builtin? ast)
                (sal-name-ref/state? ast)
                (sal-name-ref/let-decl? ast)
                (sal-qualified-name-ref/defined-at ast)
                (sal-ast/uses-next-operator? ast)
                (sal-ast/contains-state-variable? ast)
                (sal-selection/target ast)
                (sal-selection/idx ast)
                (sal-selection/update-target ast new-target)
                (sal-expr/lhs? ast)
                (sal-expr/next-lhs? ast)
                (sal-lhs->next-lhs ast)
                (sal-next-lhs->lhs lhs)
                (sal-lhs/name-expr ast)
                (sal-lhs/next-operator ast)
                (sal-lhs/remove-next-operator ast)
                (sal-lhs/trivial? ast)
                (sal-decl/recursive? ast)
                (sal-tuple-position->integer pos)
                (sal-tuple/element ast slot-id pos)
                (sal-record/element ast slot-id item-slot-id id)
                (sal-ast/check-number-of-actuals ast)
                (sal-type-list->sal-decl-list type-list)
                (sal-command/guard cmd)
                (sal-else-command/assignments command)
                (sal-ast-list/sort-using-ids ast-list)
                (make-qualified-name-expr-based-on symbol qualified-name))
        )

(define-api (make-sal-name-expr decl . place-provider)
  (let ((place-provider (optional-arg place-provider decl)))
    (make-ast-instance <sal-name-expr> place-provider
                       :decl decl)))

(define-generic (sal-decl/attribute decl attr-id))
(define-method (sal-decl/attribute (decl <sal-decl>) (attr-id <primitive>))
  #f)
(define-method (sal-decl/attribute (decl <sal-constant-decl>) (attr-id <primitive>))
  (sal-ast/attribute decl attr-id))
(define-method (sal-decl/attribute (decl <sal-type-decl>) (attr-id <primitive>))
  (sal-ast/attribute decl attr-id))

(define-generic (sal-decl/name-class decl))
(define-method (sal-decl/name-class (decl <sal-constant-decl>))
  (or (sal-decl/attribute decl :name-class)
      <sal-qualified-name-expr>))
(define-method (sal-decl/name-class (decl <sal-scalar-element-decl>))
  (or (sal-decl/attribute decl :name-class)
      <sal-scalar>))
(define-method (sal-decl/name-class (decl <sal-constructor-decl>))
  (or (sal-decl/attribute decl :name-class)
      <sal-constructor>))
(define-method (sal-decl/name-class (decl <sal-accessor-decl>))
  (or (sal-decl/attribute decl :name-class)
      <sal-accessor>))
(define-method (sal-decl/name-class (decl <sal-recognizer-decl>))
  (or (sal-decl/attribute decl :name-class)
      <sal-recognizer>))
(define-method (sal-decl/name-class (decl <sal-type-decl>))
  (or (sal-decl/attribute decl :name-class)
      <sal-qualified-type-name>))
(define-method (sal-decl/name-class (decl <sal-module-decl>))
  <sal-qualified-module-name>)
(define-method (sal-decl/name-class (decl <sal-assertion-decl>))
  <sal-qualified-assertion-name>)

(define (sal-name-ref/decl-attribute expr attr-id)
  (sal-decl/attribute (slot-value expr :decl) attr-id))

(define-generic (sal-decl/app-class decl))
(define-method (sal-decl/app-class :around (decl <sal-decl>))
  (or (sal-decl/attribute decl :app-class)
      (call-next-method)))
(define-method (sal-decl/app-class (decl <sal-decl>))
  <sal-application>)
(define-method (sal-decl/app-class (decl <sal-constructor-decl>))
  <sal-constructor-application>)
(define-method (sal-decl/app-class (decl <sal-recognizer-decl>))
  <sal-recognizer-application>)
(define-method (sal-decl/app-class (decl <sal-accessor-decl>))
  <sal-accessor-application>)

(define-generic (sal-expr/application-class expr))
(define-method (sal-expr/application-class (expr <sal-expr>))
  <sal-application>)
(define-method (sal-expr/application-class (expr <sal-name-expr>))
  (sal-decl/app-class (slot-value expr :decl)))

(define (instance-list-of? obj class)
  (and (list? obj)
       (for-all (cut instance-of? <> class) obj)))
  
(define-api (sal-ast-list? obj)
  :doc "Evaluates to true when @code{obj} is a list of @code{<sal-ast>} instances."
  (instance-list-of? obj <sal-ast>))

(define-api (sal-expr-list? obj)
  :doc "Evaluates to true when @code{obj} is a list of @code{<sal-expr>} instances."
  (instance-list-of? obj <sal-expr>))

(define-api (sal-type-list? obj)
  :doc "Evaluates to true when @code{obj} is a list of @code{<sal-type>} instances."
  (instance-list-of? obj <sal-type>))

(define-api (sal-decl-list? obj)
  :doc "Evaluates to true when @code{obj} is a list of @code{<sal-decl>} instances."
  (instance-list-of? obj <sal-decl>))

(define-generic (sal-ast/reset-cache! ast))
(define-method (sal-ast/reset-cache! (ast <sal-ast>))
  (set-slot-value! ast :internal-idx #f)
  (set-slot-value! ast :hash #f))
(define-method (sal-ast/reset-cache! (ast <sal-type>))
  (set-slot-value! ast :internal-idx #f)
  (set-slot-value! ast :hash #f))
(define-method (sal-ast/reset-cache! (ast <sal-expr>))
  (set-slot-value! ast :internal-idx #f)
  (set-slot-value! ast :hash #f)
  (set-slot-value! ast :type #f))
(define-method (sal-ast/reset-cache! (ast <sal-module>))
  (set-slot-value! ast :internal-idx #f)
  (set-slot-value! ast :hash #f)
  (set-slot-value! ast :state-vars #f)
  (set-slot-value! ast :state-vars-table #f))
(define-method (sal-ast/reset-cache! (ast <sal-base-module>))
  (set-slot-value! ast :internal-idx #f)
  (set-slot-value! ast :hash #f)
  (set-slot-value! ast :state-vars-table #f))
(define-method (sal-ast/reset-cache! (ast <sal-flat-module>))
  (set-slot-value! ast :internal-idx #f)
  (set-slot-value! ast :hash #f)
  (set-slot-value! ast :state-vars-table #f))
(define-method (sal-ast/reset-cache! (ast <sal-esm-module>))
  (set-slot-value! ast :internal-idx #f)
  (set-slot-value! ast :hash #f)
  (set-slot-value! ast :state-vars-table #f))
(define-method (sal-ast/reset-cache! (ast <sal-esm-statement>))
  (set-slot-value! ast :num-alternatives #f))

(define *next-internal-idx* 0)

(define-inline (next-internal-idx)
  (let ((result *next-internal-idx*))
    (set! *next-internal-idx* (+ *next-internal-idx* 1))
    result))

(define (sal-ast/internal-idx ast)
  (unless (slot-value ast :internal-idx)
    (set-slot-value! ast :internal-idx (next-internal-idx)))
  (slot-value ast :internal-idx))

(define (change-ast-class ast class)
  (let ((result (change-class ast class)))
    (sal-ast/reset-cache! result)
    result))

(define *empty-place-source* (make-instance <sal-ast>))

(define (sal-ast/copy-place-information! target place-source)
  [assert (target) (instance-of? target <sal-ast>)]
  [assert (place-source) (instance-of? place-source <sal-ast>)]
  (set-slot-value! target :context (slot-value place-source :context))
  (set-slot-value! target :place (slot-value place-source :place)))

(define (make-sal-identifier place-source name)
  (make-ast-instance <sal-identifier> place-source :name name))

(define-api (sal-ast/size (ast <sal-ast>))
  :doc "Return the size of a @code{<sal-ast>} node."
  :examples '((sal-ast/size (sal/expr "(+ 2 3)")))
  (sal-ast/fold-children (lambda (size child)
                           (+ size (sal-ast/size child)))
                         1
                         ast))

(define-api (sal-ast/place (ast <sal-ast>))
  :doc "Return the place information associated with a @code{<sal-ast>} instance."
  :examples '((let ((n (sal/context "mutex")))
                (sal-place/final-line (sal-ast/place n))))
  (slot-value ast :place))

(define-api (sal-ast/context (ast <sal-ast>))
  :doc "Every instance of @code{<sal-ast>} is owned by a @code{<sal-context>} instance. This fuction returns the owner of the given argument." 
  (slot-value ast :context))

(define-api (sal-ast/sal-env (ast <sal-ast>))
  :doc "Return the instance of @code{<sal-env>} that contains the context which owns the given argument."
  (slot-value (sal-ast/context ast) :sal-env))

(define-api (sal-decl/name (ast <sal-decl>))
  (slot-value (slot-value ast :id) :name))

(define-api (sal-field/name (ast <sal-field>))
  (slot-value (slot-value ast :id) :name))
  
(define-api (sal-record-entry/name (ast <sal-record-entry>))
  (slot-value (slot-value ast :id) :name))

(define-api (sal-ast/context-name (ast <sal-ast>))
  :doc "Return the name of the context which owns the given argument."
  (sal-decl/name (sal-ast/context ast)))

(define-api (sal-ast/context-file-name (ast <sal-ast>))
  :doc "Return the file name of the context which owns the given argument."
  (slot-value (sal-ast/context ast) :file-name))

(define-api (sal-name-ref/id (name-ref <sal-name-ref>))
  (slot-value (sal-name-ref/decl name-ref) :id))

(define-api (sal-name-ref/builtin? (name-ref <sal-name-ref>))
  :doc "Return true when the given argument is a reference to a constant of type declaration found in the SAL prelude."
  (eq? (slot-value (sal-name-ref/decl name-ref) :context)
       (sal-env/prelude (sal-ast/sal-env name-ref))))

(define-api (sal-name-ref/state? (name-ref <sal-name-ref>))
  :doc "Return true when the given argument is a reference to a state variable."
  (instance-of? (sal-name-ref/decl name-ref) <sal-state-var-decl>))

(define-api (sal-name-ref/let-decl? (name-ref <sal-name-ref>))
  :doc "Return true when the given argument is a return to a let declaration."
  (instance-of? (sal-name-ref/decl name-ref) <sal-let-decl>))

(define-api (sal-identifier/name (id <sal-identifier>))
  (slot-value id :name))

;; The methods (sal-name-ref/decl, sal-qualified-name-ref/context-ref, sal-qualified-name-ref/actuals, sal-local-binds-ast/local-decls ast)
;;  were introduced to solve a conflict in the application
;; of the fast slot access optimization. To solver the conflict,
;; the slot :decl is declared in each subclass of <sal-name-ref>
;; Remark: I can generate efficient code for slot access for
;; a class, when all subclasses of that class have
;; the same slot prefix.
(define-generic (sal-name-ref/decl name-ref))
(define-method (sal-name-ref/decl (name-ref <sal-name-expr>))
  (slot-value name-ref :decl))
(define-method (sal-name-ref/decl (name-ref <sal-type-name>))
  (slot-value name-ref :decl))
(define-method (sal-name-ref/decl (name-ref <sal-module-name>))
  (slot-value name-ref :decl))
(define-method (sal-name-ref/decl (name-ref <sal-qualified-assertion-name>))
  (slot-value name-ref :decl))
(define-generic (sal-qualified-name-ref/context-ref qualified-name-ref))
(define-method (sal-qualified-name-ref/context-ref (qualified-name-ref <sal-qualified-name-expr>))
  (slot-value qualified-name-ref :context-ref))
(define-method (sal-qualified-name-ref/context-ref (qualified-name-ref <sal-qualified-type-name>))
  (slot-value qualified-name-ref :context-ref))
(define-method (sal-qualified-name-ref/context-ref (qualified-name-ref <sal-qualified-module-name>))
  (slot-value qualified-name-ref :context-ref))
(define-method (sal-qualified-name-ref/context-ref (qualified-name-ref <sal-qualified-assertion-name>))
  (slot-value qualified-name-ref :context-ref))
(define-generic (sal-qualified-name-ref/actuals qualified-name-ref))
(define-method (sal-qualified-name-ref/actuals (qualified-name-ref <sal-qualified-name-expr>))
  (slot-value qualified-name-ref :actuals))
(define-method (sal-qualified-name-ref/actuals (qualified-name-ref <sal-qualified-type-name>))
  (slot-value qualified-name-ref :actuals))
(define-method (sal-qualified-name-ref/actuals (qualified-name-ref <sal-qualified-module-name>))
  (slot-value qualified-name-ref :actuals))
(define-method (sal-qualified-name-ref/actuals (qualified-name-ref <sal-qualified-assertion-name>))
  (slot-value qualified-name-ref :actuals))
(define-generic (sal-local-binds-ast/local-decls ast))
(define-method (sal-local-binds-ast/local-decls (ast <sal-parametric-module>))
  (slot-value ast :local-decls))
(define-method (sal-local-binds-ast/local-decls (ast <sal-local-binds-expr>))
  (slot-value ast :local-decls))
(define-method (sal-local-binds-ast/local-decls (ast <sal-for-all-definition>))
  (slot-value ast :local-decls))
(define-method (sal-local-binds-ast/local-decls (ast <sal-multi-command>))
  (slot-value ast :local-decls))
(define-method (sal-local-binds-ast/local-decls (ast <sal-multi-composition>))
  (slot-value ast :local-decls))
(define-method (sal-local-binds-ast/local-decls (ast <sal-multi-component-info>))
  (slot-value ast :local-decls))

(define (sal-name-ref/name name-ref)
  [assert (name-ref) (instance-of? name-ref <sal-name-ref>)]
  (sal-identifier/name (sal-name-ref/id name-ref)))

(define (sal-qualified-name-ref/defined-at name-ref)
  [assert (name-ref) (instance-of? name-ref <sal-qualified-name-ref>)]
  (sal-qualified-name-ref/context-ref name-ref))
  
(define (sal-ast/uses-next-operator? ast)
  [assert (ast) (instance-of? ast <sal-ast>)]
  (sal-ast/find (lambda (ast) (instance-of? ast <sal-next-operator>)) ast))

(define (sal-ast/contains-state-variable? ast)
  [instance-check sal-ast/contains-state-variable? ast <sal-ast>]
  (sal-ast/find (lambda (ast)
                  (and (instance-of? ast <sal-name-ref>)
                       (sal-name-ref/state? ast)))
                ast))
     
(define-generic (sal-selection/target selection))
(define-method (sal-selection/target (selection <sal-simple-selection>))
  (slot-value selection :target))
(define-method (sal-selection/target (selection <sal-array-selection>))
  (slot-value selection :fun))

(define-generic (sal-selection/idx selection))
(define-method (sal-selection/idx (selection <sal-simple-selection>))
  (slot-value selection :idx))
(define-method (sal-selection/idx (selection <sal-array-selection>))
  (slot-value selection :arg))

(define-generic (sal-selection/update-target selection new-target))
(define-method (sal-selection/update-target (selection <sal-simple-selection>) (new-target <sal-expr>))
  (copy-ast selection :target new-target))
(define-method (sal-selection/update-target (selection <sal-array-selection>) (new-target <sal-expr>))
  (copy-ast selection :fun new-target))

(define-generic (sal-expr/lhs? expr))
(define-method (sal-expr/lhs? (expr <sal-expr>)) #f)
(define-method (sal-expr/lhs? (expr <sal-name-expr>)) #t)
(define-method (sal-expr/lhs? (expr <sal-next-operator>)) #t)
(define-method (sal-expr/lhs? (expr <sal-selection>))
  (sal-expr/lhs? (sal-selection/target expr)))

(define (sal-expr/next-lhs? expr)
  [instance-check sal-expr/next-lhs? expr <sal-expr>]
  (and (sal-expr/lhs? expr)
       (sal-ast/uses-next-operator? expr)))

;;
;;
(define-generic (sal-lhs/name-expr lhs))
(define-method (sal-lhs/name-expr (lhs <sal-selection>))
  (sal-lhs/name-expr (sal-selection/target lhs)))
(define-method (sal-lhs/name-expr (lhs <sal-next-operator>))
  (sal-lhs/name-expr (slot-value lhs :name-expr)))
(define-method (sal-lhs/name-expr (lhs <sal-name-expr>))
  lhs)

;;
;;
(define-generic (sal-lhs/trivial? lhs))
(define-method (sal-lhs/trivial? (lhs <sal-selection>)) #f)
(define-method (sal-lhs/trivial? (lhs <sal-next-operator>)) #t)
(define-method (sal-lhs/trivial? (lhs <sal-name-expr>)) #t)


(define-generic (sal-lhs/next-operator lhs))
(define-method (sal-lhs/next-operator (lhs <sal-next-operator>))
  lhs)
(define-method (sal-lhs/next-operator (lhs <sal-name-expr>))
  #f)
(define-method (sal-lhs/next-operator (lhs <sal-selection>))
  (sal-lhs/next-operator (sal-selection/target lhs)))
  
(define-generic (sal-lhs/remove-next-operator lhs))
(define-method (sal-lhs/remove-next-operator (lhs <sal-next-operator>))
  (slot-value lhs :name-expr))
(define-method (sal-lhs/remove-next-operator (lhs <sal-name-expr>))
  lhs)
(define-method (sal-lhs/remove-next-operator (lhs <sal-selection>))
  (sal-selection/update-target lhs (sal-lhs/remove-next-operator (sal-selection/target lhs))))

(define-generic (sal-lhs->next-lhs lhs))
(define-method (sal-lhs->next-lhs (lhs <sal-name-expr>))
  (make-ast-instance <sal-next-operator> lhs
                     :name-expr lhs))
(define-method (sal-lhs->next-lhs (lhs <sal-selection>))
  (sal-selection/update-target lhs (sal-lhs->next-lhs (sal-selection/target lhs))))

(define-generic (sal-next-lhs->lhs lhs))
(define-method (sal-next-lhs->lhs (lhs <sal-next-operator>))
  (slot-value lhs :name-expr))
(define-method (sal-next-lhs->lhs (lhs <sal-selection>))
  (sal-selection/update-target lhs (sal-next-lhs->lhs (sal-selection/target lhs))))

;;
;;
(define-generic (sal-decl/recursive? decl))
(define-method (sal-decl/recursive? (decl <sal-decl>))
  #f)
(define-method (sal-decl/recursive? (decl <sal-constant-decl>))
  (sal-ast/contains-reference? decl decl))
(define-method (sal-decl/recursive? (decl <sal-type-decl>))
  (let ((type (slot-value decl :type)))
    (and (instance-of? type <sal-data-type>)
         (exists (lambda (constructor-name-expr)
                   (let* ((constructor-decl (slot-value constructor-name-expr :decl))
                          (constructor-type (slot-value constructor-decl :type)))
                     (and (sal-type/function? constructor-type)
                          (sal-ast/contains-reference? (sal-function-type/domain constructor-type) decl))))
                 (slot-value type :constructors)))))

(define (sal-tuple-position->integer pos)
  [assert (pos) (or (instance-of? pos <sal-numeral>)
                    (mpq? pos)
                    (integer? pos))]
  (let* ((aux (if (instance-of? pos <sal-numeral>)
                (slot-value pos :num)
                pos)))
    (if (mpq? aux)
      (mpq->integer aux)
      aux)))

(define (sal-tuple/element tuple slot-id pos)
  (let ((pos (sal-tuple-position->integer pos)))
    [assert (pos tuple) (and (>= pos 1) (<= pos (length (slot-value tuple slot-id))))]
    (list-ref (slot-value tuple slot-id) (- pos 1))))

(define (sal-record/element record slot-id item-slot-id id)
  [assert (id) (or (instance-of? id <sal-identifier>)
                   (symbol? id))]
  (let ((id (if (instance-of? id <sal-identifier>)
              (slot-value id :name)
              id)))
    (cond
     ((find (lambda (curr) (eq? id (slot-value (slot-value curr :id) :name)))
            (slot-value record slot-id)) 
      =>
      (cut slot-value <> item-slot-id))
     (else
      (sign-error record "Invalid record access, unknown field `~a'." id)))))

(define-generic (sal-ast/check-number-of-actuals ast))

(define (check-number-of-actuals ast ctx-ref actual-list)
  (let ((param-list (slot-value ctx-ref :params)))
    (unless (= (length actual-list) (length param-list))
      (sign-source-error ast
                         "Invalid context reference, context \"~a\" expects ~a parameter(s), but ~a was(were) provided." 
                         (sal-context/name ctx-ref) (length param-list) (length actual-list)))))

(define-method (sal-ast/check-number-of-actuals (ast <sal-qualified-name-ref>))
  (check-number-of-actuals ast (sal-qualified-name-ref/context-ref ast) (sal-qualified-name-ref/actuals ast)))

(define-method (sal-ast/check-number-of-actuals (ast <sal-context-name-decl>))
  (check-number-of-actuals ast (slot-value ast :context-ref) (slot-value ast :actuals)))
  
(define-method (sal-ast/check-number-of-actuals (ast <sal-module-instance>))
  (let* ((definition (sal-module-name/definition (slot-value ast :module-name)))
         (local-decl-list (slot-value definition :local-decls))
         (actual-list (slot-value ast :actuals)))
    (unless (= (length local-decl-list) (length actual-list))
      (sign-source-error ast
                         "Invalid module instance, module \"~a\" expects ~a parameter(s), but ~a was(were) provided." 
                         (sal-name-ref/name (slot-value ast :module-name))
                         (length local-decl-list) (length actual-list)))))

(define-api (sal-type-list->sal-decl-list (type-list sal-type-list?))
  (map (lambda (type)
         (let* ((arg-name (gen-unique-name 'arg))
                (arg-id (make-sal-identifier type arg-name)))
           (make-ast-instance <sal-var-decl> type
                              :id arg-id
                              :type type)))
       type-list))

(define-generic (sal-command/guard ast))
(define-method (sal-command/guard (ast <sal-guarded-command>))
  (slot-value ast :guard))
(define-method (sal-command/guard (ast <sal-labeled-command>))
  (sal-command/guard (slot-value ast :command)))
(define-method (sal-command/guard (ast <sal-multi-command>))
  (make-ast-instance <sal-exists-expr> ast
                     :local-decls (slot-value ast :local-decls)
                     :expr (sal-command/guard (slot-value ast :command))))

(define-generic (sal-else-command/assignments command))
(define-method (sal-else-command/assignments (command <sal-labeled-command>)) (sal-else-command/assignments (slot-value command :command)))
(define-method (sal-else-command/assignments (command <sal-else-command>)) (slot-value command :assignments))

(define (sal-ast-list/sort-using-ids ast-list)
  (sort ast-list
        (lambda (ast1 ast2)
                   (string<? (symbol->string (sal-identifier/name (slot-value ast1 :id)))
                             (symbol->string (sal-identifier/name (slot-value ast2 :id)))))))

(define (make-qualified-name-expr-based-on symbol qualified-name)
  (let* ((context (slot-value qualified-name :context-ref))
         (decl (sal-context/constant-declaration context symbol)))
    (unless decl
      (sign-error "Unknown constant `~a' in context `~a'." symbol (sal-context/name context)))
    (let ((result (change-class qualified-name <sal-qualified-name-expr>)))
      (set-slot-value! result :decl decl)
      result)))

         
