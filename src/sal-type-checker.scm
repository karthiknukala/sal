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

(module sal-type-checker
        (include "sal.sch")
        (include "sal-ast-table.sch")
        (import queue sal-expression sal-type gmp-scheme symbol-table sal-module sal-ast-support
                sal-ast-eq sal-context sal-ast-list sal-ast-instantiate sal-pp sal-environment
                sal-ast-for-each sal-decls)
        (export (sal-ast/type-check ast)
                <sal-type-checker-info>
                (sal-type-check ast info)
                (check-set-list-expression-arguments place-source expr-list))
        )

(define-macro (type-check-slot-def-list . slot-def-list)
  `(begin
     ,@(map (lambda (slot-def)
              (cond 
               ((keyword? slot-def)
                `(sal-type-check (slot-value ast ,slot-def) info))
               ((optional-slot-def? slot-def)
                `(when (slot-value ast ,(car slot-def))
                   (sal-type-check (slot-value ast ,(car slot-def)) info)))
               ((list-slot-def? slot-def)
                `(for-each (cut sal-type-check <> info) (slot-value ast ,(car slot-def))))
               ((queue-slot-def? slot-def)
                `(for-each (cut sal-type-check <> info) (queue->list (slot-value ast ,(car slot-def)))))
               ((and (list? slot-def)
                     (eq? (car slot-def) 'begin))
                slot-def)
               (else
                (error 'traverse "Invalid slot definition." #unspecified))))
            slot-def-list)))

(define (sal-type->string type)
  (with-output-to-string
    (lambda ()
      (case (verbosity-level)
        ((0) (display ""))
        ((1) (sal/pp-simple type))
        (else (sal/pp-detailed type))))))

(define (error-epilogue)
  (if (> (verbosity-level) 1) 
    ""
    (string-append "\nRemark: use --verbose=num (num > 1) to obtail a more detailed description of the incompatible types.")))

(define (sign-incompatible-types-error place type1 type2 msg . args)
  (let ((msg (string-append "Incompatible types " msg "\n"
                            (if (= (verbosity-level) 0)
                              "Remark: use --verbose=num (num > 0) to display the incompatible types."
                             (string-append "The following types are incompatible:\n"
                                            (sal-type->string type1)
                                            "\n\n"
                                            (sal-type->string type2)
                                            (error-epilogue))))))
    (apply sign-source-error place msg args)))

(define (sign-type-mismatch-error place type1 type2 msg . args)
  ;; (breakpoint "sign-type-mismatch-error" (type1 type2) #t)
  (let ((msg (string-append "Type mismatch " msg "\n"
                            (if (= (verbosity-level) 0)
                              "Remark: use --verbose=num (num > 0) to display the incompatible types."
                              (string-append "Expected type:\n"
                                             (sal-type->string type1)
                                             "\nActual type:\n"
                                             (sal-type->string type2)
                                             (error-epilogue))))))
    (apply sign-source-error place msg args)))

(define (check-set-list-expression-arguments place-source exprs)
  [assert (exprs) (not (null? exprs))]
  (let ((expr-types (map sal-expr/type exprs)))
    (let ((t1 (car expr-types))
          (expr1 (car exprs))
          (rest-types (cdr expr-types))
          (rest-exprs (cdr exprs)))
      (for-each (lambda (t2 expr2)
                  (unless (sal-type/equivalent? t1 t2)
                    (sign-incompatible-types-error place-source t1 t2 "in set list expression. The incompatible elements are located at: ~a and ~a."
                                                   (format-with-location expr1 "")
                                                   (format-with-location expr2 ""))))
                rest-types rest-exprs))))

(define (sign-boolean-type-mismatch-error place type2 msg . args)
  (apply sign-type-mismatch-error place (make-sal-builtin-name <sal-bool-type> place) type2 msg args))

(define (sign-number-type-mismatch-error place type2 msg . args)
  (apply sign-type-mismatch-error place (make-sal-builtin-name <sal-number-type> place) type2 msg args))

(define (sign-integer-type-mismatch-error place type2 msg . args)
  (apply sign-type-mismatch-error place (make-sal-builtin-name <sal-int-type> place) type2 msg args))

(define-macro (type-check-method class . slot-def-list)
  `(define-method (sal-type-check (ast ,class) (info <sal-type-checker-info>))
     (type-check-slot-def-list ,@slot-def-list)))

(define-class <sal-type-checker-info> () (:already-defined-positions :base-module-region :inside-property? :logic)) 

(define-generic (sal-type-check ast info))

(define (sal-ast/type-check ast)
  (sal-type-check ast (make-instance <sal-type-checker-info> 
                                     :inside-property? #f
                                     :logic #f
                                     :already-defined-positions (make-symbol-table))))

(define-method (sal-type-check (ast <sal-ast>) (info <sal-type-checker-info>))
  #t)

;;  (put 'type-check-method 'scheme-indent-function 3)


(define (check-if-match-params-and-arguments param-list actual-list proc-param-type)
  [assert (param-list actual-list) (= (length param-list) (length actual-list))]
  (for-each 
   (lambda (param actual)
     (when (instance-of? param <sal-var-decl>)
       (let ((var-param-type (proc-param-type (slot-value param :type)))
             (actual-expr-type (sal-expr/type actual)))
         (unless (sal-type/equivalent? var-param-type actual-expr-type)
           (sign-type-mismatch-error actual var-param-type actual-expr-type "in actual expression.")))))
   param-list actual-list))

(define (check-context-ref node-with-context-ref)
  (let ((ctx-ref (slot-value node-with-context-ref :context-ref)))
    (when ctx-ref
      (sal-ast/check-number-of-actuals node-with-context-ref)
      (let* ((actual-list (slot-value node-with-context-ref :actuals))
             (param-list (slot-value ctx-ref :params))
             (proc-param-type (lambda (param-type)
                                (sal-ast/instantiate param-type actual-list))))
        (check-if-match-params-and-arguments param-list actual-list proc-param-type)))))

(type-check-method <sal-context> (:params list) (:declarations queue))
(type-check-method <sal-typed-decl> :type)
(type-check-method <sal-type-decl> (:type optional)
  (begin
    (when (and (slot-value ast :type)
               (not (instance-of? (slot-value ast :type) <sal-data-type>))
               (sal-ast/contains-reference? (slot-value ast :type) ast))
      (sign-source-error ast "Invalid recursive type declaration"))))
(type-check-method <sal-module-decl> :parametric-module)
(type-check-method <sal-context-name-decl> (:actuals list) 
  (begin 
    (check-context-ref ast)))
(type-check-method <sal-parametric-module> (:local-decls list) :module)
(type-check-method <sal-let-decl> :type :value 
  (begin
    (let ((type1 (slot-value ast :type))
          (type2 (sal-expr/type (slot-value ast :value))))
      (unless (sal-type/equivalent? type1 type2)
        (sign-type-mismatch-error ast type1 type2 "in let declaration.")))))
(type-check-method <sal-constant-decl> :type (:value optional)
  (begin
    (when (slot-value ast :value)
      (let ((type1 (slot-value ast :type))
            (type2 (sal-expr/type (slot-value ast :value))))
        (unless (sal-type/equivalent? type1 type2)
          (sign-type-mismatch-error ast type1 type2 "in constant declaration."))))))
;; Small hack: The domain of function types cannot contain embedded symmetric types, unless it *is* a symmetric type.
;; See the comment in the method for <sal-function-type>. This restriction is not enforced in constructors. So,
;; I use an specialziation for constructor, accessor and recognizer declarations to avoid this problem.
(type-check-method <sal-constructor-decl>
  (begin
    (let ((type (slot-value ast :type)))
      (cond
       ((sal-type/function? type)
        (sal-type-check (sal-function-type/domain type) info)
        (sal-type-check (sal-function-type/range type) info))
       (else
        (sal-type-check type info))))))
(type-check-method <sal-accessor-decl>)
(type-check-method <sal-recognizer-decl>)
(type-check-method <sal-qualified-name-expr> (:actuals list)
  (begin
    (check-context-ref ast)))
(type-check-method <sal-qualified-type-name> (:actuals list)
  (begin
    (check-context-ref ast)))
(type-check-method <sal-qualified-module-name> (:actuals list)
  (begin
    (check-context-ref ast)))
(type-check-method <sal-qualified-assertion-name> (:actuals list)
  (begin
    (check-context-ref ast)))
(type-check-method <sal-assertion-decl> :assertion-expr)
(type-check-method <sal-function-type> :domain :range
  (begin
    (let ((domain (slot-value ast :domain)))
      (when (and (not (sal-type/scalar-set? domain))
                 (not (sal-type/ring-set? domain))
                 (sal-type/find (lambda (t) (instance-of? t <sal-symmetric-type>)) domain))
        (sign-source-error ast "The domain of a function type cannot contain a SCALARSET or RINGSET, unless it *is* a SCALARSET or RINGSET.")))))
(type-check-method <sal-tuple-type> (:types list))
(type-check-method <sal-record-type> (:fields list))
(type-check-method <sal-field> :id :type)
(type-check-method <sal-state-type> :module)
(define (check-type-contain-ref-to-state-var ast)
  (let ((state? (sal-ast/contains-state-variable? ast)))
    (when state?
      (sign-source-error state? "Type cannot contain reference to a state variable."))))
(type-check-method <sal-subtype> :expr
  (begin
    ;; (when (sal-type/find (lambda (t) (and (not (eq? t ast)) (instance-of? t <sal-symmetric-type>))) ast)
    ;;  (sign-source-error ast "Subtypes of SCALARSETs and RINGSETs are not supported."))
    (check-type-contain-ref-to-state-var ast)))
(type-check-method <sal-scalar-type> (:scalar-elements list))
(type-check-method <sal-data-type> (:constructors list)
  (begin
    (when (sal-type/negative-occurrence? ast ast)
      (sign-source-error ast "Recursive uses of the given datatype must not appear in the domain of a function type. The violation of this condition in the recursion leads to contradictions. For example, a datatype T with an accessor of type [T -> bool] would yield a contradiction since the cardinality of [T -> bool] is that of the power-set of T which by Cantor's theorem must be strictly greater than the cardinality of T. However, we have that distinct accessor elements lead to distinct datatype elements as well, and hence a contradiction."))))
(type-check-method <sal-expr>)
(type-check-method <sal-name-expr>)
(type-check-method <sal-qualified-name-expr> (:actuals list)
  (begin
    (check-context-ref ast)))
(type-check-method <sal-local-binds-expr> (:local-decls list) :expr)
(type-check-method <sal-tuple-literal> (:exprs list))
(type-check-method <sal-record-literal> (:entries list))
(type-check-method <sal-record-entry> :id :expr)
(type-check-method <sal-propositional-application> :fun :arg
  (begin 
    (for-each (lambda (expr)
                (unless (sal-type/boolean? (sal-expr/type expr))
                  (sign-boolean-type-mismatch-error expr (sal-expr/type expr) "in boolean operator.")))
              (sal-application/argument-list ast))))
(define (check-arg-is-not-scalar-set arg)
  (when (sal-type/scalar-set? (sal-expr/type arg))
    (sign-source-error arg "Invalid use of SCALARSET element.")))
(define (check-arg-is-not-ring-set arg)
  (when (sal-type/ring-set? (sal-expr/type arg))
    (sign-source-error arg "Invalid use of RINGSET element.")))
(define (check-arguments-are-numbers app)
  ;;  (breakpoint "foo" (app) #t)
  (for-each (lambda (arg)
              (unless (sal-type/number? (sal-expr/type arg))
                ;; (breakpoint "arith" (app arg) #t)
                (sign-number-type-mismatch-error arg (sal-expr/type arg) "in arithmetical operator."))
              (check-arg-is-not-scalar-set arg)
              (check-arg-is-not-ring-set arg))
            (sal-application/argument-list app)))
(define (check-arguments-are-integers app)
  (for-each (lambda (arg)
              (unless (sal-type/integer? (sal-expr/type arg))
                (sign-integer-type-mismatch-error arg (sal-expr/type arg) "in arithmetical operator."))
              (check-arg-is-not-scalar-set arg)
              (check-arg-is-not-ring-set arg))
            (sal-application/argument-list app)))
(type-check-method <sal-arith-application> :fun :arg
  (begin
    (check-arguments-are-numbers ast)))
(type-check-method <sal-idiv> :fun :arg
  (begin
    (check-arguments-are-integers ast)))
(type-check-method <sal-mod> :fun :arg
  (begin
    (check-arguments-are-integers ast)))

(define (type-check-eq ast)
  (multiple-value-bind
        (arg1 arg2)
        (sal-binary-application/arguments ast)
    (let ((type1 (sal-expr/type arg1))
          (type2 (sal-expr/type arg2)))
      (unless (sal-type/equivalent? type1 type2)
        (sign-incompatible-types-error ast type1 type2 "in the equality operator.")))))

(type-check-method <sal-eq> :fun :arg
  (begin
    (type-check-eq ast)))

(type-check-method <sal-diseq> :fun :arg
  (begin
    (type-check-eq ast)))

(type-check-method <sal-definition-expression> 
  (begin
    (unless (sal-type/boolean? (sal-expr/type (slot-value ast :expr)))
      (sign-boolean-type-mismatch-error ast (sal-expr/type (slot-value ast :expr)) 
                                        "in definition expression, the right-hand-side must be a boolean expression."))))
(type-check-method <sal-labeled-command> :expr)
(type-check-method <sal-debug-expr> :fun :arg
  (begin
    (let ((arg-list (sal-application/argument-list ast)))
      (unless (= (length arg-list) 1)
        (sign-source-error ast "Invalid number of arguments in debugging primitive. The operator dbg_expr has only 1 parameter.")))))
(type-check-method <sal-debug-print> :fun :arg
  (begin
    (let ((arg-list (sal-application/argument-list ast)))
      (when (instance-of? (list-last-element arg-list) <sal-string-expr>)
        (sign-source-error ast "The last element in a debugging primitive cannot be a string")))))
(type-check-method <sal-application> :fun :arg
  (begin
    (if (slot-value info :inside-property?)
      (when (instance-of? ast <sal-temporal-application>)
        (let ((previous-used-logic (slot-value info :logic))
              (current-used-logic (sal-application/logic-id ast)))
          (when (and previous-used-logic (not (eq? previous-used-logic current-used-logic)))
            (sign-source-error ast "Invalid property, temporal operators of different logics cannot be mixed in the same property. This is a ~a operator, but ~a operator(s) was(were) used before." current-used-logic previous-used-logic))
          (set-slot-value! info :logic current-used-logic)))
      ;; (when (instance-of? ast <sal-temporal-application>)
      ;;  (sign-source-error ast "Invalid expression, temporal logic operators can only be used to specify properties."))
      )

    (let* ((fun (slot-value ast :fun))
           (arg (slot-value ast :arg))
           (fun-type (sal-expr/type fun))
           (arg-type (sal-expr/type arg)))
      (unless (sal-type/function? fun-type)
        (sign-source-error fun "Invalid function application, expression must be a function."))
      (unless (sal-type/equivalent? (sal-function-type/domain fun-type) arg-type)
        (sign-type-mismatch-error arg (sal-function-type/domain fun-type) arg-type "in the function application.")))))
(type-check-method <sal-quantified-expr> (:local-decls list) :expr
  (begin
    (unless (sal-type/boolean? (sal-expr/type (slot-value ast :expr)))
      (sign-boolean-type-mismatch-error ast (sal-expr/type (slot-value ast :expr)) "in quantified expression, the type of the body of the quantified expression must be boolean."))))
(type-check-method <sal-ring-application> :fun :arg
  (begin
    (let* ((arg (slot-value ast :arg))
           (arg-type (sal-expr/type arg)))
      (unless (sal-type/ring-set? arg-type)
        (sign-source-error arg "Invalid use of RINGSET operation, argument is to a RINGSET.")))))
(type-check-method <sal-array-selection> :fun :arg
  (begin
    (trace 'type-checker "type checking array selection: ~a" (sal-ast->list ast))
    (let* ((array (slot-value ast :fun))
           (idx (slot-value ast :arg))
           (array-type (sal-expr/type array))
           (idx-type (sal-expr/type idx)))
      (unless (sal-type/function? array-type) ;; do not differenciate between arrays and functions: Shankar suggestion
        (trace 'type-checker "Error, expression must be an array: ~a" (sal-ast->list array-type))
        (sign-source-error array "Invalid array selection, expression must be an array."))
      (unless (sal-type/equivalent? (sal-function-type/domain array-type) idx-type)
        (sign-type-mismatch-error idx (sal-function-type/domain array-type) idx-type
                                  "in array selection, invalid index.")))))
(type-check-method <sal-tuple-selection> :target :idx
  (begin
    (let* ((target (slot-value ast :target))
           (target-type (sal-expr/type target))
           (idx (slot-value ast :idx))
           (pos (mpq->integer (slot-value idx :num))))
      (unless (sal-type/tuple? target-type)
        (sign-source-error target "Invalid tuple selection, expression must be a tuple."))
      (let ((num-elems (length (sal-tuple-type/types target-type))))
        (unless (and (>= pos 1) (<= pos num-elems))
          (sign-source-error idx "Invalid index in tuple selection. The index should be in the interval [1,~a]." num-elems))))))
(type-check-method <sal-record-selection> :target :idx
  (begin
    (let* ((target (slot-value ast :target))
           (target-type (sal-expr/type target))
           (idx (slot-value ast :idx))
           (field-name (slot-value idx :name)))
      (unless (sal-type/record? target-type)
        (sign-source-error target "Invalid record selection, expression must be a record."))
      (unless (exists (lambda (field)
                        (eq? field-name (slot-value (slot-value field :id) :name)))
                      (sal-record-type/fields target-type))
        (sign-source-error idx "Invalid record selection, unknown field name.")))))
(type-check-method <sal-function-update> :target :idx :new-value
  (begin
    (let* ((target (slot-value ast :target))
           (idx (slot-value ast :idx))
           (new-value (slot-value ast :new-value))
           (target-type (sal-expr/type target))
           (idx-type (sal-expr/type idx))
           (new-value-type (sal-expr/type new-value)))
      (unless (sal-type/function? target-type)
        (sign-source-error target "Invalid function (array) update, expression must be a function (array)."))
      (unless (sal-type/equivalent? (sal-function-type/domain target-type) idx-type)
        ;; (breakpoint "xxxx" (target-type idx-type ast idx new-value) #t)
        (sign-type-mismatch-error idx (sal-function-type/domain target-type) idx-type
                                  "in function (array) update, invalid function (array) index."))     
      (unless (sal-type/equivalent? (sal-function-type/range target-type) new-value-type)
        ;;(breakpoint "type-check" (ast target-type new-value new-value-type idx-type) #t)
        (sign-type-mismatch-error new-value (sal-function-type/range target-type) new-value-type
                                  "in function (array) update, invalid new value.")))))
(type-check-method <sal-tuple-update> :target :idx :new-value
  (begin
    (let* ((target (slot-value ast :target))
           (target-type (sal-expr/type target))
           (idx (slot-value ast :idx))
           (pos (mpq->integer (slot-value idx :num)))
           (new-value (slot-value ast :new-value))
           (new-value-type (sal-expr/type new-value)))
      (unless (sal-type/tuple? target-type)
        (sign-source-error target "Invalid tuple update, expression must be a tuple."))
      (let ((num-elems (length (sal-tuple-type/types target-type))))  
        (unless (and (>= pos 1) (<= pos num-elems))
          (sign-source-error idx "Invalid index in tuple update. The index should be in the interval [1,~a]." num-elems))
        (let ((old-value-type (sal-tuple-type/element-type target-type pos)))
          (unless (sal-type/equivalent? new-value-type old-value-type)
            (sign-type-mismatch-error new-value old-value-type new-value-type
                                      "in tuple update, invalid new value.")))))))
(type-check-method <sal-record-update> :target :idx :new-value
  (begin 
    (let* ((target (slot-value ast :target))
           (target-type (sal-expr/type target))
           (idx (slot-value ast :idx))
           (field-name (slot-value idx :name))
           (new-value (slot-value ast :new-value))
           (new-value-type (sal-expr/type new-value)))
      (unless (sal-type/record? target-type)
        (sign-source-error target "Invalid record update, expression must be a record."))
      (unless (exists (lambda (field)
                        (and (eq? field-name (slot-value (slot-value field :id) :name))
                             (if (sal-type/equivalent? (slot-value field :type) new-value-type)
                               #t
                               (sign-type-mismatch-error new-value (slot-value field :type) new-value-type
                                                         "in record update, invalid new value."))))
                      (sal-record-type/fields target-type))
        (sign-source-error idx "Invalid record update, unknown field name.")))))
(type-check-method <sal-conditional> :cond-expr :then-expr :else-expr
  (begin
    (unless (sal-type/boolean? (sal-expr/type (slot-value ast :cond-expr)))
      (sign-boolean-type-mismatch-error (slot-value ast :cond-expr) (sal-expr/type (slot-value ast :cond-expr))
                                        "in `if' condition."))
    (let ((type1 (sal-expr/type (slot-value ast :then-expr)))
          (type2 (sal-expr/type (slot-value ast :else-expr))))
      (unless (sal-type/equivalent? type1 type2)
        (sign-incompatible-types-error ast type1 type2 "in conditional expression, incompatible branches.")))))
(type-check-method <sal-next-operator> :name-expr
  (begin
    (let ((name-expr (slot-value ast :name-expr)))
      (unless (instance-of? name-expr <sal-name-expr>)
        (sign-source-error ast "Invalid use of next operator, argument must be a name expression."))
      (let ((decl (slot-value name-expr :decl)))
        (unless (instance-of? decl <sal-state-var-decl>)
          (sign-source-error ast "Invalid use of next operator, variable is not a state variable."))
;         this isn't a true constraint
;         (when (instance-of? decl <sal-input-state-var-decl>)
;           (sign-source-error ast "Invalid use of next operator, variable is an input."))
        ))))
(type-check-method <sal-mod-init> :module)
(type-check-method <sal-mod-trans> :module)

(define (append-already-defined info lhs)
  (let* ((var-name (sal-name-ref/name (sal-lhs/name-expr lhs)))
         (table (slot-value info :already-defined-positions))
         (curr-entry (cond
                      ((symbol-table/lookup table var-name) =>
                       identity)
                      (else 
                       '()))))
    (set-slot-value! info :already-defined-positions (symbol-table/add table var-name (cons (sal-lhs/remove-next-operator lhs) curr-entry)))))

(define-method (sal-type-check (ast <sal-base-module>) (info <sal-type-checker-info>))
  (for-each (lambda (decl)
              (sal-type-check decl info))
            (slot-value ast :state-vars))
  (set-slot-value! info :already-defined-positions (make-symbol-table))
  (set-slot-value! info :base-module-region 'definition)
  (for-each (cut sal-type-check <> info) (slot-value ast :definitions))
  (let ((already-defined-after-definitions (slot-value info :already-defined-positions)))
    (set-slot-value! info :base-module-region 'initialization)
    (for-each (cut sal-type-check <> info) (slot-value ast :initialization-definitions))
    (when (slot-value ast :initialization-command-section)
      (sal-type-check (slot-value ast :initialization-command-section) info))
    ;; restore already-defined-after-definitions
    (set-slot-value! info :already-defined-positions already-defined-after-definitions)
    (set-slot-value! info :base-module-region 'transition)
    (for-each (cut sal-type-check <> info) (slot-value ast :transition-definitions))
    (when (slot-value ast :transition-command-section)
      (sal-type-check (slot-value ast :transition-command-section) info))))
  
(define-method (sal-type-check (ast <sal-flat-module>) (info <sal-type-checker-info>))
  (for-each (lambda (decl)
              (sal-type-check decl info))
            (slot-value ast :state-vars))
  (sal-type-check (slot-value ast :initialization) info)
  (sal-type-check (slot-value ast :definition) info)
  (sal-type-check (slot-value ast :transition) info)
  (sal-type-check (slot-value ast :skip) info))

(define (check-if-position-was-already-defined info lhs)
  (let* ((table (slot-value info :already-defined-positions))
         (var-name (sal-name-ref/name (sal-lhs/name-expr lhs)))
         (entry (symbol-table/lookup table var-name))
         (core-lhs (sal-lhs/remove-next-operator lhs)))
    (when entry
      (for-each (lambda (defined-lhs)
                  (when (sal-ast/equivalent? defined-lhs core-lhs)
                    (sign-source-error lhs "Invalid simple definition, trying to define the same location (left-hand-side) twice, this definition conflicts with the one defined at ~a."
                                       (format-with-location defined-lhs ""))))
                entry))))
                  
(define (check-definition ast info)
  (let ((lhs (slot-value ast :lhs))
        (rhs (slot-value ast :rhs)))

    (check-if-position-was-already-defined info lhs)

    (when (and (memq (slot-value info :base-module-region) '(definition initialization))
               (sal-ast/uses-next-operator? ast))
      (sign-source-error ast "Invalid use of next operator in the initialization/definition section."))
    (when (and (eq? (slot-value info :base-module-region) 'transition)
               (not (sal-lhs/next-operator lhs)))
      (sign-source-error ast "Invalid definition, left-hand-side must use the next operator in the transition section."))
    (when (and (sal-ast/find (lambda (curr-ast)
                               (sal-ast/equivalent? curr-ast lhs))
                             rhs))
      (sign-source-error ast "Invalid `recursive' definition, the right-hand-side uses the left-hand-side."))
    (let ((lhs-var (sal-lhs/name-expr lhs)))
      (when (instance-of? (slot-value lhs-var :decl) <sal-input-state-var-decl>)
        (sign-source-error ast "Invalid definition, \"~a\" is an input variable." (sal-name-ref/name lhs-var)))
      (when (and (eq? (slot-value info :base-module-region) 'definition)
                 (instance-of? (slot-value lhs-var :decl) <sal-global-state-var-decl>))
        (sign-source-error ast "Invalid definition, \"~a\" is a global variable, and global variables cannot be defined in the definition section."
                           (sal-name-ref/name lhs-var))))))

(define (throw-lhs-exception expr)
  (sign-source-error expr "Expression is not a valid left-hand-side."))

(type-check-method <sal-simple-definition> :lhs :rhs
  (begin
    (check-definition ast info)
    (let ((lhs (slot-value ast :lhs))
          (rhs (slot-value ast :rhs)))
      (unless (sal-expr/lhs? lhs)
        (throw-lhs-exception lhs))
      (unless (sal-type/equivalent? (sal-expr/type lhs) (sal-expr/type rhs))
        (sign-incompatible-types-error ast (sal-expr/type lhs) (sal-expr/type rhs) 
                                 "in assignment."))
      (append-already-defined info lhs))))
        
(type-check-method <sal-simple-selection-definition> :lhs :rhs
  (begin
    (check-definition ast info)
    (let* ((lhs (slot-value ast :lhs))
           (lhs-type (sal-expr/type lhs))
           (rhs (slot-value ast :rhs))
           (rhs-type (sal-expr/type rhs)))
      (unless (sal-expr/lhs? lhs)
        (throw-lhs-exception lhs))
      (unless (sal-type/predicate? rhs-type)
        (sign-source-error rhs "Invalid IN definition, right-hand-side must be a predicate."))
      (unless (sal-type/equivalent? lhs-type (sal-function-type/domain rhs-type))
        (sign-incompatible-types-error ast lhs-type (sal-function-type/domain rhs-type) 
                                 "in nondeterministic assignment.")))))

(type-check-method <sal-for-all-definition> (:local-decls list) (:definitions list))

(define-method (sal-type-check (ast <sal-command-section>) (info <sal-type-checker-info>))
  (let ((already-defined-cache (slot-value info :already-defined-positions)))
    (for-each (lambda (command)
                (set-slot-value! info :already-defined-positions already-defined-cache)
                (sal-type-check command info))
              (slot-value ast :commands))
    (set-slot-value! info :already-defined-positions already-defined-cache)
    (when (slot-value ast :else-command)
      (sal-type-check (slot-value ast :else-command) info))))

(type-check-method <sal-labeled-command> :command)
(type-check-method <sal-multi-command> (:local-decls list) :command)
(type-check-method <sal-else-command> (:assignments list))
(type-check-method <sal-guarded-command> :guard (:assignments list)
  (begin
    (unless (sal-type/boolean? (sal-expr/type (slot-value ast :guard)))
      (trace 'type-checker "Invalid guard type: ~a" (sal-ast->list (sal-expr/type (slot-value ast :guard))))
      (sign-boolean-type-mismatch-error (slot-value ast :guard) (sal-expr/type (slot-value ast :guard)) 
                                        "in the guard."))))

(type-check-method <sal-org-module> :module)
(type-check-method <sal-with-module> (:new-state-vars list) :module)
(type-check-method <sal-multi-composition> (:local-decls list) :module
  (begin
    (unless (= (length (slot-value ast :local-decls)) 1)
      (sign-source-error ast "Invalid multicomposition, it contains more than one local declaration."))))

(define (check-common-inputs ast)
  (let* ((mod1 (slot-value ast :module1))
         (mod2 (slot-value ast :module2))
         (state-vars1 (sal-module/state-variables mod1)))
    (for-each
     (lambda (decl1)
       (when (instance-of? decl1 <sal-input-state-var-decl>)
         (cond 
          ((sal-module/lookup-var mod2 (sal-decl/name decl1))
           (lambda (decl2)
             (when (instance-of? decl2 <sal-input-state-var-decl>)
               (unless (sal-type/equivalent? (slot-value decl1 :type) (slot-value decl2 :type))
                 (sign-incompatible-types-error ast (slot-value decl1 :type) (slot-value decl2 :type)
                                          "in the common input port \"~a\", invalid composition." (sal-decl/name decl1)))))))))
       state-vars1)))
    
(define (check-connections ast state-vars1 mod2)
  (for-each
   (lambda (decl1)
     (when (instance-of? decl1 <sal-input-state-var-decl>)
       (cond
        ((sal-module/lookup-var mod2 (sal-decl/name decl1)) =>
         (lambda (decl2)
           (when (or (instance-of? decl2 <sal-output-state-var-decl>)
                     (instance-of? decl2 <sal-global-state-var-decl>))
             (unless (sal-type/equivalent? (slot-value decl1 :type) (slot-value decl2 :type))
               (sign-incompatible-types-error ast (slot-value decl1 :type) (slot-value decl2 :type) 
                                        "in connecting the port \"~a\", invalid composition." (sal-decl/name decl1))))
           (when (instance-of? decl2 <sal-input-state-var-decl>)
             (unless (sal-type/equivalent? (slot-value decl1 :type) (slot-value decl2 :type))
               (sign-incompatible-types-error ast (slot-value decl1 :type) (slot-value decl2 :type) 
                                              "in sharing the input port \"~a\", invalid composition." (sal-decl/name decl1))))
           )))))
   state-vars1))

(type-check-method <sal-module-composition> :module1 :module2
  (begin
    (check-common-inputs ast)
    (check-connections ast (sal-module/state-variables (slot-value ast :module1)) (slot-value ast :module2))
    (check-connections ast (sal-module/state-variables (slot-value ast :module2)) (slot-value ast :module1))))

(define-method (sal-type-check (ast <sal-observer>) (info <sal-type-checker-info>))
  (call-next-method)
  (let ((observer (slot-value ast :module2)))
    (check-observer observer (make-sal-ast-table) observer)))

(define-generic (check-observer ast found-names root))
(define-method (check-observer (ast <sal-ast>) (found-names <primitive>) (root <sal-ast>))
  (sal-ast/for-each-children (cut check-observer <> found-names root) ast))
(define-method (check-observer (ast <sal-base-module>) (found-names <primitive>) (root <sal-ast>))
  (unless (and (null? (slot-value ast :initialization-definitions))
               (not (slot-value ast :initialization-command-section))
               (null? (slot-value ast :transition-definitions))
               (not (slot-value ast :transition-command-section)))
    (sign-source-error root "Invalid observer module. Observers only have definition sections. This is not the case at ~a."
                       (format-with-location ast ""))))
(define-method (check-observer (ast <sal-module-name>) (found-names <primitive>) (root <sal-ast>))
  (unless (sal-ast-table/contains? found-names ast)
    (sal-ast-table/put! found-names ast #t)
    (let ((def (sal-module-name/definition ast)))
      (check-observer def found-names root))))
                
(type-check-method <sal-renaming> (:renames list) :module
  (begin
    (let ((nested-mod (slot-value ast :module))) 
      (for-each (lambda (rename)
                  (let* ((from-var (sal-identifier/name (slot-value rename :from-name)))
                         (from-var-decl (sal-module/lookup-var nested-mod from-var))
                         (from-var-type (slot-value from-var-decl :type))
                         (to-expr-type (sal-expr/type (slot-value rename :to-expr))))
                    (unless (sal-type/equivalent? from-var-type to-expr-type)
                      (sign-incompatible-types-error rename from-var-type to-expr-type "in the renaming."))))
                (slot-value ast :renames)))))

(type-check-method <sal-rename> :to-expr)

(type-check-method <sal-module-instance> :module-name (:actuals list)
  (begin
    (sal-ast/check-number-of-actuals ast)
    (let* ((definition (sal-module-name/definition (slot-value ast :module-name)))
           (local-decl-list (slot-value definition :local-decls))
           (actual-expr-list (slot-value ast :actuals)))
      (check-if-match-params-and-arguments local-decl-list actual-expr-list identity))))

(define-method (sal-type-check (ast <sal-module-models>) (info <sal-type-checker-info>))
  (unwind-protect
   (begin
     (sal-type-check (slot-value ast :module) info)
     (set-slot-value! info :inside-property? #t)
     (set-slot-value! info :logic #f)
     (sal-type-check (slot-value ast :expr) info)
     (let ((next-location (sal-ast/uses-next-operator? (slot-value ast :expr))))
       (when next-location
         (sign-source-error next-location "Invalid use of next operator, it cannot be used in properties.")))
     (unless (sal-type/boolean? (sal-expr/type (slot-value ast :expr)))
       (sign-boolean-type-mismatch-error (slot-value ast :expr) (sal-expr/type (slot-value ast :expr))
                                         "in the property.")))
   (begin 
     (set-slot-value! info :inside-property? #f)
     (set-slot-value! info :logic #f))))

(define-generic (sal-state-var-decl/kind decl))
(define-method (sal-state-var-decl/kind (decl <sal-local-state-var-decl>)) 'local)
(define-method (sal-state-var-decl/kind (decl <sal-input-state-var-decl>)) 'input)
(define-method (sal-state-var-decl/kind (decl <sal-output-state-var-decl>)) 'output)
(define-method (sal-state-var-decl/kind (decl <sal-global-state-var-decl>)) 'global)

(type-check-method <sal-module-implements> :module1 :module2
  (begin
    (let* ((module1 (slot-value ast :module1))
           (module2 (slot-value ast :module2))
           (vars1 (sal-module/state-variables module1))
           (vars2 (sal-module/state-variables module2)))
      (for-each (lambda (var-decl1)
                  (let* ((var-name (sal-decl/name var-decl1))
                         (var-decl2 (sal-module/lookup-var module2 var-name)))
                    ;; (breakpoint "implements" (vars1 vars2 var-name var-decl2) #t)
                    (cond
                     (var-decl2
                      (unless (sal-type/equivalent? (slot-value var-decl1 :type) (slot-value var-decl2 :type))
                        (sign-incompatible-types-error ast 
                                                       (slot-value var-decl1 :type)
                                                       (slot-value var-decl2 :type)
                                                       "in implements assertion, the types of the variable `~a' in the implementation and abstract module are not compatible."
                                                       var-name))
                      (unless (eq? (class-of var-decl1) (class-of var-decl2))
                        (sign-source-error ast "Invalid implements assertion, the state variable `~a' definition in the implementation (~a is ~a) and abstract module (~a is ~a)  is not compatible." var-name var-name (sal-state-var-decl/kind var-decl1) var-name (sal-state-var-decl/kind var-decl2))))
                     ((not (instance-of? var-decl1 <sal-local-state-var-decl>))
                      (sign-source-error ast "Invalid implements assertion, the abstract module (right-hand-side) does not contain the non local state variable: ~a. Possible olution: hide the state variable using the LOCAL module expression." var-name)))))
                vars1)
      (for-each (lambda (var-decl2)
                  (let* ((var-name (sal-decl/name var-decl2))
                         (var-decl1 (sal-module/lookup-var module1 var-name)))
                    (unless var-decl1
                      (sign-source-error ast "Invalid implements assertion, the implementation module (left-hand-side) does not contain the state variable: ~a. Possible solution: create an observer module which defines the missing state variable for the implementation module." var-name))))
                vars2))))
(type-check-method <sal-assertion-proposition> (:assertion-exprs list)
                   (begin
    (for-each (lambda (assertion-expr)
                (unless (sal-type/boolean? (sal-expr/type assertion-expr))
                  (sign-boolean-type-mismatch-error assertion-expr (sal-expr/type assertion-expr) 
                                                    "in assertion expression.")))
              (slot-value ast :assertion-exprs))))

