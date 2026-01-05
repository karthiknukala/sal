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

(module sal-expression
        (include "sal.sch")
        (include "sal-ast-table.sch")
        (import sal-environment sal-ast-env sal-type sal-ast-list sal-ast-copy sal-ast-eq sal-ast-instantiate gmp-scheme
                queue sal-module sal-ast-for-each iterators sal-context sal-ast-attributes sal-expr-evaluator sal-decls)
        (export (make-sal-true place-provider)
                (make-sal-false place-provider)
                (sal-expr/true? expr)
                (sal-expr/false? expr)
                (make-sal-application fun arg . place-provider)
                (make-application-argument . arg-list)
                (make-sal-builtin-application class place-provider . arg-list)
                (make-sal-or . arg-list)
                (make-sal-or* arg-list place-provider)
                (make-sal-and . arg-list)
                (make-sal-and* arg-list place-provider)
                (make-sal-not arg)
                (make-sal-equality arg1 arg2)
                (make-sal-xor arg1 arg2)
                (make-sal-application++ fun place-provider . arg-list)
                (sal-application/logic-id expr)
                (sal-name-expr/constructor? expr)
                (sal-name-expr/recognizer? expr)
                (sal-name-expr/accessor? expr)
                (sal-application/promote! app)
                (sal-name-expr/inline? expr)
                (sal-name-expr/type expr)
                (sal-var-decl/definition expr)
                (sal-name-expr/definition expr)
                (sal-name-expr/recursive? expr)
                (sal-name-expr/scalar-element? expr)
                (sal-name-expr/recognizes? recognizer-name constructor-name)
                (sal-name-expr/constructor-accessors constructor-name)
                (sal-name-expr/constructor-recognizer constructor-name)
                (sal-name-expr/constant-constructor? constructor-name)
                (sal-name-expr/accessor-type accessor-name)
                (sal-name-expr/accessor-other-accessors accessor-name)
                (sal-argument->argument-list expr num-expected-args)
                (sal-application/argument-list expr)
                (sal-application/force-argument-list expr num-expected-args)
                (sal-binary-application/arguments app)
                (sal-binary-application/arg1 app)
                (sal-binary-application/arg2 app)
                (sal-tuple-literal/element expr pos)
                (sal-record-literal/element expr field-id)
                (sal-array-literal/element expr idx)
                (sal-tuple-literal/update expr pos new-value)
                (sal-record-literal/update expr pos new-value)
                (sal-array-literal/update expr idx new-value)
                (sal-function/app-class fun)
                (sal-expr/apply fun arg)
                (sal-expr/apply-conservative fun arg)
                (make-sal-numeral num place-provider)
                (sal-expr/first-order-value? expr)
                (sal-lhs/ground? lhs)
                (sal-expr/value? expr)
                (sal-expr/equal-values? expr1 expr2 env)
                (sal-function/extensional-equality? arg1 arg2 env)
                (sal-expr/type expr)
                (sal-expr/set-type! expr type)
                (sal-expr/update-type! expr)
                (sal-scalar-element/idx expr)
                (sal-constructor/idx expr)
                (sal-constructor/num-expected-args constructor)
                (sal-constructor-decl/num-expected-args constructor-decl)
                (sal-recognizer/idx expr)
                (sal-scalar-element-decl/idx expr)
                (sal-constructor-decl/idx expr)
                (sal-recognizer-decl/idx expr)
                (sal-expr->next-expr ast env))
        )

(define-api (make-sal-true place-provider)
  :doc "Create the @code{true} abstract syntax tree. @code{place-provider} is a AST node which provides line information and the context which will own the new node."
  (make-sal-builtin-name <sal-true> place-provider))

(define-api (make-sal-false place-provider)
  :doc "Create the @code{false} abstract syntax tree. @code{place-provider} is a AST node which provides line information and the context which will own the new node."
  (make-sal-builtin-name <sal-false> place-provider))

(define-api (sal-expr/true? (expr <sal-expr>))
  :doc "Returns @code{#t} if the @code{expr} is the @code{true} AST node."
  (instance-of? expr <sal-true>))

(define-api (sal-expr/false? (expr <sal-expr>))
  :doc "Returns @code{#t} if the @code{expr} is the @code{false} AST node."
  (instance-of? expr <sal-false>))

(define-api (make-application-argument . arg-list)
  :doc "Creates an argument for a SAL application AST node."
  [assert (arg-list) (not (null? arg-list))]
  (if (null? (cdr arg-list))
    (car arg-list)
    (make-ast-instance <sal-arg-tuple-literal> (car arg-list)
                       :exprs arg-list)))

(define-api (make-sal-builtin-application class place-provider . arg-list)
  :doc "Create a builtin application. @code{class} is the class of the builtin application. @code{place-provider} is a AST node which provides line information and the context which will own the new node. Note: if @code{place-provider} is the value @code{#unspecified}, then the first argument is going to be used to provide place information, and the context owner for the new AST node."
  :examples '((begin
                (define arg1 (sal/expr "1"))
                (define arg2 (sal/expr "2"))
                (define place-provider arg1)
                (make-sal-builtin-application <sal-add> place-provider arg1 arg2))
              (make-sal-builtin-application <sal-and> #unspecified (sal/expr "true") (sal/expr "false")))
  (let* ((place-provider (if (eq? place-provider #unspecified) (car arg-list) place-provider))
         (sal-env (sal-ast/sal-env place-provider))
         (decl (sal-env/builtin-decl-of-class sal-env class))
         (prelude (sal-env/prelude sal-env))
         (name-ref (make-ast-instance <sal-qualified-name-expr> place-provider
                                      :decl decl
                                      :context-ref prelude
                                      :actuals '()))
         (result (make-ast-instance class place-provider)))
    (ensure ((result <sal-application>))
      (set-slot-value! result :fun name-ref)
      (set-slot-value! result :arg (apply make-application-argument arg-list))
      result)))

(define-api (make-sal-or . arg-list)
  :doc "Create an @code{or} AST node. @code{arg-list} must not be empty. if @code{arg-list} contains only one element, than it will be the returned as the result. See @code{make-sal-or*}, @code{make-sal-and}."
  :examples '((make-sal-or (sal/expr "true") (sal/expr "false"))
              (make-sal-or (sal/expr "true")))
  [sal-assert "make-sal-or" (arg-list) (not (null? arg-list))]
  (cond 
   ((null? (cdr arg-list))
    (car arg-list))
   (else
    (apply make-sal-builtin-application <sal-or> #unspecified
           arg-list))))

(define-api (make-sal-or* arg-list place-provider)
  :doc "Similar to @code{make-sal-or}, but @code{arg-list} may be empty, and the place information is provided by @code{place-provider}. See @code{make-sal-and*}."
  [assert (arg-list) (list? arg-list)]
  (if (null? arg-list)
    (make-sal-false place-provider)
    (apply make-sal-builtin-application <sal-or> place-provider arg-list)))

(define-api (make-sal-and . arg-list)
  :doc "Create an @code{or} AST node. See @code{make-sal-or}, @code{make-sal-and*}."
  [sal-assert "make-sal-and" (arg-list) (not (null? arg-list))]
  (cond
   ((null? (cdr arg-list))
    (car arg-list))
   (else
    (apply make-sal-builtin-application <sal-and> #unspecified
           arg-list))))

(define-api (make-sal-and* arg-list place-provider)
  :doc "Similar to @code{make-sal-and}, but @code{arg-list} may be empty, and the place information is provided by @code{place-provider}. See @code{make-sal-or*}."
  [assert (arg-list) (list? arg-list)]
  (if (null? arg-list)
    (make-sal-true place-provider)
    (apply make-sal-and arg-list)))

(define-api (make-sal-not arg)
  :doc "Create a @code{not} AST node."
  (make-sal-builtin-application <sal-not> #unspecified arg))

(define-api (make-sal-equality arg1 arg2)
  :doc "Create the AST @code{arg1 = arg2}."
  (make-sal-builtin-application <sal-eq> #unspecified
                                arg1 arg2))

(define-api (make-sal-xor arg1 arg2)
  :doc "Create the AST @code{arg1 xor arg2}."
  (make-sal-builtin-application <sal-xor> #unspecified arg1 arg2))

(define (sal-function/app-class fun)
  (let ((type (sal-expr/type fun)))
    (if (sal-type/array? type)
      <sal-array-selection>
      (if (instance-of? fun <sal-name-expr>)
        (cond 
         ((sal-name-ref/decl-attribute fun :app-class) =>
          identity)
         ((sal-name-expr/constructor? fun)
          <sal-constructor-application>)
         ((sal-name-expr/recognizer? fun)
          <sal-recognizer-application>)
         ((sal-name-expr/accessor? fun)
          <sal-accessor-application>)
         (else
          <sal-application>))
        <sal-application>))))

(define-api (make-sal-application fun arg . place-provider)
  :doc "Create an application node. @code{fun} is the function, @code{arg} is the argument. The function @code{make-application-argument} can be used to create n-ary applications. @code{place-provider} is a AST node which provides line information and the context which will own the new node."
  :examples '((begin
                (define f (sal/expr "lambda (x, y: nat): x + y"))
                (define arg1 (sal/expr "1"))
                (define arg2 (sal/expr "2"))
                (make-sal-application f (make-application-argument arg1 arg2))))
  (let* ((place-provider (optional-arg place-provider fun))
         (result (make-ast-instance (sal-function/app-class fun) place-provider)))
    (ensure ((result <sal-application>))
      (set-slot-value! result :fun fun)
      (set-slot-value! result :arg arg)
      result)))
        
(define-api (make-sal-application++ (fun <sal-expr>) place-provider . arg-list)
  :doc "See @code{make-sal-application}."
  (make-sal-application fun 
                        (apply make-application-argument arg-list)
                        place-provider))

(define-generic (sal-application/logic-id expr))
(define-method (sal-application/logic-id (expr <sal-application>)) #f)
(define-method (sal-application/logic-id (expr <sal-ltl-application>)) 'LTL)
(define-method (sal-application/logic-id (expr <sal-ctl-application>)) 'CTL)

(define-api (sal-name-expr/constructor? (expr <sal-name-expr>))
  :doc "Returns @code{#t} if @code{expr} is a name expression which references a constructor declaration."
  (instance-of? (slot-value expr :decl) <sal-constructor-decl>))
       
(define-api (sal-name-expr/recognizer? (expr <sal-name-expr>))
  :doc "Returns @code{#t} if @code{expr} is a name expression which references a recognizer declaration."
  (instance-of? (slot-value expr :decl) <sal-recognizer-decl>))

(define-api (sal-name-expr/accessor? (expr <sal-name-expr>))
  :doc "Returns @code{#t} if @code{expr} is a name expression which references a accessor declaration."
  (instance-of? (slot-value expr :decl) <sal-accessor-decl>))

;; transform an application in a more specific subclass
;; this function is used to handle higher order code such as:
;; ((if cond and or) x y)
;; This node is an instance of <sal-application>...
;; Now suppose that cond is always true, and we simplify this node
;; to (and x y)
;; The new node is a <sal-application>...
;; it can be transformed in a <sal-and> using
;; the sal-application/promote!
;; Returns true if the application was promoted.
(define (sal-application/promote! app)
  (ensure ((app <sal-application>.))
    (let* ((fun (slot-value app :fun))
           (app-class (sal-function/app-class fun)))
      (cond
       ((and (not (eq? app-class <sal-application>))
             (not (eq? app-class (class-of app))))
        (quick-change-class! app app-class)
        #t)
       (else
        #f)))))

(define (sal-name-expr/inline? expr)
  (ensure ((expr <sal-name-expr>))
    (sal-name-ref/decl-attribute expr :inline?)))

(define-generic (sal-name-expr/type expr)
  :doc "Returns the type of a name expression.")
(define-method (sal-name-expr/type (expr <sal-name-expr>))
  (slot-value (slot-value expr :decl) :type))
(define-method (sal-name-expr/type (expr <sal-qualified-name-expr>))
  (let ((decl-type (slot-value (slot-value expr :decl) :type)))
    (sal-ast/instantiate decl-type
                         (slot-value expr :actuals))))

(memoize-sal-ast-method (sal-name-expr/type (expr <sal-qualified-name-expr>)))


(define-generic (sal-var-decl/definition decl))
(define-method (sal-var-decl/definition (decl <sal-var-decl>))
  #f)
(define-method (sal-var-decl/definition (decl <sal-const-decl>))
  (let ((val (slot-value decl :value)))
    (if (sal-decl/recursive? decl)
      (sal-ast/unique-decls val)
      val)))

(define-generic (sal-name-expr/definition expr)
  :doc "Returns the definition of a name expression. Returns @code{#f} if the name expression is uninterpreted, or it is not a @code{<sal-const-decl>}.")
(define-method (sal-name-expr/definition (expr <sal-name-expr>))
  (sal-var-decl/definition (slot-value expr :decl)))
(define-method (sal-name-expr/definition (expr <sal-qualified-name-expr>))
  (let ((value (sal-var-decl/definition (slot-value expr :decl))))
    (and value
         (sal-ast/instantiate value
                              (slot-value expr :actuals)))))

(memoize-sal-ast-method-core (sal-name-expr/definition (expr <sal-qualified-name-expr>))
                             ;; if the qualified-name-expr is recursive, then the result is not memoized
                             ;; because for each instantiation we create a different ast. See sal-var-decl/definition
                             (lambda (ast)
                               (not (sal-decl/recursive? (slot-value expr :decl)))))

(define-api (sal-name-expr/recursive? expr)
  :doc "Returns @code{#t} if the name expression @code{expr} references a recursive declaration."
  [assert (expr) (instance-of? expr <sal-name-expr>)]
  (sal-decl/recursive? (slot-value expr :decl)))

;; do I really need this...
(define-api (sal-name-expr/scalar-element? expr)
  :doc "Returns @code{#t} if the name expression @code{expr} references a scalar element."
  (instance-of? expr <sal-scalar>))

(define-api (sal-name-expr/recognizes? (recognizer-name <sal-name-expr>) (constructor-name <sal-name-expr>))
  (and (sal-name-expr/recognizer? recognizer-name)
       (sal-name-expr/constructor? constructor-name)
       (eq? (slot-value constructor-name :decl)
            (slot-value (slot-value recognizer-name :decl) :constructor-decl))))

(define-generic (sal-name-expr/constructor-accessors constructor-name)
  :doc "Returns the accessors of the given constructor.")

(define-method (sal-name-expr/constructor-accessors (constructor-name <sal-qualified-name-expr>))
  (tlet* ((actuals <primitive> (slot-value constructor-name :actuals))
          (constructor-decl <sal-constructor-decl> (slot-value constructor-name :decl))
          (accessors <primitive> (slot-value constructor-decl :accessors)))
    (map (cut sal-ast/instantiate <> actuals) accessors)))

(define-method (sal-name-expr/constructor-accessors (constructor-name <sal-name-expr>))
  (tlet ((constructor-decl <sal-constructor-decl> (slot-value constructor-name :decl)))
    (slot-value constructor-decl :accessors)))

(define-api (sal-name-expr/constructor-recognizer (constructor-name <sal-name-expr>))
  :doc "Returns the recognizer associated with the given constructor."
  (tlet* ((constructor-decl <sal-constructor-decl> (slot-value constructor-name :decl))
          (recognizer-decl <sal-recognizer-decl> (slot-value constructor-decl :recognizer-decl)))
    (copy-ast constructor-name
              :decl recognizer-decl)))

(define-api (sal-name-expr/constant-constructor? (constructor-name <sal-name-expr>))
  :doc "Returns @code{#t} if the constructor does not have parameters."
  (null? (slot-value (slot-value constructor-name :decl) :accessors)))

(define-generic (sal-name-expr/accessor-type accessor-name)
  :doc "Returns the type of the given accessor.")

(define-method (sal-name-expr/accessor-type (accessor-name <sal-qualified-name-expr>))
  (sal-ast/instantiate (slot-value (slot-value (slot-value accessor-name :decl) :type) :range)
                       (slot-value accessor-name :actuals)))

(define-method (sal-name-expr/accessor-type (accessor-name <sal-name-expr>))
  (slot-value (slot-value (slot-value accessor-name :decl) :type) :range))

(define-generic (sal-name-expr/accessor-other-accessors accessor-name)
  :doc "Returns the other accessors of the constructor owning @code{accessor-name}.")

(define-method (sal-name-expr/accessor-other-accessors (accessor-name <sal-qualified-name-expr>))
  (let* ((accessor-decl (slot-value accessor-name :decl))
         (constructor-decl (slot-value accessor-decl :constructor-decl))
         (accessor-list (slot-value constructor-decl :accessors))
         (actuals (slot-value accessor-name :actuals)))
    (map (cut sal-ast/instantiate <> actuals) accessor-list)))

(define-method (sal-name-expr/accessor-other-accessors (accessor-name <sal-name-expr>))
  (tlet* ((accessor-decl <sal-accessor-decl> (slot-value accessor-name :decl))
          (constructor-decl <sal-constructor-decl> (slot-value accessor-decl :constructor-decl))
          (accessor-list <primitive> (slot-value constructor-decl :accessors)))
    accessor-list))

(define-generic (sal-argument->argument-list arg num-expected-args)
  :doc "Converts a SAL argument in a list of arguments. @code{num-expected-args} is the number of expected arguments. It is main purpose is to disambiguate between an unary function which receives a tuple argument from an n-ary function.")
(define-method (sal-argument->argument-list (arg <sal-expr>) (num-expected-args <primitive>))
  [assert (num-expected-args) (or (not num-expected-args) (= num-expected-args 1))]
  (list arg))
(define-method (sal-argument->argument-list (arg <sal-tuple-literal>) (num-expected-args <primitive>))
  (cond
   ((and num-expected-args (= num-expected-args 1))
    (list arg))
   (else
    [assert (num-expected-args arg) (or (not num-expected-args) (= num-expected-args (length (slot-value arg :exprs))))]
    (slot-value arg :exprs))))

(define-api (sal-binary-application/arguments (app <sal-binary-application>))
  :doc "Returns two AST nodes (i.e., the arguments of the binary application."
  [sal-assert "sal-binary-application/arguments check-1" (app) (instance-of? (slot-value app :arg) <sal-arg-tuple-literal>)]
  (let ((args (slot-value (slot-value app :arg) :exprs)))
    [sal-assert "sal-binary-application/arguments check-2" (args) (= (length args) 2)]
    (values (car args) (cadr args))))

(define-api (sal-binary-application/arg1 (app <sal-binary-application>))
  (multiple-value-bind 
      (arg1 arg2)
      (sal-binary-application/arguments app)
    arg1))

(define-api (sal-binary-application/arg2 (app <sal-binary-application>))
  (multiple-value-bind 
      (arg1 arg2)
      (sal-binary-application/arguments app)
    arg2))

(define-api (sal-application/argument-list (expr <sal-application>))
  (let ((arg (slot-value expr :arg)))
    (sal-argument->argument-list arg #f)))

(define-api (sal-application/force-argument-list (expr <sal-application>) (num-expected-args <primitive>))
  (let ((arg (slot-value expr :arg)))
    (sal-argument->argument-list arg num-expected-args)))

(define-api (sal-tuple-literal/element (expr <sal-tuple-literal>) pos)
  :doc "Returns an element of the tuple literal. @code{pos} is an integer."
  (sal-tuple/element expr :exprs pos))

(define-api (sal-record-literal/element (expr <sal-record-literal>) field-id)
  (sal-record/element expr :entries :expr field-id))

(define-api (sal-array-literal/element (expr <sal-array-literal>) idx)
  (sal-expr/apply expr idx))
  
(define-api (sal-tuple-literal/update (expr <sal-tuple-literal>) pos (new-value <sal-expr>))
  (let ((pos (sal-tuple-position->integer pos))
        (result-queue (make-queue)))
    (let loop ((i 1)
               (expr-list (slot-value expr :exprs)))
      (if (null? expr-list)
        (copy-ast expr :exprs (queue->list result-queue))
        (let ((curr-expr (car expr-list)))
          (queue/insert! result-queue (if (= i pos) new-value curr-expr))
          (loop (+ i 1) (cdr expr-list)))))))

(define-api (sal-record-literal/update (expr <sal-record-literal>) pos (new-value <sal-expr>))
  (let ((result-queue (make-queue))
        (pos (if (instance-of? pos <sal-identifier>)
               (slot-value pos :name)
               pos)))
    (let loop ((entry-list (slot-value expr :entries)))
      (if (null? entry-list)
        (copy-ast expr :entries (queue->list result-queue))
        (let* ((curr-entry (car entry-list))
               (curr-entry-id (sal-record-entry/name curr-entry)))
          (queue/insert! result-queue (if (eq? pos curr-entry-id)
                                        (copy-ast curr-entry :expr new-value)
                                        curr-entry))
          (loop (cdr entry-list)))))))

(define-api (sal-array-literal/update (expr <sal-array-literal>) idx (new-value <sal-expr>))
  (let* ((array-idx (make-sal-name-expr (car (slot-value expr :local-decls)) expr))
         (place-provider expr)
         (eq-test (make-sal-builtin-application <sal-eq> place-provider array-idx idx))
         (new-body (make-ast-instance <sal-conditional> expr
                                      :cond-expr eq-test
                                      :then-expr new-value
                                      :else-expr (slot-value expr :expr))))
    (copy-ast expr :expr new-body)))

(define-api (make-sal-numeral num place-provider)
  :doc "Creates a SAL numeral AST (see @code{<sal-numeral>}). @code{place-provider} is a AST node which provides line information and the context which will own the new node."
  (make-ast-instance <sal-numeral> place-provider :num (object->mpq num)))

(define-generic (sal-expr/apply fun arg)
  :doc "Apply @code{arg} to the function @code{fun}, that is, substitutes the argument of the function @code{fun} with @code{arg}. If @code{fun} is a name expression referencing a lambda node, then the lambda node will be expanded."
  :examples '((sal-expr/apply (sal/expr "lambda (x:nat): x + 1") (sal/expr "10"))))

(define-method (sal-expr/apply (fun <sal-lambda>) (arg <sal-expr>))
  [sal-assert "sal-expr/apply" (fun arg) (= (length (slot-value fun :local-decls)) 1)]
  (sal-ast/substitute (slot-value fun :expr) (update-env (make-empty-env) (car (slot-value fun :local-decls)) arg)))

(define-method (sal-expr/apply (fun <sal-lambda>) (arg <sal-arg-tuple-literal>))
  [sal-assert "sal-expr/apply" (fun arg) (= (length (slot-value fun :local-decls))
                                            (length (slot-value arg :exprs)))]
  (sal-ast/substitute (slot-value fun :expr) (update-env* (make-empty-env) (slot-value fun :local-decls) (slot-value arg :exprs))))

(define (make-app fun arg)
  (make-ast-instance (sal-function/app-class fun) fun
                     :fun fun
                     :arg arg))

(define-method (sal-expr/apply (fun <sal-expr>) (arg <sal-expr>))
  (make-app fun arg))

;; I should check if I really need this alternative...  
(define-method (sal-expr/apply (fun <sal-name-expr>) (arg <sal-expr>))
  (let ((def (sal-name-expr/definition fun)))
    (if (and def (or (instance-of? def <sal-lambda>) (instance-of? def <sal-name-expr>)))
      (sal-expr/apply def arg)
      (let ((result (make-app fun arg)))
        (sal-application/promote! result)
        result))))

(define-generic (sal-expr/apply-conservative fun arg)
  :doc "Similar to @code{sal-expr/apply}, but does not expand name expressions.")
(define-method (sal-expr/apply-conservative (fun <sal-lambda>) (arg <sal-expr>))
  (sal-expr/apply fun arg))
(define-method (sal-expr/apply-conservative (fun <sal-expr>) (arg <sal-expr>))
  (make-app fun arg))

(define-generic (sal-expr/value-core? expr first-order?))
(define-method (sal-expr/value-core? (expr <sal-ast>) (first-order? <primitive>)) #f)
(define-method (sal-expr/value-core? (expr <sal-numeral>) (first-order? <primitive>)) #t)
(define-method (sal-expr/value-core? (expr <sal-scalar>) (first-order? <primitive>)) #t)
(define-method (sal-expr/value-core? (expr <sal-string-expr>) (first-order? <primitive>)) #t)
(define-method (sal-expr/value-core? (expr <sal-tuple-literal>) (first-order? <primitive>))
  (for-all (cut sal-expr/value-core? <> first-order?) (slot-value expr :exprs)))
(define-method (sal-expr/value-core? (expr <sal-record-literal>) (first-order? <primitive>))
  (for-all (lambda (entry)
             (sal-expr/value-core? (slot-value entry :expr) first-order?))
           (slot-value expr :entries)))
(define-method (sal-expr/value-core? (expr <sal-constructor-application>) (first-order? <primitive>))
  (for-all (cut sal-expr/value-core? <> first-order?) (sal-application/argument-list expr)))
(define-method (sal-expr/value-core? (expr <sal-constructor>) (first-order? <primitive>)) #t)
(define-method (sal-expr/value-core? (expr <sal-lambda>) (first-order? <primitive>))
  ;; TODO I should check it expr contains free variables
  (not first-order?)) 

(define-api (sal-expr/first-order-value? expr)
  :doc "Returns @code{#t} if @code{expr} is a first order value."
  (sal-expr/value-core? expr #t))

(define (sal-expr/value? expr)
  (sal-expr/value-core? expr #f))

;; A lhs is ground if:
;; - it does not contain array-selection
;; - if it contains an array-selection, then the idx is a first-order-value.
(define-generic (sal-lhs/ground? lhs)
  :doc "Returns @code{#t} if @code{lhs} is ground. We say a left-hand-side (lhs) is ground when: it does not contain array selection, or if it contains an array selection, but the index is a first-order-value.")
(define-method (sal-lhs/ground? (lhs <sal-name-expr>)) #t)
(define-method (sal-lhs/ground? (lhs <sal-next-operator>)) #t)
(define-method (sal-lhs/ground? (lhs <sal-selection>))
  (sal-lhs/ground? (sal-selection/target lhs)))
(define-method (sal-lhs/ground? (lhs <sal-array-selection>))
  (and (sal-lhs/ground? (slot-value lhs :fun))
       (sal-expr/first-order-value? (slot-value lhs :arg))))


(define-api (sal-expr/equal-values? expr1 expr2 env)
  :doc "Returns @code{#t} if @code{expr1} and @code{expr2} are equal in the substitution environment @code{env}. The function returns @code{#f} if the values are different, and @code{'dont-known} if the truth value cannot be determined."
  (let* ((place-provider expr1)
         (equality (make-sal-builtin-application <sal-eq> place-provider expr1 expr2)))
    (try
     (let ((result (sal-expr/evaluate-core equality env 0)))
       (cond
        ((instance-of? result <sal-true>)
         #t)
        ((instance-of? result <sal-false>)
         #f)
        (else
         'dont-known)))
     (catch 'expr-evaluator
            (lambda (_)
              'dont-known)))))

(define (sal-function/extensional-equality? arg1 arg2 env)
  ;; simple implementation... should be improved in the future...
  ;; this function will not be used very often, since most of the users
  ;; to do use high order constructors.
  (try
   (let* ((function-type (sal-expr/type arg1))
          (domain-type (sal-function-type/domain function-type))
          (it (sal-type/make-iterator domain-type)))
     (bind-exit (exit)
       (iterator/for-each (lambda (domain-value)
                            (let ((expr1 (make-sal-application arg1 domain-value))
                                  (expr2 (make-sal-application arg2 domain-value)))
                              (case (sal-expr/equal-values? expr1 expr2 env)
                                ((#t) 
                                 ;; continue
                                 #unspecified)
                                ((#f)
                                 (exit #f))
                                (else
                                 (exit 'dont-known)))))
                          it)
       #t))
   (catch* '(type-iterator expr-evaluator)
           (lambda (_ __)
             'dont-known))))

(define-api (sal-expr/type expr)
  :doc "Returns the type of a SAL expression."
  :examples '((sal-expr/type (sal/expr "2"))
              (sal-expr/type (sal/expr "(# idx := 2, flag := false #)")))
  (trace 'expr "getting type of: ~a" (sal-ast->list expr))
  [assert (expr) (instance-of? expr <sal-expr>)]
  (unless (slot-value expr :type)
    (sal-expr/update-type! expr))
  (slot-value expr :type))

(define (sal-expr/set-type! expr type)
  [assert (expr) (instance-of? expr <sal-expr>)]
  [assert (type) (instance-of? type <sal-type>)]
  (set-slot-value! expr :type type))

(define-generic (sal-expr/update-type! expr))

(define-method (sal-expr/update-type! (expr <sal-numeral>))
  (sal-expr/set-type! expr
                      (let ((num (slot-value expr :num)))
                        (if (mpq/integer? num)
                          (if (>=mpq (slot-value expr :num) *mpq-zero*)
                            (make-sal-builtin-name <sal-nat-type> expr)
                            (make-sal-builtin-name <sal-int-type> expr))
                          (make-sal-builtin-name <sal-real-type> expr)))))

(define-method (sal-expr/update-type! (expr <sal-eq>))
  (sal-expr/set-type! expr (make-sal-builtin-name <sal-bool-type> expr)))

(define-method (sal-expr/update-type! (expr <sal-diseq>))
  (sal-expr/set-type! expr (make-sal-builtin-name <sal-bool-type> expr)))
  
(define-method (sal-expr/update-type! (expr <sal-in>))
  (sal-expr/set-type! expr (make-sal-builtin-name <sal-bool-type> expr)))

(define-generic (sign-invalid-app-error expr))

(define-method (sign-invalid-app-error (expr <sal-application>))
  (sign-source-error (slot-value expr :fun) "Invalid function application, expression must be a function."))

(define-method (sign-invalid-app-error (expr <sal-array-selection>))
  (sign-source-error (slot-value expr :fun) "Invalid array selection, expression must be an array."))

(define-method (sal-expr/update-type! (expr <sal-application>))
  (let* ((fun (slot-value expr :fun))
         (fun-type (sal-expr/type fun)))
    (unless (sal-type/function? fun-type)
      (sign-invalid-app-error expr))
    (sal-expr/set-type! expr (sal-function-type/range fun-type))))

(define (arguments-meet-predicate? app pred?)
  (for-all (lambda (arg)
             (pred? (sal-expr/type arg)))
           (sal-application/argument-list app)))

(define (real-arguments? app)
  (arguments-meet-predicate? app sal-type/real?))

(define (integer-arguments? app)
  (arguments-meet-predicate? app sal-type/integer?))

(define (natural-arguments? app)
  (arguments-meet-predicate? app sal-type/natural?))

(define (preserve-reals app)
  (cond
   ((real-arguments? app)
    (make-sal-builtin-name <sal-real-type> app))
   (else
    (make-sal-builtin-name <sal-number-type> app))))

(define (preserve-integers-reals app)
  (cond
   ((integer-arguments? app)
    (make-sal-builtin-name <sal-int-type> app))
   (else
    (preserve-reals app))))

(define (preserve-naturals-integers-reals app)
  (cond
   ((natural-arguments? app)
    (make-sal-builtin-name <sal-nat-type> app))
   (else
    (preserve-integers-reals app))))

(define (preserve-naturals app)
  (cond
   ((natural-arguments? app)
    (make-sal-builtin-name <sal-nat-type> app))
   (else
    (make-sal-builtin-name <sal-int-type> app))))

(define-method (sal-expr/update-type! (expr <sal-arith-op>))
  (sal-expr/set-type! expr (preserve-naturals-integers-reals expr)))

(define-method (sal-expr/update-type! (expr <sal-sub>))
  (sal-expr/set-type! expr (preserve-integers-reals expr)))

(define-method (sal-expr/update-type! (expr <sal-div>))
  (sal-expr/set-type! expr (preserve-reals expr)))

(define-method (sal-expr/update-type! (expr <sal-idiv>))
  (sal-expr/set-type! expr (preserve-naturals expr)))

(define-method (sal-expr/update-type! (expr <sal-mod>))
  (sal-expr/set-type! expr (preserve-naturals expr)))

(define (expr-list->type-list expr-list)
  (map sal-expr/type expr-list))

(define (var-decl-list->type-list var-decl-list)
  (map (lambda (var-decl)
         (ensure ((var-decl <sal-var-decl>))
           (slot-value var-decl :type)))
       var-decl-list))

(define (make-argument-type local-decls)
  [assert (local-decls) (list? local-decls)]
  [assert (local-decls) (not (null? local-decls))]
  [assert (local-decls) (for-all (cut instance-of? <> <sal-var-decl>) local-decls)]
  (let ((arg-types (var-decl-list->type-list local-decls)))
    [assert (arg-types) (not (null? arg-types))]
    (if (null? (cdr arg-types))
      (car arg-types)
      (make-ast-instance <sal-domain-tuple-type> (car arg-types)
                         :types arg-types))))

(define-method (sal-expr/update-type! (expr <sal-lambda>))
  (sal-expr/set-type! expr (make-ast-instance <sal-function-type> expr
                                              :domain (make-argument-type (slot-value expr :local-decls))
                                              :range (sal-expr/type (slot-value expr :expr)))))

(define-method (sal-expr/update-type! (expr <sal-quantified-expr>))
  (sal-expr/set-type! expr (make-sal-builtin-name <sal-bool-type> expr)))

(define-method (sal-expr/update-type! (expr <sal-array-literal>))
  (call-next-method)
  (quick-change-class! (slot-value expr :type) <sal-array-type>))

(define-method (sal-expr/update-type! (expr <sal-tuple-literal>))
  (sal-expr/set-type! expr (make-ast-instance <sal-tuple-type> expr
                                              :types (expr-list->type-list (slot-value expr :exprs)))))

(define-method (sal-expr/update-type! (expr <sal-arg-tuple-literal>))
  (sal-expr/set-type! expr (make-ast-instance <sal-domain-tuple-type> expr
                                              :types (expr-list->type-list (slot-value expr :exprs)))))

(define-method (sal-expr/update-type! (expr <sal-record-literal>))
  (sal-expr/set-type! expr 
                      (make-ast-instance <sal-record-type> expr
                                         :fields (map (lambda (entry)
                                                        (ensure ((entry <sal-record-entry>))
                                                          (make-ast-instance <sal-field> entry
                                                                             :id (slot-value entry :id)
                                                                             :type (sal-expr/type (slot-value entry :expr)))))
                                                      (slot-value expr :entries)))))

(define-method (sal-expr/update-type! (expr <sal-tuple-selection>))
  (sal-expr/set-type! expr (sal-tuple-type/element-type (sal-expr/type (slot-value expr :target))
                                                        (slot-value expr :idx))))

(define-method (sal-expr/update-type! (expr <sal-record-selection>))
  (sal-expr/set-type! expr (sal-record-type/element-type (sal-expr/type (slot-value expr :target))
                                                         (slot-value expr :idx))))
  
(define-method (sal-expr/update-type! (expr <sal-function-update>))
  (let ((target-type (sal-expr/type (slot-value expr :target))))
    (sal-expr/set-type! expr (make-ast-instance <sal-function-type> expr
                                                :domain (sal-function-type/domain target-type)
                                                :range (sal-type/union (sal-expr/type (slot-value expr :new-value))
                                                                       (sal-function-type/range target-type))))))

(define-method (sal-expr/update-type! (expr <sal-array-update>))
  (call-next-method)
  (quick-change-class! (slot-value expr :type) <sal-array-type>))

(define-method (sal-expr/update-type! (expr <sal-tuple-update>))
  (let ((target-type (sal-expr/type (slot-value expr :target)))
        (pos (mpq->integer (slot-value (slot-value expr :idx) :num)))
        (new-value-type (sal-expr/type (slot-value expr :new-value)))
        (result-queue (make-queue)))
    (let loop ((i 1)
               (type-list (sal-tuple-type/types target-type)))
      (if (null? type-list)
        (sal-expr/set-type! expr (make-ast-instance <sal-tuple-type> expr
                                                    :types (queue->list result-queue)))
        (let ((curr-type (car type-list)))
          (queue/insert! result-queue (if (= i pos)
                                        (sal-type/union new-value-type curr-type)
                                        curr-type))
          (loop (+ i 1) (cdr type-list)))))))

(define-method (sal-expr/update-type! (expr <sal-record-update>))
  (let ((target-type (sal-expr/type (slot-value expr :target)))
        (field-id (slot-value (slot-value expr :idx) :name))
        (new-value-type (sal-expr/type (slot-value expr :new-value)))
        (result-queue (make-queue)))
    (let loop ((field-list (sal-record-type/fields target-type)))
      (if (null? field-list)
        (sal-expr/set-type! expr (make-ast-instance <sal-record-type> expr
                                                    :fields (queue->list result-queue)))
        (let* ((curr-field (car field-list))
               (curr-field-id (sal-field/name curr-field)))
          (queue/insert! result-queue (if (eq? field-id curr-field-id)
                                        (copy-ast curr-field :type (sal-type/union new-value-type
                                                                                   (slot-value curr-field :type)))
                                        curr-field))
          (loop (cdr field-list)))))))

(define-method (sal-expr/update-type! (expr <sal-definition-expression>))
  (sal-expr/set-type! expr (make-sal-builtin-name <sal-bool-type> expr)))
  
(define-method (sal-expr/update-type! (expr <sal-next-operator>))
  (sal-expr/set-type! expr (sal-expr/type (slot-value expr :name-expr))))

(define-method (sal-expr/update-type! (expr <sal-pre-operator>))
  (sal-expr/set-type! expr (sal-expr/type (slot-value expr :expr))))

(define-method (sal-expr/update-type! (expr <sal-string-expr>))
  (sal-expr/set-type! expr (make-sal-builtin-name <sal-string-type> expr)))

(define-method (sal-expr/update-type! (expr <sal-let-expr>))
  (sal-expr/set-type! expr (sal-expr/type (slot-value expr :expr))))

(define-method (sal-expr/update-type! (expr <sal-ring-application>))
  (sal-expr/set-type! expr (sal-expr/type (slot-value expr :arg))))

(define-method (sal-expr/update-type! (expr <sal-debug-application>))
  (sal-expr/set-type! expr (sal-expr/type (list-last-element (sal-application/argument-list expr)))))

(define-method (sal-expr/update-type! (expr <sal-conditional>))
  (let ((then-type (sal-expr/type (slot-value expr :then-expr)))
        (else-type (sal-expr/type (slot-value expr :else-expr))))
    (trace 'expr "updating type of conditional then-type=~a and else-type=~a" (sal-ast->list then-type) (sal-ast->list else-type))
    (sal-expr/set-type! expr (sal-type/union then-type else-type))
    (trace 'expr "  result = " (sal-ast->list (sal-expr/type expr)))))

(define-method (sal-expr/update-type! (expr <sal-mod-init>))
  (let ((module-type (sal-module/type (slot-value expr :module))))
    (sal-expr/set-type! expr (make-ast-instance <sal-function-type> expr
                                                :domain module-type
                                                :range (make-sal-builtin-name <sal-bool-type> expr)))))

(define-method (sal-expr/update-type! (expr <sal-mod-trans>))
  (let ((module-type (sal-module/type (slot-value expr :module))))
    (sal-expr/set-type! expr (make-ast-instance <sal-function-type> expr
                                                :domain (make-ast-instance <sal-domain-tuple-type> expr
                                                                           :types (list module-type module-type))
                                                :range (make-sal-builtin-name <sal-bool-type> expr)))))

(define-method (sal-expr/update-type! (expr <sal-name-expr>))
  (trace 'expr "Updating type of name-expr ~a --- ~a" (sal-ast->list expr) (sal-ast->list (sal-name-expr/type expr)))
  (sal-expr/set-type! expr (sal-name-expr/type expr)))

(define-method (sal-expr/update-type! (expr <sal-assertion-expr>))
  (sal-expr/set-type! expr (make-sal-builtin-name <sal-bool-type> expr)))

(define (find-idx element-list element-decl)
  (let ((idx 0))
    (find (lambda (elem)
            (if (eq? (slot-value elem :decl) element-decl)
              #t
              (begin
                (set! idx (+ idx 1))
                #f)))
          element-list)
    idx))

(define-api (sal-scalar-element/idx expr)
  :doc "Returns the index of a scalar element."
  [sal-assert "sal-scalar-element/idx check-1" (expr) (instance-of? expr <sal-name-expr>)]
  [sal-assert "sal-scalar-element/idx check-2" (expr) (sal-name-expr/scalar-element? expr)]
  (sal-scalar-element-decl/idx (slot-value expr :decl)))

(define-api (sal-scalar-element-decl/idx element-decl)
  (let* ((scalar-type-decl (slot-value element-decl :scalar-type-decl))
         (scalar-type (slot-value scalar-type-decl :type))
         (scalar-element-list (slot-value scalar-type :scalar-elements)))
    (find-idx scalar-element-list element-decl)))
    
(define-api (sal-constructor/idx expr)
  [sal-assert "sal-constructor/idx check-1" (expr) (instance-of? expr <sal-name-expr>)]
  [sal-assert "sal-constructor/idx check-2" (expr) (sal-name-expr/constructor? expr)]
  (sal-constructor-decl/idx (slot-value expr :decl)))

(define-api (sal-constructor/num-expected-args constructor)
  (sal-constructor-decl/num-expected-args (slot-value constructor :decl)))

(define-api (sal-constructor-decl/num-expected-args constructor-decl)
  (length (slot-value constructor-decl :accessors)))

(define-api (sal-constructor-decl/idx element-decl)
  (let* ((data-type-decl (slot-value element-decl :data-type-decl))
         (data-type (slot-value data-type-decl :type))
         (constructor-list (slot-value data-type :constructors)))
    (find-idx constructor-list element-decl)))

(define-api (sal-recognizer/idx expr)
  [sal-assert "sal-recognizer/idx check-1" (expr) (instance-of? expr <sal-name-expr>)]
  [sal-assert "sal-recognizer/idx check-2" (expr) (sal-name-expr/recognizer? expr)]
  (sal-recognizer-decl/idx (slot-value expr :decl)))

(define-api (sal-recognizer-decl/idx recognizer-decl)
  (let* ((constructor-decl (slot-value recognizer-decl :constructor-decl))
         (data-type-decl (slot-value constructor-decl :data-type-decl))
         (data-type (slot-value data-type-decl :type))
         (constructor-list (slot-value data-type :constructors)))
    (find-idx constructor-list constructor-decl)))
    
(define-generic (sal-expr->next-expr ast env)
  :doc "Returns a new AST node, where the current state variables in @code{ast} are substituted by next state variables.")
(define-method (sal-expr->next-expr (ast <sal-ast>) (env <primitive>))
  (sal-ast/map ast env sal-expr->next-expr))
(define-method (sal-expr->next-expr (ast <sal-next-operator>) (env <primitive>))
  (sign-source-error ast "Error converting expression to next-expression. The expression already contains the next operator, so the transformation cannot be applied."))
(define-method (sal-expr->next-expr (ast <sal-name-expr>) (env <primitive>))
  (cond
   ((sal-name-ref/state? ast)
    (make-ast-instance <sal-next-operator> ast
                       :name-expr ast))
   (else
    (sal-ast/substitute ast env))))
