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

(module sal-ast-simplify
        (include "sal.sch")
        (import sal-environment sal-ast-env sal-expression sal-ast-copy sal-expr-evaluator sal-ast-eq queue 
                gmp-scheme sal-ast-table sal-decls unique-names sal-type sal-pp sal-rename-variables)
        (export (sal-ast/local-simplify-core ast env proc)
                (sal-quantified-ast/local-simplify-core expr quantifier var-decls)
                (sal-quantified-expr/local-simplify ast)
                (sal-let-expr/local-simplify ast)
                (sal-ast/simplify-core ast env)
                (sal-ast/simplify-without-converting-inequalities-core ast env)
                (sal-ast/simplify-without-converting-inequalities ast)
                (sal-ast/simplify ast)
                (sal-ast/local-simplify ast)
                (sal-arith-relation/local-simplify app env proc mpq-proc)
                (make-sal-let+ local-decls body arg-list env proc)
                (make-simplified-sal-builtin-application class place-source . arg-list)
                (sal-function-update/expand ast env proc)
                (make-sal-not+ arg)
                (make-sal-or+ . args)
                (make-sal-or+* args place-source)
                (make-sal-choice+ . args)
                (make-sal-choice+* args place-source)
                (make-sal-and+ . args)
                (make-sal-and+* args place-source)
                (make-sal-xor+ arg1 arg2)
                (make-sal-equality+ arg1 arg2)
                (make-sal-cond+ cond-expr then-expr else-expr . place-provider)
                (sal-ast/eager-local-simplify ast env proc)
                (sal-ast/eager-simplify-core ast env)
                (sal-ast/eager-simplify ast))
        )

;; apply simple (and cheap) simplifications...
(define-generic (sal-ast/local-simplify-core ast env proc))

(define (sal-ast/local-simplify ast)
  (sal-ast/local-simplify-core ast (make-empty-env) sal-ast/substitute))

(define (try-to-evaluate value env proc)
  (try-until-success (sal-expr/evaluate-core value env 0) (proc value env)))

(define-inline (try-to-evaluate-function fun arg env proc)
  (try
   (let ((arg (proc arg env)))
     (unless (sal-expr/first-order-value? arg)
       (fail))
     (let* ((app (sal-expr/apply fun arg))
            ;; (_ (breakpoint "simp" (fun arg app env proc) #t))
            (result (sal-expr/evaluate-core app env 0)))
       (proc result env)))
   (lambda (e p m o)
     (e #f))))

(define (make-simplified-sal-builtin-application class place-source . arg-list)
  (let ((result (apply make-sal-builtin-application class place-source arg-list)))
    (sal-ast/local-simplify result)))

(define (make-sal-not+ arg)
  (make-simplified-sal-builtin-application <sal-not> #unspecified arg))

(define (make-sal-or+ . args)
  (apply make-simplified-sal-builtin-application <sal-or> #unspecified args))

(define (make-sal-or+* args place-source)
  [assert (args) (list? args)]
  (if (null? args)
    (make-sal-false place-source)
    (apply make-sal-or+ args)))

(define (make-sal-choice+ . args)
  (let ((result (apply make-sal-or+ args)))
    (if (instance-of? result <sal-or>)
      (change-ast-class result <sal-choice>)
      result)))

(define (make-sal-choice+* args place-source)
  [assert (args) (list? args)]
  (if (null? args)
    (make-sal-false place-source)
    (apply make-sal-choice+ args)))

(define (make-sal-and+ . args)
  (apply make-simplified-sal-builtin-application <sal-and> #unspecified args))

(define (make-sal-and+* args place-source)
  [assert (args) (list? args)]
  (if (null? args)
    (make-sal-true place-source)
    (apply make-sal-and+ args)))

(define (make-sal-xor+ arg1 arg2)
  (make-simplified-sal-builtin-application <sal-xor> #unspecified arg1 arg2))

(define (make-sal-equality+ arg1 arg2)
  (make-simplified-sal-builtin-application <sal-eq> #unspecified arg1 arg2))

(define (make-sal-cond+ cond-expr then-expr else-expr . place-provider)
  (let* ((place-provider (optional-arg place-provider cond-expr))
         (result (make-ast-instance <sal-conditional> place-provider
                                    :cond-expr cond-expr
                                    :then-expr then-expr
                                    :else-expr else-expr)))
    (sal-ast/local-simplify result)))
    
(define-method (sal-ast/local-simplify-core (ast <sal-ast>) (env <primitive>) (proc <primitive>))
  (sal-ast/map ast env proc))

(define-method (sal-ast/local-simplify-core (ast <sal-type>) (env <primitive>) (proc <primitive>))
  ;; I should not simplify expressions that are located inside types
  (sal-ast/substitute ast env))

(define-method (sal-ast/local-simplify-core (ast <sal-qualified-name-ref>) (env <primitive>) (proc <primitive>))
  ;; I should not simplify expressions and types in qualified names...
  (sal-ast/substitute ast env))

(define-method (sal-ast/local-simplify-core (ast <sal-name-expr>) (env <primitive>) (proc <primitive>))
  (let ((new-ast (call-next-method)))
    (if (instance-of? new-ast <sal-name-expr>)
      (cond 
       ((sal-name-expr/definition new-ast) =>
        (lambda (definition)
          (if (instance-of? definition <sal-simple-expr>) 
            definition
            new-ast)))
       (else new-ast))
      new-ast)))

(define-method (sal-ast/local-simplify-core (ast <sal-application>) (env <primitive>) (proc <primitive>))
  (let* ((fun (proc (slot-value ast :fun) env))
         (arg (slot-value ast :arg)))
    (cond
     ((instance-of? fun <sal-name-expr>)
      (cond
       ((sal-application/promote! ast)
        ;; promoted...
        (sal-ast/local-simplify-core ast env proc))
       ((sal-name-expr/inline? fun)
        (sal-ast/local-simplify-core (sal-expr/apply (sal-name-expr/definition fun) arg) env proc))
       ((sal-name-expr/definition fun) =>
        (lambda (definition)
          (or (try-to-evaluate-function definition arg env proc)
              (call-next-method))))
       (else (call-next-method))))
     ((instance-of? fun <sal-lambda>)
      (ensure ((fun <sal-lambda>))
        (let ((local-decls (slot-value fun :local-decls)))
          (make-sal-let+ local-decls
                         (slot-value fun :expr) 
                         (sal-application/force-argument-list ast (length local-decls)) 
                         env proc))))
     (else
      (call-next-method)))))

(define-generic (sal-not-arg/local-simplify expr not-app))
(define-method (sal-not-arg/local-simplify (expr <sal-expr>) (not-app <sal-not>)) 
  (update-ast-slots not-app
                    :arg expr))
(define-method (sal-not-arg/local-simplify (expr <sal-true>) (not-app <sal-not>))
  (make-sal-false not-app))
(define-method (sal-not-arg/local-simplify (expr <sal-false>) (not-app <sal-not>))
  (make-sal-true not-app))
(define-method (sal-not-arg/local-simplify (expr <sal-not>) (not-app <sal-not>))
  (slot-value expr :arg))
(define-method (sal-not-arg/local-simplify (expr <sal-diseq>) (not-app <sal-not>))
  (apply make-sal-builtin-application <sal-eq> expr
                                (sal-application/argument-list expr)))
(define-method (sal-not-arg/local-simplify (expr <sal-eq>) (not-app <sal-not>))
  (apply make-sal-builtin-application <sal-diseq> expr
                                (sal-application/argument-list expr)))
(define-method (sal-not-arg/local-simplify (expr <sal-and>) (not-app <sal-not>))
  (apply make-sal-builtin-application <sal-or> expr
                                (map (cut sal-not-arg/local-simplify <> not-app) 
                                     (sal-application/argument-list expr))))
(define-method (sal-not-arg/local-simplify (expr <sal-or>) (not-app <sal-not>))
  (apply make-sal-builtin-application <sal-and> expr
                                (map (cut sal-not-arg/local-simplify <> not-app)
                                     (sal-application/argument-list expr))))
(define-method (sal-not-arg/local-simplify (expr <sal-exists-expr>) (not-app <sal-not>))
  (change-ast-class
   (update-ast-slots expr
                     :expr (sal-not-arg/local-simplify (slot-value expr :expr) not-app))
   <sal-for-all-expr>))
(define-method (sal-not-arg/local-simplify (expr <sal-for-all-expr>) (not-app <sal-not>))
  (change-ast-class
   (update-ast-slots expr
                     :expr (sal-not-arg/local-simplify (slot-value expr :expr) not-app))
   <sal-exists-expr>))


(define-method (sal-ast/local-simplify-core (ast <sal-not>) (env <primitive>) (proc <primitive>))
  (tlet ((new-ast <sal-not> (update-ast-slots ast 
                                              :arg (proc (slot-value ast :arg) env))))
    (sal-not-arg/local-simplify (slot-value new-ast :arg) new-ast)))

(define-inline (sal-nary-boolean-op/local-simplify app env proc proc-true? proc-false? proc-make-true)
  (bind-exit (exit)
    (let* ((expr-list (sal-application/argument-list app))
           (expr-table (make-sal-ast-table)) ;; this table is used to remove duplicates, and detect the occurence of an expression and its negation.
           (app-class (class-of app))
           (new-expr-list 
            ;; this loop is not tail recursive, since I'm trying to save memory, that is,
            ;; I don't want to create new nodes for a failed simplification attempt.
            (let loop ((expr-list expr-list))
              (if (null? expr-list)
                '()
                (let* ((expr (car expr-list))
                       (new-expr (proc expr env))
                       (rest (cdr expr-list)))
                  (cond
                   ((sal-ast-table/get expr-table new-expr)
                    ;; found another occurrence of the same expression
                    (loop rest))
                   ((or (sal-ast-table/get expr-table (make-sal-not new-expr))
                        (and-instance-of? ((new-expr <sal-not>))
                          (sal-ast-table/get expr-table (slot-value new-expr :arg))))
                    ;; found the negation of a previously found expression
                    (exit (make-sal-not+ (proc-make-true app))))
                   ((proc-true? new-expr)
                    (loop rest))
                   ((proc-false? new-expr)
                    (exit new-expr))
                   ((eq? (class-of new-expr) app-class)
                    (loop (append (sal-application/argument-list new-expr) rest)))
                   (else 
                    (sal-ast-table/put! expr-table new-expr #unspecified)
                    (let ((new-rest (loop rest)))
                      (if (and (eq? expr new-expr)
                               (eq? rest new-rest))
                        expr-list
                        (cons new-expr new-rest))))))))))
      (cond
       ((null? new-expr-list)
        (proc-make-true app))
       ((null? (cdr new-expr-list))
        (car new-expr-list))
       ((eq? new-expr-list expr-list)
        app)
       (else 
        (update-ast-slots app
                          :arg (apply make-application-argument new-expr-list)))))))

(define-method (sal-ast/local-simplify-core (ast <sal-and>) (env <primitive>) (proc <primitive>))
  (sal-nary-boolean-op/local-simplify ast env proc sal-expr/true? sal-expr/false? make-sal-true))

(define-method (sal-ast/local-simplify-core (ast <sal-or>) (env <primitive>) (proc <primitive>))
  (sal-nary-boolean-op/local-simplify ast env proc sal-expr/false? sal-expr/true? make-sal-false))

(define-method (sal-ast/local-simplify-core (ast <sal-implies>) (env <primitive>) (proc <primitive>))
  (let* ((arg1 (sal-binary-application/arg1 ast))
         (arg2 (sal-binary-application/arg2 ast))
         (new-arg1 (proc arg1 env)))
    (if (sal-expr/false? new-arg1)
      (make-sal-true ast)
      (let ((new-arg2 (proc arg2 env)))
        (cond
         ((sal-expr/true? new-arg1)
          new-arg2)
         ((sal-expr/false? new-arg2)
          (make-sal-not+ new-arg1))
         ((sal-ast/equivalent? new-arg1 new-arg2)
          (make-sal-true ast))
         (else
          (let ((not-new-arg1 (make-sal-not+ new-arg1)))
            (make-sal-or* (list not-new-arg1 new-arg2) ast))))))))

(define-method (sal-ast/local-simplify-core (ast <sal-add>) (env <primitive>) (proc <primitive>))
  (let* ((arg-list (sal-application/argument-list ast))
         (new-arg-list 
          (let loop ((arg-list arg-list)
                     (value *mpq-zero*))
            (if (null? arg-list)
              (if (=mpq value *mpq-zero*)
                '()
                (list (make-sal-numeral value ast)))
              (let* ((arg (car arg-list))
                     (new-arg (proc arg env))
                     (rest (cdr arg-list)))
                (or
                 (and-instance-of? ((new-arg <sal-numeral>))
                   (loop rest (+mpq value (slot-value new-arg :num))))
                 (and-instance-of? ((new-arg <sal-add>))
                   (append (sal-application/argument-list new-arg) (loop rest value)))
                 (let ((new-rest (loop rest value)))
                   (if (and (eq? arg new-arg)
                            (eq? rest new-rest))
                     arg-list
                     (cons new-arg new-rest)))))))))
    (cond
     ((null? new-arg-list)
      (make-sal-numeral *mpq-zero* ast))
     ((null? (cdr new-arg-list))
      (car new-arg-list))
     ((eq? arg-list new-arg-list)
      ast)
     (else
      (update-ast-slots ast
                        :arg (apply make-application-argument new-arg-list))))))

(define (sal-binary-arith-op/local-simplify ast env proc mpq-proc check-args)
  (multiple-value-bind
      (arg1 arg2)
      (sal-binary-application/arguments ast)
    (let ((new-arg1 (proc arg1 env))
          (new-arg2 (proc arg2 env)))
      (check-args new-arg1 new-arg2 ast)
      (or
       (and-instance-of? ((new-arg1 <sal-numeral>)
                          (new-arg2 <sal-numeral>))
         (copy-ast new-arg1
                   :num (try (mpq-proc (slot-value new-arg1 :num)
                                       (slot-value new-arg2 :num))
                             (lambda (e p m o)
                               (sign-source-error ast "Error evaluating arithmetic operation. Reason: ~a" m))))) 
       (and (eq? arg1 new-arg1) 
            (eq? arg2 new-arg2)
            ast)
       (copy-ast ast
                 :arg (make-application-argument new-arg1 new-arg2))))))

(define (no-check arg1 arg2 app)
  #unspecified)

(define-method (sal-ast/local-simplify-core (ast <sal-sub>) (env <primitive>) (proc <primitive>))
  (sal-binary-arith-op/local-simplify ast env proc -mpq no-check))

(define-method (sal-ast/local-simplify-core (ast <sal-mul>) (env <primitive>) (proc <primitive>))
  (bind-exit (exit)
    (let* ((arg-list (sal-application/argument-list ast))
           (new-arg-list 
            (let loop ((arg-list arg-list)
                       (value *mpq-one*))
              (if (null? arg-list)
                (if (=mpq value *mpq-one*)
                  '()
                  (list (make-sal-numeral value ast)))
                (let* ((arg (car arg-list))
                       (new-arg (proc arg env))
                       (rest (cdr arg-list)))
                  (cond
                   ((instance-of? new-arg <sal-numeral>)
                    (ensure ((new-arg <sal-numeral>))
                      (if (=mpq (slot-value new-arg :num) *mpq-zero*)
                        (exit new-arg)
                        (loop rest (*mpq value (slot-value new-arg :num))))))
                   ((instance-of? new-arg <sal-mul>)
                    (append (sal-application/argument-list new-arg) (loop rest value)))
                   (else
                    (let ((new-rest (loop rest value)))
                      (if (and (eq? arg new-arg)
                               (eq? rest new-rest))
                        arg-list
                        (cons new-arg new-rest))))))))))
      (cond
       ((eq? arg-list new-arg-list)
        ast)
       ((null? new-arg-list)
        (make-sal-numeral *mpq-one* ast))
       ((null? (cdr new-arg-list))
        (car new-arg-list))
       (else
        (update-ast-slots ast
                          :arg (apply make-application-argument new-arg-list)))))))

(define (check-division-by-zero arg1 arg2 place)
  (when (and-instance-of? ((arg2 <sal-numeral>))
          (=mpq (slot-value arg2 :num) *mpq-zero*))
    (sign-source-error place "Division by zero.")))

;; just try the trivial... 
(define-method (sal-ast/local-simplify-core (ast <sal-div>) (env <primitive>) (proc <primitive>))
  (sal-binary-arith-op/local-simplify ast env proc /mpq check-division-by-zero))

(define (check-if-integer arg place)
  (when (and-instance-of? ((arg <sal-numeral>))
          (not (mpq/integer? (slot-value arg :num))))
    (sign-source-error place "Invalid noninteger argument.")))

(define (check-integer-division arg1 arg2 place)
  (check-division-by-zero arg1 arg2 place)
  (check-if-integer arg1 place)
  (check-if-integer arg2 place))

(define-method (sal-ast/local-simplify-core (ast <sal-mod>) (env <primitive>) (proc <primitive>))
  (sal-binary-arith-op/local-simplify ast env proc %mpq check-integer-division))

(define-method (sal-ast/local-simplify-core (ast <sal-idiv>) (env <primitive>) (proc <primitive>))
  (sal-binary-arith-op/local-simplify ast env proc div-mpq check-integer-division))
   
(define (sal-arith-relation/local-simplify app env proc mpq-proc)
  (multiple-value-bind
      (arg1 arg2)
      (sal-binary-application/arguments app)
    (let ((new-arg1 (proc arg1 env))
          (new-arg2 (proc arg2 env)))
      (or
       (and-instance-of? ((new-arg1 <sal-numeral>)
                          (new-arg2 <sal-numeral>))
         (if (mpq-proc (slot-value new-arg1 :num)
                       (slot-value new-arg2 :num))
           (make-sal-true app)
           (make-sal-false app)))
       (and (eq? arg1 new-arg1) 
            (eq? arg2 new-arg2)
            app)
       (copy-ast app
                 :arg (make-application-argument new-arg1 new-arg2))))))

(define (expand-ring-application app env proc pred?)
  (let* ((place-provider app)
         (arg (slot-value app :arg))
         (arg-type (sal-expr/type arg))
         (zero (make-sal-numeral 0 place-provider))
         (one (make-sal-numeral 1 place-provider))
         (n (sal-bounded-subtype/upper arg-type))
         (aux-decl (make-ast-instance <sal-var-decl> place-provider
                                      :id (make-sal-identifier place-provider 'val)
                                      :type arg-type))
         (aux-name (make-sal-name-expr aux-decl place-provider))
         (body (if pred?
                 (make-ast-instance <sal-conditional> place-provider
                                    :cond-expr (make-sal-equality aux-name zero)
                                    :then-expr n
                                    :else-expr (make-sal-builtin-application <sal-sub> place-provider aux-name one))
                 (make-ast-instance <sal-conditional> place-provider
                                    :cond-expr (make-sal-equality aux-name n)
                                    :then-expr zero
                                    :else-expr (make-sal-builtin-application <sal-add> place-provider aux-name one)))))
    (make-sal-let+ (list aux-decl) body (list arg) env proc)))

(define-method (sal-ast/local-simplify-core (app <sal-ring-pre>) (env <primitive>) (proc <primitive>))
  (expand-ring-application app env proc #t))

(define-method (sal-ast/local-simplify-core (app <sal-ring-succ>) (env <primitive>) (proc <primitive>))
  (expand-ring-application app env proc #f))
  
(define-method (sal-ast/local-simplify-core (app <sal-lt>) (env <primitive>) (proc <primitive>))
  (sal-arith-relation/local-simplify app env proc <mpq))

(define-method (sal-ast/local-simplify-core (app <sal-le>) (env <primitive>) (proc <primitive>))
  (sal-arith-relation/local-simplify app env proc <=mpq))

(define-method (sal-ast/local-simplify-core (ast <sal-conditional>) (env <primitive>) (proc <primitive>))
  (let ((new-cond (proc (slot-value ast :cond-expr) env))
        (place-source ast))
    ;; (breakpoint "sal-ast/local-simplify-core" (ast new-cond env proc) (instance-of? (slot-value ast :then-expr) <sal-lt>))
    (cond
     ((instance-of? new-cond <sal-true>)
      (proc (slot-value ast :then-expr) env))
     ((instance-of? new-cond <sal-false>)
      (proc (slot-value ast :else-expr) env))
     (else
      (let ((then-expr (proc (slot-value ast :then-expr) env))
            (else-expr (proc (slot-value ast :else-expr) env)))
        (cond
         ((sal-ast/equivalent? then-expr else-expr)
          then-expr)
         ((instance-of? then-expr <sal-true>)
          (make-simplified-sal-builtin-application <sal-or> place-source new-cond else-expr))
         ((instance-of? then-expr <sal-false>)
          (make-simplified-sal-builtin-application <sal-and> place-source
                                                   (make-sal-not+ new-cond)
                                                   else-expr))
         ((instance-of? else-expr <sal-false>)
          (make-simplified-sal-builtin-application <sal-and> place-source new-cond then-expr))
         ((instance-of? else-expr <sal-true>)
          (make-simplified-sal-builtin-application <sal-or> place-source
                                                   (make-sal-not+ new-cond)
                                                   then-expr))
         (else
          (update-ast-slots ast
                            :cond-expr new-cond
                            :then-expr then-expr
                            :else-expr else-expr))))))))

(define-method (sal-ast/local-simplify-core (ast <sal-recognizer-application>) (env <primitive>) (proc <primitive>))
  (let ((fun (slot-value ast :fun))
        (arg (proc (slot-value ast :arg) env)))
    (cond
     ((instance-of? arg <sal-constructor>)
      (if (sal-name-expr/recognizes? fun arg)
        (make-sal-true ast)
        (make-sal-false ast)))
     ((instance-of? arg <sal-constructor-application>)
      (if (sal-name-expr/recognizes? fun (slot-value arg :fun))
        (make-sal-true ast)
        (make-sal-false ast)))
     (else
      (update-ast-slots ast :arg arg)))))

(define-method (sal-ast/local-simplify-core (ast <sal-accessor-application>) (env <primitive>) (proc <primitive>))
  (let ((fun (slot-value ast :fun))
        (arg (proc (slot-value ast :arg) env)))
    (cond
     ((instance-of? arg <sal-constructor>)
      ;; (breakpoint "sal-ast/local-simplify-core" (ast fun arg env proc) #t)
      ;; (sign-source-error ast "Invalid use of datatype accessor. Trying to use the accessor \"~a\" to access the constructor \"~a\"."
      ;;                   (sal-name-ref/name fun)
      ;;                   (sal-name-ref/name arg)))
      (update-ast-slots ast :arg arg))
     ((instance-of? arg <sal-constructor-application>)
      (let* ((constructor (slot-value arg :fun))
             (accessors (sal-name-expr/constructor-accessors constructor)))
;         (unless (exists (cut sal-ast/equivalent? fun <>) accessors)
;           (sign-source-error ast "Invalid use of datatype accessor. Trying to use the accessor \"~a\" to access the constructor \"~a\"."
;                              (sal-name-ref/name fun)
;                              (sal-name-ref/name constructor)))
        (cond
         ((not (exists (cut sal-ast/equivalent? fun <>) accessors))
          (update-ast-slots ast :arg arg))
         (else 
          (let loop ((args (sal-application/force-argument-list arg (length accessors)))
                     (accessors accessors))
            [assert (args accessors) (= (length args) (length accessors))]
            [assert (args) (not (null? args))]
            (if (sal-ast/equivalent? (car accessors) fun)
              (car args)
              (loop (cdr args) (cdr accessors))))))))
     (else
      (update-ast-slots ast :arg arg)))))

(define (sal-let-expr/local-simplify ast)
  (ensure ((ast <sal-let-expr>))
    (let* ((local-decls (slot-value ast :local-decls))
           (body (slot-value ast :expr))
           (num-ref-proc (sal-ast/number-of-references-proc body local-decls))
           (to-remove-local-decls (filter (lambda (decl)
                                            (or (<= (num-ref-proc decl) 1)
                                                (instance-of? (slot-value decl :value) <sal-simple-expr>)))
                                          local-decls)))
      (if (null? to-remove-local-decls)
        ast
        (let* ((subst-env (update-env* (make-empty-env) to-remove-local-decls (map (cut slot-value <> :value) to-remove-local-decls)))
               (new-body (sal-ast/substitute body subst-env)))
          (if (= (length to-remove-local-decls) (length local-decls))
            new-body
            (copy-ast ast 
                      :local-decls (filter (lambda (decl) (not (memq decl to-remove-local-decls))) local-decls)
                      :expr new-body)))))))

(define-method (sal-ast/local-simplify-core (ast <sal-let-expr>) (env <primitive>) (proc <primitive>))
  (let* ((local-decls (slot-value ast :local-decls))
         (body (slot-value ast :expr))
         (num-ref-proc (sal-ast/number-of-references-proc body local-decls))
         (new-env env)
         (new-local-decls (conservative-map-filter (lambda (local-decl)
                                                     (tlet ((new-local-decl <sal-let-decl> (proc local-decl env)))
                                                       (cond
                                                        ((or (<= (num-ref-proc local-decl) 1)
                                                             (instance-of? (slot-value new-local-decl :value) <sal-simple-expr>))
                                                         (set! new-env (update-env new-env local-decl (slot-value new-local-decl :value)))
                                                         #f) ;; filter element
                                                        (else
                                                         (set! new-env (update-env new-env local-decl new-local-decl))
                                                         new-local-decl))))
                                                   local-decls))
         ;; (_ [breakpoint "simplify" (new-local-decls local-decls ast env proc new-env) #t])
         (new-body (proc body new-env)))
    (if (null? new-local-decls)
      new-body
      (update-ast-slots ast
                        :local-decls new-local-decls
                        :expr new-body))))

; (define-method (sal-ast/local-simplify-core (ast <sal-let-expr>) (env <primitive>) (proc <primitive>))
;   (let ((new-ast (call-next-method)))
;     (sal-let-expr/local-simplify new-ast)))

(define (make-sal-let+ local-decls body arg-list env proc)
  (let* ((new-arg-list (map (cut proc <> env) arg-list))
         (new-local-decls (map (lambda (local-decl new-arg)
                                 (let ((result (change-ast-class local-decl <sal-let-decl>)))
                                   (set-slot-value! result :id (make-sal-identifier local-decl (gen-unique-name (slot-value (slot-value local-decl :id) :name))))
                                   (set-slot-value! result :value new-arg)
                                   result))
                               local-decls
                               new-arg-list))
         (env (update-env* env local-decls new-local-decls))
         (new-body (proc body env))
         (result (make-ast-instance <sal-let-expr> (car new-arg-list)
                                    :local-decls new-local-decls
                                    :expr new-body)))
    (with-output-to-trace 'simplify
                          (print "make-sal-let+")
                          (sal/pp result)
                          (print "\nAfter simplification:")
                          (sal/pp (sal-let-expr/local-simplify result))
                          (print "\n---------------"))
;;    [breakpoint "make-sal-let+" (local-decls body arg-list env proc new-arg-list new-local-decls new-body result) #t]
    (sal-let-expr/local-simplify result)))

(define (collection-selection/local-simplify expr env proc eval-literal)
  (let ((target (proc (slot-value expr :target) env)))
    (cond
     ((instance-of? target <sal-collection-literal>)
      (eval-literal target (slot-value expr :idx)))
     (else
      (update-ast-slots expr :target target)))))

(define-method (sal-ast/local-simplify-core (expr <sal-record-literal>) (env <primitive>) (proc <primitive>))
  (update-ast-slots expr :entries (conservative-map-1 (cut proc <> env) (slot-value expr :entries))))
  
(define-method (sal-ast/local-simplify-core (expr <sal-tuple-literal>) (env <primitive>) (proc <primitive>))
  (update-ast-slots expr :exprs (conservative-map-1 (cut proc <> env) (slot-value expr :exprs))))

(define-method (sal-ast/local-simplify-core (ast <sal-tuple-selection>) (env <primitive>) (proc <primitive>))
  (collection-selection/local-simplify ast env proc sal-tuple-literal/element))

(define-method (sal-ast/local-simplify-core (ast <sal-record-selection>) (env <primitive>) (proc <primitive>))
  (collection-selection/local-simplify ast env proc sal-record-literal/element))

(define-method (sal-ast/local-simplify-core (ast <sal-array-selection>) (env <primitive>) (proc <primitive>))
  (let ((fun (proc (slot-value ast :fun) env))
        (arg (slot-value ast :arg)))
    (cond
     ((and (instance-of? fun <sal-name-expr>)
           (sal-name-expr/definition fun))
      =>
      (lambda (definition)
        (or (try-to-evaluate-function definition arg env proc)
            (call-next-method))))
     ((instance-of? fun <sal-array-literal>)
      (ensure ((fun <sal-array-literal>))
        (make-sal-let+ (slot-value fun :local-decls) (slot-value fun :expr) (list arg) env proc)))
     ((instance-of? fun <sal-array-update>)
      (let* ((new-arg (proc arg env))
             (default (lambda (fun)
                        (let ((result (update-ast-slots ast :fun fun :arg new-arg)))
                          (if (instance-of? fun <sal-lambda>)
                            ;; apply simplification again...
                            (proc result env)
                            result)))))
        (let loop ((fun fun))
          (if (instance-of? fun <sal-array-update>)
            (ensure ((fun <sal-array-update>))
              (case (sal-expr/equal-values? (slot-value fun :idx) new-arg env)
                ((#t)
                 (slot-value fun :new-value))
                ((#f)
                 (if (instance-of? (slot-value fun :target) <sal-array-update>)
                   (loop (slot-value fun :target))
                   (default (slot-value fun :target))))
                (else
                 ;; don't know if the values are equal or not... use conservative approach
                 (default fun))))
            (default fun)))))
     (else
      (call-next-method)))))

(define (collection-update/local-simplify expr env proc eval-literal)
  (let* ((target (proc (slot-value expr :target) env))
         (new-value (proc (slot-value expr :new-value) env)))
    (cond
     ((instance-of? target <sal-collection-literal>)
      (eval-literal target (slot-value expr :idx) new-value))
     (else
      (update-ast-slots expr
                        :target target
                        :new-value new-value)))))

(define-method (sal-ast/local-simplify-core (ast <sal-tuple-update>) (env <primitive>) (proc <primitive>))
  (collection-update/local-simplify ast env proc sal-tuple-literal/update))

(define-method (sal-ast/local-simplify-core (ast <sal-record-update>) (env <primitive>) (proc <primitive>))
  (collection-update/local-simplify ast env proc sal-record-literal/update))

;; function updates are just macros in the current implementation
(define (sal-function-update/expand ast env proc)
  [assert (ast) (instance-of? ast <sal-function-update>)]
  (let* ((place-provider ast)
         (target (proc (slot-value ast :target) env))
         (idx (proc (slot-value ast :idx) env))
         (new-value (proc (slot-value ast :new-value) env))
         (target-type (sal-expr/type target))
         (arity (sal-function-type/arity target-type))
         (arg-types (sal-function-type/domain-types target-type))
         (arg-values (cond
                      ((= arity 1)
                       (list idx))
                      (else
                       ;; (breakpoint "simp function update" (ast target idx new-value target-type arity) #t)
                       (unless (instance-of? idx <sal-tuple-literal>)
                         (sign-source-error ast "Invalid function update, wrong number of arguments."))
                       (slot-value idx :exprs))))
         (arg-names (generate-list (lambda (_) (gen-unique-name 'idx)) arity))
         (arg-ids (map (cut make-sal-identifier place-provider <>) arg-names))
         (arg-decls (map (lambda (arg-id arg-type)
                           (make-ast-instance <sal-var-decl> place-provider
                                              :id arg-id
                                              :type arg-type))
                         arg-ids
                         arg-types))
         (arg-name-exprs (map (lambda (decl)
                                (make-sal-name-expr decl place-provider))
                              arg-decls))
         (arg-eq-ids (map make-sal-equality arg-name-exprs arg-values))
         (target-arg (sal-expr/apply target (apply make-application-argument arg-name-exprs)))
         (conditional (make-ast-instance <sal-conditional> place-provider
                                         :cond-expr (apply make-sal-and arg-eq-ids)
                                         :then-expr new-value
                                         :else-expr target-arg)))
    (make-ast-instance <sal-lambda> place-provider
                       :local-decls arg-decls
                       :expr conditional)))

(define-method (sal-ast/local-simplify-core (ast <sal-function-update>) (env <primitive>) (proc <primitive>))
  (sal-function-update/expand ast env proc))

;; I do not convert array updates in array literals... since I will loose information for the decision procedure
(define-method (sal-ast/local-simplify-core (ast <sal-array-update>) (env <primitive>) (proc <primitive>))
  (sal-ast/map ast env proc))

;   (let* ((target (proc (slot-value ast :target) env))
;          (idx (proc (slot-value ast :idx) env))
;          (new-value (proc (slot-value ast :new-value) env))
;          (target-type (sal-expr/type target))
;          (domain (sal-function-type/domain target-type))
;          (arg-name (gen-unique-name 'idx))
;          (place-provider ast)
;          (arg-id (make-sal-identifier place-provider arg-name))
;          (arg-decl (make-ast-instance <sal-var-decl> place-provider
;                                       :id arg-id
;                                       :type domain))
;          (arg-name-expr (make-ast-instance <sal-name-expr> place-provider
;                                            :decl arg-decl))
;          (arg-eq-idx (make-sal-equality arg-name-expr idx))
;          (target-arg (sal-expr/apply target arg-name-expr))
;          (conditional (make-ast-instance <sal-conditional> ast
;                                          :cond-expr arg-eq-idx
;                                          :then-expr new-value
;                                          :else-expr target-arg)))
;     (make-ast-instance <sal-array-literal> ast
;                        :local-decls (list arg-decl)
;                        :expr conditional)))
         
(define-generic (sal-quantified-ast/local-simplify-core expr quantifier var-decls))

(define-method (sal-quantified-ast/local-simplify-core (expr <sal-expr>) (quantifier <sal-quantified-expr>) (var-decls <primitive>))
  (let ((local-decls (conservative-filter (lambda (decl) (sal-ast/contains-reference? expr decl)) var-decls)))
    (if (null? local-decls)
      expr
      (update-ast-slots quantifier
                        :local-decls local-decls
                        :expr expr))))

(define-method (sal-quantified-ast/local-simplify-core (expr <sal-not>) (quantifier <sal-exists-expr>) (var-decls <primitive>))
  (make-sal-not+
   (sal-quantified-ast/local-simplify-core (slot-value expr :arg) (change-ast-class quantifier <sal-for-all-expr>) var-decls)))

(define-method (sal-quantified-ast/local-simplify-core (expr <sal-not>) (quantifier <sal-for-all-expr>) (var-decls <primitive>))
  (make-sal-not+
   (sal-quantified-ast/local-simplify-core (slot-value expr :arg) (change-ast-class quantifier <sal-exists-expr>) var-decls)))

(define-method (sal-quantified-ast/local-simplify-core (expr <sal-and>) (quantifier <sal-for-all-expr>) (var-decls <primitive>))
  (apply make-sal-and+
         (map (cut sal-quantified-ast/local-simplify-core <> quantifier var-decls) (sal-application/argument-list expr))))

(define-method (sal-quantified-ast/local-simplify-core (expr <sal-or>) (quantifier <sal-exists-expr>) (var-decls <primitive>))
  (make-sal-or+*  (map (cut sal-quantified-ast/local-simplify-core <> quantifier var-decls) 
                       (sal-application/argument-list expr))
                  expr))

(define-method (sal-quantified-ast/local-simplify-core (expr <sal-choice>) (quantifier <sal-exists-expr>) (var-decls <primitive>))
  (change-ast-class (call-next-method) <sal-choice>))

;; separate the expressions in different lists.
;; Suppose var-decls is x_0, ... x_n
;; - the first list will contains the expressions
;;   that do not use x0,..., x_n
;; - the second the ones that use x_0, but not x_1,...x_n
;; - the third the ones that use x_0 or x_1, but not x_2,...x_n
;; - ...
;; The result is a list of such lists
(define (distribute-exprs expr-list var-decls)
  (let ((result-queue (make-queue)))
    (let loop ((expr-list expr-list)
               (var-decls var-decls))
      (cond
       ((null? expr-list)
        (queue->list result-queue))
       ((null? var-decls)
        (queue/insert! result-queue expr-list)
        (queue->list result-queue))
       (else
        (let ((curr-expr-queue (make-queue))
              (rest-expr-queue (make-queue)))
          (for-each (lambda (expr)
                      (if (sal-ast/contains-reference*? expr var-decls)
                        (queue/insert! rest-expr-queue expr)
                        (queue/insert! curr-expr-queue expr)))
                    expr-list)
          (queue/insert! result-queue (queue->list curr-expr-queue))
          (loop (queue->list rest-expr-queue) (cdr var-decls))))))))

;; Simplify (exists (v1 .. vi) (and e1 ... en))
;; It also simplifies (for-all (v1 .. vi) (or e1 ... en)), but mk-and should
;; reference mk-or
(define (simplify-exists-and expr-list var-decls quantifier mk-and)
  (let ((expr-list-list (distribute-exprs expr-list var-decls)))
    [assert (expr-list-list) (not (null? expr-list-list))]
    (let loop ((expr-list-list expr-list-list)
               (var-decls (cons #f var-decls)))
      ;; (breakpoint "exists" (expr-list-list var-decls expr-list var-decls) #t)
      (cond 
       ((null? (cdr expr-list-list))
        (let ((expr-list (car expr-list-list)))
          [assert (expr-list) (not (null? expr-list))]
          ;; (breakpoint "exists 2" (expr-list expr-list-list) #t)
          (if (not (car var-decls))
            (apply mk-and expr-list)
            (update-ast-slots quantifier
                              :local-decls (list (car var-decls))
                              :expr (apply mk-and expr-list)))))
       (else
        (let ((rest-expr (loop (cdr expr-list-list) (cdr var-decls)))
              (expr-list (car expr-list-list)))
          ;; (breakpoint "exists 3" (rest-expr expr-list expr-list-list var-decls) #t)
          (cond
           ((not (car var-decls))
            (apply mk-and (append expr-list (list rest-expr))))
           ((null? expr-list)
            (if (sal-ast/contains-reference? rest-expr (car var-decls))
              (update-ast-slots rest-expr
                                :local-decls (cons (car var-decls) (slot-value rest-expr :local-decls)))
              rest-expr))
           (else
            (update-ast-slots rest-expr
                              :local-decls (list (car var-decls))
                              :expr (apply mk-and (append expr-list
                                                          (list rest-expr))))))))))))
                                          
(define-method (sal-quantified-ast/local-simplify-core (expr <sal-and>) (quantifier <sal-exists-expr>) (var-decls <primitive>))
  (simplify-exists-and (sal-application/argument-list expr) var-decls quantifier make-sal-and+))

(define-method (sal-quantified-ast/local-simplify-core (expr <sal-or>) (quantifier <sal-for-all-expr>) (var-decls <primitive>))
  (simplify-exists-and (sal-application/argument-list expr) var-decls quantifier make-sal-or+))

(define-method (sal-quantified-ast/local-simplify-core (expr <sal-choice>) (quantifier <sal-for-all-expr>) (var-decls <primitive>))
  (simplify-exists-and (sal-application/argument-list expr) var-decls quantifier make-sal-choice+))

(define (combine-quantifiers quantifier ref-quantifier var-decls)
  (ensure ((quantifier <sal-quantified-expr>)
           (ref-quantifier <sal-quantified-expr>))
    [assert (quantifier ref-quantifier) (eq? (instance-of? quantifier <sal-exists-expr>) (instance-of? ref-quantifier <sal-exists-expr>))]
    [assert (quantifier ref-quantifier) (eq? (instance-of? quantifier <sal-for-all-expr>) (instance-of? ref-quantifier <sal-for-all-expr>))]
    (let* ((local-decls (slot-value quantifier :local-decls))
           (num-ref-proc (sal-ast-list/number-of-references-proc local-decls var-decls)))
      (multiple-value-bind
          ;; ref-var-decls is a "subset" of var-decls which is referenced by local-decls
          (ref-var-decls non-ref-var-decls)
          (split-list var-decls (lambda (var-decl) (> (num-ref-proc var-decl) 0)))
        (let ((new-body (sal-quantified-ast/local-simplify-core (slot-value quantifier :expr) ref-quantifier
                                                                (append non-ref-var-decls local-decls))))
          (if (null? ref-var-decls)
            new-body
            (update-ast-slots ref-quantifier
                              :local-decls ref-var-decls
                              :expr new-body)))))))

(define-method (sal-quantified-ast/local-simplify-core (expr <sal-exists-expr>) (quantifier <sal-exists-expr>) (var-decls <primitive>))
  (combine-quantifiers expr quantifier var-decls))

(define-method (sal-quantified-ast/local-simplify-core (expr <sal-for-all-expr>) (quantifier <sal-for-all-expr>) (var-decls <primitive>))
  (combine-quantifiers expr quantifier var-decls))

(define (sal-quantified-expr/local-simplify ast)
  (sal-quantified-ast/local-simplify-core (slot-value ast :expr) ast (slot-value ast :local-decls)))

(define-method (sal-ast/local-simplify-core (ast <sal-quantified-expr>) (env <primitive>) (proc <primitive>))
  (let* ((new-ast (call-next-method))
	 (result (sal-quantified-expr/local-simplify new-ast)))
;;    [breakpoint "sal-ast/local-simplify-core" (ast env new-ast result proc) #t]
    result))

(define (sal-eq/local-simplify-core app env proc proc-true? proc-false? mk-true mk-false)
  (multiple-value-bind
   (arg1 arg2)
   (sal-binary-application/arguments app)
   (let* ((new-arg1 (proc arg1 env))
	  (new-arg2 (proc arg2 env))
	  (default-proc (lambda ()
			  (if (and (eq? arg1 new-arg1)
				   (eq? arg2 new-arg2))
			      app
			      (copy-ast app
					:arg (make-application-argument new-arg1 new-arg2))))))
     (cond
      ((sal-ast/equivalent? new-arg1 new-arg2)
       (mk-true app))
      ((and (sal-expr/first-order-value? new-arg1) 
	    (sal-expr/first-order-value? new-arg2))
       ;; new-arg1 and new-arg2 are first order values, but they are not equivalent, so
       ;; they are not equal.
       (mk-false app))
      ((and (instance-of? new-arg1 <sal-lambda>)
	    (instance-of? new-arg2 <sal-lambda>))
       (let ((result (sal-function/extensional-equality? new-arg1 new-arg2 env)))
	 (cond 
	  ((eq? result #t) (mk-true app))
	  ((eq? result #f) (mk-false app))
	  (else (default-proc)))))
      ((proc-true? new-arg1)
       new-arg2)
      ((proc-true? new-arg2)
       new-arg1)
      ((proc-false? new-arg1)
       (make-simplified-sal-builtin-application <sal-not> app
						new-arg2))
      ((proc-false? new-arg2)
       (make-simplified-sal-builtin-application <sal-not> app
						new-arg1))
      (else
       (default-proc))))))

(define-method (sal-ast/local-simplify-core (ast <sal-eq>) (env <primitive>) (proc <primitive>))
  (sal-eq/local-simplify-core ast env proc sal-expr/true? sal-expr/false? make-sal-true make-sal-false))

(define-method (sal-ast/local-simplify-core (ast <sal-assignment>) (env <primitive>) (proc <primitive>))
  ;; I do not simplify assignments
  (sal-ast/map ast env proc))

(define-method (sal-ast/local-simplify-core (ast <sal-diseq>) (env <primitive>) (proc <primitive>))
  (sal-eq/local-simplify-core ast env proc sal-expr/false? sal-expr/true? make-sal-false make-sal-true))

(define (sal-ast/simplify-core ast env)
  (sal-ast/local-simplify-core ast env sal-ast/simplify-core))

(define (sal-ast/simplify ast)
  (sal-ast/simplify-core ast (make-empty-env)))

(define-generic (sal-ast/eager-local-simplify ast env proc))

(define-method (sal-ast/eager-local-simplify (ast <sal-ast>) (env <primitive>) (proc <primitive>))
  (sal-ast/local-simplify-core ast env proc))

(define-method (sal-ast/eager-local-simplify (ast <sal-application>) (env <primitive>) (proc <primitive>))
  (try-until-success
   (let* ((new-ast (sal-expr/evaluate-core ast env 0))
          (result (if (sal-ast/equivalent? new-ast ast)
                    new-ast
                    (proc new-ast env))))
     (unless (< (sal-ast/size result) (sal-ast/size ast))
       (fail))
     result)
   (sal-ast/local-simplify-core ast env proc)))

(define (sal-ast/eager-simplify-core ast env)
  (sal-ast/eager-local-simplify ast env sal-ast/eager-simplify-core))

(define (sal-ast/eager-simplify ast)
  (sal-ast/eager-simplify-core ast (make-empty-env)))
 

(define-generic (sal-ast/simplify-without-converting-inequalities-core ast env))
(define-method (sal-ast/simplify-without-converting-inequalities-core (ast <sal-ast>) (env <primitive>))
  (sal-ast/local-simplify-core ast env sal-ast/simplify-without-converting-inequalities-core))
(define-method (sal-ast/simplify-without-converting-inequalities-core (app <sal-gt>) (env <primitive>))
  (sal-arith-relation/local-simplify app env 
                                     sal-ast/simplify-without-converting-inequalities-core
                                     >mpq))
(define-method (sal-ast/simplify-without-converting-inequalities-core (app <sal-ge>) (env <primitive>))
  (sal-arith-relation/local-simplify app env 
                                     sal-ast/simplify-without-converting-inequalities-core
                                     >=mpq))
(define (sal-ast/simplify-without-converting-inequalities ast)
  (sal-ast/simplify-without-converting-inequalities-core ast (make-empty-env)))

  
  
