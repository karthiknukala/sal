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

(module sat-generic-context
        (include "sal.sch")
        (include "fast-hash-table.sch")
        (import sal-ast-copy sal-ast-env sal-ast-simplify front-end runtime
                queue unique-names sal-ast-expand sal-expression sal-ast-for-each
                sal-type sal-decls iterators sal-pp sal-ast-list polarity
                sal-ast-table sal-cse sal-type-membership wttree sal-finite-expressions
                sat-context sal-environment gmp-scheme)
        (export <sat-generic-context>
                (sat-context/super-type type)
                *sat-generic-context-eliminate-array-theory*
                *sat-generic-context-map-scalars-to-booleans*
                (sat-generic-context/set-array-theory-elemination-flag! flag)
                (sat-generic-context/set-scalar2bool-flag! flag)
                (make-sat-generic-context place-provider)
                (sat-generic-context/init! ctx place-provider)
                (make-bool-aux-decl! decl-queue place-provider)
                (make-bool-aux-name-expr! decl-queue place-provider)
                (sat-generic-context/contains-instance? sat-generic-context class)
                (sat-generic-context/skolemize ast env proc)
                (sat-generic-context/remove-let ast env proc-let-value proc-expr)
                (sat-generic-context/apply-simple-transformation! sat-generic-context transformation-proc msg)
                (sat-generic-context/apply-transformation! sat-generic-context transformation-proc msg)
                (sat-generic-context/pp ctx)
                (sat-generic-context/size sat-generic-context)
                <sat-decl>
                <sat-global-decl>
                <sat-aux-decl>
                <sat-scalar-bit-decl>
                <sal-cyclic-eq>
                <sat-skolem-aux-decl>
                (sat-generic-context/remove-lets! sat-generic-context)
                (sat-generic-context/eliminate-array-theory! sat-generic-context)
                (sat-generic-context/eliminate-variables! sat-generic-context vars-to-eliminate)
                (sat-generic-context/eliminate-lambdas! sat-generic-context)
                (sat-generic-context/ho-eq-expansion! ctx)
                (sat-generic-context/lambda-expansion! sat-generic-context)
                (sat-generic-context/eliminate-quantifiers! sat-generic-context)
                (sat-generic-context/ite->bool-ite! sat-generic-context)
                (sat-generic-context/cse! sat-generic-context)
                (sat-generic-context/add-type-constraints! sat-generic-context)
                (sat-generic-context/convert-scalar-to-int! sat-generic-context)
                (sat-generic-context/convert-scalar-to-boolean! sat-generic-context)
                (sat-generic-context/preprocess-type-predicates! sat-generic-context)
                (sat-generic-context/simplify! sat-generic-context . svsv)
                <sat-generic-context-solver-interface>
                (solver-interface/solve! solver sat-generic-context place-provider)
                (sat-generic-context/collect-definitions! sat-generic-context . ite-as-decl?))
        )

;; *** Limitations ***
;; The following constraints are allowed:
;; - arithmethic constraints
;; - boolean constraints
;; - equality (may contain unintepreted function symbols, and scalar values)
;; - lambda expressions
;; - quantified expressions
;; 
;; interpreted functions were already eliminated by
;; previous transformations.


;; --------------------------------------------------------------------
;; Configuration section
;;
;;
;; --------------------------------------------------------------------

(define *sat-generic-context-eliminate-array-theory* #f)

(define (sat-generic-context/set-array-theory-elemination-flag! flag)
  (set! *sat-generic-context-eliminate-array-theory* flag))

(front-end/add-toggle-option! 
 "Extra Code Transformations" 
 "ate" 
 "array theory elimination (default: disabled). This transformation is effective only if SMV-like specifications are used, that is, without guarded commands."
 (lambda (flag)
   (set! *sat-generic-context-eliminate-array-theory* flag)))

(define *sat-generic-context-map-scalars-to-booleans* #t)

(define (sat-generic-context/set-scalar2bool-flag! flag)
  (set! *sat-generic-context-map-scalars-to-booleans* flag))

(front-end/add-toggle-option! 
 "Extra Code Transformations" 
 "s2b" 
 "the transformation of (non-boolean) scalars to booleans (default: enabled)."
 (lambda (flag)
   (set! *sat-generic-context-map-scalars-to-booleans* flag)))

(define (sat-context/super-type type)
  ;; Small hack to preserve the integers 
  ;; This hack should be removed when ICS start to generate concrete counterexamples, and become complete
  ;; Then, this methods should be just (sal-type/super-type type)
  (if (sal-type/integer? type)
    (make-sal-builtin-name <sal-int-type> type)
    (sal-type/super-type type)))


;; --------------------------------------------------------------------
;; SAT Contexts
;;
;;
;; --------------------------------------------------------------------

;;
;; REMARK:
;; The module sal-flat-data-structures can be used to flat (remove)
;; tuple and records in the specification.
;;

;; A <sat-generic-context> instance contains a set of contraints that can be checked for 
;; satisfiability. The slot :declaration-queue is a queue of declarations ("macros")
;; used to specify the constraints. The slot :constraint-queue represents the set
;; of constraints. Each constraint is a boolean <sal-expr> instance.
;; The slot :declaration-queue must contain all constant declarations used in
;; :constraint-queue
;; The slot :scalar-trace-info is used to store traceability information for
;; the scalar type flattening transformation described below.
;; The slot :eliminated-constraint-queue is used to store constraints eliminated
;; by a transformation. They are stored only for traceability purposes.
;; The slot :already-processed avoids the generation of typepred constraints for
;; already processed asts
(define-class <sat-generic-context> () (:declaration-queue 
                                        :constraint-queue 
                                        :scalar->bool-trace-info
                                        :scalar->int-trace-info
                                        :eliminated-declaration-queue
                                        :eliminated-constraint-queue
                                        :already-processed
                                        :place-provider
                                        ))

(define (make-sat-generic-context place-provider)
  (sat-generic-context/init! (make-instance <sat-generic-context>) place-provider))

(define (sat-generic-context/init! ctx place-provider)
  (set-slot-value! ctx :declaration-queue (make-queue))
  (set-slot-value! ctx :constraint-queue (make-queue))
  (set-slot-value! ctx :scalar->int-trace-info #f)
  (set-slot-value! ctx :scalar->bool-trace-info #f)
  (set-slot-value! ctx :eliminated-declaration-queue (make-queue))
  (set-slot-value! ctx :eliminated-constraint-queue (make-queue))
  (set-slot-value! ctx :already-processed (make-eq-hash-table))
  (set-slot-value! ctx :place-provider place-provider)
  ctx)

(define (sat-generic-context/pp ctx)
  (for-each (lambda (decl)
              (sal/pp decl)
              (print ";"))
            (queue->list (slot-value ctx :declaration-queue)))
  (for-each (lambda (expr)
              (sal/pp expr)
              (print ";"))
            (queue->list (slot-value ctx :constraint-queue))))

(define-class <sat-decl> (<sal-var-decl>) ())
(define-class <sat-global-decl> (<sat-decl>) ()) ;; used to represent uninterpreted constants in the context

;; a model for a <sat-generic-context> instance is composed of a set of atomic
;; constraints. An atomic constraint is a equality, disequality, or inequality.
;; Two special subclasses of <sal-eq> are used: <sal-var-eq-ground> and <sal-var-eq-term>.
;; Instances of <sal-var-eq-ground> represent equalities where the left-hand-side is
;; a variable, and the right-hand-side a ground value (no variables). 
;; Instances of <sal-var-eq-term> represent equalities where the left-hand-side is a
;; variable, and the right-hand-side a term which doesn't contain the variable on the
;; left-hand-side.
(define-class <sat-generic-context-model> () (:sat-generic-context :atomic-constraint-queue))

(define-class <sal-var-eq-ground> (<sal-eq>) ())
(define-class <sal-var-eq-term> (<sal-eq>) ())

;; <sat-generic-context> applies several transformations before posting a problem
;; to the decision procedure. Some of these transformations may create
;; several auxiliary declarations. These auxiliary declarations are
;; instances of <sat-aux-decl>
(define-class <sat-aux-decl> (<sat-decl>) ())

(define (make-bool-aux-decl! decl-queue place-provider)
  (let* ((bool-type (make-sal-builtin-name <sal-bool-type> place-provider))
         (name (gen-unique-name 'baux))
         (id (make-sal-identifier place-provider name))
         (var-decl (make-ast-instance <sat-aux-decl> place-provider
                                      :id id
                                      :type bool-type)))
    (queue/insert! decl-queue var-decl)
    var-decl))

(define (make-bool-aux-name-expr! decl-queue place-provider)
  (let ((var-decl (make-bool-aux-decl! decl-queue place-provider)))
    (make-sal-name-expr var-decl place-provider)))

(define-method (sat-context/make-and* (ctx <sat-generic-context>) (sat-args <primitive>))
  (make-sal-and* sat-args (slot-value ctx :place-provider)))

(define-method (sat-context/make-or* (ctx <sat-generic-context>) (sat-args <primitive>))
  (make-sal-or* sat-args (slot-value ctx :place-provider)))

(define-method (sat-context/make-not (ctx <sat-generic-context>) (sat-arg <sal-expr>))
  (make-sal-not sat-arg))

(define-method (sat-context/make-true (ctx <sat-generic-context>))
  (make-sal-true (slot-value ctx :flat-module)))

(define-method (sat-context/make-false (ctx <sat-generic-context>))
  (make-sal-false (slot-value ctx :flat-module)))

(define-method (sat-context/make-ite (ctx <sat-generic-context>) (c <sal-expr>) (t <sal-expr>) (e <sal-expr>))
  (make-ast-instance <sal-conditional> c
                     :cond-expr c
                     :then-expr t
                     :else-expr e))

(define-method (sat-context/make-eq (ctx <sat-generic-context>) (arg1 <sal-expr>) (arg2 <sal-expr>))
  (make-sal-equality arg1 arg2))

(define-method (sat-context/make-iff (ctx <sat-generic-context>) (arg1 <sal-expr>) (arg2 <sal-expr>))
  (make-sal-equality arg1 arg2))

(define-method (sat-context/make-eq (ctx <sat-generic-context>) (arg1 <sal-decl>) (arg2 <sal-decl>))
  (make-sal-equality (make-sal-name-expr arg1)
                     (make-sal-name-expr arg2)))

(define-method (sat-context/make-iff (ctx <sat-generic-context>) (arg1 <sal-decl>) (arg2 <sal-decl>))
  (make-sal-equality (make-sal-name-expr arg1)
                     (make-sal-name-expr arg2)))

;; --------------------------------------------------------------------
;; Insert a new constraint in the contraint-queue.
;; The constraint is simplified before being inserted.
;; A conjunction is breaken in pieces before being inserted.
;; --------------------------------------------------------------------
(define (constraint-queue/insert! constraint-queue expr)
  (let ((new-expr (sal-ast/simplify expr)))
    (unless (sal-expr/true? new-expr)
      (if (instance-of? new-expr <sal-and>)
        (queue/append! constraint-queue (sal-application/argument-list new-expr))
        (queue/insert! constraint-queue new-expr)))))

;; --------------------------------------------------------------------
;; Include a constraint in the <sat-generic-context>.
;; Before including a constraint, I try to remove the let declarations.
;; It is not possible to remove all let declarations since some of them
;; can be nested inside of a LAMBDA/QUANTIFIED expression.
;; The function constraint-queue/insert! is used.
;; --------------------------------------------------------------------
(define-method (sat-context/assert (ctx <sat-generic-context>) (expr <sal-expr>))
  ;; (breakpoint "sat-context/assert" (ctx expr) #t)
  ;; expr cannot contain references for name-expressions that are not instances of <sat-decl>
  (trace 'sat-generic-context "add-contraint! ~a" (sal-ast->list expr))
  (let* ((declaration-queue (slot-value ctx :declaration-queue))
         (constraint-queue (slot-value ctx :constraint-queue))
         (new-expr (if (sal-ast/find (cut instance-of? <> <sal-let-expr>) expr)
                     (sal-ast/remove-lets expr (make-empty-env) declaration-queue constraint-queue)
                     expr)))
    (constraint-queue/insert! constraint-queue new-expr)))

(define-method (sat-context/add-auxiliary-decl (ctx <sat-generic-context>) (decl <sal-decl>))
  (queue/insert! (slot-value ctx :declaration-queue) decl))

;; --------------------------------------------------------------------
;; Generic skolemization function
;;
;; proc receives:
;; 1- the body of the quantified expression
;; 2- an updated environment
;; 3- a list of new local declarations
;; --------------------------------------------------------------------
(define (sat-generic-context/skolemize ast env proc)
  (let* ((local-decls (slot-value ast :local-decls))
         (new-local-decls (map (lambda (decl)
                                 (let* ((name (sal-decl/name decl))
                                        (new-name (gen-unique-name name))
                                        (new-id (copy-ast (slot-value decl :id)
                                                          :name new-name)))
                                   (make-ast-instance <sat-skolem-aux-decl> decl
                                                      :id new-id
                                                      :type (slot-value decl :type))))
                               local-decls))
         (new-env (update-env* env local-decls new-local-decls)))
    (proc (slot-value ast :expr) new-env new-local-decls)))

;; --------------------------------------------------------------------
;; Generic let removal function
;;
;; proc-let-value is called to process the value of each let-decl,
;; it receives:
;;  1- the let value
;;  2- a new <sal-let-aux-decl>
;;  3- a new name-expr that must be equal to the let-value
;;
;; proc-expr is called to process the body of the let-expr,
;; it receives:
;;  1- the body of the let-expr 
;;  2- an updated environment
;; --------------------------------------------------------------------
(define (sat-generic-context/remove-let ast env proc-let-value proc-expr)
  (let* ((local-decls (slot-value ast :local-decls))
         (new-local-decls 
	  (map (lambda (let-decl)
		 (let* ((name (sal-decl/name let-decl))
			(new-name (gen-unique-name name))
			(new-id (copy-ast (slot-value let-decl :id)
					  :name new-name))
			(aux-decl (make-ast-instance <sat-let-aux-decl> let-decl :id new-id
					 :type (sat-context/super-type (slot-value let-decl :type))))
			;; In previous make-ast-instance, I need to use the supertype!!!! 
			;; Otherwise I can make the formula unsatisfiable...
			;; Example:
			;;  (= x 0)
			;;  (if (> x 0)
			;;     (let ((y::nat (- x 1)))
			;;       f(y))
			;;     f(0))
			;;  ===>
			;;  y :: nat
			;;  (= x 0)
			;;  (= y (- x 1))  << equals to -1 !!! unsat
			;;  (if (> x 0)
			;;     f(y)
			;;     f(0))
			(aux-name-expr (make-sal-name-expr aux-decl let-decl)))
		   (proc-let-value (slot-value let-decl :value) aux-decl aux-name-expr)
		   ;; sal-ast/remove-lets (slot-value let-decl :value) env decl-queue constraint-queue))
		   ;; (new-constraint (make-sal-equality aux-name-expr new-value)))
		   ;; (queue/insert! decl-queue aux-decl)
		   ;; (queue/insert! constraint-queue new-constraint)
		   aux-decl))
	       local-decls))
         (new-env (update-env* env local-decls new-local-decls)))
    (proc-expr (slot-value ast :expr) new-env)))
;; (sal-ast/remove-lets (slot-value ast :expr) new-env decl-queue constraint-queue)))



;; --------------------------------------------------------------------
;; Return the context size in number of subformulas/terms
;;
;; --------------------------------------------------------------------
(define (sat-generic-context/size sat-generic-context)
  (fold-left (lambda (size constraint)
               (+ size (sal-ast/size constraint)))
             0
             (queue->list (slot-value sat-generic-context :constraint-queue))))
  
;; --------------------------------------------------------------------
;; Template for applying simple transformations
;;
;; --------------------------------------------------------------------
(define (sat-generic-context/apply-simple-transformation! sat-generic-context transformation-proc msg)
  (verbose-message 3 "    ~a..." msg)
  (display-runtime 4 "      transformation time: ~a secs"
    (lambda ()
      (let ((constraint-queue (make-queue)))
        (for-each (lambda (expr)
                    (queue/insert! constraint-queue (transformation-proc expr)))
                  (queue->list (slot-value sat-generic-context :constraint-queue)))
        (set-slot-value! sat-generic-context :constraint-queue constraint-queue)))
    :generic-context-transformation-time))

;; --------------------------------------------------------------------
;; Template for applying transformations that create new constraints and declarations
;;
;; --------------------------------------------------------------------
(define (sat-generic-context/apply-transformation! sat-generic-context transformation-proc msg)
  (verbose-message 3 "    ~a..." msg)
  (display-runtime 4 "      transformation time: ~a secs"
    (lambda ()
      (let ((constraint-queue (make-queue))
            (declaration-queue (slot-value sat-generic-context :declaration-queue)))
        (for-each (lambda (expr)
                    ;; Remark: the new constraints created by transformation-proc are inserted before
                    ;;         the result of the transformation.
                    (let ((new-expr (transformation-proc expr declaration-queue constraint-queue)))
                      (queue/insert! constraint-queue new-expr)))
                  (queue->list (slot-value sat-generic-context :constraint-queue)))
        (set-slot-value! sat-generic-context :constraint-queue constraint-queue)))))

;; --------------------------------------------------------------------
;; Transformation: Removing LET declarations And Simplifying again
;;
;; Convert
;;   LET x : T = a IN f[x] 
;;
;; to
;;
;;  f[x]
;;
;;  and add the following declarations and constraints
;;
;;   x : T
;;   x = a
;;
;; The new declaration is an instance of <sat-let-aux-decl>  
;;
;; Remark: This transformation is only safe if the LET is not nested
;;         inside a LAMBDA expression. So, I do not apply the transformation inside of 
;;         LAMBDA/QUANTIFIED expressions.
;; --------------------------------------------------------------------
(define-class <sat-let-aux-decl> (<sat-aux-decl>) ())

(define-generic (sal-ast/remove-lets ast env decl-queue constraint-queue))

(define-method (sal-ast/remove-lets (ast <sal-ast>) (env <primitive>) (decl-queue <primitive>) (constraint-queue <primitive>))
  (sal-ast/map ast env (lambda (child-ast new-env) (sal-ast/remove-lets child-ast new-env decl-queue constraint-queue))))

(define-method (sal-ast/remove-lets (ast <sal-lambda>) (env <primitive>) (decl-queue <primitive>) (constraint-queue <primitive>))
  ;; do not apply the transformation inside of lambda expressions.
  (sal-ast/substitute ast env))

(define-method (sal-ast/remove-lets (ast <sal-quantified-expr>) (env <primitive>) (decl-queue <primitive>) (constraint-queue <primitive>))
  ;; do not apply the transformation inside of quantified expressions.
  (sal-ast/substitute ast env))

(define-method (sal-ast/remove-lets (ast <sal-let-expr>) (env <primitive>) (decl-queue <primitive>) (constraint-queue <primitive>))
  (sat-generic-context/remove-let ast env 
                                  (lambda (let-value new-decl new-name-expr)
                                    (let ((new-value (sal-ast/remove-lets let-value env decl-queue constraint-queue)))
                                      (queue/insert! decl-queue new-decl)
                                      (queue/insert! constraint-queue (make-sal-equality new-name-expr new-value))))
                                  (lambda (expr new-env)
                                    (sal-ast/remove-lets expr new-env decl-queue constraint-queue))))

;; the following function when used after the elimination of lambda expressions eliminate
;; any occurrence of <sal-local-binds-expr> in the constraints.
(define (sat-generic-context/remove-lets! sat-generic-context)
  (when (sat-generic-context/contains-let-expr? sat-generic-context)
    (let ((empty-env (make-empty-env)))
      (sat-generic-context/apply-transformation! sat-generic-context 
                                                 (lambda (expr decl-queue constraint-queue) 
                                                   (sal-ast/remove-lets expr empty-env decl-queue constraint-queue))
                                                 "flattening let expressions"))))
    
;; --------------------------------------------------------------------
;; Transformation: array theory elimination
;;
;; Remark: this transformation is on a flag. 
;;
;; It simply converts an array update 
;;
;;  (UPDATE T WITH [IDX] := VAL)
;;
;; into
;;
;;  (LAMBDA (idx : <DOMAIN of T>)
;;     IF idx = IDX THEN VAL
;;     ELSE T[idx] ENDIF)
;;
;; Remark: the application T[idx] must be pushed
;;
;; --------------------------------------------------------------------
(define-generic (sal-ast/eliminate-array-theory ast env))
(define-method (sal-ast/eliminate-array-theory (ast <sal-ast>) (env <primitive>))
  (sal-ast/map ast env sal-ast/eliminate-array-theory))
(define-method (sal-ast/eliminate-array-theory (ast <sal-array-update>) (env <primitive>))
  (quick-change-class! (sal-function-update/expand ast env sal-ast/eliminate-array-theory)
                       <sal-array-literal>))

(define (sat-generic-context/eliminate-array-theory! sat-generic-context)
  (let ((empty-env (make-empty-env)))
    (sat-generic-context/apply-simple-transformation! sat-generic-context (lambda (expr) (sal-ast/eliminate-array-theory expr empty-env))
                                              "eliminating array theory")))

;; --------------------------------------------------------------------
;; Check whether there is an instance of a given class
;;
;; --------------------------------------------------------------------
(define (sal-ast/find-instance ast class)
  (sal-ast/find (cut instance-of? <> class) ast))
(define-inline (sal-ast/contains-instance? ast class)
  (sal-ast/find-instance ast class))
(define (sat-generic-context/contains-instance? sat-generic-context class)
  (exists (cut sal-ast/contains-instance? <> class)
          (queue->list (slot-value sat-generic-context :constraint-queue))))
(define (sat-generic-context/find-instance sat-generic-context class)
  (bind-exit (exit)
    (for-each (lambda (expr)
                (let ((result (sal-ast/find-instance expr class)))
                  (when result
                    (exit result))))
              (queue->list (slot-value sat-generic-context :constraint-queue)))
    #f))

(define (sal-ast/contains-lambda? ast)
  (sal-ast/contains-instance? ast <sal-lambda>))
(define (sal-ast/contains-quantifier? ast)
  (sal-ast/contains-instance? ast <sal-quantified-expr>))
(define (sat-generic-context/contains-lambda? ctx)
  (sat-generic-context/contains-instance? ctx <sal-lambda>))
(define (sat-generic-context/contains-quantifier? ctx)
  (sat-generic-context/contains-instance? ctx <sal-quantified-expr>))
(define (sat-generic-context/contains-let-expr? ctx)
  (sat-generic-context/contains-instance? ctx <sal-let-expr>))
(define (sat-generic-context/find-lambda ctx)
  (sat-generic-context/find-instance ctx <sal-lambda>))
(define (sat-generic-context/find-quantifier ctx)
  (sat-generic-context/find-instance ctx <sal-quantified-expr>))
(define (sat-generic-context/contains-idiv? ctx)
  (sat-generic-context/contains-instance? ctx <sal-idiv>))
(define (sat-generic-context/contains-mod? ctx)
  (sat-generic-context/contains-instance? ctx <sal-mod>))

;; --------------------------------------------------------------------
;; Count the number of instances of an specific class
;;
;; --------------------------------------------------------------------
(define (sal-ast/num-instances ast class)
  (sal-ast/fold (lambda (curr ast)
                  (if (instance-of? ast class) 
                    (+ curr 1)
                    curr))
                0 ast))
(define (sat-generic-context/num-instances sat-generic-context class)
  (fold-left (lambda (curr expr)
               (+ curr (sal-ast/num-instances expr class)))
             0
             (queue->list (slot-value sat-generic-context :constraint-queue))))

(define (sat-generic-context/num-lambdas ast)
  (sat-generic-context/num-instances ast <sal-lambda>))

(define (sat-generic-context/num-quantifiers ast)
  (sat-generic-context/num-instances ast <sal-quantified-expr>))

;; --------------------------------------------------------------------
;; Transformation: Lambda pulling
;;
;; converts 
;; (IF C1 THEN T1 ELSE T2 ENDIF)
;;
;; Where T1 or T2 are higher-order terms to
;; 
;; (LAMBDA (idx: <DOMAIN OF T1>)
;;   (IF C1 THEN T1(a) ELSE T2(a) ENDIF))
;;
;; The transformation is only applied if the domain of the Lambda
;; is infinite.
;;
;; Remark: the application must be pushed inside T1 and T2,
;; when T1 or T2 are LAMBDAS, then apply beta-reduction
;;
;; This transformation is important for the next step
;; --------------------------------------------------------------------
(define-generic (sal-ast/pull-lambdas ast env))
(define-method (sal-ast/pull-lambdas (ast <sal-ast>) (env <primitive>))
  (sal-ast/map ast env sal-ast/pull-lambdas))
(define-method (sal-ast/pull-lambdas (ast <sal-conditional>) (env <primitive>))
  (let* ((place-provider ast)
         (cond-expr (sal-ast/pull-lambdas (slot-value ast :cond-expr) env))
         (then-expr (sal-ast/pull-lambdas (slot-value ast :then-expr) env))
         (else-expr (sal-ast/pull-lambdas (slot-value ast :else-expr) env))
         (then-type (sal-expr/type then-expr)))
    (if (and (sal-type/function? then-type)
             (not (sal-type/finite? (sal-function-type/domain then-type))))
      (let* ((domain-types (sal-function-type/domain-types then-type))
             (var-decls (map (lambda (type)
                               (let ((aux-id (make-sal-identifier place-provider (gen-unique-name 'idx))))
                                 (make-ast-instance <sal-var-decl> place-provider
                                                    :id aux-id
                                                    :type type)))
                             domain-types))
             (name-exprs (map make-sal-name-expr var-decls))
             (arg (apply make-application-argument name-exprs))
             (new-then-expr (sal-expr/lift then-expr arg env 0))
             (new-else-expr (sal-expr/lift else-expr arg env 0))
             (new-ite (copy-ast ast
                                :cond-expr cond-expr
                                :then-expr new-then-expr
                                :else-expr new-else-expr))
             (class (if (sal-type/array? then-type) <sal-array-literal> <sal-lambda>)))
        (make-ast-instance class place-provider
                           :local-decls var-decls
                           :expr new-ite))
      (update-ast-slots ast
                        :cond-expr cond-expr
                        :then-expr then-expr
                        :else-expr else-expr))))
         
;; --------------------------------------------------------------------
;; The following class is used to mark equalities that should not
;; be considered in variable elimination procedures based on the substitution
;; rule.
;; --------------------------------------------------------------------
(define-class <sal-cyclic-eq> (<sal-eq>) ())

;; --------------------------------------------------------------------
;; Eliminate variables.
;; vars-to-eliminate is a mapping from decl -> rhs.
;; vars-to-eliminate is an eq-hash-table.
;; This function replaces each occurrence of a variable which the declaration
;; is in the mapping by the associated expression.
;; Remark: The declarations in vars-to-eliminate may depend on each other.
;;         if there are mutual dependencies, then they are returned to
;;         the constraint-queue.
;;
;; --------------------------------------------------------------------
(define (sat-generic-context/eliminate-variables! sat-generic-context vars-to-eliminate)
  (let ((env (collect-independent-vars! sat-generic-context vars-to-eliminate)))
    ;; return remaining equalities to the constraint queue...
    (eq-hash-table/for-each (lambda (decl expr)
                              (let ((equality (quick-change-class! 
                                               (make-sal-equality (make-sal-name-expr decl expr)
                                                                  expr)
                                               <sal-cyclic-eq>)))
                                (sat-context/assert sat-generic-context equality)))
                            vars-to-eliminate)
    ;; apply substitution
    (let ((new-constraint-queue (make-queue)))
      (for-each (lambda (expr)
                  (constraint-queue/insert! new-constraint-queue (sal-ast/substitute expr env)))
                (queue->list (slot-value sat-generic-context :constraint-queue)))
      (set-slot-value! sat-generic-context :constraint-queue new-constraint-queue))))

(define (collect-independent-vars! sat-generic-context vars-to-eliminate)
  (let ((env (make-empty-env))
        (eliminated-constraint-queue (slot-value sat-generic-context :eliminated-constraint-queue))
        (eliminated-declaration-queue (slot-value sat-generic-context :eliminated-declaration-queue)))
    (let loop ()
      (let ((vars-to-remove-from-map '()))
        (eq-hash-table/for-each (lambda (decl expr)
                                  (unless (sal-ast/contains-reference-in-table? expr vars-to-eliminate)
                                    (set! env (update-env env decl expr))
                                    (unless (instance-of? decl <sat-aux-decl>)
                                      (queue/insert! eliminated-declaration-queue decl)
                                      (queue/insert! eliminated-constraint-queue 
                                                     (make-sal-equality (make-sal-name-expr decl expr)
                                                                        expr)))
                                    (push! decl vars-to-remove-from-map)))
                                vars-to-eliminate)
        (for-each (lambda (to-remove)
                    (eq-hash-table/delete! vars-to-eliminate to-remove))
                  vars-to-remove-from-map)
        ;; apply the substitution over the remaining variables
        (eq-hash-table/for-each-entry (lambda (entry)
                                        (let* ((expr (cdr entry))
                                               (new-expr (sal-ast/substitute expr env)))
                                          (unless (eq? expr new-expr)
                                            (set-cdr! entry (sal-ast/simplify new-expr)))))
                                      vars-to-eliminate)
        (unless (null? vars-to-remove-from-map)
          (loop))))
;     (wt-tree/for-each (lambda (key datum)
;                         (sal/pp key)
;                         (print "\n ---> ")
;                         (sal/pp datum)
;                         (print "\n-------------"))
;                       env)
;     (print "=========================")
    env))
    
;; --------------------------------------------------------------------
;; Transformation: Lambda Elimination
;;
;; This transformation is very effective if the user is using SMV-like
;; specifications
;;  
;; For every constraint of the form:
;; x = (LAMBDA ...)
;; I substitute every occurrence of x, by (LAMBDA...), and
;; apply beta reduction.
;;
;; --------------------------------------------------------------------
(define (sat-generic-context/lambda-elimination-register! sat-generic-context vars-to-eliminate)
  (let ((constraint-queue (make-queue))
        (empty-env (make-empty-env)))
    (for-each (lambda (expr)
                (if (and (instance-of? expr <sal-eq>)
                         ;; do not consider equations that are instance of <sal-cyclic-eq>
                         ;; if I remove this condition the program may enter in an infinite
                         ;; loop...
                         (not (instance-of? expr <sal-cyclic-eq>))) 
                  (multiple-value-bind
                      (lhs rhs)
                      (sal-binary-application/arguments expr)
                    (multiple-value-bind
                        (can-lhs can-rhs)
                        (if (instance-of? lhs <sal-name-expr>)
                          (values lhs rhs)
                          (values rhs lhs))
                      (if (and (instance-of? can-lhs <sal-name-expr>)
                               (sal-type/function? (sal-expr/type can-lhs))
                               (not (eq-hash-table/get vars-to-eliminate (slot-value can-lhs :decl)))
                               (sal-ast/contains-lambda? can-rhs)
                               (not (sal-ast/contains-reference? can-rhs (slot-value can-lhs :decl))))
                        (let ((new-rhs (if (instance-of? can-rhs <sal-lambda>) 
                                         can-rhs
                                         (sal-ast/pull-lambdas can-rhs empty-env))))
                          (if (instance-of? new-rhs <sal-lambda>)			   
                            (eq-hash-table/put! vars-to-eliminate (slot-value can-lhs :decl) new-rhs)
                            (queue/insert! constraint-queue expr)))
                        (queue/insert! constraint-queue expr))))
                  (queue/insert! constraint-queue expr)))
              (queue->list (slot-value sat-generic-context :constraint-queue)))
    (set-slot-value! sat-generic-context :constraint-queue constraint-queue)))

;; return a boolean indicating whether a lambda expression was eliminated or not.
(define (sat-generic-context/eliminate-lambdas! sat-generic-context)
  (verbose-message 3 "    eliminating lambda expressions...")
  (display-runtime 4 "      transformation time: ~a secs"
    (lambda ()
      (let ((lambda-eliminated? #f))
        (let loop ((i 1))
          (verbose-message 4 "      iteration ~a..." i)
          (let ((vars-to-eliminate (make-eq-hash-table)))
            (sat-generic-context/lambda-elimination-register! sat-generic-context vars-to-eliminate)
            (cond
             ((> (eq-hash-table/size vars-to-eliminate) 0)
              (sat-generic-context/eliminate-variables! sat-generic-context vars-to-eliminate)
              (set! lambda-eliminated? #t)
              (loop (+ i 1)))
             (else
              lambda-eliminated?))))))))

;; --------------------------------------------------------------------
;; Transformation: Funtion equality extensionality
;;
;; Transforms:
;; "f = g"       when  f (and g) are functions
;; ===>
;; FORALL (x: <DOMAIN>) : f(x) = g(x)
;;
;; 
;; Transforms:
;; "f /= g"
;; ===>
;; EXISTS (x: <DOMAIN) : f(x) /= g(x)
;;
;; Remark: These rules are only applied if:
;;             - the quantifier can be skolemized
;;             - f (and g) has a finite domain
;;             - f or g contain lambda expressions,
;;
;; --------------------------------------------------------------------
(define-generic (sal-ast/ho-eq-expansion ast env polarity))

(define-method (sal-ast/ho-eq-expansion (ast <sal-ast>) (env <primitive>) (polarity <polarity>))
  (sal-ast/map-using-polarity ast env polarity sal-ast/ho-eq-expansion))

(define-inline (sal-eq/ho-eq-expansion-core ast env polarity expand? call-next-method quantifier-class)
  (multiple-value-bind
      (arg1 arg2)
      (sal-binary-application/arguments ast)
    (if (and (sal-type/function? (sal-expr/type arg1))
             (or expand?
                 (sal-ast/contains-lambda? arg1)
                 (sal-ast/contains-lambda? arg2)))
      (let* ((place-provider ast)
             (f (sal-ast/ho-eq-expansion arg1 env polarity))
             (g (sal-ast/ho-eq-expansion arg2 env polarity))
             (f-type (sal-expr/type arg1))
             (domain-types (sal-function-type/domain-types f-type))
             (aux-var-decls (map (lambda (type)
                                   (make-ast-instance <sal-var-decl> place-provider
                                                      :id (make-sal-identifier place-provider (gen-unique-name 'arg))
                                                      :type type))
                                 domain-types))
             (aux-var-names (map make-sal-name-expr aux-var-decls))
             (x (apply make-application-argument aux-var-names))
             (f-x (sal-expr/lift f x env 0))
             (g-x (sal-expr/lift g x env 0))
             (body (copy-ast ast
                             :arg (make-application-argument f-x g-x))))
        (make-ast-instance quantifier-class place-provider
                           :local-decls aux-var-decls
                           :expr body))
      (call-next-method))))

(define-method (sal-ast/ho-eq-expansion (ast <sal-eq>) (env <primitive>) (polarity <polarity>))
  (sal-eq/ho-eq-expansion-core ast env polarity #f call-next-method <sal-for-all-expr>))

(define-method (sal-ast/ho-eq-expansion (ast <sal-eq>) (env <primitive>) (polarity <neg>))
  (sal-eq/ho-eq-expansion-core ast env polarity #t call-next-method <sal-for-all-expr>))

(define-method (sal-ast/ho-eq-expansion (ast <sal-diseq>) (env <primitive>) (polarity <polarity>))
  (sal-eq/ho-eq-expansion-core ast env polarity #f call-next-method <sal-exists-expr>))

(define-method (sal-ast/ho-eq-expansion (ast <sal-diseq>) (env <primitive>) (polarity <pos>))
  (sal-eq/ho-eq-expansion-core ast env polarity #t call-next-method <sal-exists-expr>))

(define (sat-generic-context/ho-eq-expansion! sat-generic-context)
  (when (exists (lambda (decl)
                  (sal-type/function? (slot-value decl :type)))
                (queue->list (slot-value sat-generic-context :declaration-queue)))
    (let ((empty-env (make-empty-env)))
      (sat-generic-context/apply-simple-transformation! 
       sat-generic-context 
       (lambda (expr) (sal-ast/ho-eq-expansion expr empty-env *pos*))
       "extensional equality for functions"))))

;; --------------------------------------------------------------------
;; Transformation: Lambda expansion
;;
;; This transformation expand lamba expressions with finite domains.
;; I transform every (LAMBDA ...) to
;;
;;  new_f
;; 
;;  and add the following declaration and constraints:
;;
;;  new_f : <TYPE OF LAMBDA>
;;  new_f(d1) = (LAMBDA ...)(d1)
;;  ...
;;  new_f(dn) = (LAMBDA ...)(dn)
;;  
;;  new_f is an instance of <sat-lambda-aux-decl>
;;  Remark: new_f cannot be removed using quantifier elimination.
;; --------------------------------------------------------------------
(define-class <sat-lambda-aux-decl> (<sat-aux-decl>) ())

(define-generic (sal-ast/expand-lambdas ast env decl-queue constraint-queue))

(define-method (sal-ast/expand-lambdas (ast <sal-ast>) (env <primitive>) (decl-queue <primitive>) (constraint-queue <primitive>))
  (sal-ast/map ast env (lambda (child-ast new-env) (sal-ast/expand-lambdas child-ast new-env decl-queue constraint-queue))))

(define-method (sal-ast/expand-lambdas (ast <sal-lambda>) (env <primitive>) (decl-queue <primitive>) (constraint-queue <primitive>))
  (try
   (if (sal-type/finite? (sal-function-type/domain (sal-expr/type ast)))
     (let* ((new-ast (call-next-method))
            (place-provider ast)
            (aux-var-type (sal-expr/type new-ast))
            (aux-var-name (gen-unique-name 'f))
            (aux-var-id (make-sal-identifier place-provider aux-var-name))
            (aux-var-decl (make-ast-instance <sat-lambda-aux-decl> place-provider
                                             :id aux-var-id
                                             :type aux-var-type))
            (domain-type (sal-function-type/domain aux-var-type))
            (aux-name-expr (make-sal-name-expr aux-var-decl place-provider))
            (it (sal-type/make-iterator domain-type))
            (domain-values (iterator->list it)))
       (queue/insert! decl-queue aux-var-decl)
       (for-each (lambda (domain-value)
                   (let ((lhs (sal-expr/apply aux-name-expr domain-value))
                         (rhs (sal-ast/simplify (sal-expr/apply new-ast domain-value))))
                     (queue/insert! constraint-queue 
                                    (make-sal-equality lhs rhs))))
                 domain-values)
       aux-name-expr)
     (call-next-method))
   (catch 'type-iterator
          (lambda (msg)
            (sign-source-error ast "Lambda expression cannot be expanded, reason: ~a" msg)))))

(define (sat-generic-context/lambda-expansion! sat-generic-context)
  (when (sat-generic-context/contains-lambda? sat-generic-context)
    (let ((empty-env (make-empty-env)))
      (sat-generic-context/apply-transformation! sat-generic-context 
                                         (lambda (expr decl-queue constraint-queue) 
                                           (sal-ast/expand-lambdas expr empty-env decl-queue constraint-queue))
                                         "expanding lambda expressions"))))

;; --------------------------------------------------------------------
;; Transformation: Quantifier elimination
;;
;; I use two (simple) techniques to eliminate quantifiers: skolemization & unfolding
;;
;; --------------------------------------------------------------------
(define-class <sat-skolem-aux-decl> (<sat-aux-decl>) ())

(define-generic (sal-ast/eliminate-quantifiers ast env polarity new-decl-queue))

(define-method (sal-ast/eliminate-quantifiers (ast <sal-ast>) (env <primitive>) (polarity <polarity>) (new-decl-queue <primitive>))
  (sal-ast/map-using-polarity ast env polarity (lambda (child-ast new-env new-polarity)
                                                 (sal-ast/eliminate-quantifiers child-ast new-env new-polarity new-decl-queue))))

(define-method (sal-ast/eliminate-quantifiers (ast <sal-lambda>) (env <primitive>) (polarity <polarity>) (new-decl-queue <primitive>))
  ;; do not perform quantifier elimination inside of a lambda expression
  (sal-ast/substitute ast env))

(define (skolemize ast env polarity new-decl-queue)
  (sat-generic-context/skolemize ast env
                                 (lambda (expr new-env new-local-decls)
                                   (queue/append! new-decl-queue new-local-decls)
                                   (sal-ast/eliminate-quantifiers expr new-env polarity new-decl-queue))))

(define-method (sal-ast/eliminate-quantifiers (ast <sal-quantified-expr>) (env <primitive>) (polarity <polarity>) (new-decl-queue <primitive>))
  ;; (breakpoint "quantifiers..." (ast env polarity new-decl-queue) #t)
  (sal-quantified-expr/expand-core ast env (lambda (body new-env)
                                             (sal-ast/eliminate-quantifiers body new-env polarity new-decl-queue))))

(define-method (sal-ast/eliminate-quantifiers (ast <sal-exists-expr>) (env <primitive>) (polarity <pos>) (new-decl-queue <primitive>))
  ;; (breakpoint "skolemize" (ast env polarity new-decl-queue) #t)
  (skolemize ast env polarity new-decl-queue))

(define-method (sal-ast/eliminate-quantifiers (ast <sal-for-all-expr>) (env <primitive>) (polarity <neg>) (new-decl-queue <primitive>))
  ;; (breakpoint "skolemize" (ast env polarity new-decl-queue) #t)
  (skolemize ast env polarity new-decl-queue))

(define (sat-generic-context/eliminate-quantifiers! sat-generic-context)
  (let ((empty-env (make-empty-env))
        (polarity *pos*))
    (sat-generic-context/apply-transformation! sat-generic-context 
                                       (lambda (expr decl-queue constraint-queue) 
                                         (sal-ast/eliminate-quantifiers expr empty-env polarity decl-queue))
                                       "eliminating quantifiers")))
  
;; --------------------------------------------------------------------
;; Transformation: simple common subexpression elimination
;;
;; This transformation is much simpler (and more efficient) thant the one
;; implemented in "sal-cse.scm".
;;
;; Remark: this transformation can only be applied to contexts which
;;         do not contain let,lambda, and quantifiers.
;; --------------------------------------------------------------------
(define-class <sat-alias-aux-decl> (<sat-aux-decl>) ())

(define (sat-generic-context/compute-cse! sat-generic-context)
  (let ((occ-table (make-sal-ast-table)))
    (for-each (lambda (expr)
                (sal-cse/compute-occurrences expr occ-table))
              (queue->list (slot-value sat-generic-context :constraint-queue)))
    (let ((cse-table (make-sal-ast-table)))
      (for-each (lambda (expr)
                  (sal-cse/filter-occurrences expr occ-table cse-table))
                (queue->list (slot-value sat-generic-context :constraint-queue)))
      cse-table)))

(define (eliminate-cse ast cse-table)
  (let ((empty-env (make-empty-env)))
    (let loop ((ast ast))
      (cond
       ((sal-ast-table/get cse-table ast) =>
        (lambda (entry)
          (make-sal-name-expr (cdr entry) ast)))
       ((instance-of? ast <sal-local-binds-expr>)
        ;; ast cannot contain local binds...
        (internal-error))
       (else
        (sal-ast/map ast empty-env (lambda (child-ast new-env)
                                     [assert (new-env) (eq? new-env empty-env)]
                                     (loop child-ast))))))))

(define (sat-generic-context/cse! sat-generic-context)
  (verbose-message 3 "    eliminating common subexpressions...")
  (display-runtime 4 "      transformation time: ~a secs"
    (lambda ()
      (let loop ((it 0))
        (when (< it *sal-cse-maximum-number-of-iterations*)
          (let ((cse-table (sat-generic-context/compute-cse! sat-generic-context)))
            (when (> (sal-ast-table/size cse-table) 0)
              (verbose-message 4 "      starting a new iteration of the common subexpression elimination procedure...")
              (let ((decl-table (slot-value sat-generic-context :declaration-queue))
                    (new-constraint-queue (make-queue)))
                (eq-hash-table/for-each-entry (lambda (entry)
                                                (let* ((decl (cdr entry))
                                                       (alias-decl (make-ast-instance <sat-alias-aux-decl> decl
                                                                                      :id (slot-value decl :id)
                                                                                      ;; I must use the supertype...
                                                                                      ;; check the comment in ite->ite-bool
                                                                                      :type (sat-context/super-type (slot-value decl :type))))
                                                       (alias-name-expr (make-sal-name-expr alias-decl (car entry)))
                                                       (new-constraint (make-sal-equality alias-name-expr (slot-value decl :value))))
                                                  (set-cdr! entry alias-decl)
                                                  (queue/insert! decl-table alias-decl)
                                                  (queue/insert! new-constraint-queue new-constraint)))
                                              cse-table)
                (for-each (lambda (expr)
                            (queue/insert! new-constraint-queue (eliminate-cse expr cse-table)))
                          (queue->list (slot-value sat-generic-context :constraint-queue)))
                (set-slot-value! sat-generic-context :constraint-queue new-constraint-queue)
                (loop (+ it 1))))))))))

;; --------------------------------------------------------------------
;; ITE -> BOOL ITE transformation
;;
;; <sal-ite-aux-decl> represents auxiliary declarations created by
;; the method sat-generic-context/ite->bool-ite. 
;;
;; This transformation converts every <sal-conditional>
;;  
;;   (IF C1 THEN T1 ELSE T2 ENDIF)
;;
;;   to 
;;
;;   new_aux
;;
;;   when T1 (and T2) are not booleans, 
;;   It also adds the following declaration and constraint:
;;
;;   new_aux : <SUPERTYPE OF T1>
;;   (IF C1 THEN new_aux = T1 ELSE new_aux = T2 ENDIF)
;;
;;  Remark: new_aux is an instance of <sal-ite-aux-decl>
;;
;;  Remark: this transformation can only be applied to contexts which
;;    do not contain let,lambda, and quantifiers.
;; --------------------------------------------------------------------
(define-class <sat-ite-aux-decl> (<sat-aux-decl>) ())

(define-generic (sal-ast/ite->bool-ite ast env decl-queue constraint-queue))

(define-method (sal-ast/ite->bool-ite (ast <sal-local-binds-expr>) (env <primitive>) (decl-queue <primitive>) (constraint-queue <primitive>))
  (internal-error))

(define-method (sal-ast/ite->bool-ite (ast <sal-ast>) (env <primitive>) (decl-queue <primitive>) (constraint-queue <primitive>))
  (sal-ast/map ast env (lambda (child-ast new-env) (sal-ast/ite->bool-ite child-ast new-env decl-queue constraint-queue))))

(define-method (sal-ast/ite->bool-ite (ast <sal-conditional>) (env <primitive>) (decl-queue <primitive>) (constraint-queue <primitive>))
  (if (sal-type/boolean? (sal-expr/type (slot-value ast :then-expr)))
    (call-next-method)
    (let* ((place-provider ast)
           (new-cond-expr (sal-ast/ite->bool-ite (slot-value ast :cond-expr) env decl-queue constraint-queue))
           (new-then-expr (sal-ast/ite->bool-ite (slot-value ast :then-expr) env decl-queue constraint-queue))
           (new-else-expr (sal-ast/ite->bool-ite (slot-value ast :else-expr) env decl-queue constraint-queue))
           (aux-var-type (sat-context/super-type (sal-expr/type new-then-expr)))
           (aux-var-name (gen-unique-name 'ite_aux))
           (aux-var-id (make-sal-identifier place-provider aux-var-name))
           (aux-var-decl (make-ast-instance <sat-ite-aux-decl> place-provider
                                            :id aux-var-id
                                            ;; I have to use the supertype
                                            ;; check the comment in ite->ite-bool
                                            :type aux-var-type))
           (aux-var-name (make-sal-name-expr aux-var-decl place-provider))
           (then-eq (make-sal-equality aux-var-name new-then-expr))
           (else-eq (make-sal-equality aux-var-name new-else-expr))
           (new-ite (copy-ast ast
                              :cond-expr new-cond-expr
                              :then-expr then-eq
                              :else-expr else-eq)))
      (queue/insert! decl-queue aux-var-decl)
      (queue/insert! constraint-queue new-ite)
      aux-var-name)))

(define (sat-generic-context/ite->bool-ite! sat-generic-context)
  (let ((empty-env (make-empty-env)))
    (sat-generic-context/apply-transformation! sat-generic-context 
                                       (lambda (expr decl-queue constraint-queue) 
                                         (sal-ast/ite->bool-ite expr empty-env decl-queue constraint-queue))
                                       "converting conditional expressions")))

;; --------------------------------------------------------------------
;; DIV & MODULO elimination
;;
;; Transforms
;;  x DIV n
;;  to 
;;  IF x_alias < 0 THEN -aux1 ELSE aux1 ENDIF    (when n > 0)
;;  IF x_alias < 0 THEN aux1 ELSE -aux1 ENDIF    (when n < 0)
;; 
;;  Add the following definitions & constrains when n>0
;;
;;  x_alias : INTEGER
;;  aux1: INTEGER
;;  aux2: [0..|n|-1]
;;  aux3: INTEGER
;;  aux3 = aux1 * |n| + aux2
;;  x_alias = x
;;  IF x_alias < 0 THEN
;;    x_alias = -aux3
;;  ELSE
;;    x_alias = aux3
;;  ENDIF
;;
;;
;; Transforms
;;  x MOD n
;;  to
;;  aux2      when n > 0
;;  -aux2     when n < 0
;;
;;  Add the following definitions & constrains when n>0
;;
;;  aux1: INTEGER
;;  aux2: [0..|n|-1]
;;  aux3: INTEGER
;;  x_alias : INTEGER
;;  aux3 = aux1 * |n| + aux2
;;  x_alias = x
;;  IF x_alias < 0 THEN
;;    x_alias = -aux3
;;  ELSE
;;    x_alias = aux3
;;  ENDIF
;; 
;; --------------------------------------------------------------------
(define-generic (sal-ast/eliminate-div-mod ast env decl-queue constraint-queue))

(define-method (sal-ast/eliminate-div-mod (ast <sal-local-binds-expr>) (env <primitive>) (decl-queue <primitive>) (constraint-queue <primitive>))
  (internal-error))

(define-method (sal-ast/eliminate-div-mod (ast <sal-ast>) (env <primitive>) (decl-queue <primitive>) (constraint-queue <primitive>))
  (sal-ast/map ast env (lambda (child-ast new-env) (sal-ast/eliminate-div-mod child-ast new-env decl-queue constraint-queue))))

(define (gen-div-mod-aux-constraints x n decl-queue constraint-queue place-provider)
  (let* ((int-type (make-sal-builtin-name <sal-int-type> place-provider))
         (zero (make-sal-numeral 0 place-provider))
         (n-val (slot-value n :num))
         (abs-n-val (mpq/absolute n-val))
         (abs-n (make-sal-numeral abs-n-val place-provider))
         (abs-n-1 (make-sal-numeral (-mpq abs-n-val *mpq-one*) place-provider))
         (subrange (make-sal-subrange zero abs-n-1 place-provider))
         (x-alias (make-sal-identifier place-provider (gen-unique-name 'x_alias)))
         (aux1 (make-sal-identifier place-provider (gen-unique-name 'aux1)))
         (aux2 (make-sal-identifier place-provider (gen-unique-name 'aux2)))
         (aux3 (make-sal-identifier place-provider (gen-unique-name 'aux3)))
         (x-alias-decl (make-ast-instance <sat-aux-decl> place-provider :id x-alias :type int-type))
         (aux1-decl (make-ast-instance <sat-aux-decl> place-provider :id aux1 :type int-type))
         (aux2-decl (make-ast-instance <sat-aux-decl> place-provider :id aux2 :type subrange))
         (aux3-decl (make-ast-instance <sat-aux-decl> place-provider :id aux3 :type int-type))
         (x-alias-name-expr (make-sal-name-expr x-alias-decl))
         (aux1-name-expr (make-sal-name-expr aux1-decl))
         (aux2-name-expr (make-sal-name-expr aux2-decl))
         (aux3-name-expr (make-sal-name-expr aux3-decl))
         ;; x = x_alias
         (x-eq-x-alias (make-sal-equality x x-alias-name-expr))
         ;; x_alias < 0
         (x-alias-lt-zero (make-sal-builtin-application <sal-lt> place-provider x-alias-name-expr zero))
         ;; aux3 = aux1 * |n| + aux2
         (aux-eq (make-sal-equality aux3-name-expr
                                    (make-sal-builtin-application <sal-add> place-provider
                                                                  (make-sal-builtin-application <sal-mul> place-provider
                                                                                                aux1-name-expr
                                                                                                abs-n)
                                                                  aux2-name-expr)))
         (aux-cond (make-ast-instance <sal-conditional> place-provider
                                      :cond-expr x-alias-lt-zero
                                      :then-expr (make-sal-equality x-alias-name-expr 
                                                                    (make-sal-builtin-application <sal-sub> place-provider
                                                                                                  zero
                                                                                                  aux3-name-expr))
                                      :else-expr (make-sal-equality x-alias-name-expr aux3-name-expr))))
    (queue/insert! decl-queue x-alias-decl)
    (queue/insert! decl-queue aux1-decl)
    (queue/insert! decl-queue aux2-decl)
    (queue/insert! decl-queue aux3-decl)
    (queue/insert! constraint-queue x-eq-x-alias)
    (queue/insert! constraint-queue aux-eq)
    (queue/insert! constraint-queue aux-cond)
    (values x-alias-lt-zero aux1-name-expr aux2-name-expr)))
  

(define-method (sal-ast/eliminate-div-mod (ast <sal-idiv>) (env <primitive>) (decl-queue <primitive>) (constraint-queue <primitive>))
  (multiple-value-bind
      (x n)
      (sal-binary-application/arguments ast)
    (unless (instance-of? n <sal-numeral>)
      (sign-unsupported-feature ast "Only integer division by constants is supported by this solver."))
    (let ((x (sal-ast/eliminate-div-mod x env decl-queue constraint-queue))
          (place-provider ast))
      (multiple-value-bind
          (x-alias-lt-zero aux1-name-expr aux2-name-expr)
          (gen-div-mod-aux-constraints x n decl-queue constraint-queue place-provider)
        (let* ((zero (make-sal-numeral 0 place-provider))
               (neg-aux1-name-expr (make-sal-builtin-application <sal-sub> place-provider 
                                                                 zero 
                                                                 aux1-name-expr)))
          (multiple-value-bind
              (then-expr else-expr)
              (if (<mpq (slot-value n :num) *mpq-zero*)
                (values aux1-name-expr neg-aux1-name-expr)
                (values neg-aux1-name-expr aux1-name-expr))
            (make-ast-instance <sal-conditional> place-provider
                               :cond-expr x-alias-lt-zero
                               :then-expr then-expr
                               :else-expr else-expr)))))))

(define-method (sal-ast/eliminate-div-mod (ast <sal-mod>) (env <primitive>) (decl-queue <primitive>) (constraint-queue <primitive>))
  (multiple-value-bind
      (x n)
      (sal-binary-application/arguments ast)
    (unless (instance-of? n <sal-numeral>)
      (sign-unsupported-feature ast "Only modulo by constants is supported by this solver."))
    (let ((x (sal-ast/eliminate-div-mod x env decl-queue constraint-queue))
          (place-provider ast))
      (multiple-value-bind
          (x-alias-lt-zero aux1-name-expr aux2-name-expr)
          (gen-div-mod-aux-constraints x n decl-queue constraint-queue place-provider)
        (if (<mpq (slot-value n :num) *mpq-zero*)
          (make-sal-builtin-application <sal-sub> place-provider
                                        (make-sal-numeral 0 place-provider)
                                        aux2-name-expr)
          aux2-name-expr)))))
              
(define (sat-generic-context/eliminate-div-mod! sat-generic-context)
  (when (or (sat-generic-context/contains-idiv? sat-generic-context)
            (sat-generic-context/contains-mod? sat-generic-context))
    (let ((empty-env (make-empty-env)))
      (sat-generic-context/apply-transformation! sat-generic-context 
                                                 (lambda (expr decl-queue constraint-queue) 
                                                   (sal-ast/eliminate-div-mod expr empty-env decl-queue constraint-queue))
                                                 "eliminating DIV and MOD"))))

;; --------------------------------------------------------------------
;; Integer disequality elimination
;;
;; converts 
;; e1 /= e2
;; into
;; (e1 - e2 + 1 <= 0) OR (e1 - e2 - 1 >= 0)
;;
;; If e1 - e2 is big, then I create an auxiliary varible
;; aux = e1 - e2
;;
;; and the result is (aux + 1 <= 0) OR (aux - 1 >= 0)
;;
;; That is not the whole story. I have to apply this transformation
;; tracking the polarity of the expressions
;;
;; --------------------------------------------------------------------

(define-generic (sal-ast/eliminate-int-diseq ast env pol decl-queue constraint-queue))

(define-method (sal-ast/eliminate-int-diseq (ast <sal-local-binds-expr>) (env <primitive>) (pol <polarity>) 
                                            (decl-queue <primitive>) (constraint-queue <primitive>))
  (internal-error))

(define-method (sal-ast/eliminate-int-diseq (ast <sal-ast>) (env <primitive>) (pol <polarity>) (decl-queue <primitive>) (constraint-queue <primitive>))
  (sal-ast/map-using-polarity 
   ast env pol 
   (lambda (child-ast new-env new-pol) (sal-ast/eliminate-int-diseq child-ast new-env new-pol decl-queue constraint-queue))))

;; creates
;; (x <= -1) OR (x >= 1)
(define (mk-diseq-zero x)
  (let* ((place-provider x)
         (minus-one (make-sal-numeral -1 place-provider))
         (one (make-sal-numeral 1 place-provider)))
    (make-sal-builtin-application <sal-or> place-provider
                                  (make-sal-builtin-application <sal-le> place-provider
                                                                x minus-one)
                                  (make-sal-builtin-application <sal-ge> place-provider
                                                                x one))))

;; When the number of nodes in e1-e2 is greater than the threshold,
;; an alias is created.
(define *aliasing-threshold* 50)

(define (mk-e1-minus-e2 e1 e2 env pol decl-queue constraint-queue place-provider)
  (let* ((e1 (sal-ast/eliminate-int-diseq e1 env pol decl-queue constraint-queue))
         (e2 (sal-ast/eliminate-int-diseq e2 env pol decl-queue constraint-queue))
         (e1-minus-e2 (make-sal-builtin-application <sal-sub> place-provider
                                                    e1 e2))
         (size (sal-ast/size e1-minus-e2)))
    (if (< size *aliasing-threshold*)
      e1-minus-e2
      (let* ((aux-id (make-sal-identifier place-provider (gen-unique-name 'aux)))
             (int-type (make-sal-builtin-name <sal-int-type> place-provider))
             (aux-decl (make-ast-instance <sat-aux-decl> place-provider :id aux-id :type int-type))
             (aux-name-expr (make-sal-name-expr aux-decl))
             (aux-eq (make-sal-equality aux-name-expr e1-minus-e2)))
        (queue/insert! decl-queue aux-decl)
        (queue/insert! constraint-queue aux-eq)
        aux-name-expr))))

;; creates
;; (e1 - e2 + 1 <= 0) OR (e1 - e2 - 1 >= 0)
(define (expand-diseq e1 e2 env pol decl-queue constraint-queue place-provider)
  (mk-diseq-zero (mk-e1-minus-e2 e1 e2 env pol decl-queue constraint-queue place-provider)))

;; creates a new boolean variable "c"
;; adds the global constraint
;; IF c THEN e1 - e2 = 0 ELSE (e1 - e2 + 1 <= 0) OR (e1 - e2 - 1 >= 0) ENDIF
;; and returns "c"
(define (expand-eq-diseq e1 e2 env pol decl-queue constraint-queue place-provider)
  (let* ((e1-minus-e2 (mk-e1-minus-e2 e1 e2 env pol decl-queue constraint-queue place-provider))
         (c-id (make-sal-identifier place-provider (gen-unique-name 'aux)))
         (bool-type (make-sal-builtin-name <sal-bool-type> place-provider))
         (c-decl (make-ast-instance <sat-aux-decl> place-provider :id c-id :type bool-type))
         (c-name-expr (make-sal-name-expr c-decl))
         (zero (make-sal-numeral 0 place-provider))
         (aux-cond (make-ast-instance <sal-conditional> place-provider
                                      :cond-expr c-name-expr
                                      :then-expr (make-sal-equality e1-minus-e2 zero)
                                      :else-expr (mk-diseq-zero e1-minus-e2))))
    (queue/insert! decl-queue c-decl)
    (queue/insert! constraint-queue aux-cond)
    c-name-expr))

(define (integer-equality? ast)
  (and (sal-type/integer? (sal-expr/type (sal-binary-application/arg1 ast)))
       (sal-type/integer? (sal-expr/type (sal-binary-application/arg2 ast)))))

(define-method (sal-ast/eliminate-int-diseq (ast <sal-diseq>) (env <primitive>) (pol <pos>) (decl-queue <primitive>) (constraint-queue <primitive>))
  (if (integer-equality? ast)
    (expand-diseq (sal-binary-application/arg1 ast) (sal-binary-application/arg2 ast) env pol decl-queue constraint-queue ast)
    (call-next-method)))

(define-method (sal-ast/eliminate-int-diseq (ast <sal-diseq>) (env <primitive>) (pol <pos-neg>) (decl-queue <primitive>) (constraint-queue <primitive>))
  (if (integer-equality? ast)
    (make-sal-not (expand-eq-diseq (sal-binary-application/arg1 ast) (sal-binary-application/arg2 ast) env pol decl-queue constraint-queue ast))
    (call-next-method)))

(define-method (sal-ast/eliminate-int-diseq (ast <sal-eq>) (env <primitive>) (pol <neg>) (decl-queue <primitive>) (constraint-queue <primitive>))
  (if (integer-equality? ast)
    (make-sal-not+ (expand-diseq (sal-binary-application/arg1 ast) (sal-binary-application/arg2 ast) env pol decl-queue constraint-queue ast))
    (call-next-method)))

(define-method (sal-ast/eliminate-int-diseq (ast <sal-eq>) (env <primitive>) (pol <pos-neg>) (decl-queue <primitive>) (constraint-queue <primitive>))
  (if (integer-equality? ast)
    (expand-eq-diseq (sal-binary-application/arg1 ast) (sal-binary-application/arg2 ast) env pol decl-queue constraint-queue ast)
    (call-next-method)))

(define (sat-generic-context/eliminate-int-diseq! sat-generic-context)
  (let ((empty-env (make-empty-env)))
    (sat-generic-context/apply-transformation! sat-generic-context 
                                               (lambda (expr decl-queue constraint-queue) 
                                                 (sal-ast/eliminate-int-diseq expr empty-env *pos* decl-queue constraint-queue))
                                               "eliminating integer disequalities")))

;; ********* REMARK ***********
;; 
;; After converting ITE to ITE-BOOL, and eliminating lambdas,
;; for every equality (and disequality) T1 = T2 where the types
;; of T1 and T2 are function types, we have T1 = f and T2 = g, 
;; where f and g are uninterpreted constants.
;;
;; If T1 and T2 are array types, then T1 and T2 can also be
;; array-updates.

;; --------------------------------------------------------------------
;; The following function checks if a node contains an equality
;; over "higher order" terms (functions)
;;
;; --------------------------------------------------------------------
(define (sal-ast/contains-ho-eq? ast)
  (sal-ast/find (lambda (n)
                  (and (or (instance-of? ast <sal-eq>) 
                           (instance-of? ast <sal-diseq>))
                       (sal-type/function? (sal-expr/type (sal-binary-application/arg1 ast)))))
                ast))

;; -------------------------------------------------------------------
;; Transformation: Polarity breaker for ho-eq (higher order equalities)
;; 
;; A higher-order equality formula cannot have both polarities (pos/neg),
;; because the next transformation assumes that each higher-order
;; equality contains just one polarity. So, I convert
;;
;;  (iff a b)   --->  (and (or (not a) b)  (or (not b) a))
;;  (xor a b)   --->  (and (or a b) (or (not a) (not b)))
;;  (ite a b c)  ---> (or (and a b) (and (not a c)))
;;
;;  when they contain higher-order equalities.
;;
;; Remark: this transformatio assumes that there aren't <sal-local-binds-expr>
;; in the constraints. So, I don't need a sal-ast-env.
;; -------------------------------------------------------------------
(define-generic (sal-ast/polarity-breaker-for-ho-eq ast))
(define *empty-env* (make-empty-env))
(define-method (sal-ast/polarity-breaker-for-ho-eq (ast <sal-ast>))
  (sal-ast/map ast *empty-env* (lambda (child-ast new-env)
                                 [assert (new-env) (eq? new-env *empty-env*)]
                                 (sal-ast/polarity-breaker-for-ho-eq ast))))
(define-method (sal-ast/polarity-breaker-for-ho-eq (ast <sal-local-binds-expr>))
  [assert (ast) #f]
  (internal-error))
(define (sal-eq/polarity-breaker-core ast call-next-method result-proc)
  (if (sal-type/boolean? (sal-expr/type (sal-binary-application/arg1 ast)))
    (multiple-value-bind 
        (arg1 arg2)
        (sal-binary-application/arguments ast)
        (if (or (sal-ast/contains-ho-eq? arg1) 
                (sal-ast/contains-ho-eq? arg2))
          (let ((new-arg1 (sal-ast/polarity-breaker-for-ho-eq arg1))
                (new-arg2 (sal-ast/polarity-breaker-for-ho-eq arg2)))
            (result-proc new-arg1 new-arg2))
          (call-next-method)))
    (call-next-method)))
(define-method (sal-ast/polarity-breaker-for-ho-eq (ast <sal-eq>))
  (sal-eq/polarity-breaker-core 
   ast 
   call-next-method
   (lambda (new-arg1 new-arg2)
     (make-sal-and+ (make-sal-or+ (make-sal-not+ new-arg1) new-arg2)
                    (make-sal-or+ (make-sal-not+ new-arg2) new-arg1)))))
(define-method (sal-ast/polarity-breaker-for-ho-eq (ast <sal-diseq>))
  (sal-eq/polarity-breaker-core
   ast
   call-next-method
   (lambda (new-arg1 new-arg2)
     (make-sal-and+ (make-sal-or+ new-arg1 new-arg2)
                    (make-sal-or+ (make-sal-not+ new-arg1) (make-sal-not+ new-arg2))))))
(define-method (sal-ast/polarity-breaker-for-ho-eq (ast <sal-conditional>))
  (if (sal-ast/contains-ho-eq? (slot-value ast :cond-expr))
    (let ((cond-expr (sal-ast/polarity-breaker-for-ho-eq (slot-value ast :cond-expr)))
          (then-expr (sal-ast/polarity-breaker-for-ho-eq (slot-value ast :then-expr)))
          (else-expr (sal-ast/polarity-breaker-for-ho-eq (slot-value ast :else-expr))))
      (make-sal-or+ (make-sal-and+ cond-expr then-expr)
                    (make-sal-and+ (make-sal-not+ cond-expr) else-expr)))
    (call-next-method)))

(define (sat-generic-context/break-polarities-for-ho-eq! sat-generic-context) 
  (sat-generic-context/apply-simple-transformation! sat-generic-context sal-ast/polarity-breaker-for-ho-eq
                                            "preparing to process equality over functions"))

;; --------------------------------------------------------------------
;; Transformation: "Higher order" equality (and disequality) 
;;
;; For every term f = g, where f and g are uninterpreted functions,
;; the following transformation is applied
;;
;; - Domain is finite {d1, ..., dn}
;;   the equality is mapped to:
;;
;;   f(d1) = g(d1) AND ... AND f(dn) = g(dn)
;;
;; - Domain is infinite
;;   preserve the equality... (incompleteness is not introduced)
;;
;; For disequalities
;;
;; I map f /= g to
;;
;;  f(new_aux) /= g(new_aux)
;;
;; I should add the new declaration
;;
;; new_aux : <DOMAIN OF f>
;;
;; Remark: domain f = domain g
;;
;; Remark: I must add the type constraints of new_aux.
;;
;; Remark: I have to check the polarities of each = and /=
;;
;; Remark: new_aux is an instance of <sat-ho-eq-aux-decl>
;;
;; new_aux CANNOT be eliminated (from counterexamples) using the substitution rule.
;;
;; --------------------------------------------------------------------
(define-class <sat-ho-eq-aux-decl> (<sat-aux-decl>) ())


;; --------------------------------------------------------------------
;; Transformation: eliminate subtype of functions...
;; I do not support subtype of functions, so I replace the subtype with
;; the supertype, and I also print a warning message...
;; 
;; --------------------------------------------------------------------
(define-generic (sal-type/eliminate-function-subtypes type env))
(define-method (sal-type/eliminate-function-subtypes (type <sal-ast>) (env <primitive>)) 
  type)
(define-method (sal-type/eliminate-function-subtypes (type <sal-type>) (env <primitive>))
  (sal-ast/map type env sal-type/eliminate-function-subtypes))
(define-method (sal-type/eliminate-function-subtypes (type <sal-type-name>) (env <primitive>))
  (cond
   ((sal-type-name/definition type) =>
    (lambda (type-def)
      (sal-type/eliminate-function-subtypes type-def env)))
   (else
    type)))
(define-method (sal-type/eliminate-function-subtypes (type <sal-subtype>) (env <primitive>))
  #unspecified)

;; --------------------------------------------------------------------
;; Transformation: adding type constraints 
;;
;; This transformation is required for completeness, but I provide a flag
;; to disable it.
;;
;; <sal-type-constraint-aux-decl> represents auxiliary declarations used to encode
;; type information. For instance, assume the following uninterpreted
;; function symbol:
;; 
;;   f : [INTEGER -> INTEGER];
;;
;; For every application of f(X) of f, the following declaration and constraint must be
;; included in the <sat-generic-context> if you are planning to use ICS as the backend solver:
;;  
;;   new_aux : INTEGER;
;;   f(X) = new_aux;
;;
;; Remark: the type of f becomes 
;;   f : [ INTEGER -> NUMBER ];
;; Obs.: the type of the argument is irrelevant.
;;
;; Instances of <sal-int-aux-decl> represent these auxiliary declarations.
;;
;; Consider now the following uinterpreted function symbol:
;;
;; f : [ NATURAL -> {x : NATURAL | x /= 5 AND x /= 10 } ]
;;
;; For every application f(X) of f, the following declaration and constraints must be
;; included in the <sat-generic-context>
;;
;; new_aux : INTEGER;         <==== must be integer, since I want to use the ICS command "sig new_aux : int". 
;; new_aux >= 0;
;  new_aux /= 5;
;; new_aux /= 10;
;; f(X) = new_aux;
;;
;; f(X)(Y) < 0
;;
;; where
;; f : [ NATURAL -> [ NATURAL -> NATURAL] ]
;;
;; First step:
;;
;; new_aux1 : [ NATURAL -> NATURAL ]
;; new_aux1 = f(X)
;; new_aux1(Y) < 0
;;
;; Applying the transformation again
;;
;; new_aux2 : INTEGER      
;; new_aux2 >= 0
;; new_aux2 = new_aux1(Y)
;; new_aux2 < 0            <===== inconsistency will be detected
;;
;; ***** IMPORTANT *****
;; I'm adding new constraints some of them may contain LAMBDAs OR ITEs
;; If this is the case I should reapply all transformations again.
;;  
;; --------------------------------------------------------------------
(define-class <sat-type-constraint-aux-decl> (<sat-aux-decl>) ())

;; display warning messages for non-supported types...
(define (sal-ast/display-non-supported-type-warnings ast)
  (sal-ast/for-each (lambda (ast)
                      (when (and (instance-of? ast <sal-subtype>)
                                 (sal-type/function? ast))
                        (warning-message "~a Subtypes of functions(arrays) are not supported in the connection between SAL and the SAT engines. The supertype will be used as an approximation." (format-with-location ast ""))))
                    ast))

(define (sat-generic-context/display-non-supported-type-warnings sat-generic-context)
  (for-each (lambda (decl)
              (sal-ast/display-non-supported-type-warnings (slot-value decl :type)))
            (queue->list (slot-value sat-generic-context :declaration-queue))))

(define-generic (sal-type/no-constraints? type))
(define-method (sal-type/no-constraints? (type <sal-type>))
  #t) ;; default behavior: do not have constraints...
(define-method (sal-type/no-constraints? (type <sal-subtype>))
  (if (sal-type/function? type)
    (sal-type/no-constraints? (sal-type/cast type <sal-function-type>))
    #f))
(define-method (sal-type/no-constraints? (type <sal-function-type>))
  (sal-type/no-constraints? (slot-value type :range)))
(define-method (sal-type/no-constraints? (type <sal-type-name>))
  (cond
   ((sal-type-name/definition type) =>
    sal-type/no-constraints?)
   (else
    #t))) ;; uninterpreted type does not have constraints...

(define (sal-decl/add-constraint! decl constraint-queue)
  (let ((type (slot-value decl :type)))
    (unless (or (sal-type/function? type)
                (sal-type/no-constraints? type))
      (let* ((name-expr (make-sal-name-expr decl))
             (new-constraint (sal-ast/simplify (sal-type/membership-expr type name-expr))))
        (constraint-queue/insert! constraint-queue new-constraint)))))

(define-generic (sal-ast/add-type-constraints ast env decl-queue constraint-queue already-processed))
(define-method (sal-ast/add-type-constraints (ast <sal-local-binds-expr>) (env <primitive>) (decl-queue <primitive>) (constraint-queue <primitive>)
                                             (already-processed <primitive>))
  ;; do not process lambdas,quantifiers, and lets...
  (sal-ast/substitute ast env))
(define-method (sal-ast/add-type-constraints (ast <sal-ast>) (env <primitive>) (decl-queue <primitive>) (constraint-queue <primitive>)
                                             (already-processed <primitive>))
  (sal-ast/map ast env (lambda (child-ast new-env)
                         (sal-ast/add-type-constraints child-ast new-env decl-queue constraint-queue already-processed))))
(define-method (sal-ast/add-type-constraints (ast <sal-application>) (env <primitive>) (decl-queue <primitive>) (constraint-queue <primitive>)
                                             (already-processed <primitive>))
  (cond
   ((eq-hash-table/contains? already-processed ast)
    ast)
   (else
    ;; (breakpoint "typepred" (ast) (instance-of? ast <sal-mod>))
    (let* ((fun (sal-ast/add-type-constraints (slot-value ast :fun) env decl-queue constraint-queue already-processed))
           (arg (sal-ast/add-type-constraints (slot-value ast :arg) env decl-queue constraint-queue already-processed))
           (new-app (copy-ast ast :fun fun :arg arg)))
      (eq-hash-table/put! already-processed new-app #t)
      (if (instance-of? fun <sal-name-expr>)
        (let* ((decl (slot-value fun :decl))
               (type (slot-value decl :type))
               (range (sal-function-type/range type)))
          (if (sal-type/no-constraints? range)
            new-app
            (let* ((place-provider ast)
                   (new-name (gen-unique-name 'app-aux))
                   (new-id (make-sal-identifier place-provider new-name))
                   (new-aux-decl (make-ast-instance <sat-type-constraint-aux-decl> place-provider
                                                    :id new-id
                                                    :type range))
                   (new-aux-name-expr (make-sal-name-expr new-aux-decl place-provider))
                   (new-constraint (make-sal-equality new-aux-name-expr new-app)))
              (queue/insert! decl-queue new-aux-decl)
              (queue/insert! constraint-queue new-constraint)
              new-aux-name-expr)))
        new-app)))))

(define (lift-decls! curr-decls new-declarations)
  ;; convert types in curr-decls to super-types
  (for-each (lambda (curr-decl)
              (let* ((type (slot-value curr-decl :type))
                     (place-provider type)
                     (new-type (sat-context/super-type type)))
                (set-slot-value! curr-decl :type new-type)
                (queue/insert! new-declarations curr-decl)))
            curr-decls))
  
(define (sat-generic-context/add-type-constraints! sat-generic-context)
  (verbose-message 3 "    including type constraints...")
  (display-runtime 4 "      transformation time: ~a secs"
    (lambda ()
      (sat-generic-context/display-non-supported-type-warnings sat-generic-context)
      (let ((new-declarations (make-queue))
            (empty-env (make-empty-env)))
        (let loop ((curr-decls (queue->list (slot-value sat-generic-context :declaration-queue))))
          (unless (null? curr-decls)
            (let ((new-constraint-queue (make-queue))
                  (new-decls (make-queue))
                  (already-processed (slot-value sat-generic-context :already-processed)))
              ;; add atomic constraints...
              (for-each (lambda (decl)
                          (sal-decl/add-constraint! decl new-constraint-queue))
                        curr-decls)
              ;; traverse current sat-generic-context constraints... and convert the applications there
              (for-each (lambda (expr)
                          (let ((new-expr (sal-ast/add-type-constraints expr empty-env new-decls new-constraint-queue already-processed)))
                            (queue/insert! new-constraint-queue new-expr)))
                        (queue->list (slot-value sat-generic-context :constraint-queue)))
              ;; convert types in curr-decls to super-types
              (lift-decls! curr-decls new-declarations)
              (set-slot-value! sat-generic-context :constraint-queue new-constraint-queue)
              ;; (print "end of iteration....")
              ;; (sat-generic-context/pp sat-generic-context)
              ;; (print "-----------------------------------")
              (loop (queue->list new-decls)))))
        (set-slot-value! sat-generic-context :declaration-queue new-declarations)))))

;; --------------------------------------------------------------------
;; Transformation: scalar -> integer
;;
;; Convert non-boolean scalar-values in integers
;; --------------------------------------------------------------------
(define-generic (sal-ast/scalar->int ast env))

(define-method (sal-ast/scalar->int (ast <sal-ast>) (env <primitive>))
  (sal-ast/map ast env sal-ast/scalar->int))
(define-method (sal-ast/scalar->int (ast <sal-scalar>) (env <primitive>))
  (if (sal-type/boolean? (sal-expr/type ast))
    (call-next-method)
    (make-sal-numeral (sal-scalar-element/idx ast) ast)))

(define (convert-scalar-decls! sat-generic-context)
  (let ((scalar->int-trace-info (make-eq-hash-table))
        (constraint-queue (slot-value sat-generic-context :constraint-queue)))
    (for-each (lambda (decl)
                (let ((type (slot-value decl :type)))
                  (when (and (instance-of? type <sal-scalar-type>)
                             (not (sal-type/boolean? type)))
                    (let* ((num-elems (length (slot-value type :scalar-elements)))
                           (place-provider decl)
                           (lower (make-sal-numeral 0 place-provider))
                           (upper (make-sal-numeral (- num-elems 1) place-provider))
                           (name-expr (make-sal-name-expr decl place-provider))
                           (ge-lower (make-sal-builtin-application <sal-ge> place-provider name-expr lower))
                           (le-upper (make-sal-builtin-application <sal-le> place-provider name-expr upper))
                           (int-pred (make-sal-builtin-application <sal-int-pred> place-provider name-expr)))
                      (queue/push! constraint-queue int-pred)
                      (queue/push! constraint-queue ge-lower)
                      (queue/push! constraint-queue le-upper)
                      (eq-hash-table/put! scalar->int-trace-info decl type) ;; store the original type...
;; BD:		      (display* "converting scalar declaration: ")(sal/pp decl) (newline)
;;                    (set-slot-value! decl :type (make-sal-builtin-name <sal-number-type> type))
;; original type must be maintained to recover the concrete counterexample from yices
;;
		      ))))
              (queue->list (slot-value sat-generic-context :declaration-queue)))
    (set-slot-value! sat-generic-context :scalar->int-trace-info scalar->int-trace-info)))
                          
(define (sat-generic-context/convert-scalar-to-int! sat-generic-context)
  (let ((empty-env (make-empty-env)))
    (convert-scalar-decls! sat-generic-context)
    (sat-generic-context/apply-simple-transformation! sat-generic-context 
                                              (lambda (expr) (sal-ast/scalar->int expr empty-env))
                                              "converting (remaining) scalars to integers")))

;; --------------------------------------------------------------------
;; Transformation: scalar type flattening
;;
;; This transformation is on a flag.
;;
;; If a scalar variable x only shows up in equalities and disequalities of
;; the form:
;;  -  x = v1  
;;  -  x /= v2
;;  -  v3 = x
;;  -  v4 /= x
;;
;; where v1, v2, v3, and v4 are variables which have the same property or are scalar values,
;; then the variable x can be breaken in bits (boolean values).
;; 
;; Example:
;;
;;   x : PC
;;   where PC
;;   PC : { pc1, pc2, pc3 }
;;
;; is transformed into
;;
;;   x!1 : BOOLEAN
;;   x!2 : BOOLEAN
;;   (NOT x!1 OR NOT x!2)
;;
;; Every equality/disequality is transformed in a conjunction 
;;
;; Remark: the slot :scalar-trace-info is used to store the relationship between
;; x and {x!1, x!2}.
;;
;; The ITEs must be converted to BOOLEAN-ITEs before executing this function...
;; 
;; --------------------------------------------------------------------
(define-class <sat-scalar-bit-decl> (<sat-decl>) ())
(define-class <sat-scalar-bit-aux-decl> (<sat-scalar-bit-decl> <sat-aux-decl>) ())

;; remove from scalars-to-convert table, declarations that are not worth to map to
;; boolean. A scalar is not worth to map to boolean if it is the argument of 
;; an application.
(define (filter-non-trivial-scalars! ast scalars-to-convert)
  (let loop ((ast ast)
             (eq-parent? #f)
             (sibling #unspecified))
    (cond
     ((or (instance-of? ast <sal-eq>)
          (instance-of? ast <sal-diseq>))
      (multiple-value-bind
          (arg1 arg2)
          (sal-binary-application/arguments ast)
        (loop arg1 #t arg2)
        (loop arg2 #t arg1)))
     ((and (instance-of? ast <sal-name-expr>)
           (eq-hash-table/get scalars-to-convert (slot-value ast :decl)))
      ;; check if the declaration associated with ast should be removed
      ;; from the table
      (unless (and eq-parent? ;; the node is the child of = or /=
                   (or (and (instance-of? sibling <sal-name-expr>) ;; sibling is a nameexpr and it is still in scalars-to-convert
                            (eq-hash-table/get scalars-to-convert (slot-value sibling :decl)))
                       (instance-of? sibling <sal-scalar>))) ;; or the sibling is a scalar value
        ;; (verbose-message 1 "removing... ~a" (sal-name-ref/name ast))
        (eq-hash-table/delete! scalars-to-convert (slot-value ast :decl))))
     (else
      (sal-ast/for-each-children (lambda (child)
                                   (loop child #f #unspecified))
                                 ast)))))

(define (collect-scalars-to-be-converted sat-generic-context)
  (let ((scalars-to-convert (make-eq-hash-table)))
    (for-each (lambda (decl)
                (let ((type (slot-value decl :type)))
                  (when (and (instance-of? type <sal-scalar-type>)
                             (not (sal-type/boolean? type)))
                    (eq-hash-table/put! scalars-to-convert decl #unspecified))))
              (queue->list (slot-value sat-generic-context :declaration-queue)))
    (let loop ((num-scalars-to-convert (eq-hash-table/size scalars-to-convert)))
      (when (> num-scalars-to-convert 0)
        (for-each (lambda (expr)
                    ;; (verbose-message 1 "processing ~a" (sal-ast->list expr))
                    (filter-non-trivial-scalars! expr scalars-to-convert))
                  (queue->list (slot-value sat-generic-context :constraint-queue)))
        (let ((new-num-scalars-to-convert (eq-hash-table/size scalars-to-convert)))
          (unless (= num-scalars-to-convert new-num-scalars-to-convert)
            (loop new-num-scalars-to-convert)))))
    scalars-to-convert))
    
(define (convert-scalar-decls-to-boolean-decls! sat-generic-context scalars-to-convert)
  (let ((new-decl-queue (make-queue))
        (scalar->bool-trace-info (make-eq-hash-table))
        (constraint-queue (slot-value sat-generic-context :constraint-queue))
        (eliminated-declaration-queue (slot-value sat-generic-context :eliminated-declaration-queue))
        (env (make-empty-env)))
    (for-each (lambda (decl)
                (cond
                 ((eq-hash-table/get scalars-to-convert decl)
                  (let* ((decl-list (map (lambda (new-decl)
                                           (if (instance-of? decl <sat-aux-decl>)
                                             (change-ast-class new-decl <sat-scalar-bit-aux-decl>)
                                             (change-ast-class new-decl <sat-scalar-bit-decl>)))
                                         (sal-var-decl-list->sal-bool-var-decl-list (list decl) gen-unique-name)))
                         (bit-list (map make-sal-name-expr decl-list))
                         (env-entry (cons bit-list (slot-value decl :type)))
                         (new-env (update-env env decl env-entry))
                         (name-expr (make-sal-name-expr decl))
                         (membership-expr (sal-type/finite-rep-membership-expr (slot-value decl :type) new-env name-expr)))
                    ;; (breakpoint "scalar" (decl-list bit-list decl) (instance-of? decl <sat-aux-decl>))
                    (eq-hash-table/put! scalar->bool-trace-info decl decl-list)
                    (set! env new-env)
                    (constraint-queue/insert! constraint-queue membership-expr)
                    (queue/insert! eliminated-declaration-queue decl)
                    (queue/append! new-decl-queue decl-list)))
                 (else
                  (queue/insert! new-decl-queue decl))))
              (queue->list (slot-value sat-generic-context :declaration-queue)))
    (set-slot-value! sat-generic-context :declaration-queue new-decl-queue)
    (set-slot-value! sat-generic-context :scalar->bool-trace-info scalar->bool-trace-info)
    env))

(define (sal-eq/scalar->bool ast env decl-queue constraint-queue scalars-to-convert call-next-method)
  (multiple-value-bind
      (arg1 arg2)
      (sal-binary-application/arguments ast)
    (if (or (and (instance-of? arg1 <sal-name-expr>) (eq-hash-table/get scalars-to-convert (slot-value arg1 :decl)))
            (and (instance-of? arg2 <sal-name-expr>) (eq-hash-table/get scalars-to-convert (slot-value arg2 :decl))))
      (let* ((aux-decl-queue (make-queue))
             (tmp-ast (car (sal-expr->boolean-expr-core ast env aux-decl-queue)))
             (new-ast (sal-ast/simplify (make-sal-let-expr (queue->list aux-decl-queue) tmp-ast))))
        (sal-ast/remove-lets new-ast env decl-queue constraint-queue))
      (call-next-method))))

(define-generic (sal-ast/scalar->bool ast env decl-queue constraint-queue scalars-to-convert))
(define-method (sal-ast/scalar->bool (ast <sal-ast>) (env <primitive>) (decl-queue <primitive>) 
                                     (constraint-queue <primitive>) (scalars-to-convert <primitive>))
  (sal-ast/map ast env (lambda (child-ast new-env) (sal-ast/scalar->bool child-ast new-env decl-queue constraint-queue scalars-to-convert))))
(define-method (sal-ast/scalar->bool (ast <sal-eq>) (env <primitive>) (decl-queue <primitive>) 
                                     (constraint-queue <primitive>) (scalars-to-convert <primitive>))
  (sal-eq/scalar->bool ast env decl-queue constraint-queue scalars-to-convert call-next-method))
(define-method (sal-ast/scalar->bool (ast <sal-diseq>) (env <primitive>) (decl-queue <primitive>) 
                                     (constraint-queue <primitive>) (scalars-to-convert <primitive>))
  (sal-eq/scalar->bool ast env decl-queue constraint-queue scalars-to-convert call-next-method))
                
(define (sat-generic-context/convert-scalar-to-boolean! sat-generic-context)
  (let* ((scalars-to-convert (collect-scalars-to-be-converted sat-generic-context))
         (env (convert-scalar-decls-to-boolean-decls! sat-generic-context scalars-to-convert)))
    (unless (empty-env? env)
      (sat-generic-context/apply-transformation! sat-generic-context 
                                         (lambda (expr decl-queue constraint-queue) 
                                           (let ((new-expr (sal-ast/scalar->bool expr env decl-queue constraint-queue scalars-to-convert)))
                                             [assert (new-expr) 
                                                     ;; check if new-expr doesn't contain a references to the declarations that were 
                                                     ;; supposed to be removed.
                                                     (not (sal-ast/find (lambda (n)
                                                                          (and (instance-of? n <sal-name-expr>)
                                                                               (eq-hash-table/get scalars-to-convert (slot-value n :decl))))
                                                                        new-expr))]
                                             new-expr))
                                         "converting (non-boolean) scalar values to boolean values"))))
        
;; --------------------------------------------------------------------
;; Transformation: <sal-int-pred> <sal-real-pred> preprocessing
;; 
;; Move the root <sal-int-pred> constraints to the beginning of the formula,
;; and remove the <sal-real-pred> and remaining <sal-int-pred> constraints.
;;
;; --------------------------------------------------------------------
(define-generic (sal-ast/eliminate-int-real-pred ast env polarity new-decl-queue))

(define-method (sal-ast/eliminate-int-real-pred (ast <sal-ast>) (env <primitive>) (polarity <polarity>) (new-decl-queue <primitive>))
  (sal-ast/map-using-polarity ast env polarity (lambda (child-ast new-env new-polarity)
                                                 (sal-ast/eliminate-int-real-pred child-ast new-env new-polarity new-decl-queue))))

(define-method (sal-ast/eliminate-int-real-pred (ast <sal-int-pred>) (env <primitive>) (polarity <pos>) (new-decl-queue <primitive>))
  (make-sal-true ast))
(define-method (sal-ast/eliminate-int-real-pred (ast <sal-int-pred>) (env <primitive>) (polarity <neg>) (new-decl-queue <primitive>))
  (make-sal-false ast))
(define-method (sal-ast/eliminate-int-real-pred (ast <sal-int-pred>) (env <primitive>) (polarity <polarity>) (new-decl-queue <primitive>))
  (make-bool-aux-name-expr! new-decl-queue ast))
(define-method (sal-ast/eliminate-int-real-pred (ast <sal-real-pred>) (env <primitive>) (polarity <pos>) (new-decl-queue <primitive>))
  (make-sal-true ast))
(define-method (sal-ast/eliminate-int-real-pred (ast <sal-real-pred>) (env <primitive>) (polarity <neg>) (new-decl-queue <primitive>))
  (make-sal-false ast))
(define-method (sal-ast/eliminate-int-real-pred (ast <sal-real-pred>) (env <primitive>) (polarity <polarity>) (new-decl-queue <primitive>))
  (make-bool-aux-name-expr! new-decl-queue ast))

(define (sat-generic-context/preprocess-type-predicates! sat-generic-context)
  (verbose-message 3 "    preprocessing type predicates...")
  (display-runtime 4 "      time: ~a secs"
    (lambda ()
      (let ((new-constraint-queue (make-queue))
            (empty-env (make-empty-env))
            (decl-queue (slot-value sat-generic-context :declaration-queue)))
        (for-each (lambda (expr)
                    (cond
                     ((instance-of? expr <sal-int-pred>) 
                      (queue/push! new-constraint-queue expr))
                     ((instance-of? expr <sal-real-pred>)
                      ;; ignore
                      #unspecified)
                     (else
                      (queue/insert! new-constraint-queue (sal-ast/eliminate-int-real-pred expr empty-env *pos* decl-queue)))))
                  (queue->list (slot-value sat-generic-context :constraint-queue)))
        (set-slot-value! sat-generic-context :constraint-queue new-constraint-queue)))))

;; --------------------------------------------------------------------
;; Main transformation...
;; 
;;
;; --------------------------------------------------------------------
(define (sat-generic-context/simplify! sat-generic-context . svsv)
  (verbose-message 2 "  simplifying formula...")
  (display-runtime 3 "    simplification time: ~a secs"
    (lambda ()
      (let ((force-array-theory-elimination? (svsv/value svsv :eliminate-array-theory? #f))
            (ite->ite-bool? (svsv/value svsv :ite->ite-bool? #t))
            (eliminate-div-mod? (svsv/value svsv :eliminate-div-mod? #t))
            (eliminate-int-diseq? (svsv/value svsv :eliminate-int-diseq? #f)))
        (when (or *sat-generic-context-eliminate-array-theory* force-array-theory-elimination?)
          (sat-generic-context/eliminate-array-theory! sat-generic-context))
        (let loop ()
          (let ((processed-lambdas? #f))
            (let inner-loop ()
              (let ((num-lambdas (sat-generic-context/num-lambdas sat-generic-context)))
                (when (> num-lambdas 0)
                  (set! processed-lambdas? #t)
                  (sat-generic-context/eliminate-lambdas! sat-generic-context)
                  (sat-generic-context/lambda-expansion! sat-generic-context))
                (sat-generic-context/ho-eq-expansion! sat-generic-context)
                (let ((num-quatifiers (sat-generic-context/num-quantifiers sat-generic-context)))
                  (when (> num-quatifiers 0)
                    (sat-generic-context/eliminate-quantifiers! sat-generic-context))
                  (let ((new-num-lambdas (sat-generic-context/num-lambdas sat-generic-context))
                        (new-num-quantifiers (sat-generic-context/num-quantifiers sat-generic-context)))
                    (unless (or (and (= new-num-lambdas 0) (= new-num-quantifiers 0))
                                (not (= new-num-lambdas num-lambdas))
                                (not (= new-num-quantifiers num-quatifiers)))
                      (cond
                       ((sat-generic-context/find-lambda sat-generic-context) =>
                        (lambda (lambda-ast)
                          (sign-unsupported-feature lambda-ast "Failed to remove lambda expression. Backend satisfiability solvers do not support lambda expressions.")))
                       ((sat-generic-context/find-quantifier sat-generic-context) =>
                        (lambda (quantifier)
                          (sign-unsupported-feature quantifier "Failed to remove quantifier. Backend satisfiability solvers do not support quantifiers.")))
                       (else 
                        (internal-error))))
                    (sat-generic-context/remove-lets! sat-generic-context)
                    (when (or (not (= new-num-lambdas 0))
                              (not (= new-num-quantifiers 0)))
                      (inner-loop))))))
            (when processed-lambdas?
              ;; the following transformation is expensive... it is only applied if the formula contained lambdas...
              ;; motivation: when lambda elimination is performed (using beta reduction) usually a lot of common
              ;; subexpressions are created.
              (sat-generic-context/cse! sat-generic-context))
            (when eliminate-div-mod?
              (sat-generic-context/eliminate-div-mod! sat-generic-context))
            (when eliminate-int-diseq?
              (sat-generic-context/eliminate-int-diseq! sat-generic-context))
            (when ite->ite-bool?
              (sat-generic-context/ite->bool-ite! sat-generic-context))
            (sat-generic-context/add-type-constraints! sat-generic-context)
            (when (or (sat-generic-context/find-lambda sat-generic-context)
                      (sat-generic-context/find-quantifier sat-generic-context))
              ;; new quantifiers and lambda expression were introduced by add-type-constraints...
              (loop))))
        (when *sat-generic-context-map-scalars-to-booleans*
          (sat-generic-context/convert-scalar-to-boolean! sat-generic-context))
        (sat-generic-context/convert-scalar-to-int! sat-generic-context)
        (sat-generic-context/preprocess-type-predicates! sat-generic-context)))))

;; --------------------------------------------------------------------
;; SAT context interface with a solver
;; 
;;
;; --------------------------------------------------------------------
(define-class <sat-generic-context-solver-interface> () ())
;; returns a list of constraints if satisfiable, #f otherwise
(define-generic (solver-interface/solve! solver sat-generic-context place-provider))


;; --------------------------------------------------------------------
;; Collect constraints that can be viewed as lets...
;;
;; The constraints are removed from the context, and insert 
;; into a list of pairs (decl . expr)
;;
;; Warning the removed constraints are not sent to :eliminated-constraint-queue
;; So, this transformation should be mainly used by decision procedures
;; which I do not print counter-examples.
;;
;; The optional argument ite-as-decl? specifies wheter if-then-else should
;; be converted to auxiliary declarations. This convertion is useful for
;; decision procedures which use a SMV-like case statement to encode if-then-else.
;; --------------------------------------------------------------------
(define (sat-generic-context/collect-definitions! sat-generic-context . ite-as-decl?)
  (verbose-message 2 "  collecting definitions...")
  (display-runtime 3 "    collection time: ~a secs"
    (lambda ()
      (let* ((ite-as-decl? (optional-arg ite-as-decl? #f))
             (found-var-decls (make-eq-hash-table))
             (def-queue (make-queue))
             (def-table (make-eq-hash-table))
             (new-constraint-queue (make-queue))
             (new-decl-queue (make-queue))
             (empty-env (make-empty-env))
             (ite->decl (lambda (expr)
                          (if ite-as-decl?
                            (sal-ast/ite->decl expr empty-env def-queue)
                            expr))))
        (for-each (lambda (constraint)
                    (let ((constraint (ite->decl constraint)))
                      (if (not (instance-of? constraint <sal-eq>))
                        (queue/insert! new-constraint-queue constraint)
                        (multiple-value-bind
                            (lhs rhs)
                            (sal-binary-application/arguments constraint)
                          (multiple-value-bind
                              (can-lhs can-rhs)
                              (if (instance-of? lhs <sal-name-expr>)
                                (values lhs rhs)
                                (values rhs lhs))
                            (cond
                             ((and (instance-of? can-lhs <sal-name-expr>)
                                   (instance-of? (slot-value can-lhs :decl) <sat-decl>)
                                   (not (sal-ast/contains-reference? can-rhs (slot-value can-lhs :decl)))
                                   (not (eq-hash-table/get found-var-decls (slot-value can-lhs :decl))))
                              (queue/insert! def-queue (cons (slot-value can-lhs :decl) can-rhs))
                              (eq-hash-table/put! def-table (slot-value can-lhs :decl) #t))
                             (else
                              (queue/insert! new-constraint-queue constraint))))))
                      ;; store used variables in found-var-decls
                      (sal-ast/for-each (lambda (ast)
                                          (when (and (instance-of? ast <sal-name-expr>)
                                                     (instance-of? (slot-value ast :decl) <sat-decl>))
                                            (eq-hash-table/put! found-var-decls (slot-value ast :decl) #t)))
                                        constraint)))
                  (queue->list (slot-value sat-generic-context :constraint-queue)))
        ;; populate the new-decl-queue
        (for-each (lambda (decl)
                    (unless (eq-hash-table/get def-table decl)
                      (queue/insert! new-decl-queue decl)))
                  (queue->list (slot-value sat-generic-context :declaration-queue)))
        ;; update
        (set-slot-value! sat-generic-context :declaration-queue new-decl-queue)
        (set-slot-value! sat-generic-context :constraint-queue new-constraint-queue)
        ;; return the list of pairs (decl . expr)
        (queue->list def-queue)))))

(define-generic (sal-ast/ite->decl ast env def-queue))

(define-method (sal-ast/ite->decl (ast <sal-ast>) (env <primitive>) (def-queue <primitive>))
  (sal-ast/map ast env (lambda (child-ast new-env) (sal-ast/ite->decl child-ast new-env def-queue))))

(define-method (sal-ast/ite->decl (ast <sal-conditional>) (env <primitive>) (def-queue <primitive>))
  (let* ((new-conditional (copy-ast ast
                                    :cond-expr (sal-ast/ite->decl (slot-value ast :cond-expr) env def-queue)
                                    :then-expr (sal-ast/ite->decl (slot-value ast :then-expr) env def-queue)
                                    :else-expr (sal-ast/ite->decl (slot-value ast :else-expr) env def-queue)))
         (new-name (gen-unique-name 'aux))
         (new-id (make-sal-identifier ast new-name))
         (new-decl (make-ast-instance <sat-decl> ast
                                      :id new-id
                                      :type (sal-expr/type (slot-value ast :then-expr))))
         (result (make-sal-name-expr new-decl ast)))
    (queue/insert! def-queue (cons new-decl new-conditional))
    result))

  
