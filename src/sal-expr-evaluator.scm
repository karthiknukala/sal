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

(module sal-expr-evaluator
        (include "sal.sch")
        (include "iterators.sch")
        (import sal-environment sal-ast-env sal-expression sal-ast-copy sal-type sal-ast-simplify sal-pp)
        (export (sal-expr/evaluate expr)
                (sal-expr/evaluate-core expr env depth)
                (sal-evaluator/set-max-execution-depth! max))
        )

(define *max-eval-depth* 1024)

(define (sal-evaluator/set-max-execution-depth! max)
  (set! *max-eval-depth* max))

(define-api (sal-expr/evaluate (expr <sal-expr>))
  (sal-expr/evaluate-core expr (make-empty-env) 0))

(define-generic (sal-expr/evaluate-core expr env depth))

(define (mk-proc-child env depth)
  (cut sal-expr/evaluate-core <> env depth))

(register-app-error! 'expr-evaluator)

(define (sign-evaluator-error ast msg . args)
  (error 'expr-evaluator (apply format-with-location ast msg args) #unspecified))

(define (sign-expression-cannot-be-evaluated expr)
  (sign-evaluator-error expr "Expression cannot be evaluated."))

(define (mk-proc-default expr)
  (lambda args
    (sign-expression-cannot-be-evaluated expr)))

(define (default-method expr env depth)
  (let ((proc (lambda (n e) (sal-expr/evaluate-core n e depth))))
    (sal-ast/local-simplify-core expr env proc)))

(define-method (sal-expr/evaluate-core (expr <sal-ast>) (env <primitive>) (depth <primitive>))
  (default-method expr env depth))

(define-method (sal-expr/evaluate-core :around (expr <sal-expr>) (env <primitive>) (depth <primitive>))
  (let ((result (call-next-method)))
    (unless (or (sal-expr/value? result) (instance-of? expr <sal-name-expr>))
      (sign-expression-cannot-be-evaluated expr))
    result))

(define-method (sal-expr/evaluate-core (expr <sal-builtin-application>) (env <primitive>) (depth <primitive>))
  (default-method expr env depth))

(define-method (sal-expr/evaluate-core (expr <sal-application>) (env <primitive>) (depth <primitive>))
  (let ((fun (sal-expr/evaluate-core (slot-value expr :fun) env depth)))
    (cond
     ((sal-application/promote! expr)
      ;; promoted...
      (sal-expr/evaluate-core expr env depth))
     ((instance-of? fun <sal-lambda>)
      (let ((arg (sal-expr/evaluate-core (slot-value expr :arg) env depth)))
        (when (> depth *max-eval-depth*)
          (sign-evaluator-error expr "Infinite loop detector threshold was reached while evaluating expression. Please increase the threshold or rewrite your specification."))
        ;; (breakpoint "eval app" (fun arg expr) #t)
        ;; (breakpoint "sal-expr/evaluate-core" (expr env depth fun arg) (not (= (length (slot-value fun :local-decls))
        ;;       (length (sal-argument->argument-list arg)))))
        ;; (breakpoint "expand" (fun arg) #t)
        (let* ((local-decls (slot-value fun :local-decls))
               (env (update-env* env local-decls (sal-argument->argument-list arg (length local-decls))))) 
          (sal-expr/evaluate-core (slot-value fun :expr) env (+ depth 1)))))
     (else
      (call-next-method)))))

(define-method (sal-expr/evaluate-core (expr <sal-lambda>) (env <primitive>) (depth <primitive>))
  ;; I don't want to create a closure... so, I'm performing a beta reduction...
  (sal-ast/substitute expr env))

(define-method (sal-expr/evaluate-core (expr <sal-name-expr>) (env <primitive>) (depth <primitive>))
  (let ((new-expr (sal-ast/substitute expr env)) ;; apply substitutions...
        (new-env (make-empty-env))) ;; I don't need to keep the environment, since I've already applied the substitutions...
    (if (instance-of? new-expr <sal-name-expr>)
      (cond
       ((sal-name-expr/definition new-expr) =>
        (lambda (definition)
          (sal-expr/evaluate-core definition new-env depth)))
       (else
        new-expr))
      (sal-expr/evaluate-core new-expr new-env depth))))

(define-method (sal-expr/evaluate-core (expr <sal-array-update>) (env <primitive>) (depth <primitive>))
  (let* ((target (sal-expr/evaluate-core (slot-value expr :target) env depth))
         (idx (sal-expr/evaluate-core (slot-value expr :idx) env depth))
         (new-value (sal-expr/evaluate-core (slot-value expr :new-value) env depth)))
    (cond
     ((instance-of? target <sal-collection-literal>)
      (sal-array-literal/update target idx new-value))
     (else
      (update-ast-slots expr
                        :target target
                        :idx idx
                        :new-value new-value)))))

(define-method (sal-expr/evaluate-core (expr <sal-let-expr>) (env <primitive>) (depth <primitive>))
  (let* ((local-decls (slot-value expr :local-decls))
         (new-local-decls (conservative-map-1 (cut sal-expr/evaluate-core <> env depth) local-decls))
         (env (update-env* env local-decls new-local-decls)))
    (sal-expr/evaluate-core (slot-value expr :expr) env depth)))

(define (sal-quantified-expr/evaluate expr env depth true? false? mk-true mk-false)
  (try
   (let* ((local-decls (slot-value expr :local-decls))
          (body (slot-value expr :expr))
          (types (map (cut slot-value <> :type) local-decls))
          (tuple-type (make-ast-instance <sal-tuple-type> expr :types types))
          (proc-child (lambda (child)
                        (sal-expr/evaluate-core child env depth)))
          (iterator (sal-type/make-iterator-core tuple-type proc-child)))
     (bind-exit (exit)
       (iterator/for-each 
        (lambda (tuple-literal)
;           (print "QUANT: ")
;           (sal/pp tuple-literal) (print "")
;           (sal/pp body) (print "") (print "--------------")
          (let* ((env (update-env* env local-decls (slot-value tuple-literal :exprs)))
                 (result (sal-expr/evaluate-core body env depth)))
            (cond
             ((true? result)
              (exit (mk-true expr)))
             ((false? result)
              ;; do nothing
              #unspecified)
             (else
              (sign-expression-cannot-be-evaluated expr)))))
        iterator)
       (mk-false expr)))
   (catch 'type-iterator
          (lambda (msg) 
            (sign-evaluator-error expr "Expression cannot be evaluated, failed to create type element iterator, reason: ~a." msg)))))
  
(define-method (sal-expr/evaluate-core (expr <sal-exists-expr>) (env <primitive>) (depth <primitive>))
  (sal-quantified-expr/evaluate expr env depth sal-expr/true? sal-expr/false? make-sal-true make-sal-false))
          
(define-method (sal-expr/evaluate-core (expr <sal-for-all-expr>) (env <primitive>) (depth <primitive>))
  (sal-quantified-expr/evaluate expr env depth sal-expr/false? sal-expr/true? make-sal-false make-sal-true))
            
            
          


