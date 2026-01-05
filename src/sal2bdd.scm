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

(module sal2bdd
        (include "sal.sch")
        (import bdd sal-bdd-context sal-ast-env sal-expression queue
                sal-ast-expand)
        (export <bdd>
                (sal-expr->bdd-core ast env context)
                (sal-expr->bdd ast context)
                (sal-let-expr->bdd-core ast env context proc-body)
                (collect-implicit-lets ast implicit-let-queue))
        )

(define <bdd> <primitive>)

(define-generic (sal-expr->bdd-core ast env context))

;; convert an SAL expression with references to the variables of a transition system
(define-api (sal-expr->bdd (ast <sal-ast>) (context <sal-bdd-context>))
  (sal-expr->bdd-core ast (make-empty-env) context))

(define (sign-internal-bdd-translation-error)
  (error 'internal-bdd-translation-error 'internal-bdd-translation-error 'internal-bdd-translation-error))

(define-method (sal-expr->bdd-core (ast <sal-ast>) (env <primitive>) (context <sal-bdd-context>))
  (sign-unsupported-feature ast "This constructor cannot be converted to a BDD. The current translator only supports boolean expressions, if you believe that your transition system can be represented using only boolean formulas, please convert your flat-module using the function flat-module->boolean-flat-module."))

(define-method (sal-expr->bdd-core (ast <sal-definition-expression>) (env <primitive>) (context <sal-bdd-context>))
  (sal-expr->bdd-core (slot-value ast :expr) env context))

(define-method (sal-expr->bdd-core (ast <sal-next-operator>) (env <primitive>) (context <sal-bdd-context>))
  (let ((decl (slot-value (slot-value ast :name-expr) :decl)))
    (cond 
     ((sal-bdd-context/next-var context decl) =>
      identity)
     (else
      ;; (breakpoint "sal-expr->bdd-core" (ast env context) #t)
      (sign-unsupported-feature ast "The BDD translator does not support this use of the next variable.")))))

(define-method (sal-expr->bdd-core (ast <sal-true>) (env <primitive>) (context <sal-bdd-context>))
  (bdd/true (sal-bdd-context/manager context)))

(define-method (sal-expr->bdd-core (ast <sal-false>) (env <primitive>) (context <sal-bdd-context>))
  (bdd/false (sal-bdd-context/manager context)))

(define-method (sal-expr->bdd-core (ast <sal-name-expr>) (env <primitive>) (context <sal-bdd-context>))
  (let ((decl (slot-value ast :decl))
        (bdd-manager (sal-bdd-context/manager context)))
    (cond
     ((sal-bdd-context/curr-var context decl) =>
      identity)
     ((lookup-env decl env) => identity)
     (else
      ;; (breakpoint "sal->bdd" (ast env context decl bdd-manager) #t)
      (sign-internal-bdd-translation-error)))))

(define (sal-expr-list->bdd expr-list env context bdd-proc)
  [sal-assert "sal-expr-list->bdd" (expr-list) (not (null? expr-list))]
  (fold-left (lambda (result child-expr)
               (let* ((bdd (sal-expr->bdd-core child-expr env context))
                      (new-result (bdd-proc result bdd)))
                 new-result))
             (sal-expr->bdd-core (car expr-list) env context)
             (cdr expr-list)))

(define-method (sal-expr->bdd-core (ast <sal-and>) (env <primitive>) (context <sal-bdd-context>))
  (sal-expr-list->bdd (sal-application/argument-list ast) env context bdd/and))
    
(define-method (sal-expr->bdd-core (ast <sal-or>) (env <primitive>) (context <sal-bdd-context>))
  (sal-expr-list->bdd (sal-application/argument-list ast) env context bdd/or))

(define-method (sal-expr->bdd-core (ast <sal-not>) (env <primitive>) (context <sal-bdd-context>))
  (bdd/not (sal-expr->bdd-core (slot-value ast :arg) env context)))

(define (sal-binary-op->bdd ast env context bdd-proc)
  (multiple-value-bind
      (arg1 arg2)
      (sal-binary-application/arguments ast)
    (let* ((arg1-bdd (sal-expr->bdd-core arg1 env context))
           (arg2-bdd (sal-expr->bdd-core arg2 env context))
           (result-bdd (bdd-proc arg1-bdd arg2-bdd)))
      result-bdd)))

(define-method (sal-expr->bdd-core (ast <sal-eq>) (env <primitive>) (context <sal-bdd-context>))
  (sal-binary-op->bdd ast env context bdd/iff))

(define-method (sal-expr->bdd-core (ast <sal-diseq>) (env <primitive>) (context <sal-bdd-context>))
  (sal-binary-op->bdd ast env context bdd/xor))

(define (sal-let-expr->bdd-core ast env context proc-body)
  (let* ((let-decls (slot-value ast :local-decls))
         (bdds (map (lambda (let-decl)
                      (sal-expr->bdd-core (slot-value let-decl :value) env context))
                    let-decls))
         (env (update-env* env let-decls bdds)))
    (proc-body (slot-value ast :expr) env context)))
  
(define-method (sal-expr->bdd-core (ast <sal-let-expr>) (env <primitive>) (context <sal-bdd-context>))
  (sal-let-expr->bdd-core ast env context sal-expr->bdd-core))
  
(define-method (sal-expr->bdd-core (ast <sal-conditional>) (env <primitive>) (context <sal-bdd-context>))
  (let* ((cond-bdd (sal-expr->bdd-core (slot-value ast :cond-expr) env context))
         (then-bdd (sal-expr->bdd-core (slot-value ast :then-expr) env context))
         (else-bdd (sal-expr->bdd-core (slot-value ast :else-expr) env context))
         (result-bdd (bdd/ite cond-bdd then-bdd else-bdd)))
    result-bdd))

(define (sal-quantified-expr->bdd-core ast env context bdd-op)
  (let ((local-decls (slot-value ast :local-decls)))
    (sal-bdd-context/with-local-vars context local-decls
                                     (lambda (local-bdd-vars)
                                       (let ((new-env (update-env* env local-decls local-bdd-vars)))
                                         (bdd-op (sal-expr->bdd-core (slot-value ast :expr) new-env context)
                                                 (map bdd/var local-bdd-vars)))))))

(define-method (sal-expr->bdd-core (ast <sal-for-all-expr>) (env <primitive>) (context <sal-bdd-context>))
  (sal-quantified-expr->bdd-core ast env context bdd/for-all))

(define-method (sal-expr->bdd-core (ast <sal-exists-expr>) (env <primitive>) (context <sal-bdd-context>))
  (sal-quantified-expr->bdd-core ast env context bdd/exists))
  
(define-generic (collect-implicit-lets ast implicit-let-queue))

(define-method (collect-implicit-lets (ast <sal-ast>) (implicit-let-queue <queue>))
  ;; (breakpoint "foo" (ast) #t)
  (sign-unsupported-feature ast "The following definition is not supported by the BDD translator. This error is usually signed when you used IN statements in the definition section."))

(define-method (collect-implicit-lets (ast <sal-let-expr>) (implicit-let-queue <queue>))
  (for-each (lambda (let-decl)
              (queue/insert! implicit-let-queue (cons let-decl (slot-value let-decl :value))))
            (slot-value ast :local-decls))
  (collect-implicit-lets (slot-value ast :expr) implicit-let-queue))

(define-method (collect-implicit-lets (ast <sal-true>) (implicit-let-queue <queue>))
  ;; ignore it
  #unspecified)

(define-method (collect-implicit-lets (ast <sal-name-expr>) (implicit-let-queue <queue>))
  (cond
   ((sal-name-ref/state? ast)
    (queue/insert! implicit-let-queue (cons (slot-value ast :decl) (make-sal-true ast))))
   (else
    (call-next-method))))

(define-method (collect-implicit-lets (ast <sal-definition-expression>) (implicit-let-queue <queue>))
  (collect-implicit-lets (slot-value ast :expr) implicit-let-queue))

(define-method (collect-implicit-lets (ast <sal-and>) (implicit-let-queue <queue>))
  (for-each (cut collect-implicit-lets <> implicit-let-queue) (sal-application/argument-list ast)))

(define-method (collect-implicit-lets (ast <sal-not>) (implicit-let-queue <queue>))
  (let ((arg (slot-value ast :arg)))
    (if (and (instance-of? arg <sal-name-expr>) (sal-name-ref/state? arg))
      (queue/insert! implicit-let-queue (cons (slot-value arg :decl) (make-sal-false ast)))
      (call-next-method))))

(define-method (collect-implicit-lets (ast <sal-eq>) (implicit-let-queue <queue>))
  (multiple-value-bind
      (arg1 arg2)
      (sal-binary-application/arguments ast)
    (if (and (instance-of? arg1 <sal-name-expr>) 
             (sal-name-ref/state? arg1))
      (let ((decl (slot-value arg1 :decl)))
        (when (instance-of? decl <sal-input-state-var-decl>)
          (sign-source-error ast "Input variable defined in definition section. This is a bug, or the type checker is disabled."))
        (queue/insert! implicit-let-queue (cons decl arg2)))
      (call-next-method))))
