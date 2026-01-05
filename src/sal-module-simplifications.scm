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

(module sal-module-simplifications
        (include "sal.sch")
        (import sal-type sal-expression sal-ast-copy sal-ast-env sal-assertion 
                runtime sal-pp queue sal-ast-simplify)
        (export (sal-ast/substitute-state-var-definitions ast))
        )

(define-generic (sal-ast/substitute-state-var-definitions ast))

(define-method (sal-ast/substitute-state-var-definitions (ast <sal-ast>))
  (sign-unsupported-feature ast "This abstract syntax tree node does not support state variable definition simplification."))

(define-generic (sal-ast/def-var-substitution ast env proc))

(define-method (sal-ast/def-var-substitution (ast <sal-ast>) (env <primitive>) (proc <primitive>))
  (sal-ast/map ast env proc))

(define-method (sal-ast/def-var-substitution (ast <sal-next-operator>) (env <primitive>) (proc <primitive>))
  (let ((name-ref (slot-value ast :name-expr)))
    (cond
     ((lookup-env (sal-name-ref/decl name-ref) env) =>
      identity)
     (else
      ast))))

(define (apply-substitution arg env)
  (sal-ast/def-var-substitution arg env apply-substitution))

(define-inline (update-env-if-simple-val env var-decl val)
  (cond
   ((or (instance-of? val <sal-numeral>)
        (instance-of? val <sal-scalar>))
    ;; (sal/pp var-decl) (display " ") (sal/pp val) (print "")
    (update-env env var-decl val))
   (else
    env)))

(define (definitions->env flat-module env)
  (let* ((new-def-list (make-queue))
         (new-env env)
         (changed-defs? #f)
         [process-assignment 
          (lambda (assignment)
            (let* ((var (sal-binary-application/arg1 assignment))
                   (old-val (sal-binary-application/arg2 assignment)) 
                   (new-val (let ((aux (sal-ast/substitute old-val new-env)))
                              (if (eq? old-val aux)
                                old-val
                                (sal-ast/simplify aux))))
                   (var-decl (sal-name-ref/decl var)))
              (cond
               ((eq? old-val new-val)
                (queue/insert! new-def-list assignment)
                (unless (lookup-env var-decl new-env)
                  (set! new-env (update-env-if-simple-val new-env var-decl new-val))))
               (else
                (set! changed-defs? #t)
                (set! new-env (update-env-if-simple-val new-env var-decl new-val))
                (queue/insert! new-def-list (copy-ast assignment :arg (make-application-argument var new-val)))))))]
         [process-and 
          (lambda (and-app)
            (for-each (lambda (arg)
                        (if (instance-of? arg <sal-assignment>)
                          (process-assignment arg)
                          (queue/insert! new-def-list arg)))
                      (sal-application/argument-list and-app)))]
         [process-let
          (lambda (expr)
            (let loop ((expr expr))
              (cond
               ((instance-of? expr <sal-let-expr>)
                (update-ast-slots expr 
                                  :expr (loop (slot-value expr :expr))))
               ((instance-of? expr <sal-and>)
                (process-and expr)
                (make-sal-and* (queue->list new-def-list) expr))
               ((instance-of? expr <sal-assignment>)
                (process-assignment expr)
                (queue/front new-def-list))
               (else 
                expr))))]
         (new-defs (process-let (slot-value flat-module :definition))))
    (if changed-defs?
      (values new-env (copy-ast flat-module :definition new-defs))
      (values new-env flat-module))))

(define-method (sal-ast/substitute-state-var-definitions (flat-module <sal-flat-module>))
  (status-message :subst-state-var-def)
  (verbose-message 1 "substituting simple definitions...")
  (display-runtime 2 "  substitution time: ~a secs"
    (lambda ()
      (let loop ((env (make-empty-env))
                 (flat-module flat-module))
        (multiple-value-bind
            (new-env new-flat-module)
            (definitions->env flat-module env)
          ;; (verbose-message 10 "  substitution iteration...")
          (cond
           ((and (eq? env new-env) (eq? new-flat-module flat-module))
            (if (empty-env? env)
              flat-module
              (update-ast-slots flat-module
                                :initialization (apply-substitution (slot-value flat-module :initialization) env)
                                :transition (apply-substitution (slot-value flat-module :transition) env)
                                :valid-input-expr (apply-substitution (slot-value flat-module :valid-input-expr) env)
                                :valid-state-expr (apply-substitution (slot-value flat-module :valid-state-expr) env)
                                :valid-constant-expr (apply-substitution (slot-value flat-module :valid-constant-expr) env))))
           (else
            (loop new-env new-flat-module))))))
    :subst-state-var-def-time))

(define-method (sal-ast/substitute-state-var-definitions (assertion <sal-assertion-expr>))
  (sal-assertion/transformation-core 
   assertion (make-empty-env) 
   (lambda (module env) (sal-ast/substitute-state-var-definitions module))))


