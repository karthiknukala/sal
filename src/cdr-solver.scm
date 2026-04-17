;;
;; SAL 3.3, Copyright (C) 2026, SRI International.  All Rights Reserved.
;;
;; SAL is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;

(module cdr-solver
        (include "sal.sch")
        (import utility sal-expression sal-ast-eq sal-ast-simplify sal-module sal-type)
        (export <cdr-solver-capabilities>
                (make-cdr-solver-capabilities . svsv)
                (cdr-solver-capabilities/incremental? caps)
                (cdr-solver-capabilities/models? caps)
                (cdr-solver-capabilities/interpolants? caps)
                (cdr-solver-capabilities/unsat-cores? caps)
                <cdr-cube>
                (make-cdr-cube bindings)
                (cdr-cube/bindings cube)
                (cdr-cube/size cube)
                (cdr-cube/empty? cube)
                (cdr-cube/without-binding cube idx)
                (cdr-cube/equivalent? cube1 cube2)
                (cdr-cube->expr cube place-provider)
                (cdr-cube->lemma cube place-provider)
                <cdr-solver>
                (cdr-solver/capabilities solver)
                (cdr-solver/flat-module solver)
                (cdr-solver/state-vars solver)
                (cdr-solver/trace-solver-id solver)
                (cdr-solver/solver-description solver)
                (cdr-solver/add-frame! solver)
                (cdr-solver/add-lemma-at-level! solver level lemma)
                (cdr-solver/add-lemma! solver max-level lemma)
                (cdr-solver/check-initial-bad solver)
                (cdr-solver/check-initial-cube solver cube)
                (cdr-solver/check-bad-at-frame solver level)
                (cdr-solver/check-predecessor solver level cube)
                (cdr-solver/check-predecessor-status solver level cube)
                (cdr-solver/check-lemma-propagation solver level lemma)
                (cdr-solver/interpolant-for-cube solver level cube)
                (cdr-solver/learn-forward-cube solver level cube)
                (cdr-solver/reset-induction! solver depth)
                (cdr-solver/add-induction-lemma! solver lemma)
                (cdr-solver/check-inductive-witness solver lemma)
                (cdr-solver/check-induction-path solver cube target-expr)
                (cdr-solver/usable-lemma solver lemma)
                (cdr-solver/close! solver)))

(define-class <cdr-solver-capabilities> ()
  (:incremental? :models? :interpolants? :unsat-cores?))

(define (make-cdr-solver-capabilities . svsv)
  (make-instance <cdr-solver-capabilities>
                 :incremental? (svsv/value svsv :incremental? #f)
                 :models? (svsv/value svsv :models? #f)
                 :interpolants? (svsv/value svsv :interpolants? #f)
                 :unsat-cores? (svsv/value svsv :unsat-cores? #f)))

(define (cdr-solver-capabilities/incremental? caps)
  (slot-value caps :incremental?))

(define (cdr-solver-capabilities/models? caps)
  (slot-value caps :models?))

(define (cdr-solver-capabilities/interpolants? caps)
  (slot-value caps :interpolants?))

(define (cdr-solver-capabilities/unsat-cores? caps)
  (slot-value caps :unsat-cores?))

(define-class <cdr-cube> ()
  (:bindings))

(define (normalize-cube-bindings bindings)
  (sort (map identity bindings)
        (lambda (b1 b2)
          (symbol<? (sal-decl/name (car b1))
                    (sal-decl/name (car b2))))))

(define (make-cdr-cube bindings)
  (make-instance <cdr-cube> :bindings (normalize-cube-bindings bindings)))

(define (cdr-cube/bindings cube)
  (slot-value cube :bindings))

(define (cdr-cube/size cube)
  (length (cdr-cube/bindings cube)))

(define (cdr-cube/empty? cube)
  (null? (cdr-cube/bindings cube)))

(define (cdr-cube/without-binding cube idx)
  (make-cdr-cube
   (let loop ((remaining (cdr-cube/bindings cube))
              (i 0)
              (result '()))
     (cond
      ((null? remaining)
       (reverse! result))
      ((= i idx)
       (loop (cdr remaining) (+ i 1) result))
      (else
       (loop (cdr remaining) (+ i 1) (cons (car remaining) result)))))))

(define (cdr-cube/equivalent? cube1 cube2)
  (and (= (cdr-cube/size cube1) (cdr-cube/size cube2))
       (for-all (lambda (b1 b2)
                  (and (eq? (car b1) (car b2))
                       (sal-ast/equivalent? (cdr b1) (cdr b2))))
                (cdr-cube/bindings cube1)
                (cdr-cube/bindings cube2))))

(define (binding->literal binding place-provider)
  (let* ((decl (car binding))
         (value (cdr binding))
         (name-expr (make-sal-name-expr decl place-provider))
         (type (slot-value decl :type)))
    (if (sal-type/boolean? type)
      (if (sal-expr/true? value)
        name-expr
        (make-sal-not name-expr))
      (make-sal-equality name-expr value))))

(define (cdr-cube->expr cube place-provider)
  (let ((literals (map (lambda (binding)
                         (binding->literal binding place-provider))
                       (cdr-cube/bindings cube))))
    (cond
     ((null? literals)
      (make-sal-true place-provider))
     ((null? (cdr literals))
      (car literals))
     (else
      (make-sal-and* literals place-provider)))))

(define (cdr-cube->lemma cube place-provider)
  (sal-ast/simplify (make-sal-not (cdr-cube->expr cube place-provider))))

(define-class <cdr-solver> ()
  (:capabilities :flat-module :state-vars :trace-solver-id :solver-description))

(define-generic (cdr-solver/capabilities solver))
(define-method (cdr-solver/capabilities (solver <cdr-solver>))
  (slot-value solver :capabilities))

(define-generic (cdr-solver/flat-module solver))
(define-method (cdr-solver/flat-module (solver <cdr-solver>))
  (slot-value solver :flat-module))

(define-generic (cdr-solver/state-vars solver))
(define-method (cdr-solver/state-vars (solver <cdr-solver>))
  (slot-value solver :state-vars))

(define-generic (cdr-solver/trace-solver-id solver))
(define-method (cdr-solver/trace-solver-id (solver <cdr-solver>))
  (slot-value solver :trace-solver-id))

(define-generic (cdr-solver/solver-description solver))
(define-method (cdr-solver/solver-description (solver <cdr-solver>))
  (slot-value solver :solver-description))

(define-generic (cdr-solver/add-frame! solver))
(define-generic (cdr-solver/add-lemma-at-level! solver level lemma))
(define-generic (cdr-solver/add-lemma! solver max-level lemma))
(define-generic (cdr-solver/check-initial-bad solver))
(define-generic (cdr-solver/check-initial-cube solver cube))
(define-generic (cdr-solver/check-bad-at-frame solver level))
(define-generic (cdr-solver/check-predecessor solver level cube))
(define-generic (cdr-solver/check-predecessor-status solver level cube))
(define-generic (cdr-solver/check-lemma-propagation solver level lemma))
(define-generic (cdr-solver/interpolant-for-cube solver level cube))
(define-generic (cdr-solver/learn-forward-cube solver level cube))
(define-generic (cdr-solver/reset-induction! solver depth))
(define-generic (cdr-solver/add-induction-lemma! solver lemma))
(define-generic (cdr-solver/check-inductive-witness solver lemma))
(define-generic (cdr-solver/check-induction-path solver cube target-expr))
(define-generic (cdr-solver/usable-lemma solver lemma))
(define-method (cdr-solver/usable-lemma (solver <cdr-solver>) (lemma <sal-expr>))
  lemma)
(define-generic (cdr-solver/close! solver))
