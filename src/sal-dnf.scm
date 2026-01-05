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

(module sal-dnf
        (include "sal.sch")
        (include "iterators.sch")
        (import sal-expression sal-type sal-ast-simplify)
        (export (sal-dnf/and dnf1 dnf2)
                (sal-dnf/or dnf1 dnf2)
                (sal-dnf/not dnf)
                (sal-dnf/for-each-cube dnf proc)
                (sal-cube/for-each-literal cube proc)
                (sal-expr->dnf expr . pred?)
                (sal-dnf/cube-list expr)
                (sal-cube/literal-list expr))
        )

(define-generic (sal-dnf/and dnf1 dnf2))

(define-method (sal-dnf/and (dnf1 <sal-expr>) (dnf2 <sal-expr>))
  (make-sal-and dnf1 dnf2))

(define-method (sal-dnf/and (dnf1 <sal-and>) (dnf2 <sal-expr>))
  (apply make-sal-and (append (sal-application/argument-list dnf1)
                              (list dnf2))))

(define-method (sal-dnf/and (dnf1 <sal-or>) (dnf2 <sal-expr>))
  (apply make-sal-or (map (cut sal-dnf/and <> dnf2) 
                          (sal-application/argument-list dnf1))))

(define-method (sal-dnf/and (dnf1 <sal-expr>) (dnf2 <sal-or>))
  (apply make-sal-or (map (cut sal-dnf/and dnf1 <>)
                          (sal-application/argument-list dnf2))))

(define-method (sal-dnf/and (dnf1 <sal-and>) (dnf2 <sal-or>))
  (apply make-sal-or (map (cut sal-dnf/and dnf1 <>) 
                          (sal-application/argument-list dnf2))))

(define-method (sal-dnf/and (dnf1 <sal-expr>) (dnf2 <sal-and>))
  ;; Remark I cannot implement this method as (sal-dnf/and dnf2 dnf1), since
  ;; It will not preserve the order of the conjuncts.
  ;; The same argument is applicable to other methods of this generic
  (apply make-sal-and (cons dnf1 (sal-application/argument-list dnf2))))

(define-method (sal-dnf/and (dnf1 <sal-and>) (dnf2 <sal-and>))
  (apply make-sal-and (append (sal-application/argument-list dnf1)
                              (sal-application/argument-list dnf2))))

(define-generic (sal-dnf/or dnf1 dnf2))

(define-method (sal-dnf/or (dnf1 <sal-expr>) (dnf2 <sal-expr>))
  (make-sal-or dnf1 dnf2))

(define-method (sal-dnf/or (dnf1 <sal-or>) (dnf2 <sal-expr>))
  (apply make-sal-or (append (sal-application/argument-list dnf1) 
                             (list dnf2))))

(define-method (sal-dnf/or (dnf1 <sal-expr>) (dnf2 <sal-or>))
  (apply make-sal-or (cons dnf1 (sal-application/argument-list dnf2))))

(define-method (sal-dnf/or (dnf1 <sal-or>) (dnf2 <sal-or>))
  (apply make-sal-or (append (sal-application/argument-list dnf1)
                             (sal-application/argument-list dnf2))))

(define-generic (sal-dnf/not dnf))

(define-method (sal-dnf/not (dnf <sal-expr>))
  (make-sal-not dnf))

(define-method (sal-dnf/not (dnf <sal-and>))
  (apply make-sal-or (map sal-dnf/not (sal-application/argument-list dnf))))

(define-method (sal-dnf/not (dnf <sal-or>))
  (apply make-sal-or (iterator->list
                      (apply
                       iterator/product
                       (lambda exprs
                         (apply make-sal-and exprs))
                       (map
                        (lambda (expr)
                          (cond
                           ((instance-of? expr <sal-and>)
                            (make-list-iterator (sal-application/argument-list (sal-dnf/not expr))))
                           (else 
                            (make-list-iterator (list (sal-dnf/not expr))))))
                        (sal-application/argument-list dnf))))))

(define-generic (sal-dnf/for-each-cube dnf proc))

(define-method (sal-dnf/for-each-cube (dnf <sal-expr>) (proc <primitive>))
  (proc dnf))

(define-method (sal-dnf/for-each-cube (dnf <sal-or>) (proc <primitive>))
  (for-each proc (sal-application/argument-list dnf)))

(define-generic (sal-cube/for-each-literal cube proc))

(define-method (sal-cube/for-each-literal (cube <sal-expr>) (proc <primitive>))
  (proc cube))

(define-method (sal-cube/for-each-literal (cube <sal-and>) (proc <primitive>))
  (for-each proc (sal-application/argument-list cube)))

(define (sal-expr->dnf ast . pred?)
  (let ((pred? (optional-arg pred? (lambda (_) #t))))
    (sal-ast/simplify (sal-expr->dnf-core ast pred?))))

(define-generic (sal-expr->dnf-core ast pred?))

(define-method (sal-expr->dnf-core :around (ast <sal-expr>) (pred? <primitive>))
  (if (pred? ast)
    (call-next-method)
    ast))

(define-method (sal-expr->dnf-core (ast <sal-expr>) (pred? <primitive>))
  ast)

(define-method (sal-expr->dnf-core (ast <sal-conditional>) (pred? <primitive>))
  (let ((cond-expr (sal-expr->dnf-core (slot-value ast :cond-expr) pred?))
        (then-expr (sal-expr->dnf-core (slot-value ast :then-expr) pred?))
        (else-expr (sal-expr->dnf-core (slot-value ast :else-expr) pred?)))
    (sal-dnf/or
     (sal-dnf/and cond-expr then-expr)
     (sal-dnf/and (sal-dnf/not cond-expr) else-expr))))

(define-method (sal-expr->dnf-core (ast <sal-and>) (pred? <primitive>))
  (let ((args (sal-application/argument-list ast)))
    (fold-left (lambda (result arg)
                 (sal-dnf/and result (sal-expr->dnf-core arg pred?)))
               (sal-expr->dnf-core (car args) pred?)
               (cdr args))))

(define-method (sal-expr->dnf-core (ast <sal-or>) (pred? <primitive>))
  (let ((args (sal-application/argument-list ast)))
    (fold-left (lambda (result arg)
                 (sal-dnf/or result (sal-expr->dnf-core arg pred?)))
               (sal-expr->dnf-core (car args) pred?)
               (cdr args))))

(define-method (sal-expr->dnf-core (ast <sal-eq>) (pred? <primitive>))
  (multiple-value-bind
      (arg1 arg2)
      (sal-binary-application/arguments ast)
    (if (sal-type/boolean? (sal-expr/type arg1))
      (let ((expanded-iff (make-sal-or (make-sal-and arg1 arg2)
                                       (make-sal-and (make-sal-not arg1) (make-sal-not arg2)))))
        (sal-expr->dnf-core expanded-iff pred?))
      (call-next-method))))

(define-method (sal-expr->dnf-core (ast <sal-xor>) (pred? <primitive>))
  (multiple-value-bind
      (arg1 arg2)
      (sal-binary-application/arguments ast)
    (if (sal-type/boolean? (sal-expr/type arg1))
      (let ((expanded-xor (make-sal-or (make-sal-and (make-sal-not arg1) arg2)
                                       (make-sal-and arg1 (make-sal-not arg2)))))
        (sal-expr->dnf-core expanded-xor pred?))
      (call-next-method))))

(define-method (sal-expr->dnf-core (ast <sal-implies>) (pred? <primitive>))
  (multiple-value-bind
      (arg1 arg2)
      (sal-binary-application/arguments ast)
    (sal-expr->dnf-core (make-sal-or (make-sal-not arg1) arg2) pred?)))

(define-method (sal-expr->dnf-core (ast <sal-not>) (pred? <primitive>))
  (sal-dnf/not (sal-expr->dnf-core (slot-value ast :arg) pred?)))

(define-generic (sal-dnf/cube-list expr))
(define-method (sal-dnf/cube-list (expr <sal-expr>))
  (list expr))
(define-method (sal-dnf/cube-list (expr <sal-or>))
  (sal-application/argument-list expr))

(define-generic (sal-cube/literal-list expr))
(define-method (sal-cube/literal-list (expr <sal-expr>))
  (list expr))
(define-method (sal-cube/literal-list (expr <sal-and>))
  (sal-application/argument-list expr))


