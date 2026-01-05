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

(module sal-nnf
        (include "sal.sch")
        (import sal-expression sal-type polarity)
        (export (sal-expr->nnf expr . pred?))
        )

(define (sal-expr->nnf expr . pred?)
  (let ((pred? (optional-arg pred? (lambda (_) #t))))
    (sal-expr->nnf-core expr *pos* pred?)))

(define-generic (sal-expr->nnf-core expr polarity pred?))
(define-method (sal-expr->nnf-core :around (expr <sal-expr>) (polarity <pos>) (pred? <primitive>))
  (if (pred? expr)
    (call-next-method)
    expr))
(define-method (sal-expr->nnf-core :around (expr <sal-expr>) (polarity <neg>) (pred? <primitive>))
  (if (pred? expr)
    (call-next-method)
    (make-sal-not expr)))
(define-method (sal-expr->nnf-core (expr <sal-expr>) (polarity <pos>) (pred? <primitive>))
  expr)
(define-method (sal-expr->nnf-core (expr <sal-expr>) (polarity <neg>) (pred? <primitive>))
  (make-sal-not expr))
(define-method (sal-expr->nnf-core (expr <sal-arg-tuple-literal>) (polarity <primitive>) (pred? <primitive>))
  (update-ast-slots expr
                    :exprs (map (cut sal-expr->nnf-core <> polarity pred?) (slot-value expr :exprs))))

(define (app-default-nnf expr polarity pred?)
  (update-ast-slots expr 
                    :arg (sal-expr->nnf-core (slot-value expr :arg) polarity pred?)))

(define (app-dual-nnf expr polarity pred? dual-class)
  (let ((place-provider expr))
    (apply make-sal-builtin-application dual-class place-provider
           (map (lambda (arg)
                  (sal-expr->nnf-core arg polarity pred?))
                (sal-application/argument-list expr)))))

(define-macro (gen-simple-app-method class dual-class)
  `(begin
     (define-method (sal-expr->nnf-core (expr ,class) (polarity <pos>) (pred? <primitive>))
       (app-default-nnf expr polarity pred?))
     (define-method (sal-expr->nnf-core (expr ,class) (polarity <neg>) (pred? <primitive>))
       (app-dual-nnf expr polarity pred? ,dual-class))))
  
(gen-simple-app-method <sal-and> <sal-or>)
(gen-simple-app-method <sal-or> <sal-and>)
;; LTL duals
(gen-simple-app-method <sal-ltl-u> <sal-ltl-r>)
(gen-simple-app-method <sal-ltl-r> <sal-ltl-u>)
(gen-simple-app-method <sal-ltl-f> <sal-ltl-g>)
(gen-simple-app-method <sal-ltl-g> <sal-ltl-f>)
(gen-simple-app-method <sal-ltl-m> <sal-ltl-w>)
(gen-simple-app-method <sal-ltl-w> <sal-ltl-m>)
(gen-simple-app-method <sal-ltl-x> <sal-ltl-x>)
;; CTL duals
(gen-simple-app-method <sal-ctl-au> <sal-ctl-er>)
(gen-simple-app-method <sal-ctl-er> <sal-ctl-au>)
(gen-simple-app-method <sal-ctl-eu> <sal-ctl-ar>)
(gen-simple-app-method <sal-ctl-ar> <sal-ctl-eu>)
(gen-simple-app-method <sal-ctl-ag> <sal-ctl-ef>)
(gen-simple-app-method <sal-ctl-ef> <sal-ctl-ag>)
(gen-simple-app-method <sal-ctl-af> <sal-ctl-eg>)
(gen-simple-app-method <sal-ctl-eg> <sal-ctl-af>)
(gen-simple-app-method <sal-ctl-ax> <sal-ctl-ex>)
(gen-simple-app-method <sal-ctl-ex> <sal-ctl-ax>)

(define-method (sal-expr->nnf-core (expr <sal-not>) (polarity <pos>) (pred? <primitive>))
  (sal-expr->nnf-core (slot-value expr :arg) *neg* pred?))

(define-method (sal-expr->nnf-core (expr <sal-not>) (polarity <neg>) (pred? <primitive>))
  (sal-expr->nnf-core (slot-value expr :arg) *pos* pred?))

(define-macro (gen-quantifier-method class dual-class)
  `(begin
     (define-method (sal-expr->nnf-core (expr ,class) (polarity <pos>) (pred? <primitive>))
       (update-ast-slots expr 
                         :expr (sal-expr->nnf-core (slot-value expr :expr) polarity pred?)))
     (define-method (sal-expr->nnf-core (expr ,class) (polarity <neg>) (pred? <primitive>))
       (make-ast-instance ,dual-class expr
                          :local-decls (slot-value expr :local-decls)
                          :expr (sal-expr->nnf-core (slot-value expr :expr) polarity pred?)))))

(gen-quantifier-method <sal-for-all-expr> <sal-exists-expr>)
(gen-quantifier-method <sal-exists-expr> <sal-for-all-expr>)

(define-method (sal-expr->nnf-core (expr <sal-eq>) (polarity <polarity>) (pred? <primitive>))
  (multiple-value-bind
      (arg1 arg2)
      (sal-binary-application/arguments expr)
    (if (sal-type/boolean? (sal-expr/type arg1))
      (let ((expanded-iff (make-sal-or (make-sal-and arg1 arg2)
                                       (make-sal-and (make-sal-not arg1) (make-sal-not arg2)))))
        (sal-expr->nnf-core expanded-iff polarity pred?))
      (call-next-method))))

(define-method (sal-expr->nnf-core (expr <sal-diseq>) (polarity <polarity>) (pred? <primitive>))
  (multiple-value-bind
      (arg1 arg2)
      (sal-binary-application/arguments expr)
    (if (sal-type/boolean? (sal-expr/type arg1))
      (let ((expanded-xor (make-sal-or (make-sal-and (make-sal-not arg1) arg2)
                                       (make-sal-and arg1 (make-sal-not arg2)))))
        (sal-expr->nnf-core expanded-xor polarity pred?))
      (call-next-method))))

(define-method (sal-expr->nnf-core (expr <sal-implies>) (polarity <polarity>) (pred? <primitive>))
  (multiple-value-bind
      (arg1 arg2)
      (sal-binary-application/arguments expr)
    (sal-expr->nnf-core (make-sal-or (make-sal-not arg1) arg2) polarity pred?)))
