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

(module polarity
        (include "sal.sch")
        (import sal-ast-simplify sal-ast-copy)
        (export <polarity> <pos> <neg> <pos-neg>
                *pos* *neg* *pos-neg*
                (polarity/invert pol)
                (sal-ast/map-using-polarity ast env polarity proc))
        )

(define-class <polarity> () ())
(define-class <pos> (<polarity>) ())
(define-class <neg> (<polarity>) ())
(define-class <pos-neg> (<polarity>) ())

(define *pos* (make-instance <pos>))
(define *neg* (make-instance <neg>))
(define *pos-neg* (make-instance <pos-neg>))

(define-generic (polarity/invert pol))
(define-method (polarity/invert (pol <pos>)) *neg*)
(define-method (polarity/invert (pol <neg>)) *pos*)
(define-method (polarity/invert (pol <pos-neg>)) pol)

(define-generic (sal-ast/map-using-polarity ast env polarity proc))

(define (eliminate-quantifiers-default ast env polarity proc)
  (sal-ast/map ast env (lambda (child-ast new-env) (proc child-ast new-env polarity))))

(define (eliminate-quantifiers-switching-to-pos-neg ast env proc)
  (sal-ast/map ast env (lambda (child-ast new-env) (proc child-ast new-env *pos-neg*))))

(define-method (sal-ast/map-using-polarity (ast <sal-ast>) (env <primitive>) (polarity <polarity>) (proc <primitive>))
  (eliminate-quantifiers-default ast env polarity proc))

(define-method (sal-ast/map-using-polarity (ast <sal-expr>) (env <primitive>) (polarity <polarity>) (proc <primitive>))
  ;; the default behavior is to prevent skolemization...
  (eliminate-quantifiers-switching-to-pos-neg ast env proc))

(define-method (sal-ast/map-using-polarity (ast <sal-propositional-application>) (env <primitive>) (polarity <polarity>) (proc <primitive>))
  (sal-ast/local-simplify-core ast env (lambda (child-ast new-env) (proc child-ast new-env polarity))))

(define-method (sal-ast/map-using-polarity (ast <sal-eq>) (env <primitive>) (polarity <polarity>) (proc <primitive>))
  (eliminate-quantifiers-switching-to-pos-neg ast env proc))

(define-method (sal-ast/map-using-polarity (ast <sal-diseq>) (env <primitive>) (polarity <polarity>) (proc <primitive>))
  (eliminate-quantifiers-switching-to-pos-neg ast env proc))

(define-method (sal-ast/map-using-polarity (ast <sal-not>) (env <primitive>) (polarity <polarity>) (proc <primitive>))
  (eliminate-quantifiers-default ast env (polarity/invert polarity) proc))

(define-method (sal-ast/map-using-polarity (ast <sal-implies>) (env <primitive>) (polarity <polarity>) (proc <primitive>))
  ;; convert implication A => B in ((NOT A) OR B)
  (sal-ast/map-using-polarity (sal-ast/local-simplify ast) env polarity proc))

(define-method (sal-ast/map-using-polarity (ast <sal-conditional>) (env <primitive>) (polarity <polarity>) (proc <primitive>))
  (let ((new-cond-expr (proc (slot-value ast :cond-expr) env *pos-neg*))
        (new-then-expr (proc (slot-value ast :then-expr) env polarity))
        (new-else-expr (proc (slot-value ast :else-expr) env polarity)))
    (update-ast-slots ast
                      :cond-expr new-cond-expr
                      :then-expr new-then-expr
                      :else-expr new-else-expr)))

(define-method (sal-ast/map-using-polarity (ast <sal-let-decl>) (env <primitive>) (polarity <polarity>) (proc <primitive>))
  (eliminate-quantifiers-switching-to-pos-neg ast env proc))

(define-method (sal-ast/map-using-polarity (ast <sal-let-expr>) (env <primitive>) (polarity <polarity>) (proc <primitive>))
  (eliminate-quantifiers-default ast env polarity proc))

(define-method (sal-ast/map-using-polarity (ast <sal-quantified-expr>) (env <primitive>) (polarity <polarity>) (proc <primitive>))
  (eliminate-quantifiers-default ast env polarity proc))














