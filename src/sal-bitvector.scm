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

(module sal-bitvector
        (include "sal.sch")
        (import sal-finite-expressions sal-finite-types gmp-scheme sal-type 
                sal-expression sal-ast-expand)
        (export <tvv2nat> <nat2tvv> <tvv-concat> <tvv-application>)
        )

(define-class <tvv-application> (<sal-application>) ())
(define-class <tvv2nat> (<tvv-application> <sal-unary-application>) ())
(define-class <nat2tvv> (<tvv-application> <sal-unary-application>) ())
(define-class <tvv-concat> (<tvv-application> <sal-binary-application>) ())

;----------------------------------------------------------------
;  
; expr -> bool expr
;
; Support for TransEDA operators... 
;
;----------------------------------------------------------------

(define-method (sal-expr->boolean-expr-core (app <tvv2nat>) (env <primitive>) (aux-decl-queue <primitive>))
  (let* ((arg (slot-value app :arg))
         (arg-bit-list (sal-expr->boolean-expr-core arg env aux-decl-queue))
         (num-bits (length arg-bit-list))
         (result-bit-list (reverse arg-bit-list))
         (place-provider app)
         (lower (make-sal-numeral 0 place-provider))
         (upper-num (-mpq (mpq/exp *mpq-two* (integer->mpq num-bits)) *mpq-one*))
         (upper (make-sal-numeral upper-num place-provider))
         (result-type (make-sal-subrange lower upper)))
    (values result-bit-list result-type)))

(define-method (sal-expr->boolean-expr-core (app <nat2tvv>) (env <primitive>) (aux-decl-queue <primitive>))
  (let* ((arg (slot-value app :arg))
         (result-type (sal-expr/type app))
         (num-bits (sal-type/finite-rep-num-bits result-type))
         (arg-bit-list (sal-expr->boolean-expr-core arg env aux-decl-queue))
         (tmp-bit-list (force-bit-list-size arg-bit-list num-bits app))
         (result-bit-list (reverse tmp-bit-list)))
    (values result-bit-list result-type)))

(define-method (sal-expr->boolean-expr-core (app <tvv-concat>) (env <primitive>) (aux-decl-queue <primitive>))
  (multiple-value-bind
      (arg1 arg2)
      (sal-binary-application/arguments app)
    (let* ((arg1-bit-list (sal-expr->boolean-expr-core arg1 env aux-decl-queue))
           (arg2-bit-list (sal-expr->boolean-expr-core arg2 env aux-decl-queue))
           (result-type (sal-expr/type app))
           (result-bit-list (append arg1-bit-list arg2-bit-list)))
      (values result-bit-list result-type))))

(define-method (sal-ast/expand-core (ast <tvv-application>) (env <primitive>) (depth <primitive>))
  (sal-expander/maintain-app ast env depth))
