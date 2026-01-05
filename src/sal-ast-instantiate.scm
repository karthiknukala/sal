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

(module sal-ast-instantiate
        (include "sal.sch")
        (import symbol-table sal-ast-copy sal-context sal-ast-list sal-ast-support sal-ast-for-each sal-ast-env)
        (export (sal-ast/instantiate ast actuals)
                (sal-ast/instantiate-with ast env info)
                <sal-instantiation-info>)
        )

(define-class <sal-instantiation-info> () (:context :actuals :proc))

(define-api (sal-ast/instantiate (ast <sal-ast>) actuals)
  :doc "Instantiate @code{ast} using the actual types and expressions, that is, the type and variable parameters are replaced by the actual values. The number of actual types and actual expressions should be equal to the number of parameters of the context that \"owns\" @code{ast}."
  (let* ((context (slot-value ast :context))
         (params (sal-context/params context)))
    (trace 'instantiate "Current context = ~a" (sal-context/name context))
    [sal-assert "instantiation: number of actuals is correct" (params actuals) (= (length params) (length actuals))]
    (letrec ((proc (lambda (obj env)
                     (sal-ast/instantiate-with obj env info)))
             (info (make-instance <sal-instantiation-info>
                                  :context context
                                  :actuals actuals
                                  :proc proc)))
      (let ((result (sal-ast/instantiate-with ast (make-empty-env) info)))
        result))))

(define-generic (sal-ast/instantiate-with ast env info))

(define-method (sal-ast/instantiate-with (ast <sal-ast>) (env <primitive>) (info <sal-instantiation-info>))
  (sal-ast/map ast env (slot-value info :proc)))

(define (find-actual param param-list actual-list)
  (bind-exit (exit)
    (for-each (lambda (curr-param curr-actual)
                (when (eq? curr-param param)
                  (exit curr-actual)))
              param-list actual-list)
    (internal-error)))
                     
(define (expand-param ast actuals)
  (find-actual (slot-value ast :decl)
               (sal-context/params (slot-value ast :context))
               actuals))

(define-method (sal-ast/instantiate-with (ast <sal-type-param-name>) (env <primitive>) (info <sal-instantiation-info>))
  [assert (ast info) (eq? (sal-ast/context ast) (slot-value info :context))]
  (expand-param ast (slot-value info :actuals)))

(define-method (sal-ast/instantiate-with (ast <sal-var-param-name-expr>) (env <primitive>) (info <sal-instantiation-info>))
  [assert (ast info) (eq? (sal-ast/context ast) (slot-value info :context))]
  (expand-param ast (slot-value info :actuals)))



  
