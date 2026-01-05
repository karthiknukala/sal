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

(module sal2scm-core
        (include "sal.sch")
        (import queue sal-ast-table sal-type sal-expression)
        (export *sal-scm-max-vector-size*
                (sign-sal-to-scm-error node error-msg . args)
                (sal-scm/set-max-vector-size! num)
                <sal-scm-context>
                (sal-scm-context/init! ctx)
                (sal-scm-context/add-decl! ctx code)
                (sal->scm ast ctx env)
                (sal-data-type/max-constructor-size data-type)
                (sal-constructor/max-constructor-size constructor)
                (sal-accessor/max-constructor-size accessor))
        )

(define *sal-scm-max-vector-size* 65536)

(define-api (sal-scm/set-max-vector-size! num)
  :doc "Set the maximum vector size that can be used."
  (set! *sal-scm-max-vector-size* num))

(define (sign-sal-to-scm-error node error-msg . args)
  (sign-sal-error 'sal-to-scm-error node error-msg args))

(define-class <sal-scm-context> () 
  (:global-function-decl-table ;; maps qualified-name-exprs -> symbol (i.e., reference to a global scheme function)
   :global-constant-decl-table ;; maps qualified-name-exprs -> symbol (i.e., reference to a global scheme variable)
   :idx-val-table              ;; maps (cache) SAL types -> idx->val procedures
   :val-idx-table              ;; maps (cache) SAL types -> val->idx procedures
   :scm-decl-queue             ;; list of global scheme definitions
   :gmp?                       ;; #t when GMP is used to represent SAL numerals
   :runtime-type-check?        ;; #t when runtime type checking code must be included in the generated code
   :debug?                     ;; #t when debugging primitives should be included in the generated code
   :loop-detection?            ;; #t when loop detection code is included in the generated code
   :max-vector-size            ;; maximum vector size that can be created by the generated code
   :compile?                   ;; #t when the code should be compiled
   
   :type-check-table    
   :pick-random-table))

(define-generic (sal-scm-context/add-decl! ctx code))

(define-method (sal-scm-context/add-decl! (ctx <sal-scm-context>) (code <primitive>))
  (queue/insert! (slot-value ctx :scm-decl-queue) code))

(define (sal-scm-context/init! ctx)
  (set-slot-value! ctx :global-function-decl-table (make-sal-ast-table))
  (set-slot-value! ctx :global-constant-decl-table (make-sal-ast-table))
  (set-slot-value! ctx :type-check-table (make-sal-ast-table))
  (set-slot-value! ctx :pick-random-table (make-sal-ast-table))
  (set-slot-value! ctx :idx-val-table (make-sal-ast-table))
  (set-slot-value! ctx :val-idx-table (make-sal-ast-table))
  (set-slot-value! ctx :scm-decl-queue (make-queue))
  (set-slot-value! ctx :max-vector-size *sal-scm-max-vector-size*))

(define-generic (sal->scm ast ctx env))

(define (sal-data-type/max-constructor-size data-type)
  (list-max sal-constructor/num-expected-args
            (slot-value data-type :constructors)))

(define (sal-constructor/max-constructor-size constructor)
  (sal-data-type/max-constructor-size (slot-value (slot-value (slot-value constructor :decl) :data-type-decl) :type)))
        
(define (sal-accessor/max-constructor-size accessor)
  (sal-data-type/max-constructor-size (slot-value (slot-value (slot-value (slot-value accessor :decl) :constructor-decl) :data-type-decl) :type)))
