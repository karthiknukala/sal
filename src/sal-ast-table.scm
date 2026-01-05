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

(module sal-ast-table
        (include "sal.sch")
        (include "fast-hash-table.sch")
        (import sal-ast-for-each gmp-scheme sal-ast-eq)
        (export (sal-ast/hash ast)
                (make-sal-ast-table . initial-size-and-load-factor)
                (sal-ast-table/rehash! ht)
                (sal-ast-table/put! ht k v)
                (sal-ast-table/contains? ht k)
                (sal-ast-table/get ht k)
                (sal-ast-table/delete! ht k)  
                (sal-ast-table/delete-all! ht)
                (sal-ast-table/size ht)
                (sal-ast-table/for-each proc ht)
                (sal-ast-table/for-each-key proc htable)
                (sal-ast-table/fold-keys proc init htable)
                (sal-ast-table/exists-key proc htable)
                (sal-ast-table/for-all-keys proc htable)
                (sal-ast-table/find-key proc htable)
                (sal/add-memoized-ast-method-cache-table! cache-table)
                (sal/reset-memoized-ast-method-cache-tables!))
        )

(define-generic (sal-ast/hash ast))

(define-method (sal-ast/hash (ast <sal-ast>))
  (unless (slot-value ast :hash)
    (set-slot-value! ast 
                     :hash (sal-ast/fold-children (lambda (hash ast)
                                                    (+ (* hash 3) (sal-ast/hash ast)))
                                                  (instance-class-idx ast)
                                                  ast)))
  (slot-value ast :hash))

(define-method (sal-ast/hash (ast <sal-numeral>))
  (mpq->integer (slot-value ast :num)))

(define-method (sal-ast/hash (ast <sal-identifier>))
  (obj/hash (slot-value ast :name)))

(define-method (sal-ast/hash (ast <sal-string-expr>))
  (obj/hash (slot-value ast :string)))

(define-method (sal-ast/hash (ast <sal-decl>))
  (instance-class-idx ast))

(define-method (sal-ast/hash (ast <sal-typed-decl>))
  (+ (instance-class-idx ast) 
     (sal-ast/hash (slot-value ast :type))))

(define-method (sal-ast/hash (ast <sal-const-decl>))
  (+ (instance-class-idx ast) 
     (sal-ast/hash (slot-value ast :type)) 
     (sal-ast/hash (slot-value ast :value))))

(define-inline (sal-name-ref/hash name-ref)
  (obj/hash (sal-name-ref/name name-ref)))

(define-method (sal-ast/hash (ast <sal-name-ref>))
  (obj/hash (sal-name-ref/name ast)))

(define-method (sal-ast/hash (ast <sal-subrange>))
  (+ (sal-ast/hash (slot-value ast :lower))
     (* 3 (sal-ast/hash (slot-value ast :upper)))))
     
; I dropped alpha equivalence... Maybe, in a future implementation, I will try to
; implement an efficient method that preserves alpha equivalence...
; (define-method (sal-ast/hash (ast <sal-name-expr>))
;   (if (instance-of? (slot-value ast :decl) <sal-state-var-decl>)
;     (obj/hash (sal-name-ref/name ast))
;     1))


(make-fast-hash-table-type sal-ast-table sal-ast/hash sal-ast/equivalent?)

(define *sal-memoized-ast-methods-cache-tables* '())

(define (sal/add-memoized-ast-method-cache-table! cache-table)
  (set! *sal-memoized-ast-methods-cache-tables* (cons cache-table *sal-memoized-ast-methods-cache-tables*)))

(define (sal/reset-memoized-ast-method-cache-tables!)
  (for-each sal-ast-table/delete-all! *sal-memoized-ast-methods-cache-tables*))
  



