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

(module sal-ast-attributes
        (include "sal.sch")
        (include "fast-hash-table.sch")
        (export (make-sal-ast-attribute-table . args)
                (sal-ast/set-attribute! ast attr-id value)
                (sal-ast/attribute ast attr-id))
        )

(define (make-table-key ast attribute-id)
  (cons ast attribute-id))

(define (table-key/hash key)
  (+ (obj/hash (car key))
     (* 3 (obj/hash (cdr key)))))

(define (table-key/eq? key1 key2)
  (and (eq? (car key1) (car key2))
       (eq? (cdr key1) (cdr key2))))

(make-fast-hash-table-type sal-ast-attribute-table table-key/hash table-key/eq?)

(define (sal-ast/attribute-table ast)
  (slot-value (sal-ast/sal-env ast) :ast-attributes))

(define-api (sal-ast/set-attribute! (ast <sal-ast>) (attr-id keyword?) value)
  (sal-ast-attribute-table/put! (sal-ast/attribute-table ast) (make-table-key ast attr-id) value))

(define-api (sal-ast/attribute (ast <sal-ast>) (attr-id keyword?))
  (let ((result (sal-ast-attribute-table/get (sal-ast/attribute-table ast) (make-table-key ast attr-id))))
    (and result (cdr result))))
