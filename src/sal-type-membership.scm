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

(module sal-type-membership
        (include "sal.sch")
        (import sal-type sal-module sal-expression sal-ast-simplify unique-names)
        (export (sal-type/nontrivial-membership-expr? type)
                (sal-type/membership-expr type expr))
        )
;;
;; In SAL, every type has an expression which must be satisfied by its members.
;; For instance, if "a" has type "[2..5]", then "2<=a<=5".
;; We say, that a type has trivial membership expr when it is equals to TRUE.
;; Examples of types that have trivial membership expressions: DATATYPES, SCALARTYPES,
;; and UNINTERPRETED TYPES.
;;

(define-generic (sal-type/nontrivial-membership-expr? type))

(define-method (sal-type/nontrivial-membership-expr? (type <sal-type-name>))
  (cond
   ((sal-type-name/definition type) =>
    sal-type/nontrivial-membership-expr?)
   (else
    ;; uninterpreted types have trivial membership expressions.
    #f)))

(define-method (sal-type/nontrivial-membership-expr? (type <sal-scalar-type>))
  #f)

(define-method (sal-type/nontrivial-membership-expr? (type <sal-data-type>))
  #f)

(define-method (sal-type/nontrivial-membership-expr? (type <sal-tuple-type>))
  (exists sal-type/nontrivial-membership-expr? (slot-value type :types)))

(define-method (sal-type/nontrivial-membership-expr? (type <sal-record-type>))
  (exists (lambda (field)
            (sal-type/nontrivial-membership-expr? (slot-value field :type)))
          (slot-value type :fields)))

(define-method (sal-type/nontrivial-membership-expr? (type <sal-function-type>))
  (sal-type/nontrivial-membership-expr? (slot-value type :range)))

(define-method (sal-type/nontrivial-membership-expr? (type <sal-state-type>))
  (sal-type/nontrivial-membership-expr? (sal-module/type (slot-value type :module))))

(define-method (sal-type/nontrivial-membership-expr? (type <sal-subtype>))
  #t)


(define-generic (sal-type/membership-expr type expr))

(define-method (sal-type/membership-expr :around (type <sal-type>) (expr <sal-expr>))
  (if (sal-type/nontrivial-membership-expr? type)
    (call-next-method)
    (make-sal-true expr)))

(define-method (sal-type/membership-expr (type <sal-type-name>) (expr <sal-expr>))
  (cond
   ((sal-type-name/definition type) =>
    (lambda (type-def)
      (sal-type/membership-expr type-def expr)))
   (else
    ;; uninterpreted types have trivial membership expressions.
    (make-sal-true expr))))

(define-method (sal-type/membership-expr (type <sal-tuple-type>) (expr <sal-expr>))
  (let ((conditions '())
        (place-provider expr))
    (let loop ((type-list (slot-value type :types))
               (idx 1))
      (unless (null? type-list)
        (let ((selection (make-ast-instance <sal-tuple-selection> place-provider
                                            :target expr
                                            :idx (make-sal-numeral idx place-provider))))
          (push! (sal-type/membership-expr (car type-list) selection) 
                 conditions)
          (loop (cdr type-list) (+ idx 1)))))
    (make-sal-and+* conditions place-provider)))

(define-method (sal-type/membership-expr (type <sal-record-type>) (expr <sal-expr>))
  (let ((conditions '())
        (place-provider expr))
    (let loop ((field-list (slot-value type :fields)))
      (unless (null? field-list)
        (let* ((curr-field (car field-list))
               (selection (make-ast-instance <sal-record-selection> place-provider
                                             :target expr
                                             :idx (slot-value curr-field :id))))
          (push! (sal-type/membership-expr (slot-value curr-field :type) selection)
                 conditions)
          (loop (cdr field-list)))))
    (make-sal-and+* conditions place-provider)))
  
(define-method (sal-type/membership-expr (type <sal-subtype>) (expr <sal-expr>))
  (let* ((pred (slot-value type :expr))
         (cond-expr (sal-expr/apply pred expr))
         (super-type (sal-subtype/immediate-super-type type))
         (super-type-expr (sal-type/membership-expr super-type expr)))
    (make-sal-and+ cond-expr super-type-expr)))

(define-method (sal-type/membership-expr (type <sal-subrange>) (expr <sal-expr>))
  (let ((place-provider expr))
    (make-sal-and (make-sal-builtin-application <sal-int-pred> place-provider expr)
                  (make-sal-builtin-application <sal-ge> place-provider expr (slot-value type :lower))
                  (make-sal-builtin-application <sal-le> place-provider expr (slot-value type :upper)))))

(define-method (sal-type/membership-expr (type <sal-state-type>) (expr <sal-expr>))
  (sal-type/membership-expr (sal-module/type (slot-value type :module)) expr))

(define-method (sal-type/membership-expr (type <sal-function-type>) (expr <sal-expr>))
  (let* ((range (slot-value type :range))
         (arity (sal-function-type/arity type))
         (arg-types (sal-function-type/domain-types type))
         (place-provider expr)
         (arg-names (generate-list (lambda (_) (gen-unique-name 'idx)) arity))
         (arg-ids (map (cut make-sal-identifier place-provider <>) arg-names))
         (arg-decls (map (lambda (arg-id arg-type)
                           (make-ast-instance <sal-var-decl> place-provider
                                              :id arg-id
                                              :type arg-type))
                         arg-ids
                         arg-types))
         (arg-name-exprs (map (lambda (decl)
                                (make-sal-name-expr decl place-provider))
                              arg-decls))
         (new-expr (sal-expr/apply expr (apply make-application-argument arg-name-exprs)))
         (body-expr (sal-type/membership-expr range new-expr)))
    (make-ast-instance <sal-for-all-expr> place-provider
                       :local-decls arg-decls
                       :expr body-expr)))
  


