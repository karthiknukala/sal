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

(module scm2sal
        (include "sal.sch")
        (import sal2scm-core gmp-scheme sal-expression sal-type 
                sal-ast-simplify sal2scm-runtime)
        (export (scm-value->sal value type ctx))
        )

(define-generic (scm-value->sal value type ctx))

(define-method (scm-value->sal (value <primitive>) (type <sal-scalar-type>) (ctx <sal-scm-context>))
  (list-ref (slot-value type :scalar-elements) value))

(define-method (scm-value->sal (value <primitive>) (type <sal-number-type>) (ctx <sal-scm-context>))
  (make-ast-instance <sal-numeral> type
                     :num (object->mpq value)))

(define-method (scm-value->sal (value <primitive>) (type <sal-subrange>) (ctx <sal-scm-context>))
  (make-ast-instance <sal-numeral> type
                     :num (object->mpq value)))

(define-method (scm-value->sal (value <primitive>) (type <sal-subtype>) (ctx <sal-scm-context>))
  (scm-value->sal value (sal-subtype/immediate-super-type type) ctx))

(define-method (scm-value->sal (value <primitive>) (type <sal-bool-type>) (ctx <sal-scm-context>))
  (if value
    (make-sal-true type)
    (make-sal-false type)))

(define-method (scm-value->sal (value <primitive>) (type <sal-type-name>) (ctx <sal-scm-context>))
  (let ((definition (sal-type-name/definition type)))
    (if definition
      (scm-value->sal value definition ctx)
      (sign-error "Invalid uninterpreted type in scm-value->sal."))))

(define-method (scm-value->sal (value <primitive>) (type <sal-tuple-type>) (ctx <sal-scm-context>))
  (make-ast-instance <sal-tuple-literal> type
                     :exprs (map (lambda (child-value child-type)
                                   (scm-value->sal child-value child-type ctx))
                                 (vector->list value)
                                 (slot-value type :types))))

(define-method (scm-value->sal (value <primitive>) (type <sal-record-type>) (ctx <sal-scm-context>))
  (make-ast-instance <sal-record-literal> type
                     :entries (map (lambda (child-value field)
                                     (make-ast-instance <sal-record-entry> type
                                                        :id (slot-value field :id)
                                                        :expr (scm-value->sal child-value (slot-value field :type) ctx)))
                                   (vector->list value)
                                   (sal-record-type/sorted-fields type))))

(define-method (scm-value->sal (value <primitive>) (type <sal-data-type>) (ctx <sal-scm-context>))
  (let ((constructor (list-ref (slot-value type :constructors) (car value))))
    (if (sal-name-expr/constant-constructor? constructor)
      constructor
      (let ((accessors (sal-name-expr/constructor-accessors constructor)))
        (make-ast-instance <sal-application> type
                           :fun constructor
                           :arg (apply make-application-argument 
                                       (map (lambda (child-value accessor)
                                              (scm-value->sal child-value (sal-name-expr/accessor-type accessor) ctx))
                                            (cdr value)
                                            accessors)))))))

(define-generic (scm-value->sal-array value index-type type ctx))

(define (scm-value->sal-array-core value index-type type ctx make-body-proc)
  (let* ((id (make-ast-instance <sal-identifier> type
                                :name 'idx))
         (decl (make-ast-instance <sal-var-decl> type
                                  :id id
                                  :type (sal-function-type/domain type)))
         (name-expr (make-sal-name-expr decl type)))
    (make-ast-instance <sal-lambda> type
                       :local-decls (list decl)
                       :expr (make-body-proc name-expr))))

(define-method (scm-value->sal-array (value <primitive>) (index-type <sal-scalar-type>) (type <sal-function-type>) (ctx <sal-scm-context>))
  (scm-value->sal-array-core value index-type type ctx
                             (lambda (name-expr)
                               (let loop ((scalar-elements (slot-value index-type :scalar-elements))
                                          (idx 0))
                                 (let ((sal-value (scm-value->sal (vector-ref value idx) (slot-value type :range) ctx)))
                                   (if (null? (cdr scalar-elements)) ;; is the last scalar element?
                                     sal-value
                                     (make-ast-instance <sal-conditional> type
                                                        :cond-expr (make-sal-equality name-expr (car scalar-elements))
                                                        :then-expr sal-value
                                                        :else-expr (loop (cdr scalar-elements)
                                                                         (+ idx 1)))))))))

(define-method (scm-value->sal-array (value <primitive>) (index-type <sal-type-name>) (type <sal-function-type>) (ctx <sal-scm-context>))
  (let ((definition (sal-type-name/definition index-type)))
    (if definition
      (scm-value->sal-array value definition type ctx)
      (internal-error))))

(define-method (scm-value->sal-array (value <primitive>) (index-type <sal-subrange>) (type <sal-function-type>) (ctx <sal-scm-context>))
  (let* ((lower (slot-value index-type :lower))
         (len (vector-length value)))
    (scm-value->sal-array-core value index-type type ctx
                               (lambda (name-expr)
                                 (let loop ((idx 0))
                                   (let ((sal-value (scm-value->sal (vector-ref value idx) (slot-value type :range) ctx)))
                                     (if (< idx (- len 1))
                                       (make-ast-instance <sal-conditional> type
                                                          :cond-expr (make-sal-equality name-expr 
                                                                                        (make-simplified-sal-builtin-application 
                                                                                         <sal-add> type
                                                                                         lower
                                                                                         (make-ast-instance <sal-numeral> type
                                                                                                            :num (integer->mpq idx))))
                                                          :then-expr sal-value
                                                          :else-expr (loop (+ idx 1)))
                                       sal-value)))))))

(define (constant-vector? vect)
  (let ((val (vector-ref vect 0))
        (len (vector-length vect)))
    (let loop ((i 1))
      (if (< i len)
        (if (equal? (vector-ref vect i) val)
          (loop (+ i 1))
          #f)
        #t))))

(define-method (scm-value->sal-array :around (value <primitive>) (index-type <sal-type>) (type <sal-function-type>) (ctx <sal-scm-context>))
  (if (constant-vector? value)
    (scm-value->sal-array-core value index-type type ctx
                               (lambda (name-expr)
                                 (scm-value->sal (vector-ref value 0) (slot-value type :range) ctx)))
    (call-next-method)))

(define-method (scm-value->sal-array :around (value <primitive>) (index-type <sal-type>) (type <sal-array-type>) (ctx <sal-scm-context>))
  (quick-change-class! (call-next-method) <sal-array-literal>))

(define-method (scm-value->sal (value <primitive>) (type <sal-function-type>) (ctx <sal-scm-context>))
  (let ((domain (sal-function-type/domain type)))
    (if (procedure? value)
      (sign-error "Scheme function cannot be translated a SAL function. This feature is not supported.")
      (scm-value->sal-array value domain type ctx))))



  
