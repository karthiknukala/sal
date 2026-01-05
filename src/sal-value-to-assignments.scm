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

(module sal-value-to-assignments
        (include "sal.sch")
;; BD:
;;	(import sal-pp)
        (import queue sal-expression sal-ast-eq iterators sal-expr-evaluator sal-type)
        (export (sal-value->assignments expr lhs)
                (sal-value->assignments-core expr prev-expr curr-lhs queue))
        )

(define-generic (sal-value->assignments expr lhs))

(define-method (sal-value->assignments (expr <sal-expr>) (lhs <sal-expr>))
  (let ((result (make-queue)))
    (sal-value->assignments-core expr #f lhs result)
    (queue->list result)))

(define-generic (sal-value->assignments-core expr prev-expr curr-lhs queue))

(define-method (sal-value->assignments-core (expr <sal-ast>) (prev-expr <primitive>) (curr-lhs <sal-expr>) (queue <primitive>))
  (queue/insert! queue (make-sal-equality curr-lhs expr)))

(define-method (sal-value->assignments-core (expr <sal-ast>) (prev-expr <sal-ast>) (curr-lhs <sal-expr>) (queue <primitive>))
  (unless (sal-ast/equivalent? expr prev-expr)
    (call-next-method)))

(define-method (sal-value->assignments-core (expr <sal-tuple-literal>) (prev-expr <primitive>) (curr-lhs <sal-expr>) (queue <primitive>))
  (let ((idx 1))
    (for-each (lambda (expr-child)
                (let ((child-lhs (make-ast-instance <sal-tuple-selection> curr-lhs
                                                    :target curr-lhs
                                                    :idx (make-sal-numeral idx curr-lhs))))
                  (sal-value->assignments-core expr-child #f child-lhs queue))
                (set! idx (+ idx 1)))
              (slot-value expr :exprs))))

(define-method (sal-value->assignments-core (expr <sal-tuple-literal>) (prev-expr <sal-tuple-literal>) (curr-lhs <sal-expr>) (queue <primitive>))
  (let ((idx 1))
    (for-each (lambda (expr-child prev-expr-child)
                (unless (sal-ast/equivalent? expr-child prev-expr-child)
                  (let ((child-lhs (make-ast-instance <sal-tuple-selection> curr-lhs
                                                      :target curr-lhs
                                                      :idx (make-sal-numeral idx curr-lhs))))
                    (sal-value->assignments-core expr-child prev-expr-child child-lhs queue)))
                (set! idx (+ idx 1)))
              (slot-value expr :exprs)
              (slot-value prev-expr :exprs))))

(define-method (sal-value->assignments-core (expr <sal-record-literal>) (prev-expr <primitive>) (curr-lhs <sal-expr>) (queue <primitive>))
  (for-each (lambda (entry)
              (let ((child-lhs (make-ast-instance <sal-record-selection> entry
                                                  :target curr-lhs
                                                  :idx (slot-value entry :id))))
                (sal-value->assignments-core (slot-value entry :expr) #f child-lhs queue)))
            (slot-value expr :entries)))

(define-method (sal-value->assignments-core (expr <sal-record-literal>) (prev-expr <sal-record-literal>) (curr-lhs <sal-expr>) (queue <primitive>))
  (for-each (lambda (entry prev-entry)
              [assert (entry prev-entry) (sal-ast/equivalent? (slot-value entry :id) (slot-value prev-entry :id))]
              (unless (sal-ast/equivalent? entry prev-entry)
                (let ((child-lhs (make-ast-instance <sal-record-selection> entry
                                                    :target curr-lhs
                                                    :idx (slot-value entry :id))))
                  (sal-value->assignments-core (slot-value entry :expr) (slot-value prev-entry :expr) child-lhs queue))))
            (slot-value expr :entries)
            (slot-value prev-expr :entries)))

(define (sal-function->assignments expr curr-lhs queue class call-next-method)
;;  (print "--- sal-function->assignment: ")
;;  (display* "expr is ")(sal/pp expr)(newline)
;;  (display* "curr-lhs is ")(sal/pp curr-lhs)(newline)
;;  (breakpoint "sal-function->assignments" (expr curr-lhs class queue) #t)
;;  (display* "expr-type is ")(sal/pp (sal-expr/type expr))(newline)
;;  (print "---")
  (let* ((expr-type (sal-expr/type expr))
         (domain (sal-function-type/domain expr-type)))
    (if (sal-type/finite? domain)
      (try
       (let ((it (sal-type/make-iterator domain)))
         (iterator/for-each (lambda (domain-value)
                              (let ((range-value (sal-expr/evaluate (sal-expr/apply expr domain-value)))
                                    (child-lhs (make-ast-instance class expr
                                                                  :fun curr-lhs
                                                                  :arg domain-value)))
                                (sal-value->assignments-core range-value #f child-lhs queue)))
                            it))
       (catch* '(type-iterator expr-evaluator)
               (lambda (_ __)
                 (call-next-method))))
      (queue/insert! queue (make-sal-equality curr-lhs expr)))))
    
(define-method (sal-value->assignments-core (expr <sal-array-literal>) (prev-expr <primitive>) (curr-lhs <sal-expr>) (queue <primitive>))
  (sal-function->assignments expr curr-lhs queue <sal-array-selection> call-next-method))

(define-method (sal-value->assignments-core (expr <sal-lambda>) (prev-expr <primitive>) (curr-lhs <sal-expr>) (queue <primitive>))
  (sal-function->assignments expr curr-lhs queue <sal-application> call-next-method))

(define (sal-function->delta-assignments expr prev-expr curr-lhs queue class call-next-method)
  (let* ((expr-type (sal-expr/type expr))
         (domain (sal-function-type/domain expr-type)))
    (if (sal-type/finite? domain)
      (try
       (let ((it (sal-type/make-iterator domain)))
         (iterator/for-each (lambda (domain-value)
                              (let ((range-value (sal-expr/evaluate (sal-expr/apply expr domain-value)))
                                    (prev-range-value (sal-expr/evaluate (sal-expr/apply prev-expr domain-value))))
                                (unless (sal-ast/equivalent? range-value prev-range-value)
                                  (let ((child-lhs (make-ast-instance class expr
                                                                      :fun curr-lhs
                                                                      :arg domain-value)))
                                    (sal-value->assignments-core range-value prev-range-value child-lhs queue)))))
                            it))
       (catch* '(type-iterator expr-evaluator)
               (lambda (_ __)
                 (call-next-method))))
      (unless (sal-ast/equivalent? expr prev-expr)
        (queue/insert! queue (make-sal-equality curr-lhs expr))))))
   
(define-method (sal-value->assignments-core (expr <sal-array-literal>) (prev-expr <sal-array-literal>) (curr-lhs <sal-expr>) (queue <primitive>))
  (sal-function->delta-assignments expr prev-expr curr-lhs queue <sal-array-selection> call-next-method))

(define-method (sal-value->assignments-core (expr <sal-lambda>) (prev-expr <sal-lambda>) (curr-lhs <sal-expr>) (queue <primitive>))
  (sal-function->delta-assignments expr prev-expr curr-lhs queue <sal-application> call-next-method))
