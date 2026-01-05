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


(define (encapsulated? expr tag len)
  (let ((constructor (slot-value expr :fun)))
    (and (instance-of? constructor <sal-name-expr>)
         (eq? (sal-name-ref/name constructor) tag)
         (= (length (sal-application/argument-list expr)) len))))

(define (sal-queue? expr)
  (encapsulated? expr 'mk-queue 2))

(define (pp-queue queue-expr pp-info depth pp-child-proc)
  (try
   (let* ((queue (slot-value queue-expr :arg))
          (queue-body (sal-tuple-literal/element queue 1))
          (queue-size (sal-tuple-literal/element queue 2))
          ;; assuming that queue-size is a <sal-numeral>
          (n (mpq->integer (slot-value queue-size :num)))
          ;; we need a place provider to create AST objects
          (place-provider queue-expr) 
          (doc-queue (make-queue))
          (depth (+ depth 1)))
     (let loop ((i 0))
       (when (< i n)
         (let* ((queue-elem (sal-expr/evaluate (sal-array-literal/element queue-body (make-sal-numeral i place-provider))))
                (queue-elem-doc (pp-child-proc queue-elem pp-info depth)))
           (queue/insert! doc-queue queue-elem-doc))
         (loop (+ i 1))))
     (apply pp/list-style6-core "[" "]" (queue->list doc-queue)))
   (lambda (escape proc msg obj)
     ;; failed to pretty print
     (escape #f))))

(define (pp-constructor-application expr pp-info depth pp-child-proc call-next-method)
  (or 
   (and (sal-queue? expr) (pp-queue expr pp-info depth pp-child-proc))
   (call-next-method)))

(define-method (sal-ast->sal-doc :around (expr <sal-constructor-application>) (pp-info <pp-info++>) (depth <primitive>))
  (pp-constructor-application expr pp-info depth sal-ast->sal-doc call-next-method))

(define-method (sal-ast->lsal-doc :around (expr <sal-constructor-application>) (pp-info <pp-info++>) (depth <primitive>))
  (pp-constructor-application expr pp-info depth sal-ast->lsal-doc call-next-method))

(define (sal-predicate? expr)
  (and (not (instance-of? expr <sal-array-literal>))
       (let* ((expr-type (sal-expr/type expr))
              (range-type (sal-function-type/range expr-type)))
         (sal-type/boolean? range-type))))
         
(define (pp-predicate-as-set pred pp-info depth pp-child-proc)
  (try
   (let* ((pred-type (sal-expr/type pred))
          (domain (sal-function-type/domain pred-type))
          (it (sal-type/make-iterator domain))
          (doc-queue (make-queue)))
     (bind-exit (exit)
       (iterator/for-each (lambda (domain-value)
                            (let ((range-value (sal-expr/evaluate (sal-expr/apply pred domain-value))))
                              (cond 
                               ((sal-expr/true? range-value)
                                (let ((domain-doc (pp-child-proc domain-value pp-info depth)))
                                  (queue/insert! doc-queue domain-doc)))
                               ((sal-expr/false? range-value)
                                ;; do nothing
                                #unspecified)
                               (else
                                ;; symbolic value ==> abort
                                (exit #f)))))
                          it)
       (apply pp/list-style6-core "{" "}" (queue->list doc-queue))))
   (catch* '(type-iterator expr-evaluator)
           (lambda (_ __)
             (breakpoint "set" (expr-type pred-type) #t)
             ;; iterator or evaluator failed --> abort
             #f))))

(define (pp-lambda expr pp-info depth pp-child-proc call-next-method)
  (or 
   (and (sal-predicate? expr) (pp-predicate-as-set expr pp-info depth pp-child-proc))
   (call-next-method)))

(define-method (sal-ast->sal-doc :around (expr <sal-lambda>) (pp-info <pp-info++>) (depth <primitive>))
  (pp-lambda expr pp-info depth sal-ast->sal-doc call-next-method))

(define-method (sal-ast->lsal-doc :around (expr <sal-lambda>) (pp-info <pp-info++>) (depth <primitive>))
  (pp-lambda expr pp-info depth sal-ast->lsal-doc call-next-method))

;; do not break predicates in pieces in pieces
(define-method (sal-value->assignments :around (expr <sal-lambda>) (prev-expr <sal-lambda>) (curr-lhs <sal-expr>) (queue <primitive>))
  (if (sal-predicate? expr)
    (unless (sal-ast/equivalent? expr prev-expr)
      (queue/insert! queue (make-sal-equality curr-lhs expr)))
    (call-next-method)))

(define-method (sal-value->assignments :around (expr <sal-lambda>) (prev-expr <primitive>) (curr-lhs <sal-expr>) (queue <primitive>))
  (if (sal-predicate? expr)
    (queue/insert! queue (make-sal-equality curr-lhs expr))
    (call-next-method)))
    
     
                          


       
