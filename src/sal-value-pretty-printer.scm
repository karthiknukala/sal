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

(module sal-value-pretty-printer
        (include "sal.sch")
        (import pretty lsal-pretty-printer sal-pretty-printer queue sal-expression sal-expr-evaluator
                iterators sal-type)
        (export <pp-value-info++>)
        )

(define-class <pp-value-info++> (<pp-info++>) ())

(define-method (sal-ast->lsal-doc (ast <sal-array-literal>) (pp-info <pp-value-info++>) (depth <primitive>))
  (try
   (let* ((expr-type (sal-expr/type ast))
          (domain (sal-function-type/domain expr-type))
          (it (sal-type/make-iterator domain))
          (doc-queue (make-queue))
          (depth (+ depth 1)))
     (iterator/for-each (lambda (domain-value)
                          (let ((range-value (sal-expr/evaluate (sal-expr/apply ast domain-value))))
                            (queue/insert! doc-queue
                                           (pp/group (pp/concat "[" (sal-ast->lsal-doc domain-value pp-info depth) "]" (pp/line "")
                                                                (pp/nest* 1 "=" (sal-ast->lsal-doc range-value pp-info depth)))))))
                        it)
     (apply pp/list-style1 'array (queue->list doc-queue)))
   (catch* '(type-iterator expr-evaluator)
           (lambda (_ __)
             (call-next-method)))))

(define-method (sal-ast->sal-doc (ast <sal-array-literal>) (pp-info <pp-value-info++>) (depth <primitive>))
  (try
   (let* ((expr-type (sal-expr/type ast))
          (domain (sal-function-type/domain expr-type))
          (it (sal-type/make-iterator domain))
          (doc-queue (make-queue))
          (depth (+ depth 1)))
     (iterator/for-each (lambda (domain-value)
                          (let ((range-value (sal-expr/evaluate (sal-expr/apply ast domain-value))))
                            (queue/insert! doc-queue
                                           (pp/group (pp/concat "[" (sal-ast->sal-doc domain-value pp-info depth) "]" (pp/line "")
                                                                (pp/nest* 1 "=" (sal-ast->sal-doc range-value pp-info depth)))))))
                        it)
     (doc-list-with-sep+ "[" "]" (queue->list doc-queue) ","))
   (catch* '(type-iterator expr-evaluator)
           (lambda (_ __)
             (call-next-method)))))
  
