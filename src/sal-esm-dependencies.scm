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

(module sal-esm-dependencies
        (include "sal.sch")
        (import sal-esm graph sal-ast-for-each sal-ast-table queue
                sal-expression sal-collect-state-lhs)
        (export <sal-esm-dependencies-ctx>
                (make-sal-esm-dependencies-context)
                (sal-esm/collect-dependencies! esm ctx)
                (sal-esm-dependencies-context/complete-dependencies! ctx))
        )

(define-class <sal-esm-dependencies-ctx> () (:graph :lhs-to-node))

(define (make-sal-esm-dependencies-context)
  (make-instance <sal-esm-dependencies-ctx>
                 :graph (make-graph :undirected? #t)
                 :lhs-to-node (make-sal-ast-table)))

(define-generic (sal-esm/collect-dependencies! esm ctx))

(define-method (sal-esm/collect-dependencies! (esm <primitive>) (ctx <sal-esm-dependencies-ctx>))
  ;; do nothing
  #unspecified)

(define-method (sal-esm/collect-dependencies! (esm <sal-esm-module>) (ctx <sal-esm-dependencies-ctx>))
  (sal-esm/collect-dependencies! (slot-value esm :definition) ctx)
  (sal-esm/collect-dependencies! (slot-value esm :transition) ctx))

(define-method (sal-esm/collect-dependencies! (esm <sal-esm-component>) (ctx <sal-esm-dependencies-ctx>))
  (sal-ast/for-each-children (cut sal-esm/collect-dependencies! <> ctx) esm))

(define (lhs->node lhs lhs-to-node graph)
  (cond
   ((sal-ast-table/get lhs-to-node lhs) =>
    cdr)
   (else
    (let ((new-node (graph/make-node! graph lhs)))
      (sal-ast-table/put! lhs-to-node lhs new-node)
      new-node))))

(define-method (sal-esm/collect-dependencies! (esm <sal-esm-leaf>) (ctx <sal-esm-dependencies-ctx>))
  (let ((lhs-list (collect-lhs! esm))
        (graph (slot-value ctx :graph))
        (lhs-to-node (slot-value ctx :lhs-to-node)))
    (let loop ((lhs-list lhs-list))
      (unless (null? lhs-list)
        (let* ((lhs1 (car lhs-list))
               (node1 (lhs->node lhs1 lhs-to-node graph)))
          (let inner-loop ((lhs-list (cdr lhs-list)))
            (unless (null? lhs-list)
              (let* ((lhs2 (car lhs-list))
                     (node2 (lhs->node lhs2 lhs-to-node graph)))
                ;; (sal/pp lhs1) (display " <---> ") (sal/pp lhs2) (print "")
                ;; (breakpoint "connected" (lhs1 lhs2 node1 node2 esm used-lhs-queue graph lhs-to-node) #t)
                (unless (or (eq? node1 node2) (graph-node/connected? node1 node2))
                  (graph/make-edge! graph node1 node2))
                (inner-loop (cdr lhs-list))))))
        (loop (cdr lhs-list))))))

(define (collect-lhs! ast)
  (let* ((result '())
         (add-lhs-proc! (lambda (ast)
                          (push! (if (sal-expr/next-lhs? ast)
                                   (sal-next-lhs->lhs ast)
                                   ast)
                                 result))))
    (sal-ast/collect-state-lhs! ast add-lhs-proc!)
    result))

(define (sal-esm-dependencies-context/complete-dependencies! ctx)
  (let ((graph (slot-value ctx :graph))
        (lhs-to-node (slot-value ctx :lhs-to-node)))
    (sal-ast-table/for-each (lambda (lhs node)
                              (when (instance-of? lhs <sal-selection>)
                                (let loop ((target (sal-selection/target lhs)))
                                  (cond 
                                   ((sal-ast-table/get lhs-to-node target) =>
                                    (lambda (entry)
                                      (let ((target-node (cdr entry)))
                                        (unless (graph-node/connected? node target-node)
                                          (graph/make-edge! graph node target-node)))))
                                   ((instance-of? target <sal-selection>)
                                    (loop (sal-selection/target target)))))))
                            lhs-to-node)))

