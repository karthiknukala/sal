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

(module sal-flat-support
        (include "sal.sch")
        (import queue unique-names sal-expression sal-type sal-decls sal-ast-simplify
                sal-environment)
        (export (make-bool-expr-alias! expr aux-decl-queue . alias-prefix)
                (make-expr-alias! expr aux-decl-queue . alias-prefix)
                (remove-dependent-aux-let-decls! aux-decl-queue local-decls)
                (sal-flattener/keep-boolean-application expr proc)
                (sal-flattener/keep-application expr proc)
                (alt-pair-list->lambda alt-list function-type))
        )

;; This module implements functions used by: sal-finite-expressions and sal-flat-data-structures

;;--------------------------------------------------------
;;
;; Aliases
;;
;;--------------------------------------------------------

;; create an alias for an expression
;; if expr is a <sal-simple-expr> the result is expr
;; otherwise a new let-decl is created, added to aux-decl-queue, and the result
;; is a reference to the new let-decl.
(define (make-expr-alias-core! expr type aux-decl-queue . alias-prefix)
  (if (instance-of? expr <sal-simple-expr>)
    expr
    (let* ((prefix (optional-arg alias-prefix 'aux))
           (id (gen-unique-name prefix))
           (place-provider expr)
           (decl (make-ast-instance <sal-let-decl> place-provider
                                    :id (make-sal-identifier place-provider id)
                                    :type type
                                    :value expr))
           (name-expr (make-sal-name-expr decl place-provider)))
      (queue/insert! aux-decl-queue decl)
      name-expr)))

(define (make-bool-expr-alias! expr aux-decl-queue . alias-prefix)
  (apply make-expr-alias-core! 
         expr
         (make-sal-builtin-name <sal-bool-type> expr)
         aux-decl-queue
         alias-prefix))

(define (make-expr-alias! expr aux-decl-queue . alias-prefix)
  (apply make-expr-alias-core!
         expr
         (sal-expr/type expr)
         aux-decl-queue
         alias-prefix))

;; - aux-decl-queue is a queue which contains auxiliary declarations.
;; - local-decls is a list of <sal-var-decl>
;; This procedure will remove from aux-decl-queue any auxiliary declaration
;; which depends on an element of the list local-decls.
;; Remark: if aux1 depends on aux2 which depends on an element of local-decls,
;;         then aux1 and aux2 are removed from local-decls
;; The result is a list of removed auxiliary declarations.
(define (remove-dependent-aux-let-decls! aux-decl-queue local-decls)
  (let loop ((dep-decls local-decls)
             (removed-let-decls '()))
    (let* ((dep-decl-table (sal-decl-list->eq-table dep-decls))
           (new-removed-let-decls (queue/filter! 
                                   (lambda (decl)
                                     (not (sal-ast/contains-reference-in-table? decl dep-decl-table)))
                                   aux-decl-queue)))
      (if (null? new-removed-let-decls)
        removed-let-decls
        ;; I removed some decls, so I must remove the declarations that depends on them...
        (loop new-removed-let-decls (append new-removed-let-decls removed-let-decls))))))

;;--------------------------------------------------------
;;
;; Maintaining an operator during flattening
;;
;;--------------------------------------------------------

(define (sal-flattener/keep-application-core expr result-type proc)
  (let* ((arg-list (sal-application/argument-list expr))
         (new-arg-list (map (lambda (curr-expr)
                              (let ((expr-list (proc curr-expr)))
                                [assert (expr-list) (= (length expr-list) 1)]
                                (car expr-list)))
                            arg-list))
         (new-expr (copy-ast expr 
                             :arg (apply make-application-argument new-arg-list))))
    (values (list new-expr) result-type)))

(define (sal-flattener/keep-boolean-application expr proc)
  (sal-flattener/keep-application-core expr (sal-expr/type expr) proc))

(define (sal-flattener/keep-application expr proc)
  (sal-flattener/keep-application-core expr #f proc))


;;--------------------------------------------------------
;;
;; Build a funtion from list of (domain-value . range-value)
;; this function is used by bit-list->sal-values and data-list->sal-value
;;
;;--------------------------------------------------------
(define (alt-pair-list->lambda alt-list function-type)
  (when (null? alt-list)
    (sign-source-error function-type "Invalid function type (or array type), the domain is empty."))
  (let* ((place-provider function-type)
         (domain (slot-value function-type :domain))
         (range (slot-value function-type :range))
         (arg-types (sal-function-type/domain-types function-type))
         (arg-decls (sal-type-list->sal-decl-list arg-types))
         (arg-name-exprs (sal-decl-list->sal-name-expr-list arg-decls))
         (body-expr (let loop ((body-expr (cdar alt-list))
                               (alt-list (cdr alt-list)))
                      (if (null? alt-list)
                        body-expr
                        (let* ((curr-alt (car alt-list))
                               (curr-domain-value (car curr-alt))
                               (curr-domain-value-list (sal-argument->argument-list curr-domain-value (length arg-decls)))
                               (curr-range-value (cdr curr-alt))
                               (curr-condition (make-sal-and+* (map make-sal-equality+ arg-name-exprs 
                                                                    curr-domain-value-list) place-provider))
                               (new-body-expr (make-sal-cond+ curr-condition curr-range-value body-expr place-provider)))
                          (loop new-body-expr (cdr alt-list)))))))
    (make-ast-instance <sal-lambda> function-type
                       :local-decls arg-decls
                       :expr body-expr)))
