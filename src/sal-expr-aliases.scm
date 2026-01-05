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

(module sal-expr-aliases
        (include "sal.sch")
        (import queue unique-names sal-expression)
        (export (make-bool-expr-alias! expr aux-decl-queue . alias-prefix)
                (make-expr-alias! expr aux-decl-queue . alias-prefix))
        )

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

