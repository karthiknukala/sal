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

(module sal-collect-state-lhs
        (include "sal.sch")
        (import sal-ast-for-each sal-expression)
        (export (sal-ast/collect-state-lhs! ast add-lhs-proc!))
        )

(define-generic (sal-ast/collect-state-lhs! ast add-lhs-proc!))

(define-method (sal-ast/collect-state-lhs! (ast <sal-ast>) (add-lhs-proc! <primitive>))
  (sal-ast/for-each-children (cut sal-ast/collect-state-lhs! <> add-lhs-proc!) ast))

(define-method (sal-ast/collect-state-lhs! (ast <sal-selection>) (add-lhs-proc! <primitive>))
  (if (and (sal-expr/lhs? ast)
           (instance-of? (slot-value (sal-lhs/name-expr ast) :decl) <sal-state-var-decl>))
    (let loop ((lhs ast))
      (if (sal-lhs/ground? lhs)
        (add-lhs-proc! lhs)
        (loop (sal-selection/target lhs))))))

(define-method (sal-ast/collect-state-lhs! (ast <sal-name-expr>) (add-lhs-proc! <primitive>))
  (let ((decl (slot-value ast :decl)))
    (when (and (instance-of? decl <sal-state-var-decl>)
               (not (instance-of? decl <sal-choice-input-state-var-decl>)))
      (add-lhs-proc! ast))))

(define-method (sal-ast/collect-state-lhs! (ast <sal-next-operator>) (add-lhs-proc! <primitive>))
  (unless (instance-of? (slot-value (slot-value ast :name-expr) :decl) <sal-choice-input-state-var-decl>)
    (add-lhs-proc! ast)))


