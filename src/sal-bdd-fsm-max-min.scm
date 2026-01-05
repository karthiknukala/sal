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

(module sal-bdd-fsm-max-min
        (include "sal.sch")
        (include "fast-hash-table.sch")
        (import bdd bdd-util sal-type sal-expression sal2bdd
                sal-bdd-fsm sal-api sal-slicer sal-ast-env
                sal-module)
        (export (sal-bdd-fsm/max-subset fsm state-set cost)
                (sal-bdd-fsm/min-subset fsm state-set cost))
        )

(define-api (sal-bdd-fsm/max-subset (fsm <sal-bdd-fsm>) (state-set bdd?) (cost string?))
  :doc "Return a subset of the given state-set. The returned subset contains the maximums with respect to the given 'cost' expression. The argument state-set must be a BDD, and 'cost' a valid state expression of type integer."
  (sal-bdd-fsm/min-max-core fsm state-set cost <sal-ge>))

(define-api (sal-bdd-fsm/min-subset (fsm <sal-bdd-fsm>) (state-set bdd?) (cost string?))
  :doc "Return a subset of the given state-set. The returned subset contains the minimums with respect to the given 'cost' expression. The argument state-set must be a BDD, and 'cost' a valid state expression of type integer."
  (sal-bdd-fsm/min-max-core fsm state-set cost <sal-le>))

(define (sal-bdd-fsm/min-max-core fsm state-set cost ge-le-class)
  (let* ((bool-flat-module (sal-sliced-flat-module->flat-module (slot-value fsm :flat-module)))
         (original-module (sal-derived-flat-module/original-flat-module bool-flat-module))
         (cost (sal/state-expr cost original-module))
         (curr-var-cube (sal-bdd-fsm/curr-var-cube fsm))
         (next-var-cube (sal-bdd-fsm/next-var-cube fsm))
         (state-support (bdd/support state-set)))
    (unless (bdd/empty-cube? (bdd/cube-intersection state-support next-var-cube))
      (sign-error "Invalid state-set BDD in sal-bdd-fsm/min-max-core. BDD must contain only current variables."))
    (when (sal-ast/uses-next-operator? cost)
      (sign-error "Invalid argument for sal-bdd-fsm/min-max-core. Argument 'cost' must be a current state expression."))
    (unless (sal-type/integer? (sal-expr/type cost))
      (sign-error "Invalid argument for sal-bdd-fsm/min-max-core. Argument 'cost' must have integer type."))
    (let* ((next-cost (sal-expr->next-expr cost (make-empty-env)))
           (place-provider cost)
           (cost-ge-next-cost (make-sal-builtin-application ge-le-class place-provider cost next-cost))
           (cost-ge-next-cost-bool (make-boolean-state-expression-core cost-ge-next-cost bool-flat-module))
           (cost-ge-next-cost-bdd (sal-expr->bdd cost-ge-next-cost-bool fsm))
           (cost-ge-next-cost-support (bdd/support cost-ge-next-cost-bdd))
           (next-state-set (sal-bdd-fsm/map-curr->next fsm state-set))
           (next-state-implies-cost-ge-next-cost-bdd (bdd/or (bdd/not next-state-set) cost-ge-next-cost-bdd)))
      (bdd/and state-set (bdd/for-all next-state-implies-cost-ge-next-cost-bdd next-var-cube)))))
