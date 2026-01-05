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

(module sal-lhs-subsumption
        (include "sal.sch")
        (import sal-ast-eq)
        (export (sal-lhs/subsumes? lhs1 lhs2))
        )

;; Returns #t if lhs1 subsumes lhs2
;; A lhs1 subsumes lhs2 when lhs2
;; contains lhs1.
;; Example: a[1] subsumes a[1].1
(define-generic (sal-lhs/subsumes? lhs1 lhs2))

(define-method (sal-lhs/subsumes? (lhs1 <sal-expr>) (lhs2 <sal-expr>))
  (sal-ast/equivalent? lhs1 lhs2))

(define-method (sal-lhs/subsumes? (lhs1 <sal-expr>) (lhs2 <sal-selection>))
  (or (sal-ast/equivalent? lhs1 lhs2)
      (sal-lhs/subsumes? lhs1 (sal-selection/target lhs2))))


