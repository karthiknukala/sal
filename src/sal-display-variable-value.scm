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

(module sal-display-variable-value
        (include "sal.sch")
        (import sal-value-to-assignments sal-pp queue)
        (export (sal-var-decl/display-value var-decl value)
                (sal-var-decl/display-value-differences var-decl value pre-value))
        )

(define-generic (sal-var-decl/display-value var-decl value))

(define-method (sal-var-decl/display-value (var-decl <sal-var-decl>) (value <sal-expr>))
  (sal-var-decl/display-value-differences var-decl value #f))

(define-generic (sal-var-decl/display-value-differences var-decl value pre-value))

(define-method (sal-var-decl/display-value-differences (var-decl <sal-var-decl>) (value <sal-expr>) (pre-value <primitive>))
;; BD  (breakpoint "display-value-differences" (var-decl value pre-value) #t)
  (let ((lhs (make-sal-name-expr var-decl))
        (assignments (make-queue)))
    (sal-value->assignments-core value pre-value lhs assignments)
    (for-each (lambda (assignment)
                (sal/pp assignment)
                (print ""))
              (queue->list assignments))))
