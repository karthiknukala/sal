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

(module sal-esm-access-level-table
        (include "sal.sch")
        (include "fast-hash-table.sch")
        (include "sal-ast-table.sch")
        (export (make-sal-esm-access-level-table)
                (sal-esm-access-level-table/get-level table decl)
                (sal-esm-access-level-table/update-level table decl new-level)
                (sal-esm/collect-access-levels! esm access-level-table)
                (sal-esm-module/initialization-access-levels module)
                (sal-esm-module/transition-access-levels module)
                (sal-lhs/access-level lhs))
        )

;; Compute the access level for a state variable.
;; The results are stored in a table.
;;
;; The access level of a variable "x" is "n" when
;; the maximum number of accesses in LHS of "x" is "n".
;;
;; Examples:
;; LHS         |   number of accesses
;; x           |          0
;; x'          |          0
;; x.2         |          1
;; x.2[idx]    |          2
;; x.fiedl     |          1
;; x.2[idx].1  |          3
;;
;; In the example above, the access level of "x" is 
;; 3, because the maximum number of accesses in a
;; LHS is 3.


(define (make-sal-esm-access-level-table)
  (make-eq-hash-table))

(define (sal-esm-access-level-table/get-level table decl)
  (cond
   ((eq-hash-table/get table decl) =>
    cdr)
   (else
    0)))

(define (sal-esm-access-level-table/update-level table decl new-level)
  (cond
   ((eq-hash-table/get table decl) =>
    (lambda (entry)
      (set-cdr! entry (max new-level (cdr entry)))))
   (else
    (eq-hash-table/put! table decl new-level))))

(define-generic (sal-esm/collect-access-levels! esm access-level-table))

(define-method (sal-esm/collect-access-levels! (esm <primitive>) (access-level-table <primitive>))
  ;; do nothing
  #unspecified)

(define-method (sal-esm/collect-access-levels! (esm <sal-esm-composition-statement>) (access-level-table <primitive>))
  (for-each (cut sal-esm/collect-access-levels! <> access-level-table) (slot-value esm :statements)))

(define-method (sal-esm/collect-access-levels! (esm <sal-esm-case>) (access-level-table <primitive>))
  (for-each (lambda (entry)
              (sal-esm/collect-access-levels! (slot-value entry :statement) access-level-table))
            (slot-value esm :case-entries)))

(define-method (sal-esm/collect-access-levels! (esm <sal-esm-when-undefined>) (access-level-table <primitive>))
  (sal-esm/collect-access-levels! (slot-value esm :statement) access-level-table))

(define-method (sal-esm/collect-access-levels! (esm <sal-esm-new-binds-statement>) (access-level-table <primitive>))
  (sal-esm/collect-access-levels! (slot-value esm :statement) access-level-table))

(define-method (sal-esm/collect-access-levels! (esm <sal-esm-guard>) (access-level-table <primitive>))
  ;; do nothing
  #unspecified)

(define-method (sal-esm/collect-access-levels! (esm <sal-esm-assignment>) (access-level-table <primitive>))
  (let* ((lhs (slot-value esm :lhs))
         (decl (slot-value (sal-lhs/name-expr lhs) :decl)))
    [assert (decl) (instance-of? decl <sal-state-var-decl>)]
    (unless (instance-of? decl <sal-choice-input-state-var-decl>)
      (sal-esm-access-level-table/update-level access-level-table
                                               decl
                                               (sal-lhs/access-level lhs)))))

(define-generic (sal-lhs/access-level lhs))
(define-method (sal-lhs/access-level (lhs <sal-name-expr>)) 0)
(define-method (sal-lhs/access-level (lhs <sal-next-operator>)) 0)
(define-method (sal-lhs/access-level (lhs <sal-selection>)) (+ (sal-lhs/access-level (sal-selection/target lhs)) 1))


(define-generic (sal-esm-module/initialization-access-levels module))
(define-generic (sal-esm-module/transition-access-levels module))

(define-method (sal-esm-module/initialization-access-levels (module <sal-esm-module>))
  (let ((result (make-sal-esm-access-level-table)))
    (sal-esm/collect-access-levels! (slot-value module :initialization) result)
    result))

(memoize-sal-ast-method (sal-esm-module/initialization-access-levels (module <sal-esm-module>)))

(define-method (sal-esm-module/transition-access-levels (module <sal-esm-module>))
  (let ((result (make-sal-esm-access-level-table)))
    (sal-esm/collect-access-levels! (slot-value module :transition) result)
    result))

(memoize-sal-ast-method (sal-esm-module/transition-access-levels (module <sal-esm-module>)))

     
