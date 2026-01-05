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

(module sal-transient-vars
        (include "sal.sch")
        (include "fast-hash-table.sch")
        (import sal-module sal-ast-for-each)
        (export (sal-module/transient-vars ast))
        )

(define-generic (sal-module/transient-vars ast))

(memoize-method (sal-module/transient-vars (ast <sal-module>)))

(define-method (sal-module/transient-vars (ast <sal-esm-module>))
  (flat-module/transient-vars ast))

(define-method (sal-module/transient-vars (ast <sal-flat-module>))
  (flat-module/transient-vars ast))

(define (flat-module/transient-vars ast)
  (let ((non-transient-vars (make-eq-hash-table))
        (defined-vars (sal-module/defined-variables ast))
        (state-vars (slot-value ast :state-vars)))
    (sal-ast/collect-non-transient! (slot-value ast :transition) non-transient-vars)
    (filter (lambda (var-decl)
              (and (not (eq-hash-table/get defined-vars var-decl))
                   (not (eq-hash-table/get non-transient-vars var-decl))
                   (not (instance-of? var-decl <sal-input-state-var-decl>))))
            state-vars)))
        
(define-generic (sal-ast/collect-non-transient! ast non-transient-var-table))

(define-method (sal-ast/collect-non-transient! (ast <primitive>) (non-transient-var-table <primitive>))
  [assert (ast) (not ast)]
  ;; do nothing
  #unspecified)

(define-method (sal-ast/collect-non-transient! (ast <sal-ast>) (non-transient-var-table <primitive>))
  (sal-ast/for-each-children (cut sal-ast/collect-non-transient! <> non-transient-var-table)
                             ast))

(define-method (sal-ast/collect-non-transient! (ast <sal-next-operator>) (non-transient-var-table <primitive>))
  #unspecified)

(define-method (sal-ast/collect-non-transient! (ast <sal-name-expr>) (non-transient-var-table <primitive>))
  (let ((decl (slot-value ast :decl))) 
    (when (instance-of? decl <sal-state-var-decl>)
      (eq-hash-table/put! non-transient-var-table decl #t))))
