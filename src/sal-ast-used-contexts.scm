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

(module sal-ast-used-contexts
        (include "sal.sch")
        (include "sal-ast-table.sch")
        (import sal-ast-for-each sal-module sal-type sal-expression sal-assertion)
        (export (sal-ast/used-contexts ast)
                (sal-ast/used-contexts-core ast found-ctxs found-names))
        )

(define (sal-ast/used-contexts ast)
  (let ((ctx-table (make-hashtable)))
    (sal-ast/used-contexts-core ast ctx-table (make-sal-ast-table))
    (hashtable-key-list ctx-table)))

(define-generic (sal-ast/used-contexts-core ast found-ctxs found-names))

(define-method (sal-ast/used-contexts-core :around (ast <sal-ast>) (found-ctxs <primitive>) (found-names <primitive>))
  (cond
   ((sal-ast/context ast) =>
    (lambda (ctx)
      (hashtable-put! found-ctxs ctx #t))))
  (call-next-method))

(define-method (sal-ast/used-contexts-core (ast <sal-ast>) (found-ctxs <primitive>) (found-names <primitive>))
  (sal-ast/for-each-children (cut sal-ast/used-contexts-core <> found-ctxs found-names)
                             ast))

(define-method (sal-ast/used-contexts-core (ast <sal-module-name>) (found-ctxs <primitive>) (found-names <primitive>))
  (unless (sal-ast-table/contains? found-names ast)
    (sal-ast-table/put! found-names ast #t)
    (let ((def (sal-module-name/definition ast)))
      (sal-ast/used-contexts-core def found-ctxs found-names))))

(define-method (sal-ast/used-contexts-core (ast <sal-type-name>) (found-ctxs <primitive>) (found-names <primitive>))
  (unless (sal-ast-table/contains? found-names ast)
    (sal-ast-table/put! found-names ast #t)
    (let ((def (sal-type-name/definition ast)))
      (when def
        (sal-ast/used-contexts-core def found-ctxs found-names)))))

(define-method (sal-ast/used-contexts-core (ast <sal-name-expr>) (found-ctxs <primitive>) (found-names <primitive>))
  (unless (sal-ast-table/contains? found-names ast)
    (sal-ast-table/put! found-names ast #t)
    (let ((def (sal-name-expr/definition ast)))
      (when def
        (sal-ast/used-contexts-core def found-ctxs found-names)))))

(define-method (sal-ast/used-contexts-core (ast <sal-qualified-assertion-name>) (found-ctxs <primitive>) (found-names <primitive>))
  (unless (sal-ast-table/contains? found-names ast)
    (sal-ast-table/put! found-names ast #t)
    (let ((def (sal-assertion-name/definition ast)))
      (sal-ast/used-contexts-core def found-ctxs found-names))))
    
