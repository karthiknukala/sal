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

(module sal-context
        (include "sal.sch")
        (import symbol-table queue sal-decls)
        (export (make-empty-context sal-env)
                (make-context sal-env name)
                (sal-context/initialize-internal-actuals! context)
                (sal-context/name ctx)
                (sal-context/type-declaration ctx name)
                (sal-context/constant-declaration ctx name)
                (sal-context/module-declaration ctx name)
                (sal-context/assertion-declaration ctx name)
                (sal-context/context-name-declaration ctx name)
                (sal-context/declarations ctx)
                (sal-context/param ctx name)
                (sal-context/params ctx))
        )

(define (make-empty-context sal-env)
  (let ((context 
         (make-instance
          <sal-context>
          :params '()
          :declarations (make-queue)
          :constant-declarations (make-symbol-table)
          :type-declarations (make-symbol-table)
          :module-declarations (make-symbol-table)
          :assertion-declarations (make-symbol-table)
          :context-name-declarations (make-symbol-table)
          :importers (make-queue)
          :sal-env sal-env)))
    (set-slot-value! context :context context)
    context))

(define (sal-context/initialize-internal-actuals! context)
  (set-slot-value! context :internal-actuals (map (lambda (decl)
                                                    (cond
                                                     ((instance-of? decl <sal-type-param-decl>)
                                                      (make-ast-instance <sal-type-param-name> decl
                                                                         :decl decl))
                                                     (else
                                                      [assert (decl) (instance-of? decl <sal-var-decl>)]
                                                      (make-ast-instance <sal-var-param-name-expr> decl
                                                                         :decl decl))))
                                                  (slot-value context :params))))

(define (make-context sal-env name)
  (let ((result (make-empty-context sal-env)))
    (set-slot-value! result :id (make-instance <sal-identifier> 
                                               :context result
                                               :name name))
    result))

(define (sal-context/name ctx)
  [assert (ctx) (instance-of? ctx <sal-context>)]
  (sal-ast/context-name ctx))

(define (sal-context/type-declaration ctx name)
  [assert (ctx) (instance-of? ctx <sal-context>)]
  (symbol-table/lookup (slot-value ctx :type-declarations) name))

(define (sal-context/constant-declaration ctx name)
  [assert (ctx) (instance-of? ctx <sal-context>)]
  (symbol-table/lookup (slot-value ctx :constant-declarations) name))

(define (sal-context/module-declaration ctx name)
  [assert (ctx) (instance-of? ctx <sal-context>)]
  (symbol-table/lookup (slot-value ctx :module-declarations) name))

(define (sal-context/assertion-declaration ctx name)
  [assert (ctx) (instance-of? ctx <sal-context>)]
  (symbol-table/lookup (slot-value ctx :assertion-declarations) name))

(define (sal-context/context-name-declaration ctx name)
  [assert (ctx) (instance-of? ctx <sal-context>)]
  (symbol-table/lookup (slot-value ctx :context-name-declarations) name))

(define (sal-context/declarations ctx)
  (queue->list (slot-value ctx :declarations)))

(define (sal-context/param ctx name)
  [assert (ctx) (instance-of? ctx <sal-context>)]
  (sal-decl-list/lookup (slot-value ctx :params) name))

(define (sal-context/params ctx)
  [assert (ctx) (instance-of? ctx <sal-context>)]
  (slot-value ctx :params))


