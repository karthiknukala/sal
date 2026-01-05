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

(module sal-esm-reflexivity
        (include "sal.sch")
        (import sal-type sal-esm-engine sal2scm-core sal-ast-env
                sal-expression sal-module sal-ast-simplify
                sal-ast-expand sal-ast-for-each fast-hash-table
                sal-esm-support)
        (export (sal-esm/convert-user-function module user-sal-function ctx user-next-vars?))
        )

(define (sal-esm/convert-user-function module user-sal-function ctx user-next-vars?)
  (let ((function-type (sal-expr/type user-sal-function))
        (module-type (sal-module/type module)))
    (unless (sal-type/equivalent? (sal-function-type/domain function-type) module-type)
      ;; (breakpoint "convert-user-function" (function-type module-type module) #t)
      (sign-source-error user-sal-function "This function cannot be used to evaluate the state of the module, because their types are incompatible."))
    (let* ((state-record (if user-next-vars? 
                           (sal-module/next-record-literal module)
                           (sal-module/record-literal module)))
           (app (expand-state-function (sal-expr/apply user-sal-function state-record)))
           (_ (when user-next-vars?
                (check-if-valid-user-next-proc app module)))
           (code `(lambda () ,(sal->scm app ctx (make-empty-env))))
           ;; (_ (pp code))
           (user-code-id (sal-esm-engine-scm-context/add-definition! ctx code 'user-code)))
      user-code-id)))

(define (expand-state-function ast)
  (sal-ast/simplify
   (sal-ast/expand-applications ast (make-empty-env) 
                                (lambda (app)
                                  (instance-of? (slot-value app :arg) <sal-state-record-literal>)))))

(define (check-if-valid-user-next-proc ast module)
  (let ((defined-var-table (sal-module/defined-variables module)))
    (sal-ast/for-each (lambda (child)
                        (when (instance-of? child <sal-name-expr>)
                          (let ((decl (slot-value child :decl)))
                            (when (instance-of? decl <sal-input-state-var-decl>)
                              (sign-unsupported-feature ast
                                                        "User provided state function must not use input variables."))
                            (when (and (instance-of? decl <sal-state-var-decl>)
                                       (eq-hash-table/get defined-var-table decl))
                              (sign-unsupported-feature ast
                                                        "User provided state function must not use defined variables.")))))
                      ast)))



