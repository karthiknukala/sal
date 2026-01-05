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

(module sal-global-context
        (include "sal.sch")
        (include "fast-hash-table.sch")
        (import queue sal-ast-table sal-ast-env sal-ast-copy sal-expression
                sal-pp sal-type sal-environment)
        (export <sal-global-context>
                (make-sal-global-context sal-env)
                (sal-ast/flat-globals ast env global-context)
                (sal-global-context/pp global-context))
        )

(define-class <sal-global-context> () (:const-table :type-table :module-table :assertion-table
                                       :declarations :name-table :sal-env :curr-type-decl))

(define (make-sal-global-context sal-env)
  (make-instance <sal-global-context>
                 :const-table (make-sal-ast-table)
                 :type-table (make-sal-ast-table)
                 :module-table (make-sal-ast-table)
                 :assertion-table (make-sal-ast-table)
                 :declarations (make-queue)
                 :name-table (make-eq-hash-table)
                 :sal-env sal-env
                 :curr-type-decl #f))

(define (sal-global-context/new-name! global-context name)
  (let ((name-table (slot-value global-context :name-table)))
    (cond
     ((eq-hash-table/get name-table name) =>
      (lambda (entry)
        (let ((result (symbol-append name '! (object->symbol (cdr entry)))))
          (set-cdr! entry (+ (cdr entry) 1))
          result)))
     (else
      (eq-hash-table/put! name-table name 1)
      (symbol-append name '!0)))))
;; name))))

(define (sal-global-context/pp global-context)
  (for-each (lambda (decl)
              (sal/pp decl) (print ""))
            (queue->list (slot-value global-context :declarations))))

(define-generic (sal-ast/flat-globals ast env global-context))

(define-method (sal-ast/flat-globals (ast <sal-ast>) (env <primitive>) (global-context <sal-global-context>))
  (sal-ast/map ast env (lambda (child-ast new-env) (sal-ast/flat-globals child-ast new-env global-context))))

(define-method (sal-ast/flat-globals (ast <sal-qualified-type-name>) (env <primitive>) (global-context <sal-global-context>))
  (let ((ast (sal-ast/substitute ast env)))
    (cond
     ((sal-name-ref/builtin? ast)
      ast)
     ((sal-ast-table/get (slot-value global-context :type-table) ast) =>
      (lambda (entry)
        (let ((global-decl (cdr entry)))
          (make-ast-instance <sal-type-name> ast
                             :decl global-decl))))
     (else
      (let* ((decl (slot-value ast :decl))
             (id (slot-value decl :id))
             (name (sal-identifier/name id))
             (empty-env (make-empty-env))
             (global-name (sal-global-context/new-name! global-context name))
             (global-id (copy-ast id :name global-name))
             (global-decl (copy-ast decl
                                    :id global-id
                                    :type #f))) ;; body was not flattened
        ;; cache it before processing definition...
        (sal-ast-table/put! (slot-value global-context :type-table) ast global-decl)
        (set-slot-value! global-context :curr-type-decl global-decl)
        (let* ((type-def (sal-type-name/definition ast))
               (global-type-def (and type-def (sal-ast/flat-globals type-def empty-env global-context))))
          (set-slot-value! global-decl :type global-type-def)
          (queue/insert! (slot-value global-context :declarations) global-decl)
          (make-ast-instance <sal-type-name> ast
                             :decl global-decl)))))))

(define-method (sal-ast/flat-globals (ast <sal-scalar-type>) (env <primitive>) (global-context <sal-global-context>))
  (let* ((scalar-elements (slot-value ast :scalar-elements))
         (scalar-type-decl (slot-value global-context :curr-type-decl))
         (global-scalar-elements (map (lambda (scalar-element)
                                        (let* ((name (sal-name-ref/name scalar-element))
                                          (global-name (sal-global-context/new-name! global-context name))
                                          (global-id (make-sal-identifier scalar-element global-name))
                                          (global-type (make-ast-instance <sal-type-name> scalar-element
                                                                          :decl scalar-type-decl))
                                          (global-scalar-element-decl (make-ast-instance <sal-scalar-element-decl> scalar-element
                                                                                         :id global-id
                                                                                         :type global-type
                                                                                         :scalar-type-decl scalar-type-decl)))
                                          (register-global-decl! global-context scalar-element global-scalar-element-decl)
                                          (make-sal-name-expr global-scalar-element-decl scalar-element)))
                                      scalar-elements)))
    (copy-instance ast
                   :scalar-elements global-scalar-elements)))

(define (register-global-decl! global-context qualified-name-expr global-decl)
  (sal-ast-table/put! (slot-value global-context :const-table) qualified-name-expr global-decl)
  (queue/insert! (slot-value global-context :declarations) global-decl))
  
(define-method (sal-ast/flat-globals (ast <sal-data-type>) (env <primitive>) (global-context <sal-global-context>))
  (let* ((constructors (slot-value ast :constructors))
         (data-type-decl (slot-value global-context :curr-type-decl))
         (global-constructors (map (lambda (constructor)
                                     (let* ((recognizer (sal-name-expr/constructor-recognizer constructor))
                                            (name (sal-name-ref/name constructor))
                                            (global-name (sal-global-context/new-name! global-context name))
                                            (global-recognizer-name (symbol-append global-name '?))
                                            (global-id (make-sal-identifier constructor global-name))
                                            (global-recognizer-id (make-sal-identifier constructor global-recognizer-name))
                                            (global-constructor-decl (make-ast-instance <sal-constructor-decl> constructor
                                                                                        :id global-id
                                                                                        :data-type-decl data-type-decl))
                                            (global-constructor (make-sal-name-expr global-constructor-decl constructor))
                                            (global-recognizer-decl (make-recognizer-decl global-recognizer-id
                                                                                          data-type-decl
                                                                                          global-constructor-decl))
                                            (accessors (sal-name-expr/constructor-accessors constructor))
                                            (global-accessors (map (cut convert-accessor! <> env global-constructor-decl global-context) accessors))
                                            (global-accessor-types (map sal-name-expr/accessor-type global-accessors))
                                            ;; (global-constructor-type (make-constructor-type global-constructor-decl global-accessor-types data-type-decl))
                                            )
                                       (set-slot-value! global-constructor-decl :recognizer-decl global-recognizer-decl)
                                       (set-slot-value! global-constructor-decl :type (sal-ast/flat-globals (sal-expr/type constructor)
                                                                                                            env 
                                                                                                            global-context))
                                       (register-global-decl! global-context constructor global-constructor-decl)
                                       (register-global-decl! global-context recognizer global-recognizer-decl)
                                       global-constructor))
                                   constructors)))
    (copy-instance ast
                   :constructors global-constructors)))

(define (make-recognizer-decl recognizer-id data-type-decl constructor-decl)
  (make-ast-instance <sal-recognizer-decl> recognizer-id
                     :id recognizer-id
                     :type (make-ast-instance <sal-function-type> recognizer-id
                                              :domain (make-ast-instance <sal-type-name> recognizer-id
                                                                         :decl data-type-decl)
                                              :range (make-sal-builtin-name <sal-bool-type> recognizer-id))
                     :constructor-decl constructor-decl))

; (define (make-constructor-type constructor-decl accessor-types data-type-decl)
;   (let ((data-type-name (make-ast-instance <sal-type-name> constructor-decl
;                                            :decl data-type-decl)))
;     (if (null? accessor-types)
;       data-type-name
;       (make-ast-instance <sal-function-type> constructor-decl
;                          :domain (if (null? (cdr accessor-types))
;                                    (car accessor-types)
;                                    (make-ast-instance <sal-domain-tuple-type> constructor-decl
;                                                       :types accessor-types))
;                          :range data-type-name))))

(define (convert-accessor! accessor-name-expr env constructor-decl global-context)
  (let* ((accessor-type (sal-expr/type accessor-name-expr))
         (name (sal-name-ref/name accessor-name-expr))
         (global-name (sal-global-context/new-name! global-context name))
         (global-id (make-sal-identifier accessor-name-expr global-name))
         (global-accessor-type (sal-ast/flat-globals accessor-type env global-context))
         (global-accessor-decl (make-ast-instance <sal-accessor-decl> accessor-name-expr
                                                  :id global-id
                                                  :type global-accessor-type
                                                  :constructor-decl constructor-decl)))
    (register-global-decl! global-context accessor-name-expr global-accessor-decl)
    (make-sal-name-expr global-accessor-decl accessor-name-expr)))
         
(define-method (sal-ast/flat-globals (ast <sal-qualified-name-expr>) (env <primitive>) (global-context <sal-global-context>))
  (let ((ast (sal-ast/substitute ast env)))
    (cond
     ((sal-name-ref/builtin? ast)
      ast)
     ((sal-ast-table/get (slot-value global-context :const-table) ast) =>
      (lambda (entry)
        (let ((global-decl (cdr entry)))
          (make-sal-name-expr global-decl ast))))
     (else
      (let* ((decl (slot-value ast :decl))
             (id (slot-value decl :id))
             (name (sal-identifier/name id))
             (empty-env (make-empty-env))
             (global-name (sal-global-context/new-name! global-context name))
             (global-id (copy-ast id :name global-name))
             (type (sal-name-expr/type ast))
             (global-type (sal-ast/flat-globals type empty-env global-context))
             (global-decl (copy-ast decl
                                    :id global-id
                                    :type global-type
                                    :value #f))) ;; body was not flattened yet
        ;; cache it before processing definition...
        (sal-ast-table/put! (slot-value global-context :const-table) ast global-decl)
        (let* ((definition (sal-name-expr/definition ast))
               (global-definition (and definition (sal-ast/flat-globals definition empty-env global-context))))
          (set-slot-value! global-decl :value global-definition)          
          (queue/insert! (slot-value global-context :declarations) global-decl)
          (make-sal-name-expr global-decl ast)))))))
