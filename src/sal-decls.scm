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

(module sal-decls
        (include "sal.sch")
        (include "fast-hash-table.sch")
        (import sal-ast-for-each sal-ast-env sal-ast-copy queue symbol-table sal-module gmp-scheme sal-type)
        (export (sal-ast/open-references ast)
                (sal-ast/open-reference-list ast)
                (sal-ast/contains-open-references? ast)
                (sal-ast/collect-used-auxiliary-decls ast)
                (sal-ast/collect-used-state-var-decls ast)
                (sal-ast/unique-decls ast)
                (sal-ast/unique-decls-core ast env)
                (sal-ast/contains-reference? ast decl)
                (sal-ast/contains-reference*? ast decl-list)
                (sal-ast/number-of-references ast decl)
                (sal-ast/number-of-references-table ast decl-list)
                (sal-ast-list/number-of-references-table ast-list decl-list)
                (sal-ast/update-num-ref-table! ast decl-table)
                (sal-ast-list/update-num-ref-table! ast-list decl-table)
                (sal-ast/number-of-references-proc ast decl-list)
                (sal-ast-list/number-of-references-proc ast-list decl-list)
                (sal-decl-list/lookup decl-list name)
                (sal-decl-list->symbol-table decl-list)
                (sal-decl-list/fast-lookup decl-list name)
                (sal-decl-list->sal-name-expr-list decl-list)
                (sal-decl-list->symbol-list decl-list)
                (sal-decl-list->eq-table decl-list)
                (sal-ast/contains-reference-in-table? ast eq-decl-table)
                (make-sal-let-expr let-decl-list expr)
                (sal-decl-list/num-elements decl-list)
                (sal-decl-list/sort decl-list))
        )

(define (sal-ast/open-references ast)
  (let ((result (make-eq-hash-table)))
    (sal-ast/open-references-core ast '() result)
    result))

(define (sal-ast/open-reference-list ast)
  (let ((table (sal-ast/open-references ast))
        (result '()))
    (eq-hash-table/for-each (lambda (decl _)
                              (push! decl result))
                            table)
    result))

(define (sal-ast/contains-open-references? ast)
  (let ((result (make-eq-hash-table)))
    (sal-ast/open-references-core ast '() result)
    (> (eq-hash-table/size result) 0)))

(define-generic (sal-ast/open-references-core ast decls result-table))

(define (children-open-references ast decls result-table)
  (sal-ast/for-each-children (cut sal-ast/open-references-core <> decls result-table) ast))

(define-method (sal-ast/open-references-core (ast <sal-ast>) (decls <primitive>) (result-table <primitive>))
  (children-open-references ast decls result-table))

(define-method (sal-ast/open-references-core (ast <sal-new-binds-ast>) (decls <primitive>) (result-table <primitive>))
  (let ((new-decls (sal-new-binds-ast/fold-new-binds (lambda (new-decls decl)
                                                       (cons decl new-decls))
                                                     decls
                                                     ast)))
    (children-open-references ast new-decls result-table)))

(define-method (sal-ast/open-references-core (ast <sal-module-models>) (decls <primitive>) (result-table <primitive>))
  (let ((new-decls (fold-left (lambda (new-decls decl)
                                (cons decl new-decls))
                              decls
                              (sal-module/state-variables (slot-value ast :module)))))
    (children-open-references ast new-decls result-table)))
                              
(define-method (sal-ast/open-references-core (ast <sal-name-ref>) (decls <primitive>) (result-table <primitive>))
  (let ((decl (sal-name-ref/decl ast)))
    (unless (or (instance-of? decl <sal-top-decl>)
                (instance-of? decl <sal-auxiliary-decl>)
                (memq decl decls))
      (eq-hash-table/put! result-table decl #unspecified))))

(define-method (sal-ast/open-references-core (ast <sal-qualified-name-ref>) (decls <primitive>) (result-table <primitive>))
  (children-open-references ast decls result-table))
  
(define (sal-ast/collect-used-auxiliary-decls ast)
  (let ((result '()))
    (sal-ast/for-each (lambda (n)
                        (and-instance-of? ((n <sal-name-expr>))
                          (instance-of? (slot-value n :decl) <sal-auxiliary-decl>)
                          (pushnew! (slot-value n :decl) result)))
                      ast)
    result))

(define (sal-ast/collect-used-state-var-decls ast)
  (let ((result '())
        (found-var-decls (make-eq-hash-table)))
    (sal-ast/for-each (lambda (n)
                        (and-instance-of? ((n <sal-name-expr>))
                          (instance-of? (slot-value n :decl) <sal-state-var-decl>)
                          (not (eq-hash-table/contains? found-var-decls (slot-value n :decl)))
                          (pushnew! (slot-value n :decl) result)
                          (eq-hash-table/put! found-var-decls (slot-value n :decl) #unspecified)))
                      ast)
    result))

;; return a new AST, where all declarations node are replaced with fresh ones...
;; this transformation is useful to "break" shared declaration nodes
(define (sal-ast/unique-decls ast)
  (sal-ast/unique-decls-core ast (make-empty-env)))

(define-generic (sal-ast/unique-decls-core ast env))

(define-method (sal-ast/unique-decls-core (ast <sal-ast>) (env <primitive>))
  (sal-ast/map ast env sal-ast/unique-decls-core))

(define-method (sal-ast/unique-decls-core (ast <sal-decl>) (env <primitive>))
  (copy-ast (call-next-method)))

(define (sal-ast/contains-reference? ast decl)
  [assert (ast) (instance-of? ast <sal-ast>)]
  [assert (decl) (instance-of? decl <sal-decl>)]
  (sal-ast/find (lambda (ast)
                  (and (instance-of? ast <sal-name-ref>)
                       (eq? (sal-name-ref/decl ast) decl)))
                ast))

(define (sal-ast/contains-reference*? ast decl-list)
  [assert (ast) (instance-of? ast <sal-ast>)]
  [assert (decl-list) (list? decl-list)]
  (sal-ast/find (lambda (ast) 
                  (and (instance-of? ast <sal-name-ref>)
                       (memq (sal-name-ref/decl ast) decl-list)))
                ast))

(define (sal-ast/number-of-references ast decl)
  (sal-ast/fold (lambda (result ast)
                  (if (and (instance-of? ast <sal-name-ref>)
                           (eq? (sal-name-ref/decl ast) decl))
                    (+ result 1)
                    result))
                0
                ast))

(define (sal-ast/number-of-references-table ast decl-list)
  (sal-ast-list/number-of-references-table (list ast) decl-list))

(define (sal-ast/update-num-ref-table! ast decl-table)
  (sal-ast-list/update-num-ref-table! (list ast) decl-table))

(define (sal-ast-list/number-of-references-table ast-list decl-list)
  (let ((decl-table (make-eq-hash-table)))
    (for-each (lambda (decl)
                (eq-hash-table/put! decl-table decl 0))
              decl-list)
    (sal-ast-list/update-num-ref-table! ast-list decl-table)))

(define (sal-ast-list/update-num-ref-table! ast-list decl-table)
  (for-each (lambda (ast)
              (sal-ast/for-each (lambda (n)
                                  (when (instance-of? n <sal-name-ref>)
                                    (let* ((decl (sal-name-ref/decl n))
                                           (entry (eq-hash-table/get decl-table decl)))
                                      (when entry
                                        (set-cdr! entry (+ (cdr entry) 1))))))
                                ast))
            ast-list)
  decl-table)
  
(define (sal-ast/number-of-references-proc ast decl-list)
  (sal-ast-list/number-of-references-proc (list ast) decl-list))

(define (sal-ast-list/number-of-references-proc ast-list decl-list)
  (let ((decl-table (sal-ast-list/number-of-references-table ast-list decl-list)))
    (lambda (decl)
      [assert (decl decl-table) (eq-hash-table/get decl-table decl)]
      (cdr (eq-hash-table/get decl-table decl)))))

(define *fast-lookup-table-1* #unspecified)
(define *fast-lookup-decl-list-1* #unspecified)
(define *fast-lookup-table-2* #unspecified)
(define *fast-lookup-decl-list-2* #unspecified)

(define (cache-decl-list! decl-list table)
  (set! *fast-lookup-decl-list-2* *fast-lookup-decl-list-1*)
  (set! *fast-lookup-table-2* *fast-lookup-table-1*)
  (set! *fast-lookup-decl-list-1* decl-list)
  (set! *fast-lookup-table-1* table))

(define (cached? decl-list)
  (cond
   ((eq? *fast-lookup-decl-list-1* decl-list)
    *fast-lookup-table-1*)
   ((eq? *fast-lookup-decl-list-2* decl-list)
    *fast-lookup-table-2*)
   (else #f)))

(define-api (sal-decl-list/fast-lookup decl-list name)
  :doc "A more efficient version of @code{sal-decl-list/lookup}, when several calls to @code{sal-decl-list/fast-lookup} are performed over the same @code{decl-list}."
  (let ((table (cached? decl-list)))
    (unless table
      (set! table (sal-decl-list->symbol-table decl-list))
      (cache-decl-list! decl-list table))
    (symbol-table/lookup table name)))

(define-api (sal-decl-list/lookup (decl-list sal-decl-list?) (name symbol?))
  :doc "Return the declaration in @code{decl-list} which is named @code{name}. Return false, if such declaration does not exist."
  (find (lambda (decl) (eq? name (sal-decl/name decl))) decl-list))

(define-api (sal-decl-list->symbol-table (decl-list sal-decl-list?))
  :doc "Convert @code{decl-list} in a symbol table from names to @code{<sal-decl>} instances."
  (fold-left
   (lambda (result decl)
     (symbol-table/add! result (sal-decl/name decl) decl))
   (make-symbol-table)
   decl-list))

(define-api (sal-decl-list->sal-name-expr-list (decl-list sal-decl-list?))
  (map make-sal-name-expr decl-list))

(define-api (sal-decl-list->symbol-list (decl-list sal-decl-list?))
  (map sal-decl/name decl-list))

(define-api (sal-decl-list->eq-table (decl-list sal-decl-list?))
  (let ((decl-table (make-eq-hash-table)))
    (for-each (lambda (decl)
                (eq-hash-table/put! decl-table decl #unspecified))
              decl-list)
    decl-table))

(define (sal-ast/contains-reference-in-table? ast eq-decl-table)
  (sal-ast/find (lambda (ast) 
                  (and (instance-of? ast <sal-name-ref>)
                       (eq-hash-table/get eq-decl-table (sal-name-ref/decl ast))))
                ast))

;-------------------------------------------------------------------
;
; let-decl-list + expr --->  let-expr
;
; Remark: decl-list may have interdependencies, so we may have
; to create several nested let-exprs
; 
; Example: 
; let-decl-list (x::nat (+ y 1)) (y::nat 2) (w::nat (+ y 2)) (z::nat (+ x w))
; epxr (f x y z)
; is transformed into
;
; (let ((y::nat 2))
;   (let ((x::nat (+ y 1))
;         (w::nat (+ y 2)))
;     (let ((z::nat (+ x w)))
;       (f x y z))))
;
;-------------------------------------------------------------------
(define (make-sal-let-expr let-decl-list expr)
  (let* ((decl-table (sal-decl-list->eq-table let-decl-list))
         (uses-another-let-decl? (lambda (let-decl)
                                   (sal-ast/contains-reference-in-table? let-decl decl-table))))
    (let loop ((let-decl-list let-decl-list))
      (if (null? let-decl-list)
        expr
        (let ((remaining-queue (make-queue)) ;; decls that couldn't be processed in this iteration
              (current-queue (make-queue))) ;; decls that were processes in this iteration
          (for-each (lambda (let-decl)
                      (if (uses-another-let-decl? let-decl) 
                        (queue/insert! remaining-queue let-decl)
                        (queue/insert! current-queue let-decl)))
                    let-decl-list)
          (when (queue/empty? current-queue)
            (error 'make-sal-let-expr "Invalid use of function, there is a causal cycle in the list of let-declarations." #unspecified))
          ;; remove the let-decls that can be processed in this iteration from the decl-table
          (for-each (lambda (let-decl)
                      (eq-hash-table/delete! decl-table let-decl))
                    (queue->list current-queue))
          (let ((nested-expr (loop (queue->list remaining-queue))))
            (make-ast-instance <sal-let-expr> expr
                               :local-decls (queue->list current-queue)
                               :expr nested-expr)))))))
                
(define (sal-decl-list/num-elements decl-list)      
  (if (null? decl-list)
    *mpq-zero*
    (fold-left (lambda (curr var-decl)
                 (ensure ((var-decl <sal-var-decl>))
                   (*mpq curr (sal-type/number-of-elements (slot-value var-decl :type)))))
               *mpq-one*
               decl-list)))

(define (sal-decl-list/sort decl-list)
  (sort decl-list
        (lambda (decl1 decl2)
          (symbol<? (sal-decl/name decl1) (sal-decl/name decl2)))))


