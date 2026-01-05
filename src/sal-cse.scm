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

(module sal-cse
        (include "sal.sch")
        (include "sal-ast-table.sch")
        (include "fast-hash-table.sch")
        (import sal-ast-for-each sal-expression sal-type sal-ast-copy sal-ast-env 
                unique-names sal-decls sal-ast-simplify queue sal-module
                sal-assertion runtime sal-pp)
        (export (sal-ast/cse ast)
                (sal-cse/set-maximum-number-of-iterations! max)
                (sal-ast/pull-let-decls ast)
                (sal-cse/compute-occurrences ast occ-table)
                (sal-cse/filter-occurrences ast occ-table result-table)
                *sal-cse-maximum-number-of-iterations*)
        )

;; common subexpression elimination

(define (add-occurrence! table ast)
  (let* ((entry (sal-ast-table/get table ast)))
    (cond
     (entry
      (set-cdr! entry (+ (cdr entry) 1))
      (cdr entry))
     (else (sal-ast-table/put! table ast 1)
           1))))

(define *occ-table* #unspecified)
(define *result-table* #unspecified)

(define (sal-cse/filter-occurrences ast occ-table result-table)
  (dlet ((*occ-table* occ-table)
         (*result-table* result-table))
    (sal-cse/filter-occurrences-core ast)))

(define-generic (sal-cse/filter-occurrences-core ast))

(define-method (sal-cse/filter-occurrences-core (ast <sal-ast>))
  (sal-ast/for-each-children sal-cse/filter-occurrences-core ast))


(define-method (sal-cse/filter-occurrences-core :around (ast <sal-expr>))
  (let ((entry (sal-ast-table/get *occ-table* ast)))
    (cond
     ((and entry (> (cdr entry) 1))
      (let* ((var-name (gen-unique-name 'cs))
             (id (make-sal-identifier ast var-name))
             (decl (make-ast-instance <sal-let-decl> ast
                                      :id id
                                      :type (sal-expr/type ast)
                                      :value ast)))
        (sal-ast-table/put! *result-table* ast decl)))
     (else
      (call-next-method)))))
    
(define (filter-occurrences root occ-table)
  (dlet ((*result-table* (make-sal-ast-table))
         (*occ-table* occ-table))
    (sal-cse/filter-occurrences-core root)
    *result-table*))


(define (sal-cse/compute-occurrences ast occ-table)
  (dlet ((*occ-table* occ-table))
    (sal-cse/compute-occurrences-core ast)))

(define *idx* 0)

(define (compute-common-subexpressions ast)
  (dlet ((*occ-table* (make-sal-ast-table)))
    (sal-cse/compute-occurrences-core ast)
    (set! *idx* (+ *idx* 1))
;     (with-output-to-file (string-append "cse" (number->string *idx*))
;       (lambda ()
;         (sal-ast-table/for-each (lambda (ast val)
;                                   (sal/pp ast)
;                                   (print "")
;                                   (print "hash: " (sal-ast/hash ast))
;                                   (print "occ: " val)
;                                   (print "----------"))
;                                 *occ-table*)))
    (filter-occurrences ast *occ-table*)))

(define-generic (sal-cse/compute-occurrences-core ast))

(define-inline (compute-occurrences/process-children ast)
  (sal-ast/for-each-children sal-cse/compute-occurrences-core ast))

(define-method (sal-cse/compute-occurrences-core (ast <sal-ast>))
  (compute-occurrences/process-children ast))

(define-method (sal-cse/compute-occurrences-core (ast <sal-type>))
  ;; do nothing
  #unspecified)

(define-method (sal-cse/compute-occurrences-core (ast <sal-module>))
  ;; do nothing
  #unspecified)

(define-method (sal-cse/compute-occurrences-core (ast <sal-arg-tuple-literal>))
  ;; I do not want to simplify a tuple literal that is just storing the arguments of an application...
  (compute-occurrences/process-children ast))
  
(define-method (sal-cse/compute-occurrences-core (ast <sal-ast-leaf>))
  ;; do nothing
  #unspecified)

(define-method (sal-cse/compute-occurrences-core (ast <sal-qualified-name-ref>))
  ;; do nothing
  #unspecified)

(define-method (sal-cse/compute-occurrences-core (ast <sal-expr>))
  (let ((num-occs (add-occurrence! *occ-table* ast)))
    [assert (num-occs) (>= num-occs 1)]
    (when (= num-occs 1) ;; if the number of occurrences is greater than one, doesn't make sense to analyze the children
      (compute-occurrences/process-children ast))))

(define-method (sal-cse/compute-occurrences-core (ast <sal-definition-expression>))
  (let ((num-occs (add-occurrence! *occ-table* ast)))
    [assert (num-occs) (>= num-occs 1)]
    (when (= num-occs 1) ;; if the number of occurrences is greater than one, doesn't make sense to analyze the children
      ;; I must not process the :lhs-list slot
      (sal-cse/compute-occurrences-core (slot-value ast :expr)))))

(define-method (sal-cse/compute-occurrences-core (ast <sal-assignment>))
  (let ((num-occs (add-occurrence! *occ-table* ast)))
    [assert (num-occs) (>= num-occs 1)]
    (when (= num-occs 1) ;; if the number of occurrences is greater than one, doesn't make sense to analyze the children
      ;; I must not process the lhs of the assignment
      (sal-cse/compute-occurrences-core (sal-binary-application/arg2 ast)))))

(define-method (sal-cse/compute-occurrences-core (ast <sal-name-expr>))
  ;; do nothing
  #unspecified)

(define-method (sal-cse/compute-occurrences-core (ast <sal-numeral>))
  ;; do nothing
  #unspecified)

(define-method (sal-cse/compute-occurrences-core (ast <sal-string-expr>))
  ;; do nothing
  #unspecified)

(define-method (sal-cse/compute-occurrences-core (ast <sal-next-operator>))
  ;; do nothing
  #unspecified)

;; The following generic procedure is used to compute where the new declarations will be inserted in the code.
;; Remark: a new declaration is created for each common subexpression
;; - cs-table: is a mapping from common subexpressions to a new (auxiliar) declaration (read-only)
;; - open-references: is a set (i.e., table) of declarations that are referenced by common subexpressions,
;;                    we can say this is the table of free variables that shows up in common subexpressions.
;;                    (read-only)
;;                    this table is only used to speed up the computation.
;; - to-process-new-decls: is a new table that is used to store the decls that we should place.
;;                         this declarations are in the range of cs-table. this is a read-write table.
;; - insertion-places: is a mapping from ast nodes to declarations that should be inserted in that place.
;;                     (read-write)  
(define-generic (places-to-insert-new-decls ast cs-table open-references to-process-new-decls insertion-places))

(define-method (places-to-insert-new-decls :around (ast <sal-expr>) (cs-table <primitive>) (open-references <primitive>) 
                                           (to-process-new-decls <primitive>) (insertion-places <primitive>))
  (cond 
   ((sal-ast-table/get cs-table ast) =>
    (lambda (entry)
      (eq-hash-table/put! to-process-new-decls (cdr entry) #unspecified)))
   (else
    (call-next-method))))

(define-method (places-to-insert-new-decls (ast <sal-ast>) (cs-table <primitive>) (open-references <primitive>) 
                                           (to-process-new-decls <primitive>) (insertion-places <primitive>))
  (sal-ast/for-each-children (cut places-to-insert-new-decls <> cs-table open-references to-process-new-decls insertion-places) ast))

(define-method (places-to-insert-new-decls (ast <sal-new-binds-ast>) (cs-table <primitive>) (open-references <primitive>) 
                                           (to-process-new-decls <primitive>) (insertion-places <primitive>))
  (call-next-method) ;; process the children
  ;; check if one of the declarations is referenced in a common subexpression
  (when (sal-new-binds-ast/exists-new-bind (lambda (local-decl)
                                             (eq-hash-table/get open-references local-decl))
                                           ast)
    (let ((decls-to-insert-here '()))
      (eq-hash-table/for-each (lambda (new-decl _)
                                ;; if the new declaration contains a reference to a local declaration, then
                                ;; the new declaration should be inserted here
                                (when (sal-new-binds-ast/exists-new-bind 
                                       (lambda (local-decl) 
                                         (sal-ast/contains-reference? new-decl local-decl))
                                       ast)
                                  (push! new-decl decls-to-insert-here)))
                              to-process-new-decls)
      (unless (null? decls-to-insert-here)
        ;; remove the declaration that will be inserted here from the to-process-new-decls table
        (for-each (cut eq-hash-table/delete! to-process-new-decls <>) decls-to-insert-here)
        ;; add the current node to the insertion-places table
        ;; RANDOMIZATION PROBLEM AND SOLUTION
        ;; Remark: I have to sort the declarations to avoid differentt behavior when executing
        ;; sal several times with the same flags. SAL produces the same formulas, but
        ;; the let-decls are in a different order.
        ;; The let-decls are in a different order because eq-hash-table layout depends on
        ;; how objects are stored in memory, when I convert eq-hash-table (representing a set)
        ;; to a list, I can get a different list everytime I run SAL
        (eq-hash-table/put! insertion-places ast (sal-decl-list/sort decls-to-insert-here))))))
  
(define *cs-table* #unspecified)
(define *insertion-places* #unspecified)

(define (cse ast env cs-table insertion-places)
  (dlet ((*cs-table* cs-table)
         (*insertion-places* insertion-places))
    (cse-core ast env)))

(define-generic (cse-core ast env))

(define-method (cse-core (ast <sal-ast>) (env <primitive>))
  (sal-ast/map ast env cse-core))

(define-method (cse-core (ast <sal-local-binds-expr>) (env <primitive>))
  (let ((local-decls (slot-value ast :local-decls)))
    (cond
     ((eq-hash-table/get *insertion-places* ast) =>
      (lambda (entry)
        (let* ((new-decls (cdr entry))
               (new-local-decls (sal-ast/map-list local-decls env cse-core))
               (env (update-env* env local-decls new-local-decls))
               (new-new-decls (map (cut sal-ast/substitute <> env) new-decls)))
          (let* ((env (update-env* env new-decls new-new-decls))
                 (new-expr (cse-core (slot-value ast :expr) env)))
            (copy-ast ast
                      :local-decls new-local-decls
                      :expr (make-ast-instance <sal-let-expr> ast
                                               :local-decls new-new-decls
                                               :expr new-expr))))))
     (else
      (call-next-method)))))

(define-method (cse-core :around (ast <sal-expr>) (env <primitive>))
  (cond
   ((sal-ast-table/get *cs-table* ast) =>
    (lambda (entry)
      (let ((decl (cdr entry)))
        (cond
         ((lookup-env decl env) =>
          (lambda (ref)
            (if (instance-of? ref <sal-decl>)
              (make-sal-name-expr ref ast)
              ref)))
         (else
          (make-sal-name-expr decl ast))))))
   (else
    (call-next-method))))

(define-method (cse-core (ast <sal-definition-expression>) (env <primitive>))
  (update-ast-slots ast
                    :expr (cse-core (slot-value ast :expr) env)))
(define-method (cse-core (ast <sal-assignment>) (env <primitive>))
  (multiple-value-bind 
      (lhs rhs)
      (sal-binary-application/arguments ast)
    (update-ast-slots ast
                      :arg (make-application-argument lhs 
                                                      (cse-core rhs env)))))

(define *sal-cse-maximum-number-of-iterations* 16)

(define (sal-cse/set-maximum-number-of-iterations! max)
  (set! *sal-cse-maximum-number-of-iterations* max))

(define-generic (sal-ast/cse ast)
  :doc "Remove common subexpression of the given argument.")

(define-method (sal-ast/cse (ast <sal-ast>))
  (sign-unsupported-feature ast "This abstract syntax tree node does not support common subexpression elimination."))

(define-method (sal-ast/cse (expr <sal-expr>))
  (verbose-message 2 "  eliminating common subexpressions...")
  (let loop ((it 0)
             (expr (sal-ast/unique-decls expr)))
    ;; (breakpoint "cse" (expr) #t)
    (if (>= it *sal-cse-maximum-number-of-iterations*)
      expr
      (let ((cs-table (compute-common-subexpressions expr)))
        (if (= (sal-ast-table/size cs-table) 0)
          (cond
           ((> it 0)
            (verbose-message 3 "    simplifying result of the common subexpression elimination procedure...")
            (sal-ast/simplify expr))
           (else
            (verbose-message 3 "    expression doesn't contain common (shared) subexpressions...")
            expr))
          (let ((open-references (make-eq-hash-table)))
            (verbose-message 3 "    starting a new iteration of the common subexpression elimination procedure...")
            (sal-ast-table/for-each (lambda (ast aux-decl)
                                      (let ((curr-open-references (sal-ast/open-reference-list ast)))
                                        (for-each (lambda (ref-decl)
                                                    (eq-hash-table/put! open-references ref-decl #unspecified))
                                                  curr-open-references)
                                        ))
                                    cs-table)
            (let ((to-process-new-decls (make-eq-hash-table))
                  (insertion-places (make-eq-hash-table)))
              (places-to-insert-new-decls expr cs-table open-references to-process-new-decls insertion-places)
              (let ((new-expr (cse expr (make-empty-env) cs-table insertion-places)))
                (let ((new-decl-list '()))
                  (eq-hash-table/for-each (lambda (new-decl _)
                                            (push! new-decl new-decl-list))
                                          to-process-new-decls)
                  (let ((new-expr (if (null? new-decl-list)
                                    new-expr
                                    (make-ast-instance <sal-let-expr> expr
                                                       ;; See RANDOMIZATION PROBLEM AND SOLUTION remark above
                                                       :local-decls (sal-decl-list/sort new-decl-list)
                                                       :expr new-expr))))
                    (loop (+ it 1)
                          new-expr)))))))))))

(define-method (sal-ast/cse (flat-module <sal-flat-module>))
  (status-message :executing-cse)
  (verbose-message 1 "eliminating common subexpressions in a flat module...")
  (display-runtime 2 "  flat module -- common subexpression elimination time: ~a secs"
    (lambda ()
      (update-ast-slots flat-module
                        :transition (sal-ast/cse (slot-value flat-module :transition))
                        :initialization (sal-ast/cse (slot-value flat-module :initialization))
                        :definition (sal-ast/cse (slot-value flat-module :definition))
                        :valid-input-expr (sal-ast/cse (slot-value flat-module :valid-input-expr))
                        :valid-state-expr (sal-ast/cse (slot-value flat-module :valid-state-expr))
                        :valid-constant-expr (sal-ast/cse (slot-value flat-module :valid-constant-expr))))
    :cse-time))

(define-method (sal-ast/cse (assertion <sal-assertion-expr>))
  (status-message :executing-cse)
  (verbose-message 1 "eliminating common subexpressions in an assertion...")
  (display-runtime 2 "  assertion -- common subexpression elimination time: ~a secs"
    (lambda ()
      (sal-assertion/transformation-core assertion (make-empty-env) (lambda (module env) (sal-ast/cse module))))
    :cse-time))

;;
;; Pull let-decls implementation
;;
;; It is worth to try to pull let-decls before performing common subexpression elimination.
;; 
;; Remark: This is not a SAFE transformation, unless you assume a "lazy" semantincs for let-decls
;; Example:  (if (null? lst) 
;;             0
;;             (let ((x::nat (car lst)))
;;               x))
;;
;; will be transformed into:
;;
;; (let ((x::nat (car lst)))
;;   (if (null? lst)
;;      0
;;      x))
;;


;; the function collect <sal-let-decls> that can be pulled.
;; let-decls is a sal-ast-table  <sal-let-decl> ---> list of <sal-let-decl>
;; the list contains clones of a given entry.
(define-generic (collect-let-decls ast let-decls))

(define (add-let-decl! let-decls new-let-decl)
  (cond
   ((sal-ast-table/get let-decls new-let-decl) =>
    (lambda (entry)
      (unless (eq? new-let-decl (car entry))
        (sal-ast-table/put! let-decls (car entry) (cons new-let-decl (cdr entry))))))
   (else
    (sal-ast-table/put! let-decls new-let-decl '()))))

(define-inline (collect-let-decls-default ast let-decls)
  (sal-ast/for-each-children (cut collect-let-decls <> let-decls) ast))

(define-method (collect-let-decls (ast <sal-ast>) (let-decls <primitive>))
  (collect-let-decls-default ast let-decls))

(define-method (collect-let-decls (ast <sal-type>) (let-decls <primitive>))
  #unspecified)

(define-method (collect-let-decls (ast <sal-module>) (let-decls <primitive>))
  #unspecified)

(define-method (collect-let-decls (ast <sal-flat-module>) (let-decls <primitive>))
  (collect-let-decls-default ast let-decls))

(define-method (collect-let-decls (ast <sal-assertion-expr>) (let-decls <primitive>))
  #unspecified)

(define-method (collect-let-decls (ast <sal-let-decl>) (let-decls <primitive>))
  (call-next-method)
  (add-let-decl! let-decls ast))

(define-method (collect-let-decls (ast <sal-let-expr>) (let-decls <primitive>))
  (collect-let-decls-default ast let-decls))
  
(define-method (collect-let-decls (ast <sal-new-binds-ast>) (let-decls <primitive>))
  (call-next-method)
  (let ((to-remove '()))
    (sal-ast-table/for-each (lambda (curr lst)
                              (when (sal-new-binds-ast/exists-new-bind (lambda (decl)
                                                                         (or (sal-ast/contains-reference? curr decl)
                                                                             (exists (cut sal-ast/contains-reference? <> decl) lst)))
                                                                       ast)
                                ;; I cannot lift the let-decl...
                                (push! curr to-remove)))
                            let-decls)
    (for-each (lambda (curr)
                (sal-ast-table/delete! let-decls curr))
              to-remove)))

;; return true, if let-decl uses an element in let-decl-list
;; this function is used to partially order the let-decls
(define (uses-another-let-decl? let-decl let-decl-list)
  (sal-ast/find (lambda (ast)
                  (and (instance-of? ast <sal-name-expr>)
                       (memq (slot-value ast :decl) let-decl-list)))
                let-decl))

;; partially order the let decls based on the "uses" relationship.
;; the result is a queue of queues.
(define (distribute-let-decls let-decl-table)
  (let ((let-decls (sal-ast-table/fold-keys (lambda (lst let-decl)
                                              (cons let-decl lst))
                                            '()
                                            let-decl-table))
        (result (make-queue)))
    (let loop ((let-decls let-decls)
               (curr-queue (make-queue)))
      (unless (null? let-decls)
        (let ((remaining-let-decls (filter (lambda (let-decl)
                                             (cond
                                              ((not (uses-another-let-decl? let-decl let-decls))
                                               (queue/insert! curr-queue let-decl)
                                               #f)
                                              (else
                                               #t)))
                                           let-decls)))
          [assert (curr-queue) (not (queue/empty? curr-queue))]
          (queue/insert! result curr-queue)
          (loop remaining-let-decls (make-queue)))))
    result))
    
; (define (print-let-decls-queues queues)
;   (let ((level 0))
;     (for-each (lambda (queue)
;                 (print "level " level ":")
;                 (for-each (lambda (decl)
;                             (sal/pp decl)
;                             (print ""))
;                           (queue->list queue))
;                 (print "---------------------")
;                 (set! level (+ level 1)))
;               (queue->list queues))))

;; Remove pulled let-decls.
;; pulled-let-decls is a eq-hash-table
(define-generic (remove-pulled-let-decls ast env pulled-let-decls))

(define (mk-proc-child-for-remove pulled-let-decls)
  (lambda (ast env) 
    (remove-pulled-let-decls ast env pulled-let-decls)))

(define-method (remove-pulled-let-decls (ast <sal-ast>) (env <primitive>) (pulled-let-decls <primitive>))
  (sal-ast/map ast env (mk-proc-child-for-remove pulled-let-decls)))

(define-method (remove-pulled-let-decls (ast <sal-let-expr>) (env <primitive>) (pulled-let-decls <primitive>))
  (let* ((local-decls (slot-value ast :local-decls))
         (remaining-local-decls (filter (lambda (local-decl)
                                          (not (eq-hash-table/get pulled-let-decls local-decl)))
                                        local-decls))
         (new-local-decls (sal-ast/map-list remaining-local-decls env (mk-proc-child-for-remove pulled-let-decls)))
         (env (update-env* env remaining-local-decls new-local-decls))
         (new-body (sal-ast/map (slot-value ast :expr) env (mk-proc-child-for-remove pulled-let-decls))))
    (if (null? new-local-decls)
      new-body
      (copy-ast ast
                :local-decls new-local-decls
                :expr new-body))))

(define (let-decl-table->pulled-let-decls let-decl-table)
  (let ((result (make-eq-hash-table)))
    (sal-ast-table/for-each (lambda (let-decl let-decl-list)
                              (eq-hash-table/put! result let-decl #unspecified)
                              (for-each (lambda (let-decl)
                                          (eq-hash-table/put! result let-decl #unspecified))
                                        let-decl-list))
                            let-decl-table)
    result))

;; update the environment with:
;; 1)  curr-let-decl --> new-let-decl
;; 2)  clones of curr-let-decl --> new-let-decl
;; 
;; let-decl-table is used to access the clones of curr-let-decl
(define (make-new-env-using env curr-let-decls new-let-decls let-decl-table)
  [assert (curr-let-decls new-let-decls) (= (length curr-let-decls) (length new-let-decls))]
  (let ((env (update-env* env curr-let-decls new-let-decls)))
    ;; I also need to put in the environment the clones of curr-let-decls
    (let loop ((curr-let-decls curr-let-decls)
               (new-let-decls new-let-decls)
               (env env))
      (if (null? curr-let-decls)
        env
        (let* ((curr-let-decl (car curr-let-decls))
               (new-let-decl (car new-let-decls))
               (entry (sal-ast-table/get let-decl-table curr-let-decl))
               (clones-of-curr-let-decl (if entry (cdr entry) '())))
          (let inner-loop ((clones-of-curr-let-decl clones-of-curr-let-decl)
                      (env env))
            (if (null? clones-of-curr-let-decl)
              (loop (cdr curr-let-decls) (cdr new-let-decls) env)
              (let ((new-env (update-env env (car clones-of-curr-let-decl) new-let-decl)))
                (inner-loop (cdr clones-of-curr-let-decl) new-env)))))))))

(define-generic (sal-ast/pull-let-decls ast))

(define-method (sal-ast/pull-let-decls (ast <sal-ast>))
  (sign-unsupported-feature ast "This abstract syntax tree node does not support the `pull let declarations' transformation."))

(define-method (sal-ast/pull-let-decls (expr <sal-expr>))
  (verbose-message 2 "  pulling let declarations...")
  (let ((expr (sal-ast/unique-decls expr))
        (let-decl-table (make-sal-ast-table)))
    (collect-let-decls expr let-decl-table)
    (let ((pulled-let-decls (let-decl-table->pulled-let-decls let-decl-table))
          (let-decls-queues (distribute-let-decls let-decl-table))) ;; queue of queue of let-decls
      ;; (print-let-decls-queues let-decls-queues)
      (let loop ((let-decls-queues (queue->list let-decls-queues))
                 (env (make-empty-env)))
        (if (null? let-decls-queues)
          (remove-pulled-let-decls expr env pulled-let-decls)
          (let* ((curr-let-decls (queue->list (car let-decls-queues)))
                 (new-let-decls (map (cut remove-pulled-let-decls <> env pulled-let-decls) curr-let-decls))
                 (new-env (make-new-env-using env curr-let-decls new-let-decls let-decl-table))
                 (body (loop (cdr let-decls-queues) new-env)))
            (make-ast-instance <sal-let-expr> (car new-let-decls)
                               :local-decls new-let-decls
                               :expr body)))))))
                                 
(define-method (sal-ast/pull-let-decls (flat-module <sal-flat-module>))
  (verbose-message 1 "pulling let decls in the flat module...")
  (update-ast-slots flat-module
                    :transition (sal-ast/pull-let-decls (slot-value flat-module :transition))
                    :initialization (sal-ast/pull-let-decls (slot-value flat-module :initialization))
                    :definition (sal-ast/pull-let-decls (slot-value flat-module :definition))
                    :valid-input-expr (sal-ast/pull-let-decls (slot-value flat-module :valid-input-expr))
                    :valid-state-expr (sal-ast/pull-let-decls (slot-value flat-module :valid-state-expr))
                    :valid-constant-expr (sal-ast/pull-let-decls (slot-value flat-module :valid-constant-expr))))

(define-method (sal-ast/pull-let-decls (assertion <sal-assertion-expr>))
  (sal-assertion/transformation-core assertion (make-empty-env) 
                                     (lambda (module env) 
                                       (sal-ast/pull-let-decls module))))
