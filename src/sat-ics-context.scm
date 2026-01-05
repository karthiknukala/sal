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

(module sat-ics-context
        (include "sal.sch")
        (import sat-generic-context sat-context sal-ast-env dp-translation-support
                sal-ast-copy fast-hash-table queue sal-type sal-expression gmp-scheme
                ics-interface sat-generic-context-result front-end tmp-files)
        (export <sat-ics-context>
                (make-sat-ics-context place-provider))
        )

(define-class <sat-ics-context> (<sat-generic-context>) ())

(define (make-sat-ics-context place-provider)
  (sat-generic-context/init! (make-instance <sat-ics-context>) place-provider))

;; ------------------------------------------------------------------
;; Create ICS bridge for boolean variables
;;
;; Some boolean variables are handled by the SAT solver, but some
;; of them may be arguments of an uninterpreted function symbol, so
;; I need to force the SAT solver to send this information to ICS.
;; This is accomplished by creating new boolean variables that is an
;; instance of <sat-ics-bool-aux-decl>.
;; Example
;;   x & f(x,n) 
;;  ==>
;;   x & ((bx = true) <=> x) & f(bx,n)
;;
;; bx is an instance of <sat-ics-bool-aux-decl>
;; 
;; Remark: the constraint (bx = true) <=> x is sent to ICS, but
;; it is not included in the slot :constraint-queue
;; ------------------------------------------------------------------
(define-class <sat-ics-bool-aux-decl> (<sat-aux-decl>) ())
(define-class <ics-term-true> (<sal-true>) ())
(define-class <ics-term-false> (<sal-false>) ())
(define *empty-env* (make-empty-env))
(define-generic (sal-ast/create-bool-bridge ast uf-parent? decl-queue bool-bridge-table))
(define (sal-ast/create-bool-bridge-core ast uf-parent? decl-queue bool-bridge-table)
  (sal-ast/map ast *empty-env* (lambda (child-ast new-env)
                                 [assert (new-env) (eq? new-env *empty-env*)]
                                 (sal-ast/create-bool-bridge child-ast uf-parent? decl-queue bool-bridge-table))))
(define-method (sal-ast/create-bool-bridge (ast <sal-ast>) (uf-parent? <primitive>) (decl-queue <primitive>) (bool-bridge-table <primitive>))
  (sal-ast/create-bool-bridge-core ast #f decl-queue bool-bridge-table))
(define-method (sal-ast/create-bool-bridge (ast <sal-arg-tuple-literal>) (uf-parent? <primitive>) (decl-queue <primitive>) 
                                           (bool-bridge-table <primitive>))
  ;; maintain current value
  (sal-ast/create-bool-bridge-core ast uf-parent? decl-queue bool-bridge-table))
(define-method (sal-ast/create-bool-bridge (ast <sal-application>) (uf-parent? <primitive>) (decl-queue <primitive>) (bool-bridge-table <primitive>))
  (sal-ast/create-bool-bridge-core ast #t decl-queue bool-bridge-table))
(define-method (sal-ast/create-bool-bridge (ast <sal-propositional-application>) (uf-parent? <primitive>) (decl-queue <primitive>) 
                                           (bool-bridge-table <primitive>))
  (sal-ast/create-bool-bridge-core ast #f decl-queue bool-bridge-table))
(define-method (sal-ast/create-bool-bridge (ast <sal-eq>) (uf-parent? <primitive>) (decl-queue <primitive>) (bool-bridge-table <primitive>))
  (sal-ast/create-bool-bridge-core ast #f decl-queue bool-bridge-table))
(define-method (sal-ast/create-bool-bridge (ast <sal-diseq>) (uf-parent? <primitive>) (decl-queue <primitive>) (bool-bridge-table <primitive>))
  (sal-ast/create-bool-bridge-core ast #f decl-queue bool-bridge-table))
(define-method (sal-ast/create-bool-bridge (ast <sal-name-expr>) (uf-parent? <primitive>) (decl-queue <primitive>) (bool-bridge-table <primitive>))
  (if (and uf-parent? (sal-type/boolean? (sal-expr/type ast)))
    (cond
     ((eq-hash-table/get bool-bridge-table (slot-value ast :decl)) =>
      (lambda (entry)
        (make-sal-name-expr (cdr entry) ast)))
     (else
      (let* ((decl (slot-value ast :decl))
             (new-decl (change-ast-class decl <sat-ics-bool-aux-decl>)))
        (queue/insert! decl-queue new-decl)
        (eq-hash-table/put! bool-bridge-table decl new-decl)
        (make-sal-name-expr new-decl ast))))
    (call-next-method)))
(define-method (sal-ast/create-bool-bridge (ast <sal-true>) (uf-parent? <primitive>) (decl-queue <primitive>) (bool-bridge-table <primitive>))
  (if uf-parent?
    (change-ast-class ast <ics-term-true>)
    ast))
(define-method (sal-ast/create-bool-bridge (ast <sal-false>) (uf-parent? <primitive>) (decl-queue <primitive>) (bool-bridge-table <primitive>))
  (if uf-parent?
    (change-ast-class ast <ics-term-false>)
    ast))
  

(define (sat-context/create-bool-bridge! sat-context)
  (let ((decl-queue (slot-value sat-context :declaration-queue))
        (bool-bridge-table (make-eq-hash-table)))
    (sat-generic-context/apply-simple-transformation! sat-context
                                                      (lambda (expr)
                                                        (sal-ast/create-bool-bridge expr #f decl-queue bool-bridge-table))
                                                      "creating boolean bridge between ICS terms and SAT atoms")
    bool-bridge-table))

;; ------------------------------------------------------------------
;; Display boolean bridge
;;
;; ------------------------------------------------------------------
(define (display-boolean-bridge bool-bridge-table info)
  (eq-hash-table/for-each (lambda (decl bridge-decl)
                            (let ((id1 (dp-translation-info/var-id info decl))
                                  (id2 (dp-translation-info/var-id info bridge-decl)))
                              (print "  [" id1 " <=> [" id2 " = true ]] &")))
                          bool-bridge-table)
  (print "tt"))

;; ------------------------------------------------------------------
;; Display ICS formula
;;
;; ------------------------------------------------------------------

(define (lp par?)
  (if par? "(" "["))
(define (rp par?)
  (if par? ")" "]"))
  
(define-generic (sal-ast/display-ics ast info))
(define-method (sal-ast/display-ics (ast <sal-ast>) (info <dp-translation-info>))
  (sign-unsupported-feature ast "Failed to convert to an ICS expression."))

(define (sal-arg/display-ics arg par? info)
  (unless (instance-of? arg <sal-ast-leaf>)
    (display (lp par?)))
  (sal-ast/display-ics arg info)
  (unless (instance-of? arg <sal-ast-leaf>)
    (display (rp par?))))

(define (sal-infix-application/display-ics ast ics-op par? info)
  (let ((arg-list (sal-application/argument-list ast)))
    (sal-arg/display-ics (car arg-list) par? info)
    (for-each (lambda (arg)
                (display* " " ics-op " ")
                (sal-arg/display-ics arg par? info))
              (cdr arg-list))))
(define-method (sal-ast/display-ics (ast <sal-eq>) (info <dp-translation-info>))
  (if (sal-type/boolean? (sal-expr/type (sal-binary-application/arg1 ast)))
    (sal-infix-application/display-ics ast "<=>" #f info)
    (sal-infix-application/display-ics ast "=" #t info)))
(define-method (sal-ast/display-ics (ast <sal-diseq>) (info <dp-translation-info>))
  (if (sal-type/boolean? (sal-expr/type (sal-binary-application/arg1 ast)))
    (sal-infix-application/display-ics ast "#" #f info)
    (sal-infix-application/display-ics ast "<>" #t info)))
(define-method (sal-ast/display-ics (ast <sal-and>) (info <dp-translation-info>))
  (sal-infix-application/display-ics ast "&" #f info))
(define-method (sal-ast/display-ics (ast <sal-or>) (info <dp-translation-info>))
  (sal-infix-application/display-ics ast "|" #f info))
(define-method (sal-ast/display-ics (ast <sal-not>) (info <dp-translation-info>))
  (display "[~ ")
  (sal-arg/display-ics (slot-value ast :arg) #f info)
  (display "]"))
(define-method (sal-ast/display-ics (ast <sal-add>) (info <dp-translation-info>))
  (sal-infix-application/display-ics ast "+" #t info))
(define-method (sal-ast/display-ics (ast <sal-sub>) (info <dp-translation-info>))
  (sal-infix-application/display-ics ast "-" #t info))
(define-method (sal-ast/display-ics (ast <sal-mul>) (info <dp-translation-info>))
  (sal-infix-application/display-ics ast "*" #t info))
(define-method (sal-ast/display-ics (ast <sal-mod>) (info <dp-translation-info>))
  (sign-unsupported-feature ast "ICS does not support the operator 'mod'."))
(define-method (sal-ast/display-ics (ast <sal-div>) (info <dp-translation-info>))
  (let ((new-ast (div->mul ast)))
    (unless new-ast
      (sign-unsupported-feature ast "ICS does not support the operator '/'."))
    (sal-ast/display-ics new-ast info)))
(define-method (sal-ast/display-ics (ast <sal-idiv>) (info <dp-translation-info>))
  (sign-unsupported-feature ast "ICS does not support the operator 'div'."))
(define-method (sal-ast/display-ics (ast <sal-numeral>) (info <dp-translation-info>))
  (display (mpq->string (slot-value ast :num))))
(define-method (sal-ast/display-ics (ast <sal-lt>) (info <dp-translation-info>))
  (sal-infix-application/display-ics ast "<" #t info))
(define-method (sal-ast/display-ics (ast <sal-le>) (info <dp-translation-info>))
  (sal-infix-application/display-ics ast "<=" #t info))
(define-method (sal-ast/display-ics (ast <sal-gt>) (info <dp-translation-info>))
  (sal-infix-application/display-ics ast ">" #t info))
(define-method (sal-ast/display-ics (ast <sal-ge>) (info <dp-translation-info>))
  (sal-infix-application/display-ics ast ">=" #t info))
(define-method (sal-ast/display-ics (ast <sal-name-expr>) (info <dp-translation-info>))
  (eq-hash-table/put! (slot-value info :already-visited) (slot-value ast :decl) #unspecified)
  (display (dp-translation-info/var-id info (slot-value ast :decl))))
(define-method (sal-ast/display-ics (ast <sal-true>) (info <dp-translation-info>))
  (display "tt"))
(define-method (sal-ast/display-ics (ast <sal-false>) (info <dp-translation-info>))
  (display "ff"))
(define-method (sal-ast/display-ics (ast <ics-term-true>) (info <dp-translation-info>))
  (display "true"))
(define-method (sal-ast/display-ics (ast <ics-term-false>) (info <dp-translation-info>))
  (display "false"))
(define-method (sal-ast/display-ics (ast <sal-conditional>) (info <dp-translation-info>))
  [assert (ast) (sal-type/boolean? (sal-expr/type (slot-value ast :then-expr)))]
  (display "if [")
  (sal-ast/display-ics (slot-value ast :cond-expr) info)
  (display "] then [")
  (sal-ast/display-ics (slot-value ast :then-expr) info)
  (display "] else [")
  (sal-ast/display-ics (slot-value ast :else-expr) info)
  (display "] end"))
(define-method (sal-ast/display-ics (ast <sal-application>) (info <dp-translation-info>))
  (let ((pred? (sal-type/boolean? (sal-expr/type ast))))
    (display "sal_app(")
    (sal-ast/display-ics (slot-value ast :fun) info)
    (for-each (lambda (arg)
                (display ", ")
                (sal-ast/display-ics arg info))
              (sal-application/argument-list ast))
    (display ")")
    (when pred?
      (display "= true"))))
(define-method (sal-ast/display-ics (ast <sal-array-selection>) (info <dp-translation-info>))
  (cond
   (*sat-generic-context-eliminate-array-theory*
    (call-next-method))
   (else
    (sal-ast/display-ics (slot-value ast :fun) info)
    (display "[")
    (sal-ast/display-ics (slot-value ast :arg) info)
    (display "]"))))
(define-method (sal-ast/display-ics (ast <sal-array-update>) (info <dp-translation-info>))
  [assert (ast) (not *sat-generic-context-eliminate-array-theory*)]
  (sal-ast/display-ics (slot-value ast :target) info)
  (display "[")
  (sal-ast/display-ics (slot-value ast :idx) info)
  (display " := ")
  (sal-ast/display-ics (slot-value ast :new-value) info)
  (display "]"))

;; -----------------------------------------------------------------
;; Display constraint
;;
;;
;; -----------------------------------------------------------------
(define-generic (sal-constraint/display-ics ast info))

(define-method (sal-constraint/display-ics (ast <sal-int-pred>) (info <dp-translation-info>))
  (let ((arg (slot-value ast :arg)))
    (cond
     ((instance-of? arg <sal-name-expr>)
      (print "sig " (dp-translation-info/var-id info (slot-value arg :decl)) " : int."))
     (else
      (let ((alias-id1 (dp-translation-info/new-alias-id info))
            (fresh-var (dp-translation-info/fresh-var-id info)))
        (print "sig " fresh-var " : int.")
        (display* "prop " alias-id1 " := " fresh-var " = ")
        (sal-ast/display-ics arg info)
        (print " ."))))))

(define-method (sal-constraint/display-ics (ast <sal-real-pred>) (info <dp-translation-info>))
  ;; do nothing
  #unspecified)

(define-method (sal-constraint/display-ics (ast <sal-ast>) (info <dp-translation-info>))
  (let ((alias-id1 (dp-translation-info/new-alias-id info)))
    (display* "prop " alias-id1 " := ")
    (sal-ast/display-ics ast info)
    (print " .")))

(define-method (sal-constraint/display-ics (ast <sal-eq>) (info <dp-translation-info>))
  (multiple-value-bind
      (lhs rhs)
      (sal-binary-application/arguments ast)
    (cond
     ((and (instance-of? lhs <sal-name-expr>)
           (instance-of? (slot-value lhs :decl) <sat-aux-decl>)
           (not (eq-hash-table/get (slot-value info :already-visited) (slot-value lhs :decl)))
           (sal-type/boolean? (sal-expr/type lhs))) ;; To avoid but related to (sig x : int) (def x := ...)
      (display* "prop " (dp-translation-info/var-id info (slot-value lhs :decl)) " := ")
      (sal-ast/display-ics rhs info)
      (print " ."))
     (else
      (call-next-method)))))

;; ------------------------------------------------------------------
;; Display satisfiability problem
;;
;; ------------------------------------------------------------------

(define (display-int-pred expr info)
  (let ((arg (slot-value expr :arg)))
    (when (instance-of? arg <sal-name-expr>)
      (print "sig " (dp-translation-info/var-id info (slot-value arg :decl)) " : int."))))
    
(define (print-ics-header) 
  (print "%% Automatically generated file (don't edit)")
  (display "%% Generated by: sal-inf-bmc ")
  (for-each (lambda (arg) (display* arg " ")) *arguments*)
  (print ""))

(define (sat-context/display-ics! sat-context bool-problem?)   
  (let* ((bool-bridge (if bool-problem? #unspecified (sat-context/create-bool-bridge! sat-context)))
         (info (make-dp-translation-info)))
    (print-ics-header)
    (unless bool-problem?
      (queue/insert! (slot-value info :alias-queue) "bb"))
    ;; define the constraints
    (for-each (lambda (expr)
                (sal-constraint/display-ics expr info))
              (queue->list (slot-value sat-context :constraint-queue)))
    ;; define the boolean bridge
    (unless bool-problem?
      (print "prop bb := ")
      (display-boolean-bridge bool-bridge info)
      (print "."))
    ;; print the satisfiability problem
    (print "sat ")
    (display-n-ary-infix-op "&" (queue->list (slot-value info :alias-queue)))
    (print ".")
    (slot-value info :id->decl-mapping)))

(define-method (sat-context/solve (ctx <sat-ics-context>))
  (sat-generic-context/simplify! ctx
                                 :ite->ite-bool? #t
                                 :eliminate-div-mod? #t
                                 :eliminate-int-diseq? #t)
  ;; (sat-generic-context/pp ctx)
  (sal/set-ics-in-tmp-file!)
  (let* ((id->decl-mapping (with-output-to-file *sal-tmp-in-file-to-ics*
                             (lambda () (sat-context/display-ics! ctx #f))))
         (id->decl-mapping-proc (lambda (id)
                                  (cond
                                   ((eq-hash-table/get id->decl-mapping id) => cdr)
                                   (else #f))))
         (constraint-queue (slot-value ctx :constraint-queue)))
    (if (queue/empty? constraint-queue)
      #t
      (let ((place-provider (queue/front constraint-queue)))
        (verbose-message 3 "  formula size: ~a nodes" (sat-generic-context/size ctx))
        (verbose-message 2 "  executing ICS...")
        (let ((result (unwind-protect
                       (ics/execute *sal-tmp-in-file-to-ics* id->decl-mapping-proc place-provider)
                       (sal/delete-tmp-file! *sal-tmp-in-file-to-ics*))))
          (and result
               (make-instance <sat-generic-context-result> 
                              :sat-context ctx
                              :result-constraint-list result)))))))

