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

(module sat-svc-context
        (include "sal.sch")
        (import sat-generic-context sat-context sal-ast-env dp-translation-support
                sal-ast-copy fast-hash-table queue sal-type sal-expression gmp-scheme
                svc-interface sat-generic-context-result tmp-files)
        (export <sat-svc-context>
                (make-sat-svc-context place-provider))
        )

(define-class <sat-svc-context> (<sat-generic-context>) ())

(define (make-sat-svc-context place-provider)
  (sat-generic-context/init! (make-instance <sat-svc-context>) place-provider))

;; ------------------------------------------------------------------
;; Display SVC formulas
;;
;; ------------------------------------------------------------------

(define-generic (sal-ast/display-svc ast info))
(define-method (sal-ast/display-svc (ast <sal-ast>) (info <dp-translation-info>))
  (sign-unsupported-feature ast "Failed to convert to an SVC expression."))

(define (sal-application/display-svc ast svc-op info)
  (display* "( " svc-op)
  (for-each (lambda (arg)
              (display " ")
              (sal-ast/display-svc arg info))
            (sal-application/argument-list ast))
  (display " )"))

(define-method (sal-ast/display-svc (ast <sal-eq>) (info <dp-translation-info>))
  (if (sal-type/boolean? (sal-expr/type (sal-binary-application/arg1 ast)))
    (sal-application/display-svc ast "<=>" info)
    (sal-application/display-svc ast "=" info)))
(define-method (sal-ast/display-svc (ast <sal-diseq>) (info <dp-translation-info>))
  (if (sal-type/boolean? (sal-expr/type (sal-binary-application/arg1 ast)))
    (sal-application/display-svc ast "xor" info)
    (sal-application/display-svc ast "/=" info)))
(define-method (sal-ast/display-svc (ast <sal-and>) (info <dp-translation-info>))
  (sal-application/display-svc ast "and" info))
(define-method (sal-ast/display-svc (ast <sal-or>) (info <dp-translation-info>))
  (sal-application/display-svc ast "or" info))
(define-method (sal-ast/display-svc (ast <sal-not>) (info <dp-translation-info>))
  (sal-application/display-svc ast "not" info))
(define-method (sal-ast/display-svc (ast <sal-add>) (info <dp-translation-info>))
  (sal-application/display-svc ast "+" info))
(define-method (sal-ast/display-svc (ast <sal-sub>) (info <dp-translation-info>))
  (display "( + ")
  (let ((first? #t))
    (for-each (lambda (arg) 
                (display " ")
                (unless first? 
                  (display "(* -1 "))
                (sal-ast/display-svc arg info)
                (unless first?
                  (display " )"))
                (set! first? #f))
              (sal-application/argument-list ast))
    (display " )")))
(define-method (sal-ast/display-svc (ast <sal-mul>) (info <dp-translation-info>))
  (sal-application/display-svc ast "*" info))
(define-method (sal-ast/display-svc (ast <sal-mod>) (info <dp-translation-info>))
  (sign-unsupported-feature ast "SVC does not support the operator 'mod'."))
(define-method (sal-ast/display-svc (ast <sal-div>) (info <dp-translation-info>))
  (let ((new-ast (div->mul ast)))
    (unless new-ast
      (sign-unsupported-feature ast "SVC does not support the operator '/'."))
    (sal-ast/display-svc new-ast info)))
(define-method (sal-ast/display-svc (ast <sal-idiv>) (info <dp-translation-info>))
  (sign-unsupported-feature ast "SVC does not support the operator 'div'."))
(define-method (sal-ast/display-svc (ast <sal-numeral>) (info <dp-translation-info>))
  (let ((num (slot-value ast :num)))
    (if (mpq/integer? num)
      (display* "( " (mpq->string num) " )")
      (display* "( " (mpq->string (mpq/numerator num)) "|" (mpq->string (mpq/denominator num)) " )"))))
(define-method (sal-ast/display-svc (ast <sal-lt>) (info <dp-translation-info>))
  (sal-application/display-svc ast "<" info))
(define-method (sal-ast/display-svc (ast <sal-le>) (info <dp-translation-info>))
  (sal-application/display-svc ast "<=" info))
(define-method (sal-ast/display-svc (ast <sal-gt>) (info <dp-translation-info>))
  (sal-application/display-svc ast ">" info))
(define-method (sal-ast/display-svc (ast <sal-ge>) (info <dp-translation-info>))
  (sal-application/display-svc ast ">=" info))
(define-method (sal-ast/display-svc (ast <sal-name-expr>) (info <dp-translation-info>))
  (display "( ")
  (eq-hash-table/put! (slot-value info :already-visited) (slot-value ast :decl) #unspecified)
  (display (dp-translation-info/var-id info (slot-value ast :decl)))
  (display " )"))
(define-method (sal-ast/display-svc (ast <sal-true>) (info <dp-translation-info>))
  (display "( true )"))
(define-method (sal-ast/display-svc (ast <sal-false>) (info <dp-translation-info>))
  (display "( false )"))
(define-method (sal-ast/display-svc (ast <sal-conditional>) (info <dp-translation-info>))
  (display "( ite ")
  (sal-ast/display-svc (slot-value ast :cond-expr) info)
  (display " ")
  (sal-ast/display-svc (slot-value ast :then-expr) info)
  (display " ")
  (sal-ast/display-svc (slot-value ast :else-expr) info)
  (display " )"))
(define-method (sal-ast/display-svc (ast <sal-application>) (info <dp-translation-info>))
  (display "( ")
  (let ((fun (slot-value ast :fun)))
    (unless (instance-of? fun <sal-name-expr>)
      (sign-unsupported-feature ast "SVC do not support higher order functions."))
    (display (dp-translation-info/var-id info (slot-value fun :decl)))
    (for-each (lambda (arg)
                (display " ")
                (sal-ast/display-svc arg info))
              (sal-application/argument-list ast))
    (display " )")))
(define-method (sal-ast/display-svc (ast <sal-array-selection>) (info <dp-translation-info>))
  (cond
   (*sat-generic-context-eliminate-array-theory*
    (call-next-method))
   (else
    (display "( read ")
    (sal-ast/display-svc (slot-value ast :fun) info)
    (display " ")
    (sal-ast/display-svc (slot-value ast :arg) info)
    (display " )"))))
(define-method (sal-ast/display-svc (ast <sal-array-update>) (info <dp-translation-info>))
  [assert (ast) (not *sat-generic-context-eliminate-array-theory*)]
  (display "( write ")
  (sal-ast/display-svc (slot-value ast :target) info)
  (display "  ")
  (sal-ast/display-svc (slot-value ast :idx) info)
  (display "  ")
  (sal-ast/display-svc (slot-value ast :new-value) info)
  (display " )"))

;; -----------------------------------------------------------------
;; Display constraint
;;
;;
;; -----------------------------------------------------------------
(define-generic (sal-constraint/display-svc ast info))

(define-method (sal-constraint/display-svc (ast <sal-int-pred>) (info <dp-translation-info>))
  ;; do nothing...
  #unspecified)

(define-method (sal-constraint/display-svc (ast <sal-ast>) (info <dp-translation-info>))
  (let ((alias-id1 (dp-translation-info/new-alias-id info)))
    (display* "set $" alias-id1 " ")
    (sal-ast/display-svc ast info)
    (print "")))

;; ------------------------------------------------------------------
;; Display satisfiability problem
;;
;; ------------------------------------------------------------------

(define (sat-context/display-svc! sat-context)   
  (let ((info (make-dp-translation-info)))
    ;; define the constraints
    (for-each (lambda (expr)
                (sal-constraint/display-svc expr info))
              (queue->list (slot-value sat-context :constraint-queue)))
    ;; print the satisfiability problem
    (print "check_valid ( not ( and ")
    (for-each (lambda (alias)
                (display* " $" alias))
              (queue->list (slot-value info :alias-queue)))
    (print " ) )\n quit")))

(define-method (sat-context/solve (ctx <sat-svc-context>))
  (sat-generic-context/simplify! ctx :ite->ite-bool? #f)
  (sal/set-svc-in-tmp-file!)
  (with-output-to-file *sal-tmp-in-file-to-svc*
    (lambda () (sat-context/display-svc! ctx)))
  (verbose-message 3 "  formula size: ~a nodes" (sat-generic-context/size ctx))
  (verbose-message 2 "  executing SVC...")
  (unwind-protect
   (svc/execute *sal-tmp-in-file-to-svc*)
   (sal/delete-tmp-file! *sal-tmp-in-file-to-svc*)))

