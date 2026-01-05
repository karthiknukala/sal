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

(module sat-cvcl-context
        (include "sal.sch")
        (import sat-generic-context sat-context sal-ast-env dp-translation-support
                sal-ast-copy fast-hash-table queue sal-type sal-expression gmp-scheme
                cvcl-interface sat-generic-context-result tmp-files)
        (export <sat-cvcl-context>
                <sat-cvc-context>
                (sal/enable-cvcl-integers! flag)
                (make-sat-cvcl-context place-provider)
                (make-sat-cvc-context place-provider)
                *eliminate-term-ite-for-cvcl?*
                *case-split-diseq-for-cvcl?*)
        )

(define-class <sat-cvcl-context> (<sat-generic-context>) ())
(define-class <sat-cvc-context> (<sat-generic-context>) ())

(define *cvcl-use-integers?* #f)

(define-api (sal/enable-cvcl-integers! flag)
  (set! *cvcl-use-integers?* flag))

(define (make-sat-cvcl-context place-provider)
  (sat-generic-context/init! (make-instance <sat-cvcl-context>) place-provider))
(define (make-sat-cvc-context place-provider)
  (sat-generic-context/init! (make-instance <sat-cvc-context>) place-provider))

;; ------------------------------------------------------------------
;; Display CVC Lite types
;;
;; ------------------------------------------------------------------
(define-generic (sal-type/display-cvcl type))
(define-method (sal-type/display-cvcl (type <sal-type>))
  (sign-unsupported-feature type "Failed to convert to a CVC type."))
(define-method (sal-type/display-cvcl (type <sal-function-type>))
  (display "[ ")
  (sal-type/display-cvcl (slot-value type :domain))
  (display " -> ")
  (sal-type/display-cvcl (slot-value type :range))
  (display " ]"))
(define-method (sal-type/display-cvcl (type <sal-array-type>))
  (cond 
   (*sat-generic-context-eliminate-array-theory*
    (call-next-method))
   (else
    (display "ARRAY ")
    (sal-type/display-cvcl (slot-value type :domain))
    (display " OF ")
    (sal-type/display-cvcl (slot-value type :range)))))
(define-method (sal-type/display-cvcl (type <sal-tuple-type>))
  (display "[ ")
  (let ((types (slot-value type :types)))
    (sal-type/display-cvcl (car types))
    (for-each (lambda (t)
                (display ", ")
                (sal-type/display-cvcl t))
              (cdr types)))
  (display " ]"))
(define-method (sal-type/display-cvcl (type <sal-scalar-type>))
  (cond
   ((sal-type/boolean? type)
    (display "BOOLEAN"))
   (*cvcl-use-integers?*
    (display "INT"))
   (else
    (display "REAL")))) ;; they were converted to numbers...
(define-method (sal-type/display-cvcl (type <sal-bool-type>))
  (display "BOOLEAN"))
(define-method (sal-type/display-cvcl (type <sal-number-type>))
  (if (and *cvcl-use-integers?* (sal-type/integer? type))
    (display "INT")
    (display "REAL")))
(define-method (sal-type/display-cvcl (type <sal-subtype>))
  (sal-type/display-cvcl (sal-subtype/immediate-super-type type)))
(define-method (sal-type/display-cvcl (type <sal-type-name>))
  (cond
   ((sal-type-name/definition type) =>
    sal-type/display-cvcl)
   (else
    ;; uninterpreted type
    (display "ANY"))))

;; ------------------------------------------------------------------
;; Display CVC Lite formulas
;;
;; ------------------------------------------------------------------

(define-generic (sal-ast/display-cvcl ast info))
(define-method (sal-ast/display-cvcl (ast <sal-ast>) (info <dp-translation-info>))
  (sign-unsupported-feature ast "Failed to convert to a CVC expression."))

(define (sal-arg/display-cvcl arg info)
  (unless (instance-of? arg <sal-ast-leaf>)
    (display "( "))
  (sal-ast/display-cvcl arg info)
  (unless (instance-of? arg <sal-ast-leaf>)
    (display " )")))

(define (sal-infix-application/display-cvcl ast cvc-op info)
  (let ((arg-list (sal-application/argument-list ast)))
    (sal-arg/display-cvcl (car arg-list) info)
    (for-each (lambda (arg)
                (display* " " cvc-op " ")
                (sal-arg/display-cvcl arg info))
              (cdr arg-list))))

(define-method (sal-ast/display-cvcl (ast <sal-eq>) (info <dp-translation-info>))
  (if (sal-type/boolean? (sal-expr/type (sal-binary-application/arg1 ast)))
    (sal-infix-application/display-cvcl ast "<=>" info)
    (sal-infix-application/display-cvcl ast "=" info)))
(define-method (sal-ast/display-cvcl (ast <sal-diseq>) (info <dp-translation-info>))
  (if (sal-type/boolean? (sal-expr/type (sal-binary-application/arg1 ast)))
    (sal-infix-application/display-cvcl ast "XOR" info)
    (sal-infix-application/display-cvcl ast "/=" info)))
(define-method (sal-ast/display-cvcl (ast <sal-and>) (info <dp-translation-info>))
  (sal-infix-application/display-cvcl ast "AND" info))
(define-method (sal-ast/display-cvcl (ast <sal-or>) (info <dp-translation-info>))
  (sal-infix-application/display-cvcl ast "OR" info))
(define-method (sal-ast/display-cvcl (ast <sal-not>) (info <dp-translation-info>))
  (display "(NOT ")
  (sal-arg/display-cvcl (slot-value ast :arg) info)
  (display ")"))
(define-method (sal-ast/display-cvcl (ast <sal-add>) (info <dp-translation-info>))
  (sal-infix-application/display-cvcl ast "+" info))
(define-method (sal-ast/display-cvcl (ast <sal-sub>) (info <dp-translation-info>))
  (sal-infix-application/display-cvcl ast "-" info))
(define-method (sal-ast/display-cvcl (ast <sal-mul>) (info <dp-translation-info>))
  (sal-infix-application/display-cvcl ast "*" info))
(define-method (sal-ast/display-cvcl (ast <sal-mod>) (info <dp-translation-info>))
  (sign-unsupported-feature ast "CVC and CVC-Lite do not support the operator 'mod'."))
(define-method (sal-ast/display-cvcl (ast <sal-div>) (info <dp-translation-info>))
  (let ((new-ast (div->mul ast)))
    (unless new-ast
      (sign-unsupported-feature ast "CVC and CVC-Lite do not support the operator '/'."))
    (sal-ast/display-cvcl new-ast info)))
(define-method (sal-ast/display-cvcl (ast <sal-idiv>) (info <dp-translation-info>))
  (sign-unsupported-feature ast "CVC and CVC-Lite do not support the operator 'div'."))
(define-method (sal-ast/display-cvcl (ast <sal-numeral>) (info <dp-translation-info>))
  (display (mpq->string (slot-value ast :num))))
(define-method (sal-ast/display-cvcl (ast <sal-lt>) (info <dp-translation-info>))
  (sal-infix-application/display-cvcl ast "<" info))
(define-method (sal-ast/display-cvcl (ast <sal-le>) (info <dp-translation-info>))
  (sal-infix-application/display-cvcl ast "<=" info))
(define-method (sal-ast/display-cvcl (ast <sal-gt>) (info <dp-translation-info>))
  (sal-infix-application/display-cvcl ast ">" info))
(define-method (sal-ast/display-cvcl (ast <sal-ge>) (info <dp-translation-info>))
  (sal-infix-application/display-cvcl ast ">=" info))
(define-method (sal-ast/display-cvcl (ast <sal-name-expr>) (info <dp-translation-info>))
  (eq-hash-table/put! (slot-value info :already-visited) (slot-value ast :decl) #unspecified)
  (display (dp-translation-info/var-id info (slot-value ast :decl))))
(define-method (sal-ast/display-cvcl (ast <sal-true>) (info <dp-translation-info>))
  (display "TRUE"))
(define-method (sal-ast/display-cvcl (ast <sal-false>) (info <dp-translation-info>))
  (display "FALSE"))
(define-method (sal-ast/display-cvcl (ast <sal-conditional>) (info <dp-translation-info>))
  (display "IF (")
  (sal-ast/display-cvcl (slot-value ast :cond-expr) info)
  (display ") THEN (")
  (sal-ast/display-cvcl (slot-value ast :then-expr) info)
  (display ") ELSE (")
  (sal-ast/display-cvcl (slot-value ast :else-expr) info)
  (display ") ENDIF"))
(define-method (sal-ast/display-cvcl (ast <sal-application>) (info <dp-translation-info>))
  (sal-ast/display-cvcl (slot-value ast :fun) info)
  (display "(")
  (let ((arg-list (sal-application/argument-list ast)))
    (sal-ast/display-cvcl (car arg-list) info)
    (for-each (lambda (arg)
                (display ", ")
                (sal-ast/display-cvcl arg info))
              (cdr arg-list)))
  (display ")"))
(define-method (sal-ast/display-cvcl (ast <sal-array-selection>) (info <dp-translation-info>))
  (cond
   (*sat-generic-context-eliminate-array-theory*
    (call-next-method))
   (else
    (sal-arg/display-cvcl (slot-value ast :fun) info)
    (display "[")
    (sal-ast/display-cvcl (slot-value ast :arg) info)
    (display "]"))))
(define-method (sal-ast/display-cvcl (ast <sal-array-update>) (info <dp-translation-info>))
  [assert (ast) (not *sat-generic-context-eliminate-array-theory*)]
  (sal-arg/display-cvcl (slot-value ast :target) info)
  (display " WITH [ ")
  (sal-ast/display-cvcl (slot-value ast :idx) info)
  (display " ] := ")
  (sal-arg/display-cvcl (slot-value ast :new-value) info))
    
;; -----------------------------------------------------------------
;; Display constraint
;;
;;
;; -----------------------------------------------------------------
(define-generic (sal-constraint/display-cvcl ast info))

(define-method (sal-constraint/display-cvcl (ast <sal-int-pred>) (info <dp-translation-info>))
  ;; do nothing...
  #unspecified)

(define-method (sal-constraint/display-cvcl (ast <sal-ast>) (info <dp-translation-info>))
  (let ((alias-id1 (dp-translation-info/new-alias-id info)))
    (display* alias-id1 " : BOOLEAN = ")
    (sal-ast/display-cvcl ast info)
    (print " ;")))

;; ------------------------------------------------------------------
;; Display a CVC alias
;;
;; ------------------------------------------------------------------
(define (display-cvcl-alias decl expr info)
  (display* (dp-translation-info/var-id info decl) " : ")
  (sal-type/display-cvcl (slot-value decl :type))
  (display " = ")
  (sal-ast/display-cvcl expr info)
  (print " ;"))

;; ------------------------------------------------------------------
;; Display satisfiability problem
;;
;; ------------------------------------------------------------------

(define (sat-context/display-cvcl! sat-context)  
  (let ((info (make-dp-translation-info))
        (def-list (sat-generic-context/collect-definitions! sat-context)))
    ;; declare type ANY
    (print "ANY : TYPE;")
    ;; define variables
    (for-each (lambda (decl)
                (display* (dp-translation-info/var-id info decl) " : ")
                (sal-type/display-cvcl (slot-value decl :type))
                (print " ;"))
              (queue->list (slot-value sat-context :declaration-queue)))
    ;; define "definitions" :-)
    (for-each (lambda (pair)
                (let ((decl (car pair))
                      (expr (cdr pair)))
                  (display-cvcl-alias decl expr info)))
              def-list)
    ;; define the constraints
    (for-each (lambda (expr)
                (sal-constraint/display-cvcl expr info))
              (queue->list (slot-value sat-context :constraint-queue)))
    ;; print the satisfiability problem
    (print "QUERY NOT(")
    (display-n-ary-infix-op "AND" (queue->list (slot-value info :alias-queue)))
    (print ");")))

(define *case-split-diseq-for-cvcl?* #f)
(define *eliminate-term-ite-for-cvcl?* #f)

(define-method (sat-context/solve (ctx <sat-cvcl-context>))
  (sat-generic-context/simplify! ctx 
                                 :eliminate-int-diseq? *case-split-diseq-for-cvcl?*
                                 :ite->ite-bool? *eliminate-term-ite-for-cvcl?*)
  (sal/set-cvcl-in-tmp-file!)
  (with-output-to-file *sal-tmp-in-file-to-cvcl*
    (lambda () (sat-context/display-cvcl! ctx)))
  (verbose-message 3 "  formula size: ~a nodes" (sat-generic-context/size ctx))
  (verbose-message 2 "  executing CVC Lite...")
  (unwind-protect 
   (cvcl/execute *sal-tmp-in-file-to-cvcl*)
   (sal/delete-tmp-file! *sal-tmp-in-file-to-cvcl*)))

(define-method (sat-context/solve (ctx <sat-cvc-context>))
  (sat-generic-context/simplify! ctx :ite->ite-bool? #f)
  (sal/set-cvcl-in-tmp-file!)
  (with-output-to-file *sal-tmp-in-file-to-cvcl*
    (lambda () (sat-context/display-cvcl! ctx)))
  (verbose-message 3 "  formula size: ~a nodes" (sat-generic-context/size ctx))
  (verbose-message 2 "  executing CVC...")
  (unwind-protect
   (cvc/execute *sal-tmp-in-file-to-cvcl*)
   (sal/delete-tmp-file! *sal-tmp-in-file-to-cvcl*)))

