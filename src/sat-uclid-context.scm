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

(module sat-uclid-context
        (include "sal.sch")
        (import sat-generic-context sat-context sal-ast-env dp-translation-support
                sal-ast-copy fast-hash-table queue sal-type sal-expression gmp-scheme
                uclid-interface sat-generic-context-result runtime tmp-files)
        (export <sat-uclid-context>
                (make-sat-uclid-context place-provider))
        )

(define-class <sat-uclid-context> (<sat-generic-context>) ())

(define (make-sat-uclid-context place-provider)
  (sat-generic-context/init! (make-instance <sat-uclid-context>) place-provider))

;; ------------------------------------------------------------------
;; UCLID doesn't support equalities f=g where f and g are functions.
;; the following transformation tries to remove such equalities...
;;
;; -----------------------------------------------------------------
(define (sat-context/top-ho-eq-elimination-register! sat-context vars-to-eliminate)
  (let ((constraint-queue (make-queue))
        (empty-env (make-empty-env)))
    (for-each (lambda (expr)
                (if (instance-of? expr <sal-eq>)
                  (multiple-value-bind
                      (lhs rhs)
                      (sal-binary-application/arguments expr)
                    (multiple-value-bind
                        (can-lhs can-rhs)
                        (if (instance-of? lhs <sal-name-expr>)
                          (values lhs rhs)
                          (values rhs lhs))
                      (if (and (instance-of? can-lhs <sal-name-expr>)
                               (instance-of? can-rhs <sal-name-expr>)
                               (sal-type/function? (sal-expr/type can-lhs))
                               (not (eq-hash-table/get vars-to-eliminate (slot-value can-lhs :decl))))
                        (eq-hash-table/put! vars-to-eliminate (slot-value can-lhs :decl) can-rhs)
                        (queue/insert! constraint-queue expr))))
                  (queue/insert! constraint-queue expr)))
              (queue->list (slot-value sat-context :constraint-queue)))
    (set-slot-value! sat-context :constraint-queue constraint-queue)))

;; return a boolean indicating whether a lambda expression was eliminated or not.
(define (sat-context/eliminate-top-ho-eq! sat-context)
  (verbose-message 3 "    preprocessing function symbols...")
  (display-runtime 4 "      transformation time: ~a secs"
    (lambda ()
      (let ((vars-to-eliminate (make-eq-hash-table)))
        (sat-context/top-ho-eq-elimination-register! sat-context vars-to-eliminate)
        (when (> (eq-hash-table/size vars-to-eliminate) 0)
          (sat-generic-context/eliminate-variables! sat-context vars-to-eliminate))))))

;; ------------------------------------------------------------------
;; Display UCLID types
;;
;; ------------------------------------------------------------------
(define-generic (sal-type/display-uclid type))
(define-method (sal-type/display-uclid (type <sal-type>))
  (sign-unsupported-feature type "Failed to convert to a UCLID type."))
(define-method (sal-type/display-uclid (type <sal-bool-type>))
  (display "TRUTH"))
(define-method (sal-type/display-uclid (type <sal-number-type>))
  (display "TERM"))
(define-method (sal-type/display-uclid (type <sal-subtype>))
  (sal-type/display-uclid (sal-subtype/immediate-super-type type)))
(define-method (sal-type/display-uclid (type <sal-type-name>))
  (cond
   ((sal-type-name/definition type) =>
    sal-type/display-uclid)
   (else
    ;; uninterpreted type
    (display "TERM"))))
(define-method (sal-type/display-uclid (type <sal-scalar-type>))
  (if (sal-type/boolean? type)
    (display "TRUTH")
    (display "TERM"))) ;; they were converted to numbers...
(define-method (sal-type/display-uclid (type <sal-function-type>))
  (let ((domain-types (sal-function-type/domain-types type))
        (range (slot-value type :range)))
    (when (exists sal-type/function? domain-types)
      (sign-unsupported-feature type "Higher order types are not supported by UCLID."))
    (if (sal-type/boolean? range)
      (display* "PRED[" (length domain-types) "]")
      (display* "FUNC[" (length domain-types) "]"))))

;; ------------------------------------------------------------------
;; Display UCLID formulas
;;
;; ------------------------------------------------------------------

(define-generic (sal-ast/display-uclid ast info))
(define-method (sal-ast/display-uclid (ast <sal-ast>) (info <dp-translation-info>))
  (sign-unsupported-feature ast "Failed to convert to a UCLID expression."))

(define (sal-arg/display-uclid arg info)
  (unless (instance-of? arg <sal-ast-leaf>)
    (display "( "))
  (sal-ast/display-uclid arg info)
  (unless (instance-of? arg <sal-ast-leaf>)
    (display " )")))

(define (sal-infix-application/display-uclid ast uclid-op info)
  (let ((arg-list (sal-application/argument-list ast)))
    (sal-arg/display-uclid (car arg-list) info)
    (for-each (lambda (arg)
                (display* " " uclid-op " ")
                (sal-arg/display-uclid arg info))
              (cdr arg-list))))

(define-generic (sal-term/collect-data ast pos? info add-entry-proc! inc-constant-proc!))
(define-method (sal-term/collect-data (ast <sal-ast>) (pos? <primitive>) (info <dp-translation-info>) (add-entry-proc! <primitive>)
                                      (inc-constant-proc! <primitive>))
  (add-entry-proc! ast pos?))
(define-method (sal-term/collect-data (ast <sal-add>) (pos? <primitive>) (info <dp-translation-info>) (add-entry-proc! <primitive>)
                                      (inc-constant-proc! <primitive>))
  (for-each (cut sal-term/collect-data <> pos? info add-entry-proc! inc-constant-proc!) (sal-application/argument-list ast)))
(define-method (sal-term/collect-data (ast <sal-sub>) (pos? <primitive>) (info <dp-translation-info>) (add-entry-proc! <primitive>)
                                      (inc-constant-proc! <primitive>))
  (let ((arg-list (sal-application/argument-list ast)))
    (sal-term/collect-data (car arg-list) pos? info add-entry-proc! inc-constant-proc!)
    (for-each (cut sal-term/collect-data <> (not pos?) info add-entry-proc! inc-constant-proc!) (cdr arg-list))))
(define-method (sal-term/collect-data (ast <sal-numeral>) (pos? <primitive>) (info <dp-translation-info>) (add-entry-proc! <primitive>)
                                      (inc-constant-proc! <primitive>))
  (let* ((tmp-num (slot-value ast :num))
         (num (if pos? tmp-num (-mpq *mpq-zero* tmp-num))))
    (inc-constant-proc! num)))
(define-method (sal-term/collect-data (ast <sal-mod>) (pos? <primitive>) (info <dp-translation-info>) (add-entry-proc! <primitive>)
                                      (inc-constant-proc! <primitive>))
  (sign-unsupported-feature ast "UCLID does not support the operator 'mod'."))
(define-method (sal-term/collect-data (ast <sal-div>) (pos? <primitive>) (info <dp-translation-info>) (add-entry-proc! <primitive>)
                                      (inc-constant-proc! <primitive>))
  (sign-unsupported-feature ast "UCLID does not support the operator '/'."))
(define-method (sal-term/collect-data (ast <sal-idiv>) (pos? <primitive>) (info <dp-translation-info>) (add-entry-proc! <primitive>)
                                      (inc-constant-proc! <primitive>))
  (sign-unsupported-feature ast "UCLID does not support the operator 'div'."))
(define-method (sal-term/collect-data (ast <sal-mul>) (pos? <primitive>) (info <dp-translation-info>) (add-entry-proc! <primitive>)
                                      (inc-constant-proc! <primitive>))
  (let ((arg-list (sal-application/argument-list ast)))
    [assert (ast arg-list) (>= (length arg-list) 2)]
    (when (> (length arg-list) 2)
      (sign-unsupported-feature ast "Multiplication is not supported by UCLID."))
    (multiple-value-bind
        (arg1 arg2)
        (if (instance-of? (car arg-list) <sal-numeral>)
          (values (car arg-list) (cadr arg-list))
          (values (cadr arg-list) (car arg-list)))
      (unless (and (instance-of? arg1 <sal-numeral>)
                   (or (=mpq (slot-value arg1 :num) *mpq-one*)
                       (=mpq (slot-value arg1 :num) *mpq-minus-one*)))
        (sign-unsupported-feature ast "Multiplication is not supported by UCLID."))
      (if (=mpq (slot-value arg1 :num) *mpq-minus-one*)
        (sal-term/collect-data arg2 (not pos?) info add-entry-proc! inc-constant-proc!)
        (sal-term/collect-data arg2 pos? info add-entry-proc! inc-constant-proc!)))))

(define (sign-only-separation-logic ast)
  (sign-unsupported-feature ast "Only separation logic is supported by the UCLID backend."))

(define (display-constant-prefix constant ast)
  (cond
   ((not (mpq/integer? constant))
    (sign-only-separation-logic ast))
   ((=mpq constant *mpq-zero*)
    ;; do nothing
    #unspecified)
   ((=mpq constant *mpq-one*)
    (display "succ("))
   ((=mpq constant *mpq-minus-one*)
    (display "pred("))
   ((>mpq constant *mpq-one*)
    (display* "succ^" (mpq->string constant) "("))
   ((<mpq constant *mpq-minus-one*)
    (display* "pred^" (mpq->string (mpq/absolute constant)) "("))
   (else
    ;; (breakpoint "display-constant-prefix" (constant) #t)
    (internal-error))))

(define (display-constant-suffix constant)
  (unless (=mpq constant *mpq-zero*)
    (display ")")))
  

(define (sal-arith-rel/display-uclid ast uclid-op info)
  (multiple-value-bind
      (lhs rhs)
      (sal-binary-application/arguments ast)
    (let* ((new-lhs #f)
           (new-rhs #f)
           (constant *mpq-zero*)
           (add-entry-proc!
            (lambda (expr pos?)
              (cond
               ((and pos? (not new-lhs))
                (set! new-lhs expr))
               ((and (not pos?) (not new-rhs))
                (set! new-rhs expr))
               (else
                (sign-only-separation-logic ast)))))
           (inc-constant-proc!
            (lambda (inc)
              (set! constant (+mpq constant inc)))))

      (sal-term/collect-data lhs #t info add-entry-proc! inc-constant-proc!)
      (sal-term/collect-data rhs #f info add-entry-proc! inc-constant-proc!)

      ;; display the left-hand-side
      (if new-lhs
        (sal-ast/display-uclid new-lhs info)
        (display "ZERO"))

      (display* " " uclid-op " ")

      ;; moving the constant to the right-hand-side... this is a small hack to allow me to reuse this function to print definitions.
      (set! constant (-mpq *mpq-zero* constant)) 
      ;; display the right-hand-side
      (display-constant-prefix constant ast)
      (if new-rhs
        (sal-ast/display-uclid new-rhs info)
        (display "ZERO"))
      (display-constant-suffix constant))))

(define-method (sal-ast/display-uclid (ast <sal-eq>) (info <dp-translation-info>))
  (if (sal-type/boolean? (sal-expr/type (sal-binary-application/arg1 ast)))
    (sal-infix-application/display-uclid ast "<=>" info)
    (sal-arith-rel/display-uclid ast "=" info)))
(define-method (sal-ast/display-uclid (ast <sal-diseq>) (info <dp-translation-info>))
  (if (sal-type/boolean? (sal-expr/type (sal-binary-application/arg1 ast)))
    (sal-infix-application/display-uclid ast "<=> ~" info)
    (sal-arith-rel/display-uclid ast "!=" info)))
(define-method (sal-ast/display-uclid (ast <sal-numeral>) (info <dp-translation-info>))
  (let ((constant (slot-value ast :num)))
    (display-constant-prefix constant ast)
    (display "ZERO")
    (display-constant-suffix constant)))
(define-method (sal-ast/display-uclid (ast <sal-and>) (info <dp-translation-info>))
  (sal-infix-application/display-uclid ast "&" info))
(define-method (sal-ast/display-uclid (ast <sal-or>) (info <dp-translation-info>))
  (sal-infix-application/display-uclid ast "|" info))
(define-method (sal-ast/display-uclid (ast <sal-not>) (info <dp-translation-info>))
  (display "(~ ")
  (sal-arg/display-uclid (slot-value ast :arg) info)
  (display ")"))
(define-method (sal-ast/display-uclid (ast <sal-lt>) (info <dp-translation-info>))
  (sal-arith-rel/display-uclid ast "<" info))
(define-method (sal-ast/display-uclid (ast <sal-le>) (info <dp-translation-info>))
  (sal-arith-rel/display-uclid ast "<=" info))
(define-method (sal-ast/display-uclid (ast <sal-gt>) (info <dp-translation-info>))
  (sal-arith-rel/display-uclid ast ">" info))
(define-method (sal-ast/display-uclid (ast <sal-ge>) (info <dp-translation-info>))
  (sal-arith-rel/display-uclid ast ">=" info))
(define-method (sal-ast/display-uclid (ast <sal-name-expr>) (info <dp-translation-info>))
  (eq-hash-table/put! (slot-value info :already-visited) (slot-value ast :decl) #unspecified)
  (display (dp-translation-info/var-id info (slot-value ast :decl))))
(define-method (sal-ast/display-uclid (ast <sal-true>) (info <dp-translation-info>))
  (display "1"))
(define-method (sal-ast/display-uclid (ast <sal-false>) (info <dp-translation-info>))
  (display "0"))
(define-method (sal-ast/display-uclid (ast <sal-conditional>) (info <dp-translation-info>))
  ;; The conditionals only occur in places where it is allowed to use the case-statement.
  ;; See: sat-context/collect-definitions! and sal-ast/ite->bool-ite
  (display "case ")
  (sal-ast/display-uclid (slot-value ast :cond-expr) info)
  (display " : ")
  (sal-ast/display-uclid (slot-value ast :then-expr) info)
  (display "; default : ")
  (sal-ast/display-uclid (slot-value ast :else-expr) info)
  (display " ; esac"))
(define-method (sal-ast/display-uclid (ast <sal-application>) (info <dp-translation-info>))
  (unless (instance-of? (slot-value ast :fun) <sal-name-expr>)
    (sign-unsupported-feature ast "Failed to map to UCLID, higher order function used."))
  (sal-ast/display-uclid (slot-value ast :fun) info)
  (display "(")
  (let ((arg-list (sal-application/argument-list ast)))
    (sal-ast/display-uclid (car arg-list) info)
    (for-each (lambda (arg)
                (display ", ")
                (sal-ast/display-uclid arg info))
              (cdr arg-list)))
  (display ")"))

;; -----------------------------------------------------------------
;; Display constraint
;;
;;
;; -----------------------------------------------------------------
(define-generic (sal-constraint/display-uclid ast info))

(define-method (sal-constraint/display-uclid (ast <sal-int-pred>) (info <dp-translation-info>))
  ;; do nothing...
  #unspecified)

(define-method (sal-constraint/display-uclid (ast <sal-ast>) (info <dp-translation-info>))
  (let ((alias-id1 (dp-translation-info/new-alias-id info)))
    (display* alias-id1 " := ")
    (sal-ast/display-uclid ast info)
    (print " ;")))

;; ------------------------------------------------------------------
;; Display an uclid alias (definition)
;;
;; ------------------------------------------------------------------

(define (display-uclid-alias decl expr info)
  (cond
   ((sal-type/boolean? (slot-value decl :type))
    (display (dp-translation-info/var-id info decl))
    (display " := ")
    (sal-ast/display-uclid expr info))
   (else
    (let ((eq-aux (make-sal-equality (make-sal-name-expr decl) expr)))
      (sal-arith-rel/display-uclid eq-aux ":=" info))))
  (print " ;"))

;; ------------------------------------------------------------------
;; Collect if-then-else as definitions
;;
;; ------------------------------------------------------------------
; (define (collect-ite sat-context)
;   (let ((def-queue (make-queue))

;; ------------------------------------------------------------------
;; Display satisfiability problem
;;
;; ------------------------------------------------------------------
(define (sat-context/display-uclid! sat-context)   
  (print "MODEL salenv_tmp_output\n")
  (print "CONTROL\n")
  (print "EXTVAR\n")
  (print "STOREVAR\n")
  (print "VAR\n")
  (print "CONST")
  (print "  ZERO : TERM;")
  (let* ((info (make-dp-translation-info))
         (def-list (sat-generic-context/collect-definitions! sat-context #t)))
    ;; define variables
    (for-each (lambda (decl)
                (display* (dp-translation-info/var-id info decl) " : ")
                (sal-type/display-uclid (slot-value decl :type))
                (print " ;"))
              (queue->list (slot-value sat-context :declaration-queue)))
    (print "DEFINE")
    ;; define "definitions" :-)
    (for-each (lambda (pair)
                (let ((decl (car pair))
                      (expr (cdr pair)))
                  (display-uclid-alias decl expr info)))
              def-list)
    ;; define the constraints
    (for-each (lambda (expr)
                (sal-constraint/display-uclid expr info))
              (queue->list (slot-value sat-context :constraint-queue)))
    ;; print the satisfiability problem
    (print "")
    (print "EXEC\n")
    (print "decide(~ (")
    (display-n-ary-infix-op "&" (queue->list (slot-value info :alias-queue)))
    (print ") );")))

(define-method (sat-context/solve (ctx <sat-uclid-context>))
  (sat-generic-context/simplify! ctx
                                 :ite->ite-bool? #f
                                 :eliminate-array-theory? #t)
  (sat-context/eliminate-top-ho-eq! ctx)
  (sal/set-uclid-in-tmp-file!)
  (with-output-to-file *sal-tmp-in-file-to-uclid*
    (lambda () (sat-context/display-uclid! ctx)))
  (verbose-message 3 "  formula size: ~a nodes" (sat-generic-context/size ctx))
  (verbose-message 2 "  executing UCLID...")
  (unwind-protect 
   (uclid/execute *sal-tmp-in-file-to-uclid*)
   (sal/delete-tmp-file! *sal-tmp-in-file-to-uclid*)))




