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

(module sat-yices-context
        (include "sal.sch")
        (import sat-generic-context sat-context sal-ast-env dp-translation-support
                sal-ast-copy fast-hash-table queue sal-type sal-expression gmp-scheme
                sat-generic-context-result sal-environment yices-interface tmp-files)
        (export <sat-yices-context>
                (make-sat-yices-context place-provider))
        )

(define-class <sat-yices-context> (<sat-generic-context>) ())

(define (make-sat-yices-context place-provider)
  (sat-generic-context/init! (make-instance <sat-yices-context>) place-provider))

;;------------------------------------------------------------------
;; Display yices types
;;------------------------------------------------------------------

(define-generic (sal-type/display-yices type))

(define-method (sal-type/display-yices (type <sal-type>))
  (sign-unsupported-feature type "Failed to convert to a yices type."))

(define-method (sal-type/display-yices (type <sal-function-type>))
  (display "(-> ")
  (sal-type/display-yices (slot-value type :domain))
  (display " ")
  (sal-type/display-yices (slot-value type :range))
  (display ")"))

(define-method (sal-type/display-yices (type <sal-array-type>))
  (call-next-method))

(define-method (sal-type/display-yices (type <sal-domain-tuple-type>))
  (for-each (lambda (t)
                (display " ")
                (sal-type/display-yices t))
            (slot-value type :types)))

(define-method (sal-type/display-yices (type <sal-tuple-type>))
  (display "(tuple ")
  (for-each (lambda (t)
              (display " ")
              (sal-type/display-yices t))
            (slot-value type :types))
  (display ")"))

(define-method (sal-type/display-yices (type <sal-scalar-type>))
  (if (sal-type/boolean? type)
    (display "bool")
    (display "int"))) ;; they were converted to numbers...

(define-method (sal-type/display-yices (type <sal-bool-type>))
  (display "bool"))

(define-method (sal-type/display-yices (type <sal-int-type>))
  (display "int"))

(define-method (sal-type/display-yices (type <sal-number-type>))
  (display "real"))

(define-method (sal-type/display-yices (type <sal-subtype>))
  (sal-type/display-yices (sal-subtype/immediate-super-type type)))

(define-method (sal-type/display-yices (type <sal-type-name>))
  (cond
   ((sal-type-name/definition type) =>
    sal-type/display-yices)
   (else
    ;; uninterpreted type
    (display "utype"))))

;; ------------------------------------------------------------------
;; Display yices formulas
;;
;; ------------------------------------------------------------------

(define-generic (sal-ast/display-yices ast info))
(define-method (sal-ast/display-yices (ast <sal-ast>) (info <dp-translation-info>))
  (sign-unsupported-feature ast "Failed to convert to a yices expression."))

(define (sal-application/display-yices ast yices-op info)
  (display* "(" yices-op)
  (let ((arg-list (sal-application/argument-list ast)))
    (for-each (lambda (arg)
                (display " ")
                (sal-ast/display-yices arg info))
              arg-list))
  (display ")"))

(define-method (sal-ast/display-yices (ast <sal-eq>) (info <dp-translation-info>))
  (sal-application/display-yices ast "=" info))
(define-method (sal-ast/display-yices (ast <sal-diseq>) (info <dp-translation-info>))
  (sal-application/display-yices ast "/=" info))
(define-method (sal-ast/display-yices (ast <sal-and>) (info <dp-translation-info>))
  (sal-application/display-yices ast "and" info))
(define-method (sal-ast/display-yices (ast <sal-or>) (info <dp-translation-info>))
  (sal-application/display-yices ast "or" info))
(define-method (sal-ast/display-yices (ast <sal-not>) (info <dp-translation-info>))
  (sal-application/display-yices ast "not" info))
(define-method (sal-ast/display-yices (ast <sal-add>) (info <dp-translation-info>))
  (sal-application/display-yices ast "+" info))
(define-method (sal-ast/display-yices (ast <sal-sub>) (info <dp-translation-info>))
  (sal-application/display-yices ast "-" info))
(define-method (sal-ast/display-yices (ast <sal-mul>) (info <dp-translation-info>))
  (sal-application/display-yices ast "*" info))
(define-method (sal-ast/display-yices (ast <sal-mod>) (info <dp-translation-info>))
  (sal-application/display-yices ast "%" info))
(define-method (sal-ast/display-yices (ast <sal-div>) (info <dp-translation-info>))
  (let ((new-ast (div->mul ast)))
    (unless new-ast
      (sign-unsupported-feature ast "Yices does not support nonlinear arithmetic."))
    (sal-ast/display-yices new-ast info)))
(define-method (sal-ast/display-yices (ast <sal-idiv>) (info <dp-translation-info>))
  (sal-application/display-yices ast "div" info))
(define-method (sal-ast/display-yices (ast <sal-numeral>) (info <dp-translation-info>))
  (display (mpq->string (slot-value ast :num))))
(define-method (sal-ast/display-yices (ast <sal-lt>) (info <dp-translation-info>))
  (sal-application/display-yices ast "<" info))
(define-method (sal-ast/display-yices (ast <sal-le>) (info <dp-translation-info>))
  (sal-application/display-yices ast "<=" info))
(define-method (sal-ast/display-yices (ast <sal-gt>) (info <dp-translation-info>))
  (sal-application/display-yices ast ">" info))
(define-method (sal-ast/display-yices (ast <sal-ge>) (info <dp-translation-info>))
  (sal-application/display-yices ast ">=" info))
(define-method (sal-ast/display-yices (ast <sal-name-expr>) (info <dp-translation-info>))
  (eq-hash-table/put! (slot-value info :already-visited) (slot-value ast :decl) #unspecified)
  (display (dp-translation-info/var-id info (slot-value ast :decl))))
(define-method (sal-ast/display-yices (ast <sal-true>) (info <dp-translation-info>))
  (display "true"))
(define-method (sal-ast/display-yices (ast <sal-false>) (info <dp-translation-info>))
  (display "false"))
(define-method (sal-ast/display-yices (ast <sal-conditional>) (info <dp-translation-info>))
  (display "(ite ")
  (sal-ast/display-yices (slot-value ast :cond-expr) info)
  (display " ")
  (sal-ast/display-yices (slot-value ast :then-expr) info)
  (display " ")
  (sal-ast/display-yices (slot-value ast :else-expr) info)
  (display ")"))
(define-method (sal-ast/display-yices (ast <sal-application>) (info <dp-translation-info>))
  (display "(")
  (sal-ast/display-yices (slot-value ast :fun) info)
  (let ((arg-list (sal-application/argument-list ast)))
    (for-each (lambda (arg)
                (display " ")
                (sal-ast/display-yices arg info))
              arg-list))
  (display ")"))
(define-method (sal-ast/display-yices (ast <sal-array-selection>) (info <dp-translation-info>))
  (call-next-method))
(define-method (sal-ast/display-yices (ast <sal-array-update>) (info <dp-translation-info>))
  (display "(update ")
  (sal-ast/display-yices (slot-value ast :target) info)
  (display " (")
  (sal-ast/display-yices (slot-value ast :idx) info)
  (display ") ")
  (sal-ast/display-yices (slot-value ast :new-value) info)
  (display ")"))
    
;; -----------------------------------------------------------------
;; Display constraint
;;
;;
;; -----------------------------------------------------------------
(define-generic (sal-constraint/display-yices ast info))

(define-method (sal-constraint/display-yices (ast <sal-int-pred>) (info <dp-translation-info>))
  #unspecified)
;   (let ((alias-id1 (dp-translation-info/new-alias-id info)))
;     (display* "(define " alias-id1 "::bool ")
;     (sal-application/display-yices ast "int_pred" info)
;     (print ")")))

(define-method (sal-constraint/display-yices (ast <sal-ast>) (info <dp-translation-info>))
  (let ((alias-id1 (dp-translation-info/new-alias-id info)))
    (display* "(define " alias-id1 "::bool ")
    (sal-ast/display-yices ast info)
    (print ")")))

;; ------------------------------------------------------------------
;; Display an yices alias
;;
;; ------------------------------------------------------------------
(define (display-yices-alias decl expr info)
  (display* "(define " (dp-translation-info/var-id info decl) "::")
  (sal-type/display-yices (slot-value decl :type))
  (display "  ")
  (sal-ast/display-yices expr info)
  (print ")"))

;; ------------------------------------------------------------------
;; Process int-pred contraints
;; Set integer type
;;
;; ------------------------------------------------------------------
(define (filter-int-pred! sat-context)
  (let ((int-table (make-eq-hash-table)))
    (for-each (lambda (constraint)
                (when (and (instance-of? constraint <sal-int-pred>)
                           (instance-of? (slot-value constraint :arg) <sal-name-expr>))
                  (eq-hash-table/put! int-table (slot-value (slot-value constraint :arg) :decl) #t)))
              (queue->list (slot-value sat-context :constraint-queue)))
    (for-each (lambda (decl)
                (when (eq-hash-table/contains? int-table decl)
                  (set-slot-value! decl :type (make-sal-builtin-name <sal-int-type> (slot-value decl :type)))))
              (queue->list (slot-value sat-context :declaration-queue)))))


;; ------------------------------------------------------------------
;; Display satisfiability problem
;;
;; ------------------------------------------------------------------

(define (sat-context/display-yices! sat-context)
;; BD this filtering is no longer necessary, original types are preserved in declarations
;; (cf. sat-generic-context.scm)
;;  (filter-int-pred! sat-context)
  (let ((info (make-dp-translation-info))
        (def-list '())) 
    ;; If we transform equalities in definitions, we may loose information during the model construction.
    ;; Maybe, yices should output the value of defined variables.
    ;; (def-list (sat-generic-context/collect-definitions! sat-context)))

    ;; declare type utype (uinterpreted type)
    (print "(define-type utype)")
    ;; define variables
    (for-each (lambda (decl)
                (display* "(define " (dp-translation-info/var-id info decl) "::")
                (sal-type/display-yices (slot-value decl :type))
                (print ")"))
              (queue->list (slot-value sat-context :declaration-queue)))
    ;; define "definitions" :-)
    (for-each (lambda (pair)
                (let ((decl (car pair))
                      (expr (cdr pair)))
                  (display-yices-alias decl expr info)))
              def-list)
    ;; define the constraints
    (for-each (lambda (expr)
                (sal-constraint/display-yices expr info))
              (queue->list (slot-value sat-context :constraint-queue)))
    ;; print the satisfiability problem
    (print "(assert (and")
    (for-each (lambda (alias)
                (display* " " alias))
              (queue->list (slot-value info :alias-queue)))
    (print "))")
    (print "(check)")
    (slot-value info :id->decl-mapping)))



(define-method (sat-context/solve (ctx <sat-yices-context>))
  (sat-generic-context/simplify! ctx :ite->ite-bool? #f
                                     :eliminate-div-mod? #t)
  (sal/set-yices-in-tmp-file!)
  (let* ((id->decl-mapping (with-output-to-file *sal-tmp-in-file-to-yices*
                             (lambda () (sat-context/display-yices! ctx))))
         (id->decl-mapping-proc (lambda (id)
                                  (cond
                                   ((eq-hash-table/get id->decl-mapping id) => cdr)
                                   (else #f))))
         (constraint-queue (slot-value ctx :constraint-queue)))
    (if (queue/empty? constraint-queue)
      #t
      (let ((place-provider (queue/front constraint-queue)))
        (verbose-message 3 "  formula size: ~a nodes" (sat-generic-context/size ctx))
        (verbose-message 2 "  executing Yices...")
        (let ((result (unwind-protect
                       (yices/execute *sal-tmp-in-file-to-yices* id->decl-mapping-proc place-provider)
                       (sal/delete-tmp-file! *sal-tmp-in-file-to-yices*))))
          (and result
               (make-instance <sat-generic-context-result> 
                              :sat-context ctx
                              :result-constraint-list result)))))))
