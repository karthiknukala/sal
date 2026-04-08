;;
;; SAL 3.3, Copyright (C) 2026, SRI International.  All Rights Reserved.
;;
;; SAL is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;

(module sat-smtlib2-context
        (include "sal.sch")
        (include "fast-hash-table.sch")
        (import sat-generic-context sat-context sal-ast-env dp-translation-support
                sal-ast-copy queue sal-type sal-expression gmp-scheme
                sat-generic-context-result sal-environment smtlib2-interface
                tmp-files sal-ast-for-each)
        (export <sat-smtlib2-context>
                (make-sat-smtlib2-context place-provider)))

(define-class <sat-smtlib2-context> (<sat-generic-context>) ())

(define-class <smtlib2-translation-info> ()
  (:dp-info :type->sort-id :sort->type :sort-id-queue :sort-idx))

(define (smtlib2-translation-info/init! info)
  (set-slot-value! info :dp-info (make-dp-translation-info))
  (set-slot-value! info :type->sort-id (make-eq-hash-table))
  (set-slot-value! info :sort->type (make-hashtable))
  (set-slot-value! info :sort-id-queue (make-queue))
  (set-slot-value! info :sort-idx 0)
  info)

(define (make-smtlib2-translation-info)
  (smtlib2-translation-info/init! (make-instance <smtlib2-translation-info>)))

(define (make-sat-smtlib2-context place-provider)
  (sat-generic-context/init! (make-instance <sat-smtlib2-context>) place-provider))

(define (smtlib2-translation-info/var-id info decl)
  (dp-translation-info/var-id (slot-value info :dp-info) decl))

(define (smtlib2-translation-info/sort-id info type)
  (let* ((type-name (sal-type/cast type <sal-type-name>))
         (decl (slot-value type-name :decl))
         (type->sort-id (slot-value info :type->sort-id)))
    (cond
     ((eq-hash-table/get type->sort-id decl) => cdr)
     (else
      (let ((sort-id (symbol-append 'U_ (object->symbol (slot-value info :sort-idx)))))
        (set-slot-value! info :sort-idx (+ 1 (slot-value info :sort-idx)))
        (eq-hash-table/put! type->sort-id decl sort-id)
        (hashtable-put! (slot-value info :sort->type) sort-id type)
        (queue/insert! (slot-value info :sort-id-queue) sort-id)
        sort-id)))))

(define-generic (sal-type/collect-smtlib2-sorts! type info))

(define-method (sal-type/collect-smtlib2-sorts! (type <sal-type>) (info <smtlib2-translation-info>))
  #unspecified)

(define-method (sal-type/collect-smtlib2-sorts! (type <sal-function-type>) (info <smtlib2-translation-info>))
  (sal-type/collect-smtlib2-sorts! (slot-value type :domain) info)
  (sal-type/collect-smtlib2-sorts! (slot-value type :range) info))

(define-method (sal-type/collect-smtlib2-sorts! (type <sal-tuple-type>) (info <smtlib2-translation-info>))
  (for-each (lambda (child-type)
              (sal-type/collect-smtlib2-sorts! child-type info))
            (slot-value type :types)))

(define-method (sal-type/collect-smtlib2-sorts! (type <sal-record-type>) (info <smtlib2-translation-info>))
  (for-each (lambda (field)
              (sal-type/collect-smtlib2-sorts! (slot-value field :type) info))
            (slot-value type :fields)))

(define-method (sal-type/collect-smtlib2-sorts! (type <sal-subtype>) (info <smtlib2-translation-info>))
  (sal-type/collect-smtlib2-sorts! (sal-subtype/immediate-super-type type) info))

(define-method (sal-type/collect-smtlib2-sorts! (type <sal-type-name>) (info <smtlib2-translation-info>))
  (cond
   ((sal-type-name/definition type) =>
    (lambda (def)
      (sal-type/collect-smtlib2-sorts! def info)))
   (else
    (smtlib2-translation-info/sort-id info type)
    #unspecified)))

(define-generic (sal-type/display-smtlib2 type info))

(define-method (sal-type/display-smtlib2 (type <sal-type>) (info <smtlib2-translation-info>))
  (sign-unsupported-feature type "Failed to convert to an SMT-LIB2 sort."))

(define-method (sal-type/display-smtlib2 (type <sal-array-type>) (info <smtlib2-translation-info>))
  (when (> (sal-function-type/arity type) 1)
    (sign-unsupported-feature type "SMT-LIB2 array sorts must be unary."))
  (display "(Array ")
  (sal-type/display-smtlib2 (sal-function-type/domain type) info)
  (display " ")
  (sal-type/display-smtlib2 (sal-function-type/range type) info)
  (display ")"))

(define-method (sal-type/display-smtlib2 (type <sal-function-type>) (info <smtlib2-translation-info>))
  (sign-unsupported-feature type "SMT-LIB2 does not support first-class function sorts."))

(define-method (sal-type/display-smtlib2 (type <sal-scalar-type>) (info <smtlib2-translation-info>))
  (if (sal-type/boolean? type)
    (display "Bool")
    (display "Int")))

(define-method (sal-type/display-smtlib2 (type <sal-bool-type>) (info <smtlib2-translation-info>))
  (display "Bool"))

(define-method (sal-type/display-smtlib2 (type <sal-int-type>) (info <smtlib2-translation-info>))
  (display "Int"))

(define-method (sal-type/display-smtlib2 (type <sal-number-type>) (info <smtlib2-translation-info>))
  (display "Real"))

(define-method (sal-type/display-smtlib2 (type <sal-subtype>) (info <smtlib2-translation-info>))
  (sal-type/display-smtlib2 (sal-subtype/immediate-super-type type) info))

(define-method (sal-type/display-smtlib2 (type <sal-type-name>) (info <smtlib2-translation-info>))
  (cond
   ((sal-type-name/definition type) =>
    (lambda (def)
      (sal-type/display-smtlib2 def info)))
   (else
    (display (smtlib2-translation-info/sort-id info type)))))

(define-generic (sal-ast/display-smtlib2 ast info))

(define-method (sal-ast/display-smtlib2 (ast <sal-ast>) (info <smtlib2-translation-info>))
  (sign-unsupported-feature ast "Failed to convert to an SMT-LIB2 expression."))

(define (sal-application/display-smtlib2 ast op info)
  (display "(")
  (display op)
  (for-each (lambda (arg)
              (display " ")
              (sal-ast/display-smtlib2 arg info))
            (sal-application/argument-list ast))
  (display ")"))

(define-method (sal-ast/display-smtlib2 (ast <sal-eq>) (info <smtlib2-translation-info>))
  (sal-application/display-smtlib2 ast "=" info))

(define-method (sal-ast/display-smtlib2 (ast <sal-diseq>) (info <smtlib2-translation-info>))
  (display "(not ")
  (sal-application/display-smtlib2 ast "=" info)
  (display ")"))

(define-method (sal-ast/display-smtlib2 (ast <sal-and>) (info <smtlib2-translation-info>))
  (sal-application/display-smtlib2 ast "and" info))

(define-method (sal-ast/display-smtlib2 (ast <sal-or>) (info <smtlib2-translation-info>))
  (sal-application/display-smtlib2 ast "or" info))

(define-method (sal-ast/display-smtlib2 (ast <sal-not>) (info <smtlib2-translation-info>))
  (sal-application/display-smtlib2 ast "not" info))

(define-method (sal-ast/display-smtlib2 (ast <sal-implies>) (info <smtlib2-translation-info>))
  (sal-application/display-smtlib2 ast "=>" info))

(define-method (sal-ast/display-smtlib2 (ast <sal-add>) (info <smtlib2-translation-info>))
  (sal-application/display-smtlib2 ast "+" info))

(define-method (sal-ast/display-smtlib2 (ast <sal-sub>) (info <smtlib2-translation-info>))
  (sal-application/display-smtlib2 ast "-" info))

(define-method (sal-ast/display-smtlib2 (ast <sal-mul>) (info <smtlib2-translation-info>))
  (sal-application/display-smtlib2 ast "*" info))

(define-method (sal-ast/display-smtlib2 (ast <sal-div>) (info <smtlib2-translation-info>))
  (sal-application/display-smtlib2 ast "/" info))

(define-method (sal-ast/display-smtlib2 (ast <sal-idiv>) (info <smtlib2-translation-info>))
  (sal-application/display-smtlib2 ast "div" info))

(define-method (sal-ast/display-smtlib2 (ast <sal-mod>) (info <smtlib2-translation-info>))
  (sal-application/display-smtlib2 ast "mod" info))

(define-method (sal-ast/display-smtlib2 (ast <sal-numeral>) (info <smtlib2-translation-info>))
  (display (mpq->string (slot-value ast :num))))

(define-method (sal-ast/display-smtlib2 (ast <sal-lt>) (info <smtlib2-translation-info>))
  (sal-application/display-smtlib2 ast "<" info))

(define-method (sal-ast/display-smtlib2 (ast <sal-le>) (info <smtlib2-translation-info>))
  (sal-application/display-smtlib2 ast "<=" info))

(define-method (sal-ast/display-smtlib2 (ast <sal-gt>) (info <smtlib2-translation-info>))
  (sal-application/display-smtlib2 ast ">" info))

(define-method (sal-ast/display-smtlib2 (ast <sal-ge>) (info <smtlib2-translation-info>))
  (sal-application/display-smtlib2 ast ">=" info))

(define-method (sal-ast/display-smtlib2 (ast <sal-name-expr>) (info <smtlib2-translation-info>))
  (display (smtlib2-translation-info/var-id info (slot-value ast :decl))))

(define-method (sal-ast/display-smtlib2 (ast <sal-true>) (info <smtlib2-translation-info>))
  (display "true"))

(define-method (sal-ast/display-smtlib2 (ast <sal-false>) (info <smtlib2-translation-info>))
  (display "false"))

(define-method (sal-ast/display-smtlib2 (ast <sal-conditional>) (info <smtlib2-translation-info>))
  (display "(ite ")
  (sal-ast/display-smtlib2 (slot-value ast :cond-expr) info)
  (display " ")
  (sal-ast/display-smtlib2 (slot-value ast :then-expr) info)
  (display " ")
  (sal-ast/display-smtlib2 (slot-value ast :else-expr) info)
  (display ")"))

(define-method (sal-ast/display-smtlib2 (ast <sal-application>) (info <smtlib2-translation-info>))
  (display "(")
  (sal-ast/display-smtlib2 (slot-value ast :fun) info)
  (for-each (lambda (arg)
              (display " ")
              (sal-ast/display-smtlib2 arg info))
            (sal-application/argument-list ast))
  (display ")"))

(define-method (sal-ast/display-smtlib2 (ast <sal-array-selection>) (info <smtlib2-translation-info>))
  (display "(select ")
  (sal-ast/display-smtlib2 (slot-value ast :fun) info)
  (for-each (lambda (arg)
              (display " ")
              (sal-ast/display-smtlib2 arg info))
            (sal-application/argument-list ast))
  (display ")"))

(define-method (sal-ast/display-smtlib2 (ast <sal-array-update>) (info <smtlib2-translation-info>))
  (let ((idx-list (sal-argument->argument-list (slot-value ast :idx) 1)))
    (unless (= (length idx-list) 1)
      (sign-unsupported-feature ast "SMT-LIB2 array updates must be unary."))
    (display "(store ")
    (sal-ast/display-smtlib2 (slot-value ast :target) info)
    (display " ")
    (sal-ast/display-smtlib2 (car idx-list) info)
    (display " ")
    (sal-ast/display-smtlib2 (slot-value ast :new-value) info)
    (display ")")))

(define-method (sal-ast/display-smtlib2 (ast <sal-function-update>) (info <smtlib2-translation-info>))
  (sign-unsupported-feature ast "Failed to translate a higher-order function update to SMT-LIB2."))

(define-generic (sal-constraint/display-smtlib2 ast info))

(define-method (sal-constraint/display-smtlib2 (ast <sal-int-pred>) (info <smtlib2-translation-info>))
  #unspecified)

(define-method (sal-constraint/display-smtlib2 (ast <sal-real-pred>) (info <smtlib2-translation-info>))
  #unspecified)

(define-method (sal-constraint/display-smtlib2 (ast <sal-ast>) (info <smtlib2-translation-info>))
  (display "(assert ")
  (sal-ast/display-smtlib2 ast info)
  (print ")"))

(define (display-domain-types types info)
  (display "(")
  (let loop ((remaining types)
             (first? #t))
    (unless (null? remaining)
      (unless first?
        (display " "))
      (sal-type/display-smtlib2 (car remaining) info)
      (loop (cdr remaining) #f)))
  (display ")"))

(define (display-smtlib2-declaration decl info)
  (let* ((type (slot-value decl :type))
         (id (smtlib2-translation-info/var-id info decl)))
    (display "(declare-fun ")
    (display id)
    (display " ")
    (if (and (sal-type/function? type)
             (not (instance-of? type <sal-array-type>)))
      (begin
        (display-domain-types (sal-function-type/domain-types type) info)
        (display " ")
        (sal-type/display-smtlib2 (sal-function-type/range type) info))
      (begin
        (display "() ")
        (sal-type/display-smtlib2 type info)))
    (print ")")))

(define (nonlinear-operator? ast)
  (let ((count-non-numerals
         (lambda (arg-list)
           (let loop ((remaining arg-list)
                      (count 0))
             (if (null? remaining)
               count
               (loop (cdr remaining)
                     (if (instance-of? (car remaining) <sal-numeral>)
                       count
                       (+ count 1))))))))
    (cond
     ((instance-of? ast <sal-mul>)
      (> (count-non-numerals (sal-application/argument-list ast)) 1))
     ((or (instance-of? ast <sal-div>)
          (instance-of? ast <sal-idiv>)
          (instance-of? ast <sal-mod>))
      (let ((arg-list (sal-application/argument-list ast)))
        (and (> (length arg-list) 1)
             (not (instance-of? (cadr arg-list) <sal-numeral>)))))
     (else
      #f))))

(define (sat-context/smtlib2-logic ctx)
  (let ((nonlinear? #f))
    (for-each (lambda (expr)
                (when (sal-ast/find nonlinear-operator? expr)
                  (set! nonlinear? #t)))
              (queue->list (slot-value ctx :constraint-queue)))
    (if nonlinear?
      "QF_AUFNIRA"
      "QF_AUFLIRA")))

(define (sat-context/display-smtlib2! sat-context)
  (let ((info (make-smtlib2-translation-info)))
    (for-each (lambda (decl)
                (sal-type/collect-smtlib2-sorts! (slot-value decl :type) info))
              (queue->list (slot-value sat-context :declaration-queue)))
    (print "(set-option :produce-models true)")
    (print "(set-logic " (sat-context/smtlib2-logic sat-context) ")")
    (for-each (lambda (sort-id)
                (print "(declare-sort " sort-id " 0)"))
              (queue->list (slot-value info :sort-id-queue)))
    (for-each (lambda (decl)
                (display-smtlib2-declaration decl info))
              (queue->list (slot-value sat-context :declaration-queue)))
    (if (queue/empty? (slot-value sat-context :constraint-queue))
      (print "(assert true)")
      (for-each (lambda (expr)
                  (sal-constraint/display-smtlib2 expr info))
                (queue->list (slot-value sat-context :constraint-queue))))
    (print "(check-sat)")
    (print "(get-model)")
    (cons (slot-value (slot-value info :dp-info) :id->decl-mapping)
          (slot-value info :sort->type))))

(define-method (sat-context/solve (ctx <sat-smtlib2-context>))
  (sat-generic-context/simplify! ctx :ite->ite-bool? #f
                                     :eliminate-div-mod? #f)
  (sal/set-smtlib2-in-tmp-file!)
  (let* ((translation-info (with-output-to-file *sal-tmp-in-file-to-smtlib2*
                             (lambda ()
                               (sat-context/display-smtlib2! ctx))))
         (id->decl-mapping (car translation-info))
         (sort->type-mapping (cdr translation-info))
         (id->decl-mapping-proc (lambda (id)
                                  (cond
                                   ((eq-hash-table/get id->decl-mapping id) => cdr)
                                   (else #f))))
         (sort->type-proc (lambda (sort-id)
                            (cond
                             ((hashtable-get sort->type-mapping sort-id) => identity)
                             (else #f))))
         (constraint-queue (slot-value ctx :constraint-queue)))
    (if (queue/empty? constraint-queue)
      #t
      (let ((place-provider (queue/front constraint-queue)))
        (verbose-message 3 "  formula size: ~a nodes" (sat-generic-context/size ctx))
        (verbose-message 2 "  executing SMT-LIB2 solver...")
        (let ((result (unwind-protect
                       (smtlib2/execute *sal-tmp-in-file-to-smtlib2*
                                        id->decl-mapping-proc
                                        sort->type-proc
                                        place-provider)
                       (sal/delete-tmp-file! *sal-tmp-in-file-to-smtlib2*))))
          (and result
               (make-instance <sat-generic-context-result>
                              :sat-context ctx
                              :result-constraint-list result)))))))
