;;
;; SAL 3.3, Copyright (C) 2026, SRI International.  All Rights Reserved.
;;
;; SAL is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;

(module cdr-yices2-smt2
        (include "sal.sch")
        (include "fast-hash-table.sch")
        (import utility runtime polarity sal-expression sal-module sal-assertion sal-type sal-pp
                sal-ast-env sal-ast-eq sal-ast-simplify sal-decls queue
                gmp-scheme
                dp-translation-support
                sat-context sat-bmc-context sat-generic-bmc-context
                sat-generic-context sat-generic-context-result
                sat-smtlib2-bmc-context sat-smtlib2-context smtlib2-interface
                sat-yices2-context
                yices2-interface yices2-api tmp-files
                cdr-solver)
        (export (make-cdr-yices2-solver assertion pdkind?)))

(define-class <cdr-yices2-session> ()
  (:name
   :solver
   :context
   :command
   :logic
   :interpolants?
   :process
   :input-port
   :output-port
   :transcript
   :echo-counter
   :id->decl-proc
   :sort->type-proc
   :place-provider))

(define-class <cdr-yices2-solver> (<cdr-solver>)
  (:ctx
   :info
   :property
   :definition
   :initialization
   :transition
   :valid-input
   :valid-state
   :i0
   :t01
   :bad0
   :bad1
   :init-session
   :reach-sessions
   :shared-reach-session
   :bad-sessions
   :induction-sessions
   :pdkind-induction-session
   :pdkind-induction-depth
   :minimization-session
   :decl-name->term
   :helper-name->term
   :registered-sorts
   :expr-term-cache
   :expr-text-term-cache
   :synced-decls
   :yices-owned?
   :reach-lemmas
   :induction-lemmas
   :num-levels
   :effective-command
   :pdkind?))

(define *cdr-empty-env* (make-empty-env))
(define *yices-constructor-variable* 5)
(define *yices-constructor-uninterpreted-term* 6)
(define *yices-constructor-bv-sum* 41)
(define *yices-constructor-arith-sum* 42)
(define *yices-constructor-arith-ff-sum* 43)
(define *yices-constructor-power-product* 44)

(define (ensure-string-value value label)
  (unless (string? value)
    (sign-error "sal-cdr expected ~a to be a string, received ~a." label value))
  value)

(define (ensure-yices-term value label)
  (unless (integer? value)
    (sign-error "sal-cdr expected ~a to be a Yices term id, received ~a."
                label
                value))
  value)

(define (ensure-yices-term-list values label)
  (let loop ((remaining values)
             (idx 0))
    (unless (null? remaining)
      (ensure-yices-term (car remaining)
                         (string-append label
                                        "["
                                        (object->string idx)
                                        "]"))
      (loop (cdr remaining) (+ idx 1)))))

(define (sal-expr->string expr)
  (with-output-to-string
    (lambda ()
      (sal/pp expr))))

(define (sal-expr/conjuncts expr)
  (cond
   ((sal-expr/true? expr)
    '())
   ((instance-of? expr <sal-and>)
    (apply append (map sal-expr/conjuncts
                       (sal-application/argument-list expr))))
   (else
    (list expr))))

(define (sal-expr/disjuncts expr)
  (cond
   ((sal-expr/false? expr)
    '())
   ((instance-of? expr <sal-or>)
    (apply append (map sal-expr/disjuncts
                       (sal-application/argument-list expr))))
   (else
    (list expr))))

(define (sal-expr/from-conjuncts exprs place-provider)
  (sal-ast/simplify (make-sal-and* exprs place-provider)))

(define (sal-expr/from-disjuncts exprs place-provider)
  (sal-ast/simplify (make-sal-or* exprs place-provider)))

(define (make-yices-id->decl-proc info)
  (let ((id->decl-mapping (slot-value info :id->decl-mapping)))
    (lambda (id)
      (cond
       ((eq-hash-table/get id->decl-mapping id) => cdr)
       (else #f)))))

(define (collect-formula-decls formulas keep-decls)
  (let ((table (make-eq-hash-table))
        (result '()))
    (define (remember! decl)
      (unless (eq-hash-table/contains? table decl)
        (eq-hash-table/put! table decl #unspecified)
        (set! result (cons decl result))))
    (for-each remember! keep-decls)
    (for-each
     (lambda (expr)
       (for-each remember!
                 (append (sal-ast/open-reference-list expr)
                         (sal-ast/collect-used-auxiliary-decls expr))))
     formulas)
    (reverse! result)))

(define (dedupe-decls decls)
  (let ((table (make-eq-hash-table))
        (result '()))
    (for-each
     (lambda (decl)
       (when (and decl
                  (not (eq-hash-table/contains? table decl)))
         (eq-hash-table/put! table decl #unspecified)
         (set! result (cons decl result))))
     decls)
    (reverse! result)))

(define (compact-declaration-queue! ctx)
  (let* ((decls (dedupe-decls (queue->list (slot-value ctx :declaration-queue))))
         (queue (make-queue)))
    (for-each (lambda (decl)
                (queue/insert! queue decl))
              decls)
    (set-slot-value! ctx :declaration-queue queue)
    decls))

(define (collect-unsynced-decls synced ctx-decls extra-decls)
  (let ((pending (make-eq-hash-table))
        (result '()))
    (define (remember! decl)
      (when (and decl
                 (not (eq-hash-table/contains? synced decl))
                 (not (eq-hash-table/contains? pending decl)))
        (eq-hash-table/put! pending decl #unspecified)
        (set! result (cons decl result))))
    (for-each remember! ctx-decls)
    (for-each remember! extra-decls)
    (reverse! result)))

(define (render-yices2-type-string type info)
  (normalize-yices-inline
   (with-output-to-string
     (lambda ()
       (sal-type/display-yices2 type info)))))

(define (render-yices2-expr-string expr info)
  (normalize-yices-inline
   (with-output-to-string
     (lambda ()
       (sal-ast/display-yices2 expr info)))))

(define (make-mapped-yices2-translation-info solver decls)
  (let* ((smt-info (slot-value solver :info))
         (info (make-yices2-translation-info)))
    (for-each
     (lambda (decl)
       (sal-type/collect-yices2-sorts! (slot-value decl :type) info)
       (let ((id (smtlib2-translation-info/var-id smt-info decl)))
         (when id
           (eq-hash-table/put! (slot-value info :decl->id-mapping) decl id)
           (eq-hash-table/put! (slot-value info :id->decl-mapping) id decl))))
     decls)
    info))

(define (yices-inline-whitespace? c)
  (or (char=? c #\space)
      (char=? c #\tab)
      (char=? c #\newline)
      (char=? c #\return)))

(define (normalize-yices-inline str)
  (let ((len (string-length str)))
    (with-output-to-string
      (lambda ()
        (let loop ((i 0)
                   (emitted? #f)
                   (pending-space? #f))
          (when (< i len)
            (let ((c (string-ref str i)))
              (if (yices-inline-whitespace? c)
                (loop (+ i 1)
                      emitted?
                      emitted?)
                (begin
                  (when pending-space?
                    (display #\space))
                  (display c)
                  (loop (+ i 1)
                        #t
                        #f))))))))))

(define (decl->generalization-spec decl info)
  (cons (generalization-id->string (dp-translation-info/var-id info decl))
        (render-yices2-type-string (slot-value decl :type) info)))

(define (generalization-id->string id)
  (cond
   ((string? id) id)
   ((symbol? id) (symbol->string id))
   (else
    (object->string id))))

(define (expr->generalization-conjunct-strings expr info place-provider)
  (let ((conjuncts (sal-expr/conjuncts expr)))
    (map (lambda (conjunct)
           (render-yices2-expr-string conjunct info))
         (if (null? conjuncts)
           (list (make-sal-true place-provider))
           conjuncts))))

(define (helper-generalize-formulas solver keep-decls formulas)
  (let* ((decls (collect-formula-decls formulas keep-decls))
         (_ (let* ((info (slot-value solver :info))
                   (yices-info (make-yices2-translation-info)))
              (sat-smtlib2-context/collect-translation-info! (slot-value solver :ctx) info)
              (for-each
               (lambda (decl)
                 (sal-type/collect-yices2-sorts! (slot-value decl :type) yices-info))
               decls)
              (for-each (lambda (sort-id)
                          (solver/ensure-yices-sort! solver sort-id))
                        (queue->list (slot-value yices-info :sort-id-queue)))
              (for-each
               (lambda (decl)
                 (let ((name (smt-id->string (smtlib2-translation-info/var-id info decl))))
                   (unless (solver/lookup-yices-term solver name)
                     (solver/ensure-yices-decl! solver
                                                name
                                                (render-yices2-type-string (slot-value decl :type)
                                                                           yices-info)))))
               decls)))
         (_ (for-each
             (lambda (decl)
               (let* ((name (smt-id->string (smtlib2-translation-info/var-id (slot-value solver :info) decl)))
                      (type-info (make-yices2-translation-info)))
                 (sal-type/collect-yices2-sorts! (slot-value decl :type) type-info)
                 (unless (solver/lookup-yices-term solver name)
                   (solver/ensure-helper-term! solver
                                               name
                                               (render-yices2-type-string (slot-value decl :type) type-info)))))
             decls))
         (generalized-terms
         (yices2-api/generalize-terms
           (apply append
                  (map (lambda (expr)
                         (map (lambda (conjunct)
                                (solver/expr->term/presynced solver conjunct))
                              (let ((conjuncts (sal-expr/conjuncts expr)))
                                (if (null? conjuncts)
                                  (list (make-sal-true (slot-value solver :flat-module)))
                                  conjuncts))))
                       formulas))
           (map-and-filter
            (lambda (decl)
              (and (not (member decl keep-decls))
                   (solver/lookup-yices-term
                    solver
                    (smt-id->string
                     (smtlib2-translation-info/var-id (slot-value solver :info) decl)))))
            decls)))
         (id->decl-proc (make-id->decl-proc (slot-value solver :info)))
         (exprs
          (map (lambda (term)
                 (multiple-value-bind
                     (state-constraint _max-step)
                     (build-state-constraint
                      (yices2/string->sal-expr (normalize-yices-inline (yices2-api/term->string term))
                                               id->decl-proc
                                               (slot-value solver :flat-module))
                      (slot-value (slot-value solver :ctx) :inv-step-decls)
                      (slot-value (slot-value solver :ctx) :inv-global-decls))
                   state-constraint))
               generalized-terms)))
      (sal-expr/from-conjuncts (if (null? exprs)
                                (list (make-sal-true (slot-value solver :flat-module)))
                                exprs)
                              (slot-value solver :flat-module)))))

(define (tracked-state-vars flat-module)
  (let ((defined-vars (sal-module/defined-variables flat-module)))
    (map-and-filter
     (lambda (decl)
       (and (not (instance-of? decl <sal-input-state-var-decl>))
            (not (instance-of? decl <sal-choice-input-state-var-decl>))
            (not (eq-hash-table/contains? defined-vars decl))
            decl))
     (sal-module/state-variables flat-module))))

(define (lookup-step-decl ctx decl step)
  (cond
   ((eq-hash-table/get (sat-bmc-context/decl-table-at ctx step) decl) => cdr)
   (else #f)))

(define (render-smt2-expr expr info)
  (with-output-to-string
    (lambda ()
      (sat-smtlib2-context/display-expr expr info))))

(define (render-smt2-assert expr info)
  (with-output-to-string
    (lambda ()
      (sat-smtlib2-context/display-assert expr info))))

(define (render-state-assert solver expr step)
  (render-smt2-assert (sal-ast->sat expr
                                    (slot-value solver :ctx)
                                    *cdr-empty-env*
                                    step
                                    *pos*)
                      (slot-value solver :info)))

(define (render-state-expr solver expr step)
  (render-smt2-expr (sal-ast->sat expr
                                  (slot-value solver :ctx)
                                  *cdr-empty-env*
                                  step
                                  *pos*)
                    (slot-value solver :info)))

(define (solver/lookup-or-sync-yices-term solver name)
  (or (solver/lookup-yices-term solver name)
      (begin
        (solver/sync-yices-translation! solver)
        (solver/lookup-yices-term solver name))))

(define (solver/expr->term/with-lookup solver expr lookup-term)
  (let ((cache (slot-value solver :expr-term-cache))
        (text-cache (slot-value solver :expr-text-term-cache))
        (info (slot-value solver :info)))
    (define (build expr)
      (cond
       ((eq-hash-table/get cache expr) => cdr)
       (else
        (let ((expr-text (render-smt2-expr expr info)))
          (cond
           ((hashtable-get text-cache expr-text)
           =>
            (lambda (term)
              (eq-hash-table/put! cache expr term)
              term))
           (else
            (try
             (let ((term (solver/expr->term/core solver expr build lookup-term)))
               (eq-hash-table/put! cache expr term)
               (hashtable-put! text-cache expr-text term)
               term)
             (lambda (_escape _proc msg _obj)
               (sign-error "sal-cdr failed to construct a native Yices term for subexpression:\n~a\nReason: ~a"
                           expr-text
                           msg)))))))))
    (build expr)))

(define (solver/expr->term solver expr)
  (solver/sync-yices-translation! solver
                                  (collect-formula-decls (list expr) '()))
  (solver/expr->term/with-lookup
   solver
   expr
   (lambda (name)
     (solver/lookup-or-sync-yices-term solver name))))

(define (solver/expr->term/presynced solver expr)
  (solver/expr->term/with-lookup
   solver
   expr
   (lambda (name)
     (solver/lookup-yices-term solver name))))

(define *native-yices-parse-fallback-arity* 8)

(define (solver/expr->term/core solver expr recur lookup-term)
    (cond
     ((instance-of? expr <sal-true>)
      (yices2-api/true-term))
     ((instance-of? expr <sal-false>)
      (yices2-api/false-term))
     ((instance-of? expr <sal-numeral>)
      (yices2-api/rational-term (mpq->string (slot-value expr :num))))
     ((instance-of? expr <sal-name-expr>)
      (let* ((name (smt-id->string
                    (smtlib2-translation-info/var-id (slot-value solver :info)
                                                     (slot-value expr :decl))))
             (term (lookup-term name)))
        (unless term
          (sign-error "sal-cdr failed to map SAL declaration ~a to a native Yices term."
                      (sal-decl/name (slot-value expr :decl))))
        term))
     ((instance-of? expr <sal-eq>)
      (multiple-value-bind
          (arg1 arg2)
          (sal-binary-application/arguments expr)
        (yices2-api/eq-term (recur arg1)
                            (recur arg2))))
     ((instance-of? expr <sal-diseq>)
      (multiple-value-bind
          (arg1 arg2)
          (sal-binary-application/arguments expr)
        (yices2-api/neq-term (recur arg1)
                             (recur arg2))))
     ((instance-of? expr <sal-and>)
      (let ((args (sal-application/argument-list expr)))
        (if (> (length args) *native-yices-parse-fallback-arity*)
          ;; Large boolean n-ary terms are still handled natively by Yices,
          ;; but parsing them in-process is more robust than constructing one
          ;; huge term array by hand.
          (solver/expr->parsed-term solver expr)
          (yices2-api/and-terms (map recur args)))))
     ((instance-of? expr <sal-or>)
      (let ((args (sal-application/argument-list expr)))
        (if (> (length args) *native-yices-parse-fallback-arity*)
          (solver/expr->parsed-term solver expr)
          (yices2-api/or-terms (map recur args)))))
     ((instance-of? expr <sal-not>)
      (yices2-api/not-term
       (recur (car (sal-application/argument-list expr)))))
     ((instance-of? expr <sal-implies>)
      (multiple-value-bind
          (arg1 arg2)
          (sal-binary-application/arguments expr)
        (yices2-api/implies-term (recur arg1)
                                 (recur arg2))))
     ((instance-of? expr <sal-iff>)
      (multiple-value-bind
          (arg1 arg2)
          (sal-binary-application/arguments expr)
        (yices2-api/iff-term (recur arg1)
                             (recur arg2))))
     ((instance-of? expr <sal-xor>)
      (multiple-value-bind
          (arg1 arg2)
          (sal-binary-application/arguments expr)
        (yices2-api/neq-term (recur arg1)
                             (recur arg2))))
     ((instance-of? expr <sal-add>)
      (let ((numerals (sal-numeral-list-values (sal-application/argument-list expr))))
        (if numerals
          (yices2-api/rational-term
           (mpq->string
            (let loop ((remaining numerals)
                       (result *mpq-zero*))
              (if (null? remaining)
                result
                (loop (cdr remaining)
                      (+mpq result (car remaining)))))))
          (yices2-api/add-terms
           (map recur
                (sal-application/argument-list expr))))))
     ((instance-of? expr <sal-sub>)
      (multiple-value-bind
          (arg1 arg2)
          (sal-binary-application/arguments expr)
        (let ((num1 (sal-numeral-value arg1))
              (num2 (sal-numeral-value arg2)))
          (if (and num1 num2)
            (yices2-api/rational-term (mpq->string (-mpq num1 num2)))
            (if (and num1
                     (=mpq num1 *mpq-zero*))
              (yices2-api/neg-term (recur arg2))
              (yices2-api/sub-term (recur arg1)
                                   (recur arg2)))))))
     ((instance-of? expr <sal-mul>)
      (let ((numerals (sal-numeral-list-values (sal-application/argument-list expr))))
        (if numerals
          (yices2-api/rational-term
           (mpq->string
            (let loop ((remaining numerals)
                       (result *mpq-one*))
              (if (null? remaining)
                result
                (loop (cdr remaining)
                      (*mpq result (car remaining)))))))
          (let ((args (sal-application/argument-list expr)))
            (if (and (= (length args) 2))
              (let* ((arg1 (car args))
                     (arg2 (cadr args))
                     (num1 (sal-numeral-value arg1))
                     (num2 (sal-numeral-value arg2)))
                (cond
                 ((and num1 (=mpq num1 *mpq-one*))
                  (recur arg2))
                 ((and num2 (=mpq num2 *mpq-one*))
                  (recur arg1))
                 ((and num1 (=mpq num1 *mpq-zero*))
                  (yices2-api/rational-term "0"))
                 ((and num2 (=mpq num2 *mpq-zero*))
                  (yices2-api/rational-term "0"))
                 ((and num1 (=mpq num1 *mpq-minus-one*))
                  (yices2-api/neg-term (recur arg2)))
                 ((and num2 (=mpq num2 *mpq-minus-one*))
                  (yices2-api/neg-term (recur arg1)))
                 (else
                  (yices2-api/mul-terms (map recur args)))))
              (yices2-api/mul-terms
               (map recur
                    args)))))))
     ((instance-of? expr <sal-div>)
      (multiple-value-bind
          (arg1 arg2)
          (sal-binary-application/arguments expr)
        (let ((num1 (sal-numeral-value arg1))
              (num2 (sal-numeral-value arg2)))
          (if (and num1 num2)
            (yices2-api/rational-term (mpq->string (/mpq num1 num2)))
            (yices2-api/division-term (recur arg1)
                                      (recur arg2))))))
     ((instance-of? expr <sal-idiv>)
      (multiple-value-bind
          (arg1 arg2)
          (sal-binary-application/arguments expr)
        (let ((num1 (sal-numeral-value arg1))
              (num2 (sal-numeral-value arg2)))
          (if (and num1 num2)
            (yices2-api/rational-term (mpq->string (div-mpq num1 num2)))
            (yices2-api/idiv-term (recur arg1)
                                  (recur arg2))))))
     ((instance-of? expr <sal-lt>)
      (multiple-value-bind
          (arg1 arg2)
          (sal-binary-application/arguments expr)
        (yices2-api/lt0-term
         (yices2-api/sub-term (recur arg1)
                              (recur arg2)))))
     ((instance-of? expr <sal-le>)
      (multiple-value-bind
          (arg1 arg2)
          (sal-binary-application/arguments expr)
        (yices2-api/le0-term
         (yices2-api/sub-term (recur arg1)
                              (recur arg2)))))
     ((instance-of? expr <sal-gt>)
      (multiple-value-bind
          (arg1 arg2)
          (sal-binary-application/arguments expr)
        (yices2-api/gt0-term
         (yices2-api/sub-term (recur arg1)
                              (recur arg2)))))
     ((instance-of? expr <sal-ge>)
      (multiple-value-bind
          (arg1 arg2)
          (sal-binary-application/arguments expr)
        (yices2-api/ge0-term
         (yices2-api/sub-term (recur arg1)
                              (recur arg2)))))
     ((instance-of? expr <sal-conditional>)
      (yices2-api/ite-term (recur (slot-value expr :cond-expr))
                           (recur (slot-value expr :then-expr))
                           (recur (slot-value expr :else-expr))))
     ((instance-of? expr <sal-function-update>)
      (yices2-api/update-term
       (recur (slot-value expr :target))
       (map recur
            (sal-argument->argument-list (slot-value expr :idx) #f))
       (recur (slot-value expr :new-value))))
     ((instance-of? expr <sal-application>)
      (yices2-api/application-term
       (recur (slot-value expr :fun))
       (map recur
            (sal-application/argument-list expr))))
     (else
      (sign-unsupported-feature expr "Failed to translate a CDR query formula to a native Yices term."))))

(define (solver/state-expr->term solver expr step)
  (solver/expr->term solver
                     (sal-ast->sat expr
                                   (slot-value solver :ctx)
                                   *cdr-empty-env*
                                   step
                                   *pos*)))

(define (solver/expr->parsed-term solver expr)
  (let* ((decls (collect-formula-decls (list expr) '()))
         (_ (solver/sync-yices-translation! solver decls))
         (yices-info (make-mapped-yices2-translation-info solver decls))
         (text (render-yices2-expr-string expr yices-info)))
    (yices2-api/parse-term text)))

(define (solver/state-expr->parsed-term solver expr step)
  (solver/expr->parsed-term solver
                            (sal-ast->sat expr
                                          (slot-value solver :ctx)
                                          *cdr-empty-env*
                                          step
                                          *pos*)))

(define (sal-numeral-value expr)
  (and (instance-of? expr <sal-numeral>)
       (slot-value expr :num)))

(define (sal-numeral-list-values exprs)
  (let loop ((remaining exprs)
             (result '()))
    (cond
     ((null? remaining)
      (reverse! result))
     ((sal-numeral-value (car remaining))
      =>
      (lambda (value)
        (loop (cdr remaining) (cons value result))))
     (else
      #f))))

(define (render-smt2-declaration decl info)
  (with-output-to-string
    (lambda ()
      (sat-smtlib2-context/display-declaration decl info))))

(define (declare-all-var-specs info ctx)
  (let* ((decls (queue->list (slot-value ctx :declaration-queue)))
         (yices-info (make-yices2-translation-info)))
    (for-each (lambda (decl)
                (sal-type/collect-yices2-sorts! (slot-value decl :type) yices-info))
              decls)
    (map (lambda (decl)
           (cons (smt-id->string (smtlib2-translation-info/var-id info decl))
                 (render-yices2-type-string (slot-value decl :type) yices-info)))
         decls)))

(define (smt-id->string id)
  (cond
   ((string? id) id)
   ((symbol? id) (symbol->string id))
   (else
    (object->string id))))

(define (solver/lookup-yices-term solver name)
  (or (hashtable-get (slot-value solver :decl-name->term) name)
      (hashtable-get (slot-value solver :helper-name->term) name)))

(define (solver/ensure-yices-sort! solver sort-id)
  (let ((name (smt-id->string sort-id)))
    (unless (hashtable-get (slot-value solver :registered-sorts) name)
      (yices2-api/declare-sort! name)
      (hashtable-put! (slot-value solver :registered-sorts) name #t))))

(define (solver/ensure-yices-decl! solver name type-string)
  (or (hashtable-get (slot-value solver :decl-name->term) name)
      (let ((term (yices2-api/declare-uninterpreted-term! name type-string)))
        (hashtable-put! (slot-value solver :decl-name->term) name term)
        term)))

(define (solver/ensure-helper-term! solver name type-string)
  (or (hashtable-get (slot-value solver :helper-name->term) name)
      (let ((term (yices2-api/declare-uninterpreted-term! name type-string)))
        (hashtable-put! (slot-value solver :helper-name->term) name term)
        term)))

(define (solver/sync-yices-translation! solver . maybe-extra-decls)
  (let* ((ctx (slot-value solver :ctx))
         (info (slot-value solver :info))
         (synced (slot-value solver :synced-decls))
         (extra-decls (if (null? maybe-extra-decls)
                        '()
                        (car maybe-extra-decls)))
         (ctx-decls (queue->list (slot-value ctx :declaration-queue)))
         (new-decls (collect-unsynced-decls synced
                                            ctx-decls
                                            extra-decls))
         (yices-info (make-yices2-translation-info)))
    (sat-smtlib2-context/collect-translation-info! ctx info)
    (for-each
     (lambda (decl)
       (sal-type/collect-yices2-sorts! (slot-value decl :type) yices-info))
     new-decls)
    (for-each (lambda (sort-id)
                (solver/ensure-yices-sort! solver sort-id))
              (queue->list (slot-value yices-info :sort-id-queue)))
    (for-each
     (lambda (decl)
       (let ((name (smt-id->string (smtlib2-translation-info/var-id info decl))))
         (unless (hashtable-get (slot-value solver :decl-name->term) name)
           (solver/ensure-yices-decl! solver
                                      name
                                      (render-yices2-type-string (slot-value decl :type)
                                                                 yices-info)))))
     new-decls)
    (for-each (lambda (decl)
                (eq-hash-table/put! synced decl #unspecified))
              new-decls)))

(define (solver/native-term-owned? solver term)
  (let ((name (yices2-api/term-name term)))
    (and name
         (let ((known (solver/lookup-yices-term solver name)))
           (and known
                (= known term))))))

(define (solver/collect-foreign-yices-terms solver root-term)
  (let ((seen (make-eq-hash-table))
        (result '()))
    (define (remember! term)
      (unless (find (lambda (candidate)
                      (= candidate term))
                    result)
        (set! result (cons term result))))
    (define (visit! term)
      (unless (eq-hash-table/contains? seen term)
        (eq-hash-table/put! seen term #unspecified)
        (let ((ctor (yices2-api/term-constructor term)))
          (cond
           ((or (= ctor *yices-constructor-variable*)
                (= ctor *yices-constructor-uninterpreted-term*))
            (unless (solver/native-term-owned? solver term)
              (remember! term)))
           ((or (= ctor *yices-constructor-bv-sum*)
                (= ctor *yices-constructor-arith-sum*)
                (= ctor *yices-constructor-arith-ff-sum*))
            (let ((n (yices2-api/term-num-children term)))
              (let loop ((i 0))
                (when (< i n)
                  (let ((component (yices2-api/sum-component-term term i)))
                    (when (>= component 0)
                      (visit! component)))
                  (loop (+ i 1))))))
           ((= ctor *yices-constructor-power-product*)
            (let ((n (yices2-api/term-num-children term)))
              (let loop ((i 0))
                (when (< i n)
                  (let ((factor (yices2-api/product-component-term term i)))
                    (when (>= factor 0)
                      (visit! factor)))
                  (loop (+ i 1))))))
           (else
            (let ((n (yices2-api/term-num-children term)))
              (let loop ((i 0))
                (when (< i n)
                  (visit! (yices2-api/term-child term i))
                  (loop (+ i 1))))))))))
    (visit! root-term)
    (reverse! result)))

(define (session/sanitize-interpolant-term session term model)
  (let ((solver (slot-value session :solver)))
    (let loop ((current term)
               (round 0))
      (let ((foreign (solver/collect-foreign-yices-terms solver current)))
        (cond
         ((null? foreign)
          (when (>= (verbosity-level) 6)
            (verbose-message 6 "sal-cdr: interpolant term has no foreign native leaves: ~a"
                             (yices2-api/term->string current)))
          current)
         ((>= round 4)
          (sign-error "sal-cdr failed to eliminate native Yices helper terms from interpolant:\n~a"
                      (yices2-api/term->string current)))
         (else
          (when (>= (verbosity-level) 5)
            (verbose-message 5 "sal-cdr: sanitizing interpolant term by eliminating ~a foreign Yices term(s): ~a"
                             (length foreign)
                             (map (lambda (foreign-term)
                                    (or (yices2-api/term-name foreign-term)
                                        (yices2-api/term->string foreign-term)))
                                  foreign)))
          (let* ((generalized-negated
                  (yices2-api/generalize-model-terms model
                                                     (list (yices2-api/not-term current))
                                                     foreign))
                 (sanitized
                  (yices2-api/not-term (yices2-api/and-terms generalized-negated))))
            (loop sanitized (+ round 1)))))))))

(define (session/backend-error session msg . args)
  (sign-error "~a\nSession: ~a\nBackend: ~a"
              (apply format #f msg args)
              (slot-value session :name)
              (slot-value session :command)))

(define (extract-status session status)
  (let ((label (yices2-api/status->string status)))
    (cond
     ((or (equal? label "sat")
          (equal? label "unsat")
          (equal? label "unknown"))
      label)
     (else
      (session/backend-error session "Expected sat/unsat/unknown, received Yices status ~a." label)))))

(define (make-id->decl-proc info)
  (let ((id->decl-mapping (sat-smtlib2-context/translation-id->decl-mapping info)))
    (lambda (id)
      (cond
       ((eq-hash-table/get id->decl-mapping id) => cdr)
       (else #f)))))

(define (make-sort->type-proc info)
  (let ((sort->type (sat-smtlib2-context/translation-sort->type info)))
    (lambda (sort-id)
      (cond
       ((hashtable-get sort->type sort-id) => identity)
       (else #f)))))

(define (make-session-base-terms solver expr-list)
  (map (lambda (expr)
         (solver/expr->term solver expr))
       expr-list))

(define (make-cdr-yices2-session solver name command logic interpolants? sort-ids declarations base-terms id->decl-proc sort->type-proc place-provider)
  (verbose-message 1 "sal-cdr: creating solver session ~a..." name)
  (ensure-yices-term-list base-terms "base terms")
  (solver/sync-yices-translation! solver)
  (let* ((ctx (yices2-api/new-context interpolants?))
         (session (make-instance <cdr-yices2-session>
                                 :name name
                                 :solver solver
                                 :context ctx
                                 :command (ensure-string-value command "the solver command")
                                 :logic (ensure-string-value logic "the SMT-LIB2 logic")
                                 :interpolants? interpolants?
                                 :process #f
                                 :input-port #f
                                 :output-port #f
                                 :transcript '()
                                 :echo-counter 0
                                 :id->decl-proc id->decl-proc
                                 :sort->type-proc sort->type-proc
                                 :place-provider place-provider)))
    (for-each (lambda (term)
                (yices2-api/assert-formula! ctx term))
              base-terms)
    session))

(define (session/close! session)
  (when session
    (yices2-api/free-context! (slot-value session :context))
    (set-slot-value! session :context #f)))

(define (session/assert-permanent! session term)
  (ensure-yices-term term "a permanent assertion")
  (yices2-api/assert-formula! (slot-value session :context)
                              term))

(define (session/push-asserts! session terms)
  (ensure-yices-term-list terms "pushed assertions")
  (yices2-api/context-push! (slot-value session :context))
  (for-each (lambda (term)
              (yices2-api/assert-formula! (slot-value session :context) term))
            terms))

(define (session/push-assert! session term)
  (ensure-yices-term term "a pushed assertion")
  (verbose-message 5 "sal-cdr: session ~a push/assert..." (slot-value session :name))
  (yices2-api/context-push! (slot-value session :context))
  (yices2-api/assert-formula! (slot-value session :context)
                              term))

(define (session/pop! session)
  (yices2-api/context-pop! (slot-value session :context)))

(define (session/reset! session)
  (yices2-api/context-reset! (slot-value session :context)))

(define (session/check-sat! session)
  (verbose-message 5 "sal-cdr: session ~a check-sat..." (slot-value session :name))
  (extract-status session
                  (yices2-api/check-context (slot-value session :context))))

(define (session/query-with-assertion session term)
  (session/push-assert! session term)
  (let ((status (session/check-sat! session)))
    (unwind-protect
     status
     (session/pop! session))))

(define (session/query-with-assertions session terms)
  (session/push-asserts! session terms)
  (let ((status (session/check-sat! session)))
    (unwind-protect
     status
     (session/pop! session))))

(define (query-term-bindings solver step)
  (let* ((step-bindings
          (map-and-filter
           (lambda (decl)
             (let ((step-decl (lookup-step-decl (slot-value solver :ctx) decl step)))
               (and step-decl
                    (cons decl step-decl))))
           (cdr-solver/state-vars solver)))
         (_ (solver/sync-yices-translation! solver (map cdr step-bindings))))
    (map-and-filter
     (lambda (binding)
       (let* ((decl (car binding))
              (step-decl (cdr binding))
              (var-id (smtlib2-translation-info/var-id (slot-value solver :info) step-decl)))
         (and var-id
              (let ((term (solver/lookup-yices-term solver (smt-id->string var-id))))
                (and term
                     (cons decl term))))))
     step-bindings)))

(define (session/term->sal-expr session term)
  (yices2/string->sal-expr (normalize-yices-inline (yices2-api/term->string term))
                           (slot-value session :id->decl-proc)
                           (slot-value session :place-provider)))

(define (cube->yices-model-binding-terms solver cube step)
  (let* ((ctx (slot-value solver :ctx))
         (info (slot-value solver :info))
         (step-decls
          (map-and-filter
           (lambda (binding)
             (lookup-step-decl ctx (car binding) step))
           (cdr-cube/bindings cube)))
         (_ (solver/sync-yices-translation! solver step-decls)))
    (map
     (lambda (binding)
       (let* ((decl (car binding))
              (value (cdr binding))
              (step-decl (lookup-step-decl ctx decl step))
              (var-id (and step-decl
                           (smtlib2-translation-info/var-id info step-decl)))
              (var-term (and var-id
                             (solver/lookup-yices-term solver (smt-id->string var-id)))))
         (unless var-term
           (sign-error "sal-cdr failed to map cube binding ~a at step ~a to a native Yices term."
                       (sal-decl/name decl)
                       step))
         (cons var-term
               (solver/state-expr->term solver value step))))
     (cdr-cube/bindings cube))))

(define (cube->yices-model solver cube step)
  (let* ((bindings (cube->yices-model-binding-terms solver cube step))
         (vars (map car bindings))
         (value-terms (map cdr bindings)))
    (values vars
            (yices2-api/model-from-map vars value-terms))))

(define (session/check-sat-with-model! session model terms)
  (extract-status session
                  (yices2-api/check-context-with-model (slot-value session :context)
                                                       model
                                                       terms)))

(define (session/get-model-cube solver session step)
  (let* ((term-bindings (query-term-bindings solver step))
         (_ (when (>= (verbosity-level) 5)
              (verbose-message 5 "sal-cdr: extracting step-~a cube from native session ~a via direct model values"
                               step
                               (slot-value session :name))))
         (model #f))
    (unwind-protect
     (begin
       (set! model (yices2-api/get-model (slot-value session :context)))
       (make-cdr-cube
        (let loop ((remaining term-bindings)
                   (result '()))
          (if (null? remaining)
            (reverse! result)
            (let* ((binding (car remaining))
                   (decl (car binding))
                   (term (cdr binding))
                   (value-term (yices2-api/try-value-as-term model term)))
              (cond
               (value-term
                (loop (cdr remaining)
                      (cons (cons decl
                                  (session/term->sal-expr session value-term))
                            result)))
               (else
                (when (>= (verbosity-level) 4)
                  (verbose-message 4
                                   "sal-cdr: omitting ~a from the native model cube at step ~a because Yices could not convert its value to a SAL term."
                                   (sal-decl/name decl)
                                   step))
                (loop (cdr remaining) result))))))))
     (yices2-api/free-model! model))))

(define (session/get-interpolant-expr solver session model)
  (let* ((raw-term (yices2-api/get-model-interpolant (slot-value session :context)))
         (_ (when (>= (verbosity-level) 6)
              (verbose-message 6 "sal-cdr: raw native interpolant term: ~a"
                               (yices2-api/term->string raw-term))))
         (term (session/sanitize-interpolant-term session raw-term model))
         (form (session/term->sal-expr session term)))
    (multiple-value-bind
        (expr _max-step)
        (build-state-constraint form
                                (slot-value (slot-value solver :ctx) :inv-step-decls)
                                (slot-value (slot-value solver :ctx) :inv-global-decls))
      expr)))

(define (smtlib2-root-constraint-elided? expr)
  ;; Match sat-smtlib2-context/display-assert: root int/real predicates are
  ;; preprocessing artifacts and are intentionally omitted from the emitted
  ;; SMT-LIB2 assertions.
  (or (instance-of? expr <sal-int-pred>)
      (instance-of? expr <sal-real-pred>)))

(define (constraint-queue->expr ctx place-provider)
  (let ((constraints (remove-if smtlib2-root-constraint-elided?
                                (queue->list (slot-value ctx :constraint-queue)))))
    (cond
     ((null? constraints)
      (make-sal-true place-provider))
     ((null? (cdr constraints))
      (car constraints))
     (else
     (make-sal-and* constraints place-provider)))))

(define (ensure-skeleton-decls! ctx expr)
  (let ((decl-table (make-eq-hash-table)))
    (for-each (lambda (decl)
                (eq-hash-table/put! decl-table decl #unspecified))
              (queue->list (slot-value ctx :declaration-queue)))
    (for-each
     (lambda (decl)
       (unless (eq-hash-table/contains? decl-table decl)
         (queue/insert! (slot-value ctx :declaration-queue) decl)
         (eq-hash-table/put! decl-table decl #unspecified)))
     (append (sal-ast/open-reference-list expr)
             (sal-ast/collect-used-auxiliary-decls expr)))))

(define (translate-simplified-skeleton! ctx flat-module step-expr-list)
  ;; Reuse SAL's standard SMT simplification pipeline so every helper
  ;; declaration and side constraint needed by the translated skeleton is
  ;; asserted explicitly in the persistent CDR backend.
  (set-slot-value! ctx :constraint-queue (make-queue))
  (set-slot-value! ctx :already-processed (make-eq-hash-table))
  (for-each
   (lambda (step-expr)
     (sat-context/assert ctx
                         (sal-ast->sat (cdr step-expr)
                                       ctx
                                       *cdr-empty-env*
                                       (car step-expr)
                                       *pos*)))
   step-expr-list)
  (sat-generic-context/simplify! ctx
                                 :ite->ite-bool? #f
                                 :eliminate-div-mod? #f)
  (let ((expr (constraint-queue->expr ctx flat-module)))
    (ensure-skeleton-decls! ctx expr)
    expr))

(define (render-skeleton-assert solver step-expr-list)
  (let* ((ctx (slot-value solver :ctx))
         (info (slot-value solver :info))
         (expr (translate-simplified-skeleton! ctx
                                               (slot-value solver :flat-module)
                                               step-expr-list)))
    (sat-smtlib2-context/collect-translation-info! ctx info)
    (render-smt2-assert expr info)))

(define (cube-assert-term solver cube step)
  (solver/state-expr->term solver
                           (cdr-cube->expr cube (slot-value solver :flat-module))
                           step))

(define (session/query-cube-status solver session cube step)
  (when (>= (verbosity-level) 5)
    (verbose-message 5 "sal-cdr: preparing cube query for session ~a at step ~a"
                     (slot-value session :name)
                     step))
  (let ((assert-term (cube-assert-term solver cube step)))
    (when (>= (verbosity-level) 5)
      (verbose-message 5 "sal-cdr: cube assertion for session ~a: ~a"
                       (slot-value session :name)
                       (yices2-api/term->string assert-term)))
    (session/push-assert! session assert-term)
    (let ((status (session/check-sat! session)))
      (when (>= (verbosity-level) 5)
        (verbose-message 5 "sal-cdr: cube query status for session ~a: ~a"
                         (slot-value session :name)
                         status))
      (unwind-protect
       status
       (session/pop! session)))))

(define (session/query-cube-model solver session cube query-step model-step)
  (when (>= (verbosity-level) 5)
    (verbose-message 5 "sal-cdr: preparing cube+model query for session ~a at step ~a"
                     (slot-value session :name)
                     query-step))
  (let ((assert-term (cube-assert-term solver cube query-step)))
    (when (>= (verbosity-level) 5)
      (verbose-message 5 "sal-cdr: cube assertion for session ~a: ~a"
                       (slot-value session :name)
                       (yices2-api/term->string assert-term)))
    (session/push-assert! session assert-term)
    (let ((status (session/check-sat! session)))
      (when (>= (verbosity-level) 5)
        (verbose-message 5 "sal-cdr: cube+model query status for session ~a: ~a"
                         (slot-value session :name)
                         status))
      (unwind-protect
       (cond
        ((equal? status "sat")
         (values "sat" (session/get-model-cube solver session model-step)))
        (else
         (values status #f)))
       (session/pop! session)))))

(define (build-translation-ctx assertion)
  (let* ((flat-module (slot-value assertion :module))
         (ctx (make-sat-smtlib2-bmc-context flat-module (lambda (_) #unspecified)))
         (_ (unless (slot-value ctx :scalar->int-trace-info)
              (set-slot-value! ctx :scalar->int-trace-info (make-eq-hash-table))))
         (_ (unless (slot-value ctx :scalar->bool-trace-info)
              (set-slot-value! ctx :scalar->bool-trace-info (make-eq-hash-table))))
         (property (sal-module-models/invariant-body assertion))
         (definition (slot-value flat-module :definition))
         (initialization (slot-value flat-module :initialization))
         (transition (slot-value flat-module :transition))
         (valid-input (slot-value flat-module :valid-input-expr))
         (valid-state (slot-value flat-module :valid-state-expr))
         (i0 (translate-simplified-skeleton!
              ctx
              flat-module
              (list (cons 0 valid-state)
                    (cons 0 definition)
                    (cons 0 initialization))))
         (t01 (translate-simplified-skeleton!
               ctx
               flat-module
               (list (cons 0 valid-input)
                     (cons 0 valid-state)
                     (cons 1 valid-state)
                     (cons 0 definition)
                     (cons 1 definition)
                     (cons 0 transition))))
         (bad0 (translate-simplified-skeleton!
                ctx
                flat-module
                (list (cons 0 (make-sal-not property)))))
         (bad1 (translate-simplified-skeleton!
                ctx
                flat-module
                (list (cons 1 (make-sal-not property))))))
    (values ctx i0 t01 bad0 bad1)))

(define (shared-reach-selector-id level)
  (string-append "sal_cdr_reach_level_" (object->string level)))

(define (ensure-shared-reach-selector! solver level)
  (when (slot-value solver :shared-reach-session)
    (let ((lemmas (slot-value solver :reach-lemmas)))
      (when (>= level (vector-length lemmas))
        (sign-error "sal-cdr internal error: shared reachability selector out of bounds at F~a." level))
      (when (not (vector-ref lemmas level))
        (solver/ensure-helper-term! solver (shared-reach-selector-id level) "bool")
        (vector-set! lemmas level '())))))

(define (make-shared-reach-session solver)
  (let* ((ctx (slot-value solver :ctx))
         (info (slot-value solver :info)))
    (make-cdr-yices2-session
     solver
     "reach-shared"
     (slot-value solver :effective-command)
     (sat-smtlib2-context/logic ctx)
     (cdr-solver-capabilities/interpolants?
      (cdr-solver/capabilities solver))
     '()
     '()
     (make-session-base-terms solver (list (slot-value solver :t01)))
     (make-id->decl-proc info)
     (make-sort->type-proc info)
     (slot-value solver :flat-module))))

(define (shared-reach-level-terms solver level)
  (let loop ((i 0)
             (result (if (= level 0)
                       (list (solver/expr->term solver (slot-value solver :i0)))
                       '())))
    (if (>= i (slot-value solver :num-levels))
      (reverse! result)
      (loop (+ i 1)
            (cons (let ((selector-term (solver/lookup-yices-term
                                        solver
                                        (shared-reach-selector-id i))))
                    (unless selector-term
                      (sign-error "sal-cdr internal error: missing shared reachability selector F~a." i))
                    ;; Shared reachability queries must enable the full
                    ;; cumulative prefix F0..Fk, not just the exact level k.
                    (if (<= i level)
                      selector-term
                      (yices2-api/not-term selector-term)))
                  result)))))

(define (create-level-session! solver level)
  (let* ((ctx (slot-value solver :ctx))
         (info (slot-value solver :info))
         (reach-base (if (= level 0)
                       (list (slot-value solver :i0) (slot-value solver :t01))
                       (list (slot-value solver :t01))))
         (induction-base reach-base)
         (induction (make-cdr-yices2-session
                     solver
                     (string-append "induction-" (object->string level))
                     (slot-value solver :effective-command)
                     (sat-smtlib2-context/logic ctx)
                     #f
                     '()
                     '()
                     (make-session-base-terms solver induction-base)
                     (make-id->decl-proc info)
                     (make-sort->type-proc info)
                     (slot-value solver :flat-module))))
    (when (slot-value solver :shared-reach-session)
      (ensure-shared-reach-selector! solver level))
    (unless (slot-value solver :shared-reach-session)
      (vector-set! (slot-value solver :reach-sessions) level
                   (make-cdr-yices2-session
                    solver
                    (string-append "reach-" (object->string level))
                    (slot-value solver :effective-command)
                    (sat-smtlib2-context/logic ctx)
                    (cdr-solver-capabilities/interpolants?
                     (cdr-solver/capabilities solver))
                    '()
                    '()
                    (make-session-base-terms solver reach-base)
                    (make-id->decl-proc info)
                    (make-sort->type-proc info)
                    (slot-value solver :flat-module))))
    (vector-set! (slot-value solver :bad-sessions) level
                 (make-cdr-yices2-session
                  solver
                  (string-append "bad-" (object->string level))
                  (slot-value solver :effective-command)
                  (sat-smtlib2-context/logic ctx)
                  #f
                  '()
                  '()
                  (make-session-base-terms solver
                                           (if (= level 0)
                                             (list (slot-value solver :i0)
                                                   (slot-value solver :t01))
                                             (list (slot-value solver :t01))))
                  (make-id->decl-proc info)
                  (make-sort->type-proc info)
                  (slot-value solver :flat-module)))
    (vector-set! (slot-value solver :induction-sessions) level induction)))

(define (transition-step-term solver step)
  (solver/expr->term
   solver
   (translate-simplified-skeleton!
    (slot-value solver :ctx)
    (slot-value solver :flat-module)
    (list (cons step (slot-value solver :valid-input))
          (cons step (slot-value solver :valid-state))
          (cons (+ step 1) (slot-value solver :valid-state))
          (cons step (slot-value solver :definition))
          (cons (+ step 1) (slot-value solver :definition))
          (cons step (slot-value solver :transition))))))

(define (make-pdkind-induction-session solver depth)
  (let* ((ctx (slot-value solver :ctx))
         (info (slot-value solver :info))
         (transition-asserts
          (let loop ((step 0)
                     (result '()))
            (if (>= step depth)
              (reverse! result)
              (loop (+ step 1)
                    (cons (transition-step-term solver step)
                          result)))))
         (_ (sat-smtlib2-context/collect-translation-info! ctx info)))
    (make-cdr-yices2-session
     solver
     (string-append "pdkind-induction-" (object->string depth))
     (slot-value solver :effective-command)
     (sat-smtlib2-context/logic ctx)
     #f
     '()
     '()
     transition-asserts
     (make-id->decl-proc info)
     (make-sort->type-proc info)
     (slot-value solver :flat-module))))

(define (make-cdr-yices2-solver assertion pdkind?)
  (verbose-message 1 "sal-cdr: translating the SAL assertion into SMT-LIB2 skeletons...")
  (multiple-value-bind
      (ctx i0 t01 bad0 bad1)
      (build-translation-ctx assertion)
    (let* ((flat-module (slot-value assertion :module))
           (state-vars (tracked-state-vars flat-module))
           (property (sal-module-models/invariant-body assertion))
           (definition (slot-value flat-module :definition))
           (initialization (slot-value flat-module :initialization))
           (transition (slot-value flat-module :transition))
           (valid-input (slot-value flat-module :valid-input-expr))
           (valid-state (slot-value flat-module :valid-state-expr)))
      (for-each (lambda (decl)
                  (sat-bmc-context/decl-at ctx decl 0)
                  (sat-bmc-context/decl-at ctx decl 1))
                state-vars)
      (yices2-api/acquire!)
      (let ((constructed? #f))
        (unwind-protect
         (let* ((info (make-smtlib2-translation-info))
                (_ (sat-smtlib2-context/collect-translation-info! ctx info))
                (effective-command "native-yices2-api")
                (_ (verbose-message 1 "sal-cdr: using native Yices2 API backend"))
                (_ (verbose-message 1 "sal-cdr: checking native Yices2 MCSAT support..."))
                (_ (unless (yices2-api/has-mcsat?)
                     (sign-error "sal-cdr requires a Yices2 build with MCSAT support.")))
                (interpolants-supported? #t)
                (_ (verbose-message 1
                                    "sal-cdr: Yices2 unsat-model interpolants available: ~a"
                                    interpolants-supported?))
                (_ (verbose-message 1 "sal-cdr: querying native Yices2 version..."))
                (version-line (yices2-api/version-string))
                (solver (make-instance <cdr-yices2-solver>
                                       :capabilities (make-cdr-solver-capabilities
                                                      :incremental? #t
                                                      :models? #t
                                                      :interpolants? interpolants-supported?
                                                      :unsat-cores? #f)
                                       :flat-module flat-module
                                       :state-vars state-vars
                                       :trace-solver-id 'yices2
                                       :solver-description (string-append effective-command
                                                                          " ["
                                                                          version-line
                                                                          "]")
                                       :ctx ctx
                                       :info info
                                       :property property
                                       :definition definition
                                       :initialization initialization
                                       :transition transition
                                       :valid-input valid-input
                                       :valid-state valid-state
                                       :i0 i0
                                       :t01 t01
                                       :bad0 bad0
                                       :bad1 bad1
                                       :init-session #f
                                       :reach-sessions (make-vector 4 #f)
                                       :shared-reach-session #f
                                       :bad-sessions (make-vector 4 #f)
                                       :induction-sessions (make-vector 4 #f)
                                       :pdkind-induction-session #f
                                       :pdkind-induction-depth 0
                                       :minimization-session #f
                                       :decl-name->term (make-hashtable)
                                       :helper-name->term (make-hashtable)
                                       :registered-sorts (make-hashtable)
                                       :expr-term-cache (make-eq-hash-table)
                                       :expr-text-term-cache (make-hashtable)
                                       :synced-decls (make-eq-hash-table)
                                       :yices-owned? #t
                                       :reach-lemmas (make-vector 4 #f)
                                       :induction-lemmas '()
                                       :num-levels 2
                                       :effective-command effective-command
                                       :pdkind? pdkind?)))
           (when (>= (verbosity-level) 5)
             (for-each
              (lambda (decl)
                (let ((step0 (lookup-step-decl ctx decl 0))
                      (step1 (lookup-step-decl ctx decl 1)))
                  (verbose-message 5
                                   "sal-cdr: state var ~a => step0 ~a, step1 ~a"
                                   (sal-decl/name decl)
                                   (and step0 (smtlib2-translation-info/var-id info step0))
                                   (and step1 (smtlib2-translation-info/var-id info step1)))))
              state-vars))
           (when (>= (verbosity-level) 6)
             (verbose-message 6 "sal-cdr: I0 SMT:")
             (verbose-message 6 "~a" (render-smt2-assert i0 info))
             (verbose-message 6 "sal-cdr: T01 SMT:")
             (verbose-message 6 "~a" (render-smt2-assert t01 info))
             (verbose-message 6 "sal-cdr: Bad0 SMT:")
             (verbose-message 6 "~a" (render-smt2-assert bad0 info))
             (verbose-message 6 "sal-cdr: Bad1 SMT:")
             (verbose-message 6 "~a" (render-smt2-assert bad1 info)))
           (verbose-message 1 "sal-cdr: starting the initial solver session...")
           (set-slot-value! solver
                            :init-session
                            (make-cdr-yices2-session
                             solver
                             "init"
                             effective-command
                             (sat-smtlib2-context/logic ctx)
                             interpolants-supported?
                             '()
                             '()
                             (make-session-base-terms solver (list i0))
                             (make-id->decl-proc info)
                             (make-sort->type-proc info)
                             flat-module))
           ;; Native Yices2 shared reachability is still being stabilized; use
           ;; per-level reachability sessions for correctness.
           (create-level-session! solver 0)
           (create-level-session! solver 1)
           (set! constructed? #t)
           solver)
         (unless constructed?
           (yices2-api/release!)))))))

(define (query-asserted-model solver session expr step)
  (let ((assert-term (solver/expr->term solver expr)))
    (verbose-message 4 "sal-cdr: querying session ~a at step ~a..." (slot-value session :name) step)
    (session/push-assert! session assert-term))
  (verbose-message 5 "sal-cdr: session ~a pushed query, running check-sat..." (slot-value session :name))
  (let ((status (session/check-sat! session)))
    (verbose-message 5 "sal-cdr: session ~a check-sat returned ~a." (slot-value session :name) status)
    (unwind-protect
     (cond
      ((equal? status "sat")
       (values "sat" (session/get-model-cube solver session step)))
      (else
       (values status #f)))
     (session/pop! session))))

(define (query-state-formula-model solver session expr query-step model-step)
  (session/push-assert! session (solver/state-expr->term solver expr query-step))
  (let ((status (session/check-sat! session)))
    (unwind-protect
     (cond
      ((equal? status "sat")
       (values "sat" (session/get-model-cube solver session model-step)))
      (else
       (values status #f)))
     (session/pop! session))))

(define (query-state-formula-status solver session expr query-step)
  (session/push-assert! session (solver/state-expr->term solver expr query-step))
  (let ((status (session/check-sat! session)))
    (unwind-protect
     status
     (session/pop! session))))

(define (session/query-assertion-unsat? session assert-cmd)
  (equal? (session/query-with-assertion session assert-cmd) "unsat"))

(define (make-minimization-session solver)
  (let* ((ctx (slot-value solver :ctx))
         (info (slot-value solver :info)))
    (make-cdr-yices2-session
     solver
     "minimize"
     (slot-value solver :effective-command)
     (sat-smtlib2-context/logic ctx)
     #f
     '()
     '()
     '()
     (make-id->decl-proc info)
     (make-sort->type-proc info)
     (slot-value solver :flat-module))))

(define (get-minimization-session solver)
  (or (slot-value solver :minimization-session)
      (let ((session (make-minimization-session solver)))
        (set-slot-value! solver :minimization-session session)
        session)))

(define (call/temporary-minimization-session solver proc)
  (let ((session (make-minimization-session solver)))
    (unwind-protect
     (proc session)
     (session/close! session))))

(define (state-formulas-imply/with-session? solver session antecedents consequent)
  (let ((assertions
         (append
          (map (lambda (expr)
                 (solver/state-expr->parsed-term solver expr 0))
               antecedents)
          (list (yices2-api/not-term
                 (solver/state-expr->parsed-term solver consequent 0))))))
    (when (>= (verbosity-level) 5)
      (verbose-message 5
                       "sal-cdr: checking state implication with ~a antecedent(s): ~a => ~a"
                       (length antecedents)
                       (map sal-expr->string antecedents)
                       (sal-expr->string consequent)))
    (let ((ctx (yices2-api/new-context #f)))
      (unwind-protect
       (begin
         (for-each (lambda (term)
                     (yices2-api/assert-formula! ctx term))
                   assertions)
         (let ((label (yices2-api/status->string (yices2-api/check-context ctx))))
           (when (>= (verbosity-level) 5)
             (verbose-message 5 "sal-cdr: implication check returned ~a." label))
           (cond
            ((equal? label "unsat")
             #t)
            ((or (equal? label "sat")
                 (equal? label "unknown"))
             #f)
            (else
             (sign-error "sal-cdr expected sat/unsat/unknown from a minimization implication check, received ~a."
                         label)))))
       (yices2-api/free-context! ctx)))))

(define (state-formulas-imply? solver antecedents consequent)
  (state-formulas-imply/with-session? solver
                                      #f
                                      antecedents
                                      consequent))

(define (minimize-state-formula/greedy solver expr)
  (let* ((simplified (sal-ast/simplify expr))
         (conjuncts (sal-expr/conjuncts simplified)))
    (cond
     ((or (sal-expr/true? simplified)
          (sal-expr/false? simplified)
          (null? conjuncts)
          (null? (cdr conjuncts)))
      simplified)
     (else
      (let loop ((remaining conjuncts)
                 (kept conjuncts))
        (if (null? remaining)
          (sal-expr/from-conjuncts kept (slot-value solver :flat-module))
          (let* ((candidate (car remaining))
                 (reduced (remove-if (lambda (expr)
                                       (sal-ast/equivalent? expr candidate))
                                     kept)))
            (if (state-formulas-imply? solver reduced simplified)
              (loop (cdr remaining) reduced)
              (loop (cdr remaining) kept))))))))))

(define (minimize-forward-lemma/greedy solver level lemma)
  (let* ((simplified (sal-ast/simplify lemma))
         (disjuncts (sal-expr/disjuncts simplified)))
    (cond
     ((or (sal-expr/true? simplified)
          (sal-expr/false? simplified)
          (null? disjuncts)
          (null? (cdr disjuncts)))
      simplified)
     (else
      (let loop ((remaining disjuncts)
                 (kept disjuncts))
        (if (null? remaining)
          (sal-expr/from-disjuncts kept (slot-value solver :flat-module))
          (let* ((candidate (car remaining))
                 (reduced (remove-if (lambda (expr)
                                       (sal-ast/equivalent? expr candidate))
                                     kept))
                 (reduced-lemma (sal-expr/from-disjuncts reduced
                                                         (slot-value solver :flat-module))))
            (if (forward-lemma-valid? solver level reduced-lemma)
              (loop (cdr remaining) reduced)
              (loop (cdr remaining) kept)))))))))

(define (push-shared-reach-level! solver level)
  (let ((session (slot-value solver :shared-reach-session)))
    (unless session
      (sign-error "sal-cdr internal error: missing shared reachability session."))
    (session/push-asserts! session (shared-reach-level-terms solver level))))

(define (reach-session-query-cube-status solver level cube step)
  (if (slot-value solver :shared-reach-session)
    (let ((session (slot-value solver :shared-reach-session)))
      (push-shared-reach-level! solver level)
      (unwind-protect
       (session/query-cube-status solver session cube step)
       (session/pop! session)))
    (session/query-cube-status solver
                               (vector-ref (slot-value solver :reach-sessions) level)
                               cube
                               step)))

(define (reach-session-query-cube-model solver level cube query-step model-step)
  (if (slot-value solver :shared-reach-session)
    (let ((session (slot-value solver :shared-reach-session)))
      (push-shared-reach-level! solver level)
      (unwind-protect
       (session/query-cube-model solver session cube query-step model-step)
       (session/pop! session)))
    (session/query-cube-model solver
                              (vector-ref (slot-value solver :reach-sessions) level)
                              cube
                              query-step
                              model-step)))

(define (reach-session-query-assertion-unsat? solver level assert-cmd)
  (if (slot-value solver :shared-reach-session)
    (let ((session (slot-value solver :shared-reach-session)))
      (push-shared-reach-level! solver level)
      (unwind-protect
       (session/query-assertion-unsat? session assert-cmd)
       (session/pop! session)))
    (session/query-assertion-unsat? (vector-ref (slot-value solver :reach-sessions) level)
                                    assert-cmd)))

(define (forward-lemma-valid? solver level lemma)
  (and lemma
       (let* ((init-session (slot-value solver :init-session))
              (negated-lemma (make-sal-not lemma))
              (init-assert-term (solver/state-expr->term solver
                                                         negated-lemma
                                                         0))
              (transition-assert-term (and (> level 0)
                                           (solver/state-expr->term solver
                                                                    negated-lemma
                                                                    1)))
              (init-ok? (session/query-assertion-unsat? init-session
                                                        init-assert-term))
              (transition-ok?
               (if (> level 0)
                 (reach-session-query-assertion-unsat? solver
                                                      (- level 1)
                                                      transition-assert-term)
                 #t)))
         (when (and (>= (verbosity-level) 4)
                    (or (not init-ok?) (not transition-ok?)))
           (verbose-message 4 "sal-cdr: rejected forward lemma at F~a because it failed semantic validation." level)
           (verbose-message 4 "  lemma: ~a"
                            (with-output-to-string
                              (lambda ()
                               (sal/pp lemma))))
           (when (>= (verbosity-level) 5)
             (verbose-message 5 "  init assertion: ~a"
                              (render-state-assert solver negated-lemma 0))
             (when transition-assert-term
               (verbose-message 5 "  transition assertion: ~a"
                                (render-state-assert solver negated-lemma 1))))
           (verbose-message 4 "  initial-valid? ~a, transition-valid? ~a"
                            init-ok?
                            transition-ok?))
         (and init-ok? transition-ok?))))

(define (state-formula-at-step solver expr step)
  (sal-ast->sat expr
                (slot-value solver :ctx)
                *cdr-empty-env*
                step
                *pos*))

(define (solver-step-state-decls solver step)
  (map-and-filter
   (lambda (decl)
     (lookup-step-decl (slot-value solver :ctx) decl step))
   (cdr-solver/state-vars solver)))

(define (reachability-generalization-formulas solver level target-expr)
  (append
   (if (= level 0)
     (list (slot-value solver :i0))
     '())
   (list (slot-value solver :t01))
   (map (lambda (lemma)
          (state-formula-at-step solver lemma 0))
        (or (vector-ref (slot-value solver :reach-lemmas) level)
            '()))
   (list (state-formula-at-step solver target-expr 1))))

(define (transition-step-formula solver step)
  (translate-simplified-skeleton!
   (slot-value solver :ctx)
   (slot-value solver :flat-module)
   (list (cons step (slot-value solver :valid-input))
         (cons step (slot-value solver :valid-state))
         (cons (+ step 1) (slot-value solver :valid-state))
         (cons step (slot-value solver :definition))
         (cons (+ step 1) (slot-value solver :definition))
         (cons step (slot-value solver :transition)))))

(define (induction-generalization-formulas solver depth target-expr)
  (append
   (let loop ((step 0)
              (result '()))
     (if (>= step depth)
       (reverse! result)
       (loop (+ step 1)
             (cons (transition-step-formula solver step) result))))
   (apply append
          (map (lambda (lemma)
                 (let loop ((step 0)
                            (result '()))
                   (if (>= step depth)
                     (reverse! result)
                     (loop (+ step 1)
                           (cons (state-formula-at-step solver lemma step) result)))))
               (slot-value solver :induction-lemmas)))
   (list (state-formula-at-step solver target-expr depth))))

(define-method (cdr-solver/add-frame! (solver <cdr-yices2-solver>))
  (let ((level (slot-value solver :num-levels)))
    (when (>= level (vector-length (slot-value solver :reach-sessions)))
      (let* ((old-reach (slot-value solver :reach-sessions))
             (old-lemmas (slot-value solver :reach-lemmas))
             (old-bad (slot-value solver :bad-sessions))
             (old-induction (slot-value solver :induction-sessions))
             (new-size (* 2 (vector-length old-reach)))
             (new-reach (make-vector new-size #f))
             (new-lemmas (make-vector new-size #f))
             (new-bad (make-vector new-size #f))
             (new-induction (make-vector new-size #f)))
        (let loop ((i 0))
          (when (< i (vector-length old-reach))
            (vector-set! new-reach i (vector-ref old-reach i))
            (vector-set! new-lemmas i (vector-ref old-lemmas i))
            (vector-set! new-bad i (vector-ref old-bad i))
            (vector-set! new-induction i (vector-ref old-induction i))
            (loop (+ i 1))))
        (set-slot-value! solver :reach-sessions new-reach)
        (set-slot-value! solver :reach-lemmas new-lemmas)
        (set-slot-value! solver :bad-sessions new-bad)
        (set-slot-value! solver :induction-sessions new-induction)))
    (when (>= (verbosity-level) 5)
      (verbose-message 5 "sal-cdr: creating backend frame F~a." level))
    (create-level-session! solver level)
    (set-slot-value! solver :num-levels (+ level 1))
    (when (>= (verbosity-level) 5)
      (verbose-message 5 "sal-cdr: backend frame F~a is ready." level))
    level))

(define-method (cdr-solver/add-lemma-at-level! (solver <cdr-yices2-solver>) (level <primitive>) (lemma <sal-expr>))
  (when (> level 0)
    (let ((assert-term (solver/state-expr->term solver lemma 0))
          (bad-assert-term (solver/state-expr->term solver lemma 1)))
      (if (slot-value solver :shared-reach-session)
        (begin
          (ensure-shared-reach-selector! solver level)
          (session/assert-permanent!
           (slot-value solver :shared-reach-session)
           (yices2-api/implies-term
            (or (solver/lookup-yices-term solver (shared-reach-selector-id level))
                (sign-error "sal-cdr internal error: missing shared reachability selector F~a." level))
            assert-term)))
        (session/assert-permanent! (vector-ref (slot-value solver :reach-sessions) level) assert-term))
      (vector-set! (slot-value solver :reach-lemmas)
                   level
                   (cons lemma
                         (or (vector-ref (slot-value solver :reach-lemmas) level)
                             '())))
      (session/assert-permanent! (vector-ref (slot-value solver :bad-sessions) level) bad-assert-term)
      (session/assert-permanent! (vector-ref (slot-value solver :induction-sessions) level) assert-term))))

(define-method (cdr-solver/add-lemma! (solver <cdr-yices2-solver>) (max-level <primitive>) (lemma <sal-expr>))
  (let loop ((i 1))
    (when (<= i max-level)
      (cdr-solver/add-lemma-at-level! solver i lemma)
      (loop (+ i 1)))))

(define-method (cdr-solver/check-initial-bad (solver <cdr-yices2-solver>))
  (query-asserted-model solver
                        (slot-value solver :init-session)
                        (slot-value solver :bad0)
                        0))

(define-method (cdr-solver/check-initial-cube (solver <cdr-yices2-solver>) (cube <cdr-cube>))
  (session/query-cube-status solver
                             (slot-value solver :init-session)
                             cube
                             0))

(define-method (cdr-solver/check-initial-expr (solver <cdr-yices2-solver>) (expr <sal-expr>))
  (query-state-formula-status solver
                              (slot-value solver :init-session)
                              expr
                              0))

(define-method (cdr-solver/check-bad-at-frame (solver <cdr-yices2-solver>) (level <primitive>))
  (query-asserted-model solver
                        (vector-ref (slot-value solver :bad-sessions) level)
                        (slot-value solver :bad1)
                        1))

(define-method (cdr-solver/check-predecessor (solver <cdr-yices2-solver>) (level <primitive>) (cube <cdr-cube>))
  (reach-session-query-cube-model solver level cube 1 0))

(define-method (cdr-solver/check-predecessor-expr (solver <cdr-yices2-solver>) (level <primitive>) (expr <sal-expr>))
  (if (slot-value solver :shared-reach-session)
    (let ((session (slot-value solver :shared-reach-session)))
      (push-shared-reach-level! solver level)
      (unwind-protect
       (query-state-formula-model solver session expr 1 0)
       (session/pop! session)))
    (query-state-formula-model solver
                               (vector-ref (slot-value solver :reach-sessions) level)
                               expr
                               1
                               0)))

(define-method (cdr-solver/check-predecessor-status (solver <cdr-yices2-solver>) (level <primitive>) (cube <cdr-cube>))
  (reach-session-query-cube-status solver level cube 1))

(define-method (cdr-solver/check-lemma-propagation (solver <cdr-yices2-solver>) (level <primitive>) (lemma <sal-expr>))
  (let* ((session (vector-ref (slot-value solver :induction-sessions) level))
         (negated-next-lemma (make-sal-not (sal-ast->sat lemma
                                                         (slot-value solver :ctx)
                                                         *cdr-empty-env*
                                                         1
                                                         *pos*))))
    (equal? (session/query-with-assertion session
                                          (solver/expr->term solver negated-next-lemma))
            "unsat")))

(define-method (cdr-solver/interpolant-for-cube (solver <cdr-yices2-solver>) (level <primitive>) (cube <cdr-cube>))
  (if (not (cdr-solver-capabilities/interpolants?
            (cdr-solver/capabilities solver)))
    #f
    (let ((session (if (slot-value solver :shared-reach-session)
                     (slot-value solver :shared-reach-session)
                     (vector-ref (slot-value solver :reach-sessions) level)))
          (model #f))
      (when (slot-value solver :shared-reach-session)
        (push-shared-reach-level! solver level))
      (unwind-protect
       (begin
         (multiple-value-bind
             (terms local-model)
             (cube->yices-model solver cube 1)
           (set! model local-model)
           (let ((status (session/check-sat-with-model! session model terms)))
             (when (>= (verbosity-level) 5)
             (verbose-message 5 "sal-cdr: interpolant query status at F~a: ~a"
                                level
                                status))
             (if (equal? status "unsat")
               (session/get-interpolant-expr solver session model)
               #f))))
       (when model
         (yices2-api/free-model! model))
       (when (slot-value solver :shared-reach-session)
         (session/pop! session))))))

(define-method (cdr-solver/learn-forward-cube (solver <cdr-yices2-solver>) (level <primitive>) (cube <cdr-cube>))
  (if (not (cdr-solver-capabilities/interpolants?
            (cdr-solver/capabilities solver)))
    #f
    (let* ((init-session (slot-value solver :init-session))
           (initial-lemma
            (let ((model #f))
              (unwind-protect
               (begin
                 (when (>= (verbosity-level) 5)
                   (verbose-message 5 "sal-cdr: forward learning, checking initial model assumption..."))
                 (multiple-value-bind
                     (terms local-model)
                     (cube->yices-model solver cube 0)
                   (set! model local-model)
                   (when (>= (verbosity-level) 5)
                     (verbose-message 5 "sal-cdr: forward learning initial assumption terms: ~a"
                                      (length terms)))
                   (and (equal? (session/check-sat-with-model! init-session model terms) "unsat")
                        (session/get-interpolant-expr solver init-session model))))
               (when model
                 (yices2-api/free-model! model)))))
           (transition-lemma
            (and (> level 0)
                 (let ((session (if (slot-value solver :shared-reach-session)
                                  (slot-value solver :shared-reach-session)
                                  (vector-ref (slot-value solver :reach-sessions) (- level 1))))
                       (model #f))
                   (when (slot-value solver :shared-reach-session)
                     (push-shared-reach-level! solver (- level 1)))
                   (unwind-protect
                    (begin
                      (when (>= (verbosity-level) 5)
                        (verbose-message 5 "sal-cdr: forward learning, checking transition model assumption at F~a..." (- level 1)))
                      (multiple-value-bind
                          (terms local-model)
                          (cube->yices-model solver cube 1)
                        (set! model local-model)
                        (when (>= (verbosity-level) 5)
                          (verbose-message 5 "sal-cdr: forward learning transition assumption terms: ~a"
                                           (length terms)))
                        (if (equal? (session/check-sat-with-model! session model terms) "unsat")
                          (session/get-interpolant-expr solver session model)
                          #f)))
                    (when model
                      (yices2-api/free-model! model))
                    (when (slot-value solver :shared-reach-session)
                      (session/pop! session)))))))
      (let ((lemma
             (cond
              ((and initial-lemma transition-lemma)
               (make-sal-or* (list initial-lemma transition-lemma)
                             (slot-value solver :flat-module)))
              (initial-lemma
               initial-lemma)
              (transition-lemma
               transition-lemma)
              (else
               #f))))
        (let* ((minimized (and lemma
                               (minimize-forward-lemma/greedy solver level lemma)))
               (simplified (and minimized (sal-ast/simplify minimized))))
          (cond
           ((and simplified (forward-lemma-valid? solver level simplified))
            simplified)
           ((and minimized (forward-lemma-valid? solver level minimized))
            minimized)
           ((and lemma (forward-lemma-valid? solver level lemma))
            lemma)
           (else
            #f)))))))

(define-method (cdr-solver/reset-induction! (solver <cdr-yices2-solver>) (depth <primitive>))
  (when (>= (verbosity-level) 5)
    (verbose-message 5 "sal-cdr: rebuilding PDKIND induction session for depth ~a." depth))
  (when (slot-value solver :pdkind-induction-session)
    (session/close! (slot-value solver :pdkind-induction-session)))
  (set-slot-value! solver :pdkind-induction-depth depth)
  (set-slot-value! solver :induction-lemmas '())
  (set-slot-value! solver
                   :pdkind-induction-session
                   (make-pdkind-induction-session solver depth))
  (when (>= (verbosity-level) 5)
    (verbose-message 5 "sal-cdr: PDKIND induction session for depth ~a is ready." depth))
  depth)

(define (assert-induction-lemma-at-step! solver session lemma step)
  (session/assert-permanent! session (solver/state-expr->term solver lemma step)))

(define-method (cdr-solver/add-induction-lemma! (solver <cdr-yices2-solver>) (lemma <sal-expr>))
  (let ((session (slot-value solver :pdkind-induction-session))
        (depth (slot-value solver :pdkind-induction-depth)))
    (unless session
      (sign-error "sal-cdr internal error: missing PDKIND induction session."))
    (set-slot-value! solver :induction-lemmas
                     (cons lemma (slot-value solver :induction-lemmas)))
    (assert-induction-lemma-at-step! solver session lemma 0)
    (let loop ((step 1))
      (when (< step depth)
        (assert-induction-lemma-at-step! solver session lemma step)
        (loop (+ step 1))))))

(define-method (cdr-solver/check-inductive-witness (solver <cdr-yices2-solver>) (lemma <sal-expr>))
  (let* ((session (slot-value solver :pdkind-induction-session))
         (depth (slot-value solver :pdkind-induction-depth))
         (negated-final (solver/state-expr->term solver (make-sal-not lemma) depth)))
    (unless session
      (sign-error "sal-cdr internal error: missing PDKIND induction session."))
    (session/push-assert! session negated-final)
    (let ((status (session/check-sat! session)))
      (unwind-protect
       (cond
        ((equal? status "sat")
         (values "sat"
                 (session/get-model-cube solver session 0)
                 (session/get-model-cube solver session depth)))
        (else
         (values status #f #f)))
       (session/pop! session)))))

(define-method (cdr-solver/generalize-reachability-target (solver <cdr-yices2-solver>) (level <primitive>) (target-expr <sal-expr>))
  (cdr-solver/minimize-state-formula
   solver
   (helper-generalize-formulas solver
                               (solver-step-state-decls solver 0)
                               (reachability-generalization-formulas solver
                                                                     level
                                                                     target-expr))))

(define-method (cdr-solver/generalize-induction-target (solver <cdr-yices2-solver>) (target-expr <sal-expr>))
  (cdr-solver/minimize-state-formula
   solver
   (helper-generalize-formulas solver
                               (solver-step-state-decls solver 0)
                               (induction-generalization-formulas solver
                                                                  (slot-value solver :pdkind-induction-depth)
                                                                  target-expr))))

(define-method (cdr-solver/check-induction-path (solver <cdr-yices2-solver>) (cube <cdr-cube>) (target-expr <sal-expr>))
  (let* ((session (slot-value solver :pdkind-induction-session))
         (depth (slot-value solver :pdkind-induction-depth))
         (cube-assert (cube-assert-term solver cube 0))
         (target-assert (solver/state-expr->term solver target-expr depth)))
    (unless session
      (sign-error "sal-cdr internal error: missing PDKIND induction session."))
    (session/query-with-assertions session (list cube-assert target-assert))))

(define-method (cdr-solver/usable-lemma (solver <cdr-yices2-solver>) (lemma <sal-expr>))
  (try
   (begin
     (render-state-assert solver lemma 0)
     (render-state-assert solver lemma 1)
     lemma)
   (lambda (_escape _proc _msg _obj)
     #f)))

(define-method (cdr-solver/minimize-state-formula (solver <cdr-yices2-solver>) (expr <sal-expr>))
  (minimize-state-formula/greedy solver expr))

(define-method (cdr-solver/state-formulas-imply? (solver <cdr-yices2-solver>) (antecedents <primitive>) (consequent <sal-expr>))
  (state-formulas-imply? solver antecedents consequent))

(define-method (cdr-solver/close! (solver <cdr-yices2-solver>))
  (session/close! (slot-value solver :init-session))
  (session/close! (slot-value solver :shared-reach-session))
  (session/close! (slot-value solver :pdkind-induction-session))
  (session/close! (slot-value solver :minimization-session))
  (let loop ((i 0))
    (when (< i (slot-value solver :num-levels))
      (session/close! (vector-ref (slot-value solver :reach-sessions) i))
      (session/close! (vector-ref (slot-value solver :bad-sessions) i))
      (session/close! (vector-ref (slot-value solver :induction-sessions) i))
      (loop (+ i 1))))
  (when (slot-value solver :yices-owned?)
    (set-slot-value! solver :yices-owned? #f)
    (yices2-api/release!)))
