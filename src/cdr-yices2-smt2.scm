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
   :active-assumption-model
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
;; Constructor ids come from the linked Yices2 build rather than hard-coded
;; enum values so the native walker stays aligned with the actual library.
(define *yices-constructor-variable*
  (yices2-api/constructor-variable))
(define *yices-constructor-uninterpreted-term*
  (yices2-api/constructor-uninterpreted-term))
(define *yices-constructor-select-term*
  (yices2-api/constructor-select-term))
(define *yices-constructor-bit-term*
  (yices2-api/constructor-bit-term))
(define *yices-constructor-bv-sum*
  (yices2-api/constructor-bv-sum))
(define *yices-constructor-arith-sum*
  (yices2-api/constructor-arith-sum))
(define *yices-constructor-arith-ff-sum*
  (yices2-api/constructor-arith-ff-sum))
(define *yices-constructor-power-product*
  (yices2-api/constructor-power-product))

(define (read-nonnegative-env-int name default)
  (let ((value (getenv name)))
    (if (not value)
      default
      (let ((parsed (string->integer value)))
        (if (and parsed (>= parsed 0))
          parsed
          default)))))

(define *cdr-native-term-cache-limit*
  (read-nonnegative-env-int "SAL_CDR_TERM_CACHE_LIMIT" 4096))

(define *cdr-native-term-cache-max-text-length*
  (read-nonnegative-env-int "SAL_CDR_TERM_CACHE_MAX_TEXT" 4096))

(define *cdr-native-max-rss-mb*
  (read-nonnegative-env-int "SAL_CDR_MAX_RSS_MB" 4096))

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

(define (collect-formula-reference-decls formulas keep-decls)
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
                 (sal-ast/open-reference-list expr)))
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

(define (parsed-term-cache-key text)
  (string-append "[yices-term]" text))

(define (solver/clear-expr-term-caches! solver)
  (set-slot-value! solver :expr-term-cache (make-eq-hash-table))
  (set-slot-value! solver :expr-text-term-cache (make-hashtable)))

(define (solver/cacheable-expr-text? expr-text)
  (or (= *cdr-native-term-cache-max-text-length* 0)
      (<= (string-length expr-text)
          *cdr-native-term-cache-max-text-length*)))

(define (native-term-summary term)
  (cond
   ((not (integer? term))
    (format #f "~a (non-integer)" term))
   ((<= term 0)
    (format #f "~a (invalid term id)" term))
   (else
    (let ((name (yices2-api/term-name term)))
      (if name
        (format #f "~a (~a)" term name)
        (format #f "~a (ctor ~a)" term (yices2-api/term-constructor term)))))))

(define (solver/require-native-term! term where expr-text)
  (unless (and (integer? term) (> term 0))
    (sign-error "sal-cdr produced an invalid native Yices term while ~a.\nTerm: ~a\nExpr: ~a"
                where
                term
                expr-text))
  term)

(define (solver/cache-expr-term! solver expr expr-text term)
  (solver/require-native-term! term "caching a translated expression" expr-text)
  (if (and expr-text
           (solver/cacheable-expr-text? expr-text))
    (begin
      (when (and (> *cdr-native-term-cache-limit* 0)
                 (>= (eq-hash-table/size (slot-value solver :expr-term-cache))
                     *cdr-native-term-cache-limit*))
        (when (>= (verbosity-level) 4)
          (verbose-message 4
                           "sal-cdr: clearing native expression-term caches after ~a entries to bound memory usage."
                           (eq-hash-table/size (slot-value solver :expr-term-cache))))
        (solver/clear-expr-term-caches! solver))
      (eq-hash-table/put! (slot-value solver :expr-term-cache) expr term)
      (hashtable-put! (slot-value solver :expr-text-term-cache) expr-text term)
      term)
    term))

(define (solver/release-transient-expr-term-caches! solver)
  (when (> (eq-hash-table/size (slot-value solver :expr-term-cache)) 0)
    (when (>= (verbosity-level) 5)
      (verbose-message 5
                       "sal-cdr: releasing ~a cached native expression term(s)."
                       (eq-hash-table/size (slot-value solver :expr-term-cache))))
    (solver/clear-expr-term-caches! solver)))

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

(define (sanitize-generalized-native-terms terms)
  (let loop ((remaining terms)
             (idx 0)
             (result '())
             (dropped 0))
    (if (null? remaining)
      (begin
        (when (and (> dropped 0)
                   (>= (verbosity-level) 4))
          (verbose-message 4
                           "sal-cdr: dropped ~a invalid generalized native term(s)."
                           dropped))
        (reverse! result))
      (let ((term (car remaining)))
        (if (and (integer? term) (> term 0))
          (loop (cdr remaining)
                (+ idx 1)
                (cons term result)
                dropped)
          (begin
            (when (>= (verbosity-level) 4)
              (verbose-message 4
                               "sal-cdr: dropping invalid generalized native term[~a]: ~a"
                               idx
                               term))
            (loop (cdr remaining)
                  (+ idx 1)
                  result
                  (+ dropped 1))))))))

(define (validate-native-term-list who terms)
  (let loop ((remaining terms)
             (idx 0))
    (unless (null? remaining)
      (let ((term (car remaining)))
        (unless (and (integer? term) (> term 0))
          (sign-error "sal-cdr internal error: invalid native term in ~a at index ~a: ~a"
                      who
                      idx
                      term))
        (loop (cdr remaining) (+ idx 1))))))

(define (helper-generalize-terms solver keep-decls formulas)
  (let* ((decls (collect-formula-decls formulas keep-decls))
         (reference-decls (collect-formula-reference-decls formulas keep-decls))
         (_ (when (>= (verbosity-level) 5)
              (verbose-message 5
                               "sal-cdr: helper generalization on ~a formula(s), ~a decl(s), ~a kept decl(s)."
                               (length formulas)
                               (length decls)
                               (length keep-decls))))
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
          (let* ((formula-terms
                  (map (lambda (expr)
                         (solver/expr->term/presynced
                          solver
                          (if (sal-expr/true? expr)
                            (make-sal-true (slot-value solver :flat-module))
                            expr)))
                       formulas))
                 (elim-terms
                  (map-and-filter
                   (lambda (decl)
                     (and (not (member decl keep-decls))
                          (solver/lookup-yices-term
                           solver
                           (smt-id->string
                            (smtlib2-translation-info/var-id (slot-value solver :info) decl)))))
                   reference-decls))
                 (_ (when (>= (verbosity-level) 5)
                      (verbose-message 5
                                       "sal-cdr: helper generalization invoking Yices on ~a conjunct term(s), eliminating ~a variable term(s)."
                                       (length formula-terms)
                                       (length elim-terms))))
                 (terms (yices2-api/generalize-terms formula-terms elim-terms)))
            (when (>= (verbosity-level) 5)
              (verbose-message 5
                               "sal-cdr: helper generalization received ~a generalized term(s)."
                               (length terms)))
            (sanitize-generalized-native-terms terms))))
    generalized-terms))
    
(define (native-conjunct-terms->term terms)
  (cond
   ((null? terms)
    (yices2-api/true-term))
   ((null? (cdr terms))
    (car terms))
   (else
    (yices2-api/and-terms terms))))

(define (remove-equivalent-native-conjunct-terms terms candidate)
  (remove-if (lambda (term)
               (= term candidate))
             terms))

(define (minimize-native-conjunct-terms/greedy solver terms)
  (let* ((normalized-terms
          (if (null? terms)
            (list (yices2-api/true-term))
            terms)))
    (if (or (null? normalized-terms)
            (null? (cdr normalized-terms)))
      normalized-terms
      (let ((negated-whole-term
             (yices2-api/not-term
              (native-conjunct-terms->term normalized-terms))))
        (let loop ((remaining normalized-terms)
                   (kept normalized-terms))
          (if (null? remaining)
            kept
            (let* ((candidate-term (car remaining))
                   (reduced (remove-equivalent-native-conjunct-terms kept
                                                                     candidate-term))
                   (implied? (query-minimization-session-imply? solver
                                                                reduced
                                                                negated-whole-term)))
              (if implied?
                (loop (cdr remaining) reduced)
                (loop (cdr remaining) kept)))))))))

(define (generalized-terms->state-formula solver generalized-terms)
  (let* ((id->decl-proc (make-id->decl-proc (slot-value solver :info)))
         (sanitized-terms (sanitize-generalized-native-terms generalized-terms))
         (exprs
          (map-with-pos
           (lambda (term idx)
             (let* ((raw-term-string (yices2-api/term->string term))
                    (normalized-term-string (normalize-yices-inline raw-term-string)))
               (when (>= (verbosity-level) 6)
                 (verbose-message 6
                                  "sal-cdr: generalized term[~a] constructor ~a\n  raw: ~a\n  normalized: ~a"
                                  idx
                                  (yices2-api/term-constructor term)
                                  raw-term-string
                                  normalized-term-string))
               (try
                (multiple-value-bind
                    (state-constraint _max-step)
                    (build-state-constraint
                     (yices2/string->sal-expr normalized-term-string
                                              id->decl-proc
                                              (slot-value solver :flat-module))
                     (slot-value (slot-value solver :ctx) :inv-step-decls)
                     (slot-value (slot-value solver :ctx) :inv-global-decls))
                  (try
                   (begin
                     (sal-ast/simplify state-constraint)
                     state-constraint)
                   (lambda (_escape2 _proc2 simplify-msg _obj2)
                     (sign-error
                      "sal-cdr reconstructed a malformed generalized state formula.\nConstructor: ~a\nRaw term: ~a\nNormalized term: ~a\nState formula: ~a\nReason: ~a"
                      (yices2-api/term-constructor term)
                      raw-term-string
                      normalized-term-string
                      state-constraint
                      simplify-msg))))
                (lambda (_escape _proc msg _obj)
                  (sign-error
                   "sal-cdr failed to convert a generalized native Yices term into a state formula.\nConstructor: ~a\nRaw term: ~a\nNormalized term: ~a\nReason: ~a"
                   (yices2-api/term-constructor term)
                   raw-term-string
                   normalized-term-string
                   msg)))))
           sanitized-terms)))
      (sal-expr/from-conjuncts (if (null? exprs)
                                (list (make-sal-true (slot-value solver :flat-module)))
                               exprs)
                              (slot-value solver :flat-module))))

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
  (let ((info (slot-value solver :info)))
    (define (cache-term! expr expr-text term)
      (solver/cache-expr-term! solver expr expr-text term))
    (define (parse-fallback expr expr-text failure-msg)
      (when (>= (verbosity-level) 4)
        (verbose-message 4
                         "sal-cdr: native Yices construction failed for a subexpression; retrying via yices_parse_term.\nExpr: ~a\nReason: ~a"
                         expr-text
                         failure-msg))
      (try
       (let ((term (solver/expr->parsed-term solver expr)))
         (cache-term! expr expr-text term))
       (lambda (_escape _proc parse-msg _obj)
         (sign-error "sal-cdr failed to construct a native Yices term for subexpression:\n~a\nReason: ~a\nFallback parse failure: ~a"
                     expr-text
                     failure-msg
                     parse-msg))))
    (define (build expr)
      (cond
       ((eq-hash-table/get (slot-value solver :expr-term-cache) expr) => cdr)
       (else
        (let ((expr-text (render-smt2-expr expr info)))
          (cond
           ((hashtable-get (slot-value solver :expr-text-term-cache) expr-text)
           =>
            (lambda (term)
              (eq-hash-table/put! (slot-value solver :expr-term-cache) expr term)
              term))
           (else
            (try
             (let ((term (solver/expr->term/core solver expr build lookup-term)))
               (cache-term! expr expr-text term))
             (lambda (_escape _proc msg _obj)
               (parse-fallback expr expr-text msg)))))))))
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

(define *native-yices-bool-node-arity* 8)

(define (partition-list lst chunk-size)
  (let loop ((remaining lst)
             (current '())
             (current-size 0)
             (chunks '()))
    (cond
     ((null? remaining)
      (reverse!
       (if (null? current)
         chunks
         (cons (reverse! current) chunks))))
     ((= current-size chunk-size)
      (loop remaining
            '()
            0
            (cons (reverse! current) chunks)))
     (else
      (loop (cdr remaining)
            (cons (car remaining) current)
            (+ current-size 1)
            chunks)))))

(define (build-balanced-boolean-term terms combine)
  (cond
   ((null? terms)
    (combine terms))
   ((null? (cdr terms))
    (car terms))
   (else
    (let loop ((level terms))
      (if (<= (length level) *native-yices-bool-node-arity*)
        (combine level)
        (loop (map combine
                   (partition-list level
                                   *native-yices-bool-node-arity*))))))))

(define (solver/expr->term/core solver expr recur lookup-term)
    (define (build-boolean-term label args combine)
      (let ((built-args
             (map (lambda (arg)
                    (try
                     (recur arg)
                     (lambda (_escape _proc msg _obj)
                       (sign-error "sal-cdr failed to construct a native Yices term for ~a child:\n~a\nReason: ~a"
                                   label
                                   (render-smt2-expr arg (slot-value solver :info))
                                   msg))))
                  args)))
        (for-each
         (lambda (binding)
           (unless (yices2-api/term-bool? (cdr binding))
             (sign-error "sal-cdr expected a native boolean term for ~a child:\n~a\nNative term: ~a"
                         label
                         (render-smt2-expr (car binding) (slot-value solver :info))
                         (native-term-summary (cdr binding)))))
         (map cons args built-args))
        (try
         (build-balanced-boolean-term built-args combine)
         (lambda (_escape _proc msg _obj)
           (sign-error "sal-cdr failed to combine native Yices terms for ~a:\n~a\nReason: ~a"
                       label
                       (render-smt2-expr expr (slot-value solver :info))
                       msg)))))
    (cond
     ((instance-of? expr <sal-true>)
      (yices2-api/true-term))
     ((instance-of? expr <sal-false>)
      (yices2-api/false-term))
     ((instance-of? expr <sal-numeral>)
      (yices2-api/rational-term (mpq->string (slot-value expr :num))))
     ((instance-of? expr <sal-name-expr>)
      (let* ((decl (slot-value expr :decl))
             (id (solver/existing-decl-id solver decl))
             (name (and id (smt-id->string id)))
             (term (and name (lookup-term name))))
        (unless id
          (sign-error "sal-cdr failed to find a synced SMT identifier for SAL declaration ~a."
                      (sal-decl/name decl)))
        (unless term
          (sign-error "sal-cdr failed to map SAL declaration ~a to a native Yices term."
                      (sal-decl/name decl)))
        term))
     ((instance-of? expr <sal-eq>)
      (multiple-value-bind
          (arg1 arg2)
          (sal-binary-application/arguments expr)
        (if (sal-type/boolean? (sal-expr/type arg1))
          (yices2-api/iff-term (recur arg1)
                               (recur arg2))
          (yices2-api/eq-term (recur arg1)
                              (recur arg2)))))
     ((instance-of? expr <sal-diseq>)
      (multiple-value-bind
          (arg1 arg2)
          (sal-binary-application/arguments expr)
        (if (sal-type/boolean? (sal-expr/type arg1))
          (yices2-api/not-term
           (yices2-api/iff-term (recur arg1)
                                (recur arg2)))
          (yices2-api/neq-term (recur arg1)
                               (recur arg2)))))
     ((instance-of? expr <sal-and>)
      (build-boolean-term "and"
                          (sal-application/argument-list expr)
                          yices2-api/and-terms))
     ((instance-of? expr <sal-or>)
      (build-boolean-term "or"
                          (sal-application/argument-list expr)
                          yices2-api/or-terms))
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
      (let ((args (sal-application/argument-list expr)))
        (cond
         ((null? args)
          (sign-error "sal-cdr encountered a subtraction with no arguments."))
         ((null? (cdr args))
          (let* ((arg (car args))
                 (num (sal-numeral-value arg)))
            (if num
              (yices2-api/rational-term (mpq->string (-mpq *mpq-zero* num)))
              (yices2-api/neg-term (recur arg)))))
         ((null? (cddr args))
          (let* ((arg1 (car args))
                 (arg2 (cadr args))
                 (num1 (sal-numeral-value arg1))
                 (num2 (sal-numeral-value arg2)))
            (if (and num1 num2)
              (yices2-api/rational-term (mpq->string (-mpq num1 num2)))
              (if (and num1
                       (=mpq num1 *mpq-zero*))
                (yices2-api/neg-term (recur arg2))
                (yices2-api/sub-term (recur arg1)
                                     (recur arg2))))))
         (else
          (let loop ((acc (recur (car args)))
                     (rest (cdr args)))
            (if (null? rest)
              acc
              (loop (yices2-api/sub-term acc (recur (car rest)))
                    (cdr rest))))))))
     ((instance-of? expr <sal-mul>)
      (let* ((args (sal-application/argument-list expr))
             (numerals (sal-numeral-list-values args))
             (contains-numeral?
              (find (lambda (arg)
                      (sal-numeral-value arg))
                    args)))
        (if numerals
          (yices2-api/rational-term
           (mpq->string
            (let loop ((remaining numerals)
                       (result *mpq-one*))
              (if (null? remaining)
                result
                (loop (cdr remaining)
                      (*mpq result (car remaining)))))))
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
               ;; Yices' direct yices_mul path is fragile on large numeric
               ;; coefficients. Keep symbolic-only products native, but route
               ;; coefficient-bearing products through the native parser.
               ((or num1 num2)
                (solver/expr->parsed-term solver expr))
               (else
                (yices2-api/mul-terms (map recur args)))))
            (if contains-numeral?
              (solver/expr->parsed-term solver expr)
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
        (yices2-api/lt-term (recur arg1)
                            (recur arg2))))
     ((instance-of? expr <sal-le>)
      (multiple-value-bind
          (arg1 arg2)
          (sal-binary-application/arguments expr)
        (yices2-api/le-term (recur arg1)
                            (recur arg2))))
     ((instance-of? expr <sal-gt>)
      (multiple-value-bind
          (arg1 arg2)
          (sal-binary-application/arguments expr)
        (yices2-api/gt-term (recur arg1)
                            (recur arg2))))
     ((instance-of? expr <sal-ge>)
      (multiple-value-bind
          (arg1 arg2)
          (sal-binary-application/arguments expr)
        (yices2-api/ge-term (recur arg1)
                            (recur arg2))))
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
  (when (>= (verbosity-level) 5)
    (verbose-message 5
                     "sal-cdr: converting state formula to native Yices term at step ~a."
                     step))
  (let ((sat-expr (sal-ast->sat expr
                                (slot-value solver :ctx)
                                *cdr-empty-env*
                                step
                                *pos*)))
    (when (>= (verbosity-level) 5)
      (verbose-message 5
                       "sal-cdr: converted state formula to SAT form at step ~a."
                       step))
    (let ((term (solver/expr->term solver sat-expr)))
      (when (>= (verbosity-level) 5)
        (verbose-message 5
                         "sal-cdr: finished native term construction for state formula at step ~a."
                         step))
      term)))

(define (solver/expr->parsed-term solver expr)
  (let* ((decls (collect-formula-decls (list expr) '()))
         (_ (solver/sync-yices-translation! solver decls)))
    (cond
     ((eq-hash-table/get (slot-value solver :expr-term-cache) expr) => cdr)
     (else
      (let* ((yices-info (make-mapped-yices2-translation-info solver decls))
             (text (render-yices2-expr-string expr yices-info))
             (text-key (parsed-term-cache-key text)))
        (cond
         ((hashtable-get (slot-value solver :expr-text-term-cache) text-key)
          =>
          (lambda (term)
            (eq-hash-table/put! (slot-value solver :expr-term-cache) expr term)
            term))
         (else
          (let ((term (yices2-api/parse-term text)))
            (solver/cache-expr-term! solver expr text-key term)))))))))

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

(define (solver/existing-decl-id solver decl)
  (let* ((info (slot-value solver :info))
         (dp-info (slot-value info :dp-info))
         (decl->id-mapping (slot-value dp-info :decl->id-mapping)))
    (cond
     ((eq-hash-table/get decl->id-mapping decl) => cdr)
     (else #f))))

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
        (solver/require-native-term! term
                                     (string-append "declaring native term " name)
                                     type-string)
        (hashtable-put! (slot-value solver :decl-name->term) name term)
        term)))

(define (solver/ensure-helper-term! solver name type-string)
  (or (hashtable-get (slot-value solver :helper-name->term) name)
      (let ((term (yices2-api/declare-uninterpreted-term! name type-string)))
        (solver/require-native-term! term
                                     (string-append "declaring native helper term " name)
                                     type-string)
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
           ((or (= ctor *yices-constructor-arith-sum*)
                (= ctor *yices-constructor-arith-ff-sum*))
            (let ((n (yices2-api/term-num-children term)))
              (let loop ((i 0))
                (when (< i n)
                  (let ((component (yices2-api/try-sum-component-term term i)))
                    (when component
                      (visit! component)))
                  (loop (+ i 1))))))
           ((= ctor *yices-constructor-bv-sum*)
            (let ((n (yices2-api/term-num-children term)))
              (let loop ((i 0))
                (when (< i n)
                  (let ((component (yices2-api/try-bvsum-component-term term i)))
                    (when component
                      (visit! component)))
                  (loop (+ i 1))))))
           ((= ctor *yices-constructor-power-product*)
            (let ((n (yices2-api/term-num-children term)))
              (let loop ((i 0))
                (when (< i n)
                  (let ((factor (yices2-api/try-product-component-term term i)))
                    (when factor
                      (visit! factor)))
                  (loop (+ i 1))))))
           ((or (= ctor *yices-constructor-select-term*)
                (= ctor *yices-constructor-bit-term*))
            (visit! (yices2-api/proj-arg term)))
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

(define (session/ensure-memory-budget! session)
  (when (> *cdr-native-max-rss-mb* 0)
    (let ((rss (yices2-api/current-rss-mb)))
      (when (and (integer? rss)
                 (> rss *cdr-native-max-rss-mb*))
        (session/backend-error
         session
         (string-append
          "Aborting the native CDR run before it destabilizes the machine: "
          "resident memory reached ~a MiB, exceeding SAL_CDR_MAX_RSS_MB=~a. "
          "Set SAL_CDR_MAX_RSS_MB=0 to disable this safeguard.")
         rss
         *cdr-native-max-rss-mb*)))))

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
  ;; Session base formulas are long-lived skeletons that are built only when a
  ;; session is created. Parsing them through Yices keeps the mechanism fully
  ;; in-process while avoiding the more fragile direct-DAG construction path
  ;; for large transition formulas.
  (map (lambda (expr)
         (solver/expr->parsed-term solver expr))
       expr-list))

(define (make-session-base-parsed-terms solver expr-list)
  (make-session-base-terms solver expr-list))

(define (make-cdr-yices2-session solver name command logic interpolants? sort-ids declarations base-terms id->decl-proc sort->type-proc place-provider)
  (verbose-message 1 "sal-cdr: creating solver session ~a..." name)
  (ensure-yices-term-list base-terms "base terms")
  (when (>= (verbosity-level) 5)
    (verbose-message 5 "sal-cdr: session ~a syncing native translation (~a base term(s))."
                     name
                     (length base-terms)))
  (solver/sync-yices-translation! solver)
  (when (>= (verbosity-level) 5)
    (verbose-message 5 "sal-cdr: session ~a allocating native Yices context." name))
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
                                 :active-assumption-model #f
                                 :place-provider place-provider)))
    (when (>= (verbosity-level) 5)
      (verbose-message 5 "sal-cdr: session ~a asserting ~a permanent base term(s)."
                       name
                       (length base-terms)))
    (for-each (lambda (term)
                (yices2-api/assert-formula! ctx term))
              base-terms)
    (when (>= (verbosity-level) 5)
      (verbose-message 5 "sal-cdr: session ~a ready." name))
    session))

(define (session/close! session)
  (when session
    (when (slot-value session :active-assumption-model)
      (yices2-api/free-model! (slot-value session :active-assumption-model))
      (set-slot-value! session :active-assumption-model #f))
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
  (when (slot-value session :active-assumption-model)
    (yices2-api/free-model! (slot-value session :active-assumption-model))
    (set-slot-value! session :active-assumption-model #f))
  (yices2-api/context-reset! (slot-value session :context)))

(define (session/check-sat! session)
  (verbose-message 5 "sal-cdr: session ~a check-sat..." (slot-value session :name))
  (session/ensure-memory-budget! session)
  (let ((status (extract-status session
                                (yices2-api/check-context (slot-value session :context)))))
    (session/ensure-memory-budget! session)
    status))

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

(define (session/query-with-assertions/reset! session terms)
  (session/reset! session)
  (for-each (lambda (term)
              (session/assert-permanent! session term))
            terms)
  (let ((status (session/check-sat! session)))
    (session/reset! session)
    status))

(define (interpret-implication-status status)
  (when (>= (verbosity-level) 5)
    (verbose-message 5 "sal-cdr: implication check returned ~a." status))
  (cond
   ((equal? status "unsat")
    #t)
   ((or (equal? status "sat")
        (equal? status "unknown"))
    #f)
   (else
    (sign-error "sal-cdr expected sat/unsat/unknown from a minimization implication check, received ~a."
                status))))

(define (query-minimization-session-imply? solver antecedent-terms negated-consequent-term)
  (let* ((session (get-minimization-session solver))
         (query-terms (append antecedent-terms
                              (list negated-consequent-term))))
    (interpret-implication-status
     (session/query-with-assertions/reset! session query-terms))))

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

(define (session/value-string->sal-expr session value-string)
  (let ((normalized (normalize-yices-inline value-string)))
    (try
     (session/term->sal-expr session
                             (yices2-api/parse-term normalized))
     (lambda (_escape _proc msg _obj)
       (try
        (yices2/string->sal-expr normalized
                                 (slot-value session :id->decl-proc)
                                 (slot-value session :place-provider))
        (lambda (_escape2 _proc2 _msg2 _obj2)
          (sign-error "sal-cdr failed to convert a native Yices model value into a SAL expression.\nRaw value: ~a\nReason: ~a"
                      normalized
                      msg)))))))

(define (session/value-formula-string->state-expr session formula-string)
  (let* ((solver (slot-value session :solver))
         (normalized (normalize-yices-inline formula-string))
         (form (session/term->sal-expr session
                                       (yices2-api/parse-term normalized))))
    (multiple-value-bind
        (state-constraint _max-step)
        (build-state-constraint form
                                (slot-value (slot-value solver :ctx) :inv-step-decls)
                                (slot-value (slot-value solver :ctx) :inv-global-decls))
      state-constraint)))

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

(define (call/state-expr-model solver expr step proc)
  (let ((ctx #f)
        (model #f))
    (unwind-protect
     (begin
       (when (>= (verbosity-level) 5)
         (verbose-message 5 "sal-cdr: creating temporary native model context for step-~a state expression." step))
       (set! ctx (yices2-api/new-context #f))
       (when (>= (verbosity-level) 5)
         (verbose-message 5 "sal-cdr: asserting temporary model-query formula at step ~a." step))
       (yices2-api/assert-formula! ctx (solver/state-expr->parsed-term solver expr step))
       (let ((status (yices2-api/status->string (yices2-api/check-context ctx))))
         (when (>= (verbosity-level) 5)
           (verbose-message 5 "sal-cdr: temporary model-query status at step ~a: ~a." step status))
         (cond
          ((equal? status "sat")
           (when (>= (verbosity-level) 5)
             (verbose-message 5 "sal-cdr: extracting temporary native model at step ~a." step))
           (set! model (yices2-api/get-model ctx))
           (proc (map cdr (query-term-bindings solver step))
                 model))
          ((equal? status "unsat")
           #f)
          (else
           (sign-error "sal-cdr expected a satisfiable formula-generalization query at step ~a, but Yices returned ~a."
                       step
                       status)))))
     (when model
       (yices2-api/free-model! model))
     (when ctx
       (yices2-api/free-context! ctx)))))

(define (model/term-value-as-term model term)
  (or (yices2-api/try-value-as-term model term)
      (let ((value-string (yices2-api/try-value-string model term)))
        (and value-string
             (yices2-api/parse-term (normalize-yices-inline value-string))))))

(define (model->assumption-map-model raw-model assumption-terms)
  (let ((bindings
         (map (lambda (term)
                (let ((value-term (model/term-value-as-term raw-model term)))
                  (unless value-term
                    (sign-error "sal-cdr failed to materialize a native Yices value term for model assumption ~a."
                                (or (yices2-api/term-name term)
                                    (yices2-api/term->string term))))
                  (cons term value-term)))
              assumption-terms)))
    (yices2-api/model-from-map (map car bindings)
                               (map cdr bindings))))

(define (session/check-sat-with-model! session model terms)
  (session/ensure-memory-budget! session)
  (when (slot-value session :active-assumption-model)
    (yices2-api/free-model! (slot-value session :active-assumption-model))
    (set-slot-value! session :active-assumption-model #f))
  (let ((assumption-model (model->assumption-map-model model terms)))
    (set-slot-value! session :active-assumption-model assumption-model)
    (let ((status (extract-status session
                                  (yices2-api/check-context-with-model (slot-value session :context)
                                                                       assumption-model
                                                                       terms))))
      (session/ensure-memory-budget! session)
      status)))

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
                   (value-term (yices2-api/try-value-as-term model term))
                   (value-formula-string (and (not value-term)
                                              (yices2-api/try-value-formula-string model term)))
                   (value-string (and (not value-term)
                                      (not value-formula-string)
                                      (yices2-api/try-value-string model term))))
              (cond
               (value-term
                (loop (cdr remaining)
                      (cons (cons decl
                                  (session/term->sal-expr session value-term))
                            result)))
               (value-formula-string
                (loop (cdr remaining)
                      (cons (cons decl
                                  (make-cdr-cube-binding-formula
                                   (session/value-formula-string->state-expr session
                                                                             value-formula-string)))
                            result)))
               (value-string
                (loop (cdr remaining)
                      (cons (cons decl
                                  (session/value-string->sal-expr session value-string))
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

(define (make-initial-interpolation-session solver)
  (let* ((ctx (slot-value solver :ctx))
         (info (slot-value solver :info)))
    (make-cdr-yices2-session
     solver
     "interp-init"
     (slot-value solver :effective-command)
     (sat-smtlib2-context/logic ctx)
     #t
     '()
     '()
     (make-session-base-parsed-terms solver (list (slot-value solver :i0)))
     (make-id->decl-proc info)
     (make-sort->type-proc info)
     (slot-value solver :flat-module))))

(define (make-reach-interpolation-session solver level)
  (let* ((ctx (slot-value solver :ctx))
         (info (slot-value solver :info))
         (lemmas (or (vector-ref (slot-value solver :reach-lemmas) level)
                     '()))
         (session (make-cdr-yices2-session
                   solver
                   (string-append "interp-reach-" (object->string level))
                   (slot-value solver :effective-command)
                   (sat-smtlib2-context/logic ctx)
                   #t
                   '()
                   '()
                   (make-session-base-parsed-terms solver
                                                   (if (= level 0)
                                                     (list (slot-value solver :i0)
                                                           (slot-value solver :t01))
                                                     (list (slot-value solver :t01))))
                   (make-id->decl-proc info)
                   (make-sort->type-proc info)
                   (slot-value solver :flat-module))))
    (when (>= (verbosity-level) 5)
      (verbose-message 5 "sal-cdr: replaying ~a reachability lemma(s) into interp-reach-~a."
                       (length lemmas)
                       level))
    (let loop ((remaining lemmas)
               (idx 0))
      (unless (null? remaining)
        (when (>= (verbosity-level) 5)
          (verbose-message 5 "sal-cdr: asserting replayed reachability lemma ~a into interp-reach-~a."
                           idx
                           level))
        (session/assert-permanent! session
                                   (solver/state-expr->parsed-term solver (car remaining) 0))
        (loop (cdr remaining) (+ idx 1))))
    session))

(define (call/temporary-initial-interpolation-session solver proc)
  (let ((session (make-initial-interpolation-session solver)))
    (unwind-protect
     (proc session)
     (session/close! session))))

(define (call/temporary-reach-interpolation-session solver level proc)
  (let ((session (make-reach-interpolation-session solver level)))
    (unwind-protect
     (proc session)
     (session/close! session))))

(define (interpolate-state-expr-with-session solver session expr step)
  (call/state-expr-model
   solver
   expr
   step
   (lambda (terms model)
     (and (equal? (session/check-sat-with-model! session model terms) "unsat")
          (session/get-interpolant-expr solver session model)))))

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
  (when (>= (verbosity-level) 5)
    (verbose-message 5
                     "sal-cdr: preparing model query for session ~a at query step ~a/model step ~a."
                     (slot-value session :name)
                     query-step
                     model-step))
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
  (when (>= (verbosity-level) 5)
    (verbose-message 5
                     "sal-cdr: preparing status query for session ~a at step ~a."
                     (slot-value session :name)
                     query-step))
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
  (let ((session (get-minimization-session solver)))
    (session/reset! session)
    (proc session)))

(define (state-formulas-imply/with-session? solver session antecedents consequent)
  (let* ((antecedent-terms (map (lambda (antecedent)
                                  (solver/state-expr->parsed-term solver antecedent 0))
                                antecedents))
         (negated-consequent-term (solver/state-expr->parsed-term solver
                                                                  (make-sal-not consequent)
                                                                  0))
         (query-terms (append antecedent-terms
                              (list negated-consequent-term))))
    (when (>= (verbosity-level) 5)
      (verbose-message 5
                       "sal-cdr: checking state implication with ~a antecedent(s): ~a => ~a"
                       (length antecedents)
                       (map sal-expr->string antecedents)
                       (sal-expr->string consequent)))
    (define (interpret-status label)
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
                    label))))
    (if session
      (begin
        (session/push-asserts! session query-terms)
        (unwind-protect
         (interpret-status (session/check-sat! session))
         (session/pop! session)))
      (call/temporary-minimization-session
       solver
       (lambda (fresh-session)
         (state-formulas-imply/with-session? solver
                                             fresh-session
                                             antecedents
                                             consequent))))))

(define (state-formulas-imply? solver antecedents consequent)
  (let ((antecedent-terms
         (map (lambda (antecedent)
                (solver/state-expr->parsed-term solver antecedent 0))
              antecedents))
        (negated-consequent-term
         (solver/state-expr->parsed-term solver
                                         (make-sal-not consequent)
                                         0)))
    (when (>= (verbosity-level) 5)
      (verbose-message 5
                       "sal-cdr: checking state implication with ~a antecedent(s): ~a => ~a"
                       (length antecedents)
                       (map sal-expr->string antecedents)
                       (sal-expr->string consequent)))
    (query-minimization-session-imply? solver
                                       antecedent-terms
                                       negated-consequent-term)))

(define (remove-equivalent-conjunct-bindings bindings candidate-expr)
  (remove-if (lambda (binding)
               (sal-ast/equivalent? (car binding) candidate-expr))
             bindings))

(define (minimize-state-formula/greedy solver expr)
  (let* ((simplified (sal-ast/simplify expr))
         (conjuncts (sal-expr/conjuncts simplified)))
    (if (or (sal-expr/true? simplified)
            (sal-expr/false? simplified)
            (null? conjuncts)
            (null? (cdr conjuncts)))
      simplified
      (let* ((negated-simplified-term
              (solver/state-expr->parsed-term solver
                                              (make-sal-not simplified)
                                              0))
             (bindings
              (map (lambda (conjunct)
                     (cons conjunct
                           (solver/state-expr->parsed-term solver conjunct 0)))
                   conjuncts)))
        (let loop ((remaining bindings)
                   (kept bindings))
          (if (null? remaining)
            (sal-expr/from-conjuncts (map car kept)
                                     (slot-value solver :flat-module))
            (let* ((candidate-expr (caar remaining))
                   (reduced (remove-equivalent-conjunct-bindings kept
                                                                 candidate-expr))
                   (implied? (query-minimization-session-imply? solver
                                                                (map cdr reduced)
                                                                negated-simplified-term)))
              (if implied?
                (loop (cdr remaining) reduced)
                (loop (cdr remaining) kept)))))))))

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
              (init-assert-term (solver/state-expr->parsed-term solver
                                                                negated-lemma
                                                                0))
              (transition-assert-term (and (> level 0)
                                           (solver/state-expr->parsed-term solver
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
    (call/temporary-reach-interpolation-session
     solver
     level
     (lambda (session)
       (let ((interpolant
              (interpolate-state-expr-with-session
               solver
               session
               (cdr-cube->expr cube (slot-value solver :flat-module))
               1)))
         (when (>= (verbosity-level) 5)
           (verbose-message 5 "sal-cdr: interpolant query at F~a returned ~a."
                            level
                            (if interpolant "interpolant" "no interpolant")))
         interpolant)))))

(define (finalize-forward-lemma solver level lemma)
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
      #f))))

(define-method (cdr-solver/learn-forward-expr (solver <cdr-yices2-solver>) (level <primitive>) (expr <sal-expr>))
  (if (not (cdr-solver-capabilities/interpolants?
            (cdr-solver/capabilities solver)))
    #f
    (let* ((initial-lemma
            (call/temporary-initial-interpolation-session
             solver
             (lambda (init-session)
               (call/state-expr-model
                solver
                expr
                0
                (lambda (terms model)
                  (when (>= (verbosity-level) 5)
                    (verbose-message 5 "sal-cdr: forward learning, checking initial formula assumption..."))
                  (when (>= (verbosity-level) 5)
                    (verbose-message 5 "sal-cdr: forward learning initial formula assumption terms: ~a"
                                     (length terms)))
                  (and (equal? (session/check-sat-with-model! init-session model terms) "unsat")
                       (session/get-interpolant-expr solver init-session model)))))))
           (transition-lemma
            (and (> level 0)
                 (call/temporary-reach-interpolation-session
                  solver
                  (- level 1)
                  (lambda (session)
                    (call/state-expr-model
                     solver
                     expr
                     1
                     (lambda (terms model)
                       (when (>= (verbosity-level) 5)
                         (verbose-message 5 "sal-cdr: forward learning, checking transition formula assumption at F~a..."
                                          (- level 1)))
                       (when (>= (verbosity-level) 5)
                         (verbose-message 5 "sal-cdr: forward learning transition formula assumption terms: ~a"
                                          (length terms)))
                       (and (equal? (session/check-sat-with-model! session model terms) "unsat")
                            (session/get-interpolant-expr solver session model)))))))))
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
        (finalize-forward-lemma solver level lemma)))))

(define-method (cdr-solver/learn-forward-cube (solver <cdr-yices2-solver>) (level <primitive>) (cube <cdr-cube>))
  (cdr-solver/learn-forward-expr solver
                                 level
                                 (cdr-cube->expr cube (slot-value solver :flat-module))))

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
  (unwind-protect
   (let* ((generalized-terms
           (helper-generalize-terms solver
                                    (solver-step-state-decls solver 0)
                                    (reachability-generalization-formulas solver
                                                                          level
                                                                          target-expr)))
          (_ (validate-native-term-list 'generalize-reachability-target/pre
                                        generalized-terms))
          (minimized-terms
           (minimize-native-conjunct-terms/greedy solver generalized-terms))
          (_ (validate-native-term-list 'generalize-reachability-target/post
                                        minimized-terms)))
     (sal-ast/simplify
      (generalized-terms->state-formula solver minimized-terms)))
   (solver/release-transient-expr-term-caches! solver)))

(define-method (cdr-solver/generalize-induction-target (solver <cdr-yices2-solver>) (target-expr <sal-expr>))
  (when (>= (verbosity-level) 5)
    (verbose-message 5
                     "sal-cdr: generalizing induction target at depth ~a."
                     (slot-value solver :pdkind-induction-depth)))
  (unwind-protect
   (let* ((generalized-terms
           (helper-generalize-terms solver
                                    (solver-step-state-decls solver 0)
                                    (induction-generalization-formulas solver
                                                                       (slot-value solver :pdkind-induction-depth)
                                                                       target-expr)))
          (_ (validate-native-term-list 'generalize-induction-target/pre
                                        generalized-terms))
          (minimized-terms
           (begin
             (when (>= (verbosity-level) 5)
               (verbose-message 5 "sal-cdr: minimizing generalized induction target."))
             (minimize-native-conjunct-terms/greedy solver generalized-terms)))
          (_ (validate-native-term-list 'generalize-induction-target/post
                                        minimized-terms)))
     (when (>= (verbosity-level) 5)
       (verbose-message 5
                        "sal-cdr: converting ~a generalized induction term(s) back to SAL."
                        (length minimized-terms)))
     (let ((generalized-expr
            (sal-ast/simplify
             (generalized-terms->state-formula solver minimized-terms))))
       (when (>= (verbosity-level) 5)
         (verbose-message 5 "sal-cdr: finished induction-target generalization."))
       generalized-expr))
   (solver/release-transient-expr-term-caches! solver)))

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
     (solver/state-expr->term solver lemma 0)
     (solver/state-expr->term solver lemma 1)
     lemma)
   (lambda (_escape _proc _msg _obj)
     #f)))

(define-method (cdr-solver/minimize-state-formula (solver <cdr-yices2-solver>) (expr <sal-expr>))
  (unwind-protect
   (minimize-state-formula/greedy solver expr)
   (solver/release-transient-expr-term-caches! solver)))

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
