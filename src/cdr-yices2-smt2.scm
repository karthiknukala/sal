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
        (import utility polarity sal-expression sal-module sal-assertion sal-type sal-pp
                sal-ast-env sal-ast-simplify sal-decls queue
                sat-context sat-bmc-context sat-generic-bmc-context
                sat-generic-context sat-generic-context-result
                sat-smtlib2-bmc-context sat-smtlib2-context smtlib2-interface
                yices2-interface
                cdr-solver)
        (export (make-cdr-yices2-solver assertion pdkind?)))

(define-class <cdr-yices2-session> ()
  (:name
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
   :bad-sessions
   :induction-sessions
   :pdkind-induction-session
   :pdkind-induction-depth
   :num-levels
   :effective-command
   :pdkind?))

(define *cdr-empty-env* (make-empty-env))

(define (make-string-from-lines lines)
  (with-output-to-string
    (lambda ()
      (for-each (lambda (line)
                  (display line)
                  (newline))
                lines))))

(define (ensure-string-value value label)
  (unless (string? value)
    (sign-error "sal-cdr expected ~a to be a string, received ~a." label value))
  value)

(define (ensure-string-list values label)
  (let loop ((remaining values)
             (idx 0))
    (unless (null? remaining)
      (unless (string? (car remaining))
        (sign-error "sal-cdr expected ~a[~a] to be a string, received ~a."
                    label
                    idx
                    (car remaining)))
      (loop (cdr remaining) (+ idx 1)))))

(define (string-prefix? prefix str)
  (and (string? str)
       (let ((prefix-len (string-length prefix))
             (str-len (string-length str)))
         (and (<= prefix-len str-len)
              (let loop ((i 0))
                (cond
                 ((= i prefix-len) #t)
                 ((char=? (string-ref prefix i) (string-ref str i))
                  (loop (+ i 1)))
                 (else #f)))))))

(define (string-contains? str pattern)
  (and (string? str)
       (string? pattern)
       (let ((str-len (string-length str))
             (pattern-len (string-length pattern)))
         (let loop ((i 0))
           (cond
            ((> (+ i pattern-len) str-len)
             #f)
            ((string-prefix? pattern (substring str i str-len))
             #t)
            (else
             (loop (+ i 1))))))))

(define (string-suffix? suffix str)
  (and (string? suffix)
       (string? str)
       (let ((suffix-len (string-length suffix))
             (str-len (string-length str)))
         (and (<= suffix-len str-len)
              (string=? suffix (substring str (- str-len suffix-len) str-len))))))

(define (append-unique candidate result)
  (if (or (not candidate)
          (not (string? candidate))
          (member candidate result))
    result
    (append result (list candidate))))

(define (candidate-sibling path file-name)
  (and (string? path)
       (> (string-length path) 0)
       (string-append (dirname path) "/" file-name)))

(define (configured-yices-smt2-candidates)
  (let* ((configured (and (string? *yices2-command*)
                          *yices2-command*))
         (result '()))
    (let ((result
           (cond
            ((or (string-suffix? "/yices_smt2" configured)
                 (string=? configured "yices_smt2")
                 (string-suffix? "/yices-smt2" configured)
                 (string=? configured "yices-smt2"))
             (append-unique configured result))
            ((or (string-suffix? "/yices" configured)
                 (string=? configured "yices")
                 (string-suffix? "/yices2" configured)
                 (string=? configured "yices2"))
             (append-unique (candidate-sibling configured "yices_smt2")
                            (append-unique (candidate-sibling configured "yices-smt2")
                                           result)))
            (else
             result))))
      (append-unique "yices_smt2"
                     (append-unique "yices-smt2"
                                    result)))))

(define (line->string-or-eof line)
  (cond
   ((or (not line) (eof-object? line))
    #f)
   ((string? line)
    line)
   (else
    (object->string line))))

(define (capture-command-first-line cmd)
  (let* ((proc (run-process "/bin/sh" "-c" cmd output: pipe:))
         (port (process-output-port proc)))
    (unwind-protect
     (let ((line (line->string-or-eof (read-line port))))
       (if line line ""))
     (process-kill proc))))

(define (check-yices2-base-command! command)
  (let ((version-line (capture-command-first-line (string-append "exec " command " --version 2>&1"))))
    (and (not (equal? version-line ""))
         (string-contains? version-line "Yices")
         version-line)))

(define (yices2-interpolants-supported? effective-command)
  (let ((interp-line (capture-command-first-line
                      (string-append "printf '(set-option :produce-unsat-model-interpolants true)\\n(set-logic QF_UFNRA)\\n(check-sat)\\n' | "
                                     effective-command
                                     " 2>&1"))))
    (not (or (equal? interp-line "")
             (string-contains? interp-line "produce-unsat-model-interpolants")
             (string-contains? interp-line "invalid option")
             (string-contains? interp-line "not found")
             (string-contains? interp-line "unsupported")
             (string-contains? interp-line "unknown")))))

(define (probe-yices2-command! effective-command pdkind?)
  (let ((mcsat-line (capture-command-first-line
                     (string-append "printf '(set-logic QF_UFNRA)\\n(check-sat)\\n' | "
                                    effective-command
                                    " 2>&1"))))
    (when (or (equal? mcsat-line "")
              (string-contains? mcsat-line "mcsat is not supported")
              (string-contains? mcsat-line "invalid option")
              (string-contains? mcsat-line "not found")
              (string-contains? mcsat-line "--mcsat"))
      (sign-error "sal-cdr requires a Yices2 SMT-LIB2 build with MCSAT support. Resolved command: ~a" effective-command)))
  (let ((interpolants-supported? (yices2-interpolants-supported? effective-command)))
    (when (and pdkind? (not interpolants-supported?))
      (sign-error "sal-cdr -i requires Yices2 unsat-model interpolants. Resolved command: ~a" effective-command))
    interpolants-supported?))

(define (effective-yices-smt2-command)
  (let loop ((remaining (configured-yices-smt2-candidates)))
    (cond
     ((null? remaining)
      (sign-error "sal-cdr could not locate a usable Yices2 SMT-LIB2 binary. Checked: ~a. Configure Yices2 with (sal/set-yices2-command! ...) or place yices_smt2 on PATH."
                  (configured-yices-smt2-candidates)))
     ((check-yices2-base-command! (car remaining))
      (string-append (car remaining) " --incremental --interactive --mcsat"))
     (else
      (loop (cdr remaining))))))

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

(define (render-smt2-declaration decl info)
  (with-output-to-string
    (lambda ()
      (sat-smtlib2-context/display-declaration decl info))))

(define (smt-id->string id)
  (cond
   ((string? id) id)
   ((symbol? id) (symbol->string id))
   (else
    (object->string id))))

(define (session/transcript-lines session)
  (reverse! (slot-value session :transcript)))

(define (session/transcript-string session)
  (make-string-from-lines (session/transcript-lines session)))

(define (session/append-transcript! session prefix lines)
  (set-slot-value! session
                   :transcript
                   (append (reverse! (map (lambda (line)
                                            (string-append prefix
                                                           (cond
                                                            ((string? line) line)
                                                            ((eq? line #f) "#f")
                                                            (else
                                                             (object->string line)))))
                                          lines))
                           (slot-value session :transcript))))

(define (session/backend-error session msg . args)
  (sign-error "~a\nSession: ~a\nCommand: ~a\nTranscript:\n~a"
              (apply format #f msg args)
              (slot-value session :name)
              (slot-value session :command)
              (session/transcript-string session)))

(define (session/assert-no-errors! session lines)
  (for-each
   (lambda (line)
     (when (or (string-prefix? "(error" line)
               (string-prefix? "Error" line))
       (session/backend-error session "SMT2 backend error: ~a" line)))
   lines))

(define (session/send-commands! session commands)
  (let* ((input-port (slot-value session :input-port))
         (output-port (slot-value session :output-port))
         (echo-id (+ 1 (slot-value session :echo-counter)))
         (sentinel (string-append "__SAL_CDR_ECHO_" (object->string echo-id) "__")))
    (set-slot-value! session :echo-counter echo-id)
    (session/append-transcript! session ">> " commands)
    (for-each (lambda (cmd)
                (display cmd input-port)
                (newline input-port))
              commands)
    (display "(echo \"" input-port)
    (display sentinel input-port)
    (display "\")" input-port)
    (newline input-port)
    (flush-output-port input-port)
    (let loop ((result '()))
      (let ((line (try
                   (line->string-or-eof (read-line output-port))
                   (lambda (_escape _proc msg obj)
                     (session/backend-error session
                                            "Failed while reading solver output after commands ~a. Original error: ~a ~a"
                                            commands
                                            msg
                                            obj)))))
        (when (>= (verbosity-level) 6)
          (verbose-message 6
                           "sal-cdr: session ~a raw solver line: ~a"
                           (slot-value session :name)
                           (if (string? line) line (object->string line))))
        (cond
         ((not line)
          (session/backend-error session "SMT2 backend terminated before reaching the echo sentinel."))
         ((equal? line sentinel)
          (let ((lines (reverse! result)))
            (session/append-transcript! session "<< " lines)
            lines))
         (else
          (loop (cons line result))))))))

(define (non-success-lines lines)
  (remove-if (lambda (line)
               (or (equal? line "")
                   (equal? line "success")))
             lines))

(define (extract-status session lines)
  (let ((filtered (non-success-lines lines)))
    (cond
     ((member "sat" filtered) "sat")
     ((member "unsat" filtered) "unsat")
     ((member "unknown" filtered) "unknown")
     (else
      (session/backend-error session "Expected sat/unsat/unknown, received: ~a" filtered)))))

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

(define (declare-all-vars info ctx)
  (map (lambda (decl)
         (render-smt2-declaration decl info))
       (queue->list (slot-value ctx :declaration-queue))))

(define (make-session-base-assertions info expr-list)
  (map (lambda (expr)
         (render-smt2-assert expr info))
       expr-list))

(define (make-cdr-yices2-session name command logic interpolants? sort-ids declarations base-assertions id->decl-proc sort->type-proc place-provider)
  (verbose-message 1 "sal-cdr: creating solver session ~a..." name)
  (let* ((proc (run-process "/bin/sh"
                            "-c"
                            (string-append "exec " (ensure-string-value command "the solver command") " 2>&1")
                            input: pipe:
                            output: pipe:))
         (session (make-instance <cdr-yices2-session>
                                 :name name
                                 :command (ensure-string-value command "the solver command")
                                 :logic (ensure-string-value logic "the SMT-LIB2 logic")
                                 :interpolants? interpolants?
                                 :process proc
                                 :input-port (process-input-port proc)
                                 :output-port (process-output-port proc)
                                 :transcript '()
                                 :echo-counter 0
                                 :id->decl-proc id->decl-proc
                                 :sort->type-proc sort->type-proc
                                 :place-provider place-provider))
         (commands
          (append
           (list "(set-option :print-success true)"
                 "(set-option :produce-models true)")
           (if interpolants?
             (list "(set-option :produce-unsat-model-interpolants true)")
             '())
           (list (string-append "(set-logic " (ensure-string-value logic "the SMT-LIB2 logic") ")"))
           (map (lambda (sort-id)
                 (string-append "(declare-sort " (symbol->string sort-id) " 0)"))
                sort-ids)
           declarations
           base-assertions))
         (_ (verbose-message 1 "sal-cdr: validating startup commands for session ~a..." name))
         (_ (ensure-string-list declarations "declarations"))
         (_ (ensure-string-list base-assertions "base assertions"))
         (_ (ensure-string-list commands "startup commands"))
         (_ (verbose-message 1 "sal-cdr: sending startup commands to session ~a..." name))
         (lines (session/send-commands! session commands)))
    (verbose-message 1 "sal-cdr: startup commands completed for session ~a." name)
    (session/assert-no-errors! session lines)
    (let ((remaining (non-success-lines lines)))
      (unless (null? remaining)
        (session/backend-error session "Unexpected solver output during initialization: ~a" remaining)))
    session))

(define (session/close! session)
  (when session
    (close-process-ports (slot-value session :process))
    (process-kill (slot-value session :process))))

(define (session/assert-permanent! session assert-cmd)
  (ensure-string-value assert-cmd "a permanent assertion command")
  (let ((lines (session/send-commands! session (list assert-cmd))))
    (ensure-string-list lines "permanent assertion response")
    (session/assert-no-errors! session lines)
    (let ((remaining (non-success-lines lines)))
      (unless (null? remaining)
        (session/backend-error session "Unexpected output while asserting a permanent formula: ~a" remaining)))))

(define (session/push-asserts! session assert-cmds)
  (ensure-string-list assert-cmds "pushed assertion commands")
  (let ((lines (session/send-commands! session (append (list "(push 1)") assert-cmds))))
    (ensure-string-list lines "push/assert response")
    (session/assert-no-errors! session lines)
    (let ((remaining (non-success-lines lines)))
      (unless (null? remaining)
        (session/backend-error session "Unexpected output while pushing asserted queries: ~a" remaining)))))

(define (session/push-assert! session assert-cmd)
  (ensure-string-value assert-cmd "a pushed assertion command")
  (verbose-message 5 "sal-cdr: session ~a push/assert..." (slot-value session :name))
  (let ((lines (session/send-commands! session (list "(push 1)" assert-cmd))))
    (ensure-string-list lines "push/assert response")
    (session/assert-no-errors! session lines)
    (let ((remaining (non-success-lines lines)))
      (unless (null? remaining)
        (session/backend-error session "Unexpected output while pushing an asserted query: ~a" remaining)))))

(define (session/pop! session)
  (let ((lines (session/send-commands! session (list "(pop 1)"))))
    (ensure-string-list lines "pop response")
    (session/assert-no-errors! session lines)
    (let ((remaining (non-success-lines lines)))
      (unless (null? remaining)
        (session/backend-error session "Unexpected output while popping a query context: ~a" remaining)))))

(define (session/check-sat! session)
  (verbose-message 5 "sal-cdr: session ~a check-sat..." (slot-value session :name))
  (let ((lines (session/send-commands! session (list "(check-sat)"))))
    (ensure-string-list lines "check-sat response")
    (session/assert-no-errors! session lines)
    (extract-status session lines)))

(define (session/query-with-assertion session assert-cmd)
  (session/push-assert! session assert-cmd)
  (let ((status (session/check-sat! session)))
    (unwind-protect
     status
     (session/pop! session))))

(define (session/query-with-assertions session assert-cmds)
  (session/push-asserts! session assert-cmds)
  (let ((status (session/check-sat! session)))
    (unwind-protect
     status
     (session/pop! session))))

(define (query-id-bindings solver step)
  (map-and-filter
   (lambda (decl)
     (let ((step-decl (lookup-step-decl (slot-value solver :ctx) decl step)))
       (and step-decl
            (let ((var-id (smtlib2-translation-info/var-id (slot-value solver :info) step-decl)))
              (and var-id
                   (cons decl (smt-id->string var-id)))))))
   (cdr-solver/state-vars solver)))

(define (render-get-value-command id-bindings)
  (with-output-to-string
    (lambda ()
      (display "(get-value (")
      (let loop ((remaining id-bindings)
                 (first? #t))
        (unless (null? remaining)
          (unless first?
            (display " "))
          (display (cdar remaining))
          (loop (cdr remaining) #f)))
      (display "))"))))

(define (render-single-get-value-command id)
  (string-append "(get-value (" id "))"))

(define (render-check-sat-assuming-model-command solver cube step)
  (let* ((ctx (slot-value solver :ctx))
         (info (slot-value solver :info))
         (bindings
          (map
           (lambda (binding)
             (let* ((decl (car binding))
                    (value (cdr binding))
                    (step-decl (lookup-step-decl ctx decl step))
                    (var-id (and step-decl
                                 (smtlib2-translation-info/var-id info step-decl))))
               (unless var-id
                 (sign-error "sal-cdr failed to map cube binding ~a at step ~a to an SMT-LIB2 identifier."
                             (sal-decl/name decl)
                             step))
               (cons (smt-id->string var-id)
                     (render-smt2-expr (sal-ast->sat value
                                                     ctx
                                                     *cdr-empty-env*
                                                     step
                                                     *pos*)
                                       info))))
           (cdr-cube/bindings cube))))
    (with-output-to-string
      (lambda ()
        (display "(check-sat-assuming-model (")
        (let loop-vars ((remaining bindings)
                        (first? #t))
          (unless (null? remaining)
            (unless first?
              (display " "))
            (display (caar remaining))
            (loop-vars (cdr remaining) #f)))
        (display ") (")
        (let loop-vals ((remaining bindings)
                        (first? #t))
          (unless (null? remaining)
            (unless first?
              (display " "))
            (display (cdar remaining))
            (loop-vals (cdr remaining) #f)))
        (display "))")))))

(define (normalize-get-value-forms forms)
  (cond
   ((and (= (length forms) 1)
         (list? (car forms)))
    (car forms))
   (else
    forms)))

(define (get-value-entry->binding form id->decl solver session)
  (unless (and (list? form)
               (= (length form) 2)
               (string? (car form)))
    (session/backend-error session "Unexpected get-value entry: ~a" form))
  (let ((decl (cond
               ((assoc (car form) id->decl) => cdr)
               (else #f))))
    (unless decl
      (session/backend-error session "Failed to map get-value identifier ~a back to a SAL state variable." (car form)))
    (cons decl
          (smtlib2/form->sal-expr (cadr form)
                                  (slot-value session :id->decl-proc)
                                  (slot-value session :sort->type-proc)
                                  (slot-value session :place-provider)))))

(define (session/get-model-cube solver session step)
  (let* ((id-bindings (query-id-bindings solver step))
         (id->decl (map (lambda (binding)
                          (cons (cdr binding) (car binding)))
                        id-bindings))
         (_ (when (>= (verbosity-level) 5)
              (verbose-message 5 "sal-cdr: extracting step-~a cube from session ~a via tracked get-value queries"
                               step
                               (slot-value session :name)))))
    (make-cdr-cube
     (map
      (lambda (binding)
        (let* ((id (cdr binding))
               (command (render-single-get-value-command id))
               (_ (when (>= (verbosity-level) 5)
                    (verbose-message 5 "sal-cdr: single get-value command for ~a: ~a" id command)))
               (lines (try
                       (session/send-commands! session (list command))
                       (lambda (_escape _proc msg obj)
                         (session/backend-error session
                                                "Failed while sending get-value for ~a at step ~a. Original error: ~a ~a"
                                                id
                                                step
                                                msg
                                                obj))))
               (_ (try
                   (session/assert-no-errors! session lines)
                   (lambda (_escape _proc msg obj)
                     (session/backend-error session
                                            "Failed while validating get-value output for ~a at step ~a. Original error: ~a ~a"
                                            id
                                            step
                                            msg
                                            obj))))
               (_ (when (>= (verbosity-level) 5)
                    (verbose-message 5 "sal-cdr: get-value response for ~a: ~a" id lines)))
               (forms (normalize-get-value-forms
                       (smtlib2/read-forms-from-string (make-string-from-lines lines)))))
          (unless (and (= (length forms) 1))
            (session/backend-error session "Unexpected get-value payload for ~a: ~a" id forms))
          (get-value-entry->binding (car forms) id->decl solver session)))
      id-bindings))))

(define (session/get-interpolant-expr solver session)
  (let* ((lines (session/send-commands! session (list "(get-unsat-model-interpolant)")))
         (_ (session/assert-no-errors! session lines))
         (forms (smtlib2/read-forms-from-string (make-string-from-lines lines))))
    (unless (and (pair? forms) (null? (cdr forms)))
      (session/backend-error session "Unexpected model interpolant output: ~a" lines))
    (multiple-value-bind
        (expr _max-step)
        (build-state-constraint (smtlib2/form->sal-expr (car forms)
                                                        (slot-value session :id->decl-proc)
                                                        (slot-value session :sort->type-proc)
                                                        (slot-value session :place-provider))
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

(define (cube-assert-command solver cube step)
  (render-state-assert solver
                       (cdr-cube->expr cube (slot-value solver :flat-module))
                       step))

(define (session/query-cube-status solver session cube step)
  (when (>= (verbosity-level) 5)
    (verbose-message 5 "sal-cdr: preparing cube query for session ~a at step ~a"
                     (slot-value session :name)
                     step))
  (let ((assert-cmd (cube-assert-command solver cube step)))
    (when (>= (verbosity-level) 5)
      (verbose-message 5 "sal-cdr: cube assertion for session ~a: ~a"
                       (slot-value session :name)
                       assert-cmd))
    (session/push-assert! session assert-cmd)
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
  (let ((assert-cmd (cube-assert-command solver cube query-step)))
    (when (>= (verbosity-level) 5)
      (verbose-message 5 "sal-cdr: cube assertion for session ~a: ~a"
                       (slot-value session :name)
                       assert-cmd))
    (session/push-assert! session assert-cmd)
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

(define (create-level-session! solver level)
  (let* ((ctx (slot-value solver :ctx))
         (info (slot-value solver :info))
         (sort-ids (sat-smtlib2-context/translation-sort-ids info))
         (decls (declare-all-vars info ctx))
         (reach-base (if (= level 0)
                       (list (slot-value solver :i0) (slot-value solver :t01))
                       (list (slot-value solver :t01))))
         (induction-base reach-base)
         (reach (make-cdr-yices2-session
                 (string-append "reach-" (object->string level))
                 (slot-value solver :effective-command)
                 (sat-smtlib2-context/logic ctx)
                 (cdr-solver-capabilities/interpolants?
                  (cdr-solver/capabilities solver))
                 sort-ids
                 decls
                 (make-session-base-assertions info reach-base)
                 (make-id->decl-proc info)
                 (make-sort->type-proc info)
                 (slot-value solver :flat-module)))
         (induction (make-cdr-yices2-session
                     (string-append "induction-" (object->string level))
                     (slot-value solver :effective-command)
                     (sat-smtlib2-context/logic ctx)
                     #f
                     sort-ids
                     decls
                     (make-session-base-assertions info induction-base)
                     (make-id->decl-proc info)
                     (make-sort->type-proc info)
                     (slot-value solver :flat-module))))
    (vector-set! (slot-value solver :reach-sessions) level reach)
    (vector-set! (slot-value solver :bad-sessions) level
                 (make-cdr-yices2-session
                  (string-append "bad-" (object->string level))
                  (slot-value solver :effective-command)
                  (sat-smtlib2-context/logic ctx)
                  #f
                  sort-ids
                  decls
                  (make-session-base-assertions info
                                                (if (= level 0)
                                                  (list (slot-value solver :i0)
                                                        (slot-value solver :t01))
                                                  (list (slot-value solver :t01))))
                  (make-id->decl-proc info)
                  (make-sort->type-proc info)
                  (slot-value solver :flat-module)))
    (vector-set! (slot-value solver :induction-sessions) level induction)))

(define (render-transition-step-assert solver step)
  (render-skeleton-assert
   solver
   (list (cons step (slot-value solver :valid-input))
         (cons step (slot-value solver :valid-state))
         (cons (+ step 1) (slot-value solver :valid-state))
         (cons step (slot-value solver :definition))
         (cons (+ step 1) (slot-value solver :definition))
         (cons step (slot-value solver :transition)))))

(define (make-pdkind-induction-session solver depth)
  (let* ((ctx (slot-value solver :ctx))
         (info (slot-value solver :info))
         (transition-asserts
          (let loop ((step 0)
                     (result '()))
            (if (>= step depth)
              (reverse! result)
              (loop (+ step 1)
                    (cons (render-transition-step-assert solver step)
                          result)))))
         (_ (sat-smtlib2-context/collect-translation-info! ctx info))
         (sort-ids (sat-smtlib2-context/translation-sort-ids info))
         (decls (declare-all-vars info ctx)))
    (make-cdr-yices2-session
     (string-append "pdkind-induction-" (object->string depth))
     (slot-value solver :effective-command)
     (sat-smtlib2-context/logic ctx)
     #f
     sort-ids
     decls
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
      (let* ((info (make-smtlib2-translation-info))
             (_ (sat-smtlib2-context/collect-translation-info! ctx info))
             (effective-command (effective-yices-smt2-command))
             (_ (verbose-message 1 "sal-cdr: resolved Yices2 SMT2 command: ~a" effective-command))
             (_ (verbose-message 1 "sal-cdr: probing solver command..."))
             (interpolants-supported? (probe-yices2-command! effective-command pdkind?))
             (_ (verbose-message 1
                                 "sal-cdr: Yices2 unsat-model interpolants available: ~a"
                                 interpolants-supported?))
             (version-line (capture-command-first-line (string-append "exec " effective-command " --version 2>&1")))
             (solver (make-instance <cdr-yices2-solver>
                                    :capabilities (make-cdr-solver-capabilities
                                                   :incremental? #t
                                                   :models? #t
                                                   :interpolants? interpolants-supported?
                                                   :unsat-cores? #f)
                                    :flat-module flat-module
                                    :state-vars state-vars
                                    :trace-solver-id 'yices2
                                    :solver-description (if (equal? version-line "")
                                                          effective-command
                                                          (string-append effective-command " [" version-line "]"))
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
                                    :bad-sessions (make-vector 4 #f)
                                    :induction-sessions (make-vector 4 #f)
                                    :pdkind-induction-session #f
                                    :pdkind-induction-depth 0
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
                          "init"
                          effective-command
                          (sat-smtlib2-context/logic ctx)
                          interpolants-supported?
                          (sat-smtlib2-context/translation-sort-ids info)
                          (declare-all-vars info ctx)
                          (make-session-base-assertions info (list i0))
                          (make-id->decl-proc info)
                          (make-sort->type-proc info)
                          flat-module))
        (create-level-session! solver 0)
        (create-level-session! solver 1)
        solver))))

(define (query-asserted-model solver session expr step)
  (let ((assert-cmd (render-smt2-assert expr (slot-value solver :info))))
    (ensure-string-value assert-cmd "a query assertion command")
    (verbose-message 4 "sal-cdr: querying session ~a at step ~a..." (slot-value session :name) step)
    (session/push-assert! session assert-cmd))
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

(define (session/query-assertion-unsat? session assert-cmd)
  (equal? (session/query-with-assertion session assert-cmd) "unsat"))

(define (forward-lemma-valid? solver level lemma)
  (and lemma
       (let* ((init-session (slot-value solver :init-session))
              (init-ok? (session/query-assertion-unsat? init-session
                                                        (render-state-assert solver
                                                                             (make-sal-not lemma)
                                                                             0)))
              (transition-ok?
               (if (> level 0)
                 (session/query-assertion-unsat? (vector-ref (slot-value solver :reach-sessions)
                                                             (- level 1))
                                                (render-state-assert solver
                                                                     (make-sal-not lemma)
                                                                     1))
                 #t)))
         (when (and (>= (verbosity-level) 4)
                    (or (not init-ok?) (not transition-ok?)))
           (verbose-message 4 "sal-cdr: rejected forward lemma at F~a because it failed semantic validation." level)
           (verbose-message 4 "  lemma: ~a"
                            (with-output-to-string
                              (lambda ()
                                (sal/pp lemma))))
           (verbose-message 4 "  initial-valid? ~a, transition-valid? ~a"
                            init-ok?
                            transition-ok?))
         (and init-ok? transition-ok?))))

(define-method (cdr-solver/add-frame! (solver <cdr-yices2-solver>))
  (let ((level (slot-value solver :num-levels)))
    (when (>= level (vector-length (slot-value solver :reach-sessions)))
      (let* ((old-reach (slot-value solver :reach-sessions))
             (old-bad (slot-value solver :bad-sessions))
             (old-induction (slot-value solver :induction-sessions))
             (new-size (* 2 (vector-length old-reach)))
             (new-reach (make-vector new-size #f))
             (new-bad (make-vector new-size #f))
             (new-induction (make-vector new-size #f)))
        (let loop ((i 0))
          (when (< i (vector-length old-reach))
            (vector-set! new-reach i (vector-ref old-reach i))
            (vector-set! new-bad i (vector-ref old-bad i))
            (vector-set! new-induction i (vector-ref old-induction i))
            (loop (+ i 1))))
        (set-slot-value! solver :reach-sessions new-reach)
        (set-slot-value! solver :bad-sessions new-bad)
        (set-slot-value! solver :induction-sessions new-induction)))
    (create-level-session! solver level)
    (set-slot-value! solver :num-levels (+ level 1))
    level))

(define-method (cdr-solver/add-lemma-at-level! (solver <cdr-yices2-solver>) (level <primitive>) (lemma <sal-expr>))
  (when (> level 0)
    (let ((assert-cmd (render-state-assert solver lemma 0))
          (bad-assert-cmd (render-state-assert solver lemma 1)))
      (session/assert-permanent! (vector-ref (slot-value solver :reach-sessions) level) assert-cmd)
      (session/assert-permanent! (vector-ref (slot-value solver :bad-sessions) level) bad-assert-cmd)
      (session/assert-permanent! (vector-ref (slot-value solver :induction-sessions) level) assert-cmd))))

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

(define-method (cdr-solver/check-bad-at-frame (solver <cdr-yices2-solver>) (level <primitive>))
  (query-asserted-model solver
                        (vector-ref (slot-value solver :bad-sessions) level)
                        (slot-value solver :bad1)
                        1))

(define-method (cdr-solver/check-predecessor (solver <cdr-yices2-solver>) (level <primitive>) (cube <cdr-cube>))
  (session/query-cube-model solver
                            (vector-ref (slot-value solver :reach-sessions) level)
                            cube
                            1
                            0))

(define-method (cdr-solver/check-predecessor-status (solver <cdr-yices2-solver>) (level <primitive>) (cube <cdr-cube>))
  (session/query-cube-status solver
                             (vector-ref (slot-value solver :reach-sessions) level)
                             cube
                             1))

(define-method (cdr-solver/check-lemma-propagation (solver <cdr-yices2-solver>) (level <primitive>) (lemma <sal-expr>))
  (let* ((session (vector-ref (slot-value solver :induction-sessions) level))
         (negated-next-lemma (make-sal-not (sal-ast->sat lemma
                                                         (slot-value solver :ctx)
                                                         *cdr-empty-env*
                                                         1
                                                         *pos*))))
    (equal? (session/query-with-assertion session
                                          (render-smt2-assert negated-next-lemma
                                                              (slot-value solver :info)))
            "unsat")))

(define-method (cdr-solver/interpolant-for-cube (solver <cdr-yices2-solver>) (level <primitive>) (cube <cdr-cube>))
  (if (not (cdr-solver-capabilities/interpolants?
            (cdr-solver/capabilities solver)))
    #f
    (let* ((session (vector-ref (slot-value solver :reach-sessions) level))
           (check-cmd (render-check-sat-assuming-model-command solver cube 1)))
      (when (>= (verbosity-level) 5)
        (verbose-message 5 "sal-cdr: interpolant assuming-model query at F~a: ~a"
                         level
                         check-cmd))
      (let* ((lines (session/send-commands! session (list check-cmd)))
             (_ (session/assert-no-errors! session lines))
             (status (extract-status session lines)))
        (when (>= (verbosity-level) 5)
          (verbose-message 5 "sal-cdr: interpolant query status at F~a: ~a"
                           level
                           status))
        (if (equal? status "unsat")
          (session/get-interpolant-expr solver session)
          #f)))))

(define-method (cdr-solver/learn-forward-cube (solver <cdr-yices2-solver>) (level <primitive>) (cube <cdr-cube>))
  (if (not (cdr-solver-capabilities/interpolants?
            (cdr-solver/capabilities solver)))
    #f
    (let* ((init-session (slot-value solver :init-session))
           (init-check (render-check-sat-assuming-model-command solver cube 0))
           (init-lines (session/send-commands! init-session (list init-check)))
           (_ (session/assert-no-errors! init-session init-lines))
           (init-status (extract-status init-session init-lines))
           (initial-lemma (and (equal? init-status "unsat")
                               (session/get-interpolant-expr solver init-session)))
           (transition-lemma
            (and (> level 0)
                 (let* ((session (vector-ref (slot-value solver :reach-sessions) (- level 1)))
                        (check-cmd (render-check-sat-assuming-model-command solver cube 1))
                        (lines (session/send-commands! session (list check-cmd))))
                   (session/assert-no-errors! session lines)
                   (if (equal? (extract-status session lines) "unsat")
                     (session/get-interpolant-expr solver session)
                     #f)))))
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
        (let ((simplified (and lemma (sal-ast/simplify lemma))))
          (cond
           ((and simplified (forward-lemma-valid? solver level simplified))
            simplified)
           ((and lemma (forward-lemma-valid? solver level lemma))
            lemma)
           (else
            #f)))))))

(define-method (cdr-solver/reset-induction! (solver <cdr-yices2-solver>) (depth <primitive>))
  (when (slot-value solver :pdkind-induction-session)
    (session/close! (slot-value solver :pdkind-induction-session)))
  (set-slot-value! solver :pdkind-induction-depth depth)
  (set-slot-value! solver
                   :pdkind-induction-session
                   (make-pdkind-induction-session solver depth))
  depth)

(define (assert-induction-lemma-at-step! solver session lemma step)
  (session/assert-permanent! session (render-state-assert solver lemma step)))

(define-method (cdr-solver/add-induction-lemma! (solver <cdr-yices2-solver>) (lemma <sal-expr>))
  (let ((session (slot-value solver :pdkind-induction-session))
        (depth (slot-value solver :pdkind-induction-depth)))
    (unless session
      (sign-error "sal-cdr internal error: missing PDKIND induction session."))
    (assert-induction-lemma-at-step! solver session lemma 0)
    (let loop ((step 1))
      (when (< step depth)
        (assert-induction-lemma-at-step! solver session lemma step)
        (loop (+ step 1))))))

(define-method (cdr-solver/check-inductive-witness (solver <cdr-yices2-solver>) (lemma <sal-expr>))
  (let* ((session (slot-value solver :pdkind-induction-session))
         (depth (slot-value solver :pdkind-induction-depth))
         (negated-final (render-state-assert solver (make-sal-not lemma) depth)))
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

(define-method (cdr-solver/check-induction-path (solver <cdr-yices2-solver>) (cube <cdr-cube>) (target-expr <sal-expr>))
  (let* ((session (slot-value solver :pdkind-induction-session))
         (depth (slot-value solver :pdkind-induction-depth))
         (cube-assert (cube-assert-command solver cube 0))
         (target-assert (render-state-assert solver target-expr depth)))
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

(define-method (cdr-solver/close! (solver <cdr-yices2-solver>))
  (session/close! (slot-value solver :init-session))
  (session/close! (slot-value solver :pdkind-induction-session))
  (let loop ((i 0))
    (when (< i (slot-value solver :num-levels))
      (session/close! (vector-ref (slot-value solver :reach-sessions) i))
      (session/close! (vector-ref (slot-value solver :bad-sessions) i))
      (session/close! (vector-ref (slot-value solver :induction-sessions) i))
      (loop (+ i 1)))))
