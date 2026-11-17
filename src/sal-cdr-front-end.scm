;; SAL counterexample-directed reachability (PDKIND).
;; The ordinary SAL parser, elaborator, and assertion transformations supply
;; the transition system; sal-cdr uses the existing Yices2 FFI in process.

(module sal-cdr-front-end
  (include "sal.sch")
  (import front-end runtime sal-api sal-module sal-assertion sal-environment
          sal-derived-path sal-path-pp sal-guess-reader lsal-string-reader
          sal-pp sal-cdr yices-interface yices2-interface ics-interface
          smtlib2-interface)
  (include "front-end.macros")
  (include "sal-script-util.scm")
  (main sal-cdr-native-main)
  (eval (export-all)))

(define *cdr-max-depth* 0)
(define *cdr-statistics?* #f)
(define *cdr-statistics-format* #f)
(define *cdr-live-stats* #f)
(define *cdr-live-stats-time* 100)
(define *cdr-skolemize?* #f)
(define *cdr-show-invariant?* #f)
(define *cdr-cex-graph* #f)
(define *cdr-parse-only?* #f)
(define *cdr-inputs* '())

(front-end/add-simple-option!
 "Verification"
 "--parse-only"
 "Parse and type-check each SAL context or qualified assertion."
 (lambda () (set! *cdr-parse-only?* #t)))

(front-end/add-full-option!
 "Verification"
 "-d <num>"
 "--max-depth=<num>"
 "Limit the PDKIND induction depth (default: 0, unlimited). Reaching the limit reports unknown."
 (front-end-adapter/nat-arg
  (lambda (n) (set! *cdr-max-depth* n))))

(front-end/add-simple-option!
 "Verification"
 "--pdkind-max=<num>"
 "Stop at the PDKIND frame limit and report unknown (default: 0, unlimited)."
 (front-end-adapter/nat-arg
  (lambda (n) (sal-cdr/set-option! 'max-frame n))))

(front-end/add-simple-option!
 "Verification"
 "--pdkind-induction-max=<num>"
 "Clamp induction depth to <num> and continue searching (default: 0, unlimited). This does not stop the search like --max-depth."
 (front-end-adapter/nat-arg
  (lambda (n) (sal-cdr/set-option! 'induction-max n))))

(define (cdr/add-switch! name key description)
  (front-end/add-simple-option! "Verification" name description
    (lambda () (sal-cdr/set-option! key #t))))

(cdr/add-switch!
 "--pdkind-add-backward" 'add-backward
 "Add learned reachability constraints to earlier frames.")

(cdr/add-switch!
 "--pdkind-check-deadlock" 'check-deadlock
 "Check for inconsistent reachability and induction frames during the search.")

(cdr/add-switch!
 "--pdkind-minimize-interpolants" 'minimize-interpolants
 "Try to remove redundant constraints from learned interpolants.")

(cdr/add-switch!
 "--pdkind-minimize-generalizations" 'minimize-generalizations
 "Try to remove redundant constraints from backward generalizations.")

(cdr/add-switch!
 "--pdkind-minimize-frames" 'minimize-frames
 "Try to remove redundant induction obligations from each frame.")

(front-end/add-simple-option!
 "Verification"
 "--pdkind-output-cex-graph=<file>"
 "Write the obligation graph in Graphviz DOT format. Batch filenames receive a numbered suffix."
 (lambda (file)
   (when (= (string-length file) 0)
     (sign-invalid-arg "A nonempty counterexample graph filename is required."))
   (set! *cdr-cex-graph* file)
   (sal-cdr/set-option! 'cex-graph file)))

(front-end/add-simple-option!
 "Verification"
 "--show-invariant"
 "Display the inductive invariant when the property is proved."
 (lambda () (set! *cdr-show-invariant?* #t)))

(front-end/add-simple-option!
 "Verification"
 "--yices2-mode=<mode>"
 "Select the solver mode: auto, dpllt, mcsat, or hybrid (default: auto)."
 (lambda (mode)
   (let ((value (string->symbol mode)))
     (unless (memq value '(auto dpllt mcsat hybrid))
       (sign-invalid-arg "Yices2 mode must be auto, dpllt, mcsat, or hybrid."))
     (sal-cdr/set-option! 'solver-mode value))))

(front-end/add-simple-option!
 "Verification"
 "--solver-logic=<logic>"
 "Configure the Yices2 DPLL(T) context for the given SMT-LIB logic."
 (lambda (logic) (sal-cdr/set-option! 'solver-logic logic)))

(front-end/add-simple-option!
 "Verification"
 "--smt2-output=<prefix>"
 "Write each solver query to a standalone SMT-LIB file with this prefix."
 (lambda (prefix) (sal-cdr/set-option! 'smt2-output prefix)))

(front-end/add-simple-option!
 "Verification"
 "--yices2-trace-tags=<tags>"
 "Enable comma-separated trace tags in a debug build of Yices2."
 (lambda (tags) (sal-cdr/set-option! 'solver-trace-tags tags)))

(for-each (lambda (name)
            (front-end/add-simple-option! "Verification" name
              "Display PDKIND solver and search statistics."
              (lambda () (set! *cdr-statistics?* #t))))
          '("--statistics" "--stats"))

(front-end/add-simple-option!
 "Verification"
 "--stats-format=<format>"
 "Display formatted statistics after checking the assertion. See --stats-help for fields."
 (lambda (format) (set! *cdr-statistics-format* format)))

(front-end/add-simple-option!
 "Verification"
 "--live-stats=<file>"
 "Write periodic statistics to a tab-separated file. Batch filenames receive a numbered suffix. Use - for stdout."
 (lambda (path)
   (when (= (string-length path) 0)
     (sign-invalid-arg "A nonempty live statistics filename is required."))
   (set! *cdr-live-stats* path)))

(front-end/add-simple-option!
 "Verification"
 "--live-stats-time=<num>"
 "Set the live statistics period in milliseconds (default: 100)."
 (front-end-adapter/nat-arg (lambda (period) (set! *cdr-live-stats-time* period))))

(front-end/add-simple-option!
 "Verification"
 "--stats-help"
 "List the fields available in --stats-format and exit."
 (lambda ()
   (print "Use %<field> in --stats-format. For example, --stats-format='%f %r %t'.")
   (print "  %f  Input file or SAL context")
   (print "  %e  Engine")
   (print "  %s  Solver")
   (print "  %r  Result")
   (print "  %t  Elapsed seconds")
   (for-each (lambda (field) (print "  %" (cadr field) "  " (car field)))
             (sal-cdr/statistics))
   (exit 0)))

(define (cdr-current-time)
  (exact->inexact (current-microseconds)))

(define (cdr-display-statistics-format format filename status start)
  (let* ((seconds (/fl (-fl (cdr-current-time) start) 1000000.0))
         (fields (append (map cdr (sal-cdr/statistics))
                    (list (list "f" filename) (list "e" 'pdkind) (list "s" 'yices2)
                          (list "r" status) (list "t" seconds))))
         (length (string-length format)))
    (print
      (with-output-to-string
        (lambda ()
          (let loop ((i 0))
            (when (< i length)
              (let* ((next (+ i 1)) (ch (string-ref format i))
                     (field (and (char=? ch #\%)
                              (find (lambda (field)
                                      (let ((end (+ next (string-length (car field)))))
                                        (and (<= end length)
                                             (string=? (car field) (substring format next end)))))
                                    fields))))
                (cond
                  (field (display (cadr field)) (loop (+ next (string-length (car field)))))
                  ((and (char=? ch #\%) (< next length) (char=? (string-ref format next) #\%))
                   (display ch) (loop (+ next 1)))
                  (else (display ch) (loop next)))))))))))

(define (cdr-statistics-record status)
  (cons status (map caddr (sal-cdr/statistics))))

(define (cdr-write-statistics-row fields)
  (print (with-output-to-string
           (lambda ()
             (display (car fields))
             (for-each (lambda (field) (display "\t") (display field)) (cdr fields)))))
  (flush-output-port (current-output-port)))

(define (cdr-live-stats-period milliseconds)
  (maxfl 1000.0 (*fl 1000.0 (exact->inexact milliseconds))))

(define (cdr-live-stats-reporter filename milliseconds started)
  (let ((input (current-input-port))
        (interval (cdr-live-stats-period milliseconds)))
    (define (sample record now)
      (cdr-write-statistics-row
        (append (list filename 'pdkind 'yices2
                      (if (eq? (car record) 'running) 'unknown (car record))
                      (/fl (-fl now started) 1000000.0))
                (cdr record))))
    (cdr-write-statistics-row
      (append '("filename" "engine" "solver" "result" "time")
              (map (lambda (field) (string-append "cdr::" (car field))) (sal-cdr/statistics))))
    (let loop ((record (cdr-statistics-record 'running)) (next started))
      (let ((line (and (char-ready? input) (read-line input))))
        (if (eof-object? line)
          (sample (cons 'error (cdr record)) (cdr-current-time))
          (let ((current (if line (with-input-from-string line read) record))
                (now (cdr-current-time)))
            (cond
              ((not (eq? (car current) 'running)) (sample current now))
              ((>=fl now next)
               (sample current now)
               (loop current (+fl now interval)))
              (else
               ;; Poll the pipe so completion is reported promptly even
               ;; when the caller selects a long sampling period.
               (unless line (sleep (flonum->fixnum (minfl 10000.0 (maxfl 1.0 (-fl next now))))))
               (loop current next)))))))))

(define (cdr-with-live-statistics destination filename started thunk)
  ;; The reporter is a mode of this same executable. Only the solver process
  ;; owns SAL/Yices2 state. The reporter reads snapshots from its input pipe.
  (let* ((args (list "--live-stats-reporter" filename
                     (number->string *cdr-live-stats-time*) (number->string started)
                     input: pipe: wait: #f))
         (reporter (apply run-process (executable-name)
                     (if (string=? destination "-") args
                       (append args (list output: destination)))))
         (pipe (process-input-port reporter))
         (period (minfl 10000.0 (cdr-live-stats-period *cdr-live-stats-time*)))
         (next started)
         (result 'error))
    (define (send status)
      (write (cdr-statistics-record status) pipe)
      (newline pipe)
      (flush-output-port pipe))
    (define (observe)
      (let ((now (cdr-current-time)))
        (when (>=fl now next)
          (send 'running)
          (set! next (+fl now period)))))
    (unwind-protect
      (sal-cdr/with-statistics-observer observe
        (lambda ()
          (multiple-value-bind (status path) (thunk)
            (set! result status)
            (values status path))))
      (begin
        (unwind-protect
          (unwind-protect (send result) (close-output-port pipe))
          (process-wait reporter))
        (unless (equal? (process-exit-status reporter) 0)
          (sign-error "sal-cdr: live statistics reporter failed."))))))

(front-end/add-full-option!
 "Verification"
 "-sk"
 "--skolemize"
 "Try to skolemize quantified variables whenever possible."
 (lambda () (set! *cdr-skolemize?* #t)))

(front-end/set-categories!
 '("Help" "Misc" "Assertion" "Pretty Printing" "Code Transformations"
   "Extra Code Transformations" "Abstraction" "Traceability"
   "Path Pretty Printing" "Verification" "Var Mapping"))

(define (cdr-assertion-name context assertion qualified)
  (sal/set-make-sal-string-reader-proc! make-lsal-string-reader)
  (when context (load-context-if-file-name context))
  (let ((name (mk-qualified-name qualified context assertion)))
    (guess-qualified-name-parser! name)
    name))

(define (cdr-check-assertion context assertion qualified)
  (let* ((name (cdr-assertion-name context assertion qualified))
         (flat (make-flat-assertion name :ltl? #t :skolemize? *cdr-skolemize?*)))
    (check-if-supported-assertion flat)
    (unless (sal-module-models/invariant? flat)
      (sign-error "sal-cdr supports safety invariants only."))
    (sal-module/display-var-info (slot-value flat :module))
    ;; A constant-false property still needs an initial-state feasibility
    ;; check because an empty initial-state set has no counterexample.
    (sal-cdr/invariant flat *cdr-max-depth*)))

(define (cdr-run-query context assertion qualified destination)
  (let ((start (and (or *cdr-statistics-format* destination) (cdr-current-time)))
        (filename (or context qualified)))
    (multiple-value-bind (status counter-example)
        (if destination
          (cdr-with-live-statistics destination filename start
            (lambda () (cdr-check-assertion context assertion qualified)))
          (cdr-check-assertion context assertion qualified))
      (case status
        ((valid)
         (print "proved.")
         (when *cdr-show-invariant?* (sal-cdr/display-invariant)))
        ((invalid)
         (if (eq? counter-example #unspecified)
           (print "invalid.")
           (print-counter-example (sal-derived-path->original-path counter-example))))
        ((unknown) (print "unknown."))
        (else (sign-error "Unexpected sal-cdr result: ~a" status)))
      (when *cdr-statistics?* (sal-cdr/display-statistics))
      (when *cdr-statistics-format*
        (cdr-display-statistics-format *cdr-statistics-format* filename status start)))))

(define (cdr-parse-inputs inputs)
  (if *assertion*
    (begin
      ;; Resolve the name through SAL's ordinary assertion API.
      (let ((name (cdr-assertion-name #f #f *assertion*)))
        (sal-assertion-name/definition (sal/assertion-name name)))
      (print "Ok."))
    (let loop ((inputs inputs))
      (load-context (car inputs))
      (print "Ok.")
      (unless (null? (cdr inputs))
        (unless (string=? (car inputs) (cadr inputs)) (sal/reset!))
        (loop (cdr inputs))))))

(define (cdr-numbered-output path index)
  (and path (string-append path "." (number->string index))))

(define (cdr-run-batch inputs)
  (let loop ((inputs inputs) (index 1))
    (let ((context (car inputs))
          (destination (if (equal? *cdr-live-stats* "-") "-"
                         (cdr-numbered-output *cdr-live-stats* index))))
      (when *cdr-cex-graph*
        (sal-cdr/set-option! 'cex-graph (cdr-numbered-output *cdr-cex-graph* index)))
      (verbose-message 1 "Checking assertion ~a in ~a..." (cadr inputs) context)
      (cdr-run-query context (cadr inputs) #f destination)
      (unless (null? (cddr inputs))
        (sal-cdr/reset!)
        ;; Independent files may declare the same context name. SAL caches
        ;; imports by name, so discard that cache when the input changes.
        (unless (string=? context (caddr inputs)) (sal/reset!))
        (loop (cddr inputs) (+ index 1))))))

(gen-front-end sal-cdr
 "SAL Counterexample-Directed Reachability Checker"
 "Usage: sal-cdr [options] <context-name> <assertion-name>
   or  sal-cdr [options] <file-name> <assertion-name> [<file-name> <assertion-name> ...]
   or  sal-cdr [options] --assertion='<assertion-expr>'
   or  sal-cdr --parse-only <context-or-file> ..."
 "Examples:
  sal-cdr inf_bakery mutex

  sal-cdr examples/inf-bakery/inf_bakery.sal mutex

  sal-cdr --statistics --max-depth=20 inf_bakery mutex

  sal-cdr --pdkind-induction-max=2 --show-invariant inf_bakery mutex

  sal-cdr --assertion='arbiter{10}!at_most_one_ack'"
 (lambda (arg) (set! *cdr-inputs* (cons arg *cdr-inputs*)))
 (let ((inputs (reverse! *cdr-inputs*)))
   (cond
     ((and *assertion* (pair? inputs))
      (sign-error "Supply either --assertion or positional inputs."))
     (*cdr-parse-only?*
      (when (and (not *assertion*) (null? inputs))
        (sal-cdr/simple-help))
      (cdr-parse-inputs inputs))
     (*assertion* (cdr-run-query #f #f *assertion* *cdr-live-stats*))
     ((or (null? inputs) (odd? (length inputs)))
      (print-error "Supply a context or filename and an assertion name for every query.")
      (sal-cdr/simple-help))
     ((null? (cddr inputs))
      (cdr-run-query (car inputs) (cadr inputs) #f *cdr-live-stats*))
     (else (cdr-run-batch inputs)))))

(define (sal-cdr-native-main argv)
  ;; Compile the ordinary SAL front end instead of booting the interactive
  ;; Scheme interpreter and loading its macro files on every invocation.
  (let ((args (cdr argv)))
    (if (and (pair? args) (string=? (car args) "--live-stats-reporter") (= (length args) 4))
      (cdr-live-stats-reporter (cadr args) (string->integer (caddr args)) (string->real (cadddr args)))
      (begin
        (set! *arguments* args)
        (sal/set-directory!)
        (sal/initialize-pp!)
        (sal/init-main-sal-env!)
        (load-config-file ".salrc")
        (sal-cdr/main)))))
