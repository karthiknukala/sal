;;
;; SAL 3.3, Copyright (C) 2026, SRI International.  All Rights Reserved.
;;
;; SAL is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;

(sal/enable-trace-stack! #t)

(define *cdr-name* "SAL CDR Safety Engine")
(define *cdr-usage*
"Usage: sal-cdr [options] <context-name> <assertion-name>
   or  sal-cdr [options] <file-name> <assertion-name>
   or  sal-cdr [options] --assertion='<assertion-expr>'")
(define *cdr-examples* "Examples:
  sal-cdr nonlinear_square nonnegative

  sal-cdr -i --depth=12 nonlinear_product positive

  sal-cdr --solver=yices2 ../examples/nonlinear/nonlinear_square.sal nonnegative")

(front-end/set-categories! '("Help" "Misc" "Assertion" "Pretty Printing" "Code Transformations" "Extra Code Transformations" "Abstraction" "Traceability" "Path Pretty Printing" "LTL Support" "Verification" "Var Mapping"))

(sal/load-source! "sal-script-util.scm")

(define *max-frame* 10)
(define *pdkind?* #f)
(define *solver-id* 'yices2)
(define *skolemize* #f)

(front-end/add-full-option!
 "Verification"
 "-d <num>"
 "--depth=<num>"
 "Explore frames up to index <num> before returning unknown (default: 10)."
 (front-end-adapter/nat-arg
  (lambda (n)
    (set! *max-frame* n))))

(front-end/add-simple-option!
 "Verification"
 "--to=<num>"
 "Explore frames up to index <num> before returning unknown (default: 10)."
 (front-end-adapter/nat-arg
  (lambda (n)
    (set! *max-frame* n))))

(front-end/add-full-option!
 "Verification"
 "-i"
 "--pdkind"
 "Enable the interpolation-backed PDKIND enrichment layer."
 (lambda ()
   (set! *pdkind?* #t)))

(front-end/add-full-option!
 "Verification"
 "-s <name>"
 "--solver=<name>"
 "Set backend solver to be used (default: yices2). sal-cdr v1 supports only yices2."
 (lambda (arg)
   (set! *solver-id* (string->symbol arg))))

(front-end/add-full-option!
 "Verification"
 "-sk"
 "--skolemize"
 "Try to skolemize quantified variables whenever possible."
 (lambda ()
   (set! *skolemize* #t)))

(gen-front-end sal-cdr *cdr-name* *cdr-usage* *cdr-examples*
               (lambda (else)
                 (if *main-context-name*
                   (if *main-assertion-name*
                     (begin
                       (print-error "Illegal argument `" else "'. Usage:")
                       (sal-cdr/simple-help))
                     (set! *main-assertion-name* else))
                   (set! *main-context-name* else)))
               (begin
                 (check-assertion-name-ref sal-cdr/simple-help)
                 (sal/set-make-sal-string-reader-proc! make-lsal-string-reader)
                 (unless *assertion*
                   (load-context-if-file-name *main-context-name*))
                 (let* ((assertion-qualified-name-str (mk-qualified-name *assertion* *main-context-name* *main-assertion-name*))
                        (_ (guess-qualified-name-parser! assertion-qualified-name-str))
                        (assertion (make-flat-assertion assertion-qualified-name-str :ltl? #t :skolemize? *skolemize*)))
                   (check-if-supported-assertion assertion)
                   (unless (sal-module-models/invariant? assertion)
                     (sign-error "sal-cdr supports only safety/invariant properties in v1."))
                   (check-if-trivial-assertion assertion)
                   (sal-module/display-var-info (slot-value assertion :module))
                   (multiple-value-bind
                       (status counterexample)
                       (sal-cdr/check-safety assertion *max-frame* *pdkind?* *solver-id*)
                     (cond
                      ((eq? status 'valid)
                       (print "valid."))
                      ((eq? status 'invalid)
                       (print "invalid.")
                       (when counterexample
                         (print-counter-example (sal-derived-path->original-path counterexample))))
                      (else
                       (print "unknown.")))))))

(sal-cdr/main)
