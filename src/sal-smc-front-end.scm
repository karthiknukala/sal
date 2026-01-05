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

(sal/load-source! "sal-script-util.scm")

(define (prove-core assertion prove-proc display-ce-proc liveness?)
  (let* ((flat-module (slot-value assertion :module))
         (_ (sal-module/display-var-info flat-module))
         (fsm (sal-flat-module->sal-bdd-fsm flat-module)))
    (multiple-value-bind
        (valid? counter-example)
        (prove-proc assertion fsm)
      (cond 
       (valid?
        (print "proved.")
        (when liveness?
          (warning-message "Your property is only true if it is deadlock free. You should run sal-deadlock-checker for that.")))
       ((eq? counter-example #unspecified)
        (print "invalid."))
       (else
        (display-ce-proc counter-example))))))

(define (display-ce-default ce)
  (let ((counter-example (sal-derived-path->original-path ce)))
    (print-counter-example counter-example)))
  
(define (prove-invariant assertion)
  (prove-core assertion sal-module-models/smc-invariant-core display-ce-default #f))

(define (prove-ltl-liveness assertion)
  (prove-core assertion sal-module-models/smc-accepting display-ce-default #t))

;; (define (prove-ctl assertion)
;;  (prove-core assertion sal-module-models/smc-ctl-core (lambda (_) (print "invalid."))))

(define *sal-smc-only-invariants?* #f)

(front-end/add-full-option! 
 "SMC" 
 "-oi"
 "--only-invariants"
 "Check only invariants when processing a context. This option is ignored when an assertion is specified as the target of the verification."
 (lambda ()
   (set! *sal-smc-only-invariants?* #t)))

(front-end/set-categories! '("Help" "Misc" "Assertion" "Pretty Printing" "Code Transformations" "Abstraction" "Traceability" "Path Pretty Printing" "LTL Support" "BDD Interface" "Prioritized Traversal" "SMC"))

(define (display-summary res-list)
  (print "\nSummary:")
  (for-each (lambda (decl-ce-pair)
              (let ((decl (car decl-ce-pair))
                    (ce (cdr decl-ce-pair)))
                (print "The assertion '" (sal-decl/name decl) "' located at " (format-with-location decl "") " is " (if ce "invalid" "valid") ".")))
            res-list))
              
(gen-front-end sal-smc
               "SAL Symbolic Model Checker"
"Usage: sal-smc [options] <context-name> <assertion-name>
   or  sal-smc [options] <file-name> <assertion-name>
   or  sal-smc [options] <context-name> 
   or  sal-smc [options] <file-name> 
   or  sal-smc [options] --assertion='<assertion-expr>'"
"Examples: 
  sal-smc peterson mutex

  sal-smc peterson invalid

  sal-smc ../tmp/peterson.sal invalid

  sal-smc peterson

  sal-smc ../tmp/peterson.sal

  sal-smc ~/examples/peterson.sal mutex

  sal-smc --verbose=3 peterson mutex

  sal-smc -v 3 peterson mutex

  sal-smc -v 3 ~/examples/peterson.sal mutex

  sal-smc --assertion='peterson!mutex'

  sal-smc --assertion='(@ mutex peterson)'

  sal-smc --assertion='arbiter{10}!at_most_one_ack'

  sal-smc --assertion='(@ at-most-one-ack (arbiter () (10)))'

  sal-smc --disable-traceability peterson invalid

  sal-smc --delta-path peterson invalid"
(lambda (else)
  (if *main-context-name*
    (if *main-assertion-name*
      (begin 
        (print-error "Illegal argument `" else "'. Usage:")
        (sal-smc/simple-help))
      (set! *main-assertion-name* else))
    (set! *main-context-name* else)))
(begin
  (check-assertion-name-ref-or-context sal-smc/simple-help)
  (sal/set-make-sal-string-reader-proc! make-lsal-string-reader)
  (cond
   ((or *assertion* *main-assertion-name*)
    (unless *assertion*
      (load-context-if-file-name *main-context-name*))
    (let ((assertion-qualified-name (mk-qualified-name *assertion* *main-context-name* *main-assertion-name*)))
      (guess-qualified-name-parser! assertion-qualified-name)
      (let ((assertion (make-boolean-assertion assertion-qualified-name :ltl? #t)))
        (check-if-supported-assertion assertion)
        (check-if-trivial-assertion assertion)
        (unless (instance-of? assertion <sal-module-models>)
          (print-error "Only simple assertions (MODULE-MODELS) are supported in the current version.")
          (exit -1))
        (cond
         ((sal-module-models/invariant? assertion)
          (prove-invariant assertion))
         ((sal-module-models/accepting? assertion)
          (prove-ltl-liveness assertion))
         (else
          (print-error "You should use sal-wmc to check CTL formulas.")
          (exit -1))))))
   (else
    (load-context *main-context-name*)
    (let* ((ctx (sal/context (file-name->sal-context-name *main-context-name*)))
           (res-list (sal-context/check-assertions-using-smc ctx *sal-smc-only-invariants?* (sal-smc/counter-examples?))))
      (display-summary res-list))))))

(sal-smc/main)
