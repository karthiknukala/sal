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

(sal/enable-trace-stack! #t)

(sal/load-source! "sal-script-util.scm")

(define *acyclic?* #f)
(define *solver-id* 'yices)

(define *purpose?* #f)
(define *scan?* #t)
(define *prune?* #t)
(define *slice?* #t)
(define *innerslice?* #f)
(define *branch?* #f)
(define *smcinit?* #f)
(define *initd* 8)
(define *extendd* 8)
(define *mind* 0)
(define *iincremental?* #f)
(define *eincremental?* #f)
(define *inputsonly?* #t)
(define *infbmc?* #f)
(define *simplenames?* #f)

(sal/set-trace-info-enabled! #f)


;-------------------------------
(define-method (sal-smc/find-path-from-initial-state-with-at-most (module <sal-bdd-fsm>) (goal <bdd>) (max-steps <primitive>))
  (sal-smc/find-path-from-initial-state-with-at-most-core module goal max-steps))

(define-method (sal-smc/find-path-from-initial-state-with-at-most (module <sal-flat-module>) (goal <sal-expr>) (max-steps <primitive>))
  (let* ((fsm (sal-flat-module->sal-bdd-fsm module))
         (goal-bdd (sal-expr->bdd goal fsm)))
    (sal-smc/find-path-from-initial-state-with-at-most-core fsm goal-bdd max-steps)))

(define (sal-smc/find-path-from-initial-state-with-at-most-core fsm goal-bdd max-steps)
  (verbose-message 1 "finding state starting at the initial state with at most ~a steps..." max-steps)
  (display-runtime 2 "  search time: ~a secs"
    (lambda ()
      (let ((trace (sal-bdd-fsm/make-trace-from-to-with-at-most fsm (sal-bdd-fsm/initial-states fsm) goal-bdd max-steps)))
        (cond
         (trace
          (values (sal-bdd-fsm/make-path fsm trace #t) fsm))
         (else
          (values #f fsm)))))))
;---------------------------------


(front-end/add-simple-option! "Module" "--module=<name>"
                              "Qualified module name using the LSAL syntax. Qualified module names are useful to reference modules in parametric contexts. Examples of qualified module names:\n(@ n-arbiter (arbiter () (10)))\n(@ system mutex)"
                              (lambda (arg) 
                                (set! *module* arg)))

(front-end/add-simple-option! "Test Generation" "--latching"
  "Used when trap variables are latching, so only the final state of a test need be examined to find and remove trap variables that have become true."
  (lambda ()
    (set! *scan?* #f)))

(front-end/add-simple-option! "Test Generation" "--noprune"
  "Remove only the goal that generated the current test step (otherwise all trap variables that become true are removed); --mind should be nonzero when this option is used."
  (lambda ()
    (set! *prune?* #f) (set! *mind* (max 1 *mind*))))

(front-end/add-simple-option! "Test Generation" "--noslice"
  "Do not slice before starting a new test."
  (lambda ()
    (set! *slice?* #f)))

(front-end/add-simple-option! "Test Generation" "--simplenames"
  "Temporary fix to improve performance when trap variables are simple variables only."
  (lambda ()
    (set! *simplenames?* #t)))

(front-end/add-simple-option! "Test Generation" "--innerslice"
  "Slice before extending a test."
  (lambda ()
    (set! *innerslice?* #t)))

(front-end/add-simple-option! "Test Generation" "--branch"
  "Explore multiple extensions from initial segment."
  (lambda ()
    (set! *branch?* #t)))

(front-end/add-simple-option! "Test Generation" "--incremental"
  "Increase the search depth for initial and extension segments gradually."
  (lambda ()
    (set! *iincremental?* #t)(set! *eincremental?* #t)))

(front-end/add-simple-option! "Test Generation" "--incrinit"
  "Increase the search depth for initial segments gradually when using BMC."
  (lambda ()
    (set! *iincremental?* #t)))

(front-end/add-simple-option! "Test Generation" "--incrext"
  "Increase the search depth for extension segments gradually."
  (lambda ()
    (set! *eincremental?* #t)))

(front-end/add-simple-option! "Test Generation" "--smcinit"
  "Use SMC to start each new path (default uses BMC)."
  (lambda ()
    (unless *infbmc?* (set! *smcinit?* #t))))

(front-end/add-simple-option! "Test Generation" "--infbmc"
  "Use Infinite BMC (default is finite BMC)."
  (lambda ()
    (set! *infbmc?* #t) (set! *smcinit?* #f)))

(front-end/add-simple-option! "Test Generation" "--testpurpose"
  "Generate tests satisfying a testpurpose (defined by the list of goals \"purpose-list\")."
  (lambda ()
    (set! *purpose?* #t)))

(front-end/add-simple-option! "Test Generation" "--fullpath"
  "Output the complete state at each step of the path (default prints inputs only)."
  (lambda ()
    (set! *inputsonly?* #f)))

(front-end/add-full-option! 
 "Test Generation" 
 "-s <name>"
 "--solver=<name>"
 "Set SAT or SMT solver to be used (default: yices)."
 (lambda (arg)
   (set! *solver-id* (string->symbol arg))))

(front-end/add-full-option! 
 "Test Generation" 
 "-ed <num>"
 "--extend-depth=<num>"
 "Use BMC to depth `<num>' when extending tests (default: 8); 0 means do not attempt to extend tests."
 (front-end-adapter/nat-arg 
  (lambda (n)
    (set! *extendd* n))))

(front-end/add-full-option! 
 "Test Generation" 
 "-id <num>"
 "--init-depth=<num>"
 "Use search to depth `<num>' (default 8) when starting new test (0 means no limit for SMC)."
 (front-end-adapter/nat-arg 
  (lambda (n)
    (set! *initd* n))))

(front-end/add-full-option! 
 "Test Generation" 
 "-md <num>"
 "--min-depth=<num>"
 "Extensions must be of minimum length `<num>' (default 0)."
 (front-end-adapter/nat-arg 
  (lambda (n)
    (set! *mind* (if *prune?* n (max 1 n))))))

(front-end/set-categories! '("Help" "Misc" "Module" "Pretty Printing" "Test Generation" "BDD Interface" "Prioritized Traversal"  "Path Pretty Printing"))

(gen-front-end sal-atg
               "SAL Automated Test Generator"
"Usage: sal-atg [options] <context-name> <module-name>
   or  sal-atg [options] <file-name> <module-name>
   or  sal-atg [options] --module='<module-expr>'"
"Examples: 
  sal-atg peterson system

  sal-atg -v 1 trans_ga system trans_goals.scm --incremental
  
  sal-atg -v 3 stopwatch clock clock_goals.scm --smcinit -id 0 -ed 2"

(lambda (else)
  (if *main-context-name*
    (if *main-module-name*
      (begin 
        (print-error "Illegal argument `" else "'. Usage:")
        (sal-atg/simple-help))
      (set! *main-module-name* else))
    (set! *main-context-name* else)))
(begin
  (check-module-name-ref sal-atg/simple-help)
  (sal/set-sal-pp-proc! sal-ast->sal-doc)
  (unless *module*
    (load-context-if-file-name *main-context-name*))
  (let ((module-qualified-name (or *module*
                                   (string-append "(@ " *main-module-name* " " (file-name->sal-context-name *main-context-name*) ")"))))
    (guess-qualified-name-parser! module-qualified-name)
    (let ((module (if *infbmc?* (make-flat-module module-qualified-name)
		      (make-boolean-flat-module module-qualified-name))))
      (sal/set-make-sal-string-reader-proc! make-sal-string-reader)
      (multiple-value-bind (remgoals trace)
       (testgen module goal-list (cond (*purpose?* purpose-list)
               (else #f))
          *scan?* *prune?* 
          *slice?* *innerslice?* *branch?*
          *smcinit?* *iincremental?* *eincremental?* *initd* *extendd* *mind*)
       (verbose-message 1 "Arguments to sal-atg: ~a" *arguments*)
       (if (> (length trace) 0)
           (print (length trace) " tests generated; total length " 
            (count-tests trace))
           (print "No tests generated"))
       (if (> (length remgoals) 0)
           (print (length remgoals) " undischarged test goals:" 
            (map (lambda (g) (if (list? g) (conj g) g)) remgoals))
           (print "All test goals discharged."))
;      (print-full-tests trace #f)
       (print-tests trace)
)))))

(sal/set-pp-max-ribbon! 80)
(sal/set-pp-max-width! 100)
              
(sal-atg/main)
