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

(define *module* #f)
(define *main-module-name* #f)
(define *max-states* 10)

(sal/set-trace-info-enabled! #f)
(sal-smc/enable-counter-examples! #f)

(define (check-module-name-ref help-proc)
  (when (and *module* (or *main-context-name* *main-module-name*))
    (print-error "Invalid combination of arguments, you must not specify context and module names when the option --module is used.")
    (help-proc)
    (exit -1))
  (unless (or *module* (and *main-context-name* *main-module-name*))
    (print-error "A module must be specified.")
    (help-proc)
    (exit -1)))

(define (print-invalid-states fsm states)
  (print "Total number of invalid states: " (bdd/num-solutions states (sal-bdd-fsm/num-non-choice-vars fsm)))
  (print "Invalid states:")
  (sal-bdd-fsm/display-states-without-choices fsm states *max-states* #f))

(front-end/add-simple-option! "Module" "--module=<name>"
                              "Qualified module name using the LSAL syntax. Qualified module names are useful to reference modules in parametric contexts. Examples of qualified module names:\n(@ n-arbiter (arbiter () (10)))\n(@ system mutex)"
                              (lambda (arg) 
                                (set! *module* arg)))

(front-end/add-full-option! 
 "Invalid State Detector"
 "-u <num>"
 "--max-invalid-states=<num>"
 "Maximum number of invalid states to be printed (default: 10)."
 (front-end-adapter/nz-nat-arg 
  (lambda (arg) 
    (set! *max-states* arg))))

(front-end/set-categories! '("Help" "Misc" "Module" "Pretty Printing" "Code Transformations" "BDD Interface" "Invalid State Detector"))

(gen-front-end sal-invalid-state-detector
               "SAL Invalid State Detector"
               "Usage: sal-invalid-state-detector [options] <context-name> <module-name>"
"Examples:
  sal-invalid-state-detector peterson system
 
  sal-invalid-state-detector -v 3 peterson system
 
  sal-invalid-state-detector -v 3 -u 5 peterson system
 
  sal-invalid-state-detector --module=\"(@ n-arbiter (arbiter () (10)))\""
(lambda (else)
  (if *main-context-name*
    (if *main-module-name*
      (begin 
        (print-error "Illegal argument `" else "'. Usage:")
        (sal-deadlock-checker/simple-help))
      (set! *main-module-name* else))
    (set! *main-context-name* else)))
(begin
  (check-module-name-ref sal-invalid-state-detector/simple-help)
  (sal/set-make-sal-string-reader-proc! make-lsal-string-reader)
  (let* ((module-qualified-name (or *module*
                                    (string-append "(@ " *main-module-name* " " *main-context-name* ")")))
         (fsm (make-bdd-fsm module-qualified-name))
         (rs (sal-bdd-fsm/reachable-states fsm))
         (invalid-states (bdd/and rs (bdd/not (slot-value fsm :valid-latch)))))
    (cond 
     ((bdd/false? invalid-states)
      (print "ok (module does NOT contain invalid states)."))
     (else
      (print-invalid-states fsm invalid-states))))))

(sal-invalid-state-detector/main)
