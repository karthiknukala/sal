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

(define *max-states* 10)

(sal/set-trace-info-enabled! #f)
(sal-smc/enable-counter-examples! #f)

(define (print-deadlock-states fsm states)
  (print "Total number of deadlock states: " (bdd/num-solutions states (sal-bdd-fsm/num-non-choice-vars fsm)))
  (print "Deadlock states:")
  (sal-bdd-fsm/display-states-without-choices fsm states *max-states* #f))

(front-end/add-simple-option! "Module" "--module=<name>"
                              "Qualified module name using the LSAL syntax. Qualified module names are useful to reference modules in parametric contexts. Examples of qualified module names:\n(@ n-arbiter (arbiter () (10)))\n(@ system mutex)"
                              (lambda (arg) 
                                (set! *module* arg)))

(front-end/add-full-option! 
 "Deadlock Checker" 
 "-u <num>"
 "--max-deadlocks=<num>"
 "Maximum number of deadlock states to be printed (default: 10)."
 (front-end-adapter/nz-nat-arg 
  (lambda (arg) 
    (set! *max-states* arg))))

(front-end/set-categories! '("Help" "Misc" "Module" "Pretty Printing" "Code Transformations" "BDD Interface" "Deadlock Checker"))

(gen-front-end sal-deadlock-checker
               "SAL Deadlock Checker"
"Usage: sal-deadlock-checker [options] <context-name> <module-name>
   or  sal-deadlock-checker [options] <file-name> <module-name>
   or  sal-deadlock-checker [options] --module='<module-expr>'"
"Examples: 
  sal-deadlock-checker peterson system

  sal-deadlock-checker ~/tmp/peterson.sal system

  sal-deadlock-checker -v 3 peterson system

  sal-deadlock-checker -v 3 -u 5 peterson system

  sal-deadlock-checker --module='arbiter{10}!arbiter'"
(lambda (else)
  (if *main-context-name*
    (if *main-module-name*
      (begin 
        (print-error "Illegal argument `" else "'. Usage:")
        (sal-deadlock-checker/simple-help))
      (set! *main-module-name* else))
    (set! *main-context-name* else)))
(begin
  (check-module-name-ref sal-deadlock-checker/simple-help)
  (sal/set-make-sal-string-reader-proc! make-lsal-string-reader)
  (unless *module*
    (load-context-if-file-name *main-context-name*))
  (let ((module-qualified-name (mk-qualified-name *module* *main-context-name* *main-module-name*)))
    (guess-qualified-name-parser! module-qualified-name)
    (let* ((fsm (make-bdd-fsm module-qualified-name))
           (deadlock-states (sal-bdd-fsm/deadlock-states fsm)))
      (cond 
       ((bdd/false? deadlock-states)
        (print "ok (module does NOT contain deadlock states)."))
       (else
        (print-deadlock-states fsm deadlock-states)))))))

(sal-deadlock-checker/main)
