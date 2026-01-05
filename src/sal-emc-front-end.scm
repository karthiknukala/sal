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

(define *sal-user-function* #f)

(front-end/add-full-option!
 "ESMC"
 "-w <name>"
 "--weight-function=<name>"
 "A SAL function name that will compute a weight for each visited state. The state with highest weight is selected to continue the verification."
 (lambda (arg)
   (set! *sal-user-function* (to-symbol arg))))


(front-end/set-categories! '("Help" "Misc" "Assertion" "Pretty Printing" "Traceability" 
                             "Path Pretty Printing" "LTL Support" "Dynamic Compilation" "Explicit State" "ESMC"))

(define (prove-invariant assertion)
  (print-error "Not implemented yet.")
  (exit -1))

(define (prove-ltl-liveness assertion)
  (print-error "Not implemented yet.")
  (exit -1))

(gen-front-end sal-emc
               "SAL Explicit State Model Checker"
"Usage: sal-emc [options] <context-name> <assertion-name>
   or  sal-emc [options] <file-name> <assertion-name>
   or  sal-emc [options] --assertion='<assertion-expr>'"
"Examples: 
  sal-emc peterson mutex

  sal-emc -s bfs peterson mutex

  sal-emc -s cacheless -d 100 fourslot_with_bug mutex

  sal-emc peterson invalid

  sal-emc ../tmp/peterson.sal invalid

  sal-emc ../tmp/peterson.sal

  sal-emc ~/examples/peterson.sal mutex

  sal-emc --verbose=3 peterson mutex

  sal-emc -v 3 peterson mutex

  sal-emc -v 3 ~/examples/peterson.sal mutex

  sal-emc --assertion='peterson!mutex'

  sal-emc --assertion='arbiter{3}!at_most_one_ack'

  sal-emc --disable-traceability peterson invalid

  sal-emc --delta-path peterson invalid"
(lambda (else)
  (if *main-context-name*
    (if *main-assertion-name*
      (begin 
        (print-error "Illegal argument `" else "'. Usage:")
        (sal-emc/simple-help))
      (set! *main-assertion-name* else))
    (set! *main-context-name* else)))
(begin
  (check-assertion-name-ref sal-emc/simple-help)
  (sal/set-make-sal-string-reader-proc! make-lsal-string-reader)
  (unless *assertion*
    (load-context-if-file-name *main-context-name*))
  (let* ((assertion-qualified-name (mk-qualified-name *assertion* *main-context-name* *main-assertion-name*))
         (_ (guess-qualified-name-parser! assertion-qualified-name))
         (assertion (sal/assertion-name assertion-qualified-name))
         (function (if *sal-user-function*
                     (make-qualified-name-expr-based-on *sal-user-function* assertion)
                     #f)))  
    (sal-esm/set-state-weight-function! function)
    (let ((result (sal-esmc/verify assertion)))
      (cond
       ((eq? result 'valid)
        (print "proved."))
       ((eq? result 'unknown)
        (print "no counterexample found."))
       ((eq? result 'invalid)
        (print "invalid."))
       (else
        [assert (result) (pair? result)]
        ;; (breakpoint "sal-esmc" (result) #t)
        (case (car result)
          ((deadlock)
           (print-deadlock-path (cdr result)))
          ((counterexample)
           (print-counter-example (cdr result)))
          (else
           (internal-error)))))))))

(sal-emc/main)

