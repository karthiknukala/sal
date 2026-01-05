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

(front-end/set-categories! '("Help" "Misc" "Assertion" "Pretty Printing" "Code Transformations" "Abstraction" "Traceability" "Path Pretty Printing" "LTL Support" "BDD Interface"))

(define (prove-ctl assertion)
  (let* ((flat-module (slot-value assertion :module))
         (_ (sal-module/display-var-info flat-module))
         (fsm (sal-flat-module->sal-bdd-fsm flat-module))
         (witness-or-counterexample (sal-wmc/ctl-check fsm (slot-value assertion :expr))))
    (if (instance-of? witness-or-counterexample <wmc-witness>)
        (print "VALID.")
        (print "INVALID."))
    (sal-wmc/display-trace witness-or-counterexample)))

(define (to-ctl assertion)
  (if (sal-module-models/ctl-property? assertion)
    assertion
    (try
     (let* ((property (slot-value assertion :expr))
            (ctl-property (ltl->ctl property)))
       (copy-instance assertion :expr ctl-property))
     (catch 'ltl->ctl-failure
            (lambda (msg)
              (print-error "Failed to convert LTL property to CTL. Use sal-smc to check the property.")
              (exit -1))))))
            
(gen-front-end sal-wmc
               "SAL Witness Counterexample Based Symbolic Model Checker"
"Usage: sal-wmc [options] <context-name> <assertion-name>
   or  sal-wmc [options] <file-name> <assertion-name>
   or  sal-wmc [options] --assertion='<assertion-expr>'"
"Examples: 
  sal-wmc peterson mutex_ctl

  sal-wmc ../tmp/peterson.sal mutex_ctl

  sal-wmc --verbose=3 peterson mutex_ctl

  sal-wmc -v 3 peterson mutex_ctl

  sal-wmc --assertion=\"(@ mutex_ctl peterson)\"
"
(lambda (else)
  (if *main-context-name*
    (if *main-assertion-name*
      (begin 
        (print-error "Illegal argument `" else "'. Usage:")
        (sal-wmc/simple-help))
      (set! *main-assertion-name* else))
    (set! *main-context-name* else)))
(begin
  (check-assertion-name-ref sal-wmc/simple-help)
  (sal/set-make-sal-string-reader-proc! make-lsal-string-reader)
  (unless *assertion*
    (load-context-if-file-name *main-context-name*))
  (let* ((assertion-qualified-name (mk-qualified-name *assertion* *main-context-name* *main-assertion-name*)))
    (guess-qualified-name-parser! assertion-qualified-name)
    (let ((assertion (make-boolean-assertion assertion-qualified-name)))
      (check-if-supported-assertion assertion)
      (check-if-trivial-assertion assertion)
      (unless (instance-of? assertion <sal-module-models>)
        (print-error "Only simple assertions (MODULE-MODELS) are supported in the current version.")
        (exit -1))
      (prove-ctl (to-ctl assertion))))))

(sal-wmc/main)
