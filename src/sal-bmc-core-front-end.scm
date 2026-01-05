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

(define *from-depth* 0)
(define *to-depth* 10)
(define *iterative?* #f) 
(define *acyclic?* #f)
(define *induction?* #f)
(define *lemmas* '())
(define *display-induction-ce* #f)
(define *solver-id* 'yices)

(front-end/add-full-option! 
 "Verification" 
 "-d <num>"
 "--depth=<num>"
 "Search for counterexamples in the first <num> steps (default: 10)."
 (front-end-adapter/nat-arg 
  (lambda (n)
    (set! *from-depth* 0)
    (set! *to-depth* n))))

(front-end/add-simple-option! 
 "Verification" 
 "--from=<num>"
 "Start the search at depth <num> (default: 0). This option is only available to check safety properties."
 (front-end-adapter/nat-arg 
  (lambda (n)
    (set! *from-depth* n))))

(front-end/add-simple-option! 
 "Verification" 
 "--to=<num>"
 "Search for counterexamples up to depth <num> (default: 10)."
 (front-end-adapter/nat-arg 
  (lambda (n)
    (set! *to-depth* n))))

(front-end/add-simple-option! 
 "Verification" 
 "--acyclic"
 "Consider only acyclic paths."
 (lambda ()
   (set! *acyclic?* #t)))

(front-end/add-full-option! 
 "Verification" 
 "-i"
 "--induction"
 "Use the k-induction rule to prove the property. This option can only be used to prove safety properties."
 (lambda ()
   (set! *induction?* #t)))

(front-end/add-full-option! 
 "Verification" 
 "-it"
 "--iterative"
 "Use iterative deepening, that is, a sequence of SAT problems is fired, until a counterexample is detected, or the search range is covered."
 (lambda ()
   (set! *iterative?* #t)))

(front-end/add-full-option! 
 "Verification" 
 "-l <name>"
 "--lemma=<name>"
 "Lemma to be used in the induction step. '<name>' is a name, or a qualified assertion name using the LSAL syntax.Consider only acyclic paths."
 (lambda (name)
   (push! name *lemmas*)))

(front-end/add-full-option! 
 "Verification" 
 "-ice"
 "--display-induction-ce"
 "Display counterexample for induction step."
 (lambda ()
   (set! *display-induction-ce* #t)))

(front-end/add-full-option! 
 "Verification" 
 "-s <name>"
 "--solver=<name>"
 "Set sat solver to be used (default: ics)."
 (lambda (arg)
   (set! *solver-id* (string->symbol arg))))

(define *skolemize* #f)

(front-end/add-full-option! 
 "Verification" 
 "-sk"
 "--skolemize"
 "Try to skolemize quantified variables whenever possible."
 (lambda ()
   (set! *skolemize* #t)))

(gen-front-end sal-bmc *bmc-name* *bmc-usage* *bmc-examples*
(lambda (else)
  (if *main-context-name*
    (if *main-assertion-name*
      (begin 
        (print-error "Illegal argument `" else "'. Usage:")
        (sal-bmc/simple-help))
      (set! *main-assertion-name* else))
    (set! *main-context-name* else)))
(begin
  (check-assertion-name-ref sal-bmc/simple-help)
  (when (and *induction?* (not (= *from-depth* 0)))
    (warning-message "Forcing initial depth to be 0, since the user requested the induction rule.")
    (set! *from-depth* 0))
  (unless (or *induction?* (null? *lemmas*))
    (sign-error "Lemmas can be used only in proofs by induction (i.e., option --induction is used)."))
  (unless (and (<= *from-depth* *to-depth*) (>= *from-depth* 0))
    (print-error "Invalid search range [" *from-depth* ", " *to-depth* "]")
    (sal-bmc/simple-help))
  (sal/set-make-sal-string-reader-proc! make-lsal-string-reader)
  (unless *assertion*
    (load-context-if-file-name *main-context-name*))
  (let ((assertion-qualified-name-str (mk-qualified-name *assertion* *main-context-name* *main-assertion-name*)))
    (guess-qualified-name-parser! assertion-qualified-name-str)
    (let* ((assertion (bmc/make-assertion assertion-qualified-name-str))
           (assertion-qualified-name (sal/assertion-name assertion-qualified-name-str))
           (lemma-exprs (bmc/make-lemmas *lemmas* assertion-qualified-name assertion)))
      (check-if-supported-assertion assertion)
      (unless (or (sal-module-models/invariant? assertion)
                  (sal-module-models/accepting? assertion))
        (sign-error "SAL BMC supports only LTL properties (and CTL properties which can be converted to LTL)."))
      (check-if-trivial-assertion assertion)
      (when (and (not (sal-module-models/invariant? assertion))
                 *induction?*)
        (warning-message "The option --induction can only be used to prove safety properties.")
        (set! *induction?* #f))
      (sal-module/display-var-info (slot-value assertion :module))
      (multiple-value-bind
          (valid? counter-example)
          (cond 
           ((sal-module-models/invariant? assertion)
            (bmc/invariant assertion))
           (else
            (bmc/liveness assertion)))
        (cond
         (valid?
          (if (not *induction?*)
            (print "no counterexample between depths: [" *from-depth* ", " *to-depth* "].")
            (multiple-value-bind
                (proved? counter-example)
                (bmc/k-induction assertion lemma-exprs)
              (cond
               (proved?
                (print "proved."))
               (else
                (print "k-induction rule failed, please try to increase the depth.")
                (when (and *display-induction-ce* (not (eq? counter-example #unspecified)))
                  (let ((counter-example (sal-derived-path->original-path counter-example)))
                    (print-counter-example counter-example))))))))
         ((eq? counter-example #unspecified)
          (print "invalid."))
         (else
          (let ((counter-example (sal-derived-path->original-path counter-example)))
            (print-counter-example counter-example)))))))))
