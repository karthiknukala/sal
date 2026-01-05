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

(define *exec-dotty* #f)
(define *buchi-output-file-name* #f)
(define *buchi-pos* #f)

(front-end/add-full-option!
 "LTL Support"
 "-dotty"
 "--exec-dotty"
 "Execute dotty after generating the Buchi Automata."
 (lambda ()
   (set! *exec-dotty* #t)))

(front-end/add-full-option!
 "LTL Support"
 "-o <file-name>"
 "--output=<file-name>"
 "File name that will contain the Buchi Automata DOT description."
 (lambda (file-name)
   (set! *buchi-output-file-name* file-name)))

(front-end/add-full-option!
 "LTL Support"
 "-lpos"
 "--ltl-pos"
 "The default behavior of ltl2buchi is to build a monitor (Buchi Automata) for the negation of the property. This option forces ltl2buchi to create a Buchi Automata for the property."
 (lambda ()
   (set! *buchi-pos* #t)))

(front-end/set-categories! '("Help" "Misc" "Assertion" "LTL Support"))

(gen-front-end ltl2buchi
               "SAL LTL to Buchi Automata translator/viewer"
"Usage: ltl2buchi [options] <context-name> <assertion-name>
   or  ltl2buchi [options] <file-name> <assertion-name>
   or  ltl2buchi [options] --assertion='<assertion-expr>'"
"Examples:
   ltl2buchi peterson liveness1

   ltl2buchi -v 3 peterson liveness1

   ltl2buchi peterson.sal liveness2

   ltl2buchi -dbo peterson.sal liveness2

   ltl2buchi -dotty peterson.sal liveness2"
(lambda (else)
  (if *main-context-name*
    (if *main-assertion-name*
      (begin 
        (print-error "Illegal argument `" else "'. Usage:")
        (ltl2buchi/simple-help))
      (set! *main-assertion-name* else))
    (set! *main-context-name* else)))
(begin
  (check-assertion-name-ref ltl2buchi/simple-help)
  (sal/set-make-sal-string-reader-proc! make-lsal-string-reader)
  (unless *assertion*
    (load-context-if-file-name *main-context-name*))
  (let ((assertion-qualified-name (mk-qualified-name *assertion* *main-context-name* *main-assertion-name*)))
    (guess-qualified-name-parser! assertion-qualified-name)
    (let* ((assertion1 (sal-assertion-name/definition (sal/assertion-name assertion-qualified-name)))
           (assertion2 (sal/simplify assertion1))
           (assertion  (sal-ast/expand-quantifiers assertion2)))
      (unless (instance-of? assertion <sal-module-models>)
        (print-error "Only simple assertions (MODULE-MODELS) are supported in the current version.")
        (exit -1))
      (unless (sal-module-models/ltl-property? assertion)
        (print-error "The assertion is not a valid LTL property.")
        (exit -1))
      (when (and *exec-dotty* (not *buchi-output-file-name*))
        (set! *buchi-output-file-name* *sal-dot-tmp-file*))
      (let ((output-port (if *buchi-output-file-name*
                           (open-output-file *buchi-output-file-name*)
                           (current-output-port))))
        (sal-module-models/ltl->dot assertion output-port *buchi-pos*)
        (unless (eq? output-port (current-output-port))
          (close-output-port output-port))
        (when *exec-dotty*
          (dot/show *buchi-output-file-name*)))))))

(ltl2buchi/main)
