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

(define *output-file* #f)

(front-end/add-simple-option! "Misc" "--output=<file-name>"
                              "Sets the output file that will contain the result."
                              (lambda (arg) 
                                (set! *output-file* arg)))

(front-end/set-categories! '("Help" "Misc" "Assertion" "Pretty Printing" "Code Transformations" "Abstraction" "Traceability"))


(gen-front-end sal2bool
               "SAL boolean transition relation generator"
"Usage: sal2bool [options] <context-name> <assertion-name>
   or  sal2bool [options] <file-name> <assertion-name>
   or  sal2bool [options] --assertion='<assertion-expr>'"
"Examples:
  sal2bool peterson mutex

  sal2bool ~/tmp/peterson.sal mutex

  sal2bool --assertion='arbiter{10}!at_most_one_ack'"
(lambda (else)
  (if *main-context-name*
    (if *main-assertion-name*
      (begin 
        (print "Illegal argument `" else "'. Usage:")
        (sal2bool/simple-help))
      (set! *main-assertion-name* else))
    (set! *main-context-name* else)))
(begin
  (check-assertion-name-ref sal2bool/simple-help)
  (sal/set-make-sal-string-reader-proc! make-lsal-string-reader)
  (unless *assertion*
    (load-context-if-file-name *main-context-name*))
  (let ((assertion-qualified-name (mk-qualified-name *assertion* *main-context-name* *main-assertion-name*)))
    (guess-qualified-name-parser! assertion-qualified-name)
    (let ((assertion (sal-ast/rename-variables (make-boolean-assertion assertion-qualified-name))))
      (if *output-file*
        (with-output-to-file *output-file* (lambda () (pp (sal-ast->list assertion))))
        (pp (sal-ast->list assertion)))))))

(sal2bool/main)
