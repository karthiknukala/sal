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

(front-end/add-simple-option! "Module" "--module=<name>"
                              "Qualified module name. Qualified module names are useful to reference modules in parametric contexts."
                              (lambda (arg) 
                                (set! *module* arg)))
(define *sal-user-function* #f)

(front-end/add-full-option!
 "Path Explorer"
 "-w <name>"
 "--weight-function=<name>"
 "A SAL function name that will compute a weight for each visited state. The state with highest weight is selected to continue the simulation. Ties are broken using random numbers."
 (lambda (arg)
   (sal-path-explorer/enable-guided! #t)
   (set! *sal-user-function* (to-symbol arg))))


(front-end/set-categories! '("Help" "Misc" "Module" "Pretty Printing" "Traceability" 
                             "Path Pretty Printing" "LTL Support" "Dynamic Compilation" "Explicit State" "Path Explorer"))

(gen-front-end sal-path-explorer
               "SAL Path Explorer"
"Usage: sal-path-explorer [options] <context-name> <module-name>
   or  sal-path-explorer [options] <file-name> <module-name>
   or  sal-path-explorer [options] --module='<module-name>'"
"Examples: 
  sal-path-explorer peterson system

  sal-path-explorer -v 3 -d 50 -n 10 peterson system

  sal-path-explorer -v 3 -k -g -d 50 -n 100 peterson system

  sal-path-explorer -v 3 -d 50 -n 100 --module='peterson!mutex'

  sal-path-explorer --module='arbiter{3}!arbiter'"
(lambda (else)
  (if *main-context-name*
    (if *main-module-name*
      (begin 
        (print-error "Illegal argument `" else "'. Usage:")
        (sal-path-explorer/simple-help))
      (set! *main-module-name* else))
    (set! *main-context-name* else)))
(begin
  (check-module-name-ref sal-path-explorer/simple-help)
  (sal/set-make-sal-string-reader-proc! make-lsal-string-reader)
  (unless *module*
    (load-context-if-file-name *main-context-name*))
  (let* ((module-qualified-name (mk-qualified-name *module* *main-context-name* *main-module-name*))
         (_ (guess-qualified-name-parser! module-qualified-name))
         (module (sal/module module-qualified-name))
         (function (if *sal-user-function*
                     (make-qualified-name-expr-based-on *sal-user-function* (slot-value module :module-name))
                     #f)))  
    (sal-esm/set-state-weight-function! function)
    (sal-esmc/simulate module))))

(sal-path-explorer/main)

