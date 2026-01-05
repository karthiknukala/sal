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

(define *depth* 10)
(define *acyclic?* #f)
(define *solver-id* 'yices)

(front-end/add-simple-option! "Module" "--module=<name>"
                              "Qualified module name. Qualified module names are useful to reference modules in parametric contexts."
                              (lambda (arg) 
                                (set! *module* arg)))
(front-end/add-full-option! 
 "Path Search" 
 "-d <num>"
 "--depth=<num>"
 "Search for a path of length `<num>' (default: 10)."
 (front-end-adapter/nz-nat-arg 
  (lambda (n)
    (set! *depth* n))))

(front-end/add-simple-option! "Path Search" "--acyclic"
                              "Consider only acyclic paths."
                              (lambda ()
                                (set! *acyclic?* #t)))

(front-end/add-full-option! 
 "Path Search" 
 "-s <name>"
 "--solver=<name>"
 "Set sat solver to be used (default: yices)."
 (lambda (arg)
   (set! *solver-id* (string->symbol arg))))

(front-end/set-categories! '("Help" "Misc" "Module" "Pretty Printing" "Code Transformations" "Abstraction" "Traceability" "Path Pretty Printing" "Path Search"))

(gen-front-end sal-path-finder
               "SAL Path Finder"
"Usage: sal-path-finder [options] <context-name> <module-name>
   or  sal-path-finder [options] <file-name> <module-name>
   or  sal-path-finder [options] --module='<module-expr>'"
"Examples: 
  sal-path-finder peterson system

  sal-path-finder ~/tmp/peterson.sal system
  
  sal-path-finder --depth=20 four-slot system
  
  sal-path-finder --verbose=3 peterson system
  
  sal-path-finder -v 3 peterson system

  sal-path-finder -v 3 ../examples/peterson.sal system

  sal-path-finder -v 3 --module='arbiter{10}!arbiter'
  
  sal-path-finder --disable-traceability peterson system
  
  sal-path-finder --full-trace peterson system"
(lambda (else)
  (if *main-context-name*
    (if *main-module-name*
      (begin 
        (print-error "Illegal argument `" else "'. Usage:")
        (sal-path-finder/simple-help))
      (set! *main-module-name* else))
    (set! *main-context-name* else)))
(begin
  (check-module-name-ref sal-path-finder/simple-help)
  (sal/set-make-sal-string-reader-proc! make-lsal-string-reader)
  (unless *module*
    (load-context-if-file-name *main-context-name*))
  (let ((module-qualified-name (mk-qualified-name *module* *main-context-name* *main-module-name*)))
    (guess-qualified-name-parser! module-qualified-name)
    (let* ((module (make-boolean-flat-module module-qualified-name))
           (path (sal-bmc/find-trace module *depth* *solver-id* *acyclic?*)))
      (cond
       ((not path)
        (print "no path found."))
       ((eq? path #unspecified)
        (print "path was found, but it couldn't be generated."))
       (else
        (let ((path (sal-derived-path->original-path path)))
          (print-path path))))))))

(sal-path-finder/main)
             
              
             

              
