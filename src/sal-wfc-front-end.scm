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

(define *main-context* #f)

(front-end/set-categories! '("Help" "Misc"))

(gen-front-end sal-wfc
               "SAL Well-Formedness Checker"
"Usage: sal-wfc [options] [context-name]
   or  sal-wfc [options] [file-name]"
               "Examples: 
  sal-wfc peterson

  sal-wfc ~/examples/peterson.sal

  sal-wfc ../tmp/peterson.sal

  sal-wfc -v 3 peterson

  sal-wfc -v 3 ~/examples/peterson.sal
"
              (lambda (else)
                (if *main-context*
                  (begin 
                    (print "Illegal argument `" else "'. Usage:")
                    (sal-wfc/simple-help))
                  (set! *main-context* else)))
              (begin
                (unless *main-context*
                  (sal-wfc/simple-help)
                  (exit -1))
                (let ((ctx-name (file-name->sal-context-name *main-context*)))
                  (if (equal? ctx-name *main-context*)
                    (sal-env/context *sal-env* (string->symbol *main-context*))
                    (sal-env/context-from-file *sal-env* *main-context*)))
                (print "Ok.")))

(sal-wfc/main)

      

    
