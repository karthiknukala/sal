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

(module sal-parser-front-end
        (include "utility.macros")
        (import utility wttree symbol-table scmobj api trace front-end sxml-package
                xformat gmp-scheme sal-sxml-support front-end sal-version queue sal-environment)
        (main sal-parser-main))

(define (sal-file? arg)
  (equal? (suffix arg) "sal"))

(define *output-file* #f)
(define *xml-preamble?* #f)

(front-end/add-full-option! 
 "Parser"
 "-o <name>"
 "--output=<name>"
 "Send the XML code to the specified file."
 (lambda (arg)
   (set! *output-file* arg)))

(front-end/add-full-option!
 "Parser"
 "-f"
 "--full"
 "Include the XML preamble in the generated code."
 (lambda ()
   (set! *xml-preamble?* #t)))

(front-end/set-categories! '("Help" "Misc" "Parser"))

(define (sal-parser-main argv)
  (let ((verbosity 0)
        (sal-file #f))
    (unless (front-end/parse-args (cdr argv)
                                  (lambda (arg)
                                    (cond
                                     (sal-file
                                      (sign-error "A SAL file (\"~a\") was already specified." sal-file))
                                     (else
                                      (set! sal-file arg)))))
      (sal-parser/help))
    (unless sal-file
      (sal-parser/help))
    (let ((sal-env (make-sal-env)))
      (verbose-message 1 "parsing `~a'..." sal-file)
      (let ((output-port (if *output-file*
                           (try (open-output-file *output-file*)
                                (lambda (e p m o)
                                  (sign-error "Failed to create output file \"~a\"." *output-file*)))
                           (current-output-port))))
        (with-output-to-port output-port
          (lambda ()
            (when *xml-preamble?*
              (print "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
              (print "<!DOCTYPE CONTEXT SYSTEM \"sal.dtd\">")
              (print "<!-- XML version of " sal-file " -->"))
            (sal->xml sal-file sal-env)))))))
  
(define (sal-parser/help)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print (sal/header "SAL Parser"))
      (print "Usage: sal-parser [options] file-name")
      (front-end/show-options)
      (newline)))
  (exit -1))
  
(front-end/set-help-proc! sal-parser/help)  
