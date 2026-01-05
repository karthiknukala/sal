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

(define *assertion* #f)
(define *main-context-name* #f)
(define *main-assertion-name* #f)

(front-end/add-simple-option! "Assertion" "--assertion=<name>"
                              "Qualified assertion nam. Qualified assertion names are useful to reference properties in parametric contexts. Examples of qualified assertion names:\narbiter{50}!at-most-one-ack\nmutex!invalid"
                              (lambda (arg) (set! *assertion* arg)))

(define (print-error . args)
  (with-output-to-port (current-error-port)
    (lambda ()
      (apply print "Error: " args))))

(define (print-path-core path)
  (sal-path/pp path))

(define (print-path path)
  (print-path-core path))

(define (print-counter-example counter-example)
  (print "Counterexample:")
  (print-path-core counter-example))

(define (print-deadlock-path deadlock)
  (print "Deadlock:")
  (print-path-core deadlock))

(define (check-if-supported-assertion assertion)
  (unless (instance-of? assertion <sal-module-models>)
    (print-error "Only simple assertions (MODULE-MODELS) are supported in the current version.")
    (exit -1)))

(define (check-assertion-name-ref help-proc)
  (when (and *assertion* (or *main-context-name* *main-assertion-name*))
    (print-error "Invalid combination of arguments, you must not specify context and assertion names when the option --assertion is used.")
    (help-proc))
  (unless (or *assertion* (and *main-context-name* *main-assertion-name*))
    (print-error "An assertion must be specified.")
    (help-proc)))

(define (check-assertion-name-ref-or-context help-proc)
  (when (and *assertion* (or *main-context-name* *main-assertion-name*))
    (print-error "Invalid combination of arguments, you must not specify context and assertion names when the option --assertion is used.")
    (help-proc))
  (unless (or *assertion* *main-context-name*)
    (print-error "An assertion or context must be specified.")
    (help-proc)))
  
(define (check-if-trivial-assertion assertion)
  (when (sal-module-models/trivially-true? assertion)
    (warning-message "The property is trivially true.")
    (print "proved.")
    (exit 0))
  (when (sal-module-models/trivially-false? assertion)
    (warning-message "The property is trivially false.")
    (print "invalid.")
    (exit 0)))

(define (guess-qualified-name-parser! qualified-name)
  (sal/set-make-sal-string-reader-proc! (guess-qualified-name-reader qualified-name)))

(define (load-context-if-file-name context)
  (let ((ctx-name (file-name->sal-context-name context)))
    (unless (equal? context ctx-name)
      (sal-env/import-context-from-file! *sal-env* context))))

(define (load-context context)
  (let ((ctx-name (file-name->sal-context-name context)))
    (if (equal? context ctx-name)
      (sal-env/import-context! *sal-env* context)
      (sal-env/import-context-from-file! *sal-env* context))))

(define *module* #f)
(define *main-module-name* #f)

(define (check-module-name-ref help-proc)
  (when (and *module* (or *main-context-name* *main-module-name*))
    (print-error "Invalid combination of arguments, you must not specify context and module names when the option --module is used.")
    (help-proc)
    (exit -1))
  (unless (or *module* (and *main-context-name* *main-module-name*))
    (print-error "A module must be specified.")
    (help-proc)
    (exit -1)))

(define (mk-qualified-name qualified-name context-name simple-name)
  (if qualified-name
    qualified-name
    (let ((context-name (file-name->sal-context-name context-name)))
      (if (or (char-pos #\- context-name) (char-pos #\- simple-name))
        (string-append "(@ " simple-name " " context-name ")")
        (string-append context-name "!" simple-name)))))

