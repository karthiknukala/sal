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

;;
;; BD: Added comments. This is the top-level salenv module.
;; 
;; When salenv is run, the main function 'salenv-main' in this module
;; is executed with a single parameter equal to the command line arguments.
;;
;; If the command line contains the name of a script file (in scheme)
;; then that file is loaded and executed. Otherwise, salenv-main runs
;; its own read-eval-print loop.
;; 
;; If command line argument --slave-mode is given, then global variable
;; *slave-mode* is set to true (it's false by default). In slave mode,
;; salenv prints outputs in a verbose format with '@@@...@@@' everywhere.
;; That's intended to help other tools interact with salenv using pipes.
;;

(module salenv
        (include "scmobj.macros")
        (include "utility.macros")
        (include "trace.macros")
        (include "api.macros")
        (include "salenv-api.sch")
        (export (help . args)
                (show-commands prefix)
                (apropos regex)
                *help-message*
                *sal-trace-stack-depth*
                (sal/repl-error-notifier proc msg obj)
                (sal/interrupt-notifier sig)
                (sal/repl-printer obj . port)
                (sal/load-source! src-file-name)
                (sal/enable-trace-stack! flag))
        (main salenv-main)
        (eval (export-all)))


(define *slave-mode* #f)

(front-end/add-simple-option!
 "SALenv"
 "--slave-mode"
 "Enable read-eval-print-loop that is suitable for interacting with other programs using pipes."
 (lambda ()
   (set! *slave-mode* #t)))



(define *sal-trace-stack-depth* 50)

(define *sal-trace-stack?* #f)

(define *sal-trace-stack-remark-displayed?* #f)

(define-api (sal/enable-trace-stack! (flag boolean?))
  (set! *sal-trace-stack?* flag))
  


;;
;; Functions used by salenv's read-eval-print loop:
;; - sal/repl-printer: print results
;; - sal/interrupt-notifier: to catch Unix signals
;; - sal/repl-error-notifier: not used anymore in Bigloo 
;;   but there are explicit calls to this function here (and it's exported).
;;


;; NOTE: app-error? is defined in utility.scm

(define (sal/detailed-error-msg proc msg obj)
  (let ((port (current-error-port)))
    (flush-output-port (current-output-port))
    (newline port)
    (unless (app-error? proc)
      (display "Error at \"" port)
      (display-circle proc port)
      (display #"\": " port))
    (display-circle msg port)
    (unless (eq? obj #unspecified)
      (print "")
      (sal/pp obj))
    (print "\n")
    (cond
     (*sal-trace-stack?*
      (dump-trace-stack (current-output-port) *sal-trace-stack-depth*))
     ((not *sal-trace-stack-remark-displayed?*)
      (print "Remark: the command '(sal/enable-trace-stack! #t)' can be used to force SAL to print the call/trace stack when an error happens.")
      (set! *sal-trace-stack-remark-displayed?* #t)))
    (flush-output-port port)))

(define (sal/repl-error-notifier proc msg obj)
  (cond
   (*slave-mode*
    (print "\n@@@BEGIN_ERROR_MSG@@@")
    (print msg)
    (print "@@@END_ERROR_MSG@@@"))
   ((and (app-error? proc) (not (sal-check-mode)))
    (with-output-to-port (current-error-port)
      (lambda ()
        (print "Error: " msg " " obj))))
   (else
    (sal/detailed-error-msg proc msg obj))))

;;
;; catch interrupts and print a message
;;
(define (sal/interrupt-notifier sig)
  (cond
   (*slave-mode*
    (print "\n@@@INTERRUPTED@@@")
    (flush-output-port (current-output-port)))
   (else
    (let ((port (current-error-port)))
      (newline port)
      (fprint port ">>> SALenv was INTERRUPTED!")
      (flush-output-port port)))))

;;
;; BD: in non-slave mode. 
;; !, !!, !!! store the last result, the previous result, and the one before that, respectively.
;; dynamic-define is in utility.scm. Not sure why it's used here?
;;
(define (sal/repl-printer obj . port)
  (unless *slave-mode*
    (eval '(set! !!! !!))
    (eval '(set! !! !))
    (dynamic-define '! obj))
  (try
   (with-output-to-port (if (null? port) (current-output-port) (car port))
     (lambda ()
       (sal/pp obj)
       (when *slave-mode*
         (print "\n@@@END_OF_CMD@@@"))))
   (lambda (escape proc msg obj)
     (if *slave-mode*
       (print "@@@PP_ERROR@@@")
       (with-output-to-port (current-error-port)
         (lambda ()
           (print "Error printing object...")
;;
;; THIS ERROR STUFF SEEMS TO CHANGE WITH EVERY VERSION OF BIGLOO
;;	   (error-notify (make-&error #f #f (get-trace-stack) proc msg obj)))))

	   (error-notify (mk-error proc msg obj)))))
     (escape #unspecified))))




;;=====================================================================================
;; Online help
;; - all api functions should be documented using the (define-api ... ) macros
;; - The following functions display the documentation attached to each api function.
;;=====================================================================================

(define *help-message*
  "-  (help <prefix>) print information about all procedures/classes whose name starts with <prefix>.
-  (show-commands <prefix>) print the name of all procedures/classes whose name starts with <prefix>.
-  (apropos <rexpr>) show all procedures/classes whose name/documentation match the regular expression <rexpr>.")
  
(define (help . args)
  (if (null? args)
    (begin
      (print *help-message*)
      #unspecified)
    (let* ((name (string-append (car args))))
      (api/show-entries name))))

(define (show-commands prefix)
  (api/show-entry-names prefix))

(define (apropos regex)
  (api/apropos regex))



;;
;; Load macro file 
;; - the source file should be in directory SALENV_DIR/src.
;; - this is used in boot-salenv! below, and it's exported by this module.
;;
(define (sal/load-source! src-file-name)
  [type-check sal/load-source! src-file-name string?]
  (trace 'front-end "  loading source file \"~a\"..." src-file-name)
  (let ((src (string-append (sal/src-directory) (make-string 1 (file-separator)) src-file-name)))
    (unless (file-exists? src)
      (sign-error "File ~s does not exist. Please reinstall SALenv." src))
    (try
     (loadq src)
     (lambda (escape proc msg obj)
       (sign-error "File ~s is corrupted. Please reinstall SALenv." src)))))




;;======================================================================
;; Initialize salenv
;;      header? is a boolean flag: true means that salenv was called
;;                              without script files.
;; *slave-mode* is set to true or false at this point
;;  *arguments* contains the list of all command-line arguments that
;;              are not script files
;;======================================================================

(define (boot-salenv! header?)
  (trace 'front-end "starting SALenv...")
  (sal/initialize-pp!)
  ;;
  ;; Configure prompt and printer used by (repl)
  ;; - cf. bigloo doc, Chapter 'Eval and Code Interpretation'
  ;; - $set-interrupt-notifier is undocumented (check runtime/Llib/error.scm
  ;;   in the bigloo source)
  ;;
  ($set-interrupt-notifier! sal/interrupt-notifier)
  (set-repl-printer! sal/repl-printer)
  (set-prompter! (lambda (nesting) 
                   (unless *slave-mode*
                     (display "sal > "))
                   (flush-output-port (current-output-port))))
  ;;
  ;; Set global variables *sal-directory* and *sal-src-directory*
  ;; (cf. sal-environment.scm)
  ;;
  (sal/set-directory!)

  (when (and header? (not *slave-mode*))
    (print (sal/header "SALenv"))
    (print "Type `(exit)' with parentheses to exit.")
    (print "Type `(help)' with parentheses for help."))

  ;;
  ;; Load macros.
  ;;
  ;; NOTE: scmobj.macros redefine bigloo's 
  ;;   (define-class ...)
  ;;   (define-generic ...)
  ;;   (define-method ...)
  ;; which causes annoying warning messages when running 
  ;; salenv (for recent bigloo versions). I haven't figured
  ;; out how to fix that reliably. Some SAL tools rely on
  ;; the 'define-method' in 'scmobj.macros'.
  ;;
  ;; Update: 05/27/2010. Fixed now, using (bigloo-warning-set! 0).
  ;;
  ;; Macros
  ;;   (sal-check-mode)
  ;;   (sal-collect-info)
  ;;   (FILE-NAME)
  ;; must be defined first. They occur in some of the macro files.
  ;; (They are also used at compilation time and set in the calls
  ;; to the bigloo compiler).
  ;;
  ;; Variables '! '!! '!!! are used to keep track of the last three
  ;; results in the read-eval-print loop.
  ;;
  (eval '(define-macro (sal-check-mode) #t))
  (eval '(define-macro (sal-collect-info) #f))
  (eval '(define-macro (FILE-NAME) "unknown"))
  (eval '(define ! #unspecified))
  (eval '(define !! #unspecified))
  (eval '(define !!! #unspecified))
  (sal/load-source! "api.macros")
  (sal/load-source! "utility.macros")
  (sal/load-source! "trace.macros")
  (sal/load-source! "sxml-package.macros")
  (sal/load-source! "fast-hash-table.macros")
  (sal/load-source! "fast-cache.macros")
  (let ((warning-level (bigloo-warning)))
    (bigloo-warning-set! 0)
    (unwind-protect 
     (sal/load-source! "scmobj.macros")
     (bigloo-warning-set! warning-level)))
  (sal/load-source! "sal-ast-table.macros")
  (sal/load-source! "front-end.macros")
  (sal/load-source! "iterators.macros")
  (sal/load-source! "sal-ast-support.macros")
  (sal/load-source! "no-compilation-support-code.scm")

  ;;
  ;; Initialize global variable *sal-env* defined in sal-api.scm
  ;; - this builds a global sal-env object (cf. sal-environment.scm)
  ;;
  (sal/init-main-sal-env!)

  ;;
  ;; Load user's '.salrc' file 
  ;; - try to find it in the user's HOME directory
  ;; - if it's not there, do nothing.
  ;; load-config-file is defined in utility.scm
  ;;
  (load-config-file ".salrc")

  (trace 'front-end "SALenv started.")
  (when *slave-mode*
    (print "@@@STARTED@@@")))




;;
;; Check whether arg is the name of a script file
;; (see function salenv/main below).
;;
(define (scheme-script? arg)
  (and (file-exists? arg)
       (equal? (suffix arg) "scm")))



;;=============================================================================
;; Entry point: main function of salenv
;; - this function is executed with argv equal to the list of command line arguments
;;   (as passed by the shell).
;; - each element of argc is a character string
;; - the first element is the executable name 
;; - the rest is what was passed to salenv as parameters
;;
;; The function parses the command line and recognizes the following options
;;     --version or -V   (print salenv version and exit)
;;     --slave-mode      (run in slave mode: set global variable *slave-mode* to #t)
;; 
;; Every other argument is either a script file (i.e., a file name with suffix "scm")
;; or some other parameter. 
;;
;; First step: the command-line elements are stored into two queues:
;; - scripts = queue of script files
;; - argument-queue = all other arguments
;; 
;; Second step: build the *arguments* list
;; - the argument-queue is converted into a list stored into global
;;   variable *arguments* (arguments appear in the same order in that list 
;;   as on the command line).
;; - variable *arguments* is defined in module 'front-end'
;; - the front-end module provides utilities to define options and parse 
;;   command-line arguments (that are stored in *arguments*).
;;
;; Third step: initialize all internal salenv structures (call salenv/boot).
;;
;; Fourth step: 
;; a) If there are no scripts, run the salenv read-eval-print-loop.
;;    This process its own options in the *arguments* list.
;; 
;; b) Otherwise: load and execute the script files in the order they appear 
;;    on the command line.
;;    Each sal-tool is defined in a script file that ends by a call to 
;;    (tool-name/main). This is the entry point for that tool.
;; 
;;    NOTE: There's no (define (tool-name/main) ..) so don't 'grep' for it. 
;;    The main function for each tool is constructed  indirectly by 
;;    macro 'gen-front-end" in 'front-end.macros'.
;;=============================================================================

(define (salenv-main argv)
  (try
   (let ((scripts (make-queue))
         (verbosity 0)
         (argument-queue (make-queue)))
     (for-each (lambda (arg)
                 (cond
                  ((or (equal? arg "--version") (equal? arg "-V"))
                   ;; avoid the licensing checking...
                   (print (salenv/version))
                   (exit 0))
                  ((scheme-script? arg)
                   (queue/insert! scripts arg))
                  (else
                   (queue/insert! argument-queue arg))))
               (cdr argv))
     (set! *arguments* (queue->list argument-queue))
     (when (member "--slave-mode" *arguments*)
       ;; I have to set slave-mode before the boot
       (set! *slave-mode* #t))
     (boot-salenv! (queue/empty? scripts))
     (if (queue/empty? scripts)
       (begin
         (front-end/set-categories! '("Help" "Misc" "Pretty Printing" "SALenv"))
         (unless (front-end/parse-args *arguments* (lambda (arg)
                                                     (sign-invalid-arg "Invalid argument: ~a" arg)))
           (salenv/help))
         (repl))
       (for-each (lambda (script)
                   (verbose-message 5 "loading script \"~a\"..." script)
                   (loadq script))
                 (queue->list scripts))))

   ;;
   ;; Catch exceptions
   ;;
   (lambda (escape proc msg obj)
     (with-output-to-port (current-error-port)
       (lambda ()
         (if (and (app-error? proc) (not (sal-check-mode)))
           (print "Error: " msg " " obj)
           (sal/repl-error-notifier proc msg obj))))
     (exit -1))))



;;
;; Print an error message if a command-line argument is not recognized
;; then exit.
;;
(define (salenv/help)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print (sal/header "SALenv"))
      (print "Usage: salenv [options] [scripts]")
      (print "   or: salenv-safe [options] [scripts]")
      (front-end/show-options)
      (newline)))
  (exit -1))
        
        
