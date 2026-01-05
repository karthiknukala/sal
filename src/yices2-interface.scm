;;
;; SAL 3.1, Copyright (C) 2012, SRI International.  All Rights Reserved.
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

(module yices2-interface
        (include "sal.sch")
        (include "sal-ast-table.sch")
        (import tmp-files gmp-scheme sal-expression sal-type runtime xformat 
		sal-ast-support sal-environment symbol-table)
        (export *yices2-command*
		*yices2-sat-command*
                *sal-tmp-in-file-to-yices2*
                *sal-tmp-out-file-to-yices2*
                (sal/set-yices2-in-tmp-file!)
                (sal/set-yices2-out-tmp-file!)
                (sal/set-yices2-command! cmd-name)
                (sal/set-yices2-sat-command! cmd-name)
                (sal/set-yices2-silent-mode! flag)
                (sal/yices2-silent-mode?)
                (yices2-sat/execute in-file)
                (yices2/execute-core in-file)
                (yices2/execute in-file yices2-id->sal-decl-proc place-provider)))


;;
;; Default executables:
;;  yices2: solver for the YICES2 language
;;  yices2-sat: SAT solver (input in Dimacs format)
;;
;; Note: we use yices2 here so that SAL can support both Yices1 and Yices2
;; the default solver in the Yices-2 distribution is called yices
;;
(define *yices2-command* "yices2")
(define *yices2-sat-command* "yices2-sat")

(define *sal-tmp-in-file-to-yices2* #f)
(define *sal-tmp-out-file-to-yices2* #f)

(define *silent-mode* #f)

(define-api (sal/set-yices2-command! (cmd-name string?))
  (set! *yices2-command* cmd-name))
  
(define-api (sal/set-yices2-sat-command! (cmd-name string?))
  (set! *yices2-sat-command* cmd-name))
  
(define (sal/set-yices2-silent-mode! flag)
  (set! *silent-mode* flag))

(define (sal/yices2-silent-mode?)
  *silent-mode*)

(define (sal/set-yices2-in-tmp-file!)
  (set! *sal-tmp-in-file-to-yices2* (sal/setup-tmp-file! "input.yices")))

(define (sal/set-yices2-out-tmp-file!)
  (set! *sal-tmp-out-file-to-yices2* (sal/setup-tmp-file! "output.yices")))


;; function to convert YICES ids to SAL declarations
(define *yices2-id->sal-decl* (lambda (decl) #f))
(define *place-provider* #unspecified)
(define *aux-decls* #unspecified)



;;
;; Exit code from yices
;;  0 --> success
;; 16 --> out-of-memory
;; 17 --> format/syntax error in input
;; 18 --> file not found (input file couldn't be open)
;; 20 --> some unspecified error
;; 21 --> interrupted
;; 22 --> major error
;;

;;
;; Report different errors:
;; - x = result of the (system ...) call
;;
(define (report-yices2-died x)
  (cond ((if-signaled x)
	 (sign-error "Yices2 killed by signal ~a" (term-sig x)))
	((if-stopped x)
	 (sign-error "Yices2 stopped by signal ~a" (stop-sig x)))
	(else
	 (sign-error "Yices2 terminated unexpectedly"))))


(define (report-yices2-exit x)
  (let ((error-code (exit-status x)))
    (cond 
     ((= error-code 16) (sign-error "Yices2 ran out-of-memory"))
     ((= error-code 17) (sign-error "Yices2 reported a syntax error"))
     ((= error-code 18) (sign-error "Yices2 could not open the input file"))
     ((= error-code 21) (sign-error "Yices2 was interrupted"))
     (else 
      (sign-error "Yices2 exited with code ~a" error-code)))))

(define (report-yices2-bad-output)
  (sign-error "Yices2 produced an unexpected output."))

(define (report-yices2-failed)
  (sign-error "Yices2 failed."))


(define (report-error-running-yices2 cmd)
  (sign-error "Error running Yices.\nThe following command was used to execute Yices2:\n   ~a\nYou can change this command by adding\n\n  (sal/set-yices2-command! \"<path-to-yices2>/yices\")\n\nto a `.salrc' file in your home directory." cmd))

(define (report-error-running-yices2-sat cmd)
  (sign-error "Error running Yices.\nThe following command was used to execute Yices2:\n   ~a\nYou can change this command by adding\n\n  (sal/set-yices2-sat-command! \"<path-to-yices2>/yices-sat\")\n\nto a `.salrc' file in your home directory." cmd))


;; Check whether yices exited normally
(define (yices2-exit-normal x)
  (and (>= x 0) (if-exited x) (= (exit-status x) 0)))

;; Exited with one of the error codes above
(define (yices2-exit-with-error x)
  (if (and (>= x 0) (if-exited x))
      (let ((exit-code (exit-status x)))
	(or (= exit-code 16) (= exit-code 17) (= exit-code 18) (= exit-code 19)
	    (= exit-code 20) (= exit-code 21) (= exit-code 22)))))

;; Killed or other termination
(define (yices2-terminated x)
  (or (if-signaled x) (if-stopped x)))



;;
;; Print Yices version (if verbosity level is more than 3)
;; 
(define (show-yices2-version cmd)
  (when (>= (verbosity-level) 3)
	(try 
	 (begin
	   (flush-output-port (current-output-port))
	   (system (string-append cmd " --version 2> /dev/null")))
	 (lambda (escape proc msg obj)
	   (escape #unspecified)))))



;;
;; SAT SOLVER
;;

;;
;; Call the Yices sat solver
;; - in-file = input file in Dimacs format
;; - the solver output is stored in a temporary file
;; - if the input is sat, return a Boolean assignment (in Dimacs format: list of integers
;;   where each integer encodes a literal)
;; - if the input is not sat, return #f
;;
(define (yices2-sat/execute in-file)
  (sal/set-yices2-out-tmp-file!)
  (show-yices2-version *yices2-sat-command*)
  (let* ((verbose-flag (if (> (verbosity-level) 3) " --verbose" ""))
	 (cmd (string-append *yices2-sat-command* " --model" verbose-flag 
			     " \"" in-file "\" > \"" *sal-tmp-out-file-to-yices2* "\""
			     (if *silent-mode* " 2> /dev/null " "")))
         (_ (verbose-message 3 "  Yices2 command: ~a" cmd))
         (_ (status-message :executing-yices2-sat))
         (result (display-runtime 3 "  Yices2 execution time: ~a secs"
                   (lambda () (system cmd)) :yices-time))
	 (_ (verbose-message 2  "  exit code: ~a" result)))
    (unwind-protect 
     (cond
      ;; normal exit --> process the ouput
      ((yices2-exit-normal result)
       (with-input-from-file *sal-tmp-out-file-to-yices2* 
	 (lambda () (yices2-sat/filter-output))))
      ;; error from Yices
      ((yices2-exit-with-error result)
       (report-yices2-exit result))      
      ;; killed or crashed
      ((yices2-terminated result)
       (report-yices2-died result))
      ;; any other code: assume the system call failed
      (else 
       (report-error-running-yices2-sat cmd)))
     (sal/delete-tmp-file! *sal-tmp-out-file-to-yices2*))))


;;
;; Parse the output from Yices and turn in into a Dimacs model
;;
(define (yices2-sat/filter-output)
  (try
   (let ((first-line (read-line)))
     (cond 
      ((equal? first-line "sat")
       (let* ((line (read-line))
              (tmp-list (delimited-string->list '(#\space) line)))
         (map-and-filter (lambda (str)
                           (let ((num (string->integer str)))
                             (and (not (= num 0)) ;; failed to read number
                                  num)))
                         tmp-list)))
      ((equal? first-line "unsat")
       #f)
      (else
       (report-yices2-bad-output))))
   (lambda (escape proc msg obj)
     (report-yices2-bad-output))))





;;
;; SMT SOLVER
;;

;;
;; Call yices2 on the given input file
;; - the solver output is sent to a temporary file 
;; - this function returns the output filename
;;
(define (yices2/execute-core in-file)
  (sal/set-yices2-out-tmp-file!)
  (show-yices2-version *yices2-command*)
  (let* ((verbose-flag (if (> (verbosity-level) 3) " --verbose" ""))
	 (mode-flag " --mode=one-shot")
;;	 (mode-flag " --full-model") ;; for testing on yices-smt
	 (cmd (string-append *yices2-command* mode-flag verbose-flag 
			     " \"" in-file "\" > \"" *sal-tmp-out-file-to-yices2* "\""
			     (if *silent-mode* " 2> /dev/null " "")))
         (_ (verbose-message 3 "  Yices2 command: ~a" cmd))
         (_ (status-message :executing-yices))
         (result (display-runtime 3 "  Yices2 execution time: ~a secs"
                   (lambda () (system cmd)) :yices-time))
	 (_ (verbose-message 2  "  exit code: ~a" result)))
    (cond 
     ;; normal exit: return the output file name
     ((yices2-exit-normal result) 
      *sal-tmp-out-file-to-yices2*)
     ;; error from yices
     ((yices2-exit-with-error result) 
      (report-yices2-exit result))
     ;; killed or crashed
     ((yices2-terminated result)
      (report-yices2-died result))
     ;; any other code: assume bad system call
     (else 
      (report-error-running-yices2 cmd)))))

(define (yices2/execute in-file yices2-id->sal-decl-proc place-provider)
  (yices2/execute-core in-file)
  (unwind-protect
   (parse-yices2-output-file *sal-tmp-out-file-to-yices2* yices2-id->sal-decl-proc place-provider)
   (sal/delete-tmp-file! *sal-tmp-out-file-to-yices2*)))



;;
;; Parsing of the Yices output
;;
(define *yices2-output-lexer*
  (regular-grammar
   ((first-ident-char (or alpha #\_))
    (next-ident-char (or digit #\! first-ident-char))
    (id (: first-ident-char (* next-ident-char)))
    (num (+ digit)))
   ((+ (in #\newline #a012 #a013 #\space #\tab))
     (ignore))
   ("sat" 'SAT)
   ("unsat" 'UNSAT)
   ("unknown" (warning-message "The counterexample produced by Yices may not be a true counterexample due to incompleteness.") 'SAT)
   ("true" 'TRUE)
   ("false" 'FALSE)
   ("update" 'UPDATE)
   ("function" 'FUNCTION)
   ("type" 'TYPE)
   ("default" 'DEFAULT)
   ("->" 'ARROW)
   ("tuple" 'TUPLE)
   (#\( 'LP)
   (#\) 'RP)
   (#\. 'DOT)
   (#\= (the-symbol))
   (num 
    (cons 'CONST (make-mpq (the-string))))
   ((: #\- num)
    (cons 'CONST (make-mpq (the-string))))
   ((: num #\/ num)
    (cons 'CONST (make-mpq (the-string))))
   ((: #\- num #\/ num)
    (cons 'CONST (make-mpq (the-string))))
   (id
    (cons 'ID (the-symbol)))
   (else
    (let ((c (the-failure)))
      (if (eof-object? c)
	  c
	  'ERROR)))))

(define (yices2-id->sal-name-expr yices-id)
  [assert (*yices2-id->sal-decl*) *yices2-id->sal-decl*]
  (cond
   ((*yices2-id->sal-decl* yices-id) => (lambda (decl) (make-sal-name-expr decl)))
   (else
    (cond
     ((hashtable-get *aux-decls* yices-id) => (lambda (decl) (make-sal-name-expr decl)))
     (else
      (let ((aux-decl (make-ast-instance <sal-var-decl> *place-provider*
                                         :id (make-sal-identifier *place-provider* yices-id)
                                         :type (make-sal-builtin-name <sal-any-type> *place-provider*))))
        (hashtable-put! *aux-decls* yices-id aux-decl)
        (make-sal-name-expr aux-decl)))))))


;;
;; Yices2 displays the model as a list of equalities followed by list of function specifications
;; the function specifications are of the form
;;
;;  (function f 
;;    (type <type of  f>)
;;    (= (f a1 .. aK) b1)
;;      ...
;;    (default <default value>))
;;
;; We allow: empty model
;;           model with only equalities
;;           model with only function specifications
;;
;; In a function specification:
;;   the default value may be absent
;;   the list of equalities may be empty
;;

(define *yices2-output-parser*
  (lalr-grammar
   (SAT UNSAT TRUE FALSE UPDATE FUNCTION TYPE DEFAULT ARROW TUPLE LP RP DOT = CONST ID)
   (start 
    ((UNSAT trash) #f)
    ((SAT model) model))
   (model
    (() '())
    ((eq model) (cons eq model))
    ((fun-spec more-fun-specs) (append fun-spec more-fun-specs)))
   (more-fun-specs
    (() '())
    ((fun-spec more-fun-specs) (append fun-spec more-fun-specs)))

   ;; Equalities and terms
   (eq
    ((LP = term@t1 term@t2 RP)
     (make-sal-equality t1 t2)))
   (term
    ((CONST) (make-sal-numeral CONST *place-provider*))
    ((ID) (yices2-id->sal-name-expr ID))
    ((LP term term-list RP)
     (make-sal-application term (apply make-application-argument (reverse! term-list))))
    ((LP UPDATE term@target LP term-list@idxs RP term@val RP)
     (make-ast-instance <sal-function-update> target
                        :target target
                        :idx (apply make-application-argument (reverse! idxs))
                        :new-value val))
    ((TRUE) (make-sal-true *place-provider*))
    ((FALSE) (make-sal-false *place-provider*)))
   (term-list
    ((term) (list term))
    ((term-list term) (cons term term-list)))

   ;; Function spec
   (fun-spec
    ((LP FUNCTION ID type-spec fun-spec-body RP) fun-spec-body))
   (fun-spec-body
    ((eq fun-spec-body) (cons eq fun-spec-body))
    ((LP DEFAULT term RP) '())
    (() '()))
   (type-spec
    ((LP TYPE type RP) #unspecified))
   (type
    ((ID) #unspecified)
    ((LP ARROW type-list RP) #unspecified)
    ((LP TUPLE type-list RP) #unspecified))
   (type-list
    ((type) #unspecified)
    ((type-list type) #unspecified))

   ;; Anything after unsat is ignored
   (trash
    (() #unspecified)
    ((trash trash-elem) #unspecified))
   (trash-elem
    ((SAT) #unspecified)
    ((UNSAT) #unspecified)
    ((TRUE) #unspecified)
    ((FALSE) #unspecified)
    ((UPDATE) #unspecified)
    ((FUNCTION) #unspecified)
    ((TYPE) #unspecified)
    ((DEFAULT) #unspecified)
    ((ARROW) #unspecified)
    ((TUPLE) #unspecified)
    ((LP) #unspecified)
    ((RP) #unspecified)
    ((DOT) #unspecified)
    ((=) #unspecified)
    ((CONST) #unspecified)
    ((ID) #unspecified))
   ))

(define (parse)
  (status-message :parsing-yices-output)
  (verbose-message 3 "  parsing output produced by Yices 2...")
  (try
   (read/lalrp
    *yices2-output-parser*
    *yices2-output-lexer*
    (current-input-port))
   (lambda (escape proc msg obj)
     (cond
      ((equal? proc "parser")
       (sign-error (xformat #f "Unexpected output from Yices 2: ~a. Please contact support." msg)))
      (else
       (error proc msg obj))))))


;;
;; To test the parser from salenv (on a file called "xxxx.mdl")
;; use the command 
;;  (parse-yice2-output-file "xxxx.mdl" (lambda (x) #f) (make-empty-context *sal-env*))
;;
(define (parse-yices2-output-file file-name yices2-id->sal-decl-proc place-provider)
  (set! *yices2-id->sal-decl* yices2-id->sal-decl-proc)
  (set! *place-provider* place-provider)
  (set! *aux-decls* (make-hashtable))
  (with-input-from-file file-name
    parse))
