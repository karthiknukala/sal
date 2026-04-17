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
                (sal/set-yices2-mcsat-mode! flag)
                (sal/set-yices2-silent-mode! flag)
                (sal/yices2-mcsat-mode?)
                (sal/yices2-silent-mode?)
                (yices2-sat/execute in-file)
                (yices2/execute-core in-file)
                (yices2/execute in-file yices2-id->sal-decl-proc place-provider)
                (yices2/string->sal-expr str yices2-id->sal-decl-proc place-provider)))


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
(define *yices2-mcsat-mode?* #t)

(define *sal-tmp-in-file-to-yices2* #f)
(define *sal-tmp-out-file-to-yices2* #f)

(define *silent-mode* #f)

(define-api (sal/set-yices2-command! (cmd-name string?))
  (set! *yices2-command* cmd-name))
  
(define-api (sal/set-yices2-sat-command! (cmd-name string?))
  (set! *yices2-sat-command* cmd-name))

(define-api (sal/set-yices2-mcsat-mode! flag)
  (set! *yices2-mcsat-mode?* flag))
  
(define (sal/set-yices2-silent-mode! flag)
  (set! *silent-mode* flag))

(define (sal/yices2-mcsat-mode?)
  *yices2-mcsat-mode?*)

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
(define *typed-aux-decls* '())
(define *yices2-aux-decl-idx* 0)



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
  (sign-error "Error running Yices.\nThe following command was used to execute Yices2:\n   ~a\nYou can change this command by adding\n\n  (sal/set-yices2-command! \"<path-to-yices2>/yices\")\n\nto a `.salrc' file in your home directory.\nIf your Yices2 build does not support MCSAT, you can disable it with\n\n  (sal/set-yices2-mcsat-mode! #f)" cmd))

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

(define (yices2-verbosity-flag)
  (if (> (verbosity-level) 3)
    (string-append " --verbosity=" (object->string (verbosity-level)))
    ""))



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
  (let* ((verbose-flag (yices2-verbosity-flag))
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
  (let* ((verbose-flag (yices2-verbosity-flag))
	 (mode-flag " --mode=one-shot")
         (mcsat-flag (if *yices2-mcsat-mode?* " --mcsat" ""))
;;	 (mode-flag " --full-model") ;; for testing on yices-smt
	 (cmd (string-append *yices2-command* mode-flag mcsat-flag verbose-flag 
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
(define *mpq-ten* (make-mpq "10"))

(define (yices2-decimal-string->mpq str)
  (let* ((len (string-length str))
         (exp-pos (let loop ((i 0))
                    (cond
                     ((= i len) #f)
                     ((or (char=? (string-ref str i) #\e)
                          (char=? (string-ref str i) #\E))
                      i)
                     (else
                      (loop (+ i 1))))))
         (mantissa (if exp-pos (substring str 0 exp-pos) str))
         (exponent (if exp-pos
                     (string->integer (substring str (+ exp-pos 1) len))
                     0))
         (mantissa-len (string-length mantissa))
         (negative? (and (> mantissa-len 0)
                         (char=? (string-ref mantissa 0) #\-)))
         (unsigned (if negative?
                     (substring mantissa 1 mantissa-len)
                     mantissa))
         (dot-pos (let loop ((i 0))
                    (cond
                     ((= i (string-length unsigned)) #f)
                     ((char=? (string-ref unsigned i) #\.) i)
                     (else
                      (loop (+ i 1))))))
         (whole (if dot-pos (substring unsigned 0 dot-pos) unsigned))
         (fraction (if dot-pos
                     (substring unsigned (+ dot-pos 1) (string-length unsigned))
                     ""))
         (digits (let ((raw (string-append whole fraction)))
                   (if (= (string-length raw) 0) "0" raw)))
         (scale (string-length fraction))
         (base (make-mpq digits))
         (scaled (if (= scale 0)
                   base
                   (/mpq base (mpq/exp *mpq-ten* (integer->mpq scale)))))
         (shifted (cond
                   ((= exponent 0) scaled)
                   ((> exponent 0)
                    (*mpq scaled (mpq/exp *mpq-ten* (integer->mpq exponent))))
                   (else
                    (/mpq scaled (mpq/exp *mpq-ten* (integer->mpq (- 0 exponent))))))))
    (if negative?
      (-mpq *mpq-zero* shifted)
      shifted)))

(define *yices2-output-lexer*
  (regular-grammar
   ((first-ident-char (or alpha #\_))
    (next-ident-char (or digit #\! first-ident-char))
    (id (: first-ident-char (* next-ident-char)))
    (num (+ digit))
    (sign (? #\-))
    (exp-part (: (in #\e #\E) (? (in #\+ #\-)) num))
    (decimal (: sign
                (or (: num #\. (* digit))
                    (: #\. num))
                (? exp-part)))
    (scientific (: sign num exp-part)))
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
   (decimal
    (cons 'CONST (yices2-decimal-string->mpq (the-string))))
   (scientific
    (cons 'CONST (yices2-decimal-string->mpq (the-string))))
   (num 
    (cons 'CONST (make-mpq (the-string))))
   ((: #\- num)
    (cons 'CONST (make-mpq (the-string))))
   ((: num #\/ num)
    (cons 'CONST (make-mpq (the-string))))
   ((: #\- num #\/ num)
    (cons 'CONST (make-mpq (the-string))))
   ("=>" (cons 'ID '=>))
   ("<=" (cons 'ID '<=))
   (">=" (cons 'ID '>=))
   ("<" (cons 'ID '<))
   (">" (cons 'ID '>))
   ("+" (cons 'ID '+))
   ("*" (cons 'ID '*))
   ("-" (cons 'ID '-))
   ("/" (cons 'ID '/))
   ("^" (cons 'ID '^))
   (id
    (cons 'ID (the-symbol)))
   (else
    (let ((c (the-failure)))
      (if (eof-object? c)
	  c
	  'ERROR)))))

(define *yices2-missing-term* '(yices2-missing))

(define (yices2-missing-term? term)
  (equal? term *yices2-missing-term*))

(define (make-yices2-raw-id id)
  (list 'id id))

(define (yices2-raw-id? term)
  (and (pair? term) (eq? (car term) 'id)))

(define (yices2-raw-id/value term)
  (cadr term))

(define (make-yices2-raw-app fun args)
  (list 'app fun args))

(define (yices2-raw-app? term)
  (and (pair? term) (eq? (car term) 'app)))

(define (yices2-raw-app/fun term)
  (cadr term))

(define (yices2-raw-app/args term)
  (caddr term))

(define (make-yices2-raw-update target idxs val)
  (list 'update target idxs val))

(define (yices2-raw-update? term)
  (and (pair? term) (eq? (car term) 'update)))

(define (yices2-raw-update/target term)
  (cadr term))

(define (yices2-raw-update/idxs term)
  (caddr term))

(define (yices2-raw-update/val term)
  (cadddr term))

(define (sal-type/uninterpreted? type)
  (and (instance-of? type <sal-type-name>)
       (not (instance-of? type <sal-number-type>))
       (not (sal-type-name/definition type))))

(define (injection-name utype)
  (string->symbol (string-append (symbol->string (sal-decl/name (slot-value utype :decl))) "$")))

(define (get-fun-decl ctxt name utype)
  (let ((decl (symbol-table/lookup (slot-value ctxt :constant-declarations) name)))
    (if decl
      decl
      (let* ((int-type (make-sal-builtin-name <sal-int-type> utype))
             (ftype (make-ast-instance <sal-function-type> utype
                                       :domain int-type :range utype :context ctxt))
             (decl (make-ast-instance <sal-constant-decl> utype
                                      :id (make-sal-identifier utype name)
                                      :type ftype
                                      :context ctxt)))
        (symbol-table/add! (slot-value ctxt :constant-declarations) name decl)
        decl))))

(define (coerce-int-to-uninterpreted int-expr utype)
  (let ((ctxt (slot-value utype :context))
        (name (injection-name utype)))
    (make-sal-application
     (make-sal-name-expr (get-fun-decl ctxt name utype))
     (make-application-argument int-expr))))

(define (int->scalar idx scalar-type)
  (let ((i (smpq_to_int (slot-value idx :num)))
        (elements (slot-value scalar-type :scalar-elements)))
    (list-ref elements i)))

(define (known-yices2-id->decl yices-id)
  [assert (*yices2-id->sal-decl*) *yices2-id->sal-decl*]
  (cond
   ((*yices2-id->sal-decl* yices-id) => (lambda (decl) decl))
   ((hashtable-get *aux-decls* yices-id) => (lambda (decl) decl))
   (else #f)))

(define (make-yices2-aux-decl yices-id type)
  (set! *yices2-aux-decl-idx* (+ 1 *yices2-aux-decl-idx*))
  (make-ast-instance <sal-var-decl> *place-provider*
                     :id (make-sal-identifier *place-provider*
                                              (symbol-append yices-id '$
                                                             (object->symbol *yices2-aux-decl-idx*)))
                     :type type))

(define (lookup-typed-aux-decl yices-id expected-type)
  (let loop ((entries *typed-aux-decls*))
    (if (null? entries)
      #f
      (let ((entry (car entries)))
        (if (and (eq? yices-id (car entry))
                 (sal-type/equivalent? expected-type (cadr entry)))
          (caddr entry)
          (loop (cdr entries)))))))

(define (get-typed-aux-decl yices-id expected-type)
  (or (lookup-typed-aux-decl yices-id expected-type)
      (let ((decl (make-yices2-aux-decl yices-id expected-type)))
        (set! *typed-aux-decls* (cons (list yices-id expected-type decl) *typed-aux-decls*))
        decl)))

(define (yices2-id->sal-name-expr/expected yices-id expected-type)
  (cond
   ((known-yices2-id->decl yices-id) =>
    (lambda (decl)
      (if (and (instance-of? expected-type <sal-type>)
               (instance-of? (slot-value decl :type) <sal-any-type>))
        (make-sal-name-expr (get-typed-aux-decl yices-id expected-type))
        (make-sal-name-expr decl))))
   ((instance-of? expected-type <sal-type>)
    (make-sal-name-expr (get-typed-aux-decl yices-id expected-type)))
   (else
    (cond
     ((hashtable-get *aux-decls* yices-id) => (lambda (decl) (make-sal-name-expr decl)))
     (else
      (let ((aux-decl (make-ast-instance <sal-var-decl> *place-provider*
                                         :id (make-sal-identifier *place-provider* yices-id)
                                         :type (make-sal-builtin-name <sal-any-type> *place-provider*))))
        (hashtable-put! *aux-decls* yices-id aux-decl)
        (make-sal-name-expr aux-decl)))))))

(define (yices2-pad-missing raw-args arity)
  (let loop ((args raw-args)
             (remaining arity)
             (result '()))
    (cond
     ((null? args)
      (if (<= remaining 0)
        (reverse! result)
        (loop '() (- remaining 1) (cons *yices2-missing-term* result))))
     ((<= remaining 0)
      (append (reverse! result) args))
     (else
      (loop (cdr args) (- remaining 1) (cons (car args) result))))))

(define (yices2-raw-term/expected-type raw-term)
  (cond
   ((yices2-missing-term? raw-term) #f)
   ((instance-of? raw-term <sal-expr>)
    (sal-expr/type raw-term))
   ((yices2-raw-id? raw-term)
    (cond
     ((known-yices2-id->decl (yices2-raw-id/value raw-term)) =>
      (lambda (decl) (slot-value decl :type)))
     (else #f)))
   ((yices2-raw-app? raw-term)
    (let ((fun-type (yices2-raw-term/expected-type (yices2-raw-app/fun raw-term))))
      (and (instance-of? fun-type <sal-function-type>)
           (sal-function-type/range fun-type))))
   ((yices2-raw-update? raw-term)
    (yices2-raw-term/expected-type (yices2-raw-update/target raw-term)))
   (else #f)))

(define (yices2-convert-term expr expected-type)
  (cond
   ((and (instance-of? expr <sal-numeral>)
         (instance-of? expected-type <sal-type>)
         (sal-type/uninterpreted? expected-type))
    (coerce-int-to-uninterpreted expr expected-type))
   ((and (instance-of? expr <sal-numeral>)
         (instance-of? expected-type <sal-type>)
         (sal-type/scalar? expected-type))
    (int->scalar expr expected-type))
   (else
    expr)))

(define (yices2-missing-value expected-type)
  (cond
   ((instance-of? expected-type <sal-type>)
    (make-sal-name-expr (get-typed-aux-decl 'missing expected-type)))
   (else
    (yices2-id->sal-name-expr/expected 'missing #f))))

(define (make-yices2-builtin-app class args)
  (apply make-sal-builtin-application class *place-provider* args))

(define (yices2-natural-exponent raw-term)
  (let ((expr (yices2-raw-term->sal-expr raw-term #f)))
    (and (instance-of? expr <sal-numeral>)
         (let ((num (slot-value expr :num)))
           (and (mpq/integer? num)
                (>=mpq num *mpq-zero*)
                (mpq->integer num))))))

(define (make-yices2-power-expr base exponent)
  (cond
   ((= exponent 0)
    (make-sal-numeral 1 *place-provider*))
   ((= exponent 1)
    base)
   (else
    (apply make-sal-builtin-application
           <sal-mul>
           *place-provider*
           (let loop ((remaining exponent)
                      (result '()))
             (if (= remaining 0)
               (reverse! result)
               (loop (- remaining 1) (cons base result))))))))

(define (yices2-raw-app/operator raw-term)
  (and (yices2-raw-app? raw-term)
       (let ((fun (yices2-raw-app/fun raw-term)))
         (and (yices2-raw-id? fun)
              (yices2-raw-id/value fun)))))

(define (yices2-convert-operator-app operator raw-args expected-type)
  (cond
   ((eq? operator '=)
    (make-sal-equality (yices2-raw-term->sal-expr (car raw-args) #f)
                       (yices2-raw-term->sal-expr (cadr raw-args) #f)))
   ((or (eq? operator 'distinct)
        (eq? operator '/=))
    (make-sal-and*
     (let loop ((remaining raw-args)
                (result '()))
       (if (null? remaining)
         (reverse! result)
         (let ((lhs (yices2-raw-term->sal-expr (car remaining) #f)))
           (loop (cdr remaining)
                 (append (map (lambda (rhs)
                                (make-yices2-builtin-app
                                 <sal-diseq>
                                 (list lhs
                                       (yices2-raw-term->sal-expr rhs #f))))
                              (cdr remaining))
                         result)))))
     *place-provider*))
   ((eq? operator 'and)
    (make-sal-and*
     (map (lambda (arg)
            (yices2-raw-term->sal-expr arg
                                       (make-sal-builtin-name <sal-bool-type> *place-provider*)))
          raw-args)
     *place-provider*))
   ((eq? operator 'or)
    (make-sal-or*
     (map (lambda (arg)
            (yices2-raw-term->sal-expr arg
                                       (make-sal-builtin-name <sal-bool-type> *place-provider*)))
          raw-args)
     *place-provider*))
   ((eq? operator 'not)
    (make-sal-not
     (yices2-raw-term->sal-expr (car raw-args)
                                (make-sal-builtin-name <sal-bool-type> *place-provider*))))
   ((eq? operator '=>)
    (make-yices2-builtin-app
     <sal-implies>
     (map (lambda (arg)
            (yices2-raw-term->sal-expr arg
                                       (make-sal-builtin-name <sal-bool-type> *place-provider*)))
          raw-args)))
   ((eq? operator '+)
    (apply make-sal-builtin-application
           <sal-add>
           *place-provider*
           (map (lambda (arg)
                  (yices2-raw-term->sal-expr arg expected-type))
                raw-args)))
   ((eq? operator '-)
    (let ((converted (map (lambda (arg)
                            (yices2-raw-term->sal-expr arg expected-type))
                          raw-args)))
      (if (= (length converted) 1)
        (make-yices2-builtin-app
         <sal-sub>
         (list (make-sal-numeral 0 *place-provider*)
               (car converted)))
        (apply make-sal-builtin-application
               <sal-sub>
               *place-provider*
               converted))))
   ((eq? operator '*)
    (apply make-sal-builtin-application
           <sal-mul>
           *place-provider*
           (map (lambda (arg)
                  (yices2-raw-term->sal-expr arg expected-type))
                raw-args)))
   ((eq? operator '/)
    (apply make-sal-builtin-application
           <sal-div>
           *place-provider*
           (map (lambda (arg)
                  (yices2-raw-term->sal-expr arg expected-type))
                raw-args)))
   ((eq? operator 'div)
    (apply make-sal-builtin-application
           <sal-idiv>
           *place-provider*
           (map (lambda (arg)
                  (yices2-raw-term->sal-expr arg expected-type))
                raw-args)))
   ((eq? operator 'mod)
    (apply make-sal-builtin-application
           <sal-mod>
           *place-provider*
           (map (lambda (arg)
                  (yices2-raw-term->sal-expr arg expected-type))
                raw-args)))
   ((eq? operator '<)
    (make-yices2-builtin-app
     <sal-lt>
     (map (lambda (arg)
            (yices2-raw-term->sal-expr arg #f))
          raw-args)))
   ((eq? operator '<=)
    (make-yices2-builtin-app
     <sal-le>
     (map (lambda (arg)
            (yices2-raw-term->sal-expr arg #f))
          raw-args)))
   ((eq? operator '>)
    (make-yices2-builtin-app
     <sal-gt>
     (map (lambda (arg)
            (yices2-raw-term->sal-expr arg #f))
          raw-args)))
   ((eq? operator '>=)
    (make-yices2-builtin-app
     <sal-ge>
     (map (lambda (arg)
            (yices2-raw-term->sal-expr arg #f))
          raw-args)))
   ((eq? operator '^)
    (let ((exponent (and (= (length raw-args) 2)
                         (yices2-natural-exponent (cadr raw-args)))))
      (unless exponent
        (sign-error "Unsupported Yices 2 power term: ~a"
                    (make-yices2-raw-app (make-yices2-raw-id operator) raw-args)))
      (make-yices2-power-expr
       (yices2-raw-term->sal-expr (car raw-args) expected-type)
       exponent)))
   ((eq? operator 'ite)
    (make-ast-instance
     <sal-conditional>
     *place-provider*
     :cond-expr (yices2-raw-term->sal-expr (car raw-args)
                                           (make-sal-builtin-name <sal-bool-type> *place-provider*))
     :then-expr (yices2-raw-term->sal-expr (cadr raw-args) expected-type)
     :else-expr (yices2-raw-term->sal-expr (caddr raw-args) expected-type)))
   (else
    #f)))

(define (yices2-raw-term->sal-expr raw-term expected-type)
  (cond
   ((yices2-missing-term? raw-term)
    (yices2-missing-value expected-type))
   ((instance-of? raw-term <sal-expr>)
    (yices2-convert-term raw-term expected-type))
   ((yices2-raw-id? raw-term)
    (yices2-id->sal-name-expr/expected (yices2-raw-id/value raw-term) expected-type))
   ((yices2-raw-app? raw-term)
    (or (let ((operator (yices2-raw-app/operator raw-term)))
          (and operator
               (yices2-convert-operator-app operator
                                            (yices2-raw-app/args raw-term)
                                            expected-type)))
        (let* ((fun-expr (yices2-raw-term->sal-expr (yices2-raw-app/fun raw-term) #f))
               (fun-type (sal-expr/type fun-expr))
               (domain-types (if (instance-of? fun-type <sal-function-type>)
                               (sal-function-type/domain-types fun-type)
                               '()))
               (raw-args (if (instance-of? fun-type <sal-function-type>)
                           (yices2-pad-missing (yices2-raw-app/args raw-term)
                                               (length domain-types))
                           (yices2-raw-app/args raw-term)))
               (arg-list
                (let loop ((remaining-args raw-args)
                           (remaining-types domain-types)
                           (result '()))
                  (if (null? remaining-args)
                    (reverse! result)
                    (let ((arg-type (if (null? remaining-types) #f (car remaining-types))))
                      (loop (cdr remaining-args)
                            (if (null? remaining-types) '() (cdr remaining-types))
                            (cons (yices2-raw-term->sal-expr (car remaining-args) arg-type) result)))))))
          (yices2-convert-term
           (make-sal-application fun-expr
                                 (apply make-application-argument arg-list))
           expected-type))))
   ((yices2-raw-update? raw-term)
    (let* ((target-expr (yices2-raw-term->sal-expr (yices2-raw-update/target raw-term) #f))
           (target-type (sal-expr/type target-expr))
           (domain-types (if (instance-of? target-type <sal-function-type>)
                           (sal-function-type/domain-types target-type)
                           '()))
           (idx-list
            (let loop ((remaining-idxs (yices2-pad-missing (yices2-raw-update/idxs raw-term)
                                                           (length domain-types)))
                       (remaining-types domain-types)
                       (result '()))
              (if (null? remaining-idxs)
                (reverse! result)
                (let ((idx-type (if (null? remaining-types) #f (car remaining-types))))
                  (loop (cdr remaining-idxs)
                        (if (null? remaining-types) '() (cdr remaining-types))
                        (cons (yices2-raw-term->sal-expr (car remaining-idxs) idx-type) result))))))
           (value-type (and (instance-of? target-type <sal-function-type>)
                            (sal-function-type/range target-type)))
           (new-value (yices2-raw-term->sal-expr (yices2-raw-update/val raw-term) value-type)))
      (yices2-convert-term
       (make-ast-instance <sal-function-update> target-expr
                          :target target-expr
                          :idx (apply make-application-argument idx-list)
                          :new-value new-value)
       expected-type)))
   (else
    (sign-error "Yices2 produced an unsupported model term: ~a" raw-term))))

(define (yices2-make-sal-eq raw1 raw2)
  (let* ((type1 (yices2-raw-term/expected-type raw1))
         (type2 (yices2-raw-term/expected-type raw2))
         (expr1 (yices2-raw-term->sal-expr raw1 (or type1 type2)))
         (expr2 (yices2-raw-term->sal-expr raw2 (or type2 type1))))
    (make-sal-equality expr1 expr2)))


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
     (yices2-make-sal-eq t1 t2))
    ((LP = term@t1 RP)
     (yices2-make-sal-eq t1 *yices2-missing-term*)))
   (term
    ((CONST) (make-sal-numeral CONST *place-provider*))
    ((ID) (make-yices2-raw-id ID))
    ((LP term term-list RP)
     (make-yices2-raw-app term (reverse! term-list)))
    ((LP UPDATE term@target LP term-list@idxs RP term@val RP)
     (make-yices2-raw-update target (reverse! idxs) val))
    ((TRUE) (make-sal-true *place-provider*))
    ((FALSE) (make-sal-false *place-provider*)))
   (term-list
    (() '())
    ((term) (list term))
    ((term-list term) (cons term term-list)))

   ;; Function spec
   (fun-spec
    ((LP FUNCTION ID type-spec fun-spec-body RP) fun-spec-body))
   (fun-spec-body
    ((eq fun-spec-body) (cons eq fun-spec-body))
    ((LP DEFAULT term RP) '())
    ((LP DEFAULT RP) '())
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

(define *yices2-term-parser*
  (lalr-grammar
   (TRUE FALSE UPDATE LP RP = CONST ID)
   (start
    ((term@t)
     (yices2-raw-term->sal-expr t #f)))
   (term
    ((CONST) (make-sal-numeral CONST *place-provider*))
    ((=) (make-yices2-raw-id '=))
    ((ID) (make-yices2-raw-id ID))
    ((LP term term-list RP)
     (make-yices2-raw-app term (reverse! term-list)))
    ((LP UPDATE term@target LP term-list@idxs RP term@val RP)
     (make-yices2-raw-update target (reverse! idxs) val))
    ((TRUE) (make-sal-true *place-provider*))
    ((FALSE) (make-sal-false *place-provider*)))
   (term-list
    (() '())
    ((term) (list term))
    ((term-list term) (cons term term-list)))))

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

(define (parse-term)
  (try
   (read/lalrp
    *yices2-term-parser*
    *yices2-output-lexer*
    (current-input-port))
   (lambda (escape proc msg obj)
     (cond
      ((equal? proc "parser")
       (sign-error (xformat #f "Unexpected Yices 2 term: ~a." msg)))
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
  (set! *typed-aux-decls* '())
  (set! *yices2-aux-decl-idx* 0)
  (with-input-from-file file-name
    parse))

(define (yices2/string->sal-expr str yices2-id->sal-decl-proc place-provider)
  (set! *yices2-id->sal-decl* yices2-id->sal-decl-proc)
  (set! *place-provider* place-provider)
  (set! *aux-decls* (make-hashtable))
  (set! *typed-aux-decls* '())
  (set! *yices2-aux-decl-idx* 0)
  (with-input-from-string str
    parse-term))
