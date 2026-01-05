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

(module ics-interface
        (include "sal.sch")
        (import tmp-files gmp-scheme sal-expression sal-type runtime xformat)
        (export *ics-command*
                *sal-tmp-in-file-to-ics*
                *sal-tmp-out-file-to-ics*
                *ics-output-lexer*
                (sal/set-ics-in-tmp-file!)
                (sal/set-ics-out-tmp-file!)
                (sal/set-ics-command! cmd-name)
                (sal/set-ics-silent-mode! flag)
                (sal/ics-silent-mode?)
                (ics/execute-core in-file)
                (ics/execute in-file ics-id->sal-decl-proc place-provider))
        )

(define *ics-command* "ics")
(define *sal-tmp-in-file-to-ics* #f)
(define *sal-tmp-out-file-to-ics* #f)
(define *silent-mode* #f)

(define (sal/set-ics-silent-mode! flag)
  (set! *silent-mode* flag))

(define-api (sal/set-ics-command! (cmd-name string?))
  (set! *ics-command* cmd-name))
  
(define (sal/ics-silent-mode?)
  *silent-mode*)

(define (sal/set-ics-in-tmp-file!)
  (set! *sal-tmp-in-file-to-ics* (sal/setup-tmp-file! "input.ics")))

(define (sal/set-ics-out-tmp-file!)
  (set! *sal-tmp-out-file-to-ics* (sal/setup-tmp-file! "output.ics")))

;; function which converts ICS ids to SAL declarations
(define *ics-id->sal-decl* (lambda (decl) #f))
(define *place-provider* #unspecified)

(define (ics/execute-core in-file)
  (sal/set-ics-out-tmp-file!)
  (when (>= (verbosity-level) 3)
    (try 
     (begin
       (display "  ")
       (flush-output-port (current-output-port))
       (system (string-append *ics-command* " -version 2> /dev/null")))
     (lambda (escape proc msg obj)
       (escape #unspecified))))
  (let* ((cmd (string-append *ics-command*
                             " -pp sexpr " 
                             (if (> (verbosity-level) 3) "-verbose " "") ;; -statistics " "") 
                             "\"" in-file "\" > \"" *sal-tmp-out-file-to-ics* "\""
                             (if *silent-mode* " 2> /dev/null " "")))
         (_ (verbose-message 3 "  ICS command: ~a" cmd))
         (_ (status-message :executing-ics))
         (result (display-runtime 3 "  ICS execution time: ~a secs"
                   (lambda () 
                     (system cmd))
                   :ics-time)))
    (unless (= result 0)
      (sign-error "Error executing ICS. The following command was used to execute ICS:\n~a\nIf this is not the correct command to execute ICS, it can be changed using the statement:\n\n  (sal/set-ics-command! \"<path-to-ics>/ics\")\n\nThis statement should be included in your `.salrc' file in your home directory." cmd))
    *sal-tmp-out-file-to-ics*))

(define (ics/execute in-file ics-id->sal-decl-proc place-provider)
  (ics/execute-core in-file)
  (unwind-protect
   (parse-ics-output-file *sal-tmp-out-file-to-ics* ics-id->sal-decl-proc place-provider)
   (sal/delete-tmp-file! *sal-tmp-out-file-to-ics*)))

(define *ics-output-lexer*
  (regular-grammar
   ((first-ident-char (or alpha #\_))
    (next-ident-char (or digit first-ident-char))
    (id (: first-ident-char (* next-ident-char)))
    (int-suffix "{int}")
    (num (+ digit)))
   ((+ (in #\newline #a012 #a013 #\space #\tab))
    (ignore))
   (":sat" 'SAT)
   (":unsat" 'UNSAT)
   (":model" 'MODEL)
   (":map" 'MAP)
   (":assign" 'ASSIGN)
   (":true" 'ATOM-TRUE)
   (":false" 'ATOM-FALSE)
   (":list" 'LIST)
   ("select" 'SELECT)
   ("update" 'UPDATE)
   ("0b1" 'TRUE)
   ("0b0" 'FALSE)
   ("true" 'TRUE)
   ("false" 'FALSE)
   ("sal_app" 'APPLY)
   (#\( 'LP)
   (#\) 'RP)
   ((in #\* #\+ #\- #\= #\< #\>) 
    (the-symbol))
   ((or "<=" ">=" "<>" ">0" ">=0")
    (the-symbol))
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
   ((: id int-suffix)
    (cons 'ID (string->symbol (the-substring 0 (- (the-length) 5)))))
   (else
    (let ((c (the-failure)))
      (if (eof-object? c)
        c
        'ERROR)))))

(define (ics-id->sal-name-expr ics-id)
  [assert (*ics-id->sal-decl*) *ics-id->sal-decl*]
  (cond
   ((*ics-id->sal-decl* ics-id) =>
    (lambda (decl)
      (make-sal-name-expr decl)))
   (else
    (sign-error "Unknown constant `~a' in the output produced by ICS. Please contact support."))))

(define *ics-output-parser*
  (lalr-grammar
   (SAT UNSAT MODEL MAP ASSIGN TRUE FALSE LIST SELECT UPDATE LP RP
    * + - = < > <= >= <> >0 >=0 CONST ID ATOM-TRUE ATOM-FALSE APPLY)
   (start 
    ((trash UNSAT) #f)
    ((trash SAT ID MODEL model) model))
   (model
    ((bool-assignments) bool-assignments)
    ((constraints) constraints)
    ((bool-assignments constraints) (append bool-assignments constraints)))
   (trash
    (() #unspecified)
    ((trash trash-elem)
     #unspecified))
   (trash-elem
    ((LP trash-args RP) 
     #unspecified))
   (trash-args
    (() #unspecified)
    ((trash-args trash-arg) #unspecified))
   (trash-arg
    ((*) #unspecified)
    ((-) #unspecified)
    ((+) #unspecified)
    ((=) #unspecified)
    ((>) #unspecified)
    ((<) #unspecified)
    ((>=) #unspecified)
    ((<=) #unspecified)
    ((<>) #unspecified)
    ((>0) #unspecified)
    ((>=0) #unspecified)
    ((trash-elem) #unspecified))
   (bool-assignments
    ((LP MAP bool-assignment-list RP) bool-assignment-list))
   (bool-assignment-list
    ((bool-assignment)
     (list bool-assignment))
    ((bool-assignment-list bool-assignment)
     (cons bool-assignment bool-assignment-list)))
   (bool-assignment
    ((LP ASSIGN ID bool-value RP) (make-sal-equality (ics-id->sal-name-expr ID) bool-value)))
   (bool-value
    ((ATOM-TRUE) (make-sal-true *place-provider*))
    ((ATOM-FALSE) (make-sal-false *place-provider*)))
   (constraints
    ((LP LIST constraint-list RP) constraint-list))
   (constraint-list
    ((constraint) 
     (list constraint))
    ((constraint-list constraint)
     (cons constraint constraint-list)))
   (constraint
    ((LP constraint-op term@t1 term@t2 RP) (make-sal-builtin-application constraint-op *place-provider* t1 t2))
    ((LP >=0 term RP) (make-sal-builtin-application <sal-ge> *place-provider* term (make-sal-numeral 0 *place-provider*)))
    ((LP >0 term RP) (make-sal-builtin-application <sal-gt> *place-provider* term (make-sal-numeral 0 *place-provider*))))
   (constraint-op
    ((=)  <sal-eq>)
    ((<>) <sal-diseq>)
    ((<)  <sal-lt>)
    ((>)  <sal-gt>)
    ((<=) <sal-le>)
    ((>=) <sal-ge>))
   (term
    ((CONST) (make-sal-numeral CONST *place-provider*))
    ((ID) (ics-id->sal-name-expr ID))
    ((LP SELECT term@t1 term@t2 RP) (make-ast-instance <sal-array-selection> *place-provider*
                                                       :fun t1
                                                       :arg t2))
    ((LP UPDATE term@t1 term@t2 term@t3 RP) (make-ast-instance <sal-array-update> *place-provider*
                                                               :target t1
                                                               :idx t2
                                                               :new-value t3))
    ((LP arith-op term-list RP) (apply make-sal-builtin-application arith-op *place-provider* (reverse! term-list)))
    ((LP APPLY term term-list RP) 
     (cond
      ((sal-type/array? (sal-expr/type term))
       ;; arrays may be encoded as function...
       (unless (= (length term-list) 1)
         (sign-error "Invalid array selection in the output produced by ICS."))
       (make-ast-instance <sal-array-selection> *place-provider*
                          :fun term
                          :arg (car term-list)))
      (else
       (make-sal-application term (apply make-application-argument (reverse! term-list))))))
    ((TRUE) (make-sal-true *place-provider*))
    ((FALSE) (make-sal-false *place-provider*)))
   (arith-op
    ((+) <sal-add>)
    ((-) <sal-sub>)
    ((*) <sal-mul>))
   (term-list
    ((term) (list term))
    ((term-list term) (cons term term-list)))
    ))

(define (parse)
  (status-message :parsing-ics-output)
  (verbose-message 3 "  parsing output produced by ICS...")
  (try
   (read/lalrp
    *ics-output-parser*
    *ics-output-lexer*
    (current-input-port))
   (lambda (escape proc msg obj)
     (cond
      ((equal? proc "parser")
       (sign-error (xformat #f "Unexpected output produced by ICS, Reason: ~a. Please contact support." msg)))
      (else
       (error proc msg obj))))))

(define (parse-ics-output-file file-name ics-id->sal-decl-proc place-provider)
  (set! *ics-id->sal-decl* ics-id->sal-decl-proc)
  (set! *place-provider* place-provider)
  (with-input-from-file file-name
    parse))
