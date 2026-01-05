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

(module yices-interface
        (include "sal.sch")
        (include "sal-ast-table.sch")
        (import tmp-files gmp-scheme sal-expression sal-type runtime xformat sal-ast-support 
		sal-environment symbol-table)
        (export *yices-command*
                *sal-tmp-in-file-to-yices*
                *sal-tmp-out-file-to-yices*
                (sal/set-yices-in-tmp-file!)
                (sal/set-yices-out-tmp-file!)
                (sal/set-yices-command! cmd-name)
                (sal/set-yices-silent-mode! flag)
                (sal/yices-silent-mode?)
                (yices/execute-core in-file sat-solver?)
                (yices-sat/execute in-file)
                (yices/execute in-file yices-id->sal-decl-proc place-provider))
        )

(define *yices-command* "yices")
(define *sal-tmp-in-file-to-yices* #f)
(define *sal-tmp-out-file-to-yices* #f)
(define *silent-mode* #f)

(define (sal/set-yices-silent-mode! flag)
  (set! *silent-mode* flag))

(define-api (sal/set-yices-command! (cmd-name string?))
  (set! *yices-command* cmd-name))
  
(define (sal/yices-silent-mode?)
  *silent-mode*)

(define (sal/set-yices-in-tmp-file!)
  (set! *sal-tmp-in-file-to-yices* (sal/setup-tmp-file! "input.yices")))

(define (sal/set-yices-out-tmp-file!)
  (set! *sal-tmp-out-file-to-yices* (sal/setup-tmp-file! "output.yices")))

;; function to convert YICES ids to SAL declarations
(define *yices-id->sal-decl* (lambda (decl) #f))
(define *place-provider* #unspecified)
(define *aux-decls* #unspecified)

(define (yices/execute-core in-file sat-solver?)
  (sal/set-yices-out-tmp-file!)
  (when (>= (verbosity-level) 3)
    (try 
     (begin
       (display "  Yices ")
       (flush-output-port (current-output-port))
       (system (string-append *yices-command* " --version 2> /dev/null")))
     (lambda (escape proc msg obj)
       (escape #unspecified))))
  (let* ((cmd (string-append *yices-command*
                             " --evidence " 
                             (if sat-solver? " --dimacs " "")
                             (if (> (verbosity-level) 3) " -v 3 " "") ;; -statistyices " "") 
                             "\"" in-file "\" > \"" *sal-tmp-out-file-to-yices* "\""
                             (if *silent-mode* " 2> /dev/null " "")))
         (_ (verbose-message 3 "  YICES command: ~a" cmd))
         (_ (status-message :executing-yices))
         (result (display-runtime 3 "  YICES execution time: ~a secs"
                   (lambda () 
                     (system cmd))
                   :yices-time)))
    (unless (= result 0)
      (sign-error "Error executing YICES. The following command was used to execute YICES:\n~a\nIf this is not the correct command to execute YICES, it can be changed using the statement:\n\n  (sal/set-yices-command! \"<path-to-yices>/yices\")\n\nThis statement should be included in your `.salrc' file in your home directory." cmd))
    *sal-tmp-out-file-to-yices*))

(define (sign-yices-failed)
  (sign-error "Yices failed."))

(define (yices-sat/execute in-file)
  (unwind-protect
   (begin
     (yices/execute-core in-file #t)
     (with-input-from-file *sal-tmp-out-file-to-yices*
       (lambda ()
         (yices-sat/filter-output))))
   (sal/delete-tmp-file! *sal-tmp-out-file-to-yices*)))

(define (yices-sat/filter-output)
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
       (sign-yices-failed))))
   (lambda (escape proc msg obj)
     (sign-yices-failed))))

(define (yices/execute in-file yices-id->sal-decl-proc place-provider)
  (yices/execute-core in-file #f)
  (unwind-protect
   (parse-yices-output-file *sal-tmp-out-file-to-yices* yices-id->sal-decl-proc place-provider)
   (sal/delete-tmp-file! *sal-tmp-out-file-to-yices*)))


(define *yices-output-lexer*
  (regular-grammar
   ((first-ident-char (or alpha #\_))
    (next-ident-char (or digit #\! first-ident-char))
    (id (: first-ident-char (* next-ident-char)))
    (int-suffix "{int}")
    (num (+ digit)))
   ((+ (in #\newline #a012 #a013 #\space #\tab))
     (ignore))
   ("sat" 'SAT)
   ("unsat" 'UNSAT)
   ("unknown" (warning-message "The counterexample produced by yices may not be a true counterexample due to incompleteness.") 'SAT)
   ("true" 'TRUE)
   ("false" 'FALSE)
   ("update" 'UPDATE)
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

(define (yices-id->sal-name-expr yices-id)
  [assert (*yices-id->sal-decl*) *yices-id->sal-decl*]
;;;  (print " >>> " yices-id " " (symbol? yices-id) " " (string? yices-id))
  (cond
   ((*yices-id->sal-decl* yices-id) =>
    (lambda (decl)
;;BD      (display* "decl: ")(sal/pp decl)(newline)
      (make-sal-name-expr decl)))
   (else
    (cond
     ((hashtable-get *aux-decls* yices-id) =>
      (lambda (decl)
        (make-sal-name-expr decl)))
     (else
      (let ((aux-decl (make-ast-instance <sal-var-decl> *place-provider*
                                         :id (make-sal-identifier *place-provider* yices-id)
                                         :type (make-sal-builtin-name <sal-any-type> *place-provider*))))
        (hashtable-put! *aux-decls* yices-id aux-decl)
        (make-sal-name-expr aux-decl)))))))


;----------------------------------
; BD: bug fix. 2006/09/04
;
; 1) equations from the yices model may be illtyped because Yices produces a model
; with integer values assigned to uninterpreted variables.
; 
; We need to convert the integer from yices into the correct uninterpreted type T
; This is done by introducing a new (injective) function of type [int -> T] in the context
; then applying it to the yices integer.
;
; 2) SAL scalar are translated to integers in the yices input
; For SAL to print the concrete counterexample, we must convert back integers to 
; scalar when a scalar is expected by SAL.
;

;; Check whether type is uninterpreted
;; BD: do not consider "number" as an uninterpreted type (2006/11/13)
;;     (this breaks sal-atg)
(define (sal-type/uninterpreted? type)
  (and (instance-of? type <sal-type-name>) (not (instance-of? type <sal-number-type>))
       (not (sal-type-name/definition type))))

;; Generate the function name for type utype (name is typename$)
(define (injection-name utype) 
  (string->symbol (string-append (symbol->string (sal-decl/name (slot-value utype :decl))) "$")))

;; Add declaration name: [int -> utype] in ctxt
;; (unless it's already declared)
(define (get-fun-decl ctxt name utype)
  (let ((decl (symbol-table/lookup (slot-value ctxt :constant-declarations) name)))
    (if decl 
	decl ;; already declared
	;; add declaration
	(let* ((int-type (make-sal-builtin-name <sal-int-type> utype))
	       (ftype (make-ast-instance <sal-function-type> utype 
					 :domain int-type :range utype :context ctxt))
	       (decl (make-ast-instance <sal-constant-decl> utype
					:id (make-sal-identifier utype name)
					:type ftype
					:context ctxt)))
	  (symbol-table/add! (slot-value ctxt :constant-declarations) name decl)
;;	  (print "--- Adding declaration ---")
;;	  (display* "name: ")(sal/pp name)(newline)
;;	  (display* "decl: ")(sal/pp decl)(newline)
	  decl))))

;; Get declaration of the function name
;; utype is an instance of <sal-type-name> (or <sal-qualified-type-name)
(define (coerce-int-to-uninterpreted int-expr utype) 
  (let ((ctxt (slot-value utype :context))
	(name (injection-name utype)))
    (make-sal-application
     (make-sal-name-expr (get-fun-decl ctxt name utype))
     (make-application-argument int-expr))))

;; Convert an integer index to a <sal-scalar> of the given type
;; idx must be a constant (<sal-numeral>)
(define (int->scalar idx scalar-type)
  (let ((i (smpq_to_int (slot-value idx :num)))
	(list (slot-value scalar-type :scalar-elements)))
    (list-ref list i)))
;; TODO: double check that the integer is between 0 and length of the list

(define (yices-eq->sal-eq a b)
  (let ((type_a (sal-expr/type a))
	(type_b (sal-expr/type b)))
    (cond 
     ((and (sal-type/uninterpreted? type_a)(sal-type/integer? type_b))
      (make-sal-equality a (coerce-int-to-uninterpreted b type_a)))
     ((and (sal-type/scalar? type_a)(sal-type/integer? type_b))
      (make-sal-equality a (int->scalar b type_a)))
     ((and (sal-type/uninterpreted? type_b)(sal-type/integer? type_a))
      (make-sal-equality (coerce-int-to-uninterpreted a type_b) b))
     ((and (sal-type/scalar? type_b)(sal-type/integer? type_a))
      (make-sal-equality (int->scalar a type_b) b))
     (else (make-sal-equality a b)))))

(define *yices-output-parser*
  (lalr-grammar
   (SAT UNSAT TRUE FALSE LP RP = CONST ID DOT UPDATE)
   (start 
    ((UNSAT trash) #f)
    ((SAT model) model))
   (model
    ((eq-list) eq-list))
   (eq-list
    ((eq)
     (list eq))
    ((eq-list eq)
     (cons eq eq-list)))
   (eq
    ((LP = term@t1 term@t2 RP)
     (yices-eq->sal-eq t1 t2)))
;;     (make-sal-equality t1 t2)))
   (term
    ((CONST) (make-sal-numeral CONST *place-provider*))
    ((ID) (yices-id->sal-name-expr ID))
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
   (trash
    (() #unspecified)
    ((trash trash-elem) #unspecified))
   (trash-elem
    ((ID) #unspecified)
    ((LP) #unspecified)
    ((RP) #unspecified)
    ((DOT) #unspecified)
    ((CONST) #unspecified)
    ((=) #unspecified)
    ((TRUE) #unspecified)
    ((FALSE) #unspecified))
   ))

(define (parse)
  (status-message :parsing-yices-output)
  (verbose-message 3 "  parsing output produced by YICES...")
  (try
   (read/lalrp
    *yices-output-parser*
    *yices-output-lexer*
    (current-input-port))
   (lambda (escape proc msg obj)
     (cond
      ((equal? proc "parser")
       (sign-error (xformat #f "Unexpected output produced by YICES, Reason: ~a. Please contact support." msg)))
      (else
       (error proc msg obj))))))

(define (parse-yices-output-file file-name yices-id->sal-decl-proc place-provider)
  (set! *yices-id->sal-decl* yices-id->sal-decl-proc)
  (set! *place-provider* place-provider)
  (set! *aux-decls* (make-hashtable))
  (with-input-from-file file-name
    parse))
