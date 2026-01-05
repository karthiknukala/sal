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

(module sat-boolean-ics-bmc-context
        (include "sal.sch")
        (import utility xformat ics-interface sat-boolean-bmc-context sat-boolean-ics-context 
                sat-bmc-context sal-expression fast-hash-table sal-path sat-context tmp-files)
        (export <sat-boolean-ics-bmc-context>
                (make-sat-boolean-ics-bmc-context flat-module cont-proc))
        )
        
(define-class <sat-boolean-ics-bmc-context> (<sat-boolean-ics-context> <sat-boolean-bmc-context>) ())

(define (make-sat-boolean-ics-bmc-context flat-module cont-proc)
  (let ((ctx (make-instance <sat-boolean-ics-bmc-context>)))
    (sat-boolean-bmc-context/init! ctx flat-module)
    (sat-boolean-ics-context/init! ctx 
                                   (lambda ()
                                     (cont-proc ctx)))
    ctx))

(define *curr-ctx* #unspecified)
(define *curr-step-list-vector* #unspecified)

(define *boolean-ics-output-parser*
  (lalr-grammar
   (SAT UNSAT ID MODEL MAP LP RP ASSIGN ATOM-TRUE ATOM-FALSE)
   (start 
    ((UNSAT) #f)
    ((SAT ID MODEL model) #t))
   (model
    ((bool-assignments) #unspecified))
   (bool-assignments
    ((LP MAP bool-assignment-list RP) #unspecified))
   (bool-assignment-list
    ((bool-assignment)
     #unspecified)
    ((bool-assignment-list bool-assignment)
     #unspecified))
   (bool-assignment
    ((LP ASSIGN ID bool-value RP) 
     (let ((pair (sat-bmc-context/original-decl *curr-ctx* ID)))
       (when pair
         (let* ((decl (car pair))
                (step-idx (cdr pair))
                (step (vector-ref *curr-step-list-vector* step-idx)))
           (when (instance-of? decl <sal-state-var-decl>)
             (eq-hash-table/put! (slot-value step :assignment-table) decl (if bool-value (make-sal-true decl) (make-sal-false decl)))))))))
   (bool-value
    ((ATOM-TRUE) #t)
    ((ATOM-FALSE) #f))))

(define (parse)
  (verbose-message 3 "  parsing output produced by ICS...")
  (try
   (read/lalrp
    *boolean-ics-output-parser*
    *ics-output-lexer*
    (current-input-port))
   (lambda (escape proc msg obj)
     (cond
      ((equal? proc "parser")
       (sign-error (xformat #f "Unexpected output produced by ICS, Reason: ~a. Please contact support." msg)))
      (else
       (error proc msg obj))))))

(define-method (sat-bmc-context/make-path (ctx <sat-boolean-ics-bmc-context>))
  (let ((result-file (sat-context/solve ctx))
        (num-steps (+ (slot-value ctx :max-step) 1)))
    (set! *curr-ctx* ctx)
    (set! *curr-step-list-vector* (make-vector num-steps))
    (let loop ((i 0))
      (when (< i num-steps)
        (vector-set! *curr-step-list-vector* i (make-instance <sal-step>
                                                              :assignment-table (make-eq-hash-table)
                                                              :constraint-list '()))
        (loop (+ i 1))))
    (unwind-protect
     (if (with-input-from-file result-file  parse)
       (make-instance <sal-concrete-path>
                      :flat-module (slot-value ctx :flat-module)
                      :step-info-list (vector->list *curr-step-list-vector*)
                      :auxiliary-decls '()
                      :global-constraint-list '())
       #f)
     (sal/delete-tmp-file! result-file))))

