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

(module sal-path-pp
        (include "sal.sch")
        (include "fast-hash-table.sch")
        (import front-end sal-pp queue sal-module pretty sal-expression sal-expr-evaluator
                iterators sal-type sal-ast-eq sal-path sal-derived-path 
                sal-display-variable-value sal-transition-step)
        (export (sal-path/display sal-path only-inputs?) 
                (sal-path/pp sal-path . only-inputs?)
                (sal-flat-module/display-variables flat-module assignment-table prev-assignment-table hide-locals? hide-input?)
                (sal-trace-info/display trace-info assignment-table flat-module detailed?)
                (sal-transition-step/display transition-step detailed?)
                (sal-step-list/display step-list flat-module delta? hide-locals? detailed? only-inputs? first-step-id))
        )

(define *sal-pp-delta-path?* #f)
(define *sal-pp-detailed-path?* #t)
(define *sal-pp-hide-locals?* #f)

(front-end/add-simple-option! 
 "Path Pretty Printing" 
 "--delta-path" 
 "Force SALenv to print only the value of modified variables at every step. This option may generate confusing results for symbolic counterexamples (i.e., counterexamples which contain contraints."
 (lambda () (set! *sal-pp-delta-path?* #t)))

(front-end/add-simple-option!
 "Path Pretty Printing" 
 "--hide-locals" 
 "Do not print the value of local variables."
 (lambda () (set! *sal-pp-hide-locals?* #t)))

(define-api (sal-path/pp path . only-inputs?)
  :doc "Pretty print a SAL path."
  (let ((only-inputs? (optional-arg only-inputs? #f)))
    (sal-path/display path only-inputs?)))

(define-generic (sal-path/display sal-path only-inputs?))

(define-method (sal-path/display (sal-path <sal-path>) (only-inputs? <primitive>))
  (sal-path/display-preamble sal-path)
  (let* ((flat-module (slot-value sal-path :flat-module))
         (_ [assert (flat-module) flat-module])
         (transition-trace-info (slot-value flat-module :transition-trace-info))
         (state-vars (slot-value flat-module :state-vars))
         (step-list (slot-value sal-path :step-info-list)))
    ;; (breakpoint "sal-path/display" (transition-trace-info sal-path flat-module) #t)
    (sal-step-list/display step-list flat-module
                           *sal-pp-delta-path?* *sal-pp-hide-locals?* *sal-pp-detailed-path?* only-inputs? 0)))

(define-method (sal-path/display (sal-path <sal-cyclic-path>) (only-inputs? <primitive>))
  (sal-path/display-preamble sal-path)
  (let* ((flat-module (slot-value sal-path :flat-module))
         (transition-trace-info (slot-value flat-module :transition-trace-info))
         (state-vars (slot-value flat-module :state-vars))
         (step-list (slot-value sal-path :step-info-list))
         (cycle-step-list (slot-value sal-path :cycle-step-info-list)))
    (sal-step-list/display step-list flat-module
                           *sal-pp-delta-path?* *sal-pp-hide-locals?* *sal-pp-detailed-path?* only-inputs? 0)
    (print "========================")
    (print "Begin of Cycle")
    (print "========================")
    (sal-step-list/display cycle-step-list flat-module
                           *sal-pp-delta-path?* *sal-pp-hide-locals?* *sal-pp-detailed-path?*
                           only-inputs?
                           (- (length step-list) 1))))

(define-method (sal-path/display (sal-path <sal-pseudo-cyclic-path>) (only-inputs? <primitive>))
  (print "============ Pseudo Cyclic Path ===============")
  (print "  There is a cycle modulo sliced variables. ")
  (print "")
  (call-next-method)
  (print "..."))

(define (input-var? state-var-decl)
  (and (instance-of? state-var-decl <sal-input-state-var-decl>)
       ;; the auxiliary choice vars should not be printed...
       (not (instance-of? state-var-decl <sal-choice-input-state-var-decl>))))
  
(define (display-constraints constraint-list)
  (for-each (lambda (constraint)
              (sal-value/pp constraint)
              (print ";"))
            constraint-list))

(define (display-input-variable-assignments state-vars assignment-table)
  (when (exists input-var? state-vars)
    (print "--- Input Variables (assignments) ---")
    (for-each (lambda (state-var-decl)
                (when (and (instance-of? state-var-decl <sal-input-state-var-decl>)
                           ;; the auxiliary choice vars should not be printed...
                           (not (instance-of? state-var-decl <sal-choice-input-state-var-decl>)))
                  (let ((value (cond ((eq-hash-table/get assignment-table state-var-decl) => cdr) (else #f))))
                    (when value
                      (sal-var-decl/display-value state-var-decl value)))))
              state-vars)))

(define (display-system-variable-assignments state-vars assignment-table prev-assignment-table hide-locals?)
  (print "--- System Variables (assignments) ---")
  (for-each (lambda (state-var-decl)
              (unless (or (instance-of? state-var-decl <sal-input-state-var-decl>)
                          (and hide-locals? (instance-of? state-var-decl <sal-local-state-var-decl>)))
                (let ((value (cond ((eq-hash-table/get assignment-table state-var-decl) => cdr) (else #f)))
                      (prev-value (cond ((and prev-assignment-table
                                              (eq-hash-table/get prev-assignment-table state-var-decl)) => cdr) 
                                        (else #f))))
                    (when value
                      (sal-var-decl/display-value-differences state-var-decl value prev-value)))))
            state-vars))

(define (sal-flat-module/display-variables flat-module assignment-table prev-assignment-table hide-locals? hide-input?)
  (let ((state-vars (sal-module/state-variables flat-module)))
    (unless hide-input?
      (display-input-variable-assignments state-vars assignment-table))
    (display-system-variable-assignments state-vars assignment-table prev-assignment-table hide-locals?)))
                              
(define (sal-step/display sal-step sal-prev-step state-vars delta? hide-locals? only-inputs?)
  (let* ((assignment-table (slot-value sal-step :assignment-table))
         (prev-assignment-table (if (and sal-prev-step delta?)
                                  (slot-value sal-prev-step :assignment-table)
                                  #f))
         (constraint-list (slot-value sal-step :constraint-list)))
    (display-input-variable-assignments state-vars assignment-table)
    (unless only-inputs?
      (display-system-variable-assignments state-vars assignment-table prev-assignment-table hide-locals?)
      (unless (null? constraint-list)
        (print "--- Constraints ---")
        (display-constraints constraint-list)))))
  
(define (sal-trace-info/display trace-info assignment-table flat-module detailed?)
  ;; (breakpoint "sal-trace-info/display" (trace-info) #t)
  (when trace-info
    (let ((transition-step (sal-trace-info/recover-executed-transition-info trace-info assignment-table flat-module)))
      (sal-transition-step/display transition-step detailed?))))

(define (sal-transition-step/display transition-step detailed?)
  (when transition-step
    (let ((doc (sal-transition-step->doc transition-step detailed?)))
      (unless (eq? doc *doc-nil*)
        (print "------------------------")
        (print "Transition Information: ")
        ;; (breakpoint "sal-transition-step->doc" (transition-step doc) #t)
        (sal/pretty doc)
        (print ""))))
  (print "------------------------"))

(define (sal-step-list/display step-list flat-module delta? hide-locals? detailed? only-inputs? first-step-id)
  (let ((state-vars (slot-value flat-module :state-vars)))
    (if (null? step-list)
      (warning-message "Warning: empty SAL path.")
      (let loop ((prev-step #f)
                 (step-list step-list)
                 (i first-step-id))
        (let ((curr-step (car step-list)))
          (print "Step " i ":")
          (cond
           ((null? (cdr step-list))
            ;; force the state to be completely printed in the last step
            (sal-step/display curr-step prev-step state-vars #f hide-locals? only-inputs?)) 
           (else
            (sal-step/display curr-step prev-step state-vars delta? hide-locals? only-inputs?)
            (sal-transition-step/display (slot-value curr-step :transition-step) detailed?)
            (loop curr-step (cdr step-list) (+ i 1)))))))))

(define (sal-path/display2 sal-path)
  (unless (null? (slot-value sal-path :auxiliary-decls))
    (print "Auxiliary declarations:")
    (for-each (lambda (decl)
                (sal/pp decl)
                (print ";"))
              (slot-value sal-path :auxiliary-decls)))
  (unless (null? (slot-value sal-path :global-constraint-list))
    (print "Global constraints: ")
    (for-each (lambda (expr)
                (sal/pp expr)
                (print ";"))
              (slot-value sal-path :global-constraint-list)))
  (let ((step-id 0))
    (for-each (lambda (step)
                (print "Step " step-id ":")
                (print "Assignments: ")
                (eq-hash-table/for-each (lambda (decl expr)
                                          (sal/pp (make-sal-equality (make-sal-name-expr decl)
                                                                     expr))
                                          (print ";"))
                                        (slot-value step :assignment-table))
                (print "Constraints: ")
                (for-each (lambda (expr)
                            (sal/pp expr)
                            (print ";"))
                          (slot-value step :constraint-list))
                (print "----------------------------")
                (set! step-id (+ step-id 1)))
              (slot-value sal-path :step-info-list))))

(define (sal-path/display-auxiliary-decls sal-path)
  (unless (null? (slot-value sal-path :auxiliary-decls))
    (print "========================")
    (print "Auxiliary Declarations")
    (print "========================")
    (for-each (lambda (decl)
                (sal/pp decl)
                (print ";"))
              (slot-value sal-path :auxiliary-decls))
    (print "")))

(define (sal-path/display-global-constraints sal-path)
  (unless (null? (slot-value sal-path :global-constraint-list))
    (print "========================")
    (print "Global Constraints")
    (print "========================")
    (display-constraints (slot-value sal-path :global-constraint-list))
    (print "")))

(define (sal-path/display-preamble sal-path)
  (sal-path/display-auxiliary-decls sal-path)
  (sal-path/display-global-constraints sal-path)
    (print "========================")
    (print "Path")
    (print "========================"))

