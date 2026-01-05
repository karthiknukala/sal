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

(module sat-boolean-cnf-bmc-context
        (include "sal.sch")
        (import sat-boolean-bmc-context sat-boolean-cnf-context sat-bmc-context sal-path fast-hash-table sal-expression sat-context)
        (export <sat-boolean-cnf-bmc-context>
                (sat-boolean-cnf-bmc-context/init! ctx flat-module solver-proc cont-proc)
                (make-sat-boolean-cnf-bmc-context flat-module solver-proc cont-proc))
        )


(define-class <sat-boolean-cnf-bmc-context> (<sat-boolean-cnf-context> <sat-boolean-bmc-context>) ())

(define (sat-boolean-cnf-bmc-context/init! ctx flat-module solver-proc cont-proc)
  (sat-boolean-bmc-context/init! ctx flat-module)
  (sat-boolean-cnf-context/init! ctx solver-proc
                                 (lambda ()
                                   (cont-proc ctx))))

(define (make-sat-boolean-cnf-bmc-context flat-module solver-proc cont-proc)
  (let ((ctx (make-instance <sat-boolean-cnf-bmc-context>)))
    (sat-boolean-cnf-bmc-context/init! ctx flat-module solver-proc cont-proc)
    ctx))

(define-method (sat-bmc-context/id-at (ctx <sat-boolean-cnf-bmc-context>) (id <primitive>) (step <primitive>))
  (let ((result (slot-value ctx :next-idx)))
    (set-slot-value! ctx :next-idx (+ result 1))
    result))

(define-method (sat-bmc-context/make-path (ctx <sat-boolean-cnf-bmc-context>))
  (let* ((assignment-list (sat-context/solve ctx)) ;; assignment-list is a list of integers
         (num-steps (+ (slot-value ctx :max-step) 1))
         (step-vector (make-vector num-steps)))
    (cond 
     (assignment-list
      (let loop ((i 0))
        (when (< i num-steps)
          (vector-set! step-vector i (make-instance <sal-step>
                                                    :assignment-table (make-eq-hash-table)
                                                    :constraint-list '()))
          (loop (+ i 1))))
      ;; Use explicit loop instead of for-each for Bigloo 4.x compatibility
      (let loop ((assignments assignment-list))
        (unless (null? assignments)
          (let* ((assignment (car assignments))
                 (pos? (> assignment 0))
                 (idx (if pos? assignment (- 0 assignment)))
                 (pair (sat-bmc-context/original-decl ctx idx)))
            (when pair
              (let* ((decl (car pair))
                     (step-idx (cdr pair))
                     (step (vector-ref step-vector step-idx)))
                (when (instance-of? decl <sal-state-var-decl>)
                  (eq-hash-table/put! (slot-value step :assignment-table) decl (if pos? (make-sal-true decl) (make-sal-false decl)))))))
          (loop (cdr assignments))))
      (make-instance <sal-concrete-path>
                     :flat-module (slot-value ctx :flat-module)
                     :step-info-list (vector->list step-vector)
                     :auxiliary-decls '()
                     :global-constraint-list '()))
     (else
      #f))))
    



