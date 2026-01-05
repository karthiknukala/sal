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

(module sal-esm-counterexample
        (include "sal.sch")
        (import sal-esm-engine state-cache state-to-do-list sal-path 
                fast-hash-table sal-derived-path state-entry-channel
                queue)
        (export (sal-esm/sign-empty-set-of-initial-states)
                (sal-esm/gen-path engine cache to-do goal-state-entry-idx input-provider-entry-idx)
                (sal-esm/gen-counterexample engine cache to-do goal-state-entry-idx input-provider-entry-idx)
                (sal-esm/gen-cacheless-counterexample engine history-file-name channel depth num-visited)
                (sal-esm/gen-guided-cacheless-counterexample engine history-file-name channel depth))
        )

(define (sal-esm/sign-empty-set-of-initial-states)
  (sign-error "Invalid specification, the set of initial states is empty."))

(define (assignment-table->step assignment-table)
  (make-instance <sal-step> 
                 :assignment-table assignment-table
                 :constraint-list '()))

(define (mk-state->step-proc engine cache to-do)
  (let ((curr->assignment-table! (mk-channel-curr-vars-to-assignment-table-proc engine))
        (input->assignment-table! (mk-channel-inputs-to-assignment-table-proc engine)))
    (lambda (state-idx succ-entry-idx)
      (let ((output-channel (sc/reset-output-channel! cache state-idx))
            (assignment-table (make-eq-hash-table)))
        (curr->assignment-table! output-channel assignment-table)
        (when succ-entry-idx
          (let ((entry-channel (stdl/reset-output-channel! to-do succ-entry-idx)))
            (input->assignment-table! entry-channel assignment-table)))
        (assignment-table->step assignment-table)))))

(define (mk-path engine step-list)
  (sal-step-list/recover-executed-transition-info! step-list (slot-value engine :esm-module))
  (make-instance <sal-concrete-path>
                       :flat-module (slot-value engine :esm-module)
                       :step-info-list step-list
                       :auxiliary-decls '()
                       :global-constraint-list '()))
  

(define (sal-esm/gen-path engine cache to-do goal-state-entry-idx input-provider-entry-idx)
  (let ((state->step (mk-state->step-proc engine cache to-do)))
    (let loop ((curr-entry-idx goal-state-entry-idx)
               (input-provider-entry-idx input-provider-entry-idx)
               (step-list '()))
      (cond
       ((= curr-entry-idx 0)
        (mk-path engine step-list))
       (else
        (let* ((curr-state-idx (stdl/state-idx to-do curr-entry-idx))
               (step (state->step curr-state-idx input-provider-entry-idx))
               (new-entry-idx (stdl/parent-entry-idx to-do curr-entry-idx)))
          (loop new-entry-idx
                curr-entry-idx
                (cons step step-list))))))))

(define (sal-esm/gen-counterexample engine cache to-do goal-state-entry-idx input-provider-entry-idx)
  (sal-esm/display-ce-found-message (sc/size cache))
  (let ((path (sal-esm/gen-path engine cache to-do goal-state-entry-idx input-provider-entry-idx)))
    (if (sal-esm-engine/deadlock?)
      (cons 'deadlock path)
      (cons 'counterexample path))))

(define (sal-esm/gen-cacheless-counterexample engine history-file-name channel depth num-visited)
  (sal-esm/display-ce-found-message num-visited)
  (let ((history-file (open-input-binary-file history-file-name))
        (curr->assignment-table! (mk-channel-curr-vars-to-assignment-table-proc engine))
        (input->assignment-table! (mk-channel-inputs-to-assignment-table-proc engine))
        (step-queue (make-queue)))
    (let loop ((i 0))
      (when (< i depth)
        (let ((assignment-table (make-eq-hash-table)))
          (sec/read-from-file! history-file channel)
          (curr->assignment-table! channel assignment-table)
          (input->assignment-table! channel assignment-table)
          (queue/insert! step-queue (assignment-table->step assignment-table))
          (loop (+ i 1)))))
    (let ((result (mk-path engine (queue->list step-queue))))
      (if (sal-esm-engine/deadlock?)
        (cons 'deadlock result)
        (cons 'counterexample result)))))

(define (sal-esm/gen-guided-cacheless-counterexample engine history-file-name channel depth)
  (sal-esm/display-ce-found-message #f)
  (let ((history-file (open-input-binary-file history-file-name))
        (curr->assignment-table! (mk-channel-curr-vars-to-assignment-table-proc engine))
        (input->assignment-table! (mk-channel-inputs-to-assignment-table-proc engine))
        (step-queue (make-queue)))
    (let loop ((i 0)
               (prev-table #f))
      (cond
       ((< i depth)
        (let ((assignment-table (make-eq-hash-table)))
          (sec/read-from-file! history-file channel)
          (curr->assignment-table! channel assignment-table)
          (when prev-table
            (input->assignment-table! channel prev-table)
            (queue/insert! step-queue (assignment-table->step prev-table)))
          (loop (+ i 1)
                assignment-table)))
       (else
        (when prev-table
          (input->assignment-table! channel prev-table)
          (queue/insert! step-queue (assignment-table->step prev-table))))))
    (let ((result (mk-path engine (queue->list step-queue))))
      (if (sal-esm-engine/deadlock?)
        (cons 'deadlock result)
        (cons 'counterexample result)))))
