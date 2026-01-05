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

(module sal-esmc-ndfs
        (include "sal.sch")
        (import sal-esmc-idfs-support sal-esmc-dfs-support sal-esm-options
                sal-esm-engine state-cache state-to-do-list sal-esm-action
                state-entry-channel sal-esmc-core sal-esm-support)
        (export (sal-esmc/ndfs-check-liveness engine check-accepting-proc)
                (sal-esmc/nidfs-check-liveness engine check-accepting-proc))
        )

(define (sal-esmc/ndfs-check-liveness engine check-accepting-proc)
  (status-message :esm-starting-ndfs-check-liveness)
  (verbose-message 1 "verifying liveness property using nested depth-first search...")
  (ndfs-check-liveness-core engine check-accepting-proc *esm-max-depth*))

(define (sal-esmc/nidfs-check-liveness engine check-accepting-proc)
  (status-message :esm-starting-nidfs-check-liveness)
  (verbose-message 1 "verifying liveness property using nested iterative depth-first search...")
  (sal-esmc/idfs-wrapper (lambda (bound)
                           (ndfs-check-liveness-core engine check-accepting-proc bound))))

(define (ndfs-check-liveness-core engine check-accepting-proc bound)
  (bind-exit (exit-proc)
    (let* ((cache1 (make-state-cache (slot-value engine :num-bits) (slot-value engine :num-objects)))
           (cache2 (make-state-cache (slot-value engine :num-bits) (slot-value engine :num-objects)))
           (to-do (make-state-to-do-list (slot-value engine :aux-num-bits)))
           (ndfs-to-do (make-state-to-do-list (slot-value engine :aux-num-bits)))
           (fix-idx-handler1 (sal-esm/make-fix-to-do-idxs-proc to-do cache1))
           (fix-idx-handler2 (sal-esm/make-fix-to-do-idxs-proc to-do cache2))
           (gen-init-proc (esm-engine->check-initial-states-proc engine #f (lambda () #t)
                                                                 fix-idx-handler1 cache1 
                                                                 to-do exit-proc #f))
           (set-curr-state! (mk-set-curr-state-proc engine))
           (copy-next-state-to-channel! (mk-copy-next-state-to-channel-proc engine))
           (copy-selection-vars-to-channel! (mk-copy-selection-vars-to-channel-proc engine))
           (transition-action (slot-value engine :transition-action))
           (update-tick! (sal-esm/mk-tick-counter-core-proc 
                          (lambda (depth)
                            (verbose-message 3 "  number of visited states: ~a, depth: ~a"
                                             (+ (sc/size cache1) (sc/size cache2))
                                             depth))))
           (check-bound? (number? bound))
           (bound-reached? #f)
           (seed-idx #f)
           (seed-channel (sec/copy (sc/reset-input-channel! cache1)))
           [ndfs-add-state! 
            ;; Insert the next state variables in *esm-var-vector* into cache2.
            ;; Return #f, if the state is already in cache2.
            ;; If it is a new state in cache2, then insert an entry in ndfs-to-do list.
            (lambda (curr-entry-idx)
              (let ((input-channel (sc/reset-input-channel! cache2)))
                (copy-next-state-to-channel! input-channel)
                (let ((state-idx (sc/insert-channel! cache2 input-channel fix-idx-handler2)))
                  (and state-idx
                       (let ((to-do-channel (stdl/reset-input-channel! ndfs-to-do state-idx curr-entry-idx)))
                         (copy-selection-vars-to-channel! to-do-channel)
                         (stdl/insert-channel! ndfs-to-do to-do-channel))))))]
           [ndfs-successors!
            ;; Process the successors of the state associated with then entry idx 'curr-entry-idx',
            ;; in the nested depth-first search loop.
            ;; Return the number of new successor states.
            (lambda (curr-entry-idx)
              (let* ((curr-idx (stdl/state-idx to-do curr-entry-idx))
                     (output-channel (sc/reset-output-channel! cache2 curr-idx)))
                (cond
                 ((sec/equal? output-channel seed-channel)
                  (exit-proc (sal-esm/gen-ndfs-counterexample engine cache1 cache2 to-do ndfs-to-do curr-entry-idx seed-idx #f)))
                 (else
                  (set-curr-state! output-channel)
                  (let ((num-new-states 0))
                    (esm/for-each-alternative
                     transition-action
                     (lambda ()
                       (when (ndfs-add-state! curr-entry-idx)
                         (set! num-new-states (+ num-new-states 1)))))
                    num-new-states)))))]
           [dfs-core!
            ;; Core procedure to perform depth-first search.
            ;; It is used to implement the main and nested depth first searches.
            (lambda (to-do-list dfs-successors-proc)
              (let loop ((depth 1))
                (unless (stdl/empty? to-do-list)
                  (cond
                   ((and check-bound? (> depth bound))
                    (set! bound-reached? #t)
                    (loop (sal-esmc/pop-states! to-do-list depth)))
                   (else
                    (let* ((curr-entry-idx (stdl/top to-do-list))
                           (num-new-states (dfs-successors-proc curr-entry-idx)))
                      (update-tick! num-new-states depth)
                      (if (= num-new-states 0)
                        (loop (sal-esmc/pop-states! to-do-list depth))
                        (loop (+ depth 1)))))))))]
           [ndfs! 
            ;; Nested depth first search main loop.
            (lambda (new-seed-channel)
              [assert (ndfs-to-do) (stdl/empty? ndfs-to-do)]
              (verbose-message 3 "  starting nested depth-first search...")
              (sec/copy! seed-channel new-seed-channel)
              ;; Remark: the next state in *esm-var-vector* is a successor of the state stored in new-seed-channel.
              (ndfs-add-state! 0)
              (dfs-core! ndfs-to-do ndfs-successors!)
              (verbose-message 3 "  nested depth-first search completed."))]
           [dfs-add-state! 
            ;; Insert the next state variables in *esm-var-vector* into cache1.
            ;; Return #f, if the state is already in cache1.
            ;; If it is a new state in cache1, then insert an entry in to-do list.
            (lambda (curr-entry-idx)
              (let ((input-channel (sc/reset-input-channel! cache1)))
                (copy-next-state-to-channel! input-channel)
                (let ((state-idx (sc/insert-channel! cache1 input-channel fix-idx-handler1)))
                  (and state-idx
                       (let ((to-do-channel (stdl/reset-input-channel! to-do state-idx curr-entry-idx)))
                         (copy-selection-vars-to-channel! to-do-channel)
                         (stdl/insert-channel! to-do to-do-channel))))))]
           [dfs-successors!
            ;; Process the successors of the state associated with then entry idx 'curr-entry-idx',
            ;; in the main depth-first search loop.
            (lambda (curr-entry-idx)
              (let* ((curr-idx (stdl/state-idx to-do curr-entry-idx))
                     (curr-channel (sc/reset-output-channel! cache1 curr-idx)))
                (set-curr-state! curr-channel)
                (let ((accepting? (check-accepting-proc))
                      (num-new-states 0))
                  (esm/for-each-alternative
                   transition-action
                   (lambda ()
                     (cond 
                      (accepting?
                       (ndfs! curr-channel))
                      (else
                       (when (dfs-add-state! curr-entry-idx)
                         (set! num-new-states (+ num-new-states 1)))))))
                  num-new-states)))]
           [dfs!
            ;; main depth first-search loop
            (lambda ()
              (dfs-core! to-do dfs-successors!))])
      (gen-init-proc)
      (sal-esm-engine/set-curr-memory-layout! engine)
      (dfs!)
      (status-message :esm-num-visited-states (+ (sc/size cache1) (sc/size cache2)))
      (verbose-message 2 "  number of visited states: ~a" (+ (sc/size cache1) (sc/size cache2)))
      (if bound-reached?
        'unknown
        'valid))))

(define (sal-esm/gen-ndfs-counterexample engine cache1 cache2 to-do ndfs-to-do curr-entry-idx seed-idx fake-entry-idx)
  'invalid)
