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

(module sal-esm-guided-simulation
        (include "sal.sch")
        (import state-entry-channel random sal-esm-engine
                sal-esm-action sal-esm-counterexample 
                sal-esm-support sal-esm-options tmp-files
                gmp-scheme)
        (export (sal-esm/guided-simulation engine curr-state-weight-proc next-state-weight-proc)
                (sal-esmc/guided-cacheless-check-invariant engine checking-mode check-prop-curr-proc curr-state-weight-proc next-state-weight-proc))
        )

(define *available-channels* '())

(define (make-channel! num-bits num-objs)
  (if (null? *available-channels*)
    (make-state-entry-channel num-bits num-objs)
    (let ((result (car *available-channels*)))
      (set! *available-channels* (cdr *available-channels*))
      result)))

(define (recycle-channel! channel)
  (push! channel *available-channels*))

(define (recycle-channels! channel-list)
  (for-each recycle-channel! channel-list))
              
(define-macro (add-state-proc! curr?)
  `(begin
     (set! curr-weight (state-weight-proc))
     (when (mpq? curr-weight)
       (set! curr-weight (mpq->integer curr-weight)))
     (unless (number? curr-weight)
       (sign-error "The weight function must return an integer."))
     (when (or (not min-weight)
               (< curr-weight min-weight))
       (set! min-weight curr-weight))
     (when (>= curr-weight max-weight)
       (let ((channel (make-channel! num-bits num-objs)))
         (sec/input-reset! channel)
         ,(if curr?
            '(copy-curr-state-to-channel! channel)
            '(begin
              (copy-next-state-to-channel! channel)
              (copy-selection-vars-to-channel! channel)))
         (cond
          ((= curr-weight max-weight)
           (push! channel best-states))
          (else
           (recycle-channels! best-states)
           (set! max-weight curr-weight)
           (set! best-states (list channel))))))))

(define (pick-state best-states)
  (let ((select-idx (rand (length best-states)))
        (result #f))
    (let loop ((best-states best-states)
               (idx 0))
      [assert (best-states) (not (null? best-states))]
      (cond
       ((= idx select-idx)
        (set! result (car best-states)))
       (else
        (recycle-channel! (car best-states))
        (loop (cdr best-states)
              (+ idx 1)))))
    result))

(define (engine->pick-best-initial-state-proc engine state-weight-proc)
  (let* ((copy-curr-state-to-channel! (mk-copy-curr-state-to-channel-proc engine))
         (initialization-action (slot-value engine :initialization-action))
         (num-bits (+ (slot-value engine :num-bits) (slot-value engine :aux-num-bits)))
         (num-objs (slot-value engine :num-objects))
         (num-initial-states 0)
         (update-tick! (sal-esm/mk-tick-counter-core-proc 
                        (lambda (_)
                          (status-message :esm-guided-tick-initial-state num-initial-states)
                          (verbose-message 3 "    number of initial states generated: ~a" num-initial-states))))
         (curr-weight #f)
         (max-weight -1)
         (min-weight #f)
         (best-states '()))
    (lambda ()
      (status-message :esm-guided-picking-initial-state)
      (verbose-message 2 "  picking the a maximal initial state...")
      (sal-esm-engine/reset-var-vector! engine)
      (esm/for-each-alternative 
       initialization-action
       (lambda ()
         (set! num-initial-states (+ num-initial-states 1))
         (update-tick! 1 1)
         (add-state-proc! #t)))
      (status-message :esm-guided-num-initial-states num-initial-states)
      (verbose-message 3 "number of initial states: ~a" num-initial-states)
      (unless (> num-initial-states 0)
        (sal-esm/sign-empty-set-of-initial-states))
      (verbose-message 10 "initial state min. weight: ~a" min-weight)
      (verbose-message 10 "selected initial state weight: ~a" max-weight)
      (pick-state best-states))))


(define (engine->pick-best-succ-state-proc engine checking-mode check-prop-curr-proc state-weight-proc show-info?)
  (let ((set-curr-state! (mk-set-curr-state-proc engine))
        (copy-next-state-to-channel! (mk-copy-next-state-to-channel-proc engine))
        (copy-selection-vars-to-channel! (mk-copy-selection-vars-to-channel-proc engine))
        (check-all-combinations? (eq? checking-mode 'all-input-combinations))
        (transition-action (slot-value engine :transition-action))
        (num-bits (+ (slot-value engine :num-bits) (slot-value engine :aux-num-bits)))
        (num-objs (slot-value engine :num-objects)))
    (lambda (channel history-file)
      (let ((num-successors 0)
            (curr-weight #f)
            (max-weight -1)
            (min-weight #f)
            (first? #t)
            (best-states '()))
        (bind-exit (exit)
          (sec/write-to-file! history-file channel)
          (sec/output-reset! channel)
          (set-curr-state! channel)
          (when *esmc-show-visited-states?*
            (sal-esm-engine/display-curr-state engine))
          (esm/for-each-alternative 
           transition-action
           (lambda ()
             (when *esmc-show-fired-transitions?*
               (sal-esm-engine/display-transition-info engine))
             (cond
              ((and (or first? check-all-combinations?)
                    (not (check-prop-curr-proc)))
               (exit 'counterexample))
              ((sal-esm-engine/deadlock?)
               (exit 'deadlock))
              (else
               (set! num-successors (+ num-successors 1))
               (add-state-proc! #f)
               (when show-info?
                 (verbose-message 10 "successor state weight: ~a" curr-weight))))
             (set! first? #f)))
          (when (= num-successors 0)
            (exit 'unknown))
          (when show-info?
            (status-message :esm-guided-num-successors num-successors)
            (verbose-message 10 "number of successors: ~a" num-successors)
            (verbose-message 10 "selected successor state weight: ~a" max-weight))
          (pick-state best-states))))))


(define (sal-esm/guided-simulation engine curr-state-weight-proc next-state-weight-proc)
  (status-message :esm-starting-guided-simulation)
  (verbose-message 1 "starting guided simulation...")
  (dlet ((*esmc-show-visited-states?* (not *path-explorer-silent?*))
         (*esmc-show-fired-transitions?* (not *path-explorer-silent?*)))
    (sal-esmc/guided-cacheless-check-invariant-core engine 'optimized (lambda () #t) curr-state-weight-proc next-state-weight-proc #t)))

(define (sal-esmc/guided-cacheless-check-invariant engine checking-mode check-prop-curr-proc curr-state-weight-proc next-state-weight-proc)
  (status-message :esm-starting-guided-simulation-check-invariant)
  (verbose-message 1 "verifying property using guided cacheless search...")
  (sal-esmc/guided-cacheless-check-invariant-core engine checking-mode check-prop-curr-proc curr-state-weight-proc next-state-weight-proc #f))

(define (sal-esmc/guided-cacheless-check-invariant-core engine checking-mode check-prop-curr-proc curr-state-weight-proc next-state-weight-proc show-info?)
  (let* ((bound *esm-max-depth*)
         (num-paths *esm-cacheless-num-paths*)
         (history-file-name (sal/setup-tmp-file! "execution.history"))
         (pick-initial! (engine->pick-best-initial-state-proc engine curr-state-weight-proc))
         (pick-succ! (engine->pick-best-succ-state-proc engine checking-mode check-prop-curr-proc next-state-weight-proc show-info?)))
    (verbose-message 2 "  using history file: `~a'" history-file-name)
    (let main-loop ((path 1))
      (if (and num-paths (> path num-paths))
        'unknown
        (let ((history-file (open-output-binary-file history-file-name))) 
          (status-message :esm-guided-starting-new-path)
          (verbose-message 1 "starting path: ~a" path)
          (let ((channel (pick-initial!)))
            (sal-esm-engine/set-curr-memory-layout! engine)
            (let loop ((channel channel)
                       (depth 1))
              (cond
               ((and bound (> depth bound))
                (close-binary-port history-file)
                (main-loop (+ path 1)))
               (else
                (let ((new-channel (pick-succ! channel history-file)))
                  (cond
                   ((state-entry-channel? new-channel)
                    (recycle-channel! channel)
                    (loop new-channel (+ depth 1)))
                   ((eq? new-channel 'unknown)
                    (close-binary-port history-file)
                    (main-loop (+ path 1)))
                   (else
                    [assert (new-channel) (memq new-channel '(counterexample deadlock))]
                    (close-binary-port history-file)
                    (sal-esm/gen-guided-cacheless-counterexample engine history-file-name channel depth)))))))))))))
