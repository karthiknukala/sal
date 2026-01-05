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

(module sal-esm-random-simulation
        (include "sal.sch")
        (import sal-esm-engine state-entry-channel queue
                sal-esm-counterexample fast-hash-table
                sal-esm-action sal-esm-options tmp-files
                sal-esm-support)
        (export (sal-esm/random-simulation engine)
                (sal-esmc/cacheless-check-invariant engine check-prop-curr-proc))
        )

(define *path-explorer-state-evaluator-proc* #f)

(define (engine->pick-initial-state-proc engine)
  (let ((copy-curr-state-to-channel! (mk-copy-curr-state-to-channel-proc engine))
        (initialization-action (slot-value engine :initialization-action)))
    (lambda (channel)
      (sal-esm-engine/reset-var-vector! engine)
      (unless (esm/execute-action initialization-action)
        (sal-esm/sign-empty-set-of-initial-states))
      (sec/input-reset! channel)
      (copy-curr-state-to-channel! channel))))

(define (engine->pick-succ-state-proc engine check-prop-curr-proc)
  (let ((set-curr-state! (mk-set-curr-state-proc engine))
        (copy-next-state-to-channel! (mk-copy-next-state-to-channel-proc engine))
        (copy-selection-vars-to-channel! (mk-copy-selection-vars-to-channel-proc engine))
        (transition-action (slot-value engine :transition-action)))
    (lambda (channel1 channel2 history-file)
      (sec/output-reset! channel1)
      (set-curr-state! channel1)
      (when *esmc-show-visited-states?*
        (sal-esm-engine/display-curr-state engine))
      (cond
       ((not (esm/execute-action transition-action))
        'unknown)
       (else 
        (when *esmc-show-fired-transitions?*
          (sal-esm-engine/display-transition-info engine))
        (copy-selection-vars-to-channel! channel1)
        (sec/write-to-file! history-file channel1)
        (cond
         ((not (check-prop-curr-proc))
          'counterexample)
         ((sal-esm-engine/deadlock?)
          'deadlock)
         (else
          (sec/input-reset! channel2)
          (copy-next-state-to-channel! channel2)
          'ok)))))))

(define (sal-esm/random-simulation engine)
  (dlet ((*esmc-show-visited-states?* (not *path-explorer-silent?*))
         (*esmc-show-fired-transitions?* (not *path-explorer-silent?*)))
    (status-message :esm-starting-random-simulation)
    (verbose-message 1 "starting random simulation...")
    (sal-esmc/cacheless-check-invariant-core engine (lambda () #t))))

(define (sal-esmc/cacheless-check-invariant engine check-prop-curr-proc)
  (status-message :esm-starting-random-check-invariant)
  (verbose-message 1 "verifying property using cacheless search...")
  (sal-esmc/cacheless-check-invariant-core engine check-prop-curr-proc))
  
(define (sal-esmc/cacheless-check-invariant-core engine check-prop-curr-proc)
  (let* ((bound *esm-max-depth*)
         (num-paths *esm-cacheless-num-paths*)
         (num-bits (+ (slot-value engine :num-bits) (slot-value engine :aux-num-bits)))
         (num-objs (slot-value engine :num-objects))
         (channel1 (make-state-entry-channel num-bits num-objs))
         (channel2 (make-state-entry-channel num-bits num-objs))
         (history-file-name (sal/setup-tmp-file! "execution.history"))
         (pick-initial! (engine->pick-initial-state-proc engine))
         (pick-succ! (engine->pick-succ-state-proc engine check-prop-curr-proc))
         (update-tick! (sal-esm/mk-tick-counter-core-proc 
                        (lambda (depth)
                          (status-message :esm-random-tick depth)
                          (verbose-message 3 "  number of visited states in the current path: ~a"
                                           depth))))
         (reset-tick! (lambda (depth) (update-tick! (- depth) 0))))
    (verbose-message 2 "  using history file: `~a'" history-file-name)
    (let main-loop ((path 1)
                    (visited 0))
      (if (and num-paths (> path num-paths))
        'unknown
        (let ((history-file (open-output-binary-file history-file-name))) 
          (status-message :esm-random-starting-new-path)
          (verbose-message 1 "starting path: ~a" path)
          (pick-initial! channel1)
          (sal-esm-engine/set-curr-memory-layout! engine)
          (let loop ((channel1 channel1)
                     (channel2 channel2)
                     (depth 1))
            (update-tick! 1 depth)
            (cond
             ((and bound (> depth bound))
              (close-binary-port history-file)
              (reset-tick! depth) ;; reset tick counter
              (main-loop (+ path 1) (+ visited depth)))
             (else
              (let ((succ-res (pick-succ! channel1 channel2 history-file)))
                (case succ-res
                  ((ok)
                   (loop channel2 channel1 (+ depth 1)))
                  ((unknown)
                   (close-binary-port history-file)
                   (reset-tick! depth)
                   (main-loop (+ path 1) (+ visited depth)))
                  (else
                   [assert (succ-res) (memq succ-res '(counterexample deadlock))]
                   (close-binary-port history-file)
                   (sal-esm/gen-cacheless-counterexample engine history-file-name channel1 depth visited))))))))))))


      
  
