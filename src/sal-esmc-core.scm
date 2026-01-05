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

(module sal-esmc-core
        (include "sal.sch")
        (import sal-esm-engine state-to-do-list state-cache sal-esm-action
                sal-esm-counterexample sal-esm-support sal-esm-options)
        (export 
         (sal-esm/make-fix-to-do-idxs-proc to-do-lst cache)
         (esm-engine->check-initial-states-proc engine checking-mode check-prop-proc fix-idx-handler cache to-do exit-proc 
                                                reg-new-entry-proc)
         (esm-engine->check-successors-proc engine checking-mode check-prop-curr-proc check-prop-next-proc fix-idx-handler
                                            cache to-do exit-proc reg-new-entry-proc))
        )

(define (sal-esm/make-fix-to-do-idxs-proc to-do-lst cache)
  (lambda (new-idx-proc)
    (let ((last-idx (stdl/next-idx to-do-lst)))
      (let loop ((entry-idx 1))
        (when (< entry-idx last-idx)
          (let* ((curr-state-idx (stdl/state-idx to-do-lst entry-idx))
                 (new-state-idx (new-idx-proc curr-state-idx)))
            (stdl/set-state-idx! to-do-lst entry-idx new-state-idx)
            (loop (+ entry-idx 1))))))
    #unspecified))

(define (esm-engine->check-initial-states-proc engine checking-mode check-prop-proc fix-idx-handler cache to-do exit-proc reg-new-entry-proc)
  (let* ((copy-curr-state-to-channel! (mk-copy-curr-state-to-channel-proc engine))
         (initialization-action (slot-value engine :initialization-action))
         (optimized-check? (eq? checking-mode 'optimized))
         (num-initial-states 0)
         (update-tick! (sal-esm/mk-tick-counter-core-proc 
                        (lambda (_)
                          (status-message :esm-tick-initial-states num-initial-states)
                          (verbose-message 3 "    number of initial states generated: ~a" num-initial-states)))))
    (lambda ()
      (verbose-message 2 "  computing set of initial states...")
      (sal-esm-engine/reset-var-vector! engine)
      (esm/for-each-alternative 
       initialization-action
       (lambda ()
         ;; (print *esm-var-vector*)
         ;; (display-curr-states engine)

         (set! num-initial-states (+ num-initial-states 1))
         (update-tick! 1 1)
         (let ((input-channel (sc/reset-input-channel! cache)))
           (copy-curr-state-to-channel! input-channel)
           (let ((state-idx (sc/insert-channel! cache input-channel fix-idx-handler)))
             (when state-idx
               (let ((entry-idx (stdl/insert-channel! to-do (stdl/reset-input-channel! to-do state-idx 0))))
                 (when reg-new-entry-proc
                   (reg-new-entry-proc entry-idx))
                 (when (and optimized-check? (not (check-prop-proc)))
                   (exit-proc (sal-esm/gen-counterexample engine cache to-do entry-idx #f)))))))))
      (status-message :esm-num-initial-states (sc/size cache))
      (verbose-message 3 "  number of initial states: ~a" (sc/size cache))
      (unless (> (sc/size cache) 0)
        (sal-esm/sign-empty-set-of-initial-states)))))

(define (esm-engine->check-successors-proc engine checking-mode check-prop-curr-proc check-prop-next-proc fix-idx-handler cache to-do exit-proc 
                                           reg-new-entry-proc)
  (let* ((set-curr-state! (mk-set-curr-state-proc engine))
         (copy-next-state-to-channel! (mk-copy-next-state-to-channel-proc engine))
         (copy-selection-vars-to-channel! (mk-copy-selection-vars-to-channel-proc engine))
         (transition-action (slot-value engine :transition-action))
         (optimized-check? (eq? checking-mode 'optimized))
         (check-all-combinations? (eq? checking-mode 'all-input-combinations)))
    (lambda (curr-entry-idx)
      (let* ((curr-idx (stdl/state-idx to-do curr-entry-idx))
             (num-new-states 0)
             (first-succ? #t)
             (output-channel (sc/reset-output-channel! cache curr-idx)))
        (set-curr-state! output-channel)
        (when *esmc-show-visited-states?*
          (sal-esm-engine/display-curr-state engine))
        (esm/for-each-alternative 
         transition-action
         (lambda ()
           (when *esmc-show-fired-transitions?*
             (sal-esm-engine/display-transition-info engine))
           ;; (print *esm-var-vector*)
           (when (or (and (not optimized-check?)
                          (or first-succ? check-all-combinations?)
                          (not (check-prop-curr-proc)))
                     (sal-esm-engine/deadlock?))
             ;; create a fake entry to contain the value of input variables...
             (let ((channel (stdl/reset-input-channel! to-do 0 curr-entry-idx)))
               (copy-selection-vars-to-channel! channel)
               (let ((fake-entry-idx (stdl/insert-channel! to-do channel)))
                 (exit-proc (sal-esm/gen-counterexample engine cache to-do curr-entry-idx fake-entry-idx)))))
           (set! first-succ? #f)
           (let ((input-channel (sc/reset-input-channel! cache)))
             (copy-next-state-to-channel! input-channel)
             (let ((state-idx (sc/insert-channel! cache input-channel fix-idx-handler)))
               (when state-idx
                 (set! num-new-states (+ num-new-states 1))
                 (let ((channel (stdl/reset-input-channel! to-do state-idx curr-entry-idx)))
                   (copy-selection-vars-to-channel! channel)
                   (let ((entry-idx (stdl/insert-channel! to-do channel)))
                     (when reg-new-entry-proc
                       (reg-new-entry-proc entry-idx))
                     ;; (sal-esm-engine/display-next-state engine)
                     (when (and optimized-check? (not (check-prop-next-proc)))
                       (exit-proc (sal-esm/gen-counterexample engine cache to-do entry-idx #f)))
                     ;; (print "ok...")
                     )))))))
        num-new-states))))
