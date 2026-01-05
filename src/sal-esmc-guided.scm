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

(module sal-esmc-guided
        (include "sal.sch")
        (import sal-esm-engine sal-esmc-core state-to-do-list state-cache sal-esm-support
                sal-esm-options heap)
        (export (sal-esmc/guided-check-invariant engine checking-mode check-prop-curr-proc check-prop-next-proc curr-weight-proc next-weight-proc))
        )

(define (sal-esmc/guided-check-invariant engine checking-mode check-prop-curr-proc check-prop-next-proc curr-weight-proc next-weight-proc)
  (bind-exit (exit-proc)
    (status-message :esm-guided-check-invariant)
    (verbose-message 1 "verifying property using guided search...")
    (let* ((cache (make-state-cache (slot-value engine :num-bits) (slot-value engine :num-objects)))
           (to-do (make-state-to-do-list (slot-value engine :aux-num-bits)))
           (fix-idx-handler (sal-esm/make-fix-to-do-idxs-proc to-do cache))
           (ignored-states? #f)
           (heap (make-heap <fx))
           (add-entry (lambda (entry-idx weight)
                        (if (>= weight 0)
                          (heap/add! heap weight entry-idx)
                          (set! ignored-states? #t))))
           (add-curr-entry-proc (lambda (entry-idx)
                                  (add-entry entry-idx (curr-weight-proc))))
           (add-next-entry-proc (lambda (entry-idx)
                                  (add-entry entry-idx (next-weight-proc))))
           (check-init-proc (esm-engine->check-initial-states-proc engine checking-mode check-prop-curr-proc 
                                                                   fix-idx-handler cache to-do  exit-proc add-curr-entry-proc))
           (check-trans-proc (esm-engine->check-successors-proc engine checking-mode check-prop-curr-proc 
                                                                check-prop-next-proc fix-idx-handler cache to-do exit-proc add-next-entry-proc))
           (update-tick! (sal-esm/mk-tick-counter-core-proc 
                          (lambda (_)
                            (status-message :esm-guided-tick (sc/size cache))
                            (verbose-message 3 "  number of visited states: ~a"
                                             (sc/size cache))))))
      (check-init-proc)
      (sal-esm-engine/set-curr-memory-layout! engine)
      (let loop ()
        (unless (heap/empty? heap)
          (let* ((curr-entry-idx (heap/delete-max! heap))
                 (num-new-states (check-trans-proc curr-entry-idx)))
            (update-tick! num-new-states 0)
            (loop))))
      (status-message :esm-num-visited-states (sc/size cache))
      (verbose-message 2 "  number of visited states: ~a" (sc/size cache))
      (if ignored-states?
        'unknown
        'valid))))
