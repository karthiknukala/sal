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

(module sal-esmc-bfs
        (include "sal.sch")
        (import sal-esm-engine sal-esmc-core state-to-do-list state-cache sal-esm-support
                sal-esm-options)
        (export (sal-esmc/bfs-check-invariant engine checking-mode check-prop-curr-proc check-prop-next-proc))
        )

(define (sal-esmc/bfs-check-invariant engine checking-mode check-prop-curr-proc check-prop-next-proc)
  (bind-exit (exit-proc)
    (status-message :esm-starting-bfs-check-invariant)
    (verbose-message 1 "verifying property using breadth-first search...")
    (let* ((cache (make-state-cache (slot-value engine :num-bits) (slot-value engine :num-objects)))
           (to-do (make-state-to-do-list (slot-value engine :aux-num-bits)))
           (fix-idx-handler (sal-esm/make-fix-to-do-idxs-proc to-do cache))
           (check-init-proc (esm-engine->check-initial-states-proc engine checking-mode check-prop-curr-proc 
                                                                   fix-idx-handler cache to-do  exit-proc #f))
           (check-trans-proc (esm-engine->check-successors-proc engine checking-mode check-prop-curr-proc 
                                                                check-prop-next-proc fix-idx-handler cache to-do exit-proc #f))
           (update-tick! (sal-esm/mk-tick-counter-proc cache to-do))
           (entry-idx-for-next-depth #unspecified)
           (depth 1))
      (check-init-proc)
      (set! entry-idx-for-next-depth (stdl/next-idx to-do))
      (sal-esm-engine/set-curr-memory-layout! engine)
      (let loop ()
        (unless (stdl/empty? to-do)
          (let* ((curr-entry-idx (stdl/remove-front! to-do))
                 (_ (when (= curr-entry-idx entry-idx-for-next-depth)
                      (set! depth (+ depth 1))
                      (when (and *esm-max-depth* (> depth *esm-max-depth*))
                        (exit-proc 'unknown))
                      (set! entry-idx-for-next-depth (stdl/next-idx to-do))))
                 (num-new-states (check-trans-proc curr-entry-idx)))
            (update-tick! num-new-states depth)
            (loop))))
      (status-message :esm-visited-states (sc/size cache))
      (verbose-message 2 "  number of visited states: ~a" (sc/size cache))
      'valid)))


