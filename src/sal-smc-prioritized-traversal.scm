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

(module sal-smc-prioritized-traversal
        (include "sal.sch")
        (include "fast-hash-table.sch")
        (import bdd sal-bdd-fsm sal-bdd-cluster sal-bdd-context front-end sal-flat-modules
                symbol-table sal-expression sal-path queue sal-pp runtime sal2bdd
                sal-path-pp sal-derived-path sal-smc-core heap bdd-util)
        (export 
         (sal-bdd-fsm/dfs-prioritized-find-trace fsm start-set target-set threshold)
         (sal-bdd-fsm/bfs-prioritized-find-trace fsm start-set target-set threshold)
         (sal-bdd-fsm/min-prioritized-find-trace fsm start-set target-set threshold)
         (sal-bdd-fsm/greedy-prioritized-find-trace fsm start-set target-set threshold))
        )

;; TODO : remove state-lists

(define (target-found? target-set state-list)
  (bind-exit (exit)
    (for-each (lambda (state)
                (let ((target-found (bdd/and target-set state)))
                  (unless (bdd/false? target-found)
                    (exit target-found))))
              state-list)
    #f))

(define (sal-bdd-fsm/prioritized-find-trace-core fsm start-set target-set queue-insert-proc! queue-remove-proc! queue-empty-proc? queue-info-proc threshold)
  (let* ((m (sal-bdd-fsm/manager fsm))
         (collect! (make-gc-collector-for m)))
    (cond
     ((target-found? target-set (list start-set)) => 
      (lambda (result)
        (values result start-set (list result))))
     (else
      (queue-insert-proc! (list start-set))
      (let loop ((reachable-states start-set))
        (collect!)
        (cond
         ((queue-empty-proc?)
          (verbose-message 3 "  number of visited states: ~a" (sal-bdd-fsm/num-states fsm reachable-states))
          (values #f #f #f))
         (else
          (queue-info-proc reachable-states)
          (let* ((curr-path (queue-remove-proc!))
                 (frontier (car curr-path)))
            [assert (curr-path) (and (list? curr-path) (for-all bdd? curr-path))]
            (let* ((image-list1 (sal-bdd-fsm/disj-image fsm frontier))
                   (image-list2 (bdd-or/affinity-cluster image-list1 threshold))
                   (image-list (map (lambda (image-bdd)
                                      (bdd/diff image-bdd reachable-states))
                                    image-list2))
                   (new-reachable-states (fold-left bdd/or reachable-states image-list)))
              (cond 
               ((target-found? target-set image-list) => 
                (lambda (result)
                  (verbose-message 3 "  number of visited states: ~a" (sal-bdd-fsm/num-states fsm reachable-states))
                  (values result (bdd/or result reachable-states) (cons result curr-path))))
               (else
                (for-each (lambda (image-bdd)
                            (queue-insert-proc! (cons image-bdd curr-path)))
                          image-list)
                (loop new-reachable-states))))))))))))
                            
(define (compute-split-var-idx fsm bdd)
  (let ((curr-indices (sal-bdd-fsm/curr-indices fsm))
        (support (bdd/support bdd))
        (m (bdd/manager bdd)))
    [assert (curr-indices) (not (null? curr-indices))]
    (let loop ((curr-indices curr-indices)
               (split-var-idx #f)
               (estimate-min-size (bdd/size bdd)))
      (cond
       ((null? curr-indices)
        split-var-idx)
       (else
        (let ((curr-idx (car curr-indices)))
          (if (bdd/false? (bdd/and (bdd/ith-var m curr-idx) support))
            (loop (cdr curr-indices) split-var-idx estimate-min-size)
            (let ((curr-estimate (max (bdd/estimate-cofactor bdd curr-idx #t) (bdd/estimate-cofactor bdd curr-idx #f))))
              (if (< curr-estimate estimate-min-size)
                (loop (cdr curr-indices) curr-idx curr-estimate)
                (loop (cdr curr-indices) split-var-idx estimate-min-size))))))))))
              
(define (sal-bdd-fsm/split-bdd fsm bdd)
  (let* ((m (bdd/manager bdd))
         (split-var-idx (compute-split-var-idx fsm bdd)))
    (if (not split-var-idx)
      (values #f #f #f) ;; it was not possible to split the bdd
      (let* ((var (bdd/ith-var m split-var-idx))
             (bdd1 (bdd/and var (bdd/cofactor bdd var)))
             (bdd2 (bdd/and (bdd/not var) (bdd/cofactor bdd (bdd/not var)))))
        [assert (bdd bdd1 bdd2) (bdd/eq? bdd (bdd/or bdd1 bdd2))]
        ;; (breakpoint "split" (fsm bdd bdd1 bdd2 split-var-idx) #t)
        (values bdd1 bdd2 var)))))

(define (sal-bdd-fsm/dfs-prioritized-find-trace fsm start-set target-set threshold)
  (let* ((queue '())
         (queue-insert-proc-core! (lambda (curr-path)
                                    (unless (bdd/false? (car curr-path))
                                      (set! queue (cons curr-path queue))
                                      [assert (queue)
                                              (for-all (lambda (path)
                                                         (and (list? path) (for-all bdd? path)))
                                                       queue)])))
         (queue-insert-proc! (lambda (curr-path)
                               (let loop ((curr-path curr-path))
                                 (let ((s-size (bdd/size (car curr-path))))
                                   (if (< s-size threshold)
                                     (queue-insert-proc-core! curr-path)
                                     (multiple-value-bind
                                         (s1 s2 var)
                                         (sal-bdd-fsm/split-bdd fsm (car curr-path))
                                       (cond
                                        ((and s1 s2
                                              (< (bdd/size s1) s-size)
                                              (< (bdd/size s2) s-size))
                                         [assert (s1 s2 curr-path) (bdd/eq? (bdd/or s1 s2) (car curr-path))]
                                         (loop (cons s1 (cdr curr-path)))
                                         (loop (cons s2 (cdr curr-path))))
                                        (else
                                         (queue-insert-proc-core! curr-path)))))))))
         (queue-remove-proc! (lambda ()
                               [assert (queue) (not (null? queue))]
                               (let ((result (car queue)))
                                 (set! queue (cdr queue))
                                 result)))
         (queue-empty-proc? (lambda ()
                              (null? queue)))
         (queue-info-proc (lambda (rs)
                            (when (not (null? queue))
                              (let ((curr-frontier-info (car queue)))
                                (verbose-message 2 "  depth: ~a, frontiers to process: ~a" (length curr-frontier-info) (length queue))
                                (verbose-message 3 "    frontier size: ~a nodes, reached nodes size: ~a" 
                                                 (bdd/size (car curr-frontier-info))
                                                 (bdd/size rs)))))))
    (sal-bdd-fsm/prioritized-find-trace-core fsm start-set target-set queue-insert-proc! queue-remove-proc! queue-empty-proc? queue-info-proc threshold)))

(define (sal-bdd-fsm/bfs-prioritized-find-trace fsm start-set target-set threshold)
  (let* ((queue (make-queue))
         (queue-insert-proc-core! (lambda (curr-path)
                                    (unless (bdd/false? (car curr-path))
                                      (queue/insert! queue curr-path))))
         (queue-insert-proc! (lambda (curr-path)
                               (let loop ((curr-path curr-path))
                                 (let ((s-size (bdd/size (car curr-path))))
                                   (if (< s-size threshold)
                                     (queue-insert-proc-core! curr-path)
                                     (multiple-value-bind
                                         (s1 s2 var)
                                         (sal-bdd-fsm/split-bdd fsm (car curr-path))
                                       (cond
                                        ((and s1 s2
                                              (< (bdd/size s1) s-size)
                                              (< (bdd/size s2) s-size))
                                         (loop (cons s1 (cdr curr-path)))
                                         (loop (cons s2 (cdr curr-path))))
                                        (else
                                         (queue-insert-proc-core! curr-path)))))))))
         (queue-remove-proc! (lambda ()
                               [assert (queue) (not (queue/empty? queue))]
                               (queue/pop! queue)))
         (queue-empty-proc? (lambda ()
                              (queue/empty? queue)))
         (queue-info-proc (lambda (rs)
                            (when (not (queue/empty? queue))
                              (let ((curr-path (queue/front queue)))
                                (verbose-message 2 "  depth: ~a, frontiers to process: ~a" (length curr-path) (queue/length queue))
                                (verbose-message 3 "    frontier size: ~a nodes, reached nodes size: ~a" 
                                                 (bdd/size (car curr-path))
                                                 (bdd/size rs)))))))
    (sal-bdd-fsm/prioritized-find-trace-core fsm start-set target-set queue-insert-proc! queue-remove-proc! queue-empty-proc? queue-info-proc threshold)))

(define (sal-bdd-fsm/heap-based-prioritized-find-trace fsm start-set target-set threshold lt-proc? bdd-weight-proc bdd-extra-info-proc)
  (let* ((queue (make-heap lt-proc?))
         (num-elements 0)
         (next-to-remove #f)
         (queue-insert-proc-core! (lambda (curr-path)
                                    (unless (bdd/false? (car curr-path))
                                      (set! num-elements (+ num-elements 1))
                                      (heap/add! queue (bdd-weight-proc (car curr-path)) curr-path))))
         (queue-insert-proc! (lambda (curr-path)
                               (when next-to-remove
                                 (heap/add! queue (bdd/size (car next-to-remove)) next-to-remove)
                                 (set! next-to-remove #f))
                               (let loop ((curr-path curr-path))
                                 (let ((s-size (bdd/size (car curr-path))))
                                   (if (< s-size threshold)
                                     (queue-insert-proc-core! curr-path)
                                     (multiple-value-bind
                                         (s1 s2 var)
                                         (sal-bdd-fsm/split-bdd fsm (car curr-path))
                                       (cond
                                        ((and s1 s2
                                              (< (bdd/size s1) s-size)
                                              (< (bdd/size s2) s-size))
                                         (loop (cons s1 (cdr curr-path)))
                                         (loop (cons s2 (cdr curr-path))))
                                        (else
                                         (queue-insert-proc-core! curr-path)))))))))
         (queue-empty-proc? (lambda ()
                              (and (heap/empty? queue) (not next-to-remove))))
         (queue-remove-proc! (lambda ()
                               [assert (queue next-to-remove) (not (queue-empty-proc?))]
                               (set! num-elements (- num-elements 1))
                               (cond 
                                (next-to-remove
                                 (let ((result next-to-remove))
                                   (set! next-to-remove #f)
                                   result))
                                (else
                                 (heap/delete-max! queue)))))
         (queue-info-proc (lambda (rs)
                            (unless (queue-empty-proc?)
                              (unless next-to-remove
                                (set! next-to-remove (heap/delete-max! queue)))
                              (let ((curr-path next-to-remove))
                                (verbose-message 2 "  depth: ~a, frontiers to process: ~a" (length curr-path) num-elements)
                                (verbose-message 3 "    frontier size: ~a nodes, reached nodes size: ~a" 
                                                 (bdd/size (car curr-path))
                                                 (bdd/size rs))
                                (bdd-extra-info-proc (car curr-path)))))))
    (sal-bdd-fsm/prioritized-find-trace-core fsm start-set target-set queue-insert-proc! queue-remove-proc! queue-empty-proc? queue-info-proc threshold)))
  
(define (sal-bdd-fsm/min-prioritized-find-trace fsm start-set target-set threshold)
  (sal-bdd-fsm/heap-based-prioritized-find-trace fsm start-set target-set threshold > bdd/size (lambda (bdd) #unspecified)))

(define (bdd/density fsm bdd)
  (let* ((num-states (sal-bdd-fsm/num-states fsm bdd))
         (num-nodes (bdd/size bdd))
         (density (/fl num-states (fixnum->flonum num-nodes))))
    density))

(define (sal-bdd-fsm/greedy-prioritized-find-trace fsm start-set target-set threshold)
  (sal-bdd-fsm/heap-based-prioritized-find-trace fsm start-set target-set threshold <fl (cut bdd/density fsm <>)
                                                 (lambda (bdd)
                                                   (verbose-message 3 "    frontier density: ~a" (bdd/density fsm bdd)))))

