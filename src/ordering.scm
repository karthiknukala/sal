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

(module ordering
        (include "sal.sch")
        (include "trace.sch")
        (include "fast-hash-table.sch")
        (import runtime sal-module sal-decls heap queue 
                graph sal-dependencies finite-set-as-bdd
                symbol-set sal-ast-for-each sal-expression
                random bdd sal-bdd-context sal-bdd-fsm 
                sal-component-info sal-pp)
        (export (var-order/simple flat-module)
                (var-order/min-comm flat-module la2?)
                (var-order/min-support flat-module la2?)
                (var-order/random flat-module)
                (var-order/default flat-module)
                (var-order/from-file flat-module file-name)
                (var-order/save fsm file-name)
                (make-communication-graph flat-module))
        )

(define *big-num* 99999999999.0)

(define (compute-weights flat-module dependency-graph)
  (let ((state-var-decls (sal-module/state-variables flat-module))
        (relevance-weight (make-node-mapping dependency-graph 0))
        (support-weight (make-node-mapping dependency-graph 0)))
    (graph/for-each-edge (lambda (edge)
                           (let ((source (graph-edge/source edge))
                                 (target (graph-edge/target edge)))
                             (unless (eq? (dependency-graph-node/var-decl source) (dependency-graph-node/var-decl target))
                               (node-mapping/set! relevance-weight target (+ (node-mapping/ref relevance-weight target) 1))
                               (node-mapping/set! support-weight source (+ (node-mapping/ref support-weight source) 1)))))
                         dependency-graph)
    (let* ((relevance-mapping (make-eq-hash-table))
           (support-mapping (make-eq-hash-table))
           (var-weight-mapping (make-eq-hash-table))
           (var/relevance (lambda (var-decl)
                            (cond
                             ((eq-hash-table/get relevance-mapping var-decl) => cdr)
                             (else 0))))
           (var/support (lambda (var-decl)
                          (cond
                           ((eq-hash-table/get support-mapping var-decl) => cdr)
                           (else 0))))
           (var/weight (lambda (var-decl)
                         (cond
                          ((eq-hash-table/get var-weight-mapping var-decl) => cdr)
                          (else 0.0)))))
      ;;(print "relevance weights:")
      (graph/for-each-node (lambda (node)
                             (when (dependency-graph-node/current? node)
                               (eq-hash-table/put! relevance-mapping (dependency-graph-node/var-decl node) (node-mapping/ref relevance-weight node))))
                           dependency-graph)
      (graph/for-each-node (lambda (node)
                             (when (dependency-graph-node/next? node)
                               (eq-hash-table/put! support-mapping (dependency-graph-node/var-decl node) (node-mapping/ref support-weight node))))
                           dependency-graph)
      (for-each (lambda (var-decl)
                  ;; (print "relevance " (sal-decl/name var-decl) " = " (var/relevance var-decl))
                  ;; (print "support " (sal-decl/name var-decl) " = " (var/support var-decl))
                  (let ((var-weight (if (= (var/support var-decl) 0)
                                      *big-num*
                                      (/fl (exact->inexact (var/relevance var-decl))
                                           (exact->inexact (var/support var-decl)))
                                      )))
                    (eq-hash-table/put! var-weight-mapping var-decl var-weight)
                    ;; (print "weight " (sal-decl/name var-decl) " = " var-weight)
                    ))
                state-var-decls)
      var/weight)))

(define (sort-var-decl-list-using-weight var-decl-list var-weight-proc)
  (let* ((lt? (lambda (var-decl1 var-decl2)
                (>fl (var-weight-proc var-decl1) (var-weight-proc var-decl2)))))
    (sort var-decl-list lt?)))

;; Greedy algorithm to find the (approximated) "optimal" permutation using look ahead 2.
;; The main idea is to choose two elements at a time in a greedy sense. The algorithm
;; proceeds computing the best choice of first two elements, then recursively completes
;; the ordering.
(define (find-opt-permutation-using-la2 num-elems cost-partition-func)
  (let ((permutation (make-vector num-elems)))
    ;; initialize permutation array
    (let loop ((i 0))
      (when (< i num-elems)
        (vector-set! permutation i i)
        (loop (+ i 1))))
    ;; process two elements in each iteration
    (let ((max (*fx (/fx num-elems 2) 2)))
      (let loop ((i 0))
        (when (< i max)
          (choose-best-2 permutation i cost-partition-func)
          (loop (+ i 2)))))
    permutation))

;; Similar to find-opt-permutation-using-la2, but using look ahead 1.
(define (find-opt-permutation-using-la num-elems cost-partition-func)
  (let ((permutation (make-vector num-elems)))
    ;; initialize permutation array
    (let loop ((i 0))
      (when (< i num-elems)
        (vector-set! permutation i i)
        (loop (+ i 1))))
    (let loop ((i 0))
      (when (< i (- num-elems 1))
        (choose-best permutation i cost-partition-func)
        (loop (+ i 1))))
    permutation))
  
;; choose the best two elements starting at position `loc', in the current permutation.
(define (choose-best-2 permutation loc cost-partition-func)
  (let ((best-cost (cost-partition-func permutation loc))
        (best-0 loc)
        (best-1 (+ loc 1))
        (num-elems (vector-length permutation)))
    (let loop1 ((i loc))
      (when (< i num-elems)
        (vector/swap-elements! permutation loc i)
        (let loop2 ((j (+ loc 1)))
          (when (< j num-elems)
            (vector/swap-elements! permutation (+ loc 1) j)
            (let ((curr-cost (cost-partition-func permutation loc)))
              (when (< curr-cost best-cost)
                (set! best-cost curr-cost)
                (set! best-0 i)
                (set! best-1 j)))
            (vector/swap-elements! permutation (+ loc 1) j)
            (loop2 (+ j 1))))
        (vector/swap-elements! permutation loc i)
        (loop1 (+ i 1))))
    (vector/swap-elements! permutation loc best-0)
    (vector/swap-elements! permutation (+ loc 1) best-1)))

(define (choose-best permutation loc cost-partition-func)
  (let ((best-cost (cost-partition-func permutation loc))
        (best loc)
        (num-elems (vector-length permutation)))
    (let loop ((i loc))
      (when (< i num-elems)
        (vector/swap-elements! permutation loc i)
        (let ((curr-cost (cost-partition-func permutation loc)))
          (when (< curr-cost best-cost)
            (set! best-cost curr-cost)
            (set! best i)))
        (vector/swap-elements! permutation loc i)
        (loop (+ i 1))))
    (vector/swap-elements! permutation loc best)))

(define (sal-flat-module/component-list flat-module)
  (let ((component-info (sal-component-info/simplify (slot-value flat-module :component-info))))
    (if (instance-of? component-info <sal-base-component-info>)
      (list component-info)
      (slot-value component-info :components))))

(define (sal-flat-module/num-components flat-module)
  (length (sal-flat-module/component-list flat-module)))

(define (make-communication-graph flat-module)
  (let* ((component-list (sal-flat-module/component-list flat-module))
         (output-mapping (make-eq-hash-table)) ;; symbol -> component queue
         (add-output-entry! (lambda (datum compoment)
                       [assert (datum) (instance-of? datum <sal-name-expr>)]
                       [assert (datum) (instance-of? (slot-value datum :decl) <sal-state-var-decl>)]
                       (let ((datum-decl (slot-value datum :decl)))
                         (cond
                          ((eq-hash-table/get output-mapping datum-decl) =>
                           (lambda (entry)
                             (queue/insert! (cdr entry) compoment)))
                          (else
                           (eq-hash-table/put! output-mapping datum-decl (make-queue compoment)))))))
         (component-mapping (make-eq-hash-table))
         (get-node (lambda (component)
                     (cdr (eq-hash-table/get component-mapping component))))
         (component-idx 0)
         (communication-graph (make-graph)))
    (for-each (lambda (component)
                (for-each (cut add-output-entry! <> component) (slot-value component :output-data))
                (let ((new-node (graph/make-node! communication-graph component)))
                  (eq-hash-table/put! component-mapping component new-node)
                  (set-slot-value! component :data (cons new-node component-idx))
                  (set! component-idx (+ component-idx 1))))
              component-list)
    (let* ((process-dependencies!
            (lambda (component)
              (for-each (lambda (datum)
                          [assert (datum) (instance-of? datum <sal-name-expr>)]
                          [assert (datum) (instance-of? (slot-value datum :decl) <sal-state-var-decl>)]
                          (let ((datum-decl (slot-value datum :decl)))
                            (cond 
                             ((eq-hash-table/get output-mapping datum-decl) =>
                              (lambda (entry)
                                (let ((other-component-list (queue->list (cdr entry))))
                                  (for-each (lambda (other-component)
                                              (unless (eq? component other-component)
                                                (graph/make-edge! communication-graph 
                                                                  (get-node other-component) 
                                                                  (get-node component) datum-decl)))
                                            other-component-list)))))))
                        (slot-value component :input-data)))))
      (for-each (lambda (component)
                  (process-dependencies! component))
                component-list)
      communication-graph)))

(define (make-var2idx-table state-var-decls)
  (let ((var2idx (make-eq-hash-table))
        (idx 0))
    (for-each (lambda (state-var-decl)
                (eq-hash-table/put! var2idx state-var-decl idx)
                (set! idx (+ idx 1)))
              state-var-decls)
    var2idx))
    
(define (communication-graph/component-list communication-graph)
  (let ((component-queue (make-queue)))
    (graph/for-each-node (lambda (node)
                           [assert (node) (instance-of? (graph-node/data node) <sal-component-info>)]
                           (queue/insert! component-queue (graph-node/data node)))
                         communication-graph)
    (queue->list component-queue)))

(define-macro (component/contains? component data-slot-id var-decl)
  `(exists (lambda (datum)
             [assert (datum) (instance-of? datum <sal-name-expr>)]
             (eq? (slot-value datum :decl) ,var-decl))
           (slot-value ,component ,data-slot-id)))

(define (component/contains-input? component var-decl)
  (component/contains? component :input-data var-decl))

(define (component/contains-output? component var-decl)
  (component/contains? component :output-data var-decl))

(define (valid-permutation? permutation)
  (let* ((len (vector-length permutation))
         (found-vect (make-vector len #f)))
    (bind-exit (exit)
      (let loop ((i 0))
        (when (< i len)
          (let ((curr-idx (vector-ref permutation i)))
            (unless (and (>= curr-idx 0) (< curr-idx len))
              [assert (permutation) #f]
              (exit #f))
            (when (vector-ref found-vect curr-idx)
              [assert (permutation) #f]
              (exit #f))
            (vector-set! found-vect curr-idx #t)
            (loop (+ i 1)))))
      #t)))
            
(define (permutation->component-list permutation component-vect)
  [assert (permutation component-vect) (= (vector-length permutation) (vector-length component-vect))]
  (let ((result (make-queue))
        (num-components (vector-length permutation)))
    (let loop ((i 0))
      (when (< i num-components)
        (let* ((component-idx (vector-ref permutation i))
               (component (vector-ref component-vect component-idx)))
          (queue/insert! result component)
          (loop (+ i 1)))))
    (queue->list result)))

(define (permutation/fill-partition-vect! permutation-vect cut-pos partition-vect)
  [assert (permutation-vect partition-vect) (= (vector-length partition-vect) (vector-length permutation-vect))]
  [assert (permutation-vect cut-pos) (and (>= cut-pos 0) (< cut-pos (vector-length permutation-vect)))]
  (let loop ((i 0))
    (when (<= i cut-pos)
      (vector-set! partition-vect (vector-ref permutation-vect i) #f)
      (loop (+ i 1))))
  (let ((len (vector-length permutation-vect)))
    (let loop ((i (+ cut-pos 1)))
      (when (< i len)
        (vector-set! partition-vect (vector-ref permutation-vect i) #t)
        (loop (+ i 1))))))

(define (vector/number-of-true-elements vect)
  (vector/fold (lambda (num value) (if value (+ num 1) num)) 0 vect))

(define (component-order-core flat-module make-cut-cost-proc la2?)
  (let* ((communication-graph (make-communication-graph flat-module))
         (state-var-decls (sal-module/state-variables flat-module))
         (num-vars (length state-var-decls))
         (var2idx (make-var2idx-table state-var-decls))
         (var/idx (lambda (state-var-decl)
                    (let ((entry (eq-hash-table/get var2idx state-var-decl)))
                      [assert (entry state-var-decl) entry]
                      (cdr entry))))
         (num-components (graph/num-nodes communication-graph))
         (partition-vect (make-vector num-components)) ;; mapping from component-idx -> {#f, #t}
         (variable-vect (make-vector num-vars)) ;; mapping from var-idx -> {#f, #t}
         (component-list (communication-graph/component-list communication-graph))
         (component-vect (list->vector component-list))
         (component/node (lambda (component)
                           (car (slot-value component :data))))
         (component/idx (lambda (component)
                          (cdr (slot-value component :data))))
         (cut-cost (make-cut-cost-proc component-vect component/node component/idx partition-vect variable-vect var/idx))
         (component-permutation (if la2?
                                  (find-opt-permutation-using-la2 num-components cut-cost)
                                  (find-opt-permutation-using-la num-components cut-cost))))
    [assert (component-permutation) (valid-permutation? component-permutation)]
    (permutation->component-list component-permutation component-vect)))


(define *threshold-warning1* 75)
(define *threshold-warning2* 25)

(define (min-comm-component-order flat-module la2?)
  (when (and (not la2?) (> (sal-flat-module/num-components flat-module) *threshold-warning1*))
    (warning-message "there are ~a components in your specification, static order strategy `min-comm' may be time consuming. The strategies `min-support' and `simple' are more efficient." (sal-flat-module/num-components flat-module)))
  (when (and la2? (> (sal-flat-module/num-components flat-module) *threshold-warning2*))
    (warning-message "there are ~a components in your specification, static order strategy `min-comm2' may be time consuming. The strategies `min-support' and `simple' are more efficient." (sal-flat-module/num-components flat-module)))
  (let* ((backward-factor (lambda (num-backward-dep)
                            (expt (exact->inexact 2) (exact->inexact num-backward-dep))))
         (make-cut-cost-proc 
          (lambda (component-vect component/node component/idx partition-vect variable-vect var/idx)
            (let* ((component/cost! 
                    (lambda (component-idx location)
                      (let* ((component (vector-ref component-vect component-idx))
                             (component-node (component/node component)))
                        (graph-node/for-each-out-edge 
                         (lambda (out-edge)
                           (let* ((var-decl (graph-edge/data out-edge))
                                  (target-node (graph-edge/target out-edge))
                                  (target-component (graph-node/data target-node))
                                  (target-component-idx (component/idx target-component))
                                  (target-location (vector-ref partition-vect target-component-idx)))
                             [assert (component target-component) (not (eq? component target-component))]
                             [assert (component var-decl) (component/contains-output? component var-decl)]
                             [assert (target-component var-decl) (component/contains-input? target-component var-decl)]
                             (unless (eq? target-location location)
                               (vector-set! variable-vect (var/idx var-decl) #t))))
                         component-node))))
                   (cut-cost (lambda (permutation cut-pos)
                               (permutation/fill-partition-vect! permutation cut-pos partition-vect)
                               (vector-fill! variable-vect #f)
                               (let loop ((i 0))
                                 (when (<= i cut-pos)
                                   (let ((component-idx (vector-ref permutation i)))
                                     (component/cost! component-idx #f)
                                     (loop (+ i 1)))))
                               (let ((num-forward-dep (vector/number-of-true-elements variable-vect))
                                     (num-components (vector-length component-vect)))
                                 (vector-fill! variable-vect #f)
                                 (let loop ((i (+ cut-pos 1)))
                                   (when (< i num-components)
                                     (let ((component-idx (vector-ref permutation i)))
                                       (component/cost! component-idx #t)
                                       (loop (+ i 1)))))
                                 (let* ((num-backward-dep (vector/number-of-true-elements variable-vect))
                                        (cost (+fl (exact->inexact num-forward-dep)
                                                   (backward-factor num-backward-dep))))
                                   cost)))))
              cut-cost))))
    (component-order-core flat-module make-cut-cost-proc la2?)))

(define (min-support-component-order flat-module la2?)
  (when (and la2? (> (sal-flat-module/num-components flat-module) *threshold-warning1*))
    (warning-message "there are ~a components in your specification, static order strategy `min-support2' may be time consuming. The strategies `min-support' and `simple' are more efficient (but possibly less precise)." (sal-flat-module/num-components flat-module)))
  (let ((make-cut-cost-proc 
         (lambda (component-vect component/node component/idx partition-vect variable-vect var/idx)
           (let* ((component/cost!
                   (lambda (component-idx location)
                     (let* ((component (vector-ref component-vect component-idx))
                            (component-node (component/node component)))
                       (graph-node/for-each-in-edge
                        (lambda (in-edge)
                          (let* ((var-decl (graph-edge/data in-edge))
                                 (source-node (graph-edge/source in-edge))
                                 (source-component (graph-node/data source-node))
                                 (source-component-idx (component/idx source-component))
                                 (source-location (vector-ref partition-vect source-component-idx)))
                            [assert (component source-component) (not (eq? component source-component))]
                            [assert (component var-decl) (component/contains-input? component var-decl)]
                            [assert (source-component var-decl) (component/contains-output? source-component var-decl)]
                            (unless (eq? source-location location)
                              (vector-set! variable-vect (var/idx var-decl) #t))))
                        component-node))
                     (vector/number-of-true-elements variable-vect)))
                  (cut-cost (lambda (permutation cut-pos)
                              (permutation/fill-partition-vect! permutation cut-pos partition-vect)
                              (vector-fill! variable-vect #f)
                              (let ((cost1 (component/cost! (vector-ref permutation cut-pos) #f)))
                                (vector-fill! variable-vect #f)
                                (+ cost1 (component/cost! (vector-ref permutation (+ cut-pos 1)) #t))))))
             cut-cost))))
    (component-order-core flat-module make-cut-cost-proc la2?)))

(define (for-each-dependent-input proc state-var-decl)
  (let ((next-node (slot-value state-var-decl :next-node)))
    (graph-node/for-each-adjacent-node 
     (lambda (used-node)
       (let ((used-var-decl (dependency-graph-node/var-decl used-node)))
         (when (or (instance-of? used-var-decl <sal-input-state-var-decl>)
                   (instance-of? used-var-decl <sal-global-state-var-decl>))
           (proc used-var-decl))))
     next-node)))

(define (var-order-for-component-list flat-module component-list)
  (let* ((defined-vars (sal-module/defined-variables flat-module))
         (already-processed-vars (make-eq-hash-table))
         (dependency-graph (let ((g (sal-flat-module/make-transition-next-curr-dependency-graph flat-module)))
                             (dependency-graph/delete-defined-variables! g flat-module)
                             g))
         (var-weight-proc (compute-weights flat-module dependency-graph))
         (order (make-queue)))
    (for-each (lambda (component)
                (let* ((input-data (slot-value component :input-data))
                       (owned-data (slot-value component :owned-data))
                       (owned-var-decl-list 
                        (map-and-filter (lambda (owned-datum)
                                          (and (not (eq-hash-table/contains? defined-vars (slot-value owned-datum :decl)))
                                               (slot-value owned-datum :decl)))
                                        owned-data))
                       (ordered-owned-var-decl-list (sort-var-decl-list-using-weight owned-var-decl-list var-weight-proc)))
                  (for-each (lambda (owned-var-decl)
                              [assert (owned-var-decl already-processed-vars flat-module component)
                                      (not (eq-hash-table/contains? already-processed-vars owned-var-decl))]
                              (eq-hash-table/put! already-processed-vars owned-var-decl #unspecified)
                              (let ((used-inputs (make-queue)))
                                (for-each-dependent-input (lambda (used-var-decl)
                                                            (unless (eq-hash-table/contains? already-processed-vars used-var-decl)
                                                              (queue/insert! used-inputs used-var-decl)
                                                              (eq-hash-table/put! already-processed-vars used-var-decl #unspecified)))
                                                          owned-var-decl)
                                (let ((ordered-used-inputs (sort-var-decl-list-using-weight (queue->list used-inputs) var-weight-proc)))
                                  (for-each (lambda (used-input)
                                              (queue/insert! order (sal-decl/name used-input)))
                                            ordered-used-inputs)
                                  (queue/insert! order (sal-decl/name owned-var-decl)))))
                            ordered-owned-var-decl-list)))
              component-list)
    (let ((unreferenced-vars (make-queue)))
      (for-each (lambda (state-var-decl)
                  (unless (or (eq-hash-table/contains? already-processed-vars state-var-decl)
                              (eq-hash-table/contains? defined-vars state-var-decl))
                    (queue/insert! unreferenced-vars state-var-decl)))
                (sal-module/state-variables flat-module))
      (let ((ordered-unreferenced-vars (sort-var-decl-list-using-weight (queue->list unreferenced-vars) var-weight-proc)))
        ;; (print "unreferenced vars: " (map sal-decl/name ordered-unreferenced-vars))
        ;; (print "order: " (queue->list order))
        (append (map sal-decl/name ordered-unreferenced-vars)
                (queue->list order))))))
         
(define (var-order/simple flat-module)
  (status-message :computing-simple-variable-order)
  (verbose-message 2 "  computing simple static variable ordering...")
  (display-runtime 2 "  simple order time: ~a secs"
    (lambda ()
      (var-order-for-component-list flat-module (sal-flat-module/component-list flat-module)))
    :simple-var-order-time))

(define (var-order/min-comm flat-module la2?)
  (status-message :computing-min-comm-variable-order)
  (verbose-message 2 "  computing static variable ordering (minimizing communication between components)...")
  (display-runtime 2 "  static order time: ~a secs"
    (lambda ()
      (let ((component-list (min-comm-component-order flat-module la2?)))
        (var-order-for-component-list flat-module component-list)))
    :min-comm-order-time))

(define (var-order/min-support flat-module la2?)
  (status-message :computing-min-support-variable-order)
  (verbose-message 2 "  computing static variable ordering (minimizing support)...")
  (display-runtime 2 "  static order time: ~a secs"
    (lambda ()
      (let ((component-list (min-support-component-order flat-module la2?)))
        (var-order-for-component-list flat-module component-list)))
    :min-support-time))
  
(define (var-order/random flat-module)
  (verbose-message 2 "  computing random static variable ordering...")
  (status-message :computing-random-variable-order)
  (display-runtime 2 "  random order time: ~a secs"
    (lambda ()
      (let* ((state-var-decls (slot-value flat-module :state-vars))
             (defined-vars (sal-module/defined-variables flat-module))
             (variables-to-order (let ((result (make-queue)))
                                   (for-each (lambda (state-var-decl)
                                               (unless (or (eq-hash-table/contains? defined-vars state-var-decl)
                                                           (instance-of? state-var-decl <sal-choice-input-state-var-decl>))
                                                 (queue/insert! result state-var-decl)))
                                             state-var-decls)
                                   (queue->list result)))
             (variables-to-order-vect (list->vector variables-to-order))
             (num-vars (vector-length variables-to-order-vect))
             (permutation (make-vector num-vars))
             (remaining (let ((result (make-vector num-vars)))
                          (let loop ((i 0)) (when (< i num-vars) (vector-set! result i i) (loop (+ i 1))))
                          result)))
        (let loop ((i 0)
                   (num-remaining num-vars))
          (when (< i num-vars)
            (let ((idx (rand num-remaining)))
              (vector-set! permutation i (vector-ref remaining idx))
              (let move-remaining ((j idx))
                (when (< j (- num-remaining 1))
                  (vector-set! remaining j (vector-ref remaining (+ j 1)))
                  (move-remaining (+ j 1))))
              (loop (+ i 1)
                    (- num-remaining 1)))))
        (let ((random-order (make-queue)))
          (let loop ((i 0))
            (when (< i num-vars)
              (queue/insert! random-order (vector-ref variables-to-order-vect (vector-ref permutation i)))
              (loop (+ i 1))))
          (append (map sal-decl/name (sal-module/choice-vars flat-module))
                  (map sal-decl/name (queue->list random-order))))))
    :random-order-time))
  
(define (var-order/default flat-module)
  (let* ((state-var-decls (sal-module/state-variables flat-module))
         (defined-vars (sal-module/defined-variables flat-module))
         (choice-vars (make-queue))
         (input-vars (make-queue))
         (latched-vars (make-queue)))
    (for-each (lambda (state-var-decl)
                (let ((name (sal-decl/name state-var-decl)))
                  (unless (eq-hash-table/contains? defined-vars state-var-decl)
                    (cond
                     ((instance-of? state-var-decl <sal-choice-input-state-var-decl>)
                      (queue/insert! choice-vars name))
                     ((instance-of? state-var-decl <sal-input-state-var-decl>)
                      (queue/insert! input-vars name))
                     (else
                      (queue/insert! latched-vars name))))))
              state-var-decls)
    (append (queue->list choice-vars) (queue->list input-vars) (queue->list latched-vars))))

(define (sign-invalid-order-file file-name)
  (sign-error "Invalid BDD variable order file: `~a'." file-name))
  
(define (var-order/from-file flat-module file-name)
  (status-message :reading-bdd-var-order file-name)
  (verbose-message 2 "  reading BDD variable order from file `~a'..." file-name)
  (try
   (with-input-from-file file-name
     (lambda ()
       (let ((order (read))
             (state-vars (sal-module/state-variables flat-module))
             (defined-vars (sal-module/defined-variables flat-module))
             (found-vars (make-eq-hash-table))
             (num-found-vars 0)
             (order-queue (make-queue)))
         (unless (list? order)
           (sign-invalid-order-file file-name))
         (for-each (lambda (var-name)
                     (unless (symbol? var-name)
                       (sign-invalid-order-file file-name))
                     (let ((state-var-decl (sal-module/lookup-var flat-module var-name)))
                       (cond
                        ((not state-var-decl)
                         (warning-message "unknown variable `~a' in the BDD variable order file `~a'." var-name file-name))
                        ((eq-hash-table/contains? defined-vars state-var-decl)
                         (warning-message "ignoring transient variable `~a'." var-name))
                        ((eq-hash-table/contains? found-vars state-var-decl)
                         (warning-message "variable `~a' appears more than once in the BDD variable order file `~a'."var-name file-name))
                        (else
                         (queue/insert! order-queue var-name)
                         (eq-hash-table/put! found-vars state-var-decl #unspecified)
                         (set! num-found-vars (+ num-found-vars 1))))))
                   order)
         (for-each (lambda (state-var-decl)
                     (unless (or (instance-of? state-var-decl <sal-choice-input-state-var-decl>)
                                 (eq-hash-table/contains? defined-vars state-var-decl)
                                 (eq-hash-table/contains? found-vars state-var-decl))
                       (warning-message "variable `~a' is not in the BDD variable order file `~a'." (sal-decl/name state-var-decl) file-name)
                       (queue/insert! order-queue (sal-decl/name state-var-decl))))
                   state-vars)
         (append (map sal-decl/name (sal-module/choice-vars flat-module)) (queue->list order-queue)))))
   (lambda (escape proc msg obj)
     (when (equal? proc "with-input-from-file")
       (sign-error "Failed to open file `~a'." file-name))
     (when (equal? proc "read")
       (sign-invalid-order-file file-name))
     (error proc msg obj))))

(define (var-order/save fsm file-name)
  (status-message :writing-bdd-var-order file-name)
  (verbose-message 2 "  writing BDD variable order to file `~a'..." file-name)
  (try
   (with-output-to-file file-name
     (lambda ()
       (let* ((flat-module (sal-bdd-fsm/flat-module fsm))
              (state-vars (sal-module/state-variables flat-module))
              (defined-vars (sal-module/defined-variables flat-module))
              (m (sal-bdd-fsm/manager fsm))
              (num-vars (bdd/num-vars m))
              (idx-state-var-map (let ((mapping (make-vector num-vars #f)))
                                   (for-each (lambda (state-var-decl)
                                               (unless (eq-hash-table/contains? defined-vars state-var-decl)
                                                 (cond 
                                                  ((sal-bdd-context/curr-var fsm state-var-decl) => 
                                                   (lambda (bdd)
                                                     [assert (bdd) (and (bdd? bdd)
                                                                        (bdd/eq? bdd 
                                                                                 (bdd/ith-var (bdd/manager bdd) (bdd/var bdd))))]
                                                     (vector-set! mapping (bdd/var bdd) state-var-decl))))))
                                             state-vars)
                                   mapping))
              (order (make-queue)))
         (let loop ((i 0))
           (when (< i num-vars)
             (let* ((var-idx (bdd/var-at m i))
                    (var-decl (vector-ref idx-state-var-map var-idx)))
               (when (and var-decl
                          (not (instance-of? var-decl <sal-choice-input-state-var-decl>)))
                 (queue/insert! order (sal-decl/name var-decl))))
             (loop (+ i 1))))
         (print ";; Order for the module located at: " (format-with-location flat-module ""))
         (print (queue->list order)))))
   (lambda (escape proc msg obj)
     (when (equal? proc "with-output-to-file")
       (sign-error "Failed to create file `~a'." file-name))
     (error proc msg obj))))
