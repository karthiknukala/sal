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

(module graph
        (include "utility.sch")
        (include "fast-hash-table.sch")
        (import sort queue dot-interface wttree)
        (export (make-graph . svsv)
                (graph/make-node! graph . data)
                (graph/make-edge! graph source target . data)
                (graph/for-each-node proc graph)
                (graph/fold-nodes proc graph)
                (graph/find-node proc graph)
                (graph/for-all-nodes proc graph)
                (graph/exists-node proc graph)
                (graph/for-each-edge proc graph)
                (graph/fold-edges proc graph)
                (graph/find-edge proc graph)
                (graph/for-all-edges proc graph)
                (graph/exists-edge proc graph)
                (graph-node/for-each-in-edge proc node)
                (graph-node/for-each-out-edge proc node)
                (graph-node/for-each-in-out-edge proc node)
                (graph-node/for-each-adjacent-edge proc node)
                (graph-node/for-each-adjacent-node proc node)
                (graph-node/connected? node1 node2)
                (graph/num-nodes graph)
                (graph/num-edges graph)
                (graph/directed? graph)
                (graph/undirected? graph)
                (graph/first-node graph)
                (graph/last-node graph)
                (graph-node/first-in-edge node)
                (graph-node/last-in-edge node)
                (graph-node/first-out-edge node)
                (graph-node/last-out-edge node)
                (graph-node/in-degree node)
                (graph-node/out-degree node)
                (graph-node/degree node)
                (graph-node/data node)
                (graph-node/set-data! node new-value)
                (graph-node/index node)
                (graph-edge/index edge)
                (graph-edge/data edge)
                (graph-edge/set-data! edge new-value)
                (graph-edge/source edge)
                (graph-edge/target edge)
                (graph/reassign-node-ids! graph)
                (graph/reassign-edge-ids! graph)
                (graph/reassign-ids! graph)
                (graph/delete-edge! graph edge)
                (graph/delete-node! graph node)
                (graph/delete-all-nodes! graph)
                (graph/delete-all-edges! graph)
                (graph->dot graph . svsv)
                (graph/display graph . svsv)
                (make-node-mapping graph . initial-value)
                (inline node-mapping/ref mapping node)
                (inline node-mapping/set! mapping node value)
                (make-edge-mapping graph . initial-value)
                (inline edge-mapping/ref mapping edge)
                (inline edge-mapping/set! mapping edge value)
                (graph/collapse-nodes! graph node1 node2)
                (graph/reduce-node! graph node)
                (graph/delete-self-loops! graph)
                (make-graph-node-tree)
                (graph/top-sort graph)
                (graph/top-sort! graph)
                (graph/dfs! graph node reached)
                (graph/dfs-num graph)
                (graph/scc graph)
                (graph/delete-parallel-edges! graph)
                (make-condensation-graph graph)
                (graph/transitive-closure! graph)
                (make-graph-edge-table . initial-size-and-load-factor)
                (make-graph-edge-table-for graph . initial-size-and-load-factor)
                (graph-edge-table/insert! table edge)
                (graph-edge-table/delete! table edge)
                (graph-edge-table/contains? ht source target)
                )
        )

;-------------------------------------------
; Generic graph data structure
;
;-------------------------------------------

(define-record-type graph-node 
  (mk-graph-node id ;; internal id 
                 in-edges ;; list of edges ending in the node
                 out-edges ;; list of edges starting in the node
                 graph  ;; reference to the graph that owns the node
                 succ ;; reference to the next node in the graph
                 prev ;; reference to the previous node in the graph
                 data) ;; user data
  graph-node?
  (id graph-node/id graph-node/set-id!)
  (in-edges graph-node/in-edges graph-node/set-in-edges!)
  (out-edges graph-node/out-edges graph-node/set-out-edges!)
  (graph graph-node/graph graph-node/set-graph!)
  (succ graph-node/succ graph-node/set-succ!)
  (prev graph-node/prev graph-node/set-prev!)
  (data graph-node/data graph-node/set-data!))

(define-record-type graph-edge
  (mk-graph-edge id ;; internal id
                 source ;; source node
                 target ;; target node
                 succ ;; reference to the next edge in the graph
                 prev ;; reference to the previous edge in the graph
                 adj-in-succ ;; reference to the next edge in the adjacency list
                 adj-in-prev ;; reference to the previous edge in the adjacency list
                 adj-out-succ ;; reference to the next edge in the adjacency list
                 adj-out-prev ;; reference to the previous edge in the adjacency list
                 data) ;; user data
  graph-edge?
  (id graph-edge/id graph-edge/set-id!)
  (source graph-edge/source graph-edge/set-source!)
  (target graph-edge/target graph-edge/set-target!)
  (succ graph-edge/succ graph-edge/set-succ!)
  (prev graph-edge/prev graph-edge/set-prev!)
  (adj-in-succ graph-edge/adj-in-succ graph-edge/set-adj-in-succ!)
  (adj-in-prev graph-edge/adj-in-prev graph-edge/set-adj-in-prev!)
  (adj-out-succ graph-edge/adj-out-succ graph-edge/set-adj-out-succ!)
  (adj-out-prev graph-edge/adj-out-prev graph-edge/set-adj-out-prev!)
  (data graph-edge/data graph-edge/set-data!))

(define-record-type graph
  (mk-graph nodes ;; nodes in the graph
            edges ;; edges in the graph
            n-index ;; maximum node id
            e-index ;; maximum edge id
            undirected? ;; graph is undirected?
            node-data-le? ;; compare node data
            edge-data-le?) ;; compare edge data
  graph?
  (nodes graph/nodes graph/set-nodes!)
  (edges graph/edges graph/set-edges!)
  (n-index graph/n-index graph/set-n-index!)
  (e-index graph/e-index graph/set-e-index!)
  (undirected? graph/undirected?)
  (node-data-le? graph/node-data-le?)
  (edge-data-le? graph/edge-data-le?))

;; auxiliary data structure to represent doubly linked lists
(define-record-type dlist
  (mk-dlist head tail len)
  dlist?
  (head dlist/head dlist/set-head!)
  (tail dlist/tail dlist/set-tail!)
  (len dlist/len dlist/set-len!))

;----------------------------------------
; Auxiliary doubly linked list functions
;
;----------------------------------------
(define (make-dlist)
  (mk-dlist #f #f 0))

(define-inline (dlist/append! dlist elem set-succ! set-prev!)
  (dlist/set-len! dlist (+ (dlist/len dlist) 1))
  (let ((tail (dlist/tail dlist)))
    (set-prev! elem tail)
    (set-succ! elem #f)
    (cond
     (tail
      (set-succ! tail elem))
     (else
      ;; the list is empty
      (dlist/set-head! dlist elem)))
    (dlist/set-tail! dlist elem)
    elem))
(define-inline (node-dlist/append! node-list node)
  (dlist/append! node-list node graph-node/set-succ! graph-node/set-prev!))
(define-inline (edge-dlist/append! edge-list edge)
  (dlist/append! edge-list edge graph-edge/set-succ! graph-edge/set-prev!))
(define-inline (in-edge-dlist/append! edge-list edge)
  (dlist/append! edge-list edge graph-edge/set-adj-in-succ! graph-edge/set-adj-in-prev!))
(define-inline (out-edge-dlist/append! edge-list edge)
  (dlist/append! edge-list edge graph-edge/set-adj-out-succ! graph-edge/set-adj-out-prev!))

(define-inline (dlist/push! dlist elem set-succ! set-prev!)
  (dlist/set-len! dlist (+ (dlist/len dlist) 1))
  (let ((head (dlist/head dlist)))
    (set-prev! elem #f)
    (set-succ! elem head)
    (cond
     (head
      (set-prev! head elem))
     (else
      ;; the list is empty
      (dlist/set-tail! dlist elem)))
    (dlist/set-head! dlist elem)
    elem))
(define-inline (node-dlist/push! node-list node)
  (dlist/push! node-list node graph-node/set-succ! graph-node/set-prev!))
(define-inline (edge-dlist/push! edge-list edge)
  (dlist/push! edge-list edge graph-edge/set-succ! graph-edge/set-prev!))
(define-inline (in-edge-dlist/push! edge-list edge)
  (dlist/push! edge-list edge graph-edge/set-adj-in-succ! graph-edge/set-adj-in-prev!))
(define-inline (out-edge-dlist/push! edge-list edge)
  (dlist/push! edge-list edge graph-edge/set-adj-out-succ! graph-edge/set-adj-out-prev!))

(define-inline (dlist/delete! dlist elem succ set-succ! prev set-prev!)
  (dlist/set-len! dlist (- (dlist/len dlist) 1))
  (let ((p (prev elem))
        (s (succ elem)))
    (cond
     ((and p s)
      [assert (elem dlist) (and (not (eq? (dlist/head dlist) elem))
                                (not (eq? (dlist/tail dlist) elem)))]
      (set-succ! p s)
      (set-prev! s p))
     (p
      ;; elem is the tail of the list
      [assert (elem dlist) (not (eq? (dlist/head dlist) elem))]
      [assert (s) (not s)]
      [assert (elem dlist) (eq? (dlist/tail dlist) elem)]
      (set-succ! p #f)
      (dlist/set-tail! dlist p))
     (else
      ;; elem is the head of the list
      [assert (p) (not p)]
      [assert (elem dlist) (eq? (dlist/head dlist) elem)]
      (dlist/set-head! dlist s)
      (if s
        (set-prev! s #f)
        ;; elem is also the tail of the list
        (dlist/set-tail! dlist #f))))
    (set-succ! elem #f)
    (set-prev! elem #f)))
(define-inline (node-dlist/delete! node-list node)
  (dlist/delete! node-list node graph-node/succ graph-node/set-succ! graph-node/prev graph-node/set-prev!))
(define-inline (edge-dlist/delete! edge-list edge)
  (dlist/delete! edge-list edge graph-edge/succ graph-edge/set-succ! graph-edge/prev graph-edge/set-prev!))
(define-inline (in-edge-dlist/delete! edge-list edge)
  (dlist/delete! edge-list edge 
                 graph-edge/adj-in-succ graph-edge/set-adj-in-succ!
                 graph-edge/adj-in-prev graph-edge/set-adj-in-prev!))
(define-inline (out-edge-dlist/delete! edge-list edge)
  (dlist/delete! edge-list edge 
                 graph-edge/adj-out-succ graph-edge/set-adj-out-succ!
                 graph-edge/adj-out-prev graph-edge/set-adj-out-prev!))

(define *tmp-vector* (make-vector 32))

(define (tmp-vector size)
  (when (> size (vector-length *tmp-vector*))
    (set! *tmp-vector* (make-vector size)))
  *tmp-vector*)

(define (dlist->tmp-vector dlist succ)
  (let ((v (tmp-vector (dlist/len dlist))))
    (let loop ((curr (dlist/head dlist))
               (i 0))
      (when curr
        (vector-set! v i curr)
        (loop (succ curr) (+ i 1))))
    v))

(define-inline (dlist/sort! dlist lt? succ set-succ! set-pred!)
  (let ((len (dlist/len dlist)))
    (when (> len 1)
      (let ((v (dlist->tmp-vector dlist succ))
            (last-idx (- len 1)))
        (sort! v lt? len)
        (set-pred! (vector-ref v 0) #f)
        (set-succ! (vector-ref v last-idx) #f)
        (dlist/set-head! dlist (vector-ref v 0))
        (dlist/set-tail! dlist (vector-ref v last-idx))
        (let loop ((i 0))
          (when (< i last-idx)
            (set-succ! (vector-ref v i) (vector-ref v (+ i 1)))
            (set-pred! (vector-ref v (+ i 1)) (vector-ref v i))
            (loop (+ i 1))))))))
(define (node-dlist/sort! node-list lt?)
  (dlist/sort! node-list lt? graph-node/succ graph-node/set-succ! graph-node/set-prev!))
(define (edge-dlist/sort! edge-list lt?)
  (dlist/sort! edge-list lt? graph-edge/succ graph-edge/set-succ! graph-edge/set-prev!))

;-----------------------------------
; Constructors
;
;-----------------------------------

(define (make-graph . svsv)
  (let ((undirected? (svsv/value svsv :undirected? #f))
        (node-data-le? (svsv/value svsv :node-le? (lambda (d1 d2) #f)))
        (edge-data-le? (svsv/value svsv :edge-le? (lambda (d1 d2) #f))))
    (mk-graph (make-dlist) (make-dlist) 0 0 undirected? node-data-le? edge-data-le?)))

(define (graph/make-node! graph . data)
  (let* ((data (optional-arg data #f))
         (id (graph/n-index graph))
         (node-dlist (graph/nodes graph))
         (new-node (mk-graph-node id (make-dlist) (make-dlist) graph #f #f data)))
    (graph/set-n-index! graph (+ id 1))
    (node-dlist/append! node-dlist new-node)
    new-node))

(define (graph/make-edge! graph source target . data)
  [assert (graph source) (eq? (graph-node/graph source) graph)]
  [assert (graph target) (eq? (graph-node/graph target) graph)]
  (let* ((data (optional-arg data #f))
         (id (graph/e-index graph))
         (edge-dlist (graph/edges graph))
         (in-edge-dlist (graph-node/in-edges target))
         (out-edge-dlist (graph-node/out-edges source))
         (new-edge (mk-graph-edge id source target #f #f #f #f #f #f data)))
    (graph/set-e-index! graph (+ id 1))
    (edge-dlist/append! edge-dlist new-edge)
    (in-edge-dlist/append! in-edge-dlist new-edge)
    (out-edge-dlist/append! out-edge-dlist new-edge)
    new-edge))

;-----------------------------------
; Iterators
;
;-----------------------------------

(define-inline (dlist/for-each proc dlist succ)
  (let loop ((curr (dlist/head dlist)))
    (when curr
      (proc curr)
      (loop (succ curr)))))

(define (graph/for-each-node proc graph)
  (dlist/for-each proc (graph/nodes graph) graph-node/succ))

(define *fold-nodes* (for-each->fold graph/for-each-node))
(define *find-node* (for-each->find graph/for-each-node))
(define *for-all-nodes* (for-each->for-all graph/for-each-node))
(define *exists-node* (for-each->exists graph/for-each-node))

(define (graph/fold-nodes proc graph) (*fold-nodes* proc graph))
(define (graph/find-node proc graph) (*find-node* proc graph))
(define (graph/for-all-nodes proc graph) (*for-all-nodes* proc graph))
(define (graph/exists-node proc graph) (*exists-node* proc graph))

(define (graph/for-each-edge proc graph)
  (dlist/for-each proc (graph/edges graph) graph-edge/succ))

(define *fold-edges* (for-each->fold graph/for-each-edge))
(define *find-edge* (for-each->find graph/for-each-edge))
(define *for-all-edges* (for-each->for-all graph/for-each-edge))
(define *exists-edge* (for-each->exists graph/for-each-edge))

(define (graph/fold-edges proc graph) (*fold-edges* proc graph))
(define (graph/find-edge proc graph) (*find-edge* proc graph))
(define (graph/for-all-edges proc graph) (*for-all-edges* proc graph))
(define (graph/exists-edge proc graph) (*exists-edge* proc graph))

(define (graph-node/for-each-in-edge proc node)
  (dlist/for-each proc (graph-node/in-edges node) graph-edge/adj-in-succ))

(define *fold-in-edges* (for-each->fold graph-node/for-each-in-edge))
(define *find-in-edge* (for-each->find graph-node/for-each-in-edge))

(define (graph-node/fold-in-edges proc node) (*fold-in-edges* proc node))
(define (graph-node/find-in-edge proc node) (*find-in-edge* proc node))

(define (graph-node/for-each-out-edge proc node)
  (dlist/for-each proc (graph-node/out-edges node) graph-edge/adj-out-succ))

(define *fold-out-edges* (for-each->fold graph-node/for-each-out-edge))
(define *find-out-edge* (for-each->find graph-node/for-each-out-edge))

(define (graph-node/fold-out-edges proc node) (*fold-out-edges* proc node))
(define (graph-node/find-out-edge proc node) (*find-out-edge* proc node))

(define (graph-node/for-each-in-out-edge proc node)
  (graph-node/for-each-in-edge proc node)
  (graph-node/for-each-out-edge proc node))

(define (graph-node/for-each-adjacent-edge proc node)
  (if (graph/undirected? (graph-node/graph node))
    (graph-node/for-each-in-out-edge proc node)
    (graph-node/for-each-out-edge proc node)))

(define (graph-node/for-each-adjacent-node proc node)
  (when (graph/undirected? (graph-node/graph node))
    (graph-node/for-each-in-edge 
     (lambda (edge)
       (proc (graph-edge/source edge)))
     node))
  (graph-node/for-each-out-edge 
   (lambda (edge)
     (proc (graph-edge/target edge)))
   node))

(define (graph-node/connected? node1 node2)
  (bind-exit (exit)
    (graph-node/for-each-adjacent-node (lambda (n)
                                         (when (eq? n node2)
                                           (exit #t)))
                                       node1)
    #f))

;-----------------------------------
; Auxiliary
;
;-----------------------------------

(define (graph/reassign-node-ids! graph)
  (let ((n-idx 0))
    (graph/for-each-node 
     (lambda (n)
       (graph-node/set-id! n n-idx)
       (set! n-idx (+ n-idx 1)))
     graph)
    (graph/set-n-index! graph n-idx)
    [assert (graph) (= (graph/num-nodes graph) (graph/n-index graph))]
    n-idx))

(define (graph/reassign-edge-ids! graph)
  (let ((e-idx 0))
    (graph/for-each-edge 
     (lambda (e)
       (graph-edge/set-id! e e-idx)
       (set! e-idx (+ e-idx 1)))
     graph)
    (graph/set-e-index! graph e-idx)
    [assert (graph) (= (graph/num-edges graph) (graph/e-index graph))]
    e-idx))

(define (graph/reassign-ids! graph)
  (graph/reassign-node-ids! graph)
  (graph/reassign-edge-ids! graph))

(define (check-dlist dlist succ prev check-proc)
  (let loop ((n (dlist/head dlist))
             (i 0))
    (cond
     ((not n)
      ;; check the length of the list
      (= (dlist/len dlist) i))
     (else
      ;; the head doesn't have predecessors
      (when (eq? n (dlist/head dlist))
        [assert (dlist n) (eq? (prev n) #f)])
      (when (prev n)
        ;; (the successor of the precessor of n) = n
        [assert (dlist n) (eq? (succ (prev n)) n)])
      (when (succ n)
        ;; (the precessor of the successor of n) = n
        [assert (dlist n) (eq? (prev (succ n)) n)])
      (when (not (succ n))
        ;; must be the tail of the list
        [assert (dlist n) (eq? (dlist/tail dlist) n)])
      (check-proc n)
      (loop (succ n) (+ i 1)))))
  #t)

(define (check-graph-node node)
  (check-dlist (graph-node/in-edges node) graph-edge/adj-in-succ graph-edge/adj-in-prev 
               (lambda (edge)
                 [assert (node edge) (eq? (graph-edge/target edge) node)]))
  (check-dlist (graph-node/out-edges node) graph-edge/adj-out-succ graph-edge/adj-out-prev 
               (lambda (edge)
                 [assert (node edge) (eq? (graph-edge/source edge) node)]))
  #t)

(define (check-graph graph)
  (check-dlist (graph/nodes graph) graph-node/succ graph-node/prev 
               (lambda (node)
                 [assert (graph node) (eq? graph (graph-node/graph node))]
                 (check-graph-node node)))
  (check-dlist (graph/edges graph) graph-edge/succ graph-edge/prev 
               (lambda (edge)
                 [assert (graph edge) (graph-node/find-in-edge (cut eq? <> edge) (graph-edge/target edge))]
                 [assert (graph edge) (graph-node/find-out-edge (cut eq? <> edge) (graph-edge/source edge))]
                 #unspecified))
  #t)

(define (graph/dump graph)
  (print "graph:")
  (print "  number of nodes: " (graph/num-nodes graph))
  (print "  number of edges: " (graph/num-edges graph))
  (print "  nodes:")
  (graph/for-each-node 
   (lambda (node)
     (display* (graph-node/index node) " "))
   graph)
  (print "")
  (print "  edges:")
  (graph/for-each-edge 
   (lambda (edge)
     (print "    " (graph-edge/index edge) " : "
            (graph-node/index (graph-edge/source edge))
            " --> "
            (graph-node/index (graph-edge/target edge))))
   graph)
  (print "--------------------"))
  
;-----------------------------------
; Accessors
;
;-----------------------------------
(define (graph/num-nodes graph)
  (dlist/len (graph/nodes graph)))

(define (graph/num-edges graph)
  (dlist/len (graph/edges graph)))

(define (graph/directed? graph)
  (not (graph/undirected? graph)))

(define (graph/first-node graph)
  (dlist/head (graph/nodes graph)))

(define (graph/last-node graph)
  (dlist/tail (graph/nodes graph)))

(define (graph-node/first-in-edge node)
  (dlist/head (graph-node/in-edges node)))

(define (graph-node/last-in-edge node)
  (dlist/tail (graph-node/in-edges node)))

(define (graph-node/first-out-edge node)
  (dlist/head (graph-node/out-edges node)))

(define (graph-node/last-out-edge node)
  (dlist/tail (graph-node/out-edges node)))

(define (graph-node/in-degree node)
  (dlist/len (graph-node/in-edges node)))

(define (graph-node/out-degree node)
  (dlist/len (graph-node/out-edges node)))

(define (graph-node/degree node)
  (+ (graph-node/in-degree node) (graph-node/out-degree node)))

(define (graph-node/index node)
  (graph-node/id node))

(define (graph-edge/index edge)
  (graph-edge/id edge))

(define (graph-edge/source-index edge)
  (graph-node/id (graph-edge/source edge)))

(define (graph-edge/target-index edge)
  (graph-node/id (graph-edge/target edge)))

;-----------------------------------
; Destructors
;
;-----------------------------------

(define (graph/delete-edge! graph edge)
  (edge-dlist/delete! (graph/edges graph) edge)
  (in-edge-dlist/delete! (graph-node/in-edges (graph-edge/target edge)) edge)
  (out-edge-dlist/delete! (graph-node/out-edges (graph-edge/source edge)) edge)
  (graph-edge/set-source! edge #f)
  (graph-edge/set-target! edge #f))

(define (graph/delete-node! graph node)
  [assert (graph node) (eq? graph (graph-node/graph node))]
  (let delete-in-edges ()
    (when (> (graph-node/in-degree node) 0)
      (graph/delete-edge! graph (graph-node/first-in-edge node))
      (delete-in-edges)))
  (let delete-out-edges ()
    (when (> (graph-node/out-degree node) 0)
      (graph/delete-edge! graph (graph-node/first-out-edge node))
      (delete-out-edges)))
  (node-dlist/delete! (graph/nodes graph) node))

(define (graph/delete-all-nodes! graph)
  (graph/set-nodes! graph (make-dlist))
  (graph/set-edges! graph (make-dlist)))

;; Remove all edges from the graph.
;; Remark: Complexity O(num-nodes).
(define (graph/delete-all-edges! graph)
  (graph/for-each-node 
   (lambda (node)
     (graph-node/set-in-edges! node (make-dlist))
     (graph-node/set-out-edges! node (make-dlist)))
   graph)
  (graph/set-edges! graph (make-dlist)))

;-----------------------------------
; DOT Interface
;
;-----------------------------------

(define (graph->dot graph . svsv)
  (let ((node-labeler (svsv/value svsv :node-labeler (lambda (node) (object->string (graph-node/index node)))))
        (node-attrs (svsv/value svsv :node-attrs (lambda (node) '())))
        (edge-labeler (svsv/value svsv :edge-labeler (lambda (edge) "")))
        (edge-attrs (svsv/value svsv :edge-attrs (lambda (edge) '()))))
    (if (graph/directed? graph)
      (print "digraph salgraph {")
      (print "graph salgraph {"))
    (graph/for-each-node 
     (lambda (node)
       (display* "n" (graph-node/index node) "[label=\"" (node-labeler node) "\" ")
       (for-each (lambda (attr)
                   (display* ", " attr))
                 (node-attrs node))
       (print "];"))
     graph)
    (graph/for-each-edge 
     (lambda (edge)
       (display* "n" (graph-edge/source-index edge)
                 (if (graph/directed? graph) " -> " " -- ")
                 "n" (graph-edge/target-index edge)
                 "[label=\"" (edge-labeler edge) "\" ")
       (for-each (lambda (attr)
                   (display* ", " attr))
                 (edge-attrs edge))
       (print "];"))
     graph)
    (print "}")))

(define (graph/display graph . svsv)
  (with-output-to-file *sal-dot-tmp-file*
    (lambda ()
      (apply graph->dot graph svsv)))
  (dot/show))

;-----------------------------------
; Node & Edge Mapping
;
;-----------------------------------

(define (make-node-mapping graph . initial-value)
  (let ((initial-value (optional-arg initial-value #unspecified)))
    (make-vector (graph/n-index graph) initial-value)))

(define (make-tmp-node-mapping graph . initial-value)
  (let* ((initial-value (optional-arg initial-value #unspecified))
         (mapping (tmp-vector (graph/n-index graph))))
    (vector-fill! mapping initial-value)
    mapping))

(define (make-edge-mapping graph . initial-value)
  (let ((initial-value (optional-arg initial-value #unspecified)))
    (make-vector (graph/e-index graph) initial-value)))

(define-inline (node-mapping/ref mapping node)
  (vector-ref mapping (graph-node/index node)))

(define-inline (node-mapping/set! mapping node value)
  (vector-set! mapping (graph-node/index node) value))

(define-inline (edge-mapping/ref mapping edge)
  (vector-ref mapping (graph-edge/index edge)))

(define-inline (edge-mapping/set! mapping edge value)
  (vector-set! mapping (graph-edge/index edge) value))

;-----------------------------------
; Collapse Nodes
;
;-----------------------------------
;; move the vertices from node2 to node1
(define (graph/collapse-nodes! graph node1 node2)
  (graph-node/for-each-in-edge (lambda (edge)
                                 (let ((source (graph-edge/source edge)))
                                   (graph/make-edge! graph source node1)))
                               node2)
  (graph-node/for-each-out-edge (lambda (edge)
                                  (let ((target (graph-edge/target edge)))
                                    (graph/make-edge! graph node1 target)))
                                node2)
  (graph/delete-node! graph node2))
                                 
;------------------------------------
; Reduce node
;
;------------------------------------
;; For every pair of edges (x,node) (node,y), create the edge (x,y).
;; Then, delete `node'. 
(define (graph/reduce-node! graph node)
  (graph-node/for-each-in-edge 
   (lambda (in-edge)
     (graph-node/for-each-out-edge
      (lambda (out-edge)
        (let ((source (graph-edge/source in-edge))
              (target (graph-edge/target out-edge)))
          (graph/make-edge! graph source target)))
      node))
   node)
  (graph/delete-node! graph node))

;------------------------------------
; Delete self loops
;
;------------------------------------
;; Remark: Complexity O(num-edges)
(define (graph/delete-self-loops! graph)
  (let ((self-loops '()))
    (graph/for-each-edge (lambda (edge)
                           (when (eq? (graph-edge/source edge) (graph-edge/target edge))
                             (push! edge self-loops)))
                         graph)
    (for-each (cut graph/delete-edge! graph <>) self-loops)))

;------------------------------------
; Topological sort
;
;------------------------------------

;; sorts the given graph topologically when it is acyclic.
;; the result is a mapping 'ord' from nodes to natural numbers
;; `ord[i] < ord[j] for all edges `i->j'
;; return the mapping `ord' if the graph is acyclic, #f otherwise
;; Remark: Complexity O(num-nodes + num-edges)
(define (graph/top-sort graph)
  [assert (graph) (graph/directed? graph)]
  (let ((in-degree-map (make-node-mapping graph 0))
        (zero-degree-queue (make-queue))
        (ord (make-node-mapping graph 0)))
    ;; initialize in-degree-map
    (graph/for-each-node 
     (lambda (node)
       (let ((in-degree (graph-node/in-degree node)))
         (node-mapping/set! in-degree-map node in-degree)
         (when (= in-degree 0)
           (queue/insert! zero-degree-queue node))))
     graph)
    (let loop ((ord-idx 0))
      (cond
       ((queue/empty? zero-degree-queue)
        ;; check if all node were visited...
        (and (= ord-idx (graph/num-nodes graph))
             ord))
       (else
        (let ((node (queue/pop! zero-degree-queue)))
          (node-mapping/set! ord node (+ ord-idx 1))
          (graph-node/for-each-adjacent-node 
           (lambda (adj-node)
             (node-mapping/set! in-degree-map adj-node (- (node-mapping/ref in-degree-map adj-node) 1))
             [assert (adj-node in-degree-map) (>= (node-mapping/ref in-degree-map adj-node) 0)]
             (when (= (node-mapping/ref in-degree-map adj-node) 0)
               (queue/insert! zero-degree-queue adj-node)))
           node)
          (loop (+ ord-idx 1))))))))
                        
;; rearrange nodes and vertices using the order computed in the previous function.
;; returns #t if success, #f otherwise (graph contains cycles).
;; Remark: Complexity O(num-nodes + num-edges)
(define (graph/top-sort! graph)
  [assert (graph) (graph/directed? graph)]
  (or (= (graph/num-nodes graph) 0)
      (= (graph/num-edges graph) 0)
      (let ((node-ord (graph/top-sort graph)))
        (and node-ord
             (let ((edge-ord (make-edge-mapping graph 0)))
               (graph/for-each-edge 
                (lambda (edge)
                  (edge-mapping/set! edge-ord edge (node-mapping/ref node-ord (graph-edge/target edge))))
                graph)
               (node-dlist/sort! (graph/nodes graph) (lambda (n1 n2) 
                                                       [assert (n1 n2) (and (graph-node? n1) (graph-node? n2))]
                                                       (< (node-mapping/ref node-ord n1) (node-mapping/ref node-ord n2))))
               (edge-dlist/sort! (graph/edges graph) (lambda (e1 e2) 
                                                       [assert (e1 e2) (and (graph-edge? e1) (graph-edge? e2))]
                                                       (< (edge-mapping/ref edge-ord e1) (edge-mapping/ref edge-ord e2))))
               #t)))))
                                                                        
;------------------------------------
; Depth First Search
;
;------------------------------------

;; perform a depth first search starting at `node', it visits all nodes `n' such that
;; (node-mapping/ref reached n) is #f. The mapping `reached' is updated, and a list
;; of visited nodes is returned.
;; Remark: Complexity O(num-nodes + num-edges)
(define (graph/dfs! graph node reached)            
  [assert (graph) (graph/directed? graph)]
  (let ((result-queue (make-queue)))
    (let loop ((stack (cond
                       ((node-mapping/ref reached node)
                        '())
                       (else 
                        (node-mapping/set! reached node #t)
                        (list node)))))
      (cond
       ((null? stack)
        (queue->list result-queue))
       (else
        (let ((curr (car stack))
              (new-stack (cdr stack)))
          (queue/insert! result-queue curr)
          (graph-node/for-each-adjacent-node 
           (lambda (adj-node)
             (unless (node-mapping/ref reached adj-node)
               (node-mapping/set! reached adj-node #t)
               (set! new-stack (cons adj-node new-stack))))
           curr)
          (loop new-stack)))))))

;; perform a depth first search numbering the nodes in two different ways. 
;; The results are:
;; 1) a list of edges 
;; 2) dfsnum is a mapping from node to calling time
;; 3) compnum is a mapping from node to completion time of the recursive calls.
;; Remark: Complexity O(num-nodes + num-edges)
(define (graph/dfs-num graph)
  [assert (graph) (graph/directed? graph)]
  (let ((result-queue (make-queue))
        (reached (make-tmp-node-mapping graph #f))
        (dfs-num (make-node-mapping graph 0))
        (comp-num (make-node-mapping graph 0))
        (dfscount1 0)
        (dfscount2 0))
    (graph/for-each-node 
     (lambda (node)
       (unless (node-mapping/ref reached node)
         (let dfs ((node node))
           (node-mapping/set! reached node #t)
           (set! dfscount1 (+ dfscount1 1))
           (node-mapping/set! dfs-num node dfscount1)
           (graph-node/for-each-adjacent-edge 
            (lambda (edge)
              (let ((adj-node (graph-edge/target edge)))
                (unless (node-mapping/ref reached adj-node)
                  (queue/insert! result-queue edge)
                  (dfs adj-node))))
            node)
           (set! dfscount2 (+ dfscount2 1))
           (node-mapping/set! comp-num node dfscount2))))
     graph)
    (values (queue->list result-queue)
            dfs-num
            comp-num)))

;------------------------------------
; Strongly Connected Components 
;
;------------------------------------

;; Computes the strongly connected components of a graph.
;; The results are:
;; 1) the number of components
;; 2) a mapping from node to component ids, where a component id is a number between 0 and (- num-components 1)
;; Remark: Complexity O(num-nodes + num-edges)
(define (graph/scc graph)
  [assert (graph) (graph/directed? graph)]
  (let* ((roots '())
         (unfinished '())
         (unfinished-set (make-tmp-node-mapping graph #f))
         (unfinished/push! (lambda (n)
                             (set! unfinished (cons n unfinished))
                             (node-mapping/set! unfinished-set n #t)))
         (unfinished/pop! (lambda ()
                            (let ((result (car unfinished)))
                              [assert (unfinished unfinished-set result) (node-mapping/ref unfinished-set result)]
                              (set! unfinished (cdr unfinished))
                              (node-mapping/set! unfinished-set result #f)
                              result)))
         (unfinished/member? (lambda (n)
                               (node-mapping/ref unfinished-set n)))
         (dfs-num (make-node-mapping graph -1))
         (component-map (make-node-mapping graph -1))
         (curr-dfs-num 0)
         (next-comp-idx 0))
    (graph/for-each-node 
     (lambda (node)
       (when (= (node-mapping/ref dfs-num node) -1)
         (let dfs ((node node))
           (set! curr-dfs-num (+ curr-dfs-num 1))
           (node-mapping/set! dfs-num node curr-dfs-num)
           (unfinished/push! node)
           (set! roots (cons node roots))
           (graph-node/for-each-adjacent-node 
            (lambda (adj-node)
              (cond
               ((= (node-mapping/ref dfs-num adj-node) -1)
                (dfs adj-node))
               (else
                (when (unfinished/member? adj-node)
                  (let loop ()
                    (when (> (node-mapping/ref dfs-num (car roots))
                             (node-mapping/ref dfs-num adj-node))
                      [assert (roots) (not (null? roots))]
                      (set! roots (cdr roots))
                      (loop)))))))
            node)
           (when (eq? node (car roots))
             (let loop ()
               (let ((scc-elem (unfinished/pop!)))
                 (node-mapping/set! component-map scc-elem next-comp-idx)
                 (unless (eq? scc-elem node)
                   (loop))))
             (set! next-comp-idx (+ next-comp-idx 1))
             (set! roots (cdr roots))))))
     graph)
    (values next-comp-idx
            component-map)))

;------------------------------------
; Delete Parallel Edges
;
;------------------------------------

;; Delete all parallel edges.
;; Remark: Complexity O(num-nodes + num-edges)
(define (graph/delete-parallel-edges! graph)
  (let ((edges (dlist->tmp-vector (graph/edges graph) graph-edge/succ))
        (node-num (make-node-mapping graph))
        (n 0)
        (num-edges (graph/num-edges graph)))
    (graph/for-each-node 
     (lambda (node)
       (node-mapping/set! node-num node n)
       (set! n (+ n 1)))
     graph)
    [assert (n graph) (= n (graph/num-nodes graph))]
    ;; force parallel edges to be adjacent in the vector edges
    (bucket-sort! edges n (lambda (edge) (node-mapping/ref node-num (graph-edge/source edge))) num-edges)
    (bucket-sort! edges n (lambda (edge) (node-mapping/ref node-num (graph-edge/target edge))) num-edges)
    (let ((prev-source-idx -1)
          (prev-target-idx -1))
      (let loop ((i 0))
        (when (< i num-edges)
          (let ((edge (vector-ref edges i)))
            (let ((source-idx (graph-edge/source-index edge))
                  (target-idx (graph-edge/target-index edge)))
              (cond
               ((and (= source-idx prev-source-idx)
                     (= target-idx prev-target-idx))
                (graph/delete-edge! graph edge))
               (else
                (set! prev-source-idx source-idx)
                (set! prev-target-idx target-idx)))))
          (loop (+ i 1)))))))
          
;------------------------------------
; Condensation Graph
;
;------------------------------------

;; Construct the condensation graph. A condensation graph is a a graph graph' based on the given graph 
;; where each vertex in condensation graph corresponds to a strongly connected component in the given graph.
;; an edge (u,v) is in the result if and only if there exists an edge in the given graph connecting any 
;; of the vertices in the component of u to any of the vertices in the component of v.
;; Remark: Complexity O(num-nodes + num-edges)
(define (make-condensation-graph graph)
  [assert (graph) (graph/directed? graph)]
  (multiple-value-bind 
      (num-sccs component-mapping)
      (graph/scc graph)
    (let ((result-graph (make-graph))
          (new-nodes (make-vector num-sccs)))
      ;; create num-sccs nodes in the result graph
      (let loop ((i 0))
        (when (< i num-sccs)
          (vector-set! new-nodes i (graph/make-node! result-graph (make-queue)))
          (loop (+ i 1))))
      ;; store the components in the result graph
      (graph/for-each-node 
       (lambda (node)
         (let* ((comp-id (node-mapping/ref component-mapping node))
                (new-node (vector-ref new-nodes comp-id))
                (new-node-queue (graph-node/data new-node)))
           (queue/insert! new-node-queue node)))
       graph)
      ;; create the edges of the result graph
      (graph/for-each-edge 
       (lambda (edge)
         (let* ((source (graph-edge/source edge))
                (target (graph-edge/target edge))
                (source-comp-idx (node-mapping/ref component-mapping source))
                (target-comp-idx (node-mapping/ref component-mapping target))
                (new-source (vector-ref new-nodes source-comp-idx))
                (new-target (vector-ref new-nodes target-comp-idx)))
           (unless (eq? new-source new-target)
             (graph/make-edge! result-graph new-source new-target))))
       graph)
      ;; delete parallel edges
      (graph/delete-parallel-edges! result-graph)
      result-graph)))

;------------------------------------
; Transitive Closure
;
;------------------------------------

(define *node-tree-type* (make-wt-tree-type (lambda (n1 n2)
                                              (< (graph-node/index n1)
                                                 (graph-node/index n2)))))
(define (make-graph-node-tree)
  (make-wt-tree *node-tree-type*))

(define (acyclic-graph/transitive-closure! graph)
  [assert (graph) (graph/directed? graph)]
  (let ((succ-map (make-node-mapping graph #f))
        (node-ord (graph/top-sort graph)))
    [assert (node-ord) node-ord]
    ;; sort the nodes in reverse topological order
    (node-dlist/sort! (graph/nodes graph) 
                      (lambda (n1 n2) 
                        [assert (n1 n2) (and (graph-node? n1) (graph-node? n2))]
                        (> (node-mapping/ref node-ord n1) (node-mapping/ref node-ord n2))))
    ;; TODO: I should re-implement the following loop using chain-partitioning to efficiently implement union
    (graph/for-each-node 
     (lambda (node)
       (let ((succ (make-graph-node-tree)))
         (graph-node/for-each-adjacent-node 
          (lambda (adj-node)
            (unless (wt-tree/member? adj-node succ) 
              (let ((adj-node-succ (node-mapping/ref succ-map adj-node)))
                [assert (adj-node-succ) adj-node-succ]
                (set! succ (wt-tree/union (wt-tree/add succ adj-node #unspecified)
                                          adj-node-succ)))))
          node)
         (node-mapping/set! succ-map node succ)))
     graph)
    (graph/delete-all-edges! graph)
    (graph/for-each-node 
     (lambda (node)
       (wt-tree/for-each 
        (lambda (succ-node _)
          (graph/make-edge! graph node succ-node))
        (node-mapping/ref succ-map node)))
     graph)))
    
(define (graph/transitive-closure! graph)
  [assert (graph) (graph/directed? graph)]
  (let ((condensation-graph (make-condensation-graph graph)))
    (acyclic-graph/transitive-closure! condensation-graph)
    (graph/delete-all-edges! graph)
    (graph/for-each-node 
     (lambda (scc-node)
       (let ((source-nodes (queue->list (graph-node/data scc-node))))
         ;; connect source-nodes to source-nodes since they are in the same 
         (unless (null? (cdr source-nodes))
           (for-each
            (lambda (source-node1)
              (for-each
               (lambda (source-node2)
                 (graph/make-edge! graph source-node1 source-node2))
               source-nodes))
            source-nodes))
         (graph-node/for-each-adjacent-node 
          (lambda (adj-scc-node)
            (let ((target-nodes (queue->list (graph-node/data adj-scc-node))))
              (for-each 
               (lambda (source-node)
                 (for-each 
                  (lambda (target-node)
                    (graph/make-edge! graph source-node target-node))
                  target-nodes))
               source-nodes)))
          scc-node)))
     condensation-graph)))
                            
;------------------------------------
; Edge table
;
; Useful to answer question (i,j) in G
; in constant time.
;------------------------------------

;; edge table are implemented using hash tables

(define (graph-edge/hash edge)
  (+ (graph-edge/source-index edge)
     (* 3 (graph-edge/target-index edge))))

(define (graph-edge/eq? edge1 edge2)
  (and (eq? (graph-edge/source edge1) (graph-edge/source edge2))
       (eq? (graph-edge/target edge1) (graph-edge/target edge2))))

(make-fast-hash-table-type edge-table graph-edge/hash graph-edge/eq?)

(define (make-graph-edge-table . initial-size-and-load-factor)
  (apply make-edge-table initial-size-and-load-factor))

(define (make-graph-edge-table-for graph . initial-size-and-load-factor)
  (let ((table (apply make-graph-edge-table initial-size-and-load-factor)))
    (graph/for-each-edge (lambda (edge)
                           (graph-edge-table/insert! table edge))
                         graph)
    table))

(define (graph-edge-table/insert! table edge)
  (edge-table/put! table edge #unspecified))

(define (graph-edge-table/delete! table edge)
  (edge-table/delete! table edge))

(define *dummy-edge*  (mk-graph-edge #unspecified #unspecified #unspecified #unspecified #unspecified #unspecified #unspecified #unspecified #unspecified #unspecified))

(define (graph-edge-table/contains? table source target)
  (graph-edge/set-source! *dummy-edge* source)
  (graph-edge/set-target! *dummy-edge* target)
  (edge-table/contains? table *dummy-edge*))

