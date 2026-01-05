;; -*-Scheme-*-
;;
;;  $Id: wttree.scm 1737 2010-05-14 02:45:18Z bruno $
;;
;;  Copyright (c) 1993-1994 Stephen Adams
;;
;;  References:
;;
;;    Stephen Adams, Implemeting Sets Efficiently in a Functional
;;       Language, CSTR 92-10, Department of Electronics and Computer
;;       Science, University of Southampton, 1992
;;
;;
;;  Copyright (c) 1993-94 Massachusetts Institute of Technology
;;
;;  This material was developed by the Scheme project at the Massachusetts
;;  Institute of Technology, Department of Electrical Engineering and
;;  Computer Science.  Permission to copy this software, to redistribute
;;  it, and to use it for any purpose is granted, subject to the following
;;  restrictions and understandings.
;;
;;  1. Any copy made of this software must include this copyright notice
;;  in full.
;;
;;  2. Users of this software agree to make their best efforts (a) to
;;  return to the MIT Scheme project any improvements or extensions that
;;  they make, so that these may be included in future releases; and (b)
;;  to inform MIT of noteworthy uses of this software.
;;
;;  3. All materials developed as a consequence of the use of this
;;  software shall duly acknowledge such use, in accordance with the usual
;;  standards of acknowledging credit in academic research.
;;
;;  4. MIT has made no warrantee or representation that the operation of
;;  this software will be error-free, and MIT is under no obligation to
;;  provide any services, by way of maintenance, update, or otherwise.
;;
;;  5. In conjunction with products arising from the use of this material,
;;  there shall be no use of the name of the Massachusetts Institute of
;;  Technology nor of any adaptation thereof in any advertising,
;;  promotional, or sales literature without prior written consent from
;;  MIT in each case.
;;
;;  MODIFIED by Leonardo de Moura to work with Bigloo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module wttree
  (import xformat)
  (export 
      (make-wt-tree-type key<?)
      (make-wt-tree tree-type)  
      (singleton-wt-tree type key value)
      (wt-tree? t)
      (wt-tree/type t)
      (alist->wt-tree type alist)
      (wt-tree/empty? tree)
      (wt-tree/size tree)
      (wt-tree/add tree key datum)
      (wt-tree/delete tree key)
      (wt-tree/add! tree key datum)
      (wt-tree/delete! tree key)
      (wt-tree/member? key tree)
      (wt-tree/lookup tree key default)
      (wt-tree/split< tree key)
      (wt-tree/split> tree key)
      (wt-tree/union tree1 tree2)
      (wt-tree/union+ tree1 tree2 proc)
      (wt-tree/intersection tree1 tree2)
      (wt-tree/intersection+ tree1 tree2 proc)
      (wt-tree/difference tree1 tree2)
      (wt-tree/difference+ tree1 tree2 proc)
      (wt-tree/subset? tree1 tree2)
      (wt-tree/subset+? tree1 tree2 proc)
      (wt-tree/set-equal? tree1 tree2)
      (wt-tree/fold combiner-key-datum-result init tree)
      (wt-tree/for-each action-key-datum tree)
      (wt-tree/index tree index)
      (wt-tree/index-datum tree index)
      (wt-tree/index-pair tree index)
      (wt-tree/rank tree key)
      (wt-tree/min tree)
      (wt-tree/min-datum tree)
      (wt-tree/min-pair tree)
      (wt-tree/delete-min tree)
      (wt-tree/delete-min! tree)
      number-wt-type
      string-wt-type)
  )

(define (error:wrong-type-argument obj type-name function-name)
  (error function-name (xformat #f "Wrong type argument, expected type: (~a), given object: (~a)." type-name obj) obj)) 

(define (error:bad-range-argument index function-name)
  (error function-name (xformat #f "Bad range argument, index: (~a)." index) index))
  
(define fix:fixnum? (lambda (x) (and (exact? x) (integer? x))))
(define fix:+ +)
(define fix:- -)
(define fix:< <)
(define fix:<= <=)
(define fix:> >)
(define fix:* *)

;;  A TREE-TYPE is a collection of those procedures that depend on the
;;  ordering relation.

;; MIT-Scheme structure definition
;;(define-structure
;;    (tree-type
;;     (conc-name tree-type/)
;;     (constructor %make-tree-type))
;;  (key<?       #F read-only true)
;;  (alist->tree #F read-only true)
;;  (add         #F read-only true)
;;  (insert!     #F read-only true)
;;  (delete      #F read-only true)
;;  (delete!     #F read-only true)
;;  (member?     #F read-only true)
;;  (lookup      #F read-only true)
;;  (split-lt    #F read-only true)
;;  (split-gt    #F read-only true)
;;  (union       #F read-only true)
;;  (intersection #F read-only true)
;;  (difference  #F read-only true)
;;  (subset?     #F read-only true)
;;  (rank        #F read-only true)
;;)

;; Written out by hand, using vectors:
;;
;; If possible, you should teach your system to print out something
;; like #[tree-type <] instread of the whole vector.

(define tag:tree-type (string->symbol "#[(runtime wttree)tree-type]"))

(define (%make-tree-type key<?       alist->tree 
                         add         insert!     
                         delete      delete!     
                         member?     lookup      
                         split-lt    split-gt    
                         union       intersection 
                         difference  subset?     
                         rank        union+ intersection+ subset+? difference+)
  (vector tag:tree-type
          key<?       alist->tree   add         insert!     
          delete      delete!       member?     lookup      
          split-lt    split-gt      union       intersection 
          difference  subset?       rank        union+
          intersection+ subset+? difference+))

(define (tree-type? tt)
  (and (vector? tt)
       (eq? (vector-ref tt 0) tag:tree-type)))

(define (tree-type/key<?        tt) (vector-ref tt 1))
(define (tree-type/alist->tree  tt) (vector-ref tt 2))
(define (tree-type/add          tt) (vector-ref tt 3))
(define (tree-type/insert!      tt) (vector-ref tt 4))
(define (tree-type/delete       tt) (vector-ref tt 5))
(define (tree-type/delete!      tt) (vector-ref tt 6))
(define (tree-type/member?      tt) (vector-ref tt 7))
(define (tree-type/lookup       tt) (vector-ref tt 8))
(define (tree-type/split-lt     tt) (vector-ref tt 9))
(define (tree-type/split-gt     tt) (vector-ref tt 10))
(define (tree-type/union        tt) (vector-ref tt 11))
(define (tree-type/intersection tt) (vector-ref tt 12))
(define (tree-type/difference   tt) (vector-ref tt 13))
(define (tree-type/subset?      tt) (vector-ref tt 14))
(define (tree-type/rank         tt) (vector-ref tt 15))
(define (tree-type/union+       tt) (vector-ref tt 16))
(define (tree-type/intersection+ tt) (vector-ref tt 17))
(define (tree-type/subset+?      tt) (vector-ref tt 18))
(define (tree-type/difference+   tt) (vector-ref tt 19))

;;  User level tree representation.
;;
;;  WT-TREE is a wrapper for trees of nodes.
;;
;;MIT-Scheme:
;;(define-structure
;;    (wt-tree
;;     (conc-name tree/)
;;     (constructor %make-wt-tree))
;;  (type  #F read-only true)
;;  (root  #F read-only false))

;; If possible, you should teach your system to print out something
;; like #[wt-tree] instread of the whole vector.

(define tag:wt-tree (string->symbol "#[(runtime wttree)wt-tree]"))

(define (%make-wt-tree type root)
  (vector tag:wt-tree type root))

(define (wt-tree? t)
  (and (vector? t)
       (eq? (vector-ref t 0) tag:wt-tree)))

(define (tree/type t) (vector-ref t 1))
(define (tree/root t) (vector-ref t 2))
(define (set-tree/root! t v) (vector-set! t 2 v))

(define (wt-tree/type t)
  (tree/type t))


;;  Nodes are the thing from which the real trees are built.  There are
;;  lots of these and the uninquisitibe user will never see them, so
;;  they are represented as untagged to save the slot that would be
;;  used for tagging structures.
;;  In MIT-Scheme these were all DEFINE-INTEGRABLE

(define (make-node k v l r w) (vector w l k r v))
(define (node/k node) (vector-ref node 2))
(define (node/v node) (vector-ref node 4))
(define (node/l node) (vector-ref node 1))
(define (node/r node) (vector-ref node 3))
(define (node/w node) (vector-ref node 0))

(define empty  'empty)
(define (empty? x) (eq? x 'empty))

(define (node/size node)
  (if (empty? node) 0  (node/w node)))

(define (node/singleton k v) (make-node k v empty empty 1))

(define (with-n-node node receiver)
  (receiver (node/k node) (node/v node) (node/l node) (node/r node)))

;;
;;  Constructors for building node trees of various complexity
;;

(define (n-join k v l r)
  (make-node k v l r (fix:+ 1 (fix:+ (node/size l) (node/size r)))))

(define (single-l a.k a.v x r)
  (with-n-node r
               (lambda (b.k b.v y z) (n-join b.k b.v (n-join a.k a.v x y) z))))

(define (double-l a.k a.v x r)
  (with-n-node r
               (lambda (c.k c.v r.l z)
                 (with-n-node r.l
                              (lambda (b.k b.v y1 y2)
                                (n-join b.k b.v
                                        (n-join a.k a.v x y1)
                                        (n-join c.k c.v y2 z)))))))

(define (single-r b.k b.v l z)
  (with-n-node l
               (lambda (a.k a.v x y) (n-join a.k a.v x (n-join b.k b.v y z)))))

(define (double-r c.k c.v l z)
  (with-n-node l
               (lambda (a.k a.v x l.r)
                 (with-n-node l.r
                              (lambda (b.k b.v y1 y2)
                                (n-join b.k b.v
                                        (n-join a.k a.v x y1)
                                        (n-join c.k c.v y2 z)))))))

;; (define-integrable wt-tree-ratio 5)
(define wt-tree-ratio 5)

(define (t-join k v l r)
  (define (simple-join) (n-join k v l r))
  (let ((l.n  (node/size l))
        (r.n  (node/size r)))
    (cond ((fix:< (fix:+ l.n r.n) 2)   (simple-join))
          ((fix:> r.n (fix:* wt-tree-ratio l.n))
           ;; right is too big
           (let ((r.l.n  (node/size (node/l r)))
                 (r.r.n  (node/size (node/r r))))
             (if (fix:< r.l.n r.r.n)
                 (single-l k v l r)
                 (double-l k v l r))))
          ((fix:> l.n (fix:* wt-tree-ratio r.n))
           ;; left is too big
           (let ((l.l.n  (node/size (node/l l)))
                 (l.r.n  (node/size (node/r l))))
             (if (fix:< l.r.n l.l.n)
                 (single-r k v l r)
                 (double-r k v l r))))
          (else
           (simple-join)))))
;;
;;  Node tree procedures that are independent of key<?
;;

(define (node/min node)
  (cond  ((empty? node)          (error:empty 'min))
         ((empty? (node/l node)) node)
         (else                   (node/min (node/l node)))))

(define (node/delmin node)
  (cond ((empty? node)           (error:empty 'delmin))
        ((empty? (node/l node))  (node/r node))
        (else   (t-join (node/k node) (node/v node)
                        (node/delmin (node/l node)) (node/r node)))))

(define (node/concat2 node1 node2)
  (cond ((empty? node1)   node2)
        ((empty? node2)   node1)
        (else
         (let ((min-node (node/min node2)))
           (t-join (node/k min-node) (node/v min-node)
                   node1 (node/delmin node2))))))

(define (node/inorder-fold procedure base node)
  (define (fold base node)
    (if (empty? node)
        base
        (with-n-node node
                     (lambda (k v l r)
                       (fold (procedure k v (fold base r)) l)))))
  (fold base node))

(define (node/for-each procedure node)
  (if (not (empty? node))
      (with-n-node node
                   (lambda (k v l r)
                     (node/for-each procedure l)
                     (procedure k v)
                     (node/for-each procedure r)))))

(define (node/height node)
  (if (empty? node)
      0
      (+ 1 (max (node/height (node/l node))
                (node/height (node/r node))))))

(define (node/index node index)
  (define (loop node index)
    (let ((size.l  (node/size (node/l node))))
      (cond ((fix:< index size.l)  (loop (node/l node) index))
            ((fix:> index size.l)  (loop (node/r node)
                                         (fix:- index (fix:+ 1 size.l))))
            (else                  node))))
  (let ((bound  (node/size node)))
    (if (or (< index 0)
            (>= index bound)
            (not (fix:fixnum? index)))
        (error:bad-range-argument index 'node/index)
        (loop node index))))

(define (error:empty owner)
  (error 'error:empty "Operation requires non-empty tree:" owner))


(define (local:make-wt-tree-type key<?)

  ;; MIT-Scheme definitions:
  ;;(declare (integrate key<?))
  ;;(define-integrable (key>? x y)  (key<? y x))

  (define (key>? x y)  (key<? y x))

  (define (node/find k node)
    ;; Returns either the node or #f.
    ;; Loop takes D comparisons where D is the depth of the tree
    ;; rather than the traditional compare-low, compare-high which
    ;; takes on average 1.5(D-1) comparisons
    (define (loop this best)
      (cond ((empty? this)  best)
            ((key<? k (node/k this))   (loop (node/l this) best))
            (else (loop (node/r this) this))))
    (let ((best (loop node #f)))
      (cond ((not best)               #f)
            ((key<? (node/k best) k)  #f)
            (else                     best))))

  (define (node/rank k node rank)
    (cond ((empty? node)             #f)
          ((key<? k (node/k node))  (node/rank k (node/l node) rank))
          ((key>? k (node/k node))  
           (node/rank k (node/r node)
                      (fix:+ 1 (fix:+ rank (node/size (node/l node))))))
          (else                     (fix:+ rank (node/size (node/l node))))))
  
  (define (node/add node k v)
    (if (empty? node)
        (node/singleton k v)
        (with-n-node node
                     (lambda (key val l r)
                       (cond ((key<? k key)   (t-join key val (node/add l k v) r))
                             ((key<? key k)   (t-join key val l (node/add r k v)))
                             (else            (n-join key v   l r)))))))

  (define (node/delete x node)
    (if (empty? node)
        empty
        (with-n-node node
                     (lambda (key val l r)
                       (cond ((key<? x key)   (t-join key val (node/delete x l) r))
                             ((key<? key x)   (t-join key val l (node/delete x r)))
                             (else            (node/concat2 l r)))))))

  (define (node/concat tree1 tree2)
    (cond ((empty? tree1)  tree2)
          ((empty? tree2)  tree1)
          (else
           (let ((min-node (node/min tree2)))
             (node/concat3 (node/k min-node) (node/v min-node) tree1
                           (node/delmin tree2))))))

  (define (node/concat3 k v l r)
    (cond ((empty? l)   (node/add r k v))
          ((empty? r)   (node/add l k v))
          (else
           (let ((n1  (node/size l))
                 (n2  (node/size r)))
             (cond ((fix:< (fix:* wt-tree-ratio n1) n2)
                    (with-n-node r
                                 (lambda (k2 v2 l2 r2)
                                   (t-join k2 v2 (node/concat3 k v l l2) r2))))
                   ((fix:< (fix:* wt-tree-ratio n2) n1)
                    (with-n-node l
                                 (lambda (k1 v1 l1 r1)
                                   (t-join k1 v1 l1 (node/concat3 k v r1 r)))))
                   (else
                    (n-join k v l r)))))))

  (define (node/split-lt node x)
    (cond ((empty? node)  empty)
          ((key<? x (node/k node))
           (node/split-lt (node/l node) x))
          ((key<? (node/k node) x)
           (node/concat3 (node/k node) (node/v node) (node/l node)
                         (node/split-lt (node/r node) x)))
          (else (node/l node))))

  (define (node/split-gt node x)
    (cond ((empty? node)  empty)
          ((key<? (node/k node) x)
           (node/split-gt (node/r node) x))
          ((key<? x (node/k node))
           (node/concat3 (node/k node) (node/v node) 
                         (node/split-gt (node/l node) x) (node/r node)))
          (else (node/r node))))

  (define (node/union tree1 tree2)
    (cond ((empty? tree1)  tree2)
          ((empty? tree2)  tree1)
          (else
           (with-n-node tree2
                        (lambda (ak av l r)
                          (let ((l1  (node/split-lt tree1 ak))
                                (r1  (node/split-gt tree1 ak)))
                            (node/concat3 ak av (node/union l1 l) (node/union r1 r))))))))
  
  (define (node/union+ tree1 tree2 proc-merge-values)
    (let loop ((tree1 tree1)
               (tree2 tree2))
      (cond ((empty? tree1)  tree2)
            ((empty? tree2)  tree1)
            (else
             (with-n-node tree2
                          (lambda (ak av l r)
                            (let* ((n1 (node/find ak tree1))
                                   (av (if n1 (proc-merge-values (node/v n1) av) av))
                                   (l1 (node/split-lt tree1 ak))
                                   (r1 (node/split-gt tree1 ak)))
                              (node/concat3 ak av (loop l1 l) (loop r1 r)))))))))

  (define (node/difference tree1 tree2)
    (cond ((empty? tree1)   empty)
          ((empty? tree2)   tree1)
          (else
           (with-n-node tree2
                        (lambda (ak av l r)
                          (let ((l1  (node/split-lt tree1 ak))
                                (r1  (node/split-gt tree1 ak)))
                            (node/concat (node/difference l1 l)
                                         (node/difference r1 r))))))))
  
  ;; proc is tested against the values associated with nodes of tree1 and tree2 which
  ;; have the same key. proc should return two values:
  ;; 1) a flag indicating whether the node associated with the common key should be part of the result
  ;; or not.
  ;; 2) the new value for the key when the flag is #t.
  (define (node/difference+ tree1 tree2 proc)
    (let loop ((tree1 tree1)
               (tree2 tree2))
      (cond ((empty? tree1)   empty)
            ((empty? tree2)   tree1)
            (else
             (with-n-node tree2
                          (lambda (ak av l r)
                            (let* ((l1  (node/split-lt tree1 ak))
                                   (r1  (node/split-gt tree1 ak))
                                   (n1 (node/find ak tree1))
                                   (new-l (loop l1 l))
                                   (new-r (loop r1 r)))
                              (if (not n1)
                                (node/concat new-l new-r)
                                (multiple-value-bind
                                    (keep new-v)
                                    (proc (node/v n1) av)
                                  (if keep
                                    (node/concat3 ak new-v new-l new-r)
                                    (node/concat new-l new-r)))))))))))
    
  (define (node/intersection tree1 tree2)
    (cond ((empty? tree1)   empty)
          ((empty? tree2)   empty)
          (else
           (with-n-node tree2
                        (lambda (ak av l r)
                          (let ((l1  (node/split-lt tree1 ak))
                                (r1  (node/split-gt tree1 ak)))
                            (if (node/find ak tree1)
                                (node/concat3 ak av (node/intersection l1 l)
                                              (node/intersection r1 r))
                                (node/concat (node/intersection l1 l)
                                             (node/intersection r1 r)))))))))
  
  ;; proc-merge-values must return two values:
  ;; - the first value must be a boolean. #t means keep the entry, #f means delete the entry.
  ;; - the new value to be associated with the entry, when the first returned value is #t.
  (define (node/intersection+ tree1 tree2 proc-merge-values)
    (let loop ((tree1 tree1)
               (tree2 tree2))
      (cond ((empty? tree1)   empty)
            ((empty? tree2)   empty)
            (else
             (with-n-node tree2
                          (lambda (ak av l r)
                            (let ((l1  (node/split-lt tree1 ak))
                                  (r1  (node/split-gt tree1 ak)))
                              (cond
                               ((node/find ak tree1) =>
                                (lambda (n1)
                                  (multiple-value-bind
                                      (keep? av)
                                      (proc-merge-values (node/v n1) av)
                                    (if keep?
                                      (node/concat3 ak av 
                                                    (loop l1 l)
                                                    (loop r1 r))
                                      (node/concat (loop l1 l)
                                                   (loop r1 r))))))
                               (else
                                (node/concat (loop l1 l)
                                             (loop r1 r)))))))))))

  (define (node/subset? tree1 tree2)
    (or (empty? tree1)
        (and (fix:<= (node/size tree1) (node/size tree2))
             (with-n-node tree1
                          (lambda (k v l r)
                            v
                            (cond ((key<? k (node/k tree2))
                                   (and (node/subset? l (node/l tree2))
                                        (node/find k tree2)
                                        (node/subset? r tree2)))
                                  ((key>? k (node/k tree2))
                                   (and (node/subset? r (node/r tree2))
                                        (node/find k tree2)
                                        (node/subset? l tree2)))
                                  (else
                                   (and (node/subset? l (node/l tree2))
                                        (node/subset? r (node/r tree2))))))))))

  (define (node/subset+? tree1 tree2 proc)
    (let loop ((tree1 tree1)
               (tree2 tree2))
      (or (empty? tree1)
          (and (fix:<= (node/size tree1) (node/size tree2))
               (with-n-node tree1
                            (lambda (k v l r)
                              v
                              (cond ((key<? k (node/k tree2))
                                     (and (loop l (node/l tree2))
                                          (let ((n (node/find k tree2)))
                                            (and n
                                                 (proc v (node/v n))))
                                          (loop r tree2)))
                                    ((key>? k (node/k tree2))
                                     (and (loop r (node/r tree2))
                                          (let ((n (node/find k tree2)))
                                            (and n
                                                 (proc v (node/v n))))
                                          (loop l tree2)))
                                    (else
                                     (and (proc v (node/v tree2))
                                          (loop l (node/l tree2))
                                          (loop r (node/r tree2)))))))))))


    ;;; Tree interface: stripping off or injecting the tree types

  (define (tree/map-add tree k v)
    (%make-wt-tree (tree/type tree)
                   (node/add (tree/root tree) k v)))

  (define (tree/insert! tree k v)
    (set-tree/root! tree (node/add (tree/root tree) k v)))

  (define (tree/delete tree k)
    (%make-wt-tree (tree/type tree)
                   (node/delete k (tree/root tree))))

  (define (tree/delete! tree k)
    (set-tree/root! tree (node/delete k (tree/root tree))))

  (define (tree/split-lt tree key)
    (%make-wt-tree (tree/type tree)
                   (node/split-lt (tree/root tree) key)))

  (define (tree/split-gt tree key)
    (%make-wt-tree (tree/type tree)
                   (node/split-gt (tree/root tree) key)))

  (define (tree/union tree1 tree2)
    (%make-wt-tree (tree/type tree1)
                   (node/union (tree/root tree1) (tree/root tree2))))

  (define (tree/union+ tree1 tree2 proc)
    (%make-wt-tree (tree/type tree1)
                   (node/union+ (tree/root tree1) (tree/root tree2) proc)))

  (define (tree/intersection tree1 tree2)
    (%make-wt-tree (tree/type tree1)
                   (node/intersection (tree/root tree1) (tree/root tree2))))

  (define (tree/intersection+ tree1 tree2 proc)
    (%make-wt-tree (tree/type tree1)
                   (node/intersection+ (tree/root tree1) (tree/root tree2) proc)))

  (define (tree/difference tree1 tree2)
    (%make-wt-tree (tree/type tree1)
                   (node/difference (tree/root tree1) (tree/root tree2))))

  (define (tree/difference+ tree1 tree2 proc)
    (%make-wt-tree (tree/type tree1)
                   (node/difference+ (tree/root tree1) (tree/root tree2) proc)))

  (define (tree/subset? tree1 tree2)
    (node/subset? (tree/root tree1) (tree/root tree2)))

  (define (tree/subset+? tree1 tree2 proc)
    (node/subset+? (tree/root tree1) (tree/root tree2) proc))

  (define (alist->tree alist)
    (define (loop alist node)
      (cond ((null? alist)  node)
            ((pair? alist)  (loop (cdr alist)
                                  (node/add node (caar alist) (cdar alist))))
            (else           
             (error:wrong-type-argument alist "alist" 'alist->tree))))
    (%make-wt-tree my-type (loop alist empty)))

  (define (tree/get tree key default)
    (let ((node  (node/find key (tree/root tree))))
      (if node
          (node/v node)
          default)))

  (define (tree/rank tree key)  (node/rank key (tree/root tree) 0))

  (define (tree/member? key tree)
    (and (node/find key (tree/root tree))
         #t))

  (define my-type #F)

  (set! my-type
        (%make-tree-type
         key<?      ;  key<?
         alist->tree      ;  alist->tree
         tree/map-add     ;  add
         tree/insert!     ;  insert!
         tree/delete      ;  delete
         tree/delete!     ;  delete!
         tree/member?     ;  member?
         tree/get     ;  lookup
         tree/split-lt    ;  split-lt
         tree/split-gt    ;  split-gt
         tree/union     ;  union
         tree/intersection    ;  intersection
         tree/difference    ;  difference
         tree/subset?     ;  subset?
         tree/rank      ;  rank
         tree/union+
         tree/intersection+
         tree/subset+?
         tree/difference+
         ))

  my-type)

;;
;; guarantee functions are returning #t when executed with success... this modification will allow to use them
;; in assertion declarations.
;;

(define (guarantee-tree tree procedure)
  (if (not (wt-tree? tree))
      (error:wrong-type-argument tree "weight-balanced tree" procedure)
      #t))

(define (guarantee-tree-type type procedure)
  (if (not (tree-type? type))
      (error:wrong-type-argument type "weight-balanced tree type" procedure)
      #t))

(define (guarantee-compatible-trees tree1 tree2 procedure)
  (guarantee-tree tree1 procedure)
  (guarantee-tree tree2 procedure)
  (if (not (eq? (tree/type tree1) (tree/type tree2)))
      (error 'guarantee-compatible-trees
             (xformat #f "The trees ~a and ~a have incompatible types ~a and ~a."
                     tree1 tree2 (tree/type tree1) (tree/type tree2))
             #f)
      #t))

;;;______________________________________________________________________
;;;
;;;  Export interface
;;;
(define (make-wt-tree-type key<?)
  (local:make-wt-tree-type key<?))

(define (make-wt-tree tree-type)
  (%make-wt-tree tree-type empty))

(define (singleton-wt-tree type key value)
  [assert (type) (guarantee-tree-type type 'singleton-wt-tree)]
  (%make-wt-tree type (node/singleton key value)))

(define (alist->wt-tree type alist)
  [assert (type) (guarantee-tree-type type 'alist->wt-tree)]
  ((tree-type/alist->tree type) alist))

(define (wt-tree/empty? tree)
  [assert (tree) (guarantee-tree tree 'wt-tree/empty?)]
  (empty? (tree/root tree)))

(define (wt-tree/size tree)
  [assert (tree) (guarantee-tree tree 'wt-tree/size)]
  (node/size (tree/root tree)))

(define (wt-tree/add tree key datum)
  [assert (tree) (guarantee-tree tree 'wt-tree/add)]
  ((tree-type/add (tree/type tree)) tree key datum))

(define (wt-tree/delete tree key)
  [assert (tree) (guarantee-tree tree 'wt-tree/delete)]
  ((tree-type/delete (tree/type tree)) tree key))

(define (wt-tree/add! tree key datum)
  [assert (tree) (guarantee-tree tree 'wt-tree/add!)]
  ((tree-type/insert! (tree/type tree)) tree key datum))

(define (wt-tree/delete! tree key)
  [assert (tree) (guarantee-tree tree 'wt-tree/delete!)]
  ((tree-type/delete! (tree/type tree)) tree key))

(define (wt-tree/member? key tree)
  [assert (tree) (guarantee-tree tree 'wt-tree/member?)]
  ((tree-type/member? (tree/type tree)) key tree))

(define (wt-tree/lookup tree key default)
  [assert (tree) (guarantee-tree tree 'wt-tree/lookup)]
  ((tree-type/lookup (tree/type tree)) tree key default))

(define (wt-tree/split< tree key)
  [assert (tree) (guarantee-tree tree 'wt-tree/split<)]
  ((tree-type/split-lt (tree/type tree)) tree key))

(define (wt-tree/split> tree key)
  [assert (tree) (guarantee-tree tree 'wt-tree/split>)]
  ((tree-type/split-gt (tree/type tree)) tree key))

(define (wt-tree/union tree1 tree2)
  [assert (tree1 tree2) (guarantee-compatible-trees tree1 tree2 'wt-tree/union)]
  ((tree-type/union (tree/type tree1)) tree1 tree2))

(define (wt-tree/intersection tree1 tree2)
  [assert (tree1 tree2) (guarantee-compatible-trees tree1 tree2 'wt-tree/intersection)]
  ((tree-type/intersection (tree/type tree1)) tree1 tree2))

(define (wt-tree/union+ tree1 tree2 proc)
  [assert (tree1 tree2) (guarantee-compatible-trees tree1 tree2 'wt-tree/union)]
  ((tree-type/union+ (tree/type tree1)) tree1 tree2 proc))

(define (wt-tree/intersection+ tree1 tree2 proc)
  [assert (tree1 tree2) (guarantee-compatible-trees tree1 tree2 'wt-tree/intersection)]
  ((tree-type/intersection+ (tree/type tree1)) tree1 tree2 proc))

(define (wt-tree/difference tree1 tree2)
  [assert (tree1 tree2) (guarantee-compatible-trees tree1 tree2 'wt-tree/difference)]
  ((tree-type/difference (tree/type tree1)) tree1 tree2))

(define (wt-tree/difference+ tree1 tree2 proc)
  [assert (tree1 tree2) (guarantee-compatible-trees tree1 tree2 'wt-tree/difference+)]
  ((tree-type/difference+ (tree/type tree1)) tree1 tree2 proc))

(define (wt-tree/subset? tree1 tree2)
  [assert (tree1 tree2) (guarantee-compatible-trees tree1 tree2 'wt-tree/subset?)]
  ((tree-type/subset? (tree/type tree1)) tree1 tree2))

(define (wt-tree/subset+? tree1 tree2 proc)
  [assert (tree1 tree2) (guarantee-compatible-trees tree1 tree2 'wt-tree/subset+?)]
  ((tree-type/subset+? (tree/type tree1)) tree1 tree2 proc))

(define (wt-tree/set-equal? tree1 tree2)
  (and (wt-tree/subset? tree1 tree2)
       (wt-tree/subset? tree2 tree1)))

(define (wt-tree/fold combiner-key-datum-result init tree)
  [assert (tree) (guarantee-tree tree 'wt-tree/fold)]
  (node/inorder-fold combiner-key-datum-result
                     init
                     (tree/root tree)))

(define (wt-tree/for-each action-key-datum tree)
  [assert (tree) (guarantee-tree tree 'wt-tree/for-each)]
  (node/for-each action-key-datum (tree/root tree)))

(define (wt-tree/index tree index)
  [assert (tree) (guarantee-tree tree 'wt-tree/index)]
  (let ((node  (node/index (tree/root tree) index)))
    (and node (node/k node))))

(define (wt-tree/index-datum tree index)
  [assert (tree) (guarantee-tree tree 'wt-tree/index-datum)]
  (let ((node  (node/index (tree/root tree) index)))
          (and node (node/v node))))

(define (wt-tree/index-pair tree index)
  [assert (tree) (guarantee-tree tree 'wt-tree/index-pair)]
  (let ((node  (node/index (tree/root tree) index)))
    (and node (cons (node/k node) (node/v node)))))

(define (wt-tree/rank tree key)
  [assert (tree) (guarantee-tree tree 'wt-tree/rank)]
  ((tree-type/rank (tree/type tree)) tree key))

(define (wt-tree/min tree)
  [assert (tree) (guarantee-tree tree 'wt-tree/min)]
  (node/k (node/min (tree/root tree))))

(define (wt-tree/min-datum tree)
  [assert (tree) (guarantee-tree tree 'wt-tree/min-datum)]
  (node/v (node/min (tree/root tree))))

(define (wt-tree/min-pair tree)
  [assert (tree) (guarantee-tree tree 'wt-tree/min-pair)]
  (let ((node  (node/min (tree/root tree))))
    (cons (node/k node) (node/v node))))

(define (wt-tree/delete-min tree)
  [assert (tree) (guarantee-tree tree 'wt-tree/delete-min)]
  (%make-wt-tree (tree/type tree)
                 (node/delmin (tree/root tree))))

(define (wt-tree/delete-min! tree)
  [assert (tree) (guarantee-tree tree 'wt-tree/delete-min!)]
  (set-tree/root! tree (node/delmin (tree/root tree))))

;; < is a lexpr. Many compilers can open-code < so the lambda is faster
;; than passing <.
(define number-wt-type (local:make-wt-tree-type  (lambda (u v) (< u v))))
(define string-wt-type (local:make-wt-tree-type  string<?))


;;______________________________________________________________________________
;;
;; Test code, using maps from digit strings to the numbers they represent.
;;
;; (load-option 'wt-tree)

;;#|
(define (test)

  (define (make-map lo hi step)
    (let loop ((i lo) (map (make-wt-tree string-wt-type)))
      (if (> i hi)
          map
          (loop (+ i step) (wt-tree/add map (number->string i) i)))))

  (define (wt-tree->alist t)
    (wt-tree/fold (lambda (key datum rest) (cons (cons key datum) rest)) '() t))

  (define (try-all operation trees)
    (map (lambda (t1)
           (map (lambda (t2)
                  (operation t1 t2))
                trees))
         trees))

  (define (chunk tree)
    (let ((size  (wt-tree/size tree)))
      (if (< size 8)
          size
          (let* ((midpoint (if (even? size)
                               (/ size 2)
                               (/ (+ size 1) 2)))
                 (fulcrum  (wt-tree/index tree midpoint)))
            (list (chunk (wt-tree/split< tree fulcrum))
                  (list fulcrum)
                  (chunk (wt-tree/split> tree fulcrum)))))))

  (define (verify name result expected)
    (newline)
    (display "Test ") (display name)
    (if (equal? result expected)
        (begin
          (display " passed"))
        (begin
          (display " unexpected result")
          (newline)
          (display "Expected: " expected)
          (newline)
          (display "Got:      " result))))

  (let ((t1 (make-map 0 99 2))    ; 0,2,4,...,98
        (t2 (make-map 1 100 2))   ; 1,3,5,...,99
        (t3 (make-map 0 100 3)))  ; 0,3,6,...,99


    (verify 'alist (wt-tree->alist t3)  ;
            '(("0" . 0) ("12" . 12) ("15" . 15) ("18" . 18) ("21" . 21)
              ("24" . 24) ("27" . 27) ("3" . 3) ("30" . 30) ("33" . 33)
              ("36" . 36) ("39" . 39) ("42" . 42) ("45" . 45) ("48" . 48)
              ("51" . 51) ("54" . 54) ("57" . 57) ("6" . 6) ("60" . 60)
              ("63" . 63) ("66" . 66) ("69" . 69) ("72" . 72) ("75" . 75)
              ("78" . 78) ("81" . 81) ("84" . 84) ("87" . 87) ("9" . 9)
              ("90" . 90) ("93" . 93) ("96" . 96) ("99" . 99)))


    (verify 'union-sizes
            (try-all (lambda (t1 t2) (wt-tree/size (wt-tree/union t1 t2)))
                     (list t1 t2 t3))
            '((50 100 67) (100 50 67) (67 67 34)))

    (verify 'difference-sizes
            (try-all (lambda (t1 t2)
                       (wt-tree/size (wt-tree/difference t1 t2)))
                     (list t1 t2 t3))
            '((0 50 33) (50 0 33) (17 17 0)))

    (verify 'intersection-sizes
            (try-all (lambda (t1 t2)
                       (wt-tree/size (wt-tree/intersection t1 t2)))
                     (list t1 t2 t3))
            '((50 0 17) (0 50 17) (17 17 34)))

    (verify 'equalities
            (try-all (lambda (t1 t2)
                       (wt-tree/set-equal? (wt-tree/difference t1 t2)
                                           (wt-tree/difference t2 t1)))
                     (list t1 t2 t3))
            '((#t #f #f) (#f #t #f) (#f #f #t)))

    (verify 'indexing
            (chunk (make-map 0 99 1))
            '((((7 ("15") 5) ("20") (6 ("27") 4)) ("31")
               ((6 ("38") 5) ("43") (6 ("5") 4)))
              ("54")
              (((7 ("61") 5) ("67") (6 ("73") 4)) ("78")
               ((6 ("84") 5) ("9") (5 ("95") 4)))))
    (newline)))
;;|#

