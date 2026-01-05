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

(module bdd-util
        (include "utility.sch")
        (include "trace.sch")
        (import bdd heap)
        (export (bdd/apply* proc . bdd-list)
                (bdd/and* bdd . bdd-list)
                (bdd/or* bdd . bdd-list)
                (bdd-list/size bdd-list)
                (bdd/cube-diff cube1 cube2)
                (bdd/cube-intersection cube1 cube2)
                (bdd/cube-union cube1 cube2)
                (bdd/empty-cube? cube)
                (bdd/affinity b1 b2)
                (bdd/affinity-cluster bdd-list proc threshold)
                (bdd-and/affinity-cluster bdd-list threshold)
                (bdd-or/affinity-cluster bdd-list threshold))
        )

(define (bdd/apply* proc . bdd-list)
  [assert (bdd-list) (not (null? bdd-list))]
  (fold-left (lambda (result-bdd bdd)
               (proc result-bdd bdd))
             (car bdd-list)
             (cdr bdd-list)))

(define (bdd-list/size bdd-list)
  (fold-left (lambda (size bdd) (+ size (bdd/size bdd))) 0 bdd-list))

(define (bdd/and* bdd . bdd-list)
  (apply bdd/apply* bdd/and bdd bdd-list))

(define (bdd/or* bdd . bdd-list)
  (apply bdd/apply* bdd/or bdd bdd-list))

;; computes the difference between two BDD cubes, i.e., the
;; cube of BDD variables belonging to cube1 nd not belonging to cube2. 
;; return a new cube.
(define (bdd/cube-diff cube1 cube2)
  (bdd/exists cube1 cube2))

;; computes the intersection between two BDD cubes.
;; computes the difference between two BDD cubes, i.e., the
;; cube of BDD variables belonging to cube1 AND belonging to cube2
;; return a new BDD cube.
(define (bdd/cube-intersection cube1 cube2)
  (let ((tmp (bdd/cube-diff cube1 cube2)))
    (bdd/cube-diff cube1 tmp)))

;; computes the union between two BDD cubes, i.e the
;; cube of BDD variables belonging to cube1 OR to cube2.
;; return a new BDD cube.
(define (bdd/cube-union cube1 cube2)
  (bdd/and cube1 cube2))

(define (bdd/empty-cube? cube)
  (bdd/true? cube))
          
;; Compute the `affinity' between two BDDs as 
;; suggested by Moon, Hachtel, Somenzi.
;; `affinity' is the ratio between the number of 
;; shared variables and the number of the union of
;; all variables
(define (bdd/affinity bdd1 bdd2)
  (let* ((supp1 (bdd/support bdd1))
         (supp2 (bdd/support bdd2))
         (I (bdd/cube-intersection supp1 supp2))
         (U (bdd/cube-union supp1 supp2))
         (U-size (bdd/size U)))
    (if (= U-size 0)
      0.0
      (/fl (exact->inexact (bdd/size I))
           (exact->inexact U-size)))))

;----------------------------------------------------------
;
; BDD clustering based on affinity
;
;----------------------------------------------------------

;; flag is #t if the entry (and associated bdd) is still active (i.e., it was
;; not clustered with another bdd yet) 
(define (make-entry flag bdd)
  (cons flag bdd))

(define-inline (entry/flag e)
  (car e))

(define-inline (entry/bdd e)
  (cdr e))

(define-inline (entry/set-flag! e flag)
  (set-car! e flag))

;; I just mark the entry as inactive
(define (entry/lazy-delete! e)
  (entry/set-flag! e #f))

(define (entry/active? e)
  (entry/flag e))

;; Compute the affinity of bdd with every active entry of "l".
;; The result are stored in the heap "h".
;; the result is a new list = (cons (make-entry #t bdd) l)
(define (list-heap-add bdd l h)
  (let ((n-e (make-entry #t bdd)))
    (let loop ((l l))
      (unless (null? l)
        (let ((l-e (car l)))
          (when (entry/flag l-e)
            (let ((heap-entry (cons l-e n-e))
                  (affinity (bdd/affinity (entry/bdd l-e) bdd)))
              (heap/add! h affinity heap-entry)))
          (loop (cdr l)))))
    (cons n-e l)))

;; initialize bdd clustering data structures
;; bdd-list is a list of bdd to be clustered
;; threshold is the clustering threshold
;; the function returns four results:
;;  - number of bdds to be clustered
;;  - a list of bdds that should not be clustered, since their size is above the threshold.
;;  - a list of entries to be clustered (I'm not using a list of bdds here, because I need
;;        an efficient procedure to "delete" elements from this list and the heap.
;;  - a heap of pairs of entries. The heap is ordered by the affinity of the bdds associated
;;    with each pair.
(define (affinity-move bdd-list threshold)
  (let ((h (make-heap <fl)))
    (let loop ((bdd-list bdd-list)
               (new-bdd-list '())
               (n 0) ;; number of bdds to be clustered 
               (l '()))
      (if (null? bdd-list)
        (values n new-bdd-list l h)
        (let ((bdd (car bdd-list)))
          (if (> (bdd/size bdd) threshold)
            (loop (cdr bdd-list) (cons bdd new-bdd-list) n  l)
            (let ((new-l (list-heap-add bdd l h)))
              (loop (cdr bdd-list) new-bdd-list (+ n 1) new-l))))))))
              
;; return a new list of bdds...
;; - the bdd are combined using the function proc.
;; - when proc is bdd/and-limit (bdd/or-limit)
;;     the conjunction (disjunction) of the result list is equivalent to the
;;     conjunction (disjunction) of the list "bdd-list".
;; - the length of the result list is <= the length of "bdd-list".
;; - the length of the list is decreased by using proc to
;;   combine bdds of "bdd-list"
;; - only bdds with size <= threshold are combined.
;; - the function bdd/affinity is used to decide which bdds will
;;   be combined.
;; - proc receives two arguments (bdds)
(define (bdd/affinity-cluster bdd-list bdd-proc threshold)
  ;; new version of bdd/affinity-cluster using bdd/and-limit or bdd/or-limit
  (trace 'affinity-cluster "starting bdd/affinity-cluster")
  (multiple-value-bind
      (n new-bdd-list l h)
      (affinity-move bdd-list threshold)
    (let loop ((n n)
               (new-bdd-list new-bdd-list)
               (l l))
      (trace 'affinity-cluster "n: ~a" n)
      (cond
       ((= n 0)
        new-bdd-list)
       ((heap/empty? h)
        ;; move all active entries to new-bdd-list
        (let inner-loop ((l l)
                         (new-bdd-list new-bdd-list))
          (if (null? l)
            new-bdd-list
            (let ((e (car l)))
              (if (entry/active? e)
                (inner-loop (cdr l) (cons (entry/bdd e) new-bdd-list))
                (inner-loop (cdr l) new-bdd-list))))))
       (else
        [assert (h) (not (heap/empty? h))]
        (let* ((pair (heap/delete-max! h))
               (e1 (car pair))
               (e2 (cdr pair)))
          (if (and (entry/active? e1)
                   (entry/active? e2))
            (let* ((bdd1 (entry/bdd e1))
                   (bdd2 (entry/bdd e2))
                   ;; (_ (force-gc!))
                   (new-bdd (bdd-proc bdd1 bdd2 threshold)))
              ;; (breakpoint "affinity" (bdd1 bdd2 new-bdd threshold) (and new-bdd (> (bdd/size new-bdd) 100000)))
              (cond
               ((and new-bdd
                     (< (bdd/size new-bdd) threshold))
                (entry/lazy-delete! e1)
                (entry/lazy-delete! e2)
                (if (> (bdd/size new-bdd) threshold)
                  (loop (- n 2) (cons new-bdd new-bdd-list) l)
                  (let ((new-l (list-heap-add new-bdd l h)))
                    (loop (- n 1) new-bdd-list new-l))))
               (else
                (loop n new-bdd-list l))))
            (loop n new-bdd-list l))))))))

(define (bdd-and/affinity-cluster bdd-list threshold)
  (cond
   ((find bdd/false? bdd-list) =>
    (lambda (empty)
      (list empty)))
   (else
    (let ((bdd-list (filter (lambda (bdd) (not (bdd/true? bdd))) bdd-list)))
      (bdd/affinity-cluster bdd-list bdd/and-limit threshold)))))
 
(define (bdd-or/affinity-cluster bdd-list threshold)
  (cond
   ((find bdd/true? bdd-list) =>
    (lambda (empty)
      (list empty)))
   (else
    (let ((bdd-list (filter (lambda (bdd) (not (bdd/false? bdd))) bdd-list)))
      (bdd/affinity-cluster bdd-list bdd/or-limit threshold)))))
