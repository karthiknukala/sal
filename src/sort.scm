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

(module sort
        (include "utility.sch")
        (import queue)
        (export (sort! vect lt? . size)
                (bucket-sort! vect num-buckets ord . size))
        )

(define *insertion-sort-threshold* 10)

(define (insertion-sort! vect lo hi lt?)
  (let loop ((i (+ lo 1)))
    (when (<= i hi)
      (let ((v (vector-ref vect i)))
        (let inner-loop ((j i))
          (let ((new-j (- j 1)))
            (cond
             ((and (> j lo)
                   (lt? v (vector-ref vect new-j)))
              (vector-set! vect j (vector-ref vect new-j))
              (inner-loop new-j))
             (else
              (vector-set! vect j v)))))
        (loop (+ i 1))))))

(define (quicksort! vect l r lt?)
  (let qsort ((l l)
              (r r))
    (when (> (- r l) *insertion-sort-threshold*)
      (let ((i (/fx (+ r l) 2)))
        ;; tri-median method
        (when (lt? (vector-ref vect i) (vector-ref vect l))
          (vector/swap-elements! vect l i))
        (when (lt? (vector-ref vect r) (vector-ref vect l))
          (vector/swap-elements! vect l r))
        (when (lt? (vector-ref vect i) (vector-ref vect r))
          (vector/swap-elements! vect i r))
        (let ((j (- r 1)))
          (vector/swap-elements! vect i j)
          (let ((i l)
                (v (vector-ref vect j)))
            (let loop ()
              (let inner-loop ()
                (set! i (+ i 1))
                (when (lt? (vector-ref vect i) v)
                  (inner-loop)))
              (let inner-loop ()
                (set! j (- j 1))
                (when (lt? v (vector-ref vect j))
                  (inner-loop)))
              (when (>= j i)
                (vector/swap-elements! vect i j)
                (loop)))
            (vector/swap-elements! vect i (- r 1))
            (qsort l j)
            (qsort (+ i 1) r)))))))

(define (sort! vect lt? . size)
  (let ((size (optional-arg size (vector-length vect))))
    (quicksort! vect 0 (- size 1) lt?)
    (insertion-sort! vect 0 (- size 1) lt?)))

(define (bucket-sort! vect num-buckets ord . size)
  (let ((size (optional-arg size (vector-length vect)))
        (buckets (make-vector num-buckets #f)))
    (let loop ((i 0))
      (when (< i size)
        (let ((elem (vector-ref vect i)))
          (let ((bucket-id (ord elem)))
            [assert (bucket-id elem num-buckets) (and (>= bucket-id 0) (< bucket-id num-buckets))]
            (unless (vector-ref buckets bucket-id)
              (vector-set! buckets bucket-id (make-queue)))
            (queue/insert! (vector-ref buckets bucket-id) elem)))
        (loop (+ i 1))))
    (let ((i 0))
      (vector/for-each (lambda (bucket)
                         (when bucket
                           (for-each (lambda (elem)
                                       (vector-set! vect i elem)
                                       (set! i (+ i 1)))
                                     (queue->list bucket))))
                       buckets))))

                      
