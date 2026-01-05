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

(module heap
        (include "utility.sch")
        (export (make-heap . lt?)
                (heap/add! heap val el)
                (heap/empty? heap)
                (heap/delete-max! heap))
        )

(define *heap-initial-max-len* 17)

(define (make-heap-entry val el)
  (cons val el))

(define-inline (heap-entry/val entry)
  (car entry))

(define-inline (heap-entry/element entry)
  (cdr entry))

(define-record-type heap
  (mk-heap max-len len array lt?)
  heap?
  (max-len heap/max-len heap/set-max-len!)
  (len heap/len heap/set-len!)
  (array heap/array heap/set-array!)
  (lt? heap/lt?))

(define (make-heap . lt?)
  (let ((lt? (optional-arg lt? <)))
    (mk-heap *heap-initial-max-len* 0 (make-vector *heap-initial-max-len* #unspecified) lt?)))

(define (heap/switch! heap pos1 pos2)
  (let* ((array (heap/array heap))
         (aux (vector-ref array pos1)))
    (vector-set! array pos1 (vector-ref array pos2))
    (vector-set! array pos2 aux)))

(define (heap/check-size! heap)
  (when (= (heap/max-len heap) (heap/len heap))
    (heap/set-max-len! heap (+ (* (heap/max-len heap) 2) 1))
    (heap/set-array! heap (copy-vector (heap/array heap) (heap/max-len heap)))))

(define (heap/add! heap val el)
  (heap/check-size! heap)
  (let ((pos (heap/len heap))
        (array (heap/array heap))
        (lt? (heap/lt? heap)))
    (heap/set-len! heap (+ pos 1))
    (vector-set! array pos (make-heap-entry val el))
    (let loop ((pos pos))
      (when (> pos 0)
        (let ((new-pos (/fx (- pos 1) 2)))
          (when (lt? (heap-entry/val (vector-ref array new-pos))
                     (heap-entry/val (vector-ref array pos)))
            (heap/switch! heap pos new-pos)
            (loop new-pos)))))))

(define (heap/empty? heap)
  (= (heap/len heap) 0))

(define (heap/delete-max! heap)
  [assert (heap) (not (heap/empty? heap))]
  (let* ((array (heap/array heap))
         (el (heap-entry/element (vector-ref array 0)))
         (lt? (heap/lt? heap)))
    (heap/set-len! heap (- (heap/len heap) 1))
    (unless (heap/empty? heap)
      (let ((pos 0))
        ;; copy last element to the first position
        (vector-set! array 0 (vector-ref array (heap/len heap)))
        (let loop ((pos pos))
          (when (< (+ (* 2 pos) 1) (heap/len heap))
            (let ((new-pos (+ (* 2 pos) 1)))
              (when (or (lt? (heap-entry/val (vector-ref array pos))
                             (heap-entry/val (vector-ref array new-pos)))
                        (lt? (heap-entry/val (vector-ref array pos))
                             (heap-entry/val (vector-ref array (+ new-pos 1)))))
                (cond
                 ((lt? (heap-entry/val (vector-ref array (+ new-pos 1)))
                       (heap-entry/val (vector-ref array new-pos)))
                  (heap/switch! heap pos new-pos)
                  (loop new-pos))
                 (else
                  (heap/switch! heap pos (+ new-pos 1))
                  (loop (+ new-pos 1))))))))))
    el))


