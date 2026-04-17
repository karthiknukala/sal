;;
;; SAL 3.3, Copyright (C) 2026, SRI International.  All Rights Reserved.
;;
;; SAL is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;

(module cdr-frames
        (include "sal.sch")
        (import sal-ast-table utility)
        (export <cdr-frame-store>
                (make-cdr-frame-store)
                (cdr-frame-store/num-frames store)
                (cdr-frame-store/add-frame! store)
                (cdr-frame-store/frame-lemmas store level)
                (cdr-frame-store/add-lemma-at-level! store level lemma)
                (cdr-frame-store/add-lemma! store max-level lemma)
                (cdr-frame-store/frame-contains? store level lemma)
                (cdr-frame-store/converged? store level)))

(define-class <cdr-frame-store> ()
  (:num-frames :frame-lemmas :frame-tables))

(define (make-frame-lemma-table)
  (make-sal-ast-table))

(define (make-cdr-frame-store)
  (make-instance <cdr-frame-store>
                 :num-frames 2
                 :frame-lemmas (vector '() '())
                 :frame-tables (vector (make-frame-lemma-table)
                                       (make-frame-lemma-table))))

(define (cdr-frame-store/num-frames store)
  (slot-value store :num-frames))

(define (ensure-frame-capacity! store level)
  (let ((capacity (vector-length (slot-value store :frame-lemmas))))
    (when (>= level capacity)
      (let* ((new-len (max (+ level 1) (* 2 capacity)))
             (new-lemmas (make-vector new-len '()))
             (new-tables (make-vector new-len #f)))
        (let loop ((i 0))
          (when (< i capacity)
            (vector-set! new-lemmas i (vector-ref (slot-value store :frame-lemmas) i))
            (vector-set! new-tables i (vector-ref (slot-value store :frame-tables) i))
            (loop (+ i 1))))
        (let loop ((i capacity))
          (when (< i new-len)
            (vector-set! new-tables i (make-frame-lemma-table))
            (loop (+ i 1))))
        (set-slot-value! store :frame-lemmas new-lemmas)
        (set-slot-value! store :frame-tables new-tables)))))

(define (cdr-frame-store/add-frame! store)
  (let ((level (slot-value store :num-frames)))
    (ensure-frame-capacity! store level)
    (set-slot-value! store :num-frames (+ level 1))
    level))

(define (cdr-frame-store/frame-lemmas store level)
  (vector-ref (slot-value store :frame-lemmas) level))

(define (cdr-frame-store/frame-table store level)
  (vector-ref (slot-value store :frame-tables) level))

(define (cdr-frame-store/frame-contains? store level lemma)
  (sal-ast-table/get (cdr-frame-store/frame-table store level) lemma))

(define (cdr-frame-store/add-lemma-at-level! store level lemma)
  (unless (cdr-frame-store/frame-contains? store level lemma)
    (sal-ast-table/put! (cdr-frame-store/frame-table store level) lemma #t)
    (vector-set! (slot-value store :frame-lemmas)
                 level
                 (cons lemma (vector-ref (slot-value store :frame-lemmas) level)))
    #t))

(define (cdr-frame-store/add-lemma! store max-level lemma)
  (let loop ((i 1)
             (changed? #f))
    (cond
     ((> i max-level)
      changed?)
     (else
      (loop (+ i 1)
            (or (cdr-frame-store/add-lemma-at-level! store i lemma)
                changed?))))))

(define (cdr-frame-store/converged? store level)
  (and (< (+ level 1) (cdr-frame-store/num-frames store))
       (let ((lemmas (cdr-frame-store/frame-lemmas store level))
             (next-lemmas (cdr-frame-store/frame-lemmas store (+ level 1))))
         (and (= (length lemmas) (length next-lemmas))
              (for-all (lambda (lemma)
                         (cdr-frame-store/frame-contains? store (+ level 1) lemma))
                       lemmas)))))
