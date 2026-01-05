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

(module queue
        (include "utility.sch")
        (import xformat)
        (export 
         (make-queue . args)
         (queue/reset! queue)
         (queue? obj)
         (queue/empty? q)
         (queue/first-pair q)
         (queue/last-pair q)
         (queue/front q)
         (queue/rear q)
         (queue->list q)
         (queue/length q)
         (list->queue alist)
         (queue/fast-append-last! q to-append)
         (queue/insert! q datum)
         (queue/append! q lst)
         (queue/push! q datum)
         (queue/pop! q)
         (queue/filter! proc q)
         (queue/sync! q))
        )

(define (make-queue . args)
  (let ((result (cons '() '())))
    (for-each (lambda (arg)
                (queue/insert! result arg))
              args)
    result))

(define (queue/reset! queue)
  (set-car! queue '())
  (set-cdr! queue '()))

(define (queue? obj)
  (and (pair? obj) (list? (car obj)) (list? (cdr obj))))

(define (queue/empty? q)
  (eq? (car q) '()))

(define (queue/first-pair q)
  [assert (q) (and (queue? q) (not (queue/empty? q)))]
  (car q))

(define (queue/last-pair q)
  [assert (q) (and (queue? q) (not (queue/empty? q)))]
  (cdr q))

(define (queue/front q)
  [assert (q) (and (queue? q) (not (queue/empty? q)))]
  (caar q))

(define (queue/rear q)
  [assert (q) (and (queue? q) (not (queue/empty? q)))]
  (cadr q))

(define (queue->list q)
  (car q))

(define (queue/length q)
  (length (car q)))

(define (list->queue alist)
  (queue/sync! (cons alist '())))

;; DANGER!!!
;; This destructive function appends a list at the end of the queue,
;; and returns the resulting list!
;; Note: the queue structure is not updated!
;; This function should only be used if the queue will not be used again.
(define (queue/fast-append-last! q to-append)
  (if (queue/empty? q)
    to-append
    (begin 
      (set-cdr! (cdr q) to-append)
      (car q))))

(define (queue/insert! q datum)
  (let ((new-pair (cons datum '())))
    (cond 
     ((eq? (car q) '())
      [assert (q) (eq? (cdr q) '())]
      (set-car! q new-pair)
      (set-cdr! q new-pair))
     (else
      (set-cdr! (cdr q) new-pair)
      (set-cdr! q new-pair))))
  q)

(define (queue/append! q lst)
  (for-each (cut queue/insert! q <>) lst)
  q)

(define (queue/push! q datum)
  (cond 
   ((eq? (car q) '())
    (queue/insert! q datum))
   (else
    (set-car! q (cons datum (car q)))))
  q)

(define (queue/pop! q)
  (when (eq? (car q) '())
    (error 'queue/pop! "empty queue" q))
  (let ((result (caar q)))
    (cond
     ((eq? (cdar q) '())
      (set-car! q '())
      (set-cdr! q '()))
     (else
      (set-car! q (cdr (car q)))))
    result))

(define (queue/sync! q)
  (let* ((first-pair (car q))
         (last-pair (if (null? first-pair)
                      '()
                      (let loop ((curr first-pair))
                        (if (null? (cdr curr))
                          curr
                          (loop (cdr curr)))))))
    (set-cdr! q last-pair)
    q))

;; this is not an optimal implementation
;; return a list of elements that were removed
(define (queue/filter! proc q)
  (let* ((removed-elems (make-queue))
         (non-removed-elems (make-queue)))
    (for-each (lambda (e)
                (if (proc e)
                  (queue/insert! non-removed-elems e) 
                  (queue/insert! removed-elems e)))
              (queue->list q))
    (set-car! q (queue->list non-removed-elems))
    (queue/sync! q)
    (queue->list removed-elems)))


    
