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

;;
;; Generic iterators
;;
;; a generic iterator is a function that receives one argument
;; the value of this argument may be 'has-next? or 'next!
;;
;; optionally an iterator may also receive the value 'start!
(module iterators
        (include "iterators.macros")
        (import utility queue)
        (export   
         *empty-iterator*
         (make-list-iterator alist)
         (make-queue-iterator aqueue)
         (make-singleton-iterator elem)
         (make-vector-iterator avector)
         (make-string-interator astring)
         (make-interval-iterator lower upper)
         (inline iterator/has-next? it)
         (inline iterator/next! it)
         (inline iterator/start! it)
         (iterator/for-each-1 proc it)
         (iterator/for-each proc . its)
         (iterator/length it)
         (iterator/extended-for-each-1 proc it)
         (iterator/extended-for-each proc . its)
         (iterator/for-all-1 proc it)
         (iterator/for-all proc . its)
         (iterator/exists-1 proc it)
         (iterator/map-1 proc it)
         (iterator/map proc . its)
         (iterator/fold-left proc base it)                
         (iterator/fold-right proc it base)
         (iterator/product-2 proc it1 it2)
         (iterator/product proc . its)
         (iterator/exists proc . its)
         (iterator/find proc it)
         (iterator/filter proc it)
         (iterator->list it)
         (iterator/split it)
         (iterator/number-of-elements it)
         (iterator/append it1 it2)
         (iterator/append* it-list))
        )

(define-inline (iterator/has-next? it)
  ((vector-ref it 0)))

(define-inline (iterator/next! it)
  ((vector-ref it 1)))

(define-inline (iterator/start! it)
  ((vector-ref it 2)))

(define *empty-iterator*
  (make-iterator #f #unspecified #unspecified))

(define (make-list-iterator alist)
  (let ((start alist)
        (curr alist))
    (make-iterator (not (null? curr))
                   (let ((result (car curr)))
                     (set! curr (cdr curr))
                     result)
                   (set! curr start))))

(define (make-queue-iterator aqueue)
  (make-list-iterator (queue->list aqueue)))

(define (make-singleton-iterator elem)
  (make-list-iterator (list elem)))

(define (make-vector-iterator avector)
  (let ((curr 0))
    (make-iterator (< curr (vector-length avector))
                   (let ((result (vector-ref avector curr)))
                     (set! curr (+ curr 1))
                     result)
                   (set! curr 0))))

(define (make-string-interator astring)
  (let ((curr 0))
    (make-iterator (< curr (string-length astring))
                   (let ((result (string-ref astring curr)))
                     (set! curr (+ curr 1))
                     result)
                   (set! curr 0))))

(define (make-interval-iterator lower upper)
  (let ((curr lower))
    (make-iterator (<= curr upper)
                   (let ((result curr))
                     (set! curr (+ curr 1))
                     result)
                   (set! curr lower))))

(define (iterator/for-each-1 proc it)
  (let loop ()
    (when (iterator/has-next? it)
      (proc (iterator/next! it))
      (loop))))

(define (iterator/for-each proc . its)
  (cond ((null? its)
         #unspecified)
        ((null? (cdr its))
         (iterator/for-each-1 proc (car its)))
        (else
         (let loop ()
           (when (for-all iterator/has-next? its)
             (apply proc (map iterator/next! its))
             (loop))))))

(define (iterator/length it)
  (let ((result 0))
    (iterator/for-each-1 (lambda (_) (set! result (+ result 1))) it)
    result))

(define (iterator/extended-for-each-1 proc it)
  (let loop ()
    (when (iterator/has-next? it)
      (let ((next (iterator/next! it)))
        (proc (not (iterator/has-next? it)) next)
        (loop)))))

(define (iterator/extended-for-each proc . its)
  (cond ((null? its)
         #unspecified)
        ((null? (cdr its))
         (iterator/extended-for-each-1 proc (car its)))
        (else
         (let loop ()
           (when (for-all iterator/has-next? its)
             (let* ((nexts (map iterator/next! its))
                    (last? (not (for-all iterator/has-next? its))))
               (apply proc (cons last? nexts)))
             (loop))))))

(define (iterator/for-all-1 proc it)
  (let loop ()
    (or (not (iterator/has-next? it))
        (and (proc (iterator/next! it)) (loop)))))

(define (iterator/for-all proc . its)
  (cond ((null? its)
         #unspecified)
        ((null? (cdr its))
         (iterator/for-all-1 proc (car its)))
        (else
         (let loop ()
           (or (not (for-all iterator/has-next? its))
               (and (apply proc (map iterator/next! its)) (loop)))))))

(define (iterator/exists-1 proc it)
  (let loop ()
    (and (iterator/has-next? it)
         (or (proc (iterator/next! it)) (loop)))))

(define (iterator/exists proc . its)
  (cond ((null? its)
         #unspecified)
        ((null? (cdr its))
         (iterator/exists-1 proc (car its)))
        (else   
         (let loop ()
           (and (for-all iterator/has-next? its)
                (or (apply proc (map iterator/next! its)) (loop)))))))

(define (iterator/map-1 proc it)
  (make-iterator (iterator/has-next? it)
                 (proc (iterator/next! it))
                 (iterator/start! it)))

(define (iterator/map proc . its)
  (cond ((null? its)
         #unspecified)
        ((null? (cdr its))
         (iterator/map-1 proc (car its)))
        (else
         (make-iterator (for-all iterator/has-next? its)
                        (apply proc (map iterator/next! its))
                        (for-each iterator/start! its)))))

(define (iterator/find proc it)
  (let loop ()
    (cond
     ((iterator/has-next? it)
      (let ((next (iterator/next! it)))
        (if (proc next)
          next
          (loop))))
     (else #f))))

(define (iterator/filter proc it)
  (let ((cached-value (iterator/find proc it)))
    (make-iterator cached-value
                   (let ((result cached-value))
                     (set! cached-value (iterator/find proc it))
                     result)
                   (begin
                     (iterator/start! it)
                     (set! cached-value (iterator/find proc it))))))

(define (iterator/fold-left proc base it)
  (let loop ((curr base))
    (if (iterator/has-next? it)
      (loop (proc curr (iterator/next! it)))
      curr)))

(define (iterator/fold-right proc it base)
  (let loop ()
    (if (iterator/has-next? it)
      (proc (iterator/next! it) (loop))
      base)))
  
(define (iterator/product-2 proc it1 it2)
  (let ((cache (if (iterator/has-next? it2) (iterator/next! it2) #f)))
    (make-iterator (or (iterator/has-next? it1) (iterator/has-next? it2))
                   (begin 
                     (unless (iterator/has-next? it1)
                       (set! cache (iterator/next! it2))
                       (iterator/start! it1))
                     (proc (iterator/next! it1) cache))
                   (begin (iterator/start! it1) 
                          (iterator/start! it2)
                          (set! cache (if (iterator/has-next? it2) (iterator/next! it2) #f))))))

(define (iterator/product proc . its)
  (cond ((null? its)
         #unspecified)
        ((null? (cdr its))
         (iterator/map-1 proc (car its)))
        ((null? (cddr its))
         (iterator/product-2 proc (car its) (cadr its)))
        (else
         (let* ((cache-initialized? #f)
                (cache-len (length its))
                (cache #unspecified)
                (init-cache!
                 (lambda ()
                   [assert (cache-initialized?) (not cache-initialized?)]
                   (set! cache (map iterator/next! its))
                   (set! cache-initialized? #t)))
                (move-cache!
                 (lambda ()
                   [assert (its cache) (= (length its) (length cache))]
                   (set! cache
                         (let loop ((its its)
                                    (cache cache))
                           (if (null? its)
                             '()
                             (let ((it (car its)))
                               (cond
                                ((iterator/has-next? it)
                                 (cons (iterator/next! it) (cdr cache)))
                                (else
                                 (iterator/start! it)
                                 (cons (iterator/next! it) (loop (cdr its) (cdr cache))))))))))))
           (make-iterator (exists iterator/has-next? its)
                          (begin
                            (if (not cache-initialized?)
                              (init-cache!)
                              (move-cache!))
                            (apply proc cache))
                          (begin
                            (set! cache-initialized? #f)
                            (for-each iterator/start! its)))))))

(define (iterator/append it1 it2)
  (make-iterator (or (iterator/has-next? it1) (iterator/has-next? it2))
                 (if (iterator/has-next? it1)
                   (iterator/next! it1)
                   (iterator/next! it2))
                 (begin (iterator/start! it1) (iterator/start! it2))))

(define (iterator/append* it-list)
  [assert (it-list) (not (null? it-list))]
  (fold-left iterator/append (car it-list) (cdr it-list)))

(define (iterator->list it)
  (let ((result (make-queue)))
    (iterator/for-each (lambda (e) (queue/insert! result e)) it)
    (queue->list result)))

;; Return a pair of iterators equivalent to "it"
;; note: You should not use "it", after using split.
;; The generated iterators does not support the "start" command.
(define (iterator/split it)
  (let ((delta1 (make-queue))
        (delta2 (make-queue))
        (gen-iterator (lambda (delta1 delta2)
                        (make-iterator
                         (or (not (queue/empty? delta1))
                             (iterator/has-next? it))
                         (if (queue/empty? delta1)
                           (let ((result (iterator/next! it)))
                             (queue/insert! delta2 result)
                             result)
                           (queue/pop! delta1))
                         (error 'iterator/split "start! command is not supported by this iterator" it)))))
    (values
     (gen-iterator delta1 delta2)
     (gen-iterator delta2 delta1))))

(define (iterator/number-of-elements it)
  (let ((result 0))
    (iterator/for-each (lambda (n) (set! result (+ result 1))) it)
    result))
