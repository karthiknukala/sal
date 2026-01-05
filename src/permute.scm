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

(module permute
        (import random)
        (export (permute-vector! vect)
                (permute-list l)
                (permute-vector v)
                (permute obj))
        )

(define (permute-vector! vect)
  (let ((n (vector-length vect)))
    (let loop ((i 0))
      (when (<fx i n)
        (let ((j (+fx (rand (-fx n i)) i)))
          (unless (=fx i j)
            (let ((tmp (vector-ref vect i)))
              (vector-set! vect i (vector-ref vect j))
              (vector-set! vect j tmp))))
        (loop (+ i 1))))))

(define (permute-list l)
  (let ((v (list->vector l)))
    (permute-vector! v)
    (vector->list v)))

(define (permute-vector v)
  (let ((r (copy-vector v (vector-length v))))
    (permute-vector! r)
    r))

(define (permute obj)
  [assert (obj) (or (list? obj) (vector? obj))]
  (cond
   ((list? obj)
    (permute-list obj))
   ((vector? obj)
    (permute-vector obj))))
