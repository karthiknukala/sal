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

(module fast-hash-table
        (include "utility.sch")
        (include "fast-hash-table.macros")
        (export (make-fast-hash-table . initial-size-and-load-factor)
                (fast-hash-table/statistics htable)
                (fast-hash-table/for-each-entry proc htable)
                (fast-hash-table/for-each proc htable)
                (fast-hash-table/for-each-key proc htable)
                (fast-hash-table/fold-keys proc init htable)
                (fast-hash-table/exists-key proc htable)
                (fast-hash-table/for-all-keys proc htable)
                (fast-hash-table/find-key proc htable)
                (inline fast-hash-table/map htable)
                (inline fast-hash-table/capacity htable)
                (inline fast-hash-table/size htable)
                (inline fast-hash-table/load-factor htable)
                (inline fast-hash-table/threshold htable)
                (inline fast-hash-table/mod-count htable)
                (obj/hash obj)
                (make-eq-hash-table . initial-size-and-load-factor)
                (reset-fast-hash-table! htable)
                (eq-hash-table/rehash! a-table)
                (eq-hash-table/put! a-table key value)
                (eq-hash-table/contains? a-table key)
                (eq-hash-table/get a-table key)
                (eq-hash-table/delete! a-table key)
                (eq-hash-table/for-each-entry proc htable)
                (eq-hash-table/for-each proc a-table)
                (eq-hash-table/for-each-key proc a-table)
                (eq-hash-table/fold-keys proc init htable)
                (eq-hash-table/exists-key proc htable)
                (eq-hash-table/for-all-keys proc htable)
                (eq-hash-table/find-key proc htable)
                (eq-hash-table/size a-table)
                (eq-hash-table/statistics a-table)
                (sal/add-memoized-method-cache-table! cache-table)
                (sal/reset-memoized-method-cache-tables!))
        )

(define-inline (fast-hash-table/map htable)
  (vector-ref htable 0))

(define-inline (fast-hash-table/capacity htable)
  (vector-length (fast-hash-table/map htable)))

(define-inline (fast-hash-table/size htable)
  (vector-ref htable 1))

(define-inline (fast-hash-table/load-factor htable)
  (vector-ref htable 2))

(define-inline (fast-hash-table/threshold htable)
  (vector-ref htable 3))

;; The number of times this Hashtable has been structurally modified (rehash)
(define-inline (fast-hash-table/mod-count htable)
  (vector-ref htable 4))

(define *default-initial-size* 17)
(define *default-load-factor* 0.8)

(define (make-fast-hash-table . args)
  (let* ((initial-size (if (null? args) *default-initial-size* (car args)))
         (args (if (null? args) args (cdr args)))
         (load-factor (if (null? args) *default-load-factor* (car args))))
    (vector (make-vector initial-size '()) 0 load-factor (flonum->fixnum (*fl (fixnum->flonum initial-size) load-factor)) 0)))
  
(define (reset-fast-hash-table! htable)
  (vector-set! htable 0 (make-vector (fast-hash-table/capacity htable) '()))
  (vector-set! htable 1 0))

(define (fast-hash-table/statistics htable)
  (let ((mod-count (fast-hash-table/mod-count htable))
        (maximum-list-size 0)
        (hash-conflicts 0)
        (mapping (fast-hash-table/map htable))
        (size (fast-hash-table/size htable)))
    (let loop1 ((i 0))
      (when (< i size)
        (let ((entries (vector-ref mapping i)))
          (unless (null? entries)
            (let ((len (length entries)))
              (if (> len maximum-list-size)
                (set! maximum-list-size len))
              (set! hash-conflicts (+ hash-conflicts (- len 1))))))
        (loop1 (+ i 1))))
    `((size . ,size)
      (mod-count . ,mod-count)
      (hash-conflicts . ,hash-conflicts)
      (maximum-list-size . ,maximum-list-size))))

(define (fast-hash-table/for-each-entry proc htable)
  [sal-assert "hash" (proc htable) (vector? htable)]
  (let* ((capacity (fast-hash-table/capacity htable))
         (mapping (fast-hash-table/map htable)))
    (let loop1 ((i 0))
      (when (< i capacity)
        (let loop2 ((entries (vector-ref mapping i)))
          (unless (null? entries)
            (proc (car entries))
            (loop2 (cdr entries))))
        (loop1 (+ i 1)))))) 

(define (fast-hash-table/for-each proc htable)
  (fast-hash-table/for-each-entry (lambda (entry)
                                    (proc (car entry) (cdr entry)))
                                  htable))

(define (fast-hash-table/for-each-key proc htable)
  (fast-hash-table/for-each-entry (lambda (entry)
                                    (proc (car entry)))
                                  htable))

(define *fold-keys* (for-each->fold fast-hash-table/for-each-key))
(define *exists-key* (for-each->exists fast-hash-table/for-each-key))
(define *for-all-keys* (for-each->for-all fast-hash-table/for-each-key))
(define *find-key* (for-each->find fast-hash-table/for-each-key))

(define (fast-hash-table/fold-keys proc init htable)
  (*fold-keys* proc init htable))

(define (fast-hash-table/exists-key proc htable)
  (*exists-key* proc htable))

(define (fast-hash-table/for-all-keys proc htable)
  (*for-all-keys* proc htable))

(define (fast-hash-table/find-key proc htable)
  (*find-key* proc htable))
    
(define (obj/hash obj)
  (get-hashnumber obj))

(make-fast-hash-table-type eq-hash-table obj/hash eq?)

(define *sal-memoized-method-cache-tables* '())

(define (sal/add-memoized-method-cache-table! cache-table)
  (set! *sal-memoized-method-cache-tables* (cons cache-table *sal-memoized-method-cache-tables*)))

(define (sal/reset-memoized-method-cache-tables!)
  (for-each eq-hash-table/delete-all! *sal-memoized-method-cache-tables*))
  

