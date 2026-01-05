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

(module code-table
        (include "fast-hash-table.sch")
        (import utility)
        (export (code/hash code)
                (make-code-table . initial-size-and-load-factor)
                (code-table/rehash! ht)
                (code-table/put! ht k v)
                (code-table/contains? ht k)
                (code-table/get ht k)
                (code-table/delete! ht k) 
                (code-table/delete-all! ht)
                (code-table/size ht)
                (code-table/for-each proc ht)
                (code-table/for-each-key proc htable)
                (code-table/fold-keys proc init htable)
                (code-table/exists-key proc htable)
                (code-table/for-all-keys proc htable)
                (code-table/find-key proc htable))
        )

(define (code/hash code)
  (cond
   ((list? code)
    (fold-left (lambda (r curr)
                 (+ (* r 3) (code/hash curr)))
               0 code))
   ((vector? code)
    (vector/fold (lambda (r curr)
                   (+ (* r 3) (code/hash curr)))
                 0 code))
   (else
    (obj/hash code))))

(make-fast-hash-table-type code-table code/hash equal?)


