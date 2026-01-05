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

(module symbol-set
        (include "utility.sch")
        (import wttree queue)
        (export (make-symbol-set)
                (list->symbol-set list)
                (symbol-set? st)
                (symbol-set/size st)
                (symbol-set/empty? st)
                (symbol-set/member? s st)
                (symbol-set/add st s)
                (symbol-set/add! st s)
                (symbol-set/delete st s)
                (symbol-set/delete! st s)
                (symbol-set/delete-symbols st s-list)
                (symbol-set/for-each proc st)
                (symbol-set/fold proc init st)
                (symbol-set/exists proc st)
                (symbol-set/for-all proc st)
                (symbol-set->list st)
                (map-symbol-set->list proc st)
                (symbol-set/intersection st1 st2)
                (symbol-set/union st1 st2)
                (symbol-set/difference st1 st2)
                (symbol-set/subset? st1 st2)
                (symbol-set/equal? st1 st2)
                (symbol-set/rank st key))
        )

;============================================
; Symbol Set
; This module implements functional symbol sets
; using functional well balanced trees.

(define *symbol-set-tree-type* (make-wt-tree-type symbol<?))

;###
; Build a new symbol set.
(define (make-symbol-set) 
  (make-wt-tree *symbol-set-tree-type*))

(define (symbol-set/size st)
  (wt-tree/size st))

;###
; Build a new symbol set using a list
(define (list->symbol-set l)
  (let ((result (make-symbol-set)))
    (for-each (lambda (e)
                (set! result (symbol-set/add result e)))
              l)
    result))

;###
; Return true if @code{obj} is a symbol set.
(define (symbol-set? obj)
  (and (wt-tree? obj)
       (eq? (wt-tree/type obj) *symbol-set-tree-type*)))

;###
; Return true if the symbol set @code{st} is empty.
; @lisp
; (symbol-set/empty? (make-symbol-set))
;   @result{} #t
; @end lisp
(define (symbol-set/empty? st)
  [assert (st) (symbol-set? st)]
  (wt-tree/empty? st))

;###
; Return true if the symbol set contains @code{s}.
; @lisp
; (define st (make-symbol-set))
; (symbol-set/member? 'foo st)
;   @result{} #f
; @end lisp
(define (symbol-set/member? s st)
  [assert (st) (symbol-set? st)]
  (wt-tree/member? s st))

;###
; Return a new set with a new symbol.
; @lisp
; (define st (make-symbol-set))
; (symbol-set/member? st 'foo)
;   @result{} #f
; (define st2 (symbol-set/add st 'foo))
; (symbol-set/member? 'foo st2)
;   @result{} #t
; (symbol-set/member? 'foo st)
;   @result{} #f
; @end lisp
(define (symbol-set/add st s)
  [assert (st) (symbol-set? st)]
  (wt-tree/add st s #unspecified))

(define (symbol-set/add! st s)
  [assert (st) (symbol-set? st)]
  (wt-tree/add! st s #unspecified)
  st)

;###
; Return a new symbol set that does not contain @code{s}.
(define (symbol-set/delete st s)
  [assert (st) (symbol-set? st)]
  (wt-tree/delete st s))

(define (symbol-set/delete! st s)
  [assert (st) (symbol-set? st)]
  (wt-tree/delete! st s)
  st)

;###
; Return a new symbol set that does not contain any symbol in
; the list @code{s-list}.
(define (symbol-set/delete-symbols st s-list)
  [assert (st) (symbol-set? st)]
  (let ((result st))
    (for-each (lambda (s)
                (set! result (symbol-set/delete result s)))
              s-list)
    result))

;###
; Apply the function @code{proc} to every element in the symbol set.
(define (symbol-set/for-each proc st)
  [assert (st) (symbol-set? st)]
  (wt-tree/for-each (lambda (k v) (proc k)) st))

(define (symbol-set/fold proc init st)
  (wt-tree/fold (lambda (k v curr)
                  (proc k curr))
                init
                st))

(define *symbol-set-exists* (for-each->exists symbol-set/for-each))
(define *symbol-set-for-all* (for-each->for-all symbol-set/for-each))

(define (symbol-set/exists proc st)
  *symbol-set-exists* proc st)

(define (symbol-set/for-all proc st)
  *symbol-set-for-all* proc st)

;###
; Convert a symbol set in a list of symbols.
(define (symbol-set->list st)
  (map-symbol-set->list identity st))

;###
; Convert a symbol set in a list of symbols.
; The function @code{proc} is applied over each element.
(define (map-symbol-set->list proc st)
  [assert (st) (symbol-set? st)]
  (let ((result (make-queue)))
    (symbol-set/for-each (lambda (s)
                           (queue/insert! result (proc s)))
                         st)
    (queue->list result)))

;###
; Create a new symbol set that contains the intersection of
; the given symbol sets.
(define (symbol-set/intersection st1 st2)
  [assert (st1) (symbol-set? st1)]
  [assert (st2) (symbol-set? st2)]
  (wt-tree/intersection st1 st2))

;###
; Create a new symbol set that contains the union of
; the given symbol sets.
(define (symbol-set/union st1 st2)
  [assert (st1) (symbol-set? st1)]
  [assert (st2) (symbol-set? st2)]
  (wt-tree/union st1 st2))

;###
; Create a new symbol set that contains the difference of
; the given symbol sets.
(define (symbol-set/difference st1 st2)
  [assert (st1) (symbol-set? st1)]
  [assert (st2) (symbol-set? st2)]
  (wt-tree/difference st1 st2))
  

;###
; Return true if the set of keys in @code{st1} is a subset
; of @code{st2}.
(define (symbol-set/subset? st1 st2)
  [assert (st1) (symbol-set? st1)]
  [assert (st2) (symbol-set? st2)]
  (wt-tree/subset? st1 st2))

(define (symbol-set/equal? st1 st2)
  (wt-tree/set-equal? st1 st2))

(define (symbol-set/rank st key)
  (wt-tree/rank st key))

;=== End of Symbol Set Chapter

