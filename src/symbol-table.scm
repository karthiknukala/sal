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

(module symbol-table
        (include "utility.sch")
        (import wttree queue)
        (export (make-symbol-table)
                (alist->symbol-table alist)
                (symbol-table->alist st)
                (map-symbol-table->list proc st)
                (symbol-table? st)
                (symbol-table/size st)
                (symbol-table/empty? st)
                (symbol-table/member? key st)
                (symbol-table/lookup st key)
                (symbol-table/add st key value)
                (symbol-table/add! st key value)
                (symbol-table/delete st key)
                (symbol-table/delete! st key)
                (symbol-table/delete-keys st key-list)
                (symbol-table/for-each proc st)
                (symbol-table/fold proc init st)
                (symbol-table/pp st)
                (symbol-table/intersection st1 st2)
                (symbol-table/intersection* . st-list)
                (symbol-table/intersection+ st1 st2 proc-merge-values)
                (symbol-table/union st1 st2)
                (symbol-table/union* . st-list)
                (symbol-table/union+ st1 ste proc-merge-values)
                (symbol-table/difference st1 st2)
                (symbol-table/subset? st1 st2)
                (symbol-table/keys st)
                (symbol-table/compare? st1 st2 proc))
        )

;============================================
; Symbol Table
; This module implements functional symbol tables
; using functional well balanced trees.

(define *symbol-tree-type* (make-wt-tree-type symbol<?))

;###
; Build a new symbol table.
(define (make-symbol-table) 
  (make-wt-tree *symbol-tree-type*))

(define (symbol-table/size st)
  (wt-tree/size st))

;###
; Build a new symbol table using an association list
(define (alist->symbol-table alist)
  (alist->wt-tree *symbol-tree-type* alist))

;###
; Convert a symbol table in an association list.
(define (symbol-table->alist st)
  (map-symbol-table->list cons st))

;###
; Convert a symbol table in an association list.
; the function @code{proc} is applied on each entry. 
; @code{proc} receives two parameters: key and value.
(define (map-symbol-table->list proc st)
  (let ((result (make-queue)))
    (symbol-table/for-each (lambda (k v)
                             (queue/insert! result (proc k v)))
                           st)
    (queue->list result)))

;###
; Return true if @code{obj} is a symbol table.
(define (symbol-table? obj)
  (and (wt-tree? obj)
       (eq? (wt-tree/type obj) *symbol-tree-type*)))

;###
; Return true if the symbol table @code{st} is empty.
; @lisp
; (symbol-table/empty? (make-symbol-table))
;   @result{} #t
; @end lisp
(define (symbol-table/empty? st)
  [assert (st) (symbol-table? st)]
  (wt-tree/empty? st))

;###
; Return true if the symbol table contains @code{key}.
; @lisp
; (define st (make-symbol-table))
; (symbol-table/member? 'foo st)
;   @result{} #f
; @end lisp
(define (symbol-table/member? key st)
  [assert (st) (symbol-table? st)]
  (wt-tree/member? key st))

;###
; Return the value associated with @code{key}.
; Return false if there isn't any value associated
; with @code{key}.
(define (symbol-table/lookup st key)
  [assert (st) (symbol-table? st)]
  (wt-tree/lookup st key #f))

;###
; Return a new table with a new entry.
; @lisp
; (define st (make-symbol-table))
; (symbol-table/member? st 'foo)
;   @result{} #f
; (define st2 (symbol-table/add st 'foo 10))
; (symbol-table/member? 'foo st2)
;   @result{} #t
; (symbol-table/member? 'foo st)
;   @result{} #f
; (symbol-table/lookup 'foo st2)
;   @result{} 10
; @end lisp
(define (symbol-table/add st key value)
  [assert (st) (symbol-table? st)]
  (wt-tree/add st key value))

;###
; Insert a new entry in the table.
; Important: this function performs a destructive update.
(define (symbol-table/add! st key value)
  [assert (st) (symbol-table? st)]
  (wt-tree/add! st key value)
  st)

;###
; Return a new symbol table that does not contain @code{key}.
(define (symbol-table/delete st key)
  [assert (st) (symbol-table? st)]
  (wt-tree/delete st key))

(define (symbol-table/delete! st key)
  [assert (st) (symbol-table? st)]
  (wt-tree/delete! st key)
  st)

;###
; Return a new symbol table that does not contain any entry to the symbols in
; the list @code{key-list}.
(define (symbol-table/delete-keys st key-list)
  [assert (st) (symbol-table? st)]
  (let ((result st))
    (for-each (lambda (key)
                (set! result (symbol-table/delete result key)))
              key-list)
    result))

;###
; Apply the function @code{proc} to every element in the symbol table.
; @code{proc} receives two arguments: key and value.
(define (symbol-table/for-each proc st)
  [assert (st) (symbol-table? st)]
  (wt-tree/for-each proc st))

(define (symbol-table/fold proc init st)
  (wt-tree/fold proc init st))

;###
; Pretty prints a symbol table.
; @lisp
; (define t1 (alist->symbol-table '((a . 10) (b . 20) (c . 30)))) 
; (symbol-table/pp st)
;  
; @end lisp 
(define (symbol-table/pp st)
  [assert (st) (symbol-table? st)]
  (let ((first #t))
    (display "[")
    (symbol-table/for-each 
     (lambda (key value)
       (unless first
         (display ", "))
       (display* key " = " value)
       (set! first #f))
     st)
    (display "]")))
  

;###
; Create a new symbol table that contains the intersection of
; the given symbol tables.
; If the symbol tables do not agree on the value of an entry,
; then the value in the table @code{st2} is used. So, this
; function is not commutative.
; @lisp
; (define t1 (alist->symbol-table '((a . 10) (b . 20) (c . 30)))) 
; (define t2 (alist->symbol-table '((b . 200) (d . 30) (c . 1000))))
; (symbol-table/pp (symbol-table/intersection t1 t2))
;   @result{} [b = 200, c = 1000]
; (symbol-table/pp (symbol-table/intersection t2 t1))
;   @result{} [b = 20, c = 30]
; @end lisp
(define (symbol-table/intersection st1 st2)
  [assert (st1) (symbol-table? st1)]
  [assert (st2) (symbol-table? st2)]
  (wt-tree/intersection st1 st2))

(define (symbol-table/intersection* . st-list)
  (fold-left symbol-table/intersection (make-symbol-table) st-list))

(define (symbol-table/intersection+ st1 st2 proc)
  [assert (st1) (symbol-table? st1)]
  [assert (st2) (symbol-table? st2)]
  (wt-tree/intersection+ st1 st2 proc))

;###
; Create a new symbol table that contains the union of
; the given symbol tables.
; If the symbol tables do not agree on the value of an entry,
; then the value in the table @code{st2} is used. So, this
; function is not commutative.
; @lisp
; (define t1 (alist->symbol-table '((a . 10) (b . 20) (c . 30)))) 
; (define t2 (alist->symbol-table '((b . 200) (d . 30) (c . 1000))))
; (symbol-table/pp (symbol-table/union t1 t2))
;   @result{} [b = 200, a = 10, c = 1000, d = 30]
; (symbol-table/pp (symbol-table/union t2 t1))
;   @result{} [b = 20, a = 10, c = 30, d = 30]
; @end lisp
(define (symbol-table/union st1 st2)
  [assert (st1) (symbol-table? st1)]
  [assert (st2) (symbol-table? st2)]
  (wt-tree/union st1 st2))

(define (symbol-table/union* . st-list)
  (fold-left symbol-table/union (make-symbol-table) st-list))

(define (symbol-table/union+ st1 st2 proc)
  [assert (st1) (symbol-table? st1)]
  [assert (st2) (symbol-table? st2)]
  (wt-tree/union+ st1 st2 proc))

;###
; Create a new symbol table that contains the difference of
; the given symbol tables.
; @lisp
; (define t1 (alist->symbol-table '((a . 10) (b . 20) (c . 30)))) 
; (define t2 (alist->symbol-table '((b . 200) (d . 30) (c . 1000))))
; (symbol-table/pp (symbol-table/difference t1 t2))
;   @result{} [a = 10]
; @end lisp
(define (symbol-table/difference st1 st2)
  [assert (st1) (symbol-table? st1)]
  [assert (st2) (symbol-table? st2)]
  (wt-tree/difference st1 st2))
  

;###
; Return true if the set of keys in @code{st1} is a subset
; of @code{st2}.
; @lisp
; (define t1 (alist->symbol-table '((b . 20) (c . 30)))) 
; (define t2 (alist->symbol-table '((b . 200) (d . 30) (c . 1000))))
; (symbol-table/subset? t1 t2)
;   @result{} #t
; (symbol-table/subset? t2 t1)
;   @result{} #f
; @end lisp
(define (symbol-table/subset? st1 st2)
  [assert (st1) (symbol-table? st1)]
  [assert (st2) (symbol-table? st2)]
  (wt-tree/subset? st1 st2))

;###
; Return the set of keys in the symbol table @code{st}.
; The result is a list of symbols.
; @lisp
; (define t1 (alist->symbol-table '((b . 20) (c . 30)))) 
; (symbol-table/keys t1)
;   @result{(b c)}
; @end lisp
(define (symbol-table/keys st)
  [assert (st) (symbol-table? st)]
  (let ((result (make-queue)))
    (symbol-table/for-each (lambda (k v)
                             (queue/insert! result k))
                           st)
    (queue->list result)))

(define (symbol-table/compare? st1 st2 proc)
  (bind-exit (exit)
    (symbol-table/for-each
     (lambda (k v1)
       (cond
        ((symbol-table/lookup st2 k) =>
         (lambda (v2)
           (unless (proc v1 v2)
             (exit #f))))
        (else
         (exit #f))))
     st1)
    (symbol-table/for-each
     (lambda (k v2)
       (unless (symbol-table/lookup st1 k)
         (exit #f)))
     st2)
    #t))

;!-----------------
; Here is an example:
; @lisp
; (define alist1 '((foo . 10) (boo . 20) (tst . 30) 
;                  (aaa . 40) (bbb . 50)))
; (define table1 (alist->symbol-table alist1))
; (define (check-elements st l)
;   (for-each (lambda (pair) 
;               (unless (equal? (symbol-table/lookup st (car pair))
;                               (cdr pair))
;                 (error 'check-elements "Error" #unspecified)))
;             l)
;   #t)
; (check-elements table1 alist1)
;   @result{} #t
; @end lisp


;=== End of Symbol Table Chapter

