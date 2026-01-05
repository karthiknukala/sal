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

(module sal-ast-env
        (include "utility.sch")
        (import scmobj symbol-table wttree sal-ast-support)
        (include "fast-hash-table.sch")
        (export (make-empty-env)
                (empty-env? env)
                (lookup-env ast env)
                (update-env env ast1 ast2)
                (update-env* env ast-list1 ast-list2)
                (accumulate-env env ast1 ast2)
                (accumulate-env* env ast-list1 ast-list2))
        )

(define (sal-ast/lt<? ast1 ast2)
  (< (sal-ast/internal-idx ast1)
     (sal-ast/internal-idx ast2)))

(define *env-type* (make-wt-tree-type sal-ast/lt<?))

(define *empty-env* (make-wt-tree *env-type*))

(define (make-empty-env)
  (make-wt-tree *env-type*))

(define (empty-env? env)
  (wt-tree/empty? env))

(define (lookup-env ast env)
  (wt-tree/lookup env ast #f))

(define (update-env env ast1 ast2)
  (if (and (eq? ast1 ast2) (not (lookup-env ast1 env)))
      env
      (wt-tree/add env ast1 ast2)))

(define (update-env* env ast-list1 ast-list2)
  [assert (ast-list1 ast-list2) (= (length ast-list1) (length ast-list2))]
  (let loop ((env env)
             (ast-list1 ast-list1)
             (ast-list2 ast-list2))
    (if (null? ast-list1)
	env
	(loop (update-env env (car ast-list1) (car ast-list2))
	      (cdr ast-list1)
	      (cdr ast-list2)))))

(define (apply-modifications env ast)
  (cond 
   ((lookup-env ast env) =>
    identity)
   (else
    ast)))

(define (accumulate-env env ast1 ast2)
  (update-env env ast1 (apply-modifications env ast2)))

(define (accumulate-env* env ast-list1 ast-list2)
  (let ((new-lst (conservative-map-1 (cut apply-modifications env <>) ast-list2)))
    (update-env* env ast-list1 new-lst)))
