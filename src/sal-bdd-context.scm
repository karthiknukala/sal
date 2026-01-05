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

(module sal-bdd-context
        (include "sal.sch")
        (include "fast-hash-table.sch")
        (import bdd)
        (export <sal-bdd-context>
                (sal-bdd-context/curr-var context decl)
                (sal-bdd-context/next-var context decl)
                (sal-bdd-context/add-curr-var! context decl bdd)
                (sal-bdd-context/add-next-var! context decl bdd)
                (sal-bdd-context/manager context)
                (sal-bdd-context/with-local-vars context local-decls proc)
                (make-sal-bdd-context))
        )

(define-class <sal-bdd-context> ()
  (:bdd-manager ;; reference to the BDD mananger
   :curr-vars ;; eq-hash-table: state-var-decl --> BDDs, contains state variables + input variables
   :next-vars ;; eq-hash-table: state-var-decl --> BDDs, contains state variables + next input variables
   :tmp-bdd-var-list ;; list of recycled BDD variables used to encode local variables
   ))

(define-inline (var-at slot-name context decl)
  (cond
   ((eq-hash-table/get (slot-value context slot-name) decl)
    => cdr)
   (else
    #f)))

(define (sal-bdd-context/curr-var context decl)
  (var-at :curr-vars context decl))

(define (sal-bdd-context/next-var context decl)
  (var-at :next-vars context decl))

(define (sal-bdd-context/add-curr-var! context decl bdd)
  (eq-hash-table/put! (slot-value context :curr-vars) decl bdd))

(define (sal-bdd-context/add-next-var! context decl bdd)
  (eq-hash-table/put! (slot-value context :next-vars) decl bdd))

(define (sal-bdd-context/manager context)
  (slot-value context :bdd-manager))

(define (sal-bdd-context/new-aux-var! context)
  (let ((tmp-bdd-var-list (slot-value context :tmp-bdd-var-list)))
    (cond
     ((eq? tmp-bdd-var-list '())
      (bdd/new-var (slot-value context :bdd-manager)))
     (else
      (let ((result (car tmp-bdd-var-list)))
        (set-slot-value! context :tmp-bdd-var-list (cdr tmp-bdd-var-list))
        result)))))

(define (sal-bdd-context/with-local-vars context local-decls proc)
  (let ((aux-bdd-vars (map (lambda (_)  (sal-bdd-context/new-aux-var! context)) local-decls)))
    (unwind-protect 
     (proc aux-bdd-vars)
     (set-slot-value! context :tmp-bdd-var-list (append aux-bdd-vars (slot-value context :tmp-bdd-var-list))))))
                   
(define (make-sal-bdd-context)
  (let ((m (make-bdd-manager)))
    (make-instance <sal-bdd-context>
                   :bdd-manager m
                   :curr-vars (make-eq-hash-table)
                   :next-vars (make-eq-hash-table)
                   :tmp-bdd-var-list '())))

