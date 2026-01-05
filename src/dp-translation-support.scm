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

(module dp-translation-support
        (include "sal.sch")
        (include "fast-hash-table.sch")
        (import queue sal-expression gmp-scheme front-end)
        (export (display-n-ary-infix-op op args)
                <dp-translation-info>
                (dp-translation-info/init! info)
                (make-dp-translation-info)
                (dp-translation-info/fresh-var-id info)
                (dp-translation-info/var-id info decl)
                (dp-translation-info/new-alias-id info)
                (div->mul ast))
        )

(define (display-n-ary-infix-op op args)
  [assert (args) (not (null? args))]
  (display (car args))
  (for-each (lambda (arg)
              (display* " " op " " arg))
            (cdr args)))

(define (div->mul ast)
  (multiple-value-bind
      (arg1 arg2)
      (sal-binary-application/arguments ast)
    (and (instance-of? arg2 <sal-numeral>)
         (make-sal-builtin-application <sal-mul> ast
                                       arg1
                                       (make-sal-numeral (/mpq *mpq-one* (slot-value arg2 :num)) arg2)))))
  
;; ----------------------------------------------
;;
;; Auxiliary functions used in the translation of 
;; SAL formulas to DP (decision procedure) formulas 
;;
;; ----------------------------------------------
(define-class <dp-translation-info> () (:curr-idx :alias-idx :alias-queue :decl->id-mapping :id->decl-mapping :already-visited))

(define (dp-translation-info/init! info)
  (ensure ((info <dp-translation-info>))
    (set-slot-value! info :curr-idx 0)
    (set-slot-value! info :alias-idx 0)
    (set-slot-value! info :alias-queue (make-queue))
    (set-slot-value! info :decl->id-mapping (make-eq-hash-table))
    (set-slot-value! info :id->decl-mapping (make-eq-hash-table))
    (set-slot-value! info :already-visited (make-eq-hash-table))))

(define (make-dp-translation-info) 
  (let ((result (make-instance <dp-translation-info>)))
    (dp-translation-info/init! result)
    result))

(define (dp-translation-info/fresh-var-id info)
  (let ((new-var-id (symbol-append 'x_ (object->symbol (slot-value info :curr-idx)))))
    (set-slot-value! info :curr-idx (+ (slot-value info :curr-idx) 1))
    new-var-id))

(define *show-sal2dp-var-mapping* #f)

(front-end/add-simple-option! 
 "Var Mapping" "--show-var-mapping"
 "Show the mapping from SAL variables to decision procedure variables."
 (lambda ()
   (set! *show-sal2dp-var-mapping* #t)))

(define (dp-translation-info/var-id info decl)
  (let ((decl->id-mapping (slot-value info :decl->id-mapping)))
    (cond
     ((eq-hash-table/get decl->id-mapping decl) =>
      cdr)
     (else
      (let ((new-var-id (dp-translation-info/fresh-var-id info)))
        (when *show-sal2dp-var-mapping*
          (with-output-to-port (current-error-port)
            (lambda ()
              (print "map: " (sal-decl/name decl) " -> " new-var-id))))
        (eq-hash-table/put! decl->id-mapping decl new-var-id)
        (eq-hash-table/put! (slot-value info :id->decl-mapping) new-var-id decl)
        new-var-id)))))

(define (dp-translation-info/new-alias-id info)
  (let ((alias-id (symbol-append 'a_ (object->symbol (slot-value info :alias-idx)))))
    (set-slot-value! info :alias-idx (+ (slot-value info :alias-idx) 1))
    (queue/insert! (slot-value info :alias-queue) alias-id)
    alias-id))
