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

(module sal-bound
        (include "sal.sch")
        (import sal-expression sal-decls)
        (export (sal-expr/upper-bound expr var-decl)
                (sal-expr/lower-bound expr var-decl))
        )

;------------------------------------
; This module implements heuristics 
; to extract (lower/upper) bound for
; a variable. 
;
; The code is very simple and only
; extract bound for simple cases.
; 
; This functions are used to check
; if a subtype of number is bounded.
;-----------------------------------

;; <bound> models the expression x `op' :value, where `op' may be <, <=, >, >= 
(define-class <bound> () (:value))
(define-class <upper-bound> (<bound>) ())
(define-class <lower-bound> (<bound>) ())
(define-class <le-bound> (<upper-bound>) ())
(define-class <lt-bound> (<upper-bound>) ())
(define-class <ge-bound> (<lower-bound>) ())
(define-class <gt-bound> (<lower-bound>) ())

(define-generic (bound/invert bound))
(define-method (bound/invert (bound <le-bound>))
  (change-class bound <gt-bound>))
(define-method (bound/invert (bound <lt-bound>))
  (change-class bound <ge-bound>))
(define-method (bound/invert (bound <ge-bound>))
  (change-class bound <lt-bound>))
(define-method (bound/invert (bound <gt-bound>))
  (change-class bound <le-bound>))

(define-generic (bound/strict->non-strict bound))
(define-method (bound/strict->non-strict (bound <bound>))
  bound)
;; x < value --->  x <= value - 1
(define-method (bound/strict->non-strict (bound <lt-bound>))
  (let* ((value (slot-value bound :value))
         (place-provider value)
         (new-value (make-sal-builtin-application <sal-sub> place-provider
                                                  value
                                                  (make-sal-numeral 1 place-provider))))
    (make-instance <le-bound> :value new-value)))
;; x > value --> x >= value + 1
(define-method (bound/strict->non-strict (bound <gt-bound>))
  (let* ((value (slot-value bound :value))
         (place-provider value)
         (new-value (make-sal-builtin-application <sal-add> place-provider
                                                  value
                                                  (make-sal-numeral 1 place-provider))))
    (make-instance <ge-bound> :value new-value)))

(define (ref-to? expr var-decl)
  (and (instance-of? expr <sal-name-expr>)
       (eq? (slot-value expr :decl) var-decl)))

(define (mk-bound expr var-decl class inv-class)
  (multiple-value-bind
      (arg1 arg2)
      (sal-binary-application/arguments expr)
    (cond
     ((and (ref-to? arg1 var-decl)
           (not (sal-ast/contains-reference? arg2 var-decl)))
      (make-instance class :value arg2))
     ((and (ref-to? arg2 var-decl)
           (not (sal-ast/contains-reference? arg1 var-decl)))
      (make-instance inv-class :value arg1))
     (else
      #f))))

(define-generic (sal-expr/bound expr var-decl))
(define-method (sal-expr/bound (expr <sal-ast>) (var-decl <sal-decl>))
  #f)
(define-method (sal-expr/bound (expr <sal-le>) (var-decl <sal-decl>))
  (mk-bound expr var-decl <le-bound> <ge-bound>))
(define-method (sal-expr/bound (expr <sal-lt>) (var-decl <sal-decl>))
  (mk-bound expr var-decl <lt-bound> <gt-bound>))
(define-method (sal-expr/bound (expr <sal-ge>) (var-decl <sal-decl>))
  (mk-bound expr var-decl <ge-bound> <le-bound>))
(define-method (sal-expr/bound (expr <sal-gt>) (var-decl <sal-decl>))
  (mk-bound expr var-decl <gt-bound> <lt-bound>))
(define-method (sal-expr/bound (expr <sal-not>) (var-decl <sal-decl>))
  (let ((bound (sal-expr/bound (slot-value expr :arg) var-decl)))
    (and bound (bound/invert bound))))

(define (sal-expr/lower-upper-bound-core expr var-decl class)
  (let ((bound (sal-expr/bound expr var-decl)))
    (and bound 
         (instance-of? bound class)
         (slot-value (bound/strict->non-strict bound) :value))))

(define (sal-expr/upper-bound expr var-decl)
  (sal-expr/lower-upper-bound-core expr var-decl <upper-bound>))

(define (sal-expr/lower-bound expr var-decl)
  (sal-expr/lower-upper-bound-core expr var-decl <lower-bound>))
