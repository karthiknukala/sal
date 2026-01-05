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

(module gmp-scheme
    (extern 
     (include "gmp.h")
     (type mpq (opaque) "mpq_t *")
     (sgmp_init::void () "sgmp_init")
     (smpq_new::mpq (::string) "smpq_new")
     (smpq_new_core::mpq (::string ::int) "smpq_new_core")
     (smpq_add::mpq (::mpq ::mpq) "smpq_add")
     (smpq_sub::mpq (::mpq ::mpq) "smpq_sub")
     (smpq_mul::mpq (::mpq ::mpq) "smpq_mul")
     (smpq_div::mpq (::mpq ::mpq) "smpq_div")
     (smpq_idiv::mpq (::mpq ::mpq) "smpq_idiv")
     (smpq_modulo::mpq (::mpq ::mpq) "smpq_modulo")
     (smpq_cmp::int (::mpq ::mpq) "smpq_cmp")
     (smpq_hash::int (::mpq) "smpq_hash")
     (smpq_get_numerator::mpq (::mpq) "smpq_get_num")
     (smpq_get_denominator::mpq (::mpq) "smpq_get_den")
     (smpq_to_int::int (::mpq) "smpq_to_int")
     (int_to_smpq::mpq (::int) "int_to_smpq")
     (smpq_to_string::string (::mpq) "smpq_to_string"))
    (import utility)
    (include "scmobj.sch")
    (include "api.sch")
    (export (make-mpq astring)
            (make-mpq-integer str base)
            *mpq-zero*
            *mpq-one*
            *mpq-minus-one*
            *mpq-two*
            *mpq-max-int*
            (+mpq arg1 arg2)
            (-mpq arg1 arg2)
            (*mpq arg1 arg2)
            (/mpq arg1 arg2)
            (mpq/exp arg1 arg2)
            (div-mpq arg1 arg2)
            (%mpq arg1 arg2)
            (=mpq arg1 arg2)
            (<mpq arg1 arg2)
            (>mpq arg1 arg2)
            (>=mpq arg1 arg2)
            (<=mpq arg1 arg2)
            (mpq/numerator arg)
            (mpq/denominator arg)
            (mpq/integer? arg)
            (mpq-hash::int arg)
            (mpq->integer::int arg)
            (integer->mpq ::int)
            (object->mpq obj)
            (mpq/absolute arg)
            (mpq/max arg1 arg2)
            (mpq/min arg1 arg2)
            (mpq->string::string arg)
            (mpq/num-bits val))
;;    (eval (export-all)))
)

(begin
  (sgmp_init ) 
  #unspecified)

;=============================================
; GMP interface
; This module implements an interface with the
; GNU Multiprecision Library (GMP).
; These functions manipulate big
; integer/rational numbers.
;

(define *mpq-zero* (make-mpq "0"))
(define *mpq-one* (make-mpq "1"))
(define *mpq-minus-one* (make-mpq "-1"))
(define *mpq-two* (make-mpq "2"))
(define *mpq-max-int* (make-mpq "268435456"))

;###
; Create a new GMP number.
; @lisp
; (make-mpq "10202034049588657868593399332828282")
; (make-mpq "10/30")
; @end lisp
(define-api (make-mpq str)
  :doc "Create a new GMP number."
  :examples '((make-mpq "10202034049588657868593399332828282")
              (make-mpq "10/30"))
  (let ((len (string-length str))
        (arg1 #unspecified)
        (first 0)
        (bar? #f))
    (let loop ((pos 0))
      (when (< pos len)
        (when (eq? (string-ref str pos) #\/)
          (when (or bar? (= pos (- len 1)))
            (error 'make-mpq "Invalid number" str))
          (set! bar? #t)
          (set! arg1 (substring str first pos))
          (set! first (+ pos 1)))
        (loop (+ pos 1))))
    (if bar?
      (let ((arg2 (substring str first len)))
        (/mpq (smpq_new arg1) (smpq_new arg2)))
      (smpq_new str))))

(define-api (make-mpq-integer str base)
  (smpq_new_core str base))

;###
(define (+mpq arg1 arg2)
  (smpq_add arg1 arg2))

;###
(define (-mpq arg1 arg2)
  (smpq_sub arg1 arg2))

;###
(define (*mpq arg1 arg2)
  (smpq_mul arg1 arg2))

;###
(define (/mpq arg1 arg2)
  (smpq_div arg1 arg2))

;###
(define (%mpq arg1 arg2)
  (smpq_modulo arg1 arg2))

;###
(define (div-mpq arg1 arg2)
  (smpq_idiv arg1 arg2))

;###
; Convert a big number in a string.
(define (mpq->string arg)
  (smpq_to_string arg))

;###
(define (=mpq arg1 arg2)
  (= (smpq_cmp arg1 arg2) 0))

;###
(define (<mpq arg1 arg2)
  (< (smpq_cmp arg1 arg2) 0))

;###
(define (>mpq arg1 arg2)
  (> (smpq_cmp arg1 arg2) 0))

;###
(define (<=mpq arg1 arg2)
  (<= (smpq_cmp arg1 arg2) 0))

;###
(define (>=mpq arg1 arg2)
  (>= (smpq_cmp arg1 arg2) 0))

;###
; Compute the hash code of a big number.
(define (mpq-hash arg1)
  (smpq_hash arg1))

;###
; Convert a big number in a fixed precision number.
; If the number is too big, it will be truncated.
; @lisp
; (mpq->integer (make-mpq "100000"))
;   @result{} 100000
; (mpq->integer (make-mpq "10000000000000000"))
;   @result{} -272564224
; @end lisp
(define (mpq->integer arg1)
  (smpq_to_int arg1))

;###
(define (integer->mpq arg1)
  (int_to_smpq arg1))

;###
; Exponential
; @lisp
; (mpq/exp (make-mpq "2") (make-mpq "3"))
;   @result{} 3
; @end lisp
(define (mpq/exp arg1 arg2)
  (let loop ((result *mpq-one*)
             (arg2 arg2))
    (if (=mpq arg2 *mpq-zero*)
      result
      (loop (*mpq result arg1) (-mpq arg2 *mpq-one*)))))

;###
; Convert an object in a big number.
; The accepted objects are: strings, numbers, and big numbers.
(define (object->mpq obj)
  (cond
   ((string? obj)
    (make-mpq obj))
   ((integer? obj)
    (integer->mpq obj))
   ((mpq? obj)
    obj)
   (else
    [assert (obj) #f]
    (internal-error))))

(define (mpq/denominator arg)
  (smpq_get_denominator arg))

(define (mpq/numerator arg)
  (smpq_get_numerator arg))

;###
; Return true if @code{arg} is an integer number.
; @lisp
; (mpq/integer? (make-mpq "102020494034092020202/2"))
;  @result{} #t
; (mpq/integer? (make-mpq "102020494034092020202/2"))
;  @result{} #f
; @end lisp
(define (mpq/integer? arg)
  (=mpq (mpq/denominator arg) *mpq-one*))

(define (mpq/absolute arg)
  [assert (arg) (mpq? arg)]
  (if (<mpq arg *mpq-zero*) (-mpq *mpq-zero* arg) arg))

(define (mpq/max arg1 arg2)
  [assert (arg1) (mpq? arg1)]
  [assert (arg2) (mpq? arg2)]
  (if (<mpq arg1 arg2) arg2 arg1))

(define (mpq/min arg1 arg2)
  [assert (arg1) (mpq? arg1)]
  [assert (arg2) (mpq? arg2)]
  (if (<mpq arg1 arg2) arg1 arg2))

(define (mpq/num-bits val)
  (let ((val (object->mpq val)))
    (let loop ((val (-mpq val *mpq-one*))
               (num-bits 0))
      (if (=mpq val *mpq-zero*)
        num-bits
        (loop (div-mpq val *mpq-two*)
              (+ num-bits 1))))))


;=== End of GMP Chapter

