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

(module sal-esm-runtime
        (include "sal.sch")
        (import gmp-scheme)
        (export (esm-runtime/int-cmp? val1 val2)
                (esm-runtime/mpq-cmp? val1 val2)
                (esm-runtime/bool-cmp? val1 val2)
                (esm-runtime/ring-succ-idx idx size)
                (esm-runtime/scalar-set-cmp? constraint val1 val2)
                (esm-runtime/ring-set-cmp? constraint val1 val2)
                (scalar-set-constraint/reset! constraint)
                (ring-set-constraint/reset! constraint)
                (scalar-set-constraint/fill! constraint)
                (ring-set-constraint/fill! constraint)
                (scalar-set-constraint/normalized-value constraint val)
                (ring-set-constraint/normalized-value constraint val)
                (scalar-set-constraint/normalize! constraint val)
                (ring-set-constraint/normalize! constraint val)
                )
        )


(define (esm-runtime/int-cmp? val1 val2)
  (cond
   ((<fx val1 val2) -1)
   ((=fx val1 val2)  0)
   (else             1)))

(define (esm-runtime/mpq-cmp? val1 val2)
  (cond
   ((<mpq val1 val2) -1)
   ((=mpq val1 val2)  0)
   (else              1)))

(define (esm-runtime/bool-cmp? val1 val2)
  (cond 
   ((and (not val1) val2) -1)
   ((eq? val1 val2)        0)
   (else                   1)))

(define (esm-runtime/ring-succ-idx idx size)
  (let ((idx (+ idx 1)))
    (if (= idx size) 0 idx)))
      

;; ----------------------------------------------------
;;
;; Symmetry reduction runtime constraints
;;
;; The runtime constraints are used to 'canonize'
;; symmetric values at runtime.
;; 
;; ----------------------------------------------------

(define-record-type scalar-set-constraint
  (mk-scalar-set-constraint next-available-value permutation inv-permutation)
  scalar-set-constraint?
  (next-available-value scalar-set-constraint/next-available-value scalar-set-constraint/set-next-available-value!)
  (permutation scalar-set-constraint/permutation)
  (inv-permutation scalar-set-constraint/inv-permutation))

(define-record-type ring-set-constraint
  (mk-ring-set-constraint delta size)
  ring-set-constraint?
  (delta ring-set-constraint/delta ring-set-constraint/set-delta!)
  (size ring-set-constraint/size))

(define (scalar-set-constraint/reset! constraint)
  (scalar-set-constraint/set-next-available-value! constraint 0)
  (vector-fill! (scalar-set-constraint/permutation constraint) 'undef)
  (vector-fill! (scalar-set-constraint/inv-permutation constraint) 'undef))

(define (ring-set-constraint/reset! constraint)
  (ring-set-constraint/set-delta! constraint 'undef))

;; After executing a transition, we may not have a complete permutation.
;; This function completes the permutation associated with a Scalar Set type.
(define (scalar-set-constraint/fill! constraint)
  (let* ((permutation-vect (scalar-set-constraint/permutation constraint))
         (inv-permutation-vect (scalar-set-constraint/inv-permutation constraint))
         (permutation-vect-size (vector-length permutation-vect))
         (next-available-idx (scalar-set-constraint/next-available-value constraint)))
    (when (< next-available-idx permutation-vect-size)
      (let loop ((j 0))
        (when (< j permutation-vect-size)
          (when (eq? (vector-ref permutation-vect j) 'undef)
            (vector-set! permutation-vect j next-available-idx)
            (vector-set! inv-permutation-vect next-available-idx j)
            (set! next-available-idx (+ next-available-idx 1)))
          (loop (+ j 1)))))
    [assert (next-available-idx permutation-vect-size) (= next-available-idx permutation-vect-size)]
    (scalar-set-constraint/set-next-available-value! constraint permutation-vect-size)))

;; After executing a transition, we may not have a found a rotation for a Ring Set type.
;; This function sets the default rotation: 0.
(define (ring-set-constraint/fill! constraint)
  (when (eq? (ring-set-constraint/delta constraint) 'undef)
    (ring-set-constraint/set-delta! constraint 0)))

(define-inline (rotation-normal val delta size)
  (if (>= val delta)
    (- val delta)
    (- size (- delta val))))

;; Return the normalized ('canonical') value associated with the give value.
;; Remark: may return 'undef when the permutation is not defined for the given value.
(define (scalar-set-constraint/normalized-value constraint val)
  (let* ((permutation (scalar-set-constraint/permutation constraint)))
    (vector-ref permutation val)))

;; Return the normalized ('canonical') value associated with the give value.
;; Remark: may return 'undef when the rotation is not defined.
(define (ring-set-constraint/normalized-value constraint val)
  (let ((delta (ring-set-constraint/delta constraint)))
    (if (eq? delta 'undef)
      'undef
      (rotation-normal val delta (ring-set-constraint/size constraint)))))

;; Return a normalized value, if the permutation is not defined for the given value,
;; then define the permutation for the given value.
(define (scalar-set-constraint/normalize! constraint val)
  (let* ((permutation (scalar-set-constraint/permutation constraint))
         (normalized-value (vector-ref permutation val)))
        (cond
         ((eq? normalized-value 'undef)
          (let ((inv-permutation (scalar-set-constraint/inv-permutation constraint))
                (next-available-value (scalar-set-constraint/next-available-value constraint)))
            (vector-set! permutation val next-available-value)
            (vector-set! inv-permutation next-available-value val)
            (scalar-set-constraint/set-next-available-value! constraint (+ next-available-value 1))
            next-available-value))
         (else
          normalized-value))))

;; Return a normalized value, if the rotation is not defined, then set it equal
;; to the given value, that is, the given value will be mapped to 0.
(define (ring-set-constraint/normalize! constraint val)
  (let ((delta (ring-set-constraint/delta constraint)))
    (cond 
     ((eq? delta 'undef)
      (ring-set-constraint/set-delta! constraint val)
      0)
     (else  
      ;; ROTATE RINGSET ELEMENT
      (rotation-normal val delta (ring-set-constraint/size constraint))))))

(define-inline (symmetric-cmp? val1 val2 norm-val1 norm-val2)
    (cond 
     ((and (number? norm-val1) (number? norm-val2))
      (esm-runtime/int-cmp? val1 val2))   ;; PRECISE
     ((number? norm-val1)
      [assert (norm-val2) (eq? norm-val2 'undef)]
      -1)                     ;; PRECISE
     ((number? norm-val2)
      [assert (norm-val1) (eq? norm-val1 'undef)]
      1)                      ;; PRECISE
     (else
      0)))                    ;; APPROXIMATION

(define (esm-runtime/scalar-set-cmp? constraint val1 val2)
  (let ((norm-val1 (scalar-set-constraint/normalized-value constraint val1))
        (norm-val2 (scalar-set-constraint/normalized-value constraint val2)))
    (symmetric-cmp? val1 val2 norm-val1 norm-val2)))

(define (esm-runtime/ring-set-cmp? constraint val1 val2)
  (let ((norm-val1 (ring-set-constraint/normalized-value constraint val1))
        (norm-val2 (ring-set-constraint/normalized-value constraint val2)))
    (symmetric-cmp? val1 val2 norm-val1 norm-val2)))
