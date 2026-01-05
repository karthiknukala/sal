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

(module sal-esm-util
        (include "sal.sch")
        (import gmp-scheme sal2scm-core sal-ast-env sal2scm-runtime state-entry-channel)
        (export (sal-bounded-subtype/num-bits-and-lower type ctx)
                (mk-esm-datatype-layout tag-num-bits constructor-vect)
                (esm-datatype-layout? obj)
                (esm-datatype-layout/tag-num-bits layout)
                (esm-datatype-layout/constructor-vect layout)
                (esm-datatype-value->channel! value layout channel proc-vect)
                (mk-esm-atom-layout num-bits lower)
                esm-atom-layout?
                (esm-atom-layout/num-bits layout)
                (esm-atom-layout/lower layout)
                (esm-atom-value->channel! value layout channel)
                (channel->esm-atom-value! channel layout)
                (mk-esm-mpq-layout num-bits lower)
                (esm-mpq-layout? obj)
                (esm-mpq-layout/num-bits layout) 
                (esm-mpq-layout/lower layout)
                (esm-mpq-value->channel! value layout channel)
                (channel->esm-mpq-value! channel layout)
                (mk-esm-ref-layout idx recursive?)
                (esm-ref-layout? obj)
                (esm-ref-layout/definition-idx layout)
                (esm-ref-layout/recursive? layout)
                (sal-data-type/tag-num-bits type)
                (sal-scalar-type/num-bits type)
                ) 
        )

(define *esm-num-lower-limit* (make-mpq "-524288"))
(define *esm-num-upper-limit* (make-mpq "524287"))

(define (calculate-bound bound sal-scm-ctx)
  (try
   (object->mpq (eval (sal->scm bound sal-scm-ctx (make-empty-env))))
   (lambda (escape p m o)
     (escape #f))))

(define (sal-esm/map-to-bits? lower upper)
  (or (not lower) (not upper) (<mpq lower *esm-num-lower-limit*) (>mpq upper *esm-num-upper-limit*)))

(define (sal-bounded-subtype/num-bits-and-lower type ctx)
  (let ((lower (calculate-bound (slot-value type :lower) ctx))
        (upper (calculate-bound (slot-value type :upper) ctx)))
    (if (sal-esm/map-to-bits? lower upper)
      (values 'infinite #unspecified)
      (values (mpq/num-bits (+mpq (-mpq upper lower) *mpq-one*)) 
              (mpq->integer lower)))))

(define-record-type esm-datatype-layout
  (mk-esm-datatype-layout tag-num-bits constructor-vect)
  esm-datatype-layout?
  (tag-num-bits esm-datatype-layout/tag-num-bits)
  (constructor-vect esm-datatype-layout/constructor-vect))

(define (esm-datatype-value->channel! value layout channel proc-value)
  (cond
   ((eq? value 'not-assigned)
    ;; chocie variables may be unassigned... using tag-idx = 0
    (sec/add-num! channel 0 (esm-datatype-layout/tag-num-bits layout))
    (let* ((new-layout (vector-ref (esm-datatype-layout/constructor-vect layout) 0)))
      [assert (new-layout) (list? new-layout)]
      (for-each (lambda (child-layout)
                  (proc-value 'not-assigned child-layout))
                new-layout)))
   (else
    (let* ((tag-idx (car value))
           (new-layout (vector-ref (esm-datatype-layout/constructor-vect layout) tag-idx))
           (new-value (cdr value)))
      (sec/add-num! channel tag-idx (esm-datatype-layout/tag-num-bits layout))
      [assert (new-layout new-value) (and (list? new-layout) (list? new-value)
                                          (= (length new-layout) (length new-value)))]
      (for-each proc-value new-value new-layout)))))

(define-record-type esm-atom-layout
  (mk-esm-atom-layout num-bits lower)
  esm-atom-layout?
  (num-bits esm-atom-layout/num-bits)
  (lower esm-atom-layout/lower))

(define (esm-atom-value->channel! value layout channel)
  [assert (value) (or (number? value) (eq? value 'not-assigned))]
  (let ((lower (esm-atom-layout/lower layout))
        (num-bits (esm-atom-layout/num-bits layout)))
    (if (number? value)
      (sec/add-num! channel (- value lower) num-bits)
      (sec/add-num! channel 0 num-bits)))) ;; choice variables may be unassigned

(define (channel->esm-atom-value! channel layout)
  (let* ((lower (esm-atom-layout/lower layout))
         (num-bits (esm-atom-layout/num-bits layout)))
    (+fx (sec/read-num! channel num-bits) lower)))

(define-record-type esm-mpq-layout
  (mk-esm-mpq-layout num-bits lower)
  esm-mpq-layout?
  (num-bits esm-mpq-layout/num-bits) 
  (lower esm-mpq-layout/lower)) 

(define (esm-mpq-value->channel! value layout channel)
  [assert (value) (or (mpq? value) (eq? value 'not-assigned))]
  (let ((lower (esm-mpq-layout/lower layout))
        (num-bits (esm-mpq-layout/num-bits layout)))
    (if (mpq? value)
      (sec/add-num! channel (- (mpq->integer value) lower) num-bits)
      (sec/add-num! channel 0 num-bits)))) ;; choice variables may be unassigned

(define (channel->esm-mpq-value! channel layout)
  (let ((lower (esm-mpq-layout/lower layout))
        (num-bits (esm-mpq-layout/num-bits layout)))
    (integer->mpq (+ (sec/read-num! channel num-bits) lower))))

(define-record-type esm-ref-layout
  (mk-esm-ref-layout idx recursive?)
  esm-ref-layout?
  (idx esm-ref-layout/definition-idx)
  (recursive? esm-ref-layout/recursive?))

(define (sal-data-type/tag-num-bits type)
  (mpq/num-bits (length (slot-value type :constructors))))

(define (sal-scalar-type/num-bits type)
  (mpq/num-bits (integer->mpq (length (slot-value type :scalar-elements)))))

