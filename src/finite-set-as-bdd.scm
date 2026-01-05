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

(module finite-set-as-bdd
        (include "utility.sch")
        (import bdd gmp-scheme)
        (export (make-finite-set-manager universe-size elem->nat nat->elem . svsv)
                (make-empty-finite-set manager)
                (make-universe-finite-set manager)
                (make-singleton-set manager elem)
                (finite-set/union set1 set2)
                (finite-set/intersection set1 set2)
                (finite-set/difference set1 set2)
                (finite-set/complement set)
                (finite-set/eq? set1 set2)
                (finite-set/empty? set)
                (finite-set/universe? set)
                (finite-set/insert set elem)
                (finite-set/delete set elem)
                (finite-set/contains? set elem)
                (finite-set/size set)
                (finite-set/for-each proc set)
                (list->finite-set manager lst))
        )

(define-record-type finite-set-manager
  (mk-finite-set-manager bdd-manager universe-size universe-bdd elem->nat nat->elem)
  finite-set-manager?
  (bdd-manager finite-set-manager/bdd-manager)
  (universe-size finite-set-manager/universe-size)
  (universe-bdd finite-set-manager/universe-bdd)
  (elem->nat finite-set-manager/elem->nat)
  (nat->elem finite-set-manager/nat->elem))

(define-record-type finite-set
  (mk-finite-set set-manager bdd)
  finite-set?
  (set-manager finite-set/manager)
  (bdd finite-set/bdd))

(define (universe-bdd bdd-manager universe-size)
  (let ((max (-mpq universe-size *mpq-one*))
        (num-vars (bdd/num-vars bdd-manager))
        (lt-bit (lambda (bit1 bit2)
                  (bdd/and (bdd/not bit1) bit2)))
        (le-bit (lambda (bit1 bit2)
                  (bdd/or (bdd/not bit1) bit2))))
    (let loop ((var-idx 0)
               (max max)
               (result #f))
      (if (< var-idx num-vars)
        (let* ((curr-bit (if (=mpq (%mpq max *mpq-two*) *mpq-zero*) 
                           (bdd/false bdd-manager)
                           (bdd/true bdd-manager)))
               (curr-var (bdd/ith-var bdd-manager var-idx))
               (new-result (if result 
                             (bdd/or (lt-bit curr-var curr-bit)
                                     (bdd/and (le-bit curr-var curr-bit)
                                              result))
                             (le-bit curr-var curr-bit))))
          (loop (+ var-idx 1)
                (div-mpq max *mpq-two*)
                new-result))
        result))))
        
(define (make-finite-set-manager universe-size elem->nat nat->elem . svsv)
  [assert (universe-size) (>mpq (object->mpq universe-size) *mpq-zero*)]
  (let* ((dynamic-reordering? (svsv/value svsv :dynamic-reordering? #t))
         (reorder-strategy (svsv/value svsv :reorder-strategy 'bdd-reorder-sift))
         (bdd-manager (make-bdd-manager))
         (size (object->mpq universe-size))
         (num-bits (mpq/num-bits size)))
    (when dynamic-reordering? 
      (bdd/enable-dynamic-reordering! bdd-manager reorder-strategy))
    (let loop ((i 0))
      (when (< i num-bits)
        (bdd/new-var bdd-manager)
        (loop (+ i 1))))
    (mk-finite-set-manager bdd-manager size (universe-bdd bdd-manager size) elem->nat nat->elem)))

(define (make-empty-finite-set manager)
  (mk-finite-set manager (bdd/false (finite-set-manager/bdd-manager manager))))

(define (make-universe-finite-set manager)
  (mk-finite-set manager (finite-set-manager/universe-bdd manager)))

(define-macro (gen-set-op op-name bdd-op)
  `(define (,op-name set1 set2)
     [assert (set1 set2) (eq? (finite-set/manager set1) (finite-set/manager set2))]
     (let* ((manager (finite-set/manager set1)))
       (mk-finite-set manager (,bdd-op (finite-set/bdd set1) (finite-set/bdd set2))))))

(gen-set-op finite-set/union bdd/or)
(gen-set-op finite-set/intersection bdd/and)
(gen-set-op finite-set/difference bdd/diff)

(define (finite-set/complement set)
  (let ((manager (finite-set/manager set)))
    (mk-finite-set manager (bdd/and (bdd/not (finite-set/bdd set))
                                    (finite-set-manager/universe-bdd manager)))))

(define (finite-set/eq? set1 set2)
  [assert (set1 set2) (eq? (finite-set/manager set1) (finite-set/manager set2))]
  (bdd/eq? (finite-set/bdd set1) (finite-set/bdd set2)))

(define (finite-set/empty? set)
  (bdd/false? (finite-set/bdd set)))

(define (finite-set/universe? set)
  (bdd/eq? (finite-set-manager/universe-bdd (finite-set/manager set)) (finite-set/bdd set)))
  
(define (elem->bdd manager elem)
  (let* ((elem->nat (finite-set-manager/elem->nat manager))
         (bdd-manager (finite-set-manager/bdd-manager manager))
         (num-vars (bdd/num-vars bdd-manager))
         (value (object->mpq (elem->nat elem))))
    [assert (value) (>=mpq value *mpq-zero*)]
    [assert (value manager) (<mpq value (finite-set-manager/universe-size manager))]
    (let loop ((value value)
               (var-idx 0)
               (result (bdd/true bdd-manager)))
      (if (< var-idx num-vars)
        (let* ((on? (=mpq (%mpq value *mpq-two*) *mpq-one*))
               (bdd-var (bdd/ith-var bdd-manager var-idx))
               (result (bdd/and result
                                (if on? bdd-var (bdd/not bdd-var)))))
          (loop (div-mpq value *mpq-two*)
                (+ var-idx 1)
                result))
        result))))

(define (minterm->elem manager minterm)
  (let* ((nat->elem (finite-set-manager/nat->elem manager))
         (bdd-manager (finite-set-manager/bdd-manager manager))
         (num-vars (bdd/num-vars bdd-manager)))
    (let loop ((var-idx (- num-vars 1))
               (result *mpq-zero*))
      (if (>= var-idx 0)
        (let* ((curr-bit (bdd/le? minterm (bdd/ith-var bdd-manager var-idx)))
               (new-result (if curr-bit
                             (+mpq (*mpq result *mpq-two*) *mpq-one*)
                             (*mpq result *mpq-two*))))
          (loop (- var-idx 1)
                new-result))
        result))))

(define (finite-set/insert set elem)
  (let* ((manager (finite-set/manager set))
         (elem-bdd (elem->bdd manager elem)))
    (mk-finite-set manager (bdd/or (finite-set/bdd set) elem-bdd))))

(define (make-singleton-set manager elem)
  (finite-set/insert (make-empty-finite-set manager) elem))

(define (finite-set/delete set elem)
  (let* ((manager (finite-set/manager set))
         (elem-bdd (elem->bdd manager elem)))
    (mk-finite-set manager (bdd/and (finite-set/bdd set) (bdd/not elem-bdd)))))

(define (finite-set/contains? set elem)
  (let* ((manager (finite-set/manager set))
         (elem-bdd (elem->bdd manager elem)))
    (bdd/le? elem-bdd (finite-set/bdd set))))

(define (finite-set/size set)
  (let* ((manager (finite-set/manager set))
         (bdd-manager (finite-set-manager/bdd-manager manager)))
    (bdd/num-solutions (finite-set/bdd set) (bdd/num-vars bdd-manager))))
         
(define (finite-set/for-each proc set)
  ;; Inefficient implementation... I should use for-each minterm
  (let* ((manager (finite-set/manager set))
         (size (finite-set-manager/universe-size manager))
         (nat->elem (finite-set-manager/nat->elem manager))
         (bdd-set (finite-set/bdd set)))
    (let loop ((i *mpq-zero*))
      (when (<mpq i size)
        (let* ((elem (nat->elem i))
               (elem-bdd (elem->bdd manager elem)))
          (when (bdd/le? elem-bdd bdd-set)
            (proc elem))
          (loop (+mpq i *mpq-one*)))))))
  
(define (list->finite-set manager lst)
  (fold-left (lambda (set elem)
               (finite-set/insert set elem))
             (make-empty-finite-set manager)
             lst))
    
    
