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

(module sal-lhs-set
        (include "sal.sch")
        (import wttree sal-lhs-subsumption
                sal-expression sal-ast-eq sal-type
                iterators)
        (export (make-sal-lhs-set)
                (sal-lhs-set/empty? set)
                (sal-lhs-set/add set lhs)
                (sal-lhs-set/contains? set lhs)
                (sal-lhs-set/remove set lhs)
                (sal-lhs-set/union set1 set2)
                (sal-lhs-set/subset? set1 set2)
                (sal-lhs-set/difference set1 set2)
                (sal-lhs-set/intersection set1 set2)
                (sal-lhs-set->list set))
        )

;; ---------------------------------------
;; A sal-lhs-set is used to represent set of
;; ground lhs ASTs.
;;
;; ---------------------------------------
(define (sal-decl/lt<? decl1 decl2)
  (< (sal-ast/internal-idx decl1)
     (sal-ast/internal-idx decl2)))
  
(define *env-type* (make-wt-tree-type sal-decl/lt<?))

(define (make-sal-lhs-set)
  (make-wt-tree *env-type*))

(define (sal-lhs-set/empty? set)
  (wt-tree/empty? set))

;; Add a lhs to the set.
;; Return a new set, and lhs (or a target of lhs).
;; A target is returned when the lhs is merged with other elements.
;; For instance, suppose the set contains the elements t.1 t.2,
;; and t is a tuple with 3 components. Now, we add t.3. Then, the
;; elements t.1 and t.2 are removed from the set, t is added, and
;; t is returned.
;; Remark: returns (set and #f) when lhs was not added to the set
(define (sal-lhs-set/add set lhs)
  (let ((lhs-decl (slot-value (sal-lhs/name-expr lhs) :decl)))
    (cond
     ((wt-tree/lookup set lhs-decl #f) =>
      (lambda (curr-lhs-list)
        (multiple-value-bind
            (new-lhs-list result-lhs)
            (lhs-list/add curr-lhs-list lhs)
          (if result-lhs
            (values (wt-tree/add set lhs-decl new-lhs-list) result-lhs)
            (values set #f)))))
     (else
      (values (wt-tree/add set lhs-decl (list lhs)) lhs)))))

(define (lhs-list/contains? lhs-list lhs)
  ;; there is an element in lhs-list that subsumes lhs
  (exists (cut sal-lhs/subsumes? <> lhs) lhs-list))

(define (lhs-list/add lhs-list lhs)
  [sal-assert "lhs-list/add" (lhs) (instance-of? lhs <sal-expr>)]
  (if (not (lhs-list/contains? lhs-list lhs))
    ;; remove from curr-lhs-list the elements subsumed by lhs
    (let* ((tmp-lhs-list1 (filter (lambda (curr-lhs)
                                    (not (sal-lhs/subsumes? lhs curr-lhs)))
                                  lhs-list))
           (tmp-lhs-list2 (cons lhs tmp-lhs-list1)))
      (merge-common-lhs tmp-lhs-list2 lhs))
    (values lhs-list #f)))

(define (merge-common-lhs lhs-list new-lhs)
  (multiple-value-bind
      (new-lhs-list result-lhs)
      (merge-common-lhs-core lhs-list new-lhs)
    (if (eq? result-lhs new-lhs)
      (values new-lhs-list result-lhs)
      (merge-common-lhs new-lhs-list result-lhs))))

;; Merge common lhs elements of lhs-list.
;; For instance if lhs-list contains (t.1 t.2 t.3) and t
;; is a tuple with three elements, them t.1 t.2 t.3 are replaced
;; by t.
;; The result is a new list and the updated new-lhs.
(define-generic (merge-common-lhs-core lhs-list new-lhs))

(define-method (merge-common-lhs-core (lhs-list <primitive>) (new-lhs <sal-name-expr>))
  (values lhs-list new-lhs))

(define-method (merge-common-lhs-core (lhs-list <primitive>) (new-lhs <sal-next-operator>))
  (values lhs-list new-lhs))

(define-method (merge-common-lhs-core (lhs-list <primitive>) (new-lhs <sal-selection>))
  (let* ((target (sal-selection/target new-lhs))
         (target-type (sal-expr/type target))
         (num-targets-in-lhs-list (num-common-target lhs-list target))
         (target-type-size (sal-lhs-target-type/num-components target-type)))
    [assert (lhs-list new-lhs target target-type target-type-size)
            (<= num-targets-in-lhs-list target-type-size)]
    (if (= target-type-size num-targets-in-lhs-list)
      (values (cons target (remove-common-target lhs-list target)) target)
      (values lhs-list new-lhs))))

(define (num-common-target lhs-list new-target)
  (let loop ((i 0)
             (lhs-list lhs-list))
    (if (null? lhs-list)
      i
      (let ((curr-lhs (car lhs-list)))
        (if (and (instance-of? curr-lhs <sal-selection>)
                 (sal-ast/equivalent? (sal-selection/target curr-lhs) new-target))
          (loop (+ i 1) (cdr lhs-list))
          (loop i (cdr lhs-list)))))))

(define (remove-common-target lhs-list new-target)
  (filter (lambda (curr-lhs)
            (or (not (instance-of? curr-lhs <sal-selection>))
                (not (sal-ast/equivalent? (sal-selection/target curr-lhs) new-target))))
          lhs-list))

(define-generic (sal-lhs-target-type/num-components type))

(define-method (sal-lhs-target-type/num-components (type <sal-type-name>))  
  (sal-lhs-target-type/num-components (sal-type-name/definition type)))

(define-method (sal-lhs-target-type/num-components (type <sal-subtype>))
  (sal-lhs-target-type/num-components (sal-subtype/immediate-super-type type)))

(define-method (sal-lhs-target-type/num-components (type <sal-tuple-type>))
  (length (slot-value type :types)))

(define-method (sal-lhs-target-type/num-components (type <sal-record-type>))
  (length (slot-value type :fields)))

(define-method (sal-lhs-target-type/num-components (type <sal-function-type>))
  (let ((domain (slot-value type :domain)))
    (sal-type/number-of-elements-as-integer domain)))

;; return true if the set contains lhs or an element that subsumes lhs
(define (sal-lhs-set/contains? set lhs)
  (let ((lhs-decl (slot-value (sal-lhs/name-expr lhs) :decl)))
    (cond
     ((wt-tree/lookup set lhs-decl #f) =>
      (lambda (curr-lhs-list)
        (lhs-list/contains? curr-lhs-list lhs)))
     (else
      #f))))

;; Removes lhs and any component subsumed by lhs.
;; A lhs in the set can be broken in pieces if lhs is part of it.
;; Example: set contains 't', and lhs is 't.1'
(define (sal-lhs-set/remove set lhs)
  (let ((lhs-decl (slot-value (sal-lhs/name-expr lhs) :decl)))
    (cond
     ((wt-tree/lookup set lhs-decl #f) =>
      (lambda (curr-lhs-list)
        (let ((new-lhs-list (lhs-list/remove curr-lhs-list lhs)))
          (if (null? new-lhs-list)
            (wt-tree/delete set lhs-decl)
            (wt-tree/add set lhs-decl new-lhs-list)))))
     (else
      set))))

(define (lhs-list/remove curr-lhs-list lhs)
  (let ((subsumed? #f) ;; lhs subsumed an element of curr-lhs-list
        (subsumed-by #f) ;; lhs was subsumed by an element of curr-lhs-list
        (tmp-lhs-list '()))
    (for-each (lambda (curr-lhs)
                [assert (curr-lhs lhs subsumed? subsumed-by)
                        ;; if lhs was subsumed by an element of curr-lhs-list, then
                        ;; it cannot subsume another element of curr-lhs-list.
                        (imply subsumed-by (not (sal-lhs/subsumes? lhs curr-lhs)))]
                [assert (curr-lhs lhs subsumed? subsumed-by)
                        ;; if lhs subsumed an element of curr-lhs-list, then
                        ;; it cannot be subsumed by another element of curr-lhs-list.
                        (imply subsumed? (not (sal-lhs/subsumes? curr-lhs lhs)))]
                (if (and (not subsumed-by) (sal-lhs/subsumes? lhs curr-lhs))
                  (set! subsumed? #t)
                  (cond 
                   ((and (not subsumed?) (sal-lhs/subsumes? curr-lhs lhs))
                    ;; lhs can only be subsumed once.
                    [assert (subsumed-by) (not subsumed-by)]
                    ;; the subsumed-by element will be broken in pieces...
                    (set! subsumed-by curr-lhs))
                   (else
                    (push! curr-lhs tmp-lhs-list)))))
              curr-lhs-list)
    [assert (subsumed? subsumed-by) (not (and subsumed-by subsumed?))]
    (when subsumed-by
      ;; I have to break the element subsumed-by in pieces, and remove
      ;; the one associated with lhs
      [assert (subsumed-by lhs) (sal-lhs/subsumes? subsumed-by lhs)]
      (let loop ((subsumed-by subsumed-by))
        (unless (sal-ast/equivalent? subsumed-by lhs)
          (let* ((subsumed-by-type (sal-expr/type subsumed-by))
                 (type-components (sal-lhs-type/components subsumed-by-type subsumed-by))
                 (new-subsumed-by #f))
            (for-each (lambda (component)
                        [assert (new-subsumed-by component lhs)
                                ;; only one component can subsume lhs
                                (imply new-subsumed-by (not (sal-lhs/subsumes? component lhs)))]
                        (cond
                         ((and (not new-subsumed-by) (sal-lhs/subsumes? component lhs))
                          (set! new-subsumed-by component))
                         (else
                          (push! component tmp-lhs-list))))
                      type-components)
            ;; one of the components must subsume lhs
            [assert (new-subsumed-by) new-subsumed-by]
            (loop new-subsumed-by)))))
    tmp-lhs-list))
  

(define-generic (sal-lhs-type/components lhs-type lhs))

(define-method (sal-lhs-type/components (lhs-type <sal-type-name>) (lhs <sal-expr>))
  (sal-lhs-type/components (sal-type-name/definition lhs-type) lhs))

(define-method (sal-lhs-type/components (lhs-type <sal-tuple-type>) (lhs <sal-expr>))
  (let ((n (length (slot-value lhs-type :types))))
    (let loop ((i 1)
               (result '()))
      (if (<= i n)
        (let ((selection (make-ast-instance <sal-tuple-selection> lhs
                                            :target lhs
                                            :idx (make-sal-numeral i lhs))))
          (loop (+ i 1)
                (cons selection result)))
        result))))

(define-method (sal-lhs-type/components (lhs-type <sal-record-type>) (lhs <sal-expr>))
  (map (lambda (field)
         (make-ast-instance <sal-record-selection> lhs
                            :target lhs
                            :idx (slot-value field :id)))
       (slot-value lhs-type :fields)))

(define (sal-lhs-function-type/components lhs-type lhs class)
  (try
   (let* ((domain (slot-value lhs-type :domain))
          (it (sal-type/make-iterator domain))
          (elems '()))
     (iterator/for-each 
      (lambda (idx)
        (let ((new-elem (make-ast-instance class lhs
                                           :fun lhs
                                           :arg idx)))
          (push! new-elem elems)))
      it)
     elems)
   (catch* '(type-iterator expr-evaluator)
           (lambda (_ msg)
             (sign-unsupported-feature lhs "Failed to rearrange commands. Reason: ~a" msg)))))

(define-method (sal-lhs-type/components (lhs-type <sal-function-type>) (lhs <sal-expr>))
  (sal-lhs-function-type/components lhs-type lhs <sal-application>))
                
(define-method (sal-lhs-type/components (lhs-type <sal-array-type>) (lhs <sal-expr>))
  (sal-lhs-function-type/components lhs-type lhs <sal-array-selection>))

(define (sal-lhs-set/union set1 set2)
  (wt-tree/union+ set1 set2 (lambda (lhs-list1 lhs-list2)
                              (let ((new-list lhs-list1))
                                (for-each (lambda (lhs2)
                                            (set! new-list (lhs-list/add new-list lhs2)))
                                          lhs-list2)
                                new-list))))

;; Returns #t if set1 is a subset of set2
(define (sal-lhs-set/subset? set1 set2)
  (wt-tree/subset+? set1 set2 (lambda (lhs-list1 lhs-list2)
                                (breakpoint "sal-lhs-set/subset?" (set1 set2 lhs-list1 lhs-list2) (or (not (list? lhs-list2)) (not (list? lhs-list1))))
                                (for-all (lambda (lhs1)
                                           (lhs-list/contains? lhs-list2 lhs1))
                                         lhs-list1))))

(define (sal-lhs-set/difference set1 set2)
  (wt-tree/difference+ set1 set2 (lambda (lhs-list1 lhs-list2)
                                   (let ((result lhs-list1))
                                     (for-each (lambda (lhs2)
                                                 (set! result (lhs-list/remove result lhs2)))
                                               lhs-list2)
                                     (values (not (null? result)) result)))))

(define (sal-lhs-set/intersection set1 set2)
  (wt-tree/intersection+ set1 set2 (lambda (lhs-list1 lhs-list2)
                                     (let ((result '()))
                                       (for-each (lambda (lhs1)
                                                   (when (lhs-list/contains? lhs-list2 lhs1)
                                                     (set! result (lhs-list/add result lhs1))))
                                                 lhs-list1)
                                       (for-each (lambda (lhs2)
                                                   (when (lhs-list/contains? lhs-list1 lhs2)
                                                     (set! result (lhs-list/add result lhs2))))
                                                 lhs-list2)
                                       (if (null? result)
                                         (values #f #unspecified)
                                         (values #t result))))))
  
(define (sal-lhs-set->list set)
  (let ((result '()))
    (wt-tree/for-each (lambda (_ lhs-list)
                        (for-each (lambda (lhs)
                                    (push! lhs result))
                                  lhs-list))
                      set)
    result))
