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

(module sal-esm-state
        (include "sal.sch")
        (import fast-hash-table sal-esm-support sal-esm-action
                iterators sal2scm-core sal2scm sal-ast-env sal2scm-runtime sal-type
                sal-pp gmp-scheme state-entry-channel sal-expression sal-esm-util
                sal-esm-symmetry sal2scm-random sal-esm-access-level-table)
        (export (make-esm-state-vector size)
                (sal-esm/update-vector! vect idx new-value)
                (sal-esm/update-composite-state-var! state-vector var-idx access-list rhs)
                (sal-esm/safe-update-composite-state-var! state-vector var-idx access-list rhs ctx-name line column)
                (mk-esm-input-vars-non-det-choice-action sal-scm-ctx input-vars var-vector)
                (mk-esm-input-vars-random-choice-action sal-scm-ctx input-vars input-vector)
                (mk-esm-fill-unassinged-state-vars-action state-vars first-curr-var-idx first-next-var-idx var-vector access-levels)
                (mk-esm-non-det-fill-unassinged-state-vars-action sal-scm-ctx state-vars first-curr-var-idx var-vector access-levels)
                (mk-esm-random-fill-unassinged-state-vars-action sal-scm-ctx state-vars first-curr-var-idx var-vector access-levels)
                (mk-esm-committed-random-fill-unassinged-state-vars-action sal-scm-ctx state-vars first-curr-var-idx var-vector access-levels)
                (sal-type/esm-memory-layout type ctx)
                (memory-layout/num-bits-and-num-objs memory-layout layout-definition-vector)
                (var-vector->input-channel! var-vector first-idx layout layout-def-vect constraint-vect channel)
                (memory-layout->var-vector memory-layout var-vector first-idx layout-def-vect)
                (output-channel->var-vector! channel var-vector first-idx layout layout-def-vect))
        )

(define (make-esm-state-vector size)
  (make-vector size 'not-assigned))
    
(define (sal-esm/update-vector! vect idx new-value)
  (let ((old-value (vector-ref vect idx)))
    (esm/add-trail! (lambda ()
                      (vector-set! vect idx old-value)))
    (vector-set! vect idx new-value)))

(define-macro (mk-update-composite-state-var-proc proc-name safe?)
  `(define (,proc-name state-vector var-idx top-access-list rhs ,@(if safe? '(ctx-name line column) '()))
     (let loop ((access-list top-access-list)
                (curr-vect state-vector)
                (curr-idx var-idx))
       [assert (curr-idx access-list curr-vect top-access-list) (number? curr-idx)]
       ,(if safe?
          '(sal-scm/check-bounds ctx-name line column curr-vect curr-idx)
          #unspecified)
       (cond
        ((null? access-list)
         ,(if safe?
            '(when (and (not (eq? (vector-ref curr-vect curr-idx) 'not-assigned))
                        (not (eq? (vector-ref curr-vect curr-idx) 'delayed)))
               (sign-sal-scm-runtime-error ctx-name line column "Left-hand-side was already defined."))
            #unspecified)
         (sal-esm/update-vector! curr-vect curr-idx rhs))
        (else
         (let* ((new-vect (vector-ref curr-vect curr-idx))
                (curr-access (car access-list))
                (new-idx (let ((idx (cdr curr-access)))
                           (cond
                            ((number? idx)
                             idx)
                            ((mpq? idx)
                             (mpq->integer idx))
                            (else
                             ;; An assignment may be delayed because the value
                             ;; of the index is not available yet.
                             ;; This is a problem, because we cannot mark that
                             ;; the value of the location associated with the assignment
                             ;; was delayed. We can't mark because we don't know it.
                             ;; This may cause a bug. The file sched_bug.sal illustrated
                             ;; the problem. Consider the following code fragment.
                             ;;
                             ;;  val1 = arr[idx]; 
                             ;;  arr[idx] = FALSE;
                             ;;  val2 = arr[idx]; 
                             ;; 
                             ;; The assignment arr[idx] = FALSE cannot be evaluated, because
                             ;; the value of idx is not available. Moreover, the location
                             ;; arr[idx] cannot be marked as delayed, since we don't
                             ;; know the value of arr[idx]. The assignments val1 = arr[idx],
                             ;; val2 = arr[idx] are also delayed, but in this case the
                             ;; locations val1 and val2 are correctly marked as delayed.
                             ;; Eventually, we will assign nondeterministic values to
                             ;; the non-assigned locations. Unfortunately, arr[idx] will
                             ;; receive a non-deterministic value, this will not affect
                             ;; arr[idx], because the action arr[idx] = FALSE will be
                             ;; resumed after idx also receives a non-deterministic
                             ;; value. The bug happens, when val1 = arr[idx] or
                             ;; val2 = arr[idx] are resumed before arr[idx] = FALSE,
                             ;; and get a wrong value (i.e., TRUE instead of FALSE).
                             ;; 
                             ;; To solve this problem, I created a new category of delayed
                             ;; action called "weak-delay". A weak delayed action is forced
                             ;; to resume by the explicit resume-delay-actions action. If
                             ;; it cannot be resumed, the specification is rejected, and
                             ;; an error is reported to the user. So, we must use the explicit
                             ;; resume-delay-actions action before assigning nondeterministic
                             ;; value to not-assigned locations in the initialization code,
                             ;; or copying the previous value in the transition code.
                             ;;
                             ;; (print "idx: " idx)
                             (try
                              (esm/force-integer ((eval idx)))
                              (lambda (e p m o)
                                (if (eq? p 'esm-delay-action)
                                  (esm/weak-delay) ;; promote the delay to a weak-delay
                                  (error p m o)))))))))
           (if (eq? new-vect 'not-assigned)
             (let* ((new-vect-size (car curr-access))
                    (new-vect (make-vector new-vect-size 'not-assigned)))
               (esm/add-trail! (lambda ()
                                 (vector-set! curr-vect curr-idx 'not-assigned)))
               (vector-set! curr-vect curr-idx new-vect)
               (loop (cdr access-list)
                     new-vect
                     new-idx))
             (loop (cdr access-list)
                   new-vect
                   new-idx))))))))

(mk-update-composite-state-var-proc sal-esm/update-composite-state-var! #f)
(mk-update-composite-state-var-proc sal-esm/safe-update-composite-state-var! #t)
  
(define (mk-esm-input-vars-choice-action-core sal-scm-ctx input-vars var-vector mk-choice-proc)
  (let ((empty-env (make-empty-env))
        (idx 0))
    (mk-esm-seq-action* (map (lambda (input-var)
                               (lambda ()
                                 (let* ((it (sal-type->scm-iterator (slot-value input-var :type) sal-scm-ctx empty-env))
                                        (values (iterator->list (eval it)))
                                        (curr-idx idx)
                                        (result (mk-choice-proc values (lambda ()
                                                                         (mk-esm-primitive-action
                                                                          (lambda ()
                                                                            (vector-set! var-vector 
                                                                                         curr-idx 
                                                                                         (esm/bounded-value 0)))
                                                                          #f)))))
                                   (set! idx (+ idx 1))
                                   result)))
                             input-vars))))

(define (mk-esm-input-vars-non-det-choice-action sal-scm-ctx input-vars var-vector)
  (mk-esm-input-vars-choice-action-core sal-scm-ctx input-vars var-vector mk-esm-multi-choice-action))

(define (mk-esm-input-vars-random-choice-action sal-scm-ctx input-vars var-vector)
  (mk-esm-input-vars-choice-action-core sal-scm-ctx input-vars var-vector mk-esm-multi-random-choice-action))

(define (mk-esm-fill-unassinged-state-vars-action state-vars first-curr-var-idx first-next-var-idx var-vector access-levels)
  (let ((access-level-vector (list->vector (map (lambda (state-var)
                                                  (sal-esm-access-level-table/get-level access-levels state-var))
                                                state-vars)))
        (n (length state-vars)))
    (mk-esm-primitive-action (lambda ()
                               (let loop ((i 0))
                                 (when (< i n)
                                   (let inner-loop ((curr-vect var-vector)
                                                    (next-vect var-vector)
                                                    (curr-idx (+ first-curr-var-idx i))
                                                    (next-idx (+ first-next-var-idx i))
                                                    (curr-level (vector-ref access-level-vector i)))
                                     (let ((curr-elem (vector-ref curr-vect curr-idx))
                                           (next-elem (vector-ref next-vect next-idx)))
                                       (cond
                                        ((eq? next-elem 'not-assigned)
                                         ;; found unassined slot... complete it with the value in curr-vect
                                         (vector-set! next-vect next-idx curr-elem)
                                         (esm/add-trail! (lambda () (vector-set! next-vect next-idx 'not-assigned))))
                                        ((= curr-level 0)
                                         ;; I don't need to explore the children of next-elem.
                                         ;; So, do nothing...
                                         #unspecified)
                                        ((vector? next-elem)
                                         [assert (curr-elem next-elem curr-level access-level-vector) 
                                                 (and (vector? curr-elem)
                                                      (vector? next-elem)
                                                      (= (vector-length curr-elem) (vector-length next-elem)))]
                                         (let ((size (vector-length curr-elem)))
                                           (let it-loop ((j 0))
                                             (when (< j size)
                                               (inner-loop curr-elem next-elem j j (- curr-level 1))
                                               (it-loop (+ j 1)))))))))
                                   (loop (+ i 1)))))
                             #f)))


(define (assigned-access? state-vector idx access-list)
  (let loop ((curr-elem (vector-ref state-vector idx))
             (access-list access-list))
    (and (not (eq? curr-elem 'not-assigned))
         (or (null? access-list)
             (not (vector? curr-elem))
             (let* ((curr-access (car access-list))
                    (curr-idx (cdr curr-access)))
               (loop (vector-ref curr-elem curr-idx)
                     (cdr access-list)))))))

(define-generic (get-type-components type num-components))
(define-method (get-type-components (type <sal-tuple-type>) (num-components <primitive>))
  [assert (num-components type) (= (length (slot-value type :types)) num-components)]
  (slot-value type :types))
(define-method (get-type-components (type <sal-record-type>) (num-components <primitive>))
  [assert (num-components type) (= (length (slot-value type :fields)) num-components)]
  (map (lambda (field) (slot-value field :type)) (slot-value type :fields)))
(define-method (get-type-components (type <sal-function-type>) (num-components <primitive>))
  (let ((range (slot-value type :range)))
    (generate-list (lambda (_)
                     range)
                   num-components)))
(define-method (get-type-components (type <sal-type-name>) (num-components <primitive>))
  (get-type-components (sal-type-name/definition type) num-components))

(define (mk-catch-translation-errors-proc var-decl)
  (lambda (escape proc msg obj)
    (if (eq? proc 'sal-to-scm-error)
      (escape (mk-esm-primitive-action 
               (lambda ()
                 (sign-sal-scm-runtime-error-old var-decl "Variable `~a' (or parts of it) must be explicitly initialized, because the tool failed to produce a non-deterministic assignment for it. Reason: ~a" (sal-decl/name var-decl) msg))
               #f))
      (error proc msg obj))))

(define (gen-non-det-assignment-if-needed var-decl level var-vector var-idx sal-scm-ctx mk-lazy-choice-proc)
  (let loop ((type (slot-value var-decl :type))
             (level level)
             (access-list '()))
    (let ((rev-access-list (reverse access-list)))
      (let* ((empty-env (make-empty-env))
             (mk-main-action 
              (lambda ()
                (mk-esm-cond-action 
                 (lambda ()
;                    (print var-vector)
;                    (print "checking " var-idx " access-list " rev-access-list 
;                           " accessible: " (assigned-access? var-vector var-idx rev-access-list))
                   (not (assigned-access? var-vector var-idx rev-access-list)))
                 (lambda ()
                   (try 
                    (let ((it (sal-type->scm-iterator type sal-scm-ctx empty-env)))
                      (mk-lazy-choice-proc (lambda ()
                                             (iterator->list (eval it)))
                                           (lambda ()
                                             (mk-esm-primitive-action 
                                              (lambda ()
                                                (sal-esm/update-composite-state-var!
                                                 var-vector 
                                                 var-idx
                                                 rev-access-list
                                                 (esm/bounded-value 0)))
                                              #f))))
                    (mk-catch-translation-errors-proc var-decl)))))))
        (if (= level 0)
          (mk-main-action)
          (let ((type-vector-size (sal-type/number-of-elements-as-integer type)))
            (if (not type-vector-size)
              (mk-main-action)
              (let* ((type-components (get-type-components type type-vector-size))
                     ;; (_ (begin (sal/pp type-components) (print "")))
                     (idx 0)
                     (mk-components-action 
                      (lambda ()
                        (mk-esm-seq-action* 
                         (map (lambda (comp-type)
                                (lambda ()
                                  (let ((curr-idx idx))
                                    (set! idx (+ idx 1))
                                    (loop comp-type (- level 1) (cons (cons type-vector-size curr-idx) access-list)))))
                              type-components)))))
                (mk-esm-seq-action mk-main-action mk-components-action)))))))))


(define (mk-esm-non-det-fill-unassinged-state-vars-action-core sal-scm-ctx state-vars first-curr-var-idx var-vector access-levels mk-lazy-choice-proc)
  (let ((empty-env (make-empty-env))
        (access-level-list (map (lambda (state-var)
                                  (sal-esm-access-level-table/get-level access-levels state-var))
                                state-vars))
        (idx first-curr-var-idx))
    ;; (print "access-level-list: " access-level-list)
    (mk-esm-seq-action* 
     (map (lambda (var-decl level)
            (lambda ()
              (let ((curr-idx idx))
                (set! idx (+ idx 1))
                (gen-non-det-assignment-if-needed var-decl level var-vector curr-idx sal-scm-ctx mk-lazy-choice-proc))))
            state-vars
            access-level-list))))

(define (mk-esm-non-det-fill-unassinged-state-vars-action sal-scm-ctx state-vars first-curr-var-idx var-vector access-levels)
  (mk-esm-non-det-fill-unassinged-state-vars-action-core
   sal-scm-ctx state-vars first-curr-var-idx var-vector access-levels mk-esm-lazy-multi-choice-action))

(define (mk-esm-random-fill-unassinged-state-vars-action sal-scm-ctx state-vars first-curr-var-idx var-vector access-levels)
  (mk-esm-non-det-fill-unassinged-state-vars-action-core
   sal-scm-ctx state-vars first-curr-var-idx var-vector access-levels mk-esm-lazy-multi-random-choice-action))

(define (gen-pick-random-if-needed var-decl level var-vector var-idx sal-scm-ctx)
  (let loop ((type (slot-value var-decl :type))
             (level level)
             (access-list '()))
    (let ((rev-access-list (reverse access-list)))
      (let* ((empty-env (make-empty-env))
             (mk-main-action 
              (lambda ()
                (try 
                 (let ((pick-random-proc (sal-type/gen-pick-random-elem type sal-scm-ctx empty-env)))
                   (mk-esm-primitive-action
                    (lambda ()
                      (when (not (assigned-access? var-vector var-idx rev-access-list))
                        (sal-esm/update-composite-state-var!
                         var-vector 
                         var-idx
                         rev-access-list
                         (eval pick-random-proc))))
                    #f))
                 (mk-catch-translation-errors-proc var-decl)))))
        (if (= level 0)
          (mk-main-action)
          (let ((type-vector-size (sal-type/number-of-elements-as-integer type)))
            (if (not type-vector-size)
              (mk-main-action)
              (let* ((type-components (get-type-components type type-vector-size))
                     (idx 0)
                     (mk-components-action 
                      (lambda ()
                        (mk-esm-seq-action* 
                         (map (lambda (comp-type)
                                (lambda ()
                                  (let ((curr-idx idx))
                                    (set! idx (+ idx 1))
                                    (loop comp-type (- level 1) (cons (cons type-vector-size curr-idx) access-list)))))
                              type-components)))))
                (mk-esm-seq-action mk-main-action mk-components-action)))))))))
  
;; Fill the unassigned variables with random values, and do not create backtrack points to obtain the other alternatives
;; This action is useful to implement random simulators where you don't care about other alternatives.
;; Remark: This is not the case for guided simulation, where you should be able to evaluate all possible initial states
;; a pick the one with highest score.
(define (mk-esm-committed-random-fill-unassinged-state-vars-action sal-scm-ctx state-vars first-curr-var-idx var-vector access-levels)
  (let ((empty-env (make-empty-env))
        (access-level-list (map (lambda (state-var)
                                  (sal-esm-access-level-table/get-level access-levels state-var))
                                state-vars))
        (idx first-curr-var-idx))
    (mk-esm-seq-action* 
     (map (lambda (var-decl level)
            (lambda ()
              (let ((curr-idx idx))
                (set! idx (+ idx 1))
                (gen-pick-random-if-needed var-decl level var-vector curr-idx sal-scm-ctx))))
            state-vars
            access-level-list))))

(define-generic (sal-type/esm-memory-layout type ctx))

(define-method (sal-type/esm-memory-layout (type <primitive>) (ctx <sal-scm-context>))
  'infinite)

(define-method (sal-type/esm-memory-layout (type <sal-bool-type>) (ctx <sal-scm-context>))
  'boolean)

(define-method (sal-type/esm-memory-layout (type <sal-scalar-type>) (ctx <sal-scm-context>))
  (mk-esm-atom-layout (sal-scalar-type/num-bits type) 0))

(define-method (sal-type/esm-memory-layout (type <sal-bounded-subtype>) (ctx <sal-scm-context>))
  (multiple-value-bind
      (num-bits lower)
      (sal-bounded-subtype/num-bits-and-lower type ctx)
    (if num-bits 
      (if (slot-value ctx :gmp?)
        (mk-esm-mpq-layout num-bits lower)
        (mk-esm-atom-layout num-bits lower))
      'infinite)))

(define-method (sal-type/esm-memory-layout (type <sal-number-type>) (ctx <sal-scm-context>))
  'infinite)

(define-method (sal-type/esm-memory-layout (type <sal-real-type>) (ctx <sal-scm-context>))
  'infinite)

(define-method (sal-type/esm-memory-layout (type <sal-int-type>) (ctx <sal-scm-context>))
  'infinite)

(define-method (sal-type/esm-memory-layout (type <sal-nat-type>) (ctx <sal-scm-context>))
  'infinite)

(define-method (sal-type/esm-memory-layout (type <sal-data-type>) (ctx <sal-scm-context>))
  (let ((tag-num-bits (sal-data-type/tag-num-bits type)))
    (mk-esm-datatype-layout tag-num-bits 
                            (apply vector (map (cut sal-constructor/esm-memory-layout <> ctx)
                                               (slot-value type :constructors))))))

(define (sal-constructor/esm-memory-layout constructor ctx)
  (let ((type (sal-name-expr/type constructor)))
    (if (sal-type/function? type)
      (let* ((domain (sal-function-type/domain type))
             (layout (sal-type/esm-memory-layout domain ctx)))
        (if (sal-type/tuple? domain)
          (vector->list layout)
          (list layout)))
      '())))

(define-method (sal-type/esm-memory-layout (type <sal-tuple-type>) (ctx <sal-scm-context>))
  (apply vector (map (cut sal-type/esm-memory-layout <> ctx) (slot-value type :types))))

(define-method (sal-type/esm-memory-layout (type <sal-record-type>) (ctx <sal-scm-context>))
  (apply vector (map (lambda (field)
                       (sal-type/esm-memory-layout (slot-value field :type) ctx))
                     (sal-record-type/sorted-fields type))))

(define-method (sal-type/esm-memory-layout (type <sal-type-name>) (ctx <sal-scm-context>))
  (if (sal-type-name/recursive? type)
    'infinite
    (let ((definition (sal-type-name/definition type)))
      (unless definition
        (sign-unsupported-feature type "Uninterpreted types are not supported by this tool."))
      (sal-type/esm-memory-layout definition ctx))))

(define-method (sal-type/esm-memory-layout (type <sal-function-type>) (ctx <sal-scm-context>))
  (let ((domain (slot-value type :domain)))
    (let* ((range-data (sal-type/esm-memory-layout (slot-value type :range) ctx))
           (domain-size (sal-type/number-of-elements-as-integer type)))
      (make-vector domain-size range-data))))

(define-method (sal-type/esm-memory-layout (type <sal-subtype>) (ctx <sal-scm-context>))
  (sal-type/esm-memory-layout (sal-subtype/immediate-super-type type) ctx))

(define (memory-layout/num-bits-and-num-objs memory-layout layout-def-vect)
  (let loop ((mem-layout memory-layout)
             (num-bits 0)
             (num-objs 0))
    (cond
     ((vector? mem-layout)
      (vector-layout/num-bits-and-num-objs mem-layout num-bits num-objs loop))
     ((list? mem-layout)
      (list-layout/num-bits-and-num-objs mem-layout num-bits num-objs loop))
     ((eq? mem-layout 'boolean)
      (values (+ num-bits 1) num-objs))
     ((esm-atom-layout? mem-layout)
      (values (+ num-bits (esm-atom-layout/num-bits mem-layout)) num-objs))
     ((esm-sym-atom-layout? mem-layout)
      (values (+ num-bits (esm-sym-atom-layout/num-bits mem-layout)) num-objs))
     ((esm-ref-layout? mem-layout)
      (if (esm-ref-layout/recursive? mem-layout)
        (values num-bits (+ num-objs 1))
        (loop (vector-ref layout-def-vect (esm-ref-layout/definition-idx mem-layout)) num-bits num-objs)))
     ((esm-scalar-set-array-layout? mem-layout)
      (vector-layout/num-bits-and-num-objs (esm-scalar-set-array-layout/body mem-layout) num-bits num-objs loop))
     ((esm-ring-set-array-layout? mem-layout)
      (vector-layout/num-bits-and-num-objs (esm-ring-set-array-layout/body mem-layout) num-bits num-objs loop))
     ((esm-datatype-layout? mem-layout)
      (datatype/num-bits-and-num-objs mem-layout num-bits num-objs loop))
     ((or (eq? mem-layout 'infinite) (eq? mem-layout 'gmp-infinite))
      (values num-bits (+ num-objs 1)))
     ((esm-mpq-layout? mem-layout)
      (values (+ num-bits (esm-mpq-layout/num-bits mem-layout)) num-objs))
     ((esm-sym-mpq-layout? mem-layout)
      (values (+ num-bits (esm-sym-mpq-layout/num-bits mem-layout)) num-objs))
     (else
      (internal-error)))))

(define (vector-layout/num-bits-and-num-objs layout num-bits num-objs proc-child)
  (let ((n (vector-length layout)))
    (let loop ((i 0)
               (num-bits num-bits)
               (num-objs num-objs))
      (if (< i n)
        (multiple-value-bind
            (new-num-bits new-num-objs)
            (proc-child (vector-ref layout i) num-bits num-objs)
          (loop (+ i 1) new-num-bits new-num-objs))
        (values num-bits num-objs)))))

(define (list-layout/num-bits-and-num-objs layout num-bits num-objs proc-child)
  (if (null? layout)
    (values num-bits num-objs)
    (multiple-value-bind
        (new-num-bits new-num-objs)
        (proc-child (car layout) num-bits num-objs)
      (list-layout/num-bits-and-num-objs (cdr layout) new-num-bits new-num-objs proc-child))))
       
(define (datatype/num-bits-and-num-objs layout num-bits num-objs proc-child) 
  (let* ((tag-bits (esm-datatype-layout/tag-num-bits layout))
         (constructors-layout (esm-datatype-layout/constructor-vect layout))
         (n (vector-length constructors-layout))
         (num-bits (+ tag-bits num-bits)))
    (let loop ((i 0)
               (max-num-bits num-bits)
               (max-num-objs 0))
      (if (< i n)
        (multiple-value-bind
            (new-num-bits new-num-objs)
            (proc-child (vector-ref constructors-layout i) num-bits num-objs)
          (loop (+ i 1) (max max-num-bits new-num-bits) (max max-num-objs new-num-objs)))
        (values max-num-bits max-num-objs)))))

(define-inline (follow-non-recursive-reference layout layout-def-vect)
  (cond
   ((or (not (esm-ref-layout? layout))
        (esm-ref-layout/recursive? layout))
    layout)
   (else
    ;; (print "following definition: " layout " " (vector-ref layout-def-vect (esm-ref-layout/definition-idx layout)))
    (follow-non-recursive-reference (vector-ref layout-def-vect (esm-ref-layout/definition-idx layout)) layout-def-vect))))

(define (var-vector->input-channel! var-vector first-idx layout layout-def-vect constraint-vect channel)
;   (print "var vector:")
;   (pp var-vector)
;   (print "first-idx: ")
;   (pp first-idx)
;   (print "layout: ")
;   (pp layout)
;   (print "layout-def-vect: ")
;   (pp layout-def-vect)
  (letrec ([proc-value-vect
            (lambda (value-vector layout first-idx)
              (let ((size (vector-length layout)))
                (let loop ((idx 0))
                  (when (< idx size)
                    (let* ((value-idx (+ idx first-idx))
                           (curr-value (vector-ref value-vector value-idx))
                           (curr-layout (vector-ref layout idx)))
                      (proc-value curr-value curr-layout))
                    (loop (+ idx 1))))))]
           [proc-value
            (lambda (value layout)
              (let ((layout (follow-non-recursive-reference layout layout-def-vect)))
                (cond 
                 ;; BOOLEANS
                 ((eq? layout 'boolean)
                  [assert (value) (or (boolean? value) (eq? value 'not-assigned))]
                  (sec/add-bit! channel value))
                 ;; FINITE INTEGERS
                 ((esm-atom-layout? layout)
                  (esm-atom-value->channel! value layout channel))
                 ;; TUPLES, RECORDS, AND ARRAYS
                 ((vector? layout)
                  (let ((value (if (not (eq? value 'not-assigned)) ;; choice variables may be unassigned
                                      value
                                      (make-vector (vector-length layout) 'not-assigned))))
                    (proc-value-vect value layout 0)))
                 ;; SCALAR SET and RING SET ATOMS
                 ((esm-sym-atom-layout? layout)
                  (esm-sym-atom-value->channel! value layout constraint-vect channel))
                 ;; SCALAR SET ARRAYS
                 ((esm-scalar-set-array-layout? layout)
                  (esm-scalar-set-array->channel! value layout layout-def-vect constraint-vect proc-value))
                 ;; RING SET ARRAYS
                 ((esm-ring-set-array-layout? layout)
                  (esm-ring-set-array->channel! value layout layout-def-vect constraint-vect proc-value))
                 ;; RECURSIVE REFERENCES SHOULD BE STORED AS OBJECTS (after normalization)
                 ((esm-ref-layout? layout)
                  [assert (layout) (esm-ref-layout/recursive? layout)]
                  (sec/add-obj! channel (esm/normalize-symmetric-object value layout layout-def-vect constraint-vect)))
                 ;; INFINITE DATA: RECURSIVE DATATYPES, RATIONALS, INTEGERS, NATURALS
                 ((or (eq? layout 'infinite) (eq? layout 'gmp-infinite))
                  (sec/add-obj! channel value))
                 ;; DATATYPES 
                 ((esm-datatype-layout? layout)
                  (esm-datatype-value->channel! value layout channel proc-value))
                 ;; BOUNDED GMP NUMBERS
                 ((esm-mpq-layout? layout)
                  (esm-mpq-value->channel! value layout channel))
                 ;; SCALAR SET and RING SET ATOMS WHEN GMP IS USED
                 ((esm-sym-mpq-layout? layout)
                  (esm-sym-mpq-value->channel! value layout constraint-vect channel))
                 (else
                  [assert (value layout) #f]
                  (internal-error)))))])
    (proc-value-vect var-vector layout first-idx)))
    

(define (memory-layout->var-vector layout var-vector first-idx layout-def-vect) 
  (let loop ((value-vector var-vector)
             (layout layout)
             (first-idx first-idx))
    (let ((size (vector-length layout)))
      (let inner-loop ((idx 0))
        (when (< idx size)
          (let* ((value-idx (+ idx first-idx))
                 (layout-idx idx)
                 (curr-layout (follow-non-recursive-reference (vector-ref layout layout-idx) layout-def-vect)))
            (cond
             ((vector? curr-layout)
              (vector-layout->var-vector curr-layout value-vector value-idx loop))
             ((esm-scalar-set-array-layout? curr-layout)
              (vector-layout->var-vector (esm-scalar-set-array-layout/body curr-layout) value-vector value-idx loop))
             ((esm-ring-set-array-layout? curr-layout)
              (vector-layout->var-vector (esm-ring-set-array-layout/body curr-layout) value-vector value-idx loop))
             (else
              (vector-set! value-vector value-idx 'not-assigned))))
          (inner-loop (+ idx 1)))))))

(define (vector-layout->var-vector layout value-vector value-idx proc)
  (let* ((size (vector-length layout))
         (new-vector (make-vector size)))
    (vector-set! value-vector value-idx new-vector)
    (proc new-vector layout 0)))
  
(define (output-channel->var-vector! channel var-vector first-idx layout layout-def-vect)
  (letrec ([fill-value-vect
            (lambda (value-vector layout-vector first-idx)
              [assert (value-vector layout-vector) (and (vector? value-vector) (vector? layout-vector))]
              (let ((size (vector-length layout-vector)))
                (let loop ((idx 0))
                  (when (< idx size)
                    (let* ((value-idx (+ idx first-idx))
                           (layout-idx idx)
                           (curr-value (vector-ref value-vector value-idx))
                           (curr-layout (follow-non-recursive-reference (vector-ref layout-vector layout-idx) layout-def-vect)))
                      (cond
                       ((vector? curr-value)
                        (cond 
                         ((vector? curr-layout)
                          (fill-value-vect curr-value curr-layout 0))
                         ((esm-scalar-set-array-layout? curr-layout)
                          (fill-value-vect curr-value (esm-scalar-set-array-layout/body curr-layout) 0))
                         ((esm-ring-set-array-layout? curr-layout)
                          (fill-value-vect curr-value (esm-ring-set-array-layout/body curr-layout) 0))
                         (else
                          (internal-error))))
                       (else
                        (vector-set! value-vector value-idx (read-value curr-layout))))
                      (loop (+ idx 1)))))))]
           [read-vector-value 
            (lambda (layout)
              [assert (layout) (vector? layout)]
               (let ((new-vect (make-vector (vector-length layout))))
                 (fill-value-vect new-vect layout 0)
                 new-vect))]
           [read-value
            (lambda (layout)
              (let ((layout (follow-non-recursive-reference layout layout-def-vect)))
                ;; (print "read-value layout: " layout)
                (cond
                 ;; BOOLEANS
                 ((eq? layout 'boolean)
                  (sec/read-bit! channel))  
                 ;; FINITE NUMBERS
                 ((esm-atom-layout? layout)
                  (channel->esm-atom-value! channel layout))
                 ;; TUPLES, RECORDS, AND ARRAYS
                 ((vector? layout)
                  (read-vector-value layout))
                 ;; SCALAR SET and RING SET ATOMS
                 ((esm-sym-atom-layout? layout)
                  (channel->esm-sym-atom-value! channel layout))
                 ;; SCALAR SET ARRAYS
                 ((esm-scalar-set-array-layout? layout)
                  (read-vector-value (esm-scalar-set-array-layout/body layout)))
                 ;; RING SET ARRAYS
                 ((esm-ring-set-array-layout? layout)
                  (read-vector-value (esm-ring-set-array-layout/body layout)))
                 ;; INFINITE DATA: RECURSIVE DATATYPES, RATIONALS, INTEGERS, NATURALS
                 ((or (eq? layout 'infinite)
                      (eq? layout 'gmp-infinite)
                      (esm-ref-layout? layout))
                  [assert (layout) (or (not (esm-ref-layout? layout)) (esm-ref-layout/recursive? layout))]
                  (sec/read-obj! channel))
                 ;; DATATYPES 
                 ((esm-datatype-layout? layout)
                  ;; (print "reading datatype: " layout)
                  (let* ((tag-num-bits (esm-datatype-layout/tag-num-bits layout))
                         (tag-idx (sec/read-num! channel tag-num-bits))
                         (constructor-layouts (esm-datatype-layout/constructor-vect layout))
                         (new-layout (vector-ref constructor-layouts tag-idx)))
                    ;; (print "tag-idx: " tag-idx "new-layout: " new-layout)
                    [assert (new-layout) (list? new-layout)]
                    (cons tag-idx (map read-value new-layout))))
                 ;; BOUNDED GMP NUMBERS
                 ((esm-mpq-layout? layout)
                  (channel->esm-mpq-value! channel layout))
                 ;; SCALAR SET and RING SET ATOMS WHEN GMP IS USED
                 ((esm-sym-mpq-layout? layout)
                  (channel->esm-sym-mpq-value! channel layout))
                 (else
                  (internal-error)))))])
    (fill-value-vect var-vector layout first-idx)))
            









