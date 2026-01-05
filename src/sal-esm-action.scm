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

(module sal-esm-action
        (include "sal.sch")
        (import permute xformat random)
        (export (esm/execute-action start-pc)
                (esm/next-alternative)
                (esm/execute-delayed-actions continue?)
                (esm/backtrack!)
                (esm/add-trail! trail)
                (esm/bounded-value reg-idx)
                (mk-esm-main-action stmt-proc)
                (mk-esm-primitive-action proc place-provider)
                (mk-esm-execute-delayed-actions-action)
                (mk-esm-seq-action . stmt-procs)
                (mk-esm-seq-action* stmt-procs)
                (mk-esm-choice-action . stmt-procs)
                (mk-esm-choice-action* stmt-procs)
                (mk-esm-random-choice-action . stmt-procs)
                (mk-esm-random-choice-action* stmt-procs)
                (mk-esm-multi-choice-action-core values-or-proc stmt-proc permute? lazy?)
                (mk-esm-multi-choice-action values-or-proc stmt-proc)
                (mk-esm-multi-random-choice-action values-or-proc stmt-proc)
                (mk-esm-lazy-multi-choice-action values-or-proc stmt-proc)
                (mk-esm-lazy-multi-random-choice-action values-or-proc stmt-proc)
                (mk-esm-multi-seq-action values stmt-proc)
                (mk-esm-cond-action pred stmt-proc)
                (mk-esm-noop-action)
                (esm/fail)
                (mk-esm-guard-action pred place-provider)
                (esm/delay)
                (esm/weak-delay)
                (esm/for-each-alternative start-pc proc)
                (esm/for-each-k-firt-alternatives start-pc k proc)
                )
        )

(define-record-type esm-backtrack-entry
  (mk-esm-backtrack-entry pc saved-bound-vals trail-stack-top-idx delayed-actions)
  esm-backtrack-entry?
  (pc esm-backtrack-entry/pc esm-backtrack-entry/set-pc!)
  (saved-bound-vals esm-backtrack-entry/saved-bound-vals esm-backtrack-entry/set-saved-bound-vals!)
  (trail-stack-top-idx esm-backtrack-entry/trail-stack-top-idx esm-backtrack-entry/set-trail-stack-top-idx!)
  (delayed-actions esm-backtrack-entry/delayed-actions esm-backtrack-entry/set-delayed-actions!))

(define-record-type esm-delayed-action
  (mk-esm-delayed-action proc saved-bound-vals place-provider)
  esm-delayed-action?
  (proc esm-delayed-action/proc esm-delayed-action/set-proc!)
  (saved-bound-vals esm-delayed-action/saved-bound-vals esm-delayed-action/set-saved-bound-vals!)
  (place-provider esm-delayed-action/place-provider esm-delayed-action/set-place-provider!))

(define *esm-default-vectors-size* 8)

(define *esm-bound-vals* #f)

(define *esm-next-bound-val-idx* 0)

(define *esm-delayed-actions* '())

(define *esm-trail-stack* #f)

(define *esm-trail-stack-top-idx* 0)

(define *esm-backtrack-stack* #f)

(define *esm-backtrack-stack-top-idx* 0)

(define *esm-code* #f)

(define *esm-next-code-idx* #f)

(define *esm-pc* 0)

(define (vector-fill-with! vect from to proc)
  (let loop ((idx from))
    (when (< idx to)
      (vector-set! vect idx (proc))
      (loop (+ idx 1)))))
  
(define (expand-vect! vect proc)
  (let* ((size (vector-length vect))
         (new-size (* size 2))
         (new-vect (copy-vector vect new-size)))
    (vector-fill-with! new-vect size new-size proc)
    new-vect))
 
(define (init-vect! proc)
  (let ((new-vect (make-vector *esm-default-vectors-size*)))
    (let loop ((idx 0))
      (when (< idx *esm-default-vectors-size*)
        (vector-set! new-vect idx (proc))
        (loop (+ idx 1))))
    new-vect))

(define (mk-empty-backtrack-entry)
  (mk-esm-backtrack-entry #unspecified #unspecified #unspecified #unspecified))

(define (mk-unspecified)
  #unspecified)

(define (mk-false)
  #f)

(define-inline (check-esm-code)
  (when (>= *esm-next-code-idx* (vector-length *esm-code*))
    (set! *esm-code* (expand-vect! *esm-code* mk-false))))  

(define (esm/add-cmd! cmd)
  (check-esm-code)
  (let ((idx *esm-next-code-idx*))
    (vector-set! *esm-code* idx cmd)
    (set! *esm-next-code-idx* (+ idx 1))
    idx))

(define (esm/setup!)
  (set! *esm-bound-vals* (init-vect! mk-unspecified))
  (set! *esm-trail-stack* (init-vect! mk-unspecified))
  (set! *esm-backtrack-stack* (init-vect! mk-empty-backtrack-entry))
  (set! *esm-code* (init-vect! mk-false))
  (set! *esm-next-code-idx* 0)
  (esm/add-cmd! 'failed))

(define *esm-failure-cmd-idx* 0)

(esm/setup!)

(define (esm/init! start-pc)
  (set! *esm-delayed-actions* '())
  (set! *esm-trail-stack-top-idx* 0)
  (set! *esm-backtrack-stack-top-idx* 0)
  (set! *esm-next-bound-val-idx* 0)
  (set! *esm-pc* start-pc))

(define (esm/execute-action start-pc)
  (esm/init! start-pc)
  (esm/main-loop))

(define (esm/main-loop)
  (let loop ()
    (let ((cmd (vector-ref *esm-code* *esm-pc*)))
      (cond
       ((procedure? cmd)
        (cmd)
        (loop))
       ((eq? cmd 'succeeded)
        (case (esm/execute-delayed-actions #f)
          ((succeeded)
           #t)
          ((failed)
           (esm/backtrack!)
           (loop))))
       ((eq? cmd 'failed)
        #f)))))

(define (esm/next-alternative)
  (esm/backtrack!)
  (esm/main-loop))

(define-inline (esm/inc-pc!)
  (set! *esm-pc* (+ *esm-pc* 1)))

(define (delayed-actions->locations delayed-actions)
  (map (lambda (delayed-action)
         (format-with-location (esm-delayed-action/place-provider delayed-action) ""))
       (filter (lambda (act)
                 (and act
                      (not (eq? (sal-ast/context-name (esm-delayed-action/place-provider act)) 
                                'prelude))))
               delayed-actions)))

(define (exec-delayed-action delayed-action)
  (let ((act-proc (esm-delayed-action/proc delayed-action))
        (saved-bound-vals (esm-delayed-action/saved-bound-vals delayed-action)))
    [sal-assert "esm/execute-delayed-actions" (act-proc delayed-action) (procedure? act-proc)]
    (esm/restore-bound-vals! saved-bound-vals)
    (try
     (begin 
       (act-proc)
       'succeeded)
     (lambda (escape proc msg obj)
       (cond
        ((eq? proc 'esm-action-failure)
         (escape 'failed))
        ((eq? proc 'esm-delay-action)
         (escape 'delayed))
        ((eq? proc 'esm-weak-delay-action)
         (escape 'weak-delay))
        (else
         (error proc msg obj)))))))


(define (esm/execute-delayed-actions continue?)
  (let ((curr-bounded-vals (esm/save-bound-vals)))
    (unwind-protect
     (let loop ((delayed-actions *esm-delayed-actions*)
                (executed-something? #f)
                (remaining-actions '())
                (weak-delayed-actions '()))
       (if (null? delayed-actions)
         (if (or (null? remaining-actions) (not executed-something?))
           (cond
            (continue?
             (unless (null? weak-delayed-actions)
               ;; A forced execution of delayed actions must execute all weak delayed actions...
               ;; So, we must sign an error to the user if this is not the case...
               ;; Se a comment on sal-esm-state regarding weak delayed actions for more details.
               (error 'sal-scm-runtime-error
                      (xformat #f "Specification is not supported by the explicit state execution engine. The index of one (or more) assignments depend on value(s) that are not available yet. This problem usually happens when the index variable used in the problematic assignment was not initialized or its next value was not specified.\nProblem source: ~a"
                               (delayed-actions->locations weak-delayed-actions))
                      #unspecified))
             (set! *esm-delayed-actions* remaining-actions)
             'succeeded)
            ((null? remaining-actions)
             (set! *esm-delayed-actions* '())
             'succeeded)
            (else
             (error 'sal-scm-runtime-error 
                    (xformat #f "Specification has a circular dependency.\nProblem source: ~a"
                             (delayed-actions->locations remaining-actions))
                    #unspecified)))
           (loop remaining-actions #f '() '()))
         (case (exec-delayed-action (car delayed-actions))
           ((succeeded)
            (loop (cdr delayed-actions) #t remaining-actions weak-delayed-actions))
           ((failed)
            'failed)
           ((delayed)
            (loop (cdr delayed-actions) 
                  executed-something? 
                  (cons (car delayed-actions) remaining-actions)
                  weak-delayed-actions))
           ((weak-delay)
            (loop (cdr delayed-actions)
                  executed-something?
                  (cons (car delayed-actions) remaining-actions)
                  (cons (car delayed-actions) weak-delayed-actions)))
           (else
            (internal-error)))))
     (esm/restore-bound-vals! curr-bounded-vals))))

(define (esm/backtrack!)
  ;; (print "backtracking...")
  (cond
   ((= *esm-backtrack-stack-top-idx* 0)
    (set! *esm-pc* *esm-failure-cmd-idx*))
   (else 
    (set! *esm-backtrack-stack-top-idx* (- *esm-backtrack-stack-top-idx* 1))
    (let ((backtrack-entry (vector-ref *esm-backtrack-stack* *esm-backtrack-stack-top-idx*)))
      (set! *esm-pc* (esm-backtrack-entry/pc backtrack-entry))
      (esm-backtrack-entry/set-pc! backtrack-entry #unspecified)
      (esm/restore-bound-vals! (esm-backtrack-entry/saved-bound-vals backtrack-entry))
      (esm-backtrack-entry/set-saved-bound-vals! backtrack-entry #unspecified)
      (esm/pop-trail-until! (esm-backtrack-entry/trail-stack-top-idx backtrack-entry))
      (esm-backtrack-entry/set-trail-stack-top-idx! backtrack-entry #unspecified)
      (set! *esm-delayed-actions* (esm-backtrack-entry/delayed-actions backtrack-entry))
      (esm-backtrack-entry/set-delayed-actions! backtrack-entry #unspecified)))))

(define (esm/add-trail! trail)
  (when (> *esm-backtrack-stack-top-idx* 0)
    (when (>= *esm-trail-stack-top-idx* (vector-length *esm-trail-stack*))
      (set! *esm-trail-stack* (expand-vect! *esm-trail-stack* mk-unspecified)))
    (vector-set! *esm-trail-stack* *esm-trail-stack-top-idx* trail)
    (set! *esm-trail-stack-top-idx* (+ *esm-trail-stack-top-idx* 1))))

(define (esm/pop-trail-until! idx)
  [assert (idx *esm-trail-stack-top-idx*) (<= idx *esm-trail-stack-top-idx*)]
  (let loop ()
    (when (< idx *esm-trail-stack-top-idx*)
      [assert (idx *esm-trail-stack-top-idx*) (> *esm-trail-stack-top-idx* 0)]
      (set! *esm-trail-stack-top-idx* (- *esm-trail-stack-top-idx* 1))
      ((vector-ref *esm-trail-stack* *esm-trail-stack-top-idx*))
      (vector-set! *esm-trail-stack* *esm-trail-stack-top-idx* #unspecified)
      (loop))))

(define (esm/delay-action! proc place-provider)
  (push! (mk-esm-delayed-action proc (esm/save-bound-vals) place-provider)
         *esm-delayed-actions*)
  (esm/inc-pc!))

(define (esm/save-bound-vals)
  (let ((result '()))
    (let loop ((idx (- *esm-next-bound-val-idx* 1)))
      (when (>= idx 0)
        (push! (vector-ref *esm-bound-vals* idx) result)
        (loop (- idx 1))))
    ;; (print "saved-bound-vals: " result)
    result))

(define (esm/restore-bound-vals! saved-values)
  (let ((idx 0))
    (for-each (lambda (val)
                (vector-set! *esm-bound-vals* idx val)
                (set! idx (+ idx 1)))
              saved-values)
    ;; (print "restoring next-bound-idx: " idx)
    (set! *esm-next-bound-val-idx* idx)))
      
(define (esm/set-bounded-val! val) 
  ;; (print "bounding idx: " *esm-next-bound-val-idx* ", val: " val)
  (when (>= *esm-next-bound-val-idx* (vector-length *esm-bound-vals*))
    (set! *esm-bound-vals* (expand-vect! *esm-bound-vals* mk-unspecified)))
  (vector-set! *esm-bound-vals* *esm-next-bound-val-idx* val)
  (set! *esm-next-bound-val-idx* (+ *esm-next-bound-val-idx* 1)))

(define (esm/unset-bounded-val!)
  [assert (*esm-next-bound-val-idx*) (> *esm-next-bound-val-idx* 0)]
  ;; (print "unbounding idx: " *esm-next-bound-val-idx* ", val: " (vector-ref *esm-bound-vals* *esm-next-bound-val-idx*))
  (set! *esm-next-bound-val-idx* (- *esm-next-bound-val-idx* 1)))

(define (esm/bounded-value reg-idx)
  [assert (reg-idx) (< reg-idx *esm-next-bound-val-idx*)]
  (vector-ref *esm-bound-vals* reg-idx))

(define (mk-esm-primitive-action proc place-provider)
  (esm/add-cmd! (lambda ()
                  (try
                   (begin
                     (proc)
                     (esm/inc-pc!))
                   (lambda (e p m o)
                     (cond 
                      ((eq? p 'esm-action-failure)
                       (e (esm/backtrack!)))
                      ((or (eq? p 'esm-delay-action) (eq? p 'esm-weak-delay-action))
                       (e (esm/delay-action! proc place-provider)))
                      (else (error p m o))))))))

(define (mk-esm-noop-action)
  *esm-next-bound-val-idx*)

(define (mk-esm-execute-delayed-actions-action)
  (esm/add-cmd! (lambda ()
                  (let ((result (esm/execute-delayed-actions #t)))
                    (cond
                     ((eq? result 'succeeded)
                      (esm/inc-pc!))
                     (else
                      [assert (result) (eq? result 'failed)]
                      (esm/backtrack!)))))))

(define (mk-esm-seq-action . stmt-procs)
  (mk-esm-seq-action* stmt-procs))

(define (mk-esm-seq-action* stmt-procs)
  (let ((idx *esm-next-code-idx*))
    (for-each (lambda (proc)
                (proc))
              stmt-procs)
    idx))

(define (mk-esm-main-action stmt-proc)
  (let ((idx *esm-next-code-idx*))
    (stmt-proc)
    (esm/add-cmd! 'succeeded)
    idx))

(define (mk-goto-cmd-at! code-idx new-pc)
  (vector-set! *esm-code* code-idx (lambda ()
                                     (set! *esm-pc* new-pc))))

(define (esm/push-backtrack-entry! next-alt-pc)
  (when (>= *esm-backtrack-stack-top-idx* (vector-length *esm-backtrack-stack*))
    (set! *esm-backtrack-stack* (expand-vect! *esm-backtrack-stack* mk-empty-backtrack-entry)))
  (let ((next-backtrack-entry (vector-ref *esm-backtrack-stack* *esm-backtrack-stack-top-idx*)))
    (set! *esm-backtrack-stack-top-idx* (+ *esm-backtrack-stack-top-idx* 1))
    (esm-backtrack-entry/set-pc! next-backtrack-entry next-alt-pc)
    (esm-backtrack-entry/set-saved-bound-vals! next-backtrack-entry (esm/save-bound-vals))
    (esm-backtrack-entry/set-trail-stack-top-idx! next-backtrack-entry *esm-trail-stack-top-idx*)
    (esm-backtrack-entry/set-delayed-actions! next-backtrack-entry *esm-delayed-actions*)))

(define (mk-choice-cmd-at! code-idx next-alt-pc)
  (vector-set! *esm-code* code-idx (lambda ()
                                     (esm/push-backtrack-entry! next-alt-pc)
                                     (esm/inc-pc!))))
                                     

(define (esm/save-cmd-place)
  (check-esm-code)
  (let ((idx *esm-next-code-idx*))
    (set! *esm-next-code-idx* (+ *esm-next-code-idx* 1))
    idx))

(define (mk-esm-choice-action . stmt-procs)
  (mk-esm-choice-action* stmt-procs))

(define (mk-esm-choice-action* stmt-procs)
  (let ((result-idx *esm-next-code-idx*))
    (if (null? stmt-procs)
      (esm/add-cmd! (lambda () (esm/backtrack!)))
      (let ((goto-end-locs '()))
        (let loop ((stmt-procs stmt-procs))
          (unless (null? stmt-procs)
            (let* ((curr-stmt-proc (car stmt-procs))
                   (last? (null? (cdr stmt-procs)))
                   (first-idx (if (not last?) (esm/save-cmd-place)))
                   (_ (curr-stmt-proc))
                   (last-idx (if (not last?) (esm/save-cmd-place))))
              (when (not last?)
                (mk-choice-cmd-at! first-idx *esm-next-code-idx*)
                (push! last-idx goto-end-locs)))
            (loop (cdr stmt-procs))))
        (for-each (lambda (idx)
                    (mk-goto-cmd-at! idx *esm-next-code-idx*))
                  goto-end-locs)))
    result-idx))

(define (mk-esm-random-choice-action . stmt-procs)
  (mk-esm-random-choice-action* stmt-procs))

(define (mk-esm-random-choice-action* stmt-procs)
  (if (null? stmt-procs)
    (esm/add-cmd! (lambda () (esm/backtrack!)))
    (let* ((result *esm-next-code-idx*)
           (loc-vector (make-vector (length stmt-procs)))
           (loc-vector-len (vector-length loc-vector))
           (init-loc-idx 0)
           (loc-idx 0)
           (setup-loc (esm/add-cmd! (lambda ()
                                      (let ((prev-init-loc-idx init-loc-idx))
                                        (set! init-loc-idx (rand loc-vector-len))
                                        (esm/add-trail! (lambda () (set! init-loc-idx prev-init-loc-idx)))
                                        (set! loc-idx init-loc-idx)
                                        (esm/inc-pc!)))))
           (choice-loc *esm-next-code-idx*)
           (choice-loc2 (esm/add-cmd! (lambda ()
                                        (set! *esm-pc* (vector-ref loc-vector loc-idx))
                                        (let ((next-idx (+ loc-idx 1)))
                                          (when (= next-idx loc-vector-len)
                                            (set! next-idx 0))
                                          (unless (= next-idx init-loc-idx)
                                            (esm/push-backtrack-entry! choice-loc)
                                            (esm/add-trail! (lambda () (set! loc-idx next-idx)))))))))
      [assert (choice-loc choice-loc2) (= choice-loc2 choice-loc)]
      (let loop ((stmt-procs stmt-procs)
                 (goto-end-locs '())
                 (idx 0))
        (cond
         ((null? stmt-procs)
          ;; (print loc-vector)
          ;; (breakpoint "mk-esm-random-choice-action*" (loc-vector goto-end-locs idx) #t)
          (for-each (lambda (goto-loc)
                      (mk-goto-cmd-at! goto-loc *esm-next-code-idx*))
                    goto-end-locs)
          result)
         (else
          (let ((stmt-proc (car stmt-procs)))
            (vector-set! loc-vector idx (stmt-proc))
            (loop (cdr stmt-procs)
                  (cons (esm/save-cmd-place)
                        goto-end-locs)
                  (+ idx 1)))))))))

(define (mk-esm-multi-choice-action values-or-proc stmt-proc)
  (mk-esm-multi-choice-action-core values-or-proc stmt-proc #f #f))

(define (mk-esm-multi-random-choice-action values-or-proc stmt-proc)
  (mk-esm-multi-choice-action-core values-or-proc stmt-proc #t #f))

(define (mk-esm-lazy-multi-choice-action values-or-proc stmt-proc)
  (mk-esm-multi-choice-action-core values-or-proc stmt-proc #f #t))

(define (mk-esm-lazy-multi-random-choice-action values-or-proc stmt-proc)
  (mk-esm-multi-choice-action-core values-or-proc stmt-proc #t #t))

(define (mk-esm-multi-choice-action-core values-or-proc stmt-proc random? lazy?)
  (bind-exit (exit)
    (let* ((result *esm-next-code-idx*)
           (value-vector (if lazy? 
                           #unspecified 
                           (cond
                            ((list? values-or-proc)
                             (when (null? values-or-proc)
                               (esm/add-cmd! (lambda () (esm/backtrack!)))
                               (exit result))
                             (list->vector values-or-proc))
                            (else
                             [assert (values-or-proc) (procedure? values-or-proc)]
                             (let ((values (values-or-proc)))
                               (when (null? values)
                                 (esm/add-cmd! (lambda () (esm/backtrack!)))
                                 (exit result))
                               (list->vector values))))))
           (value-vector-len (if lazy? 
                               #unspecified 
                               (vector-length value-vector)))
           (init-idx 0)
           (curr-idx 0)
           (mk-vect-loc (when lazy?
                          (esm/add-cmd! 
                           (lambda ()
                             [assert (values-or-proc) (procedure? values-or-proc)]
                             (let ((values (values-or-proc)))
                               (cond
                                ((null? values)
                                 (esm/backtrack!))
                                (else
                                 (set! value-vector (list->vector values))
                                 (set! value-vector-len (vector-length value-vector))
                                 (esm/inc-pc!))))))))
           (setup-loc (esm/add-cmd! (lambda ()
                                      (cond
                                       (random?
                                        (let ((prev-init-idx init-idx))
                                          (set! init-idx (rand value-vector-len))
                                          (esm/add-trail! (lambda () (set! init-idx prev-init-idx)))))
                                       (else
                                        (set! init-idx 0)))
                                      (set! curr-idx init-idx)
                                      (esm/inc-pc!))))
           (choice-loc *esm-next-code-idx*)
           (choice-loc2 (esm/add-cmd! (lambda ()
                                        ;; (print "curr-idx: " curr-idx ", init-idx: " init-idx)
                                        (let ((curr-val (vector-ref value-vector curr-idx))
                                              (next-idx (+ curr-idx 1)))
                                          (when (= next-idx value-vector-len)
                                            (set! next-idx 0))
                                          (unless (= next-idx init-idx)
                                            (esm/push-backtrack-entry! choice-loc)
                                            (esm/add-trail! (lambda () 
                                                              (set! curr-idx next-idx))))
                                          (esm/set-bounded-val! curr-val))
                                        (esm/inc-pc!))))
           (prog-loc (stmt-proc))
           (unset-loc (esm/add-cmd! (lambda ()
                                      (esm/unset-bounded-val!)
                                      (esm/inc-pc!)))))
      [assert (choice-loc choice-loc2) (= choice-loc2 choice-loc)]
      result)))

(define (mk-esm-multi-seq-action values stmt-proc)
  (let ((result *esm-next-code-idx*))
    (if (null? values)
      result ;; do nothing
      (let* ((value-vect (list->vector values))
             (value-vect-len (vector-length value-vect))
             (value-idx 0)
             (_ (esm/add-cmd! (lambda ()
                                ;; reset idx
                                (set! value-idx 0)
                                (esm/inc-pc!))))
             (begin-loop-loc *esm-next-code-idx*))
        (esm/add-cmd! (lambda ()
                        (esm/set-bounded-val! (vector-ref value-vect value-idx))
                        (esm/inc-pc!)))
        (stmt-proc)
        (esm/add-cmd! (lambda ()
                        (esm/unset-bounded-val!)
                        (let ((old-value-idx value-idx))
                          (set! value-idx (+ value-idx 1))
                          (esm/add-trail! (lambda () (set! value-idx old-value-idx)))) 
                        (if (< value-idx value-vect-len)
                          (set! *esm-pc* begin-loop-loc)
                          (esm/inc-pc!))))
        result))))

(define (mk-esm-cond-action pred stmt-proc)
  (let* ((result-idx *esm-next-code-idx*)
         (begin-proc-loc (+ result-idx 2))
         (_ (esm/add-cmd! (lambda ()
                            (if (pred)
                              (set! *esm-pc* begin-proc-loc)
                              (esm/inc-pc!)))))
         (goto-loc (esm/save-cmd-place))
         (_ (stmt-proc)))
    (mk-goto-cmd-at! goto-loc *esm-next-code-idx*)
    result-idx))

;; Closes (backtracks) the current execution path.
;; This function is used to implement primitive actions.
(define (esm/fail)
  (error 'esm-action-failure #unspecified #unspecified))

(define (mk-esm-guard-action pred place-provider)
  (mk-esm-primitive-action (lambda ()
                             (unless (pred)
                               (esm/fail)))
                           place-provider))

;; Delay the execution of the action.
;; This function is used to implement primitive actions.
(define (esm/delay)
  (error 'esm-delay-action #unspecified #unspecified))

;; See comment regarding weak-delays in sal-esm-state.
;; Weak-delays were introduced to solve a bug related
;; to delayed actions
(define (esm/weak-delay)
  (error 'esm-weak-delay-action #unspecified #unspecified))

(define (esm/for-each-alternative start-pc proc)
  (let loop ((r (esm/execute-action start-pc)))
    (when r
      (proc)
      (loop (esm/next-alternative)))))

(define (esm/for-each-k-firt-alternatives start-pc k proc)
  (let loop ((r (esm/execute-action start-pc))
             (i 1))
    (when (and r (< i k))
      (proc)
      (loop (esm/next-alternative) (+ i 1)))))
