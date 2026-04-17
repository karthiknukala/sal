;;
;; SAL 3.3, Copyright (C) 2026, SRI International.  All Rights Reserved.
;;
;; SAL is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;

(module cdr-pdkind
        (include "sal.sch")
        (import utility runtime sal-expression sal-ast-eq sal-ast-simplify sal-assertion sal-pp
                cdr-solver cdr-frames sal-inf-bmc)
        (export <cdr-pdkind-obligation>
                (make-cdr-pdkind-obligation f-fwd target-cube target-expr distance score refined parent edge-length)
                (cdr-pdkind-obligation/f-fwd obligation)
                (cdr-pdkind-obligation/target-cube obligation)
                (cdr-pdkind-obligation/target-expr obligation)
                (cdr-pdkind-obligation/distance obligation)
                (cdr-pdkind-obligation/score obligation)
                (cdr-pdkind-obligation/refined obligation)
                (cdr-pdkind-obligation/parent obligation)
                (cdr-pdkind-obligation/edge-length obligation)
                (cdr-pdkind/check-safety assertion solver max-frame)))

(define-class <cdr-pdkind-obligation> ()
  (:f-fwd :target-cube :target-expr :distance :score :refined :parent :edge-length))

(define (make-cdr-pdkind-obligation f-fwd target-cube target-expr distance score refined parent edge-length)
  (make-instance <cdr-pdkind-obligation>
                 :f-fwd f-fwd
                 :target-cube target-cube
                 :target-expr target-expr
                 :distance distance
                 :score score
                 :refined refined
                 :parent parent
                 :edge-length edge-length))

(define (cdr-pdkind-obligation/f-fwd obligation)
  (slot-value obligation :f-fwd))

(define (cdr-pdkind-obligation/target-cube obligation)
  (slot-value obligation :target-cube))

(define (cdr-pdkind-obligation/target-expr obligation)
  (slot-value obligation :target-expr))

(define (cdr-pdkind-obligation/distance obligation)
  (slot-value obligation :distance))

(define (cdr-pdkind-obligation/score obligation)
  (slot-value obligation :score))

(define (cdr-pdkind-obligation/refined obligation)
  (slot-value obligation :refined))

(define (cdr-pdkind-obligation/parent obligation)
  (slot-value obligation :parent))

(define (cdr-pdkind-obligation/edge-length obligation)
  (slot-value obligation :edge-length))

(define *pdkind-score-unit* 1000)

(define (pdkind/initial-score)
  *pdkind-score-unit*)

(define (pdkind/decrease-score score)
  (max 1 (quotient (* score 9) 10)))

(define (pdkind/next-frame-score score)
  (+ (quotient score 2) (quotient *pdkind-score-unit* 2)))

(define (sal-expr->string expr)
  (with-output-to-string
    (lambda ()
      (sal/pp expr))))

(define (cube-satisfies-cube? candidate target)
  (or (not target)
      (for-all
       (lambda (target-binding)
         (let ((candidate-binding
                (find (lambda (candidate-binding)
                        (eq? (car candidate-binding) (car target-binding)))
                      (cdr-cube/bindings candidate))))
           (and candidate-binding
                (sal-ast/equivalent? (cdr candidate-binding)
                                     (cdr target-binding)))))
       (cdr-cube/bindings target))))

(define (pdkind/obligation-equivalent? obligation1 obligation2)
  (and (sal-ast/equivalent? (cdr-pdkind-obligation/f-fwd obligation1)
                            (cdr-pdkind-obligation/f-fwd obligation2))
       (let ((cube1 (cdr-pdkind-obligation/target-cube obligation1))
             (cube2 (cdr-pdkind-obligation/target-cube obligation2)))
         (cond
          ((and cube1 cube2)
           (cdr-cube/equivalent? cube1 cube2))
          ((or cube1 cube2)
           #f)
          (else
           (sal-ast/equivalent? (cdr-pdkind-obligation/target-expr obligation1)
                                (cdr-pdkind-obligation/target-expr obligation2)))))))

(define (pdkind/obligation-tie-breaker obligation)
  (string-append (sal-expr->string (cdr-pdkind-obligation/target-expr obligation))
                 " :: "
                 (sal-expr->string (cdr-pdkind-obligation/f-fwd obligation))))

(define (pdkind/obligation-better? obligation1 obligation2)
  (let ((distance1 (cdr-pdkind-obligation/distance obligation1))
        (distance2 (cdr-pdkind-obligation/distance obligation2))
        (score1 (cdr-pdkind-obligation/score obligation1))
        (score2 (cdr-pdkind-obligation/score obligation2)))
    (cond
     ((< (* distance1 score2) (* distance2 score1))
      #t)
     ((> (* distance1 score2) (* distance2 score1))
      #f)
     ((> score1 score2)
      #t)
     ((< score1 score2)
      #f)
     (else
      (string<? (pdkind/obligation-tie-breaker obligation1)
                (pdkind/obligation-tie-breaker obligation2))))))

(define (pdkind/remove-obligation obligations obligation)
  (remove-if (lambda (candidate)
               (pdkind/obligation-equivalent? candidate obligation))
             obligations))

(define (pdkind/insert-obligation obligations obligation)
  (let loop ((remaining obligations)
             (result '()))
    (cond
     ((null? remaining)
      (reverse! (cons obligation result)))
     ((pdkind/obligation-better? obligation (car remaining))
      (append (reverse! result) (cons obligation remaining)))
     (else
      (loop (cdr remaining) (cons (car remaining) result))))))

(define (pdkind/frame-add obligations obligation)
  (if (find (lambda (candidate)
              (pdkind/obligation-equivalent? candidate obligation))
            obligations)
    obligations
    (cons obligation obligations)))

(define (pdkind/frame-replace obligations old-obligation new-obligation)
  (cons new-obligation (pdkind/remove-obligation obligations old-obligation)))

(define (pdkind/frame-equivalent? frame1 frame2)
  (and (= (length frame1) (length frame2))
       (for-all (lambda (obligation1)
                  (find (lambda (obligation2)
                          (pdkind/obligation-equivalent? obligation1 obligation2))
                        frame2))
                frame1)))

(define (pdkind/make-obligation-retry obligation score)
  (make-cdr-pdkind-obligation (cdr-pdkind-obligation/f-fwd obligation)
                              (cdr-pdkind-obligation/target-cube obligation)
                              (cdr-pdkind-obligation/target-expr obligation)
                              (cdr-pdkind-obligation/distance obligation)
                              score
                              (cdr-pdkind-obligation/refined obligation)
                              (cdr-pdkind-obligation/parent obligation)
                              (cdr-pdkind-obligation/edge-length obligation)))

(define (pdkind/ensure-reachability-level! frame-store solver level)
  (let loop ()
    (when (>= level (cdr-frame-store/num-frames frame-store))
      (let ((store-level (cdr-frame-store/add-frame! frame-store))
            (solver-level (cdr-solver/add-frame! solver)))
        (unless (= store-level solver-level)
          (sign-error "sal-cdr internal error: PDKIND reachability frames diverged."))
        (loop)))))

(define (pdkind/report-lemma prefix level lemma)
  (verbose-message 3 "  ~a at reachability frame F~a." prefix level)
  (when (>= (verbosity-level) 4)
    (verbose-message 4 "    ~a" (sal-expr->string lemma))))

(define (pdkind/select-forward-lemma solver level cube)
  (let* ((fallback (cdr-cube->lemma cube (cdr-solver/flat-module solver)))
         (candidate (cdr-solver/learn-forward-cube solver level cube))
         (simplified (and candidate (sal-ast/simplify candidate)))
         (lemma (or (and simplified (cdr-solver/usable-lemma solver simplified))
                    (and candidate (cdr-solver/usable-lemma solver candidate))
                    fallback)))
    (unless lemma
      (sign-error "sal-cdr PDKIND learned a lemma that could not be encoded for the backend:\n~a"
                  (sal-expr->string (or simplified candidate fallback))))
    (if (sal-expr/true? lemma)
      fallback
      lemma)))

(define (pdkind/add-reachability-lemma! frame-store solver level cube)
  (let ((lemma (pdkind/select-forward-lemma solver level cube)))
    (when (and (> level 0)
               (not (sal-expr/true? lemma))
               (cdr-frame-store/add-lemma! frame-store level lemma))
      (cdr-solver/add-lemma! solver level lemma)
      (pdkind/report-lemma "learned reachability lemma" level lemma))
    lemma))

(define (pdkind/generalize-induction-cube solver cube target-expr)
  (let loop ((idx 0)
             (current cube))
    (if (or (>= idx (cdr-cube/size current))
            (<= (cdr-cube/size current) 1))
      current
      (let ((candidate (cdr-cube/without-binding current idx)))
        (if (equal? (cdr-solver/check-induction-path solver candidate target-expr) "sat")
          (loop idx candidate)
          (loop (+ idx 1) current))))))

(define (pdkind/check-reachable-at frame-store solver level cube)
  (cond
   ((< level 0)
    (values 'unreachable #f))
   ((= level 0)
    (let ((status (cdr-solver/check-initial-cube solver cube)))
      (cond
       ((equal? status "sat")
        (values 'reachable 0))
       ((equal? status "unsat")
        (values 'unreachable #f))
       (else
        (values 'unknown #f)))))
   (else
    (pdkind/ensure-reachability-level! frame-store solver level)
    (let loop ((stack (list (cons level cube))))
      (if (null? stack)
        (values 'unreachable #f)
        (let* ((entry (car stack))
               (k (car entry))
               (current (cdr entry)))
          (cond
           ((= k 0)
            (values 'reachable level))
           (else
            (multiple-value-bind
                (status predecessor)
                (cdr-solver/check-predecessor solver (- k 1) current)
              (cond
               ((equal? status "sat")
                (loop (cons (cons (- k 1) predecessor) stack)))
               ((equal? status "unsat")
                (pdkind/add-reachability-lemma! frame-store solver k current)
                (loop (cdr stack)))
               (else
                (values 'unknown #f))))))))))))

(define (pdkind/check-reachable frame-store solver start end cube)
  (let ((start (max 0 start)))
    (let loop ((k start))
      (cond
       ((> k end)
        (values 'unreachable #f))
       (else
        (multiple-value-bind
            (status witness-k)
            (pdkind/check-reachable-at frame-store solver k cube)
          (cond
           ((eq? status 'reachable)
            (values 'reachable witness-k))
           ((eq? status 'unknown)
            (values 'unknown #f))
           (else
            (loop (+ k 1))))))))))

(define (pdkind/reconstruct-counterexample solver root-expr root-k obligation first-edge-length)
  (let* ((module (cdr-solver/flat-module solver))
         (solver-id (cdr-solver/trace-solver-id solver))
         (path (sal-inf-bmc/find-path-from-initial-state module
                                                         root-expr
                                                         (if (> root-k 0) root-k 1)
                                                         solver-id)))
    (unless path
      (sign-error "sal-cdr PDKIND could not replay the root counterexample state:\n~a"
                  (sal-expr->string root-expr)))
    (let ((path
           (sal-inf-bmc/extend-path+ path
                                     module
                                     (cdr-pdkind-obligation/target-expr obligation)
                                     first-edge-length
                                     first-edge-length
                                     solver-id)))
      (unless path
        (sign-error "sal-cdr PDKIND could not replay the first induction edge into:\n~a"
                    (sal-expr->string (cdr-pdkind-obligation/target-expr obligation))))
      (let loop ((path path)
                 (current obligation))
        (let ((parent (cdr-pdkind-obligation/parent current)))
          (if (not parent)
            path
            (let ((next-path
                   (sal-inf-bmc/extend-path+ path
                                             module
                                             (cdr-pdkind-obligation/target-expr parent)
                                             (cdr-pdkind-obligation/edge-length current)
                                             (cdr-pdkind-obligation/edge-length current)
                                             solver-id)))
              (unless next-path
                (sign-error "sal-cdr PDKIND could not replay a counterexample suffix into:\n~a"
                            (sal-expr->string (cdr-pdkind-obligation/target-expr parent))))
              (loop next-path parent))))))))

(define (cdr-pdkind/check-safety assertion solver max-frame)
  (let* ((property (sal-module-models/invariant-body assertion))
         (bad-expr (make-sal-not property))
         (frame-store (make-cdr-frame-store))
         (root-obligation (make-cdr-pdkind-obligation property
                                                      #f
                                                      bad-expr
                                                      0
                                                      (pdkind/initial-score)
                                                      0
                                                      #f
                                                      0))
         (frame-index 0)
         (frame-depth 1)
         (frame-next-index 1)
         (frame (list root-obligation))
         (queue (list root-obligation))
         (next-obligations '())
         (invalid-root-expr #f)
         (invalid-root-k #f)
         (invalid-obligation #f)
         (invalid-edge-length #f))
    (define (report-obligation prefix obligation)
      (verbose-message 3 "  ~a (distance ~a, score ~a)."
                       prefix
                       (cdr-pdkind-obligation/distance obligation)
                       (cdr-pdkind-obligation/score obligation))
      (when (>= (verbosity-level) 4)
        (verbose-message 4 "    F_fwd: ~a"
                         (sal-expr->string (cdr-pdkind-obligation/f-fwd obligation)))
        (verbose-message 4 "    target: ~a"
                         (sal-expr->string (cdr-pdkind-obligation/target-expr obligation)))))
    (define (queue-add! obligation)
      (set! queue (pdkind/insert-obligation (pdkind/remove-obligation queue obligation)
                                            obligation)))
    (define (frame-add! obligation)
      (set! frame (pdkind/frame-add frame obligation)))
    (define (frame-contains? obligation)
      (find (lambda (candidate)
              (pdkind/obligation-equivalent? candidate obligation))
            frame))
    (define (frame-replace! old-obligation new-obligation)
      (set! frame (pdkind/frame-replace frame old-obligation new-obligation))
      (set! queue (pdkind/remove-obligation queue old-obligation)))
    (define (next-add! obligation)
      (set! next-obligations (pdkind/frame-add next-obligations obligation)))
    (define (register-induction-lemma! lemma)
      (cdr-solver/add-induction-lemma! solver lemma)
      (pdkind/report-lemma "added induction lemma" frame-index lemma))
    (define (forward-obligation score target-cube target-expr parent edge-length)
      (let* ((lemma (pdkind/select-forward-lemma solver frame-index target-cube))
             (obligation (make-cdr-pdkind-obligation lemma
                                                     target-cube
                                                     target-expr
                                                     (+ edge-length
                                                        (cdr-pdkind-obligation/distance parent))
                                                     score
                                                     0
                                                     parent
                                                     edge-length)))
        (if (frame-contains? obligation)
          #f
          (begin
            (frame-add! obligation)
            (register-induction-lemma! lemma)
            (queue-add! obligation)
            (report-obligation "queued forward obligation" obligation)
            obligation))))
    (define (mark-invalid! root-cube root-k obligation first-edge-length)
      (set! invalid-root-expr (cdr-cube->expr root-cube (cdr-solver/flat-module solver)))
      (set! invalid-root-k root-k)
      (set! invalid-obligation obligation)
      (set! invalid-edge-length first-edge-length))
    (define (target-hit? obligation end-cube)
      (let ((target-cube (cdr-pdkind-obligation/target-cube obligation)))
        (or (not target-cube)
            (cube-satisfies-cube? end-cube target-cube))))
    (define (retry-obligation! obligation)
      (let ((retried (pdkind/make-obligation-retry obligation
                                                   (pdkind/decrease-score
                                                    (cdr-pdkind-obligation/score obligation)))))
        (frame-replace! obligation retried)
        (queue-add! retried)))
    (define (push-obligation! obligation)
      (multiple-value-bind
          (status start-cube end-cube)
          (cdr-solver/check-inductive-witness solver
                                              (cdr-pdkind-obligation/f-fwd obligation))
        (cond
         ((equal? status "unsat")
          (next-add! obligation)
          'continue)
         ((not (equal? status "sat"))
          'unknown)
         ((target-hit? obligation end-cube)
          (let* ((generalized-start
                  (pdkind/generalize-induction-cube solver
                                                    start-cube
                                                    (cdr-pdkind-obligation/target-expr obligation)))
                 (reach-start (max 0 (- (+ frame-index 1) frame-depth)))
                 (reach-end frame-index))
            (multiple-value-bind
                (reach-status root-k)
                (pdkind/check-reachable frame-store
                                        solver
                                        reach-start
                                        reach-end
                                        generalized-start)
              (cond
               ((eq? reach-status 'reachable)
                (multiple-value-bind
                    (exact-status exact-root-k)
                    (pdkind/check-reachable frame-store
                                            solver
                                            reach-start
                                            reach-end
                                            start-cube)
                  (cond
                   ((eq? exact-status 'reachable)
                    (mark-invalid! start-cube exact-root-k obligation frame-depth)
                    'invalid)
                   ((eq? exact-status 'unknown)
                    'unknown)
                   (else
                    (forward-obligation (pdkind/initial-score)
                                        generalized-start
                                        (cdr-cube->expr generalized-start
                                                        (cdr-solver/flat-module solver))
                                        obligation
                                        frame-depth)
                    (retry-obligation! obligation)
                    'continue))))
               ((eq? reach-status 'unknown)
                'unknown)
               (else
                (forward-obligation (pdkind/initial-score)
                                    generalized-start
                                    (cdr-cube->expr generalized-start
                                                    (cdr-solver/flat-module solver))
                                    obligation
                                    frame-depth)
                (retry-obligation! obligation)
                'continue)))))
         (else
          (let* ((negated-f-fwd (sal-ast/simplify
                                 (make-sal-not (cdr-pdkind-obligation/f-fwd obligation))))
                 (generalized-start
                  (pdkind/generalize-induction-cube solver
                                                    start-cube
                                                    negated-f-fwd))
                 (reach-start (max 0 (- (+ frame-index 1) frame-depth)))
                 (reach-end frame-index))
            (multiple-value-bind
                (reach-status root-k)
                (pdkind/check-reachable frame-store
                                        solver
                                        reach-start
                                        reach-end
                                        generalized-start)
              (cond
               ((eq? reach-status 'reachable)
                (set! frame-next-index
                      (min frame-next-index
                           (+ root-k frame-depth)))
                (let* ((candidate (sal-ast/simplify
                                   (make-sal-not (cdr-pdkind-obligation/target-expr obligation))))
                       (pushed-f-fwd (or (cdr-solver/usable-lemma solver candidate)
                                         (cdr-cube->lemma (cdr-pdkind-obligation/target-cube obligation)
                                                          (cdr-solver/flat-module solver))))
                       (next-obligation
                        (make-cdr-pdkind-obligation pushed-f-fwd
                                                    (cdr-pdkind-obligation/target-cube obligation)
                                                    (cdr-pdkind-obligation/target-expr obligation)
                                                    (cdr-pdkind-obligation/distance obligation)
                                                    (max (pdkind/initial-score)
                                                         (cdr-pdkind-obligation/distance obligation))
                                                    (cdr-pdkind-obligation/refined obligation)
                                                    (cdr-pdkind-obligation/parent obligation)
                                                    (cdr-pdkind-obligation/edge-length obligation))))
                  (next-add! next-obligation)
                  'continue))
               ((eq? reach-status 'unknown)
                'unknown)
               (else
                (let* ((learned (pdkind/select-forward-lemma solver frame-index generalized-start))
                       (refined-f-fwd
                        (or (cdr-solver/usable-lemma solver
                                                     (sal-ast/simplify
                                                      (make-sal-and* (list (cdr-pdkind-obligation/f-fwd obligation)
                                                                           learned)
                                                                     (cdr-solver/flat-module solver))))
                            learned))
                       (refined
                        (make-cdr-pdkind-obligation refined-f-fwd
                                                    (cdr-pdkind-obligation/target-cube obligation)
                                                    (cdr-pdkind-obligation/target-expr obligation)
                                                    (cdr-pdkind-obligation/distance obligation)
                                                    (pdkind/decrease-score
                                                     (cdr-pdkind-obligation/score obligation))
                                                    (+ 1 (cdr-pdkind-obligation/refined obligation))
                                                    (cdr-pdkind-obligation/parent obligation)
                                                    (cdr-pdkind-obligation/edge-length obligation))))
                  (frame-replace! obligation refined)
                  (register-induction-lemma! learned)
                  (queue-add! refined)
                  'continue)))))))))
    (define (current-frame-valid?)
      (pdkind/frame-equivalent? frame next-obligations))
    (define (load-frame! obligations)
      (set! frame obligations)
      (set! queue '())
      (for-each
       (lambda (obligation)
         (let ((updated
                (make-cdr-pdkind-obligation (cdr-pdkind-obligation/f-fwd obligation)
                                            (cdr-pdkind-obligation/target-cube obligation)
                                            (cdr-pdkind-obligation/target-expr obligation)
                                            (cdr-pdkind-obligation/distance obligation)
                                            (pdkind/next-frame-score
                                             (cdr-pdkind-obligation/score obligation))
                                            (cdr-pdkind-obligation/refined obligation)
                                            (cdr-pdkind-obligation/parent obligation)
                                            (cdr-pdkind-obligation/edge-length obligation))))
           (set! frame (pdkind/frame-replace frame obligation updated))
           (register-induction-lemma! (cdr-pdkind-obligation/f-fwd updated))
           (queue-add! updated)))
       obligations))
    (cdr-solver/reset-induction! solver frame-depth)
    (register-induction-lemma! property)
    (let search-loop ()
      (set! frame-next-index (+ frame-index frame-depth))
      (verbose-message 1 "sal-cdr: PDKIND frame ~a with induction depth ~a (~a obligations)."
                       frame-index
                       frame-depth
                       (length frame))
      (let frame-loop ()
        (cond
         (invalid-obligation
          (values 'invalid
                  (pdkind/reconstruct-counterexample solver
                                                     invalid-root-expr
                                                     invalid-root-k
                                                     invalid-obligation
                                                     invalid-edge-length)))
         ((null? queue)
          (cond
           ((current-frame-valid?)
            (verbose-message 1 "sal-cdr PDKIND proved an inductive invariant at frame ~a." frame-index)
            (values 'valid #f))
           ((>= frame-index max-frame)
            (values 'unknown #f))
           (else
            (set! frame-index frame-next-index)
            (set! frame-depth (+ frame-depth 1))
            (set! next-obligations (reverse! next-obligations))
            (cdr-solver/reset-induction! solver frame-depth)
            (load-frame! next-obligations)
            (set! next-obligations '())
            (search-loop))))
         (else
          (let ((obligation (car queue)))
            (set! queue (cdr queue))
            (let ((result (push-obligation! obligation)))
              (cond
               ((eq? result 'continue)
                (frame-loop))
               ((eq? result 'invalid)
               (frame-loop))
               (else
                (values 'unknown #f)))))))))))
