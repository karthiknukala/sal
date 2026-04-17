;;
;; SAL 3.3, Copyright (C) 2026, SRI International.  All Rights Reserved.
;;
;; SAL is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;

(module sal-cdr
        (include "sal.sch")
        (import utility runtime sal-assertion sal-expression sal-module sal-ast-simplify sal-pp
                smtlib2-interface
                cdr-solver cdr-frames cdr-obligations cdr-pdkind cdr-yices2-smt2)
        (export (sal-cdr/check-safety assertion max-frame pdkind? solver-id)))

(define (sal-expr->string expr)
  (with-output-to-string
    (lambda ()
      (sal/pp expr))))

(define (cdr/mode-name pdkind?)
  (if pdkind? "PDKIND" "PDR"))

(define *cdr-max-block-attempts-per-frontier* 256)

(define (cdr/report-frame-summary frame-store frontier)
  (verbose-message 2 "  exploring frame F~a..." frontier)
  (when (>= (verbosity-level) 3)
    (let loop ((level 1))
      (when (< level (cdr-frame-store/num-frames frame-store))
        (verbose-message 3 "    F~a lemmas: ~a"
                         level
                         (length (cdr-frame-store/frame-lemmas frame-store level)))
        (loop (+ level 1))))))

(define (cdr/report-lemma prefix level lemma)
  (verbose-message 3 "  ~a at F~a." prefix level)
  (when (>= (verbosity-level) 4)
    (verbose-message 4 "    ~a" (sal-expr->string lemma))))

(define (cdr/learn-lemma! frame-store solver max-level lemma)
  (when (and (not (sal-expr/true? lemma))
             (cdr-frame-store/add-lemma! frame-store max-level lemma))
    (cdr-solver/add-lemma! solver max-level lemma)
    (cdr/report-lemma "learned lemma" max-level lemma)))

(define (cdr/generalize-cube solver level cube)
  (if (<= level 0)
    cube
    (let loop ((idx 0)
               (current cube))
      (if (or (>= idx (cdr-cube/size current))
              (<= (cdr-cube/size current) 1))
        current
        (let ((candidate (cdr-cube/without-binding current idx)))
          (when (>= (verbosity-level) 5)
            (verbose-message 5 "sal-cdr: trying cube generalization candidate at F~a: ~a"
                             level
                             (sal-expr->string (cdr-cube->expr candidate
                                                               (cdr-solver/flat-module solver)))))
          (multiple-value-bind
              (status)
              (cdr-solver/check-predecessor-status solver (- level 1) candidate)
            (when (>= (verbosity-level) 5)
              (verbose-message 5 "sal-cdr: generalization candidate status: ~a" status))
            (cond
             ((equal? status "unsat")
              (loop idx candidate))
             (else
              (loop (+ idx 1) current)))))))))

(define (cdr/usable-blocking-lemma solver level lemma)
  (and lemma
       (let* ((simplified (sal-ast/simplify lemma))
              (usable (or (cdr-solver/usable-lemma solver simplified)
                          (cdr-solver/usable-lemma solver lemma))))
         (and usable
              (not (sal-expr/true? usable))
              (or (<= level 0)
                  (cdr-solver/check-lemma-propagation solver (- level 1) usable))
              usable))))

(define (cdr/select-blocking-lemma solver level cube)
  (when (>= (verbosity-level) 5)
    (verbose-message 5 "sal-cdr: selecting a blocking lemma at F~a..." level))
  (let* ((generalized-cube (cdr/generalize-cube solver level cube))
         (_ (when (>= (verbosity-level) 5)
              (verbose-message 5 "sal-cdr: generalized cube: ~a"
                               (sal-expr->string
                                (cdr-cube->expr generalized-cube
                                                (cdr-solver/flat-module solver))))))
         (fallback-lemma (cdr-cube->lemma generalized-cube (cdr-solver/flat-module solver)))
         (_ (when (>= (verbosity-level) 5)
              (verbose-message 5 "sal-cdr: fallback lemma: ~a"
                               (sal-expr->string fallback-lemma))))
         (forward-raw (cdr-solver/learn-forward-cube solver
                                                     level
                                                     cube))
         (_ (when (and forward-raw (>= (verbosity-level) 5))
              (verbose-message 5 "sal-cdr: forward-learning candidate: ~a"
                               (sal-expr->string forward-raw))))
         (forward-lemma (and forward-raw
                             (cdr/usable-blocking-lemma solver
                                                        level
                                                        forward-raw)))
         (_ (when (and forward-lemma (>= (verbosity-level) 5))
              (verbose-message 5 "sal-cdr: validated forward lemma: ~a"
                               (sal-expr->string forward-lemma))))
         (interpolant-raw (and (> level 0)
                               (cdr-solver/interpolant-for-cube solver
                                                                (- level 1)
                                                                cube)))
         (_ (when (and interpolant-raw (>= (verbosity-level) 5))
              (verbose-message 5 "sal-cdr: interpolant candidate: ~a"
                               (sal-expr->string interpolant-raw))))
         (interpolant-lemma (and interpolant-raw
                                 (cdr/usable-blocking-lemma solver
                                                            level
                                                            interpolant-raw)))
         (_ (when (and interpolant-lemma (>= (verbosity-level) 5))
              (verbose-message 5 "sal-cdr: validated interpolant lemma: ~a"
                               (sal-expr->string interpolant-lemma))))
         (candidate-lemma (or forward-lemma
                              interpolant-lemma
                              fallback-lemma))
         (_ (when (>= (verbosity-level) 5)
              (verbose-message 5 "sal-cdr: candidate lemma: ~a"
                               (sal-expr->string candidate-lemma))))
         (simplified-candidate (sal-ast/simplify candidate-lemma))
         (_ (when (>= (verbosity-level) 5)
              (verbose-message 5 "sal-cdr: simplified candidate lemma: ~a"
                               (sal-expr->string simplified-candidate))))
         (lemma (or forward-lemma
                    interpolant-lemma
                    (cdr-solver/usable-lemma solver simplified-candidate)
                    (cdr-solver/usable-lemma solver candidate-lemma)
                    fallback-lemma)))
    (unless lemma
      (sign-error "sal-cdr ~a learned a lemma that could not be encoded for the backend:\n~a"
                  "PDR"
                  (sal-expr->string simplified-candidate)))
    (values generalized-cube
            (if (sal-expr/true? lemma)
              fallback-lemma
              lemma))))

(define (cdr/block-obligation solver frame-store root-obligation)
  (let ((queue (make-cdr-obligation-queue)))
    (cdr-obligation-queue/insert! queue root-obligation)
    (let loop ()
      (if (cdr-obligation-queue/empty? queue)
        (values 'blocked #f)
        (let* ((obligation (cdr-obligation-queue/pop! queue))
               (level (cdr-obligation/level obligation))
               (cube (cdr-obligation/cube obligation)))
          (when (>= (verbosity-level) 4)
            (verbose-message 4 "sal-cdr: processing obligation at level ~a" level)
            (verbose-message 4 "  cube: ~a" (sal-expr->string (cdr-cube->expr cube (cdr-solver/flat-module solver)))))
          (cond
           ((= level 0)
            (verbose-message 2 "  reached a level-0 witness obligation.")
            (values 'counterexample obligation))
           (else
            (multiple-value-bind
                (status predecessor)
                (cdr-solver/check-predecessor solver (- level 1) cube)
              (cond
               ((equal? status "sat")
                (when (>= (verbosity-level) 4)
                  (verbose-message 4 "sal-cdr: predecessor found at level ~a" (- level 1))
                  (verbose-message 4 "  predecessor cube: ~a"
                                   (sal-expr->string (cdr-cube->expr predecessor
                                                                     (cdr-solver/flat-module solver)))))
                (cdr-obligation-queue/insert! queue obligation)
                (cdr-obligation-queue/insert! queue
                                              (make-cdr-obligation predecessor
                                                                   (- level 1)
                                                                   obligation))
               (loop))
               ((equal? status "unsat")
                (multiple-value-bind
                    (_generalized-cube lemma)
                    (cdr/select-blocking-lemma solver level cube)
                  (when (>= (verbosity-level) 4)
                    (verbose-message 4 "sal-cdr: blocking cube at level ~a with lemma:" level)
                    (verbose-message 4 "  ~a" (sal-expr->string lemma)))
                  (cdr/learn-lemma! frame-store solver level lemma)
                  (loop)))
               (else
                (values 'unknown #f)))))))))))

(define (cdr/propagate-lemmas! solver frame-store frontier)
  (let propagate-levels ((level 1))
    (when (< level frontier)
      (for-each
       (lambda (lemma)
         (unless (cdr-frame-store/frame-contains? frame-store (+ level 1) lemma)
           (when (cdr-solver/check-lemma-propagation solver level lemma)
             (cdr-frame-store/add-lemma-at-level! frame-store (+ level 1) lemma)
             (cdr-solver/add-lemma-at-level! solver (+ level 1) lemma)
             (cdr/report-lemma "propagated lemma" (+ level 1) lemma))))
       (cdr-frame-store/frame-lemmas frame-store level))
      (propagate-levels (+ level 1))))
  (let find-convergence ((level 1))
    (cond
     ((>= level frontier)
      #f)
     ((cdr-frame-store/converged? frame-store level)
      level)
     (else
      (find-convergence (+ level 1))))))

(define (cdr/normalize-smtlib2-backend!)
  ;; Pull in environment overrides so the shared SMT-LIB2 translator sees the
  ;; same settings as the rest of SAL. The sal-cdr backend resolves Yices2
  ;; separately because users commonly point the generic SMT-LIB2 command at Z3.
  (verbose-message 1 "sal-cdr: loading SMT-LIB2 backend configuration...")
  (let ((cmd (sal/smtlib2-command)))
    (unless (string? cmd)
      (sign-error "sal-cdr expected the SMT-LIB2 command to be a string, received ~a." cmd))
    (verbose-message 1 "sal-cdr: configured generic SMT-LIB2 command: ~a" cmd)))

(define (make-cdr-search-solver assertion pdkind? solver-id)
  (unless (eq? solver-id 'yices2)
    (sign-error "sal-cdr v1 only supports --solver=yices2."))
  (cdr/normalize-smtlib2-backend!)
  (verbose-message 1 "sal-cdr: creating the Yices2 backend...")
  (let ((solver (make-cdr-yices2-solver assertion pdkind?)))
    (let ((caps (cdr-solver/capabilities solver)))
      (unless (cdr-solver-capabilities/incremental? caps)
        (cdr-solver/close! solver)
        (sign-error "sal-cdr requires an incremental solver backend."))
      (unless (cdr-solver-capabilities/models? caps)
        (cdr-solver/close! solver)
        (sign-error "sal-cdr requires model generation support."))
      (when (and pdkind? (not (cdr-solver-capabilities/interpolants? caps)))
        (cdr-solver/close! solver)
        (sign-error "sal-cdr -i requires interpolant support."))
      solver)))

(define (cdr/invalid-result solver obligation)
  (verbose-message 1 "sal-cdr: reconstructing a concrete counterexample trace...")
  (values 'invalid
          (cdr-obligation/reconstruct-counterexample obligation
                                                     (cdr-solver/flat-module solver)
                                                     (cdr-solver/trace-solver-id solver))))

(define (cdr/saturate-frontier! solver frame-store frontier)
  (let loop ((attempts 0))
    (multiple-value-bind
        (status bad-cube)
        (cdr-solver/check-bad-at-frame solver frontier)
      (cond
       ((equal? status "sat")
        (if (>= attempts *cdr-max-block-attempts-per-frontier*)
          (begin
            (verbose-message 1 "sal-cdr reached the frontier blocking budget at F~a; returning unknown." frontier)
            (values 'unknown #f))
          (multiple-value-bind
              (block-status witness)
              (cdr/block-obligation solver
                                    frame-store
                                    (make-cdr-obligation bad-cube frontier))
            (cond
             ((eq? block-status 'blocked)
              (loop (+ attempts 1)))
             ((eq? block-status 'counterexample)
              (values 'invalid witness))
             (else
              (values 'unknown #f))))))
       ((equal? status "unknown")
        (values 'unknown #f))
       (else
        (values 'clear #f))))))

(define (cdr/explore-frontiers solver frame-store frontier max-frame)
  (if (> frontier max-frame)
    (values 'unknown #f)
    (begin
      (cdr/report-frame-summary frame-store frontier)
      (multiple-value-bind
          (frontier-status witness)
          (cdr/saturate-frontier! solver frame-store frontier)
        (cond
         ((eq? frontier-status 'invalid)
          (cdr/invalid-result solver witness))
         ((eq? frontier-status 'unknown)
          (values 'unknown #f))
         ((= frontier max-frame)
          (values 'unknown #f))
         (else
          (let ((store-level (cdr-frame-store/add-frame! frame-store))
                (solver-level (cdr-solver/add-frame! solver)))
            (unless (= store-level solver-level)
              (sign-error "sal-cdr internal error: frame store and solver frontier diverged."))
            (cond
             ((cdr/propagate-lemmas! solver frame-store solver-level) =>
              (lambda (converged-level)
                (verbose-message 1 "sal-cdr converged at F~a." converged-level)
                (values 'valid #f)))
             (else
              (cdr/explore-frontiers solver frame-store solver-level max-frame))))))))))

(define (sal-cdr/check-safety assertion max-frame pdkind? solver-id)
  (unless (sal-module-models/invariant? assertion)
    (sign-error "sal-cdr supports only safety/invariant properties in v1."))
  (let ((solver #f))
    (unwind-protect
     (begin
       (set! solver (make-cdr-search-solver assertion pdkind? solver-id))
       (verbose-message 1 "sal-cdr mode: ~a" (cdr/mode-name pdkind?))
       (verbose-message 1 "sal-cdr solver: ~a" (cdr-solver/solver-description solver))
       (verbose-message 1 "sal-cdr: checking the initial frame against Bad...")
       (multiple-value-bind
          (initial-status initial-cube)
          (cdr-solver/check-initial-bad solver)
         (cond
          ((equal? initial-status "sat")
           (cdr/invalid-result solver (make-cdr-obligation initial-cube 0)))
          ((equal? initial-status "unknown")
           (values 'unknown #f))
          (else
           (verbose-message 1 "sal-cdr: checking one-step transitions from I into Bad...")
           (multiple-value-bind
               (step-status step-cube)
               (cdr-solver/check-bad-at-frame solver 0)
             (cond
              ((equal? step-status "sat")
               (cdr/invalid-result solver (make-cdr-obligation step-cube 0)))
              ((equal? step-status "unknown")
              (values 'unknown #f))
              (else
               (if pdkind?
                 (cdr-pdkind/check-safety assertion solver max-frame)
                 (cdr/explore-frontiers solver
                                        (make-cdr-frame-store)
                                        1
                                        max-frame)))))))))
     (if solver
       (cdr-solver/close! solver)
       #unspecified))))
