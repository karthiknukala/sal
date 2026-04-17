;;
;; SAL 3.3, Copyright (C) 2026, SRI International.  All Rights Reserved.
;;
;; SAL is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;

(module cdr-obligations
        (include "sal.sch")
        (import utility sal-inf-bmc sal-pp cdr-solver)
        (export <cdr-obligation>
                (make-cdr-obligation cube level . parent)
                (cdr-obligation/cube obligation)
                (cdr-obligation/level obligation)
                (cdr-obligation/parent obligation)
                <cdr-obligation-queue>
                (make-cdr-obligation-queue)
                (cdr-obligation-queue/empty? queue)
                (cdr-obligation-queue/insert! queue obligation)
                (cdr-obligation-queue/pop! queue)
                (cdr-obligation/reconstruct-counterexample obligation module solver-id)))

(define-class <cdr-obligation> ()
  (:cube :level :parent))

(define (make-cdr-obligation cube level . parent)
  (make-instance <cdr-obligation>
                 :cube cube
                 :level level
                 :parent (optional-arg parent #f)))

(define (cdr-obligation/cube obligation)
  (slot-value obligation :cube))

(define (cdr-obligation/level obligation)
  (slot-value obligation :level))

(define (cdr-obligation/parent obligation)
  (slot-value obligation :parent))

(define-class <cdr-obligation-queue> ()
  (:obligations))

(define (make-cdr-obligation-queue)
  (make-instance <cdr-obligation-queue> :obligations '()))

(define (cdr-obligation-queue/empty? queue)
  (null? (slot-value queue :obligations)))

(define (cdr-obligation-queue/insert! queue obligation)
  (let loop ((remaining (slot-value queue :obligations))
             (result '()))
    (cond
     ((null? remaining)
      (set-slot-value! queue :obligations (reverse! (cons obligation result))))
     ((<= (cdr-obligation/level (car remaining))
          (cdr-obligation/level obligation))
      (loop (cdr remaining) (cons (car remaining) result)))
     (else
      (set-slot-value! queue :obligations
                       (append (reverse! result) (cons obligation remaining)))))))

(define (cdr-obligation-queue/pop! queue)
  (let ((obligations (slot-value queue :obligations)))
    (and (pair? obligations)
         (begin
           (set-slot-value! queue :obligations (cdr obligations))
           (car obligations)))))

(define (obligation->cube-list obligation)
  (reverse!
   (let loop ((current obligation)
              (result '()))
     (if current
       (loop (cdr-obligation/parent current)
             (cons (cdr-obligation/cube current) result))
       result))))

(define (cdr-obligation/reconstruct-counterexample obligation module solver-id)
  (let ((cube-list (obligation->cube-list obligation)))
    (and (pair? cube-list)
         (let* ((first-goal (cdr-cube->expr (car cube-list) module))
                (path (sal-inf-bmc/find-path-from-initial-state module first-goal 1 solver-id)))
           (when (>= (verbosity-level) 4)
             (verbose-message 4 "sal-cdr: reconstructing with solver ~a" solver-id)
             (verbose-message 4 "sal-cdr: first replay goal:")
             (verbose-message 4 "  ~a"
                              (with-output-to-string
                                (lambda ()
                                  (sal/pp first-goal)))))
           (if (not path)
             (sign-error "sal-cdr found a candidate counterexample state that could not be reached from the initial condition:\n~a"
                         (with-output-to-string
                           (lambda ()
                             (sal/pp first-goal))))
             (let loop ((path path)
                        (remaining (cdr cube-list)))
               (if (null? remaining)
                 path
                 (let* ((goal (cdr-cube->expr (car remaining) module))
                        (_ (when (>= (verbosity-level) 4)
                             (verbose-message 4 "sal-cdr: extending replay with goal:")
                             (verbose-message 4 "  ~a"
                                              (with-output-to-string
                                                (lambda ()
                                                  (sal/pp goal))))))
                        (new-path (sal-inf-bmc/extend-path path
                                                           module
                                                           goal
                                                           1
                                                           solver-id)))
                   (if new-path
                     (loop new-path (cdr remaining))
                     (sign-error "sal-cdr found a candidate counterexample suffix that could not be replayed concretely:\n~a"
                                 (with-output-to-string
                                   (lambda ()
                                     (sal/pp goal)))))))))))))
