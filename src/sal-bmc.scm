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

(module sal-bmc
        (include "sal.sch")
        (import sat-bmc-context sal-assertion sal-api sal-module sal-expression
                sat-boolean-ics-bmc-context sat-boolean-zchaff-bmc-context 
                sat-boolean-grasp-bmc-context sat-boolean-siege-bmc-context 
                sat-boolean-berkmin-bmc-context sat-boolean-smt-bmc-context
		sat-boolean-yices-bmc-context sat-boolean-lingeling-bmc-context
		sat-boolean-minisat-bmc-context sat-boolean-yices2-bmc-context
		sat-boolean-kissat-bmc-context)
        (export (sal-bmc/add-solver! solver-id make-bmc-context-proc)
                (sal-bmc/create-assertion-lemmas lemma-str-list main-assertion-name bool-assertion)
                (sal-bmc/find-trace module depth solver-id . acyclic?)
                (sal-bmc/find-path-from-initial-state module to-expr depth solver-id)
                (sal-bmc/find-path module from-expr to-expr depth solver-id)
                (sal-bmc/extend-path path module goal depth solver-id)
                (sal-bmc/extend-path+ path module goal starting-at depth solver-id)
                (sal-bmc/invariant module-models from-depth to-depth acyclic? single-step? solver-id)
                (sal-bmc/liveness module-models depth single-step? solver-id)
                (sal-bmc/k-induction module-models depth acyclic? lemmas solver-id))
        )

(define *solvers* `((yices . ,make-sat-boolean-yices-bmc-context)
		    (yices2 . ,make-sat-boolean-yices2-bmc-context)
                    (ics . ,make-sat-boolean-ics-bmc-context)
                    (zchaff . ,make-sat-boolean-zchaff-bmc-context)
                    (grasp . ,make-sat-boolean-grasp-bmc-context)
                    (siege . ,make-sat-boolean-siege-bmc-context)
                    (berkmin . ,make-sat-boolean-berkmin-bmc-context)
                    (smt . ,make-sat-boolean-smt-bmc-context)
		    (lingeling . ,make-sat-boolean-lingeling-bmc-context)
		    (minisat . ,make-sat-boolean-minisat-bmc-context)
		    (kissat . ,make-sat-boolean-kissat-bmc-context)))

(define-api (sal-bmc/add-solver! solver-id  make-bmc-context-proc)
  (push! (cons solver-id make-bmc-context-proc) *solvers*))

(define (sal-bmc/create-assertion-lemmas lemma-str-list main-assertion-name bool-assertion)
  (let ((process-proc (lambda (lemma)
                        (let* ((bool-flat-module (slot-value bool-assertion :module))
                               (flat-module (sal-derived-flat-module/original-flat-module bool-flat-module))
                               (lemma-module (slot-value lemma :module))
                               (lemma-expr (slot-value lemma :expr))
                               (new-lemma-expr (sal-module/rebind-expr lemma-module lemma-expr flat-module))
                               (lemma-inv (car (sal-application/argument-list new-lemma-expr)))
                               (bool-lemma-body (make-boolean-state-expression-core lemma-inv bool-flat-module)))
                          (bmc/check-if-valid-lemma bool-flat-module bool-lemma-body)
                          bool-lemma-body))))
    (bmc/create-assertion-lemmas-core lemma-str-list main-assertion-name process-proc)))

(define (solver-id->make-proc id)
  (cond
   ((assq id *solvers*) =>
    cdr)
   (else
    (sign-error "Unknown solver identifier `~a'. Available solvers: ~a" id (map car *solvers*)))))

(define (sal-bmc/find-trace module depth solver-id . acyclic?)
  (let ((acyclic? (optional-arg acyclic? #f)))
    (bmc/find-trace-core module depth acyclic? (solver-id->make-proc solver-id))))

(define (sal-bmc/extend-path path module goal depth solver-id)
  (bmc/extend-path-core path module goal 0 depth (solver-id->make-proc solver-id)))

(define (sal-bmc/extend-path+ path module goal starting-at depth solver-id)
  (bmc/extend-path-core path module goal starting-at depth (solver-id->make-proc solver-id)))

(define (sal-bmc/find-path-from-initial-state module to-expr depth solver-id)
  (sal-bmc/find-path module (slot-value module :initialization) to-expr depth solver-id))

(define (sal-bmc/find-path module from-expr to-expr depth solver-id)
  (bmc/find-path-core module from-expr to-expr depth (solver-id->make-proc solver-id)))

(define (sal-bmc/invariant module-models from-depth to-depth acyclic? single-step? solver-id)
  (bmc/invariant-core module-models from-depth to-depth acyclic? single-step? (solver-id->make-proc solver-id)))

(define (sal-bmc/liveness module-models depth single-step? solver-id)
  (bmc/liveness-core module-models depth single-step? (solver-id->make-proc solver-id)))

(define (sal-bmc/k-induction module-models depth acyclic? lemmas solver-id)
  (bmc/k-induction-core module-models depth acyclic? lemmas (solver-id->make-proc solver-id)))
