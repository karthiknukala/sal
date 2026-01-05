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

(module sal-inf-bmc
        (include "sal.sch")
        (import sat-bmc-context 
                sal-api sal-assertion sal-expression sal-module sal-ast-for-each
                sat-ics-bmc-context sat-svc-bmc-context sat-cvcl-bmc-context
                sat-uclid-bmc-context sat-yices-bmc-context sat-yices2-bmc-context)
        (export (sal-inf-bmc/add-solver! solver-id  make-bmc-context-proc)
                (sal-inf-bmc/create-assertion-lemmas lemma-str-list main-assertion-name bool-assertion)
                (sal-inf-bmc/find-trace module depth solver-id . acyclic?)
                (sal-inf-bmc/find-path-from-initial-state module to-expr depth solver-id)
                (sal-inf-bmc/find-path module from-expr to-expr depth solver-id)
                (sal-inf-bmc/extend-path path module goal depth solver-id)
                (sal-inf-bmc/extend-path+ path module goal starting-at depth solver-id)
                (sal-inf-bmc/invariant module-models from-depth to-depth acyclic? single-step? solver-id)
                (sal-inf-bmc/liveness module-models depth single-step? solver-id)
                (sal-inf-bmc/k-induction module-models depth acyclic? lemmas solver-id))
        )

(define *solvers* `((ics . ,make-sat-ics-bmc-context)
                    (svc . ,make-sat-svc-bmc-context)
                    (cvc . ,make-sat-cvc-bmc-context)
                    (cvcl . ,make-sat-cvcl-bmc-context)
                    (uclid . ,make-sat-uclid-bmc-context)
                    (yices . ,make-sat-yices-bmc-context)
		    (yices2 . ,make-sat-yices2-bmc-context)))

(define (sal-inf-bmc/add-solver! solver-id  make-bmc-context-proc)
  (push! (cons solver-id make-bmc-context-proc) *solvers*))

(define (sal-inf-bmc/create-assertion-lemmas lemma-str-list main-assertion-name flat-assertion)
  (let ((process-proc (lambda (lemma)
                        (let* ((simple-data-flat-module (slot-value flat-assertion :module))
                               (flat-module (sal-derived-flat-module/original-flat-module simple-data-flat-module))
                               (lemma-module (slot-value lemma :module))
                               (lemma-expr (slot-value lemma :expr))
                               (new-lemma-expr (sal-module/rebind-expr lemma-module lemma-expr flat-module))
                               (lemma-inv (car (sal-application/argument-list new-lemma-expr)))
                               (lemma-body (make-simple-data-state-expression-core lemma-inv simple-data-flat-module)))
                          (bmc/check-if-valid-lemma simple-data-flat-module lemma-body)
                          lemma-body))))
    (bmc/create-assertion-lemmas-core lemma-str-list main-assertion-name process-proc)))

(define (solver-id->make-proc id)
  (cond
   ((assq id *solvers*) =>
    cdr)
   (else
    (sign-error "Unknown solver identifier `~a'. Available solvers: ~a" id (map car *solvers*)))))

(define (sal-inf-bmc/find-trace module depth solver-id . acyclic?)
  (let ((acyclic? (optional-arg acyclic? #f)))
    (bmc/find-trace-core module depth acyclic? (solver-id->make-proc solver-id))))

(define (sal-inf-bmc/find-path-from-initial-state module to-expr depth solver-id)
  (sal-inf-bmc/find-path module (slot-value module :initialization) to-expr depth solver-id))

(define (sal-inf-bmc/find-path module from-expr to-expr depth solver-id)
  (bmc/find-path-core module from-expr to-expr depth (solver-id->make-proc solver-id)))

(define (sal-inf-bmc/extend-path path module goal depth solver-id)
  (bmc/extend-path-core path module goal 0 depth (solver-id->make-proc solver-id)))

(define (sal-inf-bmc/extend-path+ path module goal starting-at depth solver-id)
  (bmc/extend-path-core path module goal starting-at depth (solver-id->make-proc solver-id)))

(define (sal-inf-bmc/invariant module-models from-depth to-depth acyclic? single-step? solver-id)
  (bmc/invariant-core module-models from-depth to-depth acyclic? single-step? (solver-id->make-proc solver-id)))

(define (sal-inf-bmc/liveness module-models depth single-step? solver-id)
  (bmc/liveness-core module-models depth single-step? (solver-id->make-proc solver-id)))

(define (sal-inf-bmc/k-induction module-models depth acyclic? lemmas solver-id)
  (bmc/k-induction-core module-models depth acyclic? lemmas (solver-id->make-proc solver-id)))
