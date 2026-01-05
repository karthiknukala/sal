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

(module sal-esm-engine-scm-context
        (include "sal.sch")
        (import sal2scm-core compile-and-load queue sal-ast-table code-table unique-names)
        (export <sal-esm-engine-scm-context>
                (sal-esm-engine-scm-context/compile-code! ctx)
                (sal-esm-engine-scm-context/init! ctx)
                (sal-esm-engine-scm-context/add-definition! ctx code id-prefix)
                (make-sal-esm-engine-scm-context))
        )


(define-class <sal-esm-engine-scm-context> (<sal-scm-context>) (:curr-var-idx-table
                                                                :next-var-idx-table
                                                                :section-id
                                                                :cached-code
                                                                :action-scm-decl-queue
                                                                :randomize?
                                                                ;; A restricted randomized context do not allow 
                                                                ;; the generation of all possible alternatives.
                                                                ;; In other words, it is only useful for random simulation.
                                                                ;; Its main advantage is: it is possible to make choices in
                                                                ;; infinite domains such as naturals and integers.
                                                                ;; This feature is used to pick random values 
                                                                ;; for uninitialized natural/integer variables.
                                                                :restricted-randomization?
                                                                :compile?

                                                                ;; #t if symmetry reduction is enabled.
                                                                :symmetry?

                                                                ;; Store a mapping from SAL state vardecl to Scheme global variable name which
                                                                ;; contains the current value of the variable in the explicit state model checker.
                                                                :curr-var-name-table
                                                                ;; Store a mapping from SAL state vardecl to Scheme global variable name which
                                                                ;; contains the next value of the variable in the explicit state model checker.
                                                                :next-var-name-table
                                                                
                                                                ;; maps (cache) SAL types -> val->bitstream procedures
                                                                :to-bitstream-proc-table
                                                                ;; maps (cache) SAL types -> bitstream->val procedures
                                                                :from-bitstream-proc-table
                                                                ;; maps (cache) SAL types -> normalization procedures
                                                                :normalized-proc-table
                                                                ;; maps (cache) SAL types -> comparison procedures
                                                                :cmp-proc-table

                                                                ;; maps symmetric types -> Scheme global variables which store, at runtime,
                                                                ;; the constraints associated with symmetric types. These constraints are
                                                                ;; used to normalized ('canonize') symmetric values.
                                                                :symmetry-constraint-table
                                                                ))

(define-generic (sal-esm-engine-scm-context/compile-code! ctx))

(define-method (sal-esm-engine-scm-context/compile-code! (ctx <sal-esm-engine-scm-context>))
  (if (slot-value ctx :compile?)
    (compile-and-load (queue->list (queue/append! (slot-value ctx :scm-decl-queue)
                                                  (queue->list (slot-value ctx :action-scm-decl-queue)))))
    (for-each eval (queue->list (slot-value ctx :action-scm-decl-queue)))))
  
(define (sal-esm-engine-scm-context/init! ctx)
  (sal-scm-context/init! ctx)
  (set-slot-value! ctx :cached-code (make-code-table))
  (set-slot-value! ctx :action-scm-decl-queue (make-queue))

  (set-slot-value! ctx :to-bitstream-proc-table (make-sal-ast-table))
  (set-slot-value! ctx :from-bitstream-proc-table (make-sal-ast-table))
  (set-slot-value! ctx :normalized-proc-table (make-sal-ast-table))
  (set-slot-value! ctx :cmp-proc-table (make-sal-ast-table))
  (set-slot-value! ctx :symmetry-constraint-table (make-sal-ast-table)))

(define (sal-esm-engine-scm-context/add-definition! ctx code id-prefix)
  (let ((cached-code (slot-value ctx :cached-code)))
    (cond
     ((code-table/get cached-code code) =>
      cdr)
     (else
      (let* ((id (gen-unique-name id-prefix))
             (def `(define ,id ,code)))
        ;; (pp def)
        (queue/insert! (slot-value ctx :action-scm-decl-queue) def)
        (code-table/put! cached-code code id)
        id)))))

(define (make-sal-esm-engine-scm-context)
  (let ((ctx (make-instance <sal-esm-engine-scm-context>)))
    (sal-esm-engine-scm-context/init! ctx)
    ctx))
