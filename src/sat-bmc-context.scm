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

(module sat-bmc-context
        (include "sal.sch")
        (include "fast-hash-table.sch")
        (import sal-expression sal-ast-copy sal-ast-env sal-module
                sal-ast-simplify sal-assertion sal-ast-list queue runtime
                sat-context sal-environment
                ;; sat-context-result 
                sal-ast-for-each sal-path sal-pp gmp-scheme
                sal-type sal-ast-eq sal-assertion sal-context polarity 
                sal-ast-expand iterators sal-expr-evaluator tmp-files sat-generic-context 
                sal-pp sal-guess-reader)
        (export <sat-bmc-context>
                (sat-bmc-context/init! ctx flat-module)
                (sat-bmc-context/id-at ctx id step)
                (sat-bmc-context/decl-table-at ctx step)
                (sat-bmc-context/decl-at ctx decl step)
                (sat-bmc-context/make-path ctx)
                (sat-bmc-context/original-decl ctx sat-decl)
                (sal-ast->sat ast ctx env step polarity)
                (make-bmc-problem-sat-context module-models depth acyclic? checking-from checking-to make-ctx-proc)
                (make-path-extension-sat-context path module goal starting-at delta-depth make-ctx-proc)
                (make-find-path-sat-context module from-expr to-expr starting-at depth make-ctx-proc)
                (make-k-induction-problem-sat-context module-models depth acyclic? lemmas make-ctx-proc)
                (make-bmc-liveness-problem-sat-context module-models depth make-ctx-proc)
                (make-find-trace-problem-sat-context module depth acyclic? make-ctx-proc)
                (bmc/find-trace-core module depth acyclic? make-ctx-proc)
                (bmc/find-path-core module from-expr to-expr depth make-ctx-proc)
                (bmc/extend-path-core path module goal starting-at delta-depth make-ctx-proc)
                (bmc/invariant-core module-models from-depth to-depth acyclic? single-step? make-ctx-proc)
                (bmc/liveness-core module-models depth single-step? make-ctx-proc)
                (bmc/k-induction-core module-models depth acyclic? lemmas make-ctx-proc)
                (bmc/create-assertion-lemmas-core lemma-str-list main-assertion-name process-proc)
                (bmc/check-if-valid-lemma flat-module lemma-body))
        )

(define *step-vector-initial-size* 8192)

(define-class <sat-bmc-context> () (:step-decl-tables ;; vector of [ original-decl -> decl ]
                                    :inv-step-decls   ;; decl -> (original-decl . step)
                                    :global-decls     ;; original-decl -> decl
                                    :inv-global-decls ;; decl -> original-decl
                                    :max-step
                                    :flat-module))

;; returns a pair (original-decl . step) associated with a sat-decl (or identifier).
(define (sat-bmc-context/original-decl ctx sat-decl)
  (cond 
   ((eq-hash-table/get (slot-value ctx :inv-step-decls) sat-decl) =>
    cdr)
   (else
    #f)))

(define (sat-bmc-context/init! ctx flat-module)
  (set-slot-value! ctx :step-decl-tables (make-vector *step-vector-initial-size*))
  (set-slot-value! ctx :global-decls (make-eq-hash-table))
  (set-slot-value! ctx :flat-module flat-module)
  (set-slot-value! ctx :inv-step-decls (make-eq-hash-table))
  (set-slot-value! ctx :inv-global-decls (make-eq-hash-table))
  (set-slot-value! ctx :max-step 0))
  
(define (make-sat-bmc-context flat-module)
  (let ((result (make-instance <sat-bmc-context>)))
    (sat-bmc-context/init! result flat-module)
    result))
    
(define (sat-bmc-context/decl-table-at ctx step)
  (when (> step (slot-value ctx :max-step))
    (set-slot-value! ctx :max-step step))
  (when (>= step (vector-length (slot-value ctx :step-decl-tables)))
    (set-slot-value! ctx :step-decl-tables (copy-vector (slot-value ctx :step-decl-tables) (* step 2))))
  (let ((name-tables (slot-value ctx :step-decl-tables)))
    (when (eq? (vector-ref name-tables step) #unspecified)
      (vector-set! name-tables step (make-eq-hash-table)))
    (vector-ref name-tables step)))

(define-generic (sat-bmc-context/id-at ctx id step))

(define-method (sat-bmc-context/id-at (ctx <sat-bmc-context>) (id <primitive>) (step <primitive>))
  (symbol-append id '_@_ (object->symbol step)))

(define-generic (sat-bmc-context/decl-at ctx decl step))

(define-generic (sat-bmc-context/make-path ctx))

;; ------------------------------------------------------
;; Internalize an ast in a sat-bmc-context.
;;
;;
;; ------------------------------------------------------

(define-generic (sal-ast->sat ast ctx env step polarity))

(define-method (sal-ast->sat (ast <sal-definition-expression>) (ctx <sat-bmc-context>) (env <primitive>) (step <primitive>) (polarity <polarity>))
  (sal-ast->sat (slot-value ast :expr) ctx env step polarity))

(define-method (sal-ast->sat (ast <sal-next-operator>) (ctx <sat-bmc-context>) (env <primitive>) (step <primitive>) (polarity <polarity>))
  (sal-ast->sat (slot-value ast :name-expr) ctx env (+ step 1) polarity))

(define-method (sal-ast->sat (ast <sal-true>) (ctx <sat-bmc-context>) (env <primitive>) (step <primitive>) (polarity <polarity>))
  (sat-context/make-true ctx))

(define-method (sal-ast->sat (ast <sal-false>) (ctx <sat-bmc-context>) (env <primitive>) (step <primitive>) (polarity <polarity>))
  (sat-context/make-false ctx))

(define (sal-application->sat ast ctx env step polarity proc)
  (let ((args (map (cut sal-ast->sat <> ctx env step polarity) (sal-application/argument-list ast))))
    (proc ctx args)))

(define-method (sal-ast->sat (ast <sal-and>) (ctx <sat-bmc-context>) (env <primitive>) (step <primitive>) (polarity <polarity>))
  (sal-application->sat ast ctx env step polarity sat-context/make-and*))

(define-method (sal-ast->sat (ast <sal-or>) (ctx <sat-bmc-context>) (env <primitive>) (step <primitive>) (polarity <polarity>))
  (sal-application->sat ast ctx env step polarity sat-context/make-or*))

(define (make-eq* ctx args)
  [assert (args) (= (length args) 2)]
  (sat-context/make-eq ctx (car args) (cadr args)))

(define-method (sal-ast->sat (ast <sal-diseq>) (ctx <sat-bmc-context>) (env <primitive>) (step <primitive>) (polarity <polarity>))
  (sat-context/make-not ctx (sal-application->sat ast ctx env step *pos-neg* make-eq*)))

(define-method (sal-ast->sat (ast <sal-eq>) (ctx <sat-bmc-context>) (env <primitive>) (step <primitive>) (polarity <polarity>))
  (sal-application->sat ast ctx env step *pos-neg* make-eq*))

(define-method (sal-ast->sat (ast <sal-not>) (ctx <sat-bmc-context>) (env <primitive>) (step <primitive>) (polarity <polarity>))
  (let ((arg (sal-ast->sat (slot-value ast :arg) ctx env step (polarity/invert polarity))))
    (sat-context/make-not ctx arg)))

(define-method (sal-ast->sat (ast <sal-implies>) (ctx <sat-bmc-context>) (env <primitive>) (step <primitive>) (polarity <polarity>))
  (multiple-value-bind
      (arg1 arg2)
      (sal-binary-application/arguments ast)
    (let ((new-arg1 (sal-ast->sat arg1 ctx env step (polarity/invert polarity)))
          (new-arg2 (sal-ast->sat arg2 ctx env step polarity)))
      (sat-context/make-or* ctx (list (sat-context/make-not ctx new-arg1) new-arg2)))))
     
(define-method (sal-ast->sat (ast <sal-conditional>) (ctx <sat-bmc-context>) (env <primitive>) (step <primitive>) (polarity <polarity>))
  (let ((c (sal-ast->sat (slot-value ast :cond-expr) ctx env step *pos-neg*))
        (t (sal-ast->sat (slot-value ast :then-expr) ctx env step polarity))
        (e (sal-ast->sat (slot-value ast :else-expr) ctx env step polarity)))
    (sat-context/make-ite ctx c t e)))

(define (expand-quantifier-for ast ctx env step polarity proc)
  (try
   (let* ((place-provider ast)
          (local-decls (slot-value ast :local-decls))
          (body (slot-value ast :expr))
          (types (map (cut slot-value <> :type) local-decls))
          (tuple-type (make-ast-instance <sal-tuple-type> ast :types types))
          (proc-child (lambda (child)
                        (sal-expr/evaluate-core child env 0)))
          (iterator (sal-type/make-iterator-core tuple-type proc-child))
          (values (make-queue)))
     (iterator/for-each 
      (lambda (tuple-literal)
        (let* ((env (update-env* env local-decls (slot-value tuple-literal :exprs)))
               (new-value (sal-ast->sat ast ctx env step polarity)))
          (queue/insert! values new-value)))
      iterator)
     (proc ctx (queue->list values)))
   (catch* '(type-iterator expr-evaluator)
           (lambda (_ msg)
             (sign-expander-error ast "Failed to expand quantifier, reason: ~a" msg)))))

(define-method (sal-ast->sat (ast <sal-exists-expr>) (ctx <sat-bmc-context>) (env <primitive>) (step <primitive>) (polarity <polarity>))
  (expand-quantifier-for ast ctx env step polarity sat-context/make-or*))

(define-method (sal-ast->sat (ast <sal-for-all-expr>) (ctx <sat-bmc-context>) (env <primitive>) (step <primitive>) (polarity <polarity>))
  (expand-quantifier-for ast ctx env step polarity sat-context/make-and*))
                         


;;-------------------------------------------------------
;;
;; BMC formula generation support
;;
;;-------------------------------------------------------

(define (unfold-core sal-expr ctx from to combine-proc)
  (let ((empty-env (make-empty-env)))
    (let loop ((i from)
               (atoms '()))
      (if (<= i to)
        (loop (+ i 1) (cons (sal-ast->sat sal-expr ctx empty-env i *pos*) atoms))
        (combine-proc atoms)))))

(define (unfold-and sal-expr ctx from to)
  (unfold-core sal-expr ctx from to (lambda (atoms) (sat-context/make-and* ctx atoms))))

(define (unfold-or sal-expr ctx from to)
  (unfold-core sal-expr ctx from to (lambda (atoms) (sat-context/make-or* ctx atoms))))

(define (valid-inputs->sat ctx from to)
  (let* ((flat-module (slot-value ctx :flat-module))
         (valid-input-expr (slot-value flat-module :valid-input-expr)))
    (unfold-and valid-input-expr ctx from to)))

(define (valid-states->sat ctx from to)
  (let* ((flat-module (slot-value ctx :flat-module))
         (valid-input-expr (slot-value flat-module :valid-state-expr)))
    (unfold-and valid-input-expr ctx from to)))

(define (make-steps-equal ctx step1 step2)
  (let* ((flat-module (slot-value ctx :flat-module))
         (defined-vars-set (sal-module/defined-variables flat-module))
         (state-vars (sal-module/state-variables flat-module))
         (equalities 
          (map-and-filter (lambda (var-decl)
                            (and (not (instance-of? var-decl <sal-input-state-var-decl>))
                                 (not (eq-hash-table/contains? defined-vars-set var-decl))
                                 (let ((arg1 (sat-bmc-context/decl-at ctx var-decl step1))
                                       (arg2 (sat-bmc-context/decl-at ctx var-decl step2)))
                                   (if (sal-type/boolean? (slot-value var-decl :type))
                                     (sat-context/make-iff ctx arg1 arg2)
                                     (sat-context/make-eq ctx arg1 arg2)))))
                          state-vars)))
    (sat-context/make-and* ctx equalities)))

(define (make-steps-distinct ctx step1 step2)
  (sat-context/make-not ctx (make-steps-equal ctx step1 step2)))

(define (make-acyclic-path ctx depth)
  (let ((atoms '()))
    (let loop1 ((i 0))
      (when (< i depth)
        (let loop2 ((j (+ i 1)))
          (when (<= j depth)
            (push! (make-steps-distinct ctx i j) atoms)
            (loop2 (+ j 1))))
        (loop1 (+ i 1))))
    (sat-context/make-and* ctx atoms)))

(define (make-cycle-starting-at ctx start-depth depth)
  (cond
   ((>= start-depth depth)
    (sat-context/make-false ctx))
   (else
    (let loop ((i (+ start-depth 1))
               (atoms '()))
      (if (<= i depth)
        (loop (+ i 1) (cons (make-steps-equal ctx start-depth i) atoms))
        (sat-context/make-or* ctx atoms))))))

(define (make-final-cycle ctx final-expr depth)
  (let ((empty-env (make-empty-env)))
    (let loop ((i 0)
               (atoms '()))
      (if (< i depth)
        (loop (+ i 1) (cons (sat-context/make-and* ctx (list (sal-ast->sat final-expr ctx empty-env i *pos*)
                                                             (make-cycle-starting-at ctx i depth)))
                            atoms))
        (sat-context/make-or* ctx atoms)))))

;;-------------------------------------------------------
;;
;; BMC formula generation
;;
;;-------------------------------------------------------

;; Build a bmc formula which extends the path 'path', 
;; looking for the goal 'goal' after 'starting-at' extra steps,
;; with at most 'delta-depth' steps.
(define-generic (make-path-extension-sat-context path module goal starting-at delta-depth make-ctx-proc))

(define-method (make-path-extension-sat-context (path <sal-cyclic-path>) (module <sal-flat-module>) (goal <sal-expr>)
                                                (starting-at <primitive>) (delta-depth <primitive>) (make-ctx-proc <primitive>))
  (sign-error "Only acyclic paths can be extended."))

(define-method (make-path-extension-sat-context (path <sal-concrete-path>) (module <sal-flat-module>) (goal <sal-expr>)
                                                (starting-at <primitive>) (delta-depth <primitive>) (make-ctx-proc <primitive>))
  [assert (delta-depth) (>= delta-depth 1)]
  [assert (path) (null? (slot-value path :auxiliary-decls))]
  [assert (path) (null? (slot-value path :global-constraint-list))]
  (let ((last-state-expr (sal-path/state-expr path (- (sal-path/length path) 1) #f)))
    (make-find-path-sat-context module last-state-expr goal starting-at delta-depth make-ctx-proc)))

(define-method (make-path-extension-sat-context (path <sal-path>) (module <sal-flat-module>) (goal <sal-expr>) 
                                                (starting-at <primitive>) (delta-depth <primitive>) (make-ctx-proc <primitive>))
  (verbose-message 2 "  building BMC (path extension) formula...")
  [assert (delta-depth) (>= delta-depth 1)]
  (display-runtime 3 "    build time: ~a secs"
    (lambda ()
      (make-ctx-proc module
                     (lambda (ctx)
                       (let ((path-module (slot-value path :flat-module))
                             (transition (slot-value module :transition))
                             (definition (slot-value module :definition))
                             (path-len (sal-path/length path)))
                         (let ((new-depth (- (+ path-len delta-depth) 1))
                               (empty-env (make-empty-env)))
                           ;; copy auxiliary declarations...
                           (for-each (cut sat-context/add-auxiliary-decl ctx <>) (slot-value path :auxiliary-decls))
                           ;; add global constrains
                           (for-each (lambda (global-constraint)
                                       (sat-context/assert ctx (sal-ast->sat global-constraint ctx empty-env 0 *pos*)))
                                     (slot-value path :global-constraint-list))
                           ;; add path constraints
                           (let loop ((i 0)
                                      (step-info-list (slot-value path :step-info-list)))
                             (unless (null? step-info-list)
                               (let* ((step-info (car step-info-list))
                                      (last-step? (null? (cdr step-info-list)))
                                      ;; I must not include choice variables in the laststep, since they may produce a deadlock.
                                      ;; the extended path will choose an appropriate value for this variables.
                                      (state-expr (step-info->state-expr step-info module (not last-step?))))
                                 ;; I don't need to rebind since module is eq? to path-module or it is a slice of path-module.
                                 ;; (new-state-expr (if (eq? path-module module) state-expr (sal-module/rebind-expr path-module state-expr module))))
                                 (sat-context/assert ctx (sal-ast->sat state-expr ctx empty-env i *pos*))
                                 (loop (+ i 1)
                                       (cdr step-info-list)))))
                           ;; add transition info
                           (sat-context/assert ctx (valid-inputs->sat ctx path-len new-depth))
                           (sat-context/assert ctx (valid-states->sat ctx path-len new-depth))
                           (sat-context/assert ctx (unfold-and definition ctx path-len new-depth))
                           (when (>= new-depth path-len)
                             (sat-context/assert ctx (unfold-and transition ctx (if (> path-len 0) (- path-len 1) 0) (- new-depth 1))))
                           ;; add goal
                           (sat-context/assert ctx (unfold-or goal ctx (+ path-len starting-at) new-depth))
                           ctx)))))))
    
(define (make-bmc-problem-sat-context module-models depth acyclic? checking-from checking-to make-ctx-proc)
  (status-message :building-bmc-formula)
  (verbose-message 2 "  building BMC formula...")
  (display-runtime 3 "    build time: ~a secs"
    (lambda ()
      (let ((module (slot-value module-models :module)))
        (make-ctx-proc module 
                       (lambda (ctx)
                         (let* ((property-body (sal-module-models/invariant-body module-models))
                                (not-property (make-sal-not property-body))
                                (transition (slot-value module :transition))
                                (initialization (slot-value module :initialization))
                                (definition (slot-value module :definition))
                                (empty-env (make-empty-env)))
                           (sat-context/assert ctx (valid-inputs->sat ctx 0 depth))
                           (sat-context/assert ctx (valid-states->sat ctx 0 depth))
                           (sat-context/assert ctx (sal-ast->sat initialization ctx empty-env 0 *pos*))
                           (sat-context/assert ctx (unfold-and definition ctx 0 depth))
                           (when (> depth 0)
                             (sat-context/assert ctx (unfold-and transition ctx 0 (- depth 1))))
                           (sat-context/assert ctx (unfold-or not-property ctx checking-from checking-to))
                           (when acyclic?
                             (sat-context/assert ctx (make-acyclic-path ctx depth))))))))
    :building-bmc-formula-time))

(define (make-find-path-sat-context module from-expr to-expr starting-at depth make-ctx-proc)
  (verbose-message 2 "  building BMC (path search) formula...")
  (display-runtime 3 "    build time: ~a secs"
    (lambda ()
      (make-ctx-proc module
                     (lambda (ctx)
                       (let ((transition (slot-value module :transition))
                             (definition (slot-value module :definition))
                             (empty-env (make-empty-env)))
                         ;; add from-expr
                         (sat-context/assert ctx (sal-ast->sat from-expr ctx empty-env 0 *pos*))
                         ;; add transition info
                         (sat-context/assert ctx (valid-inputs->sat ctx 0 depth))
                         (sat-context/assert ctx (valid-states->sat ctx 0 depth))
                         (sat-context/assert ctx (unfold-and definition ctx 0 depth))
                         (when (> depth 0)
                           (sat-context/assert ctx (unfold-and transition ctx 0 (- depth 1))))
                         (sat-context/assert ctx (unfold-or to-expr ctx starting-at depth))))))))

(define (make-k-induction-problem-sat-context module-models depth acyclic? lemmas make-ctx-proc)
  (status-message :building-k-induction-formula)
  (verbose-message 2 "  building k-induction formula...")
  (display-runtime 3 "    build time: ~a secs"
    (lambda ()
      (let* ((module (slot-value module-models :module))
             (property (sal-module-models/invariant-body module-models))
             (place-provider module-models)
             (not-property (make-sal-not property))
             (transition (slot-value module :transition))
             (definition (slot-value module :definition))
             (valid-state-expr (slot-value module :valid-state-expr))
             (empty-env (make-empty-env))
             (lemma-expr (make-sal-and+* lemmas place-provider)))
        (make-ctx-proc module
                       (lambda (ctx)
                         (sat-context/assert ctx (valid-inputs->sat ctx 0 depth))
                         (sat-context/assert ctx (valid-states->sat ctx 0 depth))
                         (when (> depth 0)
                           (sat-context/assert ctx (unfold-and property ctx 0 (- depth 1))))
                         (sat-context/assert ctx (unfold-and lemma-expr ctx 0 depth))
                         (sat-context/assert ctx (unfold-and definition ctx 0 depth))
                         (when (> depth 0)
                           (sat-context/assert ctx (unfold-and transition ctx 0 (- depth 1))))
                         (sat-context/assert ctx (sal-ast->sat not-property ctx empty-env depth *pos*))
                         (when acyclic?
                           (sat-context/assert ctx (make-acyclic-path ctx depth)))))))
    :building-k-induction-formula-time))

(define (make-bmc-liveness-problem-sat-context module-models depth make-ctx-proc)
  (status-message :building-bmc-liveness-formula)
  (verbose-message 2 "  building BMC (liveness) formula...")
  (display-runtime 3 "    build time: ~a secs"
    (lambda ()
      (let* ((module (slot-value module-models :module))
             (final-body (sal-module-models/accepting-body module-models))
             (transition (slot-value module :transition))
             (initialization (slot-value module :initialization))
             (definition (slot-value module :definition))
             (valid-state-expr (slot-value module :valid-state-expr))
             (empty-env (make-empty-env)))
        (make-ctx-proc module
                       (lambda (ctx)
                         (sat-context/assert ctx (valid-inputs->sat ctx 0 depth))
                         (sat-context/assert ctx (valid-states->sat ctx 0 depth))
                         (sat-context/assert ctx (sal-ast->sat initialization ctx empty-env 0 *pos*))
                         (sat-context/assert ctx (unfold-and definition ctx 0 depth))
                         (when (> depth 0)
                           (sat-context/assert ctx (unfold-and transition ctx 0 (- depth 1))))
                         (sat-context/assert ctx (make-final-cycle ctx final-body depth))))))
    :building-bmc-liveness-formula-time))

(define (make-find-trace-problem-sat-context module depth acyclic? make-ctx-proc)
  (verbose-message 2 "  building path formula...")
  (display-runtime 3 "    build time: ~a secs"
    (lambda ()
      (make-ctx-proc module
                     (lambda (ctx)
                       (let ((transition (slot-value module :transition))
                             (initialization (slot-value module :initialization))
                             (definition (slot-value module :definition))
                             (valid-state-expr (slot-value module :valid-state-expr))
                             (empty-env (make-empty-env)))
                         (sat-context/assert ctx (valid-inputs->sat ctx 0 depth))
                         (sat-context/assert ctx (valid-states->sat ctx 0 depth))
                         (sat-context/assert ctx (sal-ast->sat initialization ctx empty-env 0 *pos*))
                         (sat-context/assert ctx (unfold-and definition ctx 0 depth))
                         (when (> depth 0)
                           (sat-context/assert ctx (unfold-and transition ctx 0 (- depth 1))))
                         (when acyclic?
                           (sat-context/assert ctx (make-acyclic-path ctx depth)))))))))

;;-------------------------------------------------------
;;
;; BMC interface
;;
;;-------------------------------------------------------

(define (bmc/find-trace-core module depth acyclic? make-ctx-proc)
  (verbose-message 1 "trying to find trace of length: ~a" depth)
  (display-runtime 2 "  trace finder time: ~a secs"
    (lambda ()
      (let ((ctx (make-find-trace-problem-sat-context module depth acyclic? make-ctx-proc)))
        (sat-bmc-context/make-path ctx)))))

(define (bmc/find-path-core module from-expr to-expr depth make-ctx-proc)
  (verbose-message 2 "  Finding path of length: ~a" depth)
  (unless (instance-of? module <sal-flat-module>)
    (sign-source-error module "BMC can only be applied over flat modules."))
  (let* ((ctx (make-find-path-sat-context module from-expr to-expr 0 depth make-ctx-proc))
         (path (sat-bmc-context/make-path ctx)))
    (cond
     ((instance-of? path <sal-path>)
      (sal-path/cut path to-expr))
     (path
      path)
     (else
      #f))))

(define-generic (append-to-path path ctx goal starting-at))

(define-method (append-to-path (path <sal-path>) (ctx <sat-bmc-context>) (goal <sal-expr>) (starting-at <primitive>))
  (let ((new-path (sat-bmc-context/make-path ctx)))
    (if new-path
      (sal-path/cut-after new-path goal (+ (sal-path/length path) starting-at))
      #f)))

(define-method (append-to-path (path <sal-concrete-path>) (ctx <sat-bmc-context>) (goal <sal-expr>) (starting-at <primitive>))
  (let* ((path-to-append-tmp (sat-bmc-context/make-path ctx))
         (path-to-append (if path-to-append-tmp
                           (sal-path/cut-after path-to-append-tmp goal starting-at)
                           #f)))
    (if path-to-append
      (let* ((last-step-in-path (list-last-element (slot-value path :step-info-list)))
             (first-step-in-path-to-append (car (slot-value path-to-append :step-info-list))))
        (sal-step-info/copy-vars! first-step-in-path-to-append last-step-in-path)
        (copy-instance path
                       :step-info-list (append (slot-value path :step-info-list) (cdr (slot-value path-to-append :step-info-list)))))
      #f)))

(define (bmc/extend-path-core path module goal starting-at delta-depth make-ctx-proc)
  (verbose-message 2 "  Extending path (max. delta: ~a)..." delta-depth)
  (unless (or (eq? (slot-value path :flat-module) module)
              (sal-flat-module/slice-of? module (slot-value path :flat-module)))
    (sign-error "A path can only be extended using the same module or a slice of it.")) 
  (unless (instance-of? module <sal-flat-module>)
    (sign-source-error module "BMC can only be applied over flat modules."))
  (let ((ctx (make-path-extension-sat-context path module goal starting-at delta-depth make-ctx-proc)))
    (append-to-path path ctx goal starting-at)))

(define (invariant-core module-models depth acyclic? checking-from checking-to make-ctx-proc)
  (verbose-message 2 "  BMC depth: ~a..." depth)
  (let ((flat-module (slot-value module-models :module)))
    (unless (instance-of? flat-module <sal-flat-module>)
      (sign-source-error flat-module "BMC can only be applied over flat modules."))
    (let ((ctx (make-bmc-problem-sat-context module-models depth acyclic? checking-from checking-to make-ctx-proc)))
      (sat-bmc-context/make-path ctx))))

(define (check-if-invariant module-models fun-name)
  (unless (sal-module-models/invariant? module-models)
    (sign-source-error module-models "The property is not an invariant, and the proof method ~a can only be used to prove invariants." fun-name)))

(define (bmc/invariant-core module-models from-depth to-depth acyclic? single-step? make-ctx-proc)
  ;; (sal/pp module-models)
  (check-if-invariant module-models 'sal-module-models/bmc)
  (unless (<= from-depth to-depth)
    (sign-error "Invalid depth range [~a,~a] for SAL BMC." from-depth to-depth))
  (verbose-message 1 "executing BMC from depth ~a to ~a..." from-depth to-depth)
  (display-runtime 2 "  BMC time: ~a secs" 
    (lambda ()
      (if single-step?
        (let ((counter-example (invariant-core module-models to-depth acyclic? from-depth to-depth make-ctx-proc)))
          ;; the counter-example is not optimal, since the bad state may be in the middle of the path.
          (let* ((property-body (sal-module-models/invariant-body module-models))
                 (not-property (make-sal-not property-body)))
            (cond
             ((instance-of? counter-example <sal-path>)
              (values #f (sal-path/cut counter-example not-property)))
             (counter-example
              (values #f #unspecified))
             (else
              (values #t #unspecified)))))
        (let loop ((i from-depth))
          (if (<= i to-depth)
            (let ((counter-example (invariant-core module-models i acyclic? i i make-ctx-proc))) ;; check only the last step
              (cond 
               (counter-example
                (values #f counter-example))
               (else
                (loop (+ i 1)))))
            (values #t #unspecified))))))))

(define (liveness-core module-models depth make-ctx-proc)
  (verbose-message 2 "  BMC depth: ~a..." depth)
  (let ((flat-module (slot-value module-models :module)))
    (unless (instance-of? flat-module <sal-flat-module>)
      (sign-source-error flat-module "BMC can only be applied over flat modules."))
    (let* ((ctx (make-bmc-liveness-problem-sat-context module-models depth make-ctx-proc))
           (counter-example (sat-bmc-context/make-path ctx)))
      (cond
       ((instance-of? counter-example <sal-path>)
        (let ((final-expr (sal-module-models/accepting-body module-models)))
          (values #f (sal-path->cyclic-path counter-example final-expr))))
       (counter-example
        (values #f #unspecified))
       (else
        (values #t #unspecified))))))

(define (bmc/liveness-core module-models depth single-step? make-ctx-proc)
  (unless (sal-module-models/accepting? module-models)
    (sign-error "Only accepting condition properties (low level encoding of LTL properties) can be verified by sal-bmc. Please use sal-module-models/ltl->ba to convert the property."))
  (verbose-message 1 "executing (liveness property) BMC up to depth ~a" depth)
  (display-runtime 2 "  BMC time: ~a secs"
    (lambda ()
      (if single-step?
        (liveness-core module-models depth make-ctx-proc)
        (let loop ((i 1))
          (if (<= i depth)
            (multiple-value-bind
                (result counter-example)
                (liveness-core module-models i make-ctx-proc)
              (if result
                (loop (+ i 1))
                (values #f counter-example)))
            (values #t #unspecified)))))))

(define (bmc/k-induction-core module-models depth acyclic? lemmas make-ctx-proc)
  (check-if-invariant module-models 'sal-module-models/bmc)
  (verbose-message 1 "executing k-induction with k=~a" depth)
  (display-runtime 2 "  induction time: ~a secs" 
    (lambda ()
      (let ((module (slot-value module-models :module)))
        (unless (instance-of? module <sal-flat-module>)
          (sign-source-error module "The induction rule can only be applied over flat modules."))
        (let* ((ctx (make-k-induction-problem-sat-context module-models depth acyclic? lemmas make-ctx-proc))
               (path (sat-bmc-context/make-path ctx)))
          (if path
            (values #f path)
            (values #t #unspecified)))))))

;;-------------------------------------------------------
;;
;; Lemma generation
;;
;;-------------------------------------------------------

;; Template function used to create lemmas
;; - lemmas are invariants
;; - lemma-str-list is a list of strings
;; - main-assertion-name is the assertion we are trying to prove using the given lemmas
;; - a lemma-str is a qualified assertion name, or it is just a name assumed to be 
;;   in the same context of main-assertion-name
;; - process-proc is a procedure that maps a lemma module-models AST to an expression
;;   which is binded to the variables in the flat-module associated with main-assertion-name
(define (bmc/create-assertion-lemmas-core lemma-str-list main-assertion-name process-proc)
  (let* ((main-assertion-body (sal-assertion-name/definition main-assertion-name))
         (module (slot-value main-assertion-body :module))
         (context (slot-value main-assertion-name :context-ref))   
         (sal-env (sal-ast/sal-env module))
         (result '()))
    (for-each (lambda (lemma-str)
                (let ((lemma (cond
                              ((or (sal-string-index lemma-str #\()
                                   (sal-string-index lemma-str #\!))
                               (sal-env/with-string-reader sal-env (guess-qualified-name-reader lemma-str)
                                                           (lambda ()
                                                             ;; lemma-str is a qualified assertion name)
                                                             (sal-assertion-name/definition (sal-env/assertion-name-string->ast sal-env lemma-str)))))
                              (else
                               ;; assume that lemma is the same context of main-assertion-name
                               (let ((decl (sal-context/assertion-declaration context (string->symbol lemma-str))))
                                 (unless decl
                                   (sign-error "Unknown lemma '~a'." lemma-str))
                                 (sal-assertion-name/definition (copy-ast main-assertion-name
                                                                          :decl decl)))))))
                  (unless (and (instance-of? lemma <sal-module-models>)
                               (sal-module-models/invariant? lemma))
                    (sign-error "Lemma '~a' is not an invariant. Lemmas must be invariants." lemma-str))
                  (unless (sal-ast/equivalent? module (slot-value lemma :module))
                    (sign-error "Invalid lemma, a lemma must state a property of the same module specified by the assertion '~a'. Remark: in this sanity check, two modules are equivalent if they are syntatically equivalent." lemma-str))
                  (push! (process-proc lemma) result)))
              lemma-str-list)
    result))

(define (bmc/check-if-valid-lemma flat-module lemma-body)
  (when (instance-of? flat-module <sal-sliced-flat-module>)
    (let ((used-sliced-var (sal-ast/find (lambda (node)
                                           (and (instance-of? node <sal-name-expr>)
                                                (instance-of? (slot-value node :decl) <sal-state-var-decl>)
                                                (not (sal-module/lookup-var flat-module (sal-name-ref/name node)))))
                                         lemma-body)))
      (when used-sliced-var
        (sign-source-error lemma-body "Invalid lemma, it uses the sliced variable `~a'." (sal-name-ref/name used-sliced-var))))))
