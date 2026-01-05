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

(module sal-derived-path
        (include "sal.sch")
        (include "fast-hash-table.sch")
        (import queue sal-module sal-expression sal-finite-expressions sal-flat-data-structures 
                sal-ast-env sal-ast-table sal-ast-copy unique-names sal-path sal-bmc 
                yices-interface sal-inf-bmc sal-pp sal-expr-evaluator gmp-scheme iterators sal-type
                sal-transition-step sal-trace-info)
        (export (sal-derived-path->original-path path)
                (sal-sliced-path->path path)
                (sal-derived-input-seq->original-input-seq path)
                (boolean-assignment-table->assignment-table bool-assignment-table bool-flat-module)
                (sal-bool-step-list->step-list step-list bool-flat-module)
                (sal-path/state-variable-value-at path var-id step-idx)
                (sal-trace-info/recover-executed-transition-info trace-info assignment-table flat-module)
                (sal-step-list/recover-executed-transition-info! step-list flat-module))
        )

;; --------------------------------------------------------------------
;; 
;; (sal-derived-path->original-path path)
;;
;; --------------------------------------------------------------------
(define-api (sal-derived-path->original-path path)
  :doc "Convert a path to a path of the original module. SAL performs several transformations in a module to be able to perform symbolic or bounded model checking. So, the paths (e.g., counterexamples) produced by these tools are related to the module produced by these transformations. So, this procedure must be used to obtain a path of the original module."
  (sal-derived-path->original-path-core path (slot-value path :flat-module)))

(define-generic (sal-derived-path->original-path-core path module))

(define-method (sal-derived-path->original-path-core (path <sal-path>) (module <sal-flat-module>))
  path)

(define (sal-sliced-derived-path->original-path-core path module)
  (let* ((original-module (slot-value module :original-module))
         (new-path (update-slots path 
                                 :flat-module original-module)))
    (sal-derived-path->original-path-core new-path original-module)))

(define-method (sal-derived-path->original-path-core (path <sal-path>) (module <sal-sliced-flat-module>))
  (sal-sliced-derived-path->original-path-core path module))

(define-method (sal-derived-path->original-path-core (path <sal-concrete-path>) (module <sal-sliced-flat-module>))
  (sal-sliced-derived-path->original-path-core path module))

(define-method (sal-derived-path->original-path-core (path <sal-cyclic-path>) (module <sal-sliced-flat-module>))
  (sal-sliced-derived-path->original-path-core path module))

(define (make-empty-path module)
  (make-instance <sal-concrete-path>
                     :global-constraint-list '() ;; ***** FIXME ******
                     :auxiliary-decls '()
                     :flat-module module
                     :step-info-list '()))

(define (sal-sliced-path->path-core path find-path-proc extend-path-proc)
  (status-message :recovering-sliced-variables)
  (verbose-message 1 "  building path with sliced variables...")
  (let* ((module (slot-value path :flat-module))
         (_ [assert (module) (instance-of? module <sal-sliced-flat-module>)])
         (original-module (slot-value module :original-module))
         (step-info-list (slot-value path :step-info-list))
         (new-step-queue (make-queue))
         (old-mode (sal/yices-silent-mode?)))
    [assert (module original-module) (<= (length (sal-module/state-variables module)) (length (sal-module/state-variables original-module)))]
    [assert (module original-module) (for-all (lambda (mod-var-decl)
                                                (sal-module/lookup-var original-module (sal-decl/name mod-var-decl)))
                                              (sal-module/state-variables module))]
    (cond 
     ((null? step-info-list)
      (make-empty-path original-module))
     (else
      (sal/set-yices-silent-mode! #t)
      (unwind-protect
       (let* ((curr-step-info (car step-info-list))
              (curr-verbosity-level (verbosity-level))
              (rest-step-info-list (cdr step-info-list))
              (curr-step-info-expr (step-info->state-expr curr-step-info original-module))
              (init-constraint (sal-module/rebind-expr module curr-step-info-expr original-module))
              (path0 (find-path-proc original-module init-constraint 0 'yices)))
         (let loop ((curr-path path0)
                    (step-info-list rest-step-info-list)
                    (i 1))
           (verbose-message 2 "  iteration: ~a" i)
           (cond 
            ((null? step-info-list)
             [assert (curr-path original-module) (eq? (slot-value curr-path :flat-module) original-module)]
             curr-path)
            (else 
             (let* ((curr-step (car step-info-list))
                    (curr-expr (step-info->state-expr curr-step original-module))
                    (curr-constraint (sal-module/rebind-expr module curr-expr original-module))
                    (new-path (extend-path-proc curr-path original-module curr-constraint 1 'yices)))
               (unless new-path
                 (sign-error "Recovering value of sliced variables. The sliced counterexample is not a valid one. Your specification may contain a type error."))
               [sal-assert "sliced:path-extension" (curr-path new-path) (= (+ (sal-path/length curr-path) 1) (sal-path/length new-path))]
               (loop new-path (cdr step-info-list) (+ i 1)))))))
       (sal/set-yices-silent-mode! old-mode))))))

(define (sal-sliced-path->path path)
  (if (instance-of? (slot-value path :flat-module) <sal-boolean-flat-module>)
    (sal-sliced-path->path-core path sal-bmc/find-path-from-initial-state sal-bmc/extend-path)
    (sal-sliced-path->path-core path sal-inf-bmc/find-path-from-initial-state sal-inf-bmc/extend-path)))

(define (sal-sliced-path->original-path-core path)
  (let ((path (let loop ((path path))
                ;; get the first module which the original module is not a sliced module
                (let* ((module (slot-value path :flat-module))
                       (_ [assert (module) (instance-of? module <sal-sliced-flat-module>)])
                       (original-module (slot-value module :original-module)))
                  (cond
                   ((instance-of? original-module (class-of module))
                    (loop (copy-instance path
                                         :flat-module original-module)))
                   (else
                    path))))))
    (let ((new-path (sal-sliced-path->path path)))
      (sal-derived-path->original-path-core new-path (slot-value new-path :flat-module)))))

(define-method (sal-derived-path->original-path-core :around (path <sal-path>) (module <sal-module>))
  (let ((result (call-next-method))
        (flat-module (slot-value path :flat-module)))
    (when (slot-value flat-module :transition-trace-info)
      (sal-path/recover-executed-transition-info! path))
    result))
  
(define-method (sal-derived-path->original-path-core (path <sal-concrete-path>) (module <sal-sliced-boolean-flat-module>))
  (sal-sliced-path->original-path-core path))

(define-method (sal-derived-path->original-path-core (path <sal-path>) (module <sal-sliced-simple-data-flat-module>))
  (sal-sliced-path->original-path-core path))

(define (sal-cyclic-path->concrete-path path)
  [assert (path) (instance-of? path <sal-cyclic-path>)]
  (let ((new-path (make-instance <sal-concrete-path>
                                 :global-constraint-list '() ;; ***** FIXME ******
                                 :auxiliary-decls '()
                                 :flat-module (slot-value path :flat-module)
                                 :step-info-list (append (slot-value path :step-info-list) (cdr (slot-value path :cycle-step-info-list))))))
    new-path))

(define-method (sal-derived-path->original-path-core (path <sal-cyclic-path>) (module <sal-sliced-boolean-flat-module>))
  (let* ((new-path (sal-cyclic-path->concrete-path path))
         (result-path (sal-sliced-path->original-path-core new-path)))
    (change-class result-path <sal-pseudo-cyclic-path>)))

(define (sal-boolean-concrete-path->original-path path)
  (let* ((module (slot-value path :flat-module))
         (original-module (slot-value module :original-module))
         (new-path (make-instance <sal-concrete-path>
                                  :global-constraint-list '() ;; ******* FIXME **********
                                  :auxiliary-decls '() 
                                  :flat-module original-module
                                  :step-info-list (sal-bool-step-list->step-list (slot-value path :step-info-list) module))))
    new-path))

(define-method (sal-derived-path->original-path-core (path <sal-concrete-path>) (module <sal-boolean-flat-module>))
  (sal-derived-path->original-path-core (sal-boolean-concrete-path->original-path path)
                                        (slot-value module :original-module)))

(define (sal-boolean-cyclic-path->original-path path)
  (let* ((module (slot-value path :flat-module))
         (original-module (slot-value module :original-module))
         (new-path (make-instance <sal-cyclic-path>
                                  :global-constraint-list '() ;; ********* FIXME ************
                                  :auxiliary-decls '() 
                                  :flat-module original-module
                                  :step-info-list (sal-bool-step-list->step-list (slot-value path :step-info-list) module)
                                  :cycle-step-info-list (sal-bool-step-list->step-list (slot-value path :cycle-step-info-list) module))))
    new-path))
  
(define-method (sal-derived-path->original-path-core (path <sal-cyclic-path>) (module <sal-boolean-flat-module>))
  (sal-derived-path->original-path-core (sal-boolean-cyclic-path->original-path path) 
                                        (slot-value module :original-module)))
  
(define (sal-simple-data-path->original-path path)
  (let* ((module (slot-value path :flat-module))
         (inv-map (create-inv-map module))
         (original-module (slot-value module :original-module))
         (original-state-vars (slot-value original-module :state-vars))
         (var-trace-info (slot-value module :var-trace-info))
         (global-constraint-list (slot-value path :global-constraint-list))
         (new-global-constraint-list (flat-constraint-list->constraint-list global-constraint-list inv-map))
         (step-info-list (slot-value path :step-info-list))
         (new-step-info-list (flat-step-info-list->step-info-list step-info-list original-state-vars var-trace-info inv-map))
         (result-path (make-instance <sal-path> 
                                     :flat-module original-module
                                     :step-info-list new-step-info-list
                                     :auxiliary-decls (slot-value path :auxiliary-decls)
                                     :global-constraint-list new-global-constraint-list)))
    (sal-path/eliminate-bad-vars! result-path)
    result-path))

(define-method (sal-derived-path->original-path-core (path <sal-path>) (module <sal-simple-data-flat-module>))
  (sal-derived-path->original-path-core (sal-simple-data-path->original-path path) (slot-value module :original-module)))


;; --------------------------------------------------------------------
;; 
;; (sal-derived-input-seq->original-input-seq path)
;;
;; --------------------------------------------------------------------

;; This method is similar to sal-derived-path->original-path, but only the
;; input variables are considered. So, it is more efficient, since BMC doesn't
;; need to be used to recover the value of sliced variables.
(define (sal-derived-input-seq->original-input-seq path)
  (sal-derived-input-seq->original-input-seq-core path (slot-value path :flat-module)))

(define-generic (sal-derived-input-seq->original-input-seq-core path module))

(define-method (sal-derived-input-seq->original-input-seq-core (path <sal-path>) (module <sal-flat-module>))
  path)

(define-method (sal-derived-input-seq->original-input-seq-core :around (path <sal-path>) (module <sal-module>))
  (let ((result (call-next-method))
        (flat-module (slot-value path :flat-module)))
    (when (slot-value flat-module :transition-trace-info)
      (sal-path/recover-executed-transition-info! path))
    result))

(define (sal-sliced-input-seq->original-input-seq path)
  (let* ((module (slot-value path :flat-module))
         (original-module (slot-value module :original-module))
         (new-path (update-slots path 
                                 :flat-module original-module)))
    (sal-derived-input-seq->original-input-seq-core new-path original-module)))

(define-method (sal-derived-input-seq->original-input-seq-core (path <sal-path>) (module <sal-sliced-flat-module>))     
  (sal-sliced-input-seq->original-input-seq path))
(define-method (sal-derived-input-seq->original-input-seq-core (path <sal-concrete-path>) (module <sal-sliced-flat-module>))      
  (sal-sliced-input-seq->original-input-seq path))
(define-method (sal-derived-input-seq->original-input-seq-core (path <sal-cyclic-path>) (module <sal-sliced-flat-module>))      
  (sal-sliced-input-seq->original-input-seq path))

(define-method (sal-derived-input-seq->original-input-seq-core (path <sal-cyclic-path>) (module <sal-sliced-boolean-flat-module>))
  (let* ((new-path (sal-cyclic-path->concrete-path path))
         (result-path (sal-derived-input-seq->original-input-seq-core new-path module)))
    (change-class result-path <sal-pseudo-cyclic-path>)))

(define-method (sal-derived-input-seq->original-input-seq-core (path <sal-concrete-path>) (module <sal-boolean-flat-module>))
  (sal-derived-input-seq->original-input-seq-core (sal-boolean-concrete-path->original-path path)
                                        (slot-value module :original-module)))

(define-method (sal-derived-input-seq->original-input-seq-core (path <sal-cyclic-path>) (module <sal-boolean-flat-module>))
  (sal-derived-input-seq->original-input-seq-core (sal-boolean-cyclic-path->original-path path) 
                                        (slot-value module :original-module)))

(define-method (sal-derived-input-seq->original-input-seq-core (path <sal-path>) (module <sal-simple-data-flat-module>))
  (sal-derived-input-seq->original-input-seq-core (sal-simple-data-path->original-path path) (slot-value module :original-module)))
         

;; --------------------------------------------------------------------
;; 
;; Auxiliary functions to convert a path of a boolean-flat-module in a path of the original associated
;; flat module.
;;
;; --------------------------------------------------------------------

(define (bool-var-decl-list->bit-list bool-var-decl-list bool-assignment-table place-provider)
  (map (lambda (bool-var-decl)
         (cond
          ((eq-hash-table/get bool-assignment-table bool-var-decl) =>
           cdr)
          (else
           (make-sal-false place-provider))))
       bool-var-decl-list))

(define (make-original-var-value original-var-decl var-trace-info bool-assignment-table)
  ;; (breakpoint "value" (original-var-decl var-trace-info bool-variable-values) #t) 
  (let* ((var-type (slot-value original-var-decl :type))
         (bool-var-decl-list (let ((entry (eq-hash-table/get var-trace-info original-var-decl)))
                               [assert (entry var-trace-info original-var-decl) (and entry (pair? entry))]
                               (cdr entry)))
         (bit-list (bool-var-decl-list->bit-list bool-var-decl-list bool-assignment-table original-var-decl)))
    ;; (breakpoint "make-original-var-value" (original-var-decl var-trace-info bool-variable-values bit-list var-type) (null? bit-list))
    (bit-list->sal-value var-type bit-list)))

(define (boolean-assignment-table->assignment-table bool-assignment-table bool-flat-module)
  ;; (eq-hash-table/for-each (lambda (k val)
  ;; (sal/pp k) (display " -> ") (sal/pp val) (print ""))
  ;; bool-assignment-table)
  ;; (print "--------------")
  (let* ((result (make-eq-hash-table))
         (original-flat-module (slot-value bool-flat-module :original-module))
         (original-state-vars (slot-value original-flat-module :state-vars))
         (var-trace-info (slot-value bool-flat-module :var-trace-info)))
    (for-each (lambda (original-var-decl)
                (let ((var-value (make-original-var-value original-var-decl var-trace-info bool-assignment-table)))
                  (eq-hash-table/put! result original-var-decl var-value)))
              original-state-vars)
    result))

(define (sal-bool-step-list->step-list step-list bool-flat-module)
  ;; (breakpoint "bool-step-list" (step-list bool-flat-module) #t)
  (unless (instance-of? bool-flat-module <sal-boolean-flat-module>)
    (sign-usage-error 'sal-boolean-flat-module-path->sal-flat-module-path "The argument is not a path of a boolean flat module."))
  (let ((new-steps-queue (make-queue)))
    (for-each (lambda (bool-step)
                (let* ((bool-assignment-table (slot-value bool-step :assignment-table))
                       (new-assignment-table (boolean-assignment-table->assignment-table bool-assignment-table bool-flat-module))
                       (new-step (make-instance <sal-step>
                                                :assignment-table new-assignment-table
                                                :constraint-list '()))) 
                  (queue/insert! new-steps-queue new-step)))
              step-list)
    (queue->list new-steps-queue)))

;; --------------------------------------------------------------------
;; 
;; Auxiliary functions to convert a path of a simple-data-flat-module in a path of the original associated
;; flat module.
;;
;; --------------------------------------------------------------------

(define (flat-step-info-list->step-info-list step-info-list original-state-vars var-trace-info inv-map)
  (map (cut flat-step-info->step-info <> original-state-vars var-trace-info inv-map) step-info-list))

(define (flat-step-info->step-info step original-state-vars var-trace-info inv-map)
  (let* ((assignment-table (slot-value step :assignment-table))
         (constraint-list (slot-value step :constraint-list))
         (new-assignment-table (make-eq-hash-table))
         (processed-table (make-eq-hash-table))
         (empty-env (make-empty-env)))
    ;; first step try to build a new assignment table
    (for-each (lambda (original-var-decl)
                (bind-exit (exit)
                  (let* ((flat-var-decls (cdr (eq-hash-table/get var-trace-info original-var-decl)))
                         (flat-var-values (map (lambda (flat-var-decl)
                                                 (cond 
                                                  ((eq-hash-table/get assignment-table flat-var-decl) =>
                                                   (lambda (entry)
                                                     (flat-data-ast->ast (cdr entry) empty-env inv-map)))
                                                  (else
                                                   (exit #unspecified)))) ;; cannot handle this var-decl
                                               flat-var-decls)))
                    ;; mark flat-var-decls as processed...
                    (for-each (cut eq-hash-table/put! processed-table <> #unspecified) flat-var-decls)
                    ;; convert flat-var-values & store result in the new assignment-table 
                    (eq-hash-table/put! new-assignment-table
                                        original-var-decl
                                        (data-list->sal-value (slot-value original-var-decl :type)
                                                              flat-var-values)))))
              original-state-vars)
    ;; move non processed var-decls in the assignment table to the constraint list
    (eq-hash-table/for-each (lambda (decl expr)
                              (unless (eq-hash-table/put! processed-table decl #unspecified)
                                (let ((constraint (make-sal-equality (make-sal-name-expr decl)
                                                                     expr)))
                                  (push! constraint constraint-list))))
                            assignment-table)
    ;; convert constraint list
    (let ((new-constraint-list (flat-constraint-list->constraint-list constraint-list inv-map)))
      (copy-instance step
                     :assignment-table new-assignment-table
                     :constraint-list new-constraint-list))))

(define (flat-constraint-list->constraint-list constraint-list inv-map)
  (let ((empty-env (make-empty-env)))
    (map (lambda (constraint)
           (let ((result (flat-data-ast->ast constraint empty-env inv-map)))
;              (print "converting constraint...")
;              (sal/pp constraint)
;              (print "")
;              (sal/pp result)
;              (print "\n--------------")
             result))
         constraint-list)))

(define (create-inv-map flat-module)
  (let ((inv-map (make-eq-hash-table))
        (var-trace-info (slot-value flat-module :var-trace-info))
        (const-trace-info (slot-value flat-module :const-trace-info)))
    (eq-hash-table/for-each (lambda (decl decl-list)
                              (populate-inv-flat-data-map! (slot-value decl :type) decl-list 
                                                           (make-sal-name-expr decl)
                                                           inv-map))
                            var-trace-info)
    (sal-ast-table/for-each (lambda (qualified-name-expr decl-list)
                              (populate-inv-flat-data-map! (sal-expr/type qualified-name-expr) decl-list qualified-name-expr inv-map))
                            const-trace-info)
    inv-map))
      
;; --------------------------------------------------------------------
;; 
;; Eliminate BAD choice variables
;; 
;; A choice variable is bad if it is not in the assignment table, but
;; in the constraint list. 
;; These "bad" variables are substituted by auxiliary variables which
;; the name is based on the original variable associated with the 
;; choice variable.
;;
;; --------------------------------------------------------------------

(define-generic (find-original-choice-var-name trace-info choice-var-name))
(define-method (find-original-choice-var-name (trace-info <sal-transition-trace-info>) (choice-var-name <primitive>))
  (sign-error "Decision procedure provided insufficient information to build counterexample. Try to use --disable-traceability."))
(define-method (find-original-choice-var-name (trace-info <sal-nested-trace-info>) (choice-var-name <primitive>))
  (find-original-choice-var-name (slot-value trace-info :info) choice-var-name))
(define-method (find-original-choice-var-name (trace-info <sal-nested-list-trace-info>) (choice-var-name <primitive>))
  (find (cut find-original-choice-var-name <> choice-var-name) (slot-value trace-info :info-list)))
(define-method (find-original-choice-var-name (trace-info <sal-multi-choice-trace-info>) (choice-var-name <primitive>))
  [assert (trace-info) (= (length (slot-value trace-info :choice-var-names)) (length (slot-value trace-info :original-var-names)))] 
  (let loop ((choice-var-names (slot-value trace-info :choice-var-names))
             (original-var-names (slot-value trace-info :original-var-names)))
    (cond
     ((null? choice-var-names)
      (call-next-method))
     ((eq? (car choice-var-names) choice-var-name)
      (car original-var-names))
     (else
      (loop (cdr choice-var-names) (cdr original-var-names))))))

(define-class <ebv-info> () (:step-vector :new-aux-var-decl-queue :transition-trace-info :curr-step))

(define-generic (sal-ast/eliminate-bad-vars! ast env num-pre-op ebv-info))
(define-method (sal-ast/eliminate-bad-vars! (ast <sal-ast>) (env <primitive>) (num-pre-op <primitive>) (ebv-info <ebv-info>))
  (sal-ast/map ast env (lambda (child-ast new-env) 
                         (sal-ast/eliminate-bad-vars! child-ast new-env num-pre-op ebv-info))))
(define-method (sal-ast/eliminate-bad-vars! (ast <sal-pre-operator>) (env <primitive>) (num-pre-op <primitive>) (ebv-info <ebv-info>))
  (sal-ast/eliminate-bad-vars! (slot-value ast :expr) env (+ num-pre-op 1) ebv-info))
(define-method (sal-ast/eliminate-bad-vars! (ast <sal-name-expr>) (env <primitive>) (num-pre-op <primitive>) (ebv-info <ebv-info>))
  (let ((decl (slot-value ast :decl)))
    (cond 
     ((not (sal-name-ref/state? ast))
      (call-next-method))
     ((not (instance-of? decl <sal-choice-input-state-var-decl>))
      [assert (decl) (instance-of? decl <sal-state-var-decl>)]
      (let loop ((result (call-next-method))
                 (num-pre-op num-pre-op))
        (if (> num-pre-op 0)
          (loop (make-ast-instance <sal-pre-operator> result
                                   :expr result)
                (- num-pre-op 1))
          result)))
     (else
      [assert (decl) (instance-of? decl <sal-choice-input-state-var-decl>)]
      (let* ((step-idx (- (slot-value ebv-info :curr-step) num-pre-op))
             (_ [assert (step-idx) (>= step-idx 0)])
             (step (vector-ref (slot-value ebv-info :step-vector) step-idx))
             (assignment-table (slot-value step :assignment-table)))
        (cond
         ((eq-hash-table/get assignment-table decl) => cdr)
         (else
          (let* ((name (sal-name-ref/name ast))
                 (original-name (let ((tmp (find-original-choice-var-name (slot-value ebv-info :transition-trace-info) name)))
                                  (if tmp
                                    (gen-unique-name tmp)
                                    (gen-unique-name 'aux)
                                    )))
                 (new-id (copy-ast (slot-value decl :id) 
                                   :name original-name))
                 (new-aux-var-decl (make-ast-instance <sal-var-decl> decl
                                                      :id new-id
                                                      :type (slot-value decl :type)))
                 (new-aux-name-expr (make-sal-name-expr new-aux-var-decl)))
            (eq-hash-table/put! assignment-table decl new-aux-name-expr)
            (queue/insert! (slot-value ebv-info :new-aux-var-decl-queue) new-aux-var-decl)
            new-aux-name-expr))))))))

(define (sal-path/eliminate-bad-vars! path)
  (let* ((flat-module (slot-value path :flat-module))
         (ebv-info (make-instance <ebv-info>
                                  :step-vector (list->vector (slot-value path :step-info-list))
                                  :new-aux-var-decl-queue (make-queue)
                                  :transition-trace-info (slot-value flat-module :transition-trace-info)
                                  :curr-step 0))
         (empty-env (make-empty-env)))
    (for-each (lambda (step)
                (set-slot-value! step :constraint-list
                                 (map (cut sal-ast/eliminate-bad-vars! <> empty-env 0 ebv-info) 
                                      (slot-value step :constraint-list)))
                (set-slot-value! ebv-info :curr-step (+ (slot-value ebv-info :curr-step) 1)))
              (slot-value path :step-info-list))
    (set-slot-value! path :auxiliary-decls (append (slot-value path :auxiliary-decls)
                                                   (queue->list (slot-value ebv-info :new-aux-var-decl-queue))))))
        

;; --------------------------------------------------------------------
;; Variable value extraction
;; 
;; Extract the value of a variable from a specific step in a path.
;; --------------------------------------------------------------------
(define-api (sal-path/state-variable-value-at path var-id step-idx)
  :doc "Extract the value of a variable from a specific step in a path. 
The value may be unavailable. Reasons: 
- parts of the variable were sliced
- there is an assignment associated with the variable (i.e., the variable is only associated with constraints).
Parameters:
- path: a path object.
- var-id: a symbol representing the original variable name.
- step-idx: an integer specifying the step from which the value will be extracted."
  (bind-exit (exit)
    (let* ((module (slot-value path :flat-module))
           (var-bits (sal-module/var-bits module var-id))
           (step-info (sal-path/step-data path step-idx))
           (assignment-table (slot-value step-info :assignment-table))
           (var-decls (var-bits->var-decls module var-bits)))
      (unless var-decls
        (exit #f)) ;; the value of the variable is unavailable...
      (let ((var-values (var-decls->var-values module var-decls assignment-table)))
        (unless var-values
          (exit #f))
        (derived-value->value module var-id var-values)))))

(define-generic (sal-module/var-bits module var-id))
(define-method (sal-module/var-bits (module <sal-flat-module>) (var-id <primitive>))
  (list var-id))
(define-method (sal-module/var-bits (module <sal-sliced-flat-module>) (var-id <primitive>))
  (sal-module/var-bits (slot-value module :original-module) var-id))
(define (module-var-bits-core module var-id)
  (let* ((original-module (slot-value module :original-module))
         (var-decl (sal-module/lookup-var original-module var-id))
         (pair (eq-hash-table/get (slot-value module :var-trace-info) var-decl)))
    (and pair
         (map sal-decl/name (cdr pair)))))
(define-method (sal-module/var-bits (module <sal-boolean-flat-module>) (var-id <primitive>))
  (module-var-bits-core module var-id))
(define-method (sal-module/var-bits (module <sal-simple-data-flat-module>) (var-id <primitive>))
  (module-var-bits-core module var-id))

(define (var-bits->var-decls module var-bits)
  (bind-exit (exit)
    (map (lambda (var-bit)
           (let ((var-decl (sal-module/lookup-var module var-bit)))
             (unless var-decl
               (exit #f)) ;; the value of the variable is unavailable...
             var-decl))
         var-bits)))

(define-generic (var-decls->var-values module var-decls assignment-table))
(define-method (var-decls->var-values (module <sal-simple-data-flat-module>) (var-decls <primitive>) (assignment-table <primitive>))
  (bind-exit (exit)
    (let ((inv-map (create-inv-map module))
          (empty-env (make-empty-env)))
      (map (lambda (var-decl)
             (cond 
              ((eq-hash-table/get assignment-table var-decl) =>
               (lambda (entry)
                 (flat-data-ast->ast (cdr entry) empty-env inv-map)))
              (else
               (exit #f)))) ;; cannot handle this var-decl
           var-decls))))
(define-method (var-decls->var-values (module <sal-boolean-flat-module>) (var-decls <primitive>) (assignment-table <primitive>))
  (bind-exit (exit)
    (map (lambda (var-decl)
           (cond 
            ((eq-hash-table/get assignment-table var-decl) =>
             (lambda (entry)
               (cdr entry)))
            (else
             (exit #f)))) ;; cannot handle this var-decl
         var-decls)))

(define-generic (derived-value->value module var-id var-values))
(define-method (derived-value->value (module <sal-sliced-flat-module>) (var-id <primitive>) (var-values <primitive>))
  (derived-value->value (slot-value module :original-module) var-id var-values))
(define-method (derived-value->value (module <sal-flat-module>) (var-id <primitive>) (var-values <primitive>))
  [assert (var-values) (= (length var-values) 1)]
  (car var-values))
(define-method (derived-value->value (module <sal-boolean-flat-module>) (var-id <primitive>) (var-values <primitive>))
  (let* ((original-module (slot-value module :original-module))
         (var-decl (sal-module/lookup-var original-module var-id)))
    [assert (var-decl) var-decl]
    (bit-list->sal-value (slot-value var-decl :type) var-values)))
(define-method (derived-value->value (module <sal-simple-data-flat-module>) (var-id <primitive>) (var-values <primitive>))
  (let* ((original-module (slot-value module :original-module))
         (var-decl (sal-module/lookup-var original-module var-id)))
    [assert (var-decl) var-decl]
    (data-list->sal-value (slot-value var-decl :type) var-values)))


;; --------------------------------------------------------------------
;;
;; Recover transition step information...
;;
;; --------------------------------------------------------------------


(define-generic (sal-path/recover-executed-transition-info! path))

(define-method (sal-path/recover-executed-transition-info! (path <sal-path>))
  (sal-step-list/recover-executed-transition-info! (slot-value path :step-info-list) (slot-value path :flat-module)))

(define-method (sal-path/recover-executed-transition-info! (path <sal-cyclic-path>))
  (sal-step-list/recover-executed-transition-info! (slot-value path :step-info-list) (slot-value path :flat-module))
  (sal-step-list/recover-executed-transition-info! (slot-value path :cycle-step-info-list) (slot-value path :flat-module)))

(define (sal-step-list/recover-executed-transition-info! step-list flat-module)
  (let ((state-vars (slot-value flat-module :state-vars))
        (transition-trace-info (slot-value flat-module :transition-trace-info)))
    [assert (transition-trace-info) transition-trace-info]
    (unless (null? step-list)
      (let loop ((step-list step-list))
        (unless (null? (cdr step-list))
          (let* ((curr-step (car step-list))
                 (transition-step (sal-trace-info/recover-executed-transition-info transition-trace-info 
                                                                                   (slot-value curr-step :assignment-table)
                                                                                   flat-module)))
            (set-slot-value! curr-step :transition-step transition-step) 
            (loop (cdr step-list))))))))

(define (sal-trace-info/recover-executed-transition-info trace-info assignment-table flat-module)
  (if (and trace-info (sal-trace-info/contains-choice? trace-info))
    (let* ((choice-assignment-proc (lambda (choice-var-name)
                                     (cond
                                      ((sal-module/lookup-var flat-module choice-var-name) =>
                                       (lambda (decl)
                                         [assert (decl) (instance-of? decl <sal-choice-input-state-var-decl>)]
                                         (cond
                                          ((eq-hash-table/get assignment-table decl) => cdr)
                                          (else
                                           (make-sal-name-expr decl)))))
                                      (else (internal-error))))))
      (make-sal-transition-step trace-info choice-assignment-proc))
    #f))





