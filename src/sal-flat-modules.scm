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

(module sal-flat-modules
        (include "sal.sch")
        (import sal-module sal-ast-copy symbol-set sal-expression 
                sal-implicit-assignments sal-ast-env 
                sal-environment sal-ast-list sal-ast-for-each queue
                unique-names sal-assertion sal-type runtime sal-decls
                sal-type-membership sal-ast-simplify sal-ast-eq
                sal-expand-for-all-definitions sal-trace-info 
                sal-flat-modules-core)
        (export (sal-ast/flat-modules ast)
                (sal-ast/flat-modules-core ast env)
                (sal-command/flat-modules-core command env info synch-decl-list)
                (sal-module/flat-modules-core ast env info)
                <sal-command-expansion-info>)
        )

;; ******** FIX ME ********
;; The :skip section of the resultant flat-module
;; doesn't include the skip of GLOBAL variables.
;; This isn't a serious problem, since the :skip slot
;; is not used outside of this module


;;--------------------------------------------------------------------
;;
;; Support functions
;;
;;--------------------------------------------------------------------

(define (make-sal-sequence place-provider . list)
  (if (null? list)
    (make-sal-true place-provider)
    (apply make-sal-builtin-application <sal-and> place-provider list)))

(define (make-sal-choice place-provider . list)
  (if (null? list)
    (make-sal-true place-provider) ;; it is really true! no choice means skip instead of dead-lock
    (quick-change-class! (apply make-sal-builtin-application <sal-or> place-provider list)
                         <sal-choice>)))

(define (choice-vars->state-vars flat-module)
  (sal-module/set-state-variables! flat-module (append (slot-value flat-module :state-vars)
                                                       (slot-value flat-module :choice-vars)))
  (set-slot-value! flat-module :choice-vars '()))

;;--------------------------------------------------------------------
;;
;; Main flattening function
;;         
;;--------------------------------------------------------------------

(define-generic (sal-ast/flat-modules-core ast env))
(define-method (sal-ast/flat-modules-core (ast <sal-ast>) (env <primitive>))
  (sal-ast/map ast env sal-ast/flat-modules-core))
(define-method (sal-ast/flat-modules-core (ast <sal-type>) (env <primitive>))
  ;; it is not necessary to copy types... since they do not contain state variables.
  ast)
(define-method (sal-ast/flat-modules-core (ast <sal-next-operator>) (env <primitive>))
  (let ((lhs (sal-ast/flat-modules-core (slot-value ast :name-expr) env)))
    [assert (lhs) (sal-expr/lhs? lhs)]
    (sal-lhs->next-lhs lhs)))
(define-method (sal-ast/flat-modules-core (ast <sal-simple-definition>) (env <primitive>))
  (quick-change-class!
   (make-sal-builtin-application <sal-eq> ast
                                 (sal-ast/flat-modules-core (slot-value ast :lhs) env)
                                 (sal-ast/flat-modules-core (slot-value ast :rhs) env))
   <sal-assignment>))
(define-method (sal-ast/flat-modules-core (ast <sal-simple-selection-definition>) (env <primitive>))
  (let ((lhs (sal-ast/flat-modules-core (slot-value ast :lhs) env))
        (rhs (sal-ast/flat-modules-core (slot-value ast :rhs) env)))
    (make-ast-instance <sal-definition-expression> ast
                       :lhs-list (list lhs)
                       ;;
                       ;; In a previous version I was mapping simple-selection definitions to applications...
                       ;; that is, "x IN f" was mapped to "f(x)", unfortunately, this is transformation
                       ;; is not correct (actually it is "incomplete"). Assume the following example:
                       ;;
                       ;; LOCAL x : [0..7];
                       ;; DEFINITION
                       ;;   x IN { i : [2..4] | TRUE };
                       ;;
                       ;; In the previous implementation, this definition would be transformed into TRUE (wrong!)
                       ;; The correct result for this examples is (2 <= x AND x <= 4).
                       ;;
                       ;; Without this modification, we can produce false counterexamples.
                       ;; Consider the following simple example:
                       ;;
                       ;; in_bug : CONTEXT =
                       ;; BEGIN
                       ;;   main : MODULE =
                       ;;   BEGIN
                       ;;     LOCAL x : [0..7]
                       ;;     DEFINITION
                       ;;       x IN { i : [2..4] | TRUE }
                       ;;   END;
                       ;;
                       ;;   th : THEOREM main |- G(x<=4);
                       ;; END
                       ;; 
                       ;; Remark:
                       ;;   IN is also a builtin constant, and the expression "x IN f" is equivalent to "f(x)".
                       ;;   By the way, the following SAL expression is type incorrect unless 2<=n<=4:
                       ;;     LET x : [0..7] = n
                       ;;     IN (x IN { i : [2..4] | TRUE })
                       ;;
                       ;; SOLUTION:
                       ;;   f(x) AND "x in domain of f"
                       ;;
                       ;; Remark: If the type of "x" is equals to the type of "f", then I don't need the extra
                       ;;         expression.
                       :expr (let* ((fun rhs)
                                    (arg lhs)
                                    (place-provider ast)
                                    (basic-expr (make-ast-instance <sal-in> place-provider
                                                                  :arg arg
                                                                  :fun fun))
                                    (fun-type (sal-expr/type fun))
                                    (domain (sal-function-type/domain fun-type))
                                    (domain-expr (if (sal-ast/equivalent? (sal-expr/type arg)
                                                                          domain)
                                                   (make-sal-true place-provider)
                                                   (sal-type/membership-expr domain arg))))
                               (make-sal-and+ basic-expr domain-expr)))))

(define-method (sal-ast/flat-modules-core (ast <sal-for-all-definition>) (env <primitive>))
  (apply make-sal-sequence ast
         (map (lambda (definition)
                (make-ast-instance <sal-for-all-expr> ast
                                   :local-decls (slot-value ast :local-decls)
                                   :expr (sal-ast/flat-modules-core definition env)))
              (slot-value ast :definitions))))

(define-method (sal-ast/flat-modules-core (ast <sal-module>) (env <primitive>))
  (sal-module/flat-modules-core ast env (make-module-flattening-info)))

(define-method (sal-ast/flat-modules-core (ast <sal-module-models>) (env <primitive>))
  (let* ((module (slot-value ast :module))
         (state-vars (sal-module/state-variables module))
         (new-module (sal-ast/flat-modules-core module env))
         (new-state-vars (sal-module/state-variables new-module)))
    [assert (state-vars new-state-vars) (= (length state-vars) (length new-state-vars))]
    (choice-vars->state-vars new-module)
    (let ((env (update-env* env state-vars new-state-vars)))
      (update-ast-slots ast
                        :module new-module
                        :expr (sal-ast/flat-modules-core (slot-value ast :expr) env)))))

(define-method (sal-ast/flat-modules-core (ast <sal-module-implements>) (env <primitive>))
  (let ((new-module1 (sal-ast/flat-modules-core (slot-value ast :module1) env))
        (new-module2 (sal-ast/flat-modules-core (slot-value ast :module2) env)))
    (choice-vars->state-vars new-module1)
    (choice-vars->state-vars new-module2)
    (update-ast-slots ast
                      :module1 new-module1
                      :module2 new-module2)))

(define-method (sal-ast/flat-modules-core (ast <sal-qualified-assertion-name>) (env <primitive>))
  (sal-ast/flat-modules-core (sal-assertion-name/definition ast) env))

;;--------------------------------------------------------------------
;;
;; External API
;;         
;;--------------------------------------------------------------------
(define-generic (sal-ast/flat-modules ast))

(define-method (sal-ast/flat-modules (ast <sal-ast>))
  (sal-ast/flat-modules-core ast (make-empty-env)))

(define-method (sal-ast/flat-modules (ast <sal-module>))
  (status-message :building-flat-module)
  (verbose-message 1 "flattening module at ~a" (format-with-location ast ""))
  (display-runtime 2 "  module flattening time: ~a secs"
    (lambda ()
      (let ((result (sal-ast/flat-modules-core ast (make-empty-env))))
        (choice-vars->state-vars result)
        result))
    :flat-module-time))

(define-method (sal-ast/flat-modules (ast <sal-assertion-expr>))
  (status-message :bulding-flat-assertion)
  (verbose-message 1 "flattening modules in the assertion located at ~a" (format-with-location ast ""))
  (display-runtime 2 "  assertion flattening time: ~a secs"
    (lambda ()
      (sal-ast/flat-modules-core ast (make-empty-env)))
    :flat-assertion-time))

;;---------------------------------------------------------------------------
;;
;; Support functions to flatten guarded commands, multi-commands, labeled commands,
;;                              else commands
;;
;;---------------------------------------------------------------------------
     
(define-class <sal-command-expansion-info> () (:choice-var-decl
                                               :choice-idx
                                               :choice-vars-queue
                                               :transition?))

(define (make-sal-command-expansion-info choice-var-decl transition?)
  (make-instance <sal-command-expansion-info>
                 :choice-var-decl choice-var-decl
                 :choice-idx 0
                 :choice-vars-queue (if choice-var-decl
                                      (make-queue choice-var-decl)
                                      (make-queue))
                 :transition? transition?))

(define (generate-trace-info? expansion-info)
  (and (sal/trace-info-enabled?) (slot-value expansion-info :transition?)))


;; synch-decl-list is a list of variable declarations of multi-synchronous modules
(define-generic (sal-command/flat-modules-core command env info synch-decl-list))
(define-method (sal-command/flat-modules-core (command <sal-guarded-command>) (env <primitive>) (info <sal-command-expansion-info>) (synch-decl-list <primitive>))
  (let* ((place-provider command)
         (trace-aux-expr (if (and (slot-value info :transition?) (slot-value info :choice-var-decl))
                           (make-sal-choice-test (slot-value info :choice-var-decl) synch-decl-list
                                                 (make-sal-numeral (slot-value info :choice-idx) place-provider)
                                                 place-provider)
                           (make-sal-true place-provider)))
         (trace-info (if (slot-value info :transition?) (make-sal-transition-trace-info place-provider) #f)))
    (values (apply make-sal-sequence place-provider
                   trace-aux-expr
                   (sal-ast/flat-modules-core (slot-value command :guard) env)
                   (map (cut sal-ast/flat-modules-core <> env) (slot-value command :assignments)))
            trace-info)))
(define-method (sal-command/flat-modules-core (command <sal-labeled-command>) (env <primitive>) (info <sal-command-expansion-info>) (synch-decl-list <primitive>))
  (multiple-value-bind
      (expr trace-info)
      (sal-command/flat-modules-core (slot-value command :command) env info synch-decl-list)
    (values expr
            (if (slot-value info :transition?) (make-sal-labeled-trace-info command trace-info) #f))))


(define-method (sal-command/flat-modules-core (command <sal-multi-command>) (env <primitive>) (info <sal-command-expansion-info>) (synch-decl-list <primitive>))
  (multiple-value-bind
      (command-expr trace-info)
      (sal-command/flat-modules-core (slot-value command :command) env info synch-decl-list)
    (if (generate-trace-info? info)
      (multiple-value-bind
          (new-trace-info choice-decls)
          (make-sal-multi-command-choice-trace-info command synch-decl-list trace-info)
        (let* ((place-provider command)
               (local-decls (slot-value command :local-decls)) 
               (condition (apply make-sal-sequence place-provider
                                 (map (lambda (choice-decl local-decl)
                                        (make-sal-choice-test choice-decl synch-decl-list 
                                                              (make-sal-name-expr local-decl) 
                                                              place-provider))
                                      choice-decls
                                      local-decls)))
               (new-command-expr (make-ast-instance <sal-multi-choice-expr> place-provider
                                                    :local-decls local-decls
                                                    :expr (make-sal-sequence place-provider condition command-expr))))
          (queue/append! (slot-value info :choice-vars-queue) choice-decls)
          (values new-command-expr new-trace-info)))
      (values (make-ast-instance <sal-multi-choice-expr> command
                                 :local-decls (slot-value command :local-decls)
                                 :expr command-expr)
              #f))))

(define (sal-else-command/flat-modules-core else-command commands env info synch-decl-list)
  (let* ((place-provider else-command)
         (guards (map sal-command/guard commands))
         (pre-implicit-guard (make-sal-not (make-sal-or* (map (cut sal-ast/flat-modules-core <> env) guards) place-provider)))
         (implicit-guard (sal-ast/flat-modules-core pre-implicit-guard env))
         (trace-aux-expr (if (and (generate-trace-info? info) (slot-value info :choice-var-decl))
                           (make-sal-choice-test (slot-value info :choice-var-decl) synch-decl-list
                                                 (make-sal-numeral (slot-value info :choice-idx) place-provider)
                                                 place-provider)
                           (make-sal-true place-provider)))
         (trace-info (if (slot-value info :transition?) (sal-else-command/trace-info else-command) #f)))
    (values (apply make-sal-sequence place-provider
                   trace-aux-expr
                   implicit-guard
                   (map (cut sal-ast/flat-modules-core <> env) 
                        (sal-else-command/assignments else-command)))
            trace-info)))

(define (sal-command-section/flat-modules-core cmd-sec env synch-decl-list transition?)
  (let* ((choice-var-decl (make-sal-command-section-choice-var-decl cmd-sec synch-decl-list))
         (expansion-info (make-sal-command-expansion-info choice-var-decl transition?)))
    (multiple-value-bind
        (cmd-sec-expr trace-info)
        (sal/flat-command-section-core 
         cmd-sec env choice-var-decl transition?
         (cut sal-command/flat-modules-core <> env expansion-info synch-decl-list)
         (lambda () (sal-else-command/flat-modules-core (slot-value cmd-sec :else-command) (slot-value cmd-sec :commands) env expansion-info synch-decl-list))
         (lambda (choice-list place-provider)
           (apply make-sal-choice place-provider choice-list))
         (lambda ()
           (set-slot-value! expansion-info :choice-idx (+ (slot-value expansion-info :choice-idx) 1))))
      (values cmd-sec-expr trace-info (queue->list (slot-value expansion-info :choice-vars-queue))))))

;;---------------------------------------------------------------------------
;;
;; module flattening support functions
;;
;;---------------------------------------------------------------------------

(define (update-state-vars! ast flat-module)
  (sal-module/set-state-variables! flat-module (sal-module/state-variables ast))
  flat-module)

;;---------------------------------------------------------------------------
;;
;; base-module flattening support 
;;
;;---------------------------------------------------------------------------

(define (sal-base-module/preprocess base-module env)
  (let* ((m1 (sal-base-module/expand-for-all-definitions base-module env))
         (m2 (sal-base-module/insert-implicit-assignments m1)))
    m2))

(define (expand-def-and-cmds base-module env def-slot cmd-sec-slot synch-decl-list transition?)
  (let* ((place-provider base-module)
         (definitions (slot-value base-module def-slot))
         (cmd-sec (slot-value base-module cmd-sec-slot))
         (definition-exprs (map (cut sal-ast/flat-modules-core <> env) definitions)))
    (multiple-value-bind
        (cmd-sec-expr trace-info choice-vars)
        (if cmd-sec
          (sal-command-section/flat-modules-core cmd-sec env synch-decl-list transition?)
          (values (make-sal-true base-module) (make-ast-instance <sal-trace-info> place-provider) '()))
      (let ((result-expr (apply make-sal-sequence place-provider
                                (append definition-exprs (list cmd-sec-expr)))))
        (values result-expr trace-info choice-vars)))))

(define-generic (sal-definition/defined-variables ast))
(define-method (sal-definition/defined-variables (ast <sal-simple-definition>))
  (list (sal-name-ref/name (sal-lhs/name-expr (slot-value ast :lhs)))))
(define-method (sal-definition/defined-variables (ast <sal-for-all-definition>))
  (fold-left (lambda (curr definition)
               (append (sal-definition/defined-variables definition)
                       curr))
             '()
             (slot-value ast :definitions)))

(define (sal-base-module/defined-variables ast)
  (fold-left (lambda (curr definition)
               (fold-left (lambda (curr var-name)
                            (symbol-set/add curr var-name))
                          curr
                          (sal-definition/defined-variables definition)))
             (make-symbol-set)
             (slot-value ast :definitions)))

(define (sal-base-module/skip-variables ast)
  (let ((result (make-symbol-set))
        (defined-vars (sal-base-module/defined-variables ast)))
    (fold-left
     (lambda (lst decl)
       (let ((var-name (sal-decl/name decl)))
         (if (and (not (symbol-set/member? var-name defined-vars))
                  (not (instance-of? decl <sal-input-state-var-decl>))
                  (not (instance-of? decl <sal-global-state-var-decl>)))
           (cons decl lst)
           lst)))
     '()
     (sal-module/state-variables ast))))

(define (sal-var-decl/skip var-decl env place-provider)
  (let* ((name-expr (make-sal-name-expr var-decl place-provider))
         (next-name-expr (make-ast-instance <sal-next-operator> place-provider
                                            :name-expr name-expr))
         (flat-name-expr (sal-ast/flat-modules-core name-expr env))
         (flat-next-name-expr (sal-ast/flat-modules-core next-name-expr env)))
    (quick-change-class!
     (make-sal-builtin-application <sal-eq> place-provider
                                   flat-next-name-expr
                                   flat-name-expr)
     <sal-assignment>)))

(define (sal-base-module/skip ast env)
  (let ((state-vars (sal-module/state-variables ast))
        (skip-vars (sal-base-module/skip-variables ast))
        (place-provider ast))
    (apply make-sal-sequence place-provider
           (map (cut sal-var-decl/skip <> env place-provider) skip-vars))))

(define (sal-base-module/component-info ast env aux-env)
  (let ((place-provider ast)
        (state-vars (sal-module/state-variables ast))
        (input-data-queue (make-queue))
        (output-data-queue (make-queue))
        (owned-data-queue (make-queue)))
    (for-each (lambda (state-var-decl)
                (let* ((place-provider state-var-decl)
                       (name-expr (make-sal-name-expr state-var-decl place-provider))
                       (flat-name-expr (sal-ast/flat-modules-core name-expr env))
                       (data (sal-ast/substitute flat-name-expr aux-env)))
                  (cond
                   ((instance-of? state-var-decl <sal-input-state-var-decl>)
                    (queue/insert! input-data-queue data))
                   ((instance-of? state-var-decl <sal-output-state-var-decl>)
                    (queue/insert! output-data-queue data)
                    (queue/insert! owned-data-queue data))
                   ((instance-of? state-var-decl <sal-global-state-var-decl>)
                    (queue/insert! output-data-queue data)
                    (queue/insert! input-data-queue data))
                   (else
                    [assert (state-var-decl) (instance-of? state-var-decl <sal-local-state-var-decl>)]
                    (queue/insert! owned-data-queue data)))))
              state-vars)
    (make-ast-instance <sal-base-component-info> place-provider
                       :input-data (queue->list input-data-queue)
                       :output-data (queue->list output-data-queue)
                       :owned-data (queue->list owned-data-queue))))

;;---------------------------------------------------------------------------
;;
;; module flattening
;;
;;---------------------------------------------------------------------------

(define-class <module-flattening-info> () (:synch-decl-list ;; list of index-var-decls of multisynchronous modules
                                           ;; auxiliary environment used to create components, it basically
                                           ;; maps index-var-decls of multi-modules to index-var-decl of multi-components
                                           :aux-env))

(define (make-module-flattening-info)
  (make-instance <module-flattening-info> :synch-decl-list '() :aux-env (make-empty-env)))
                                                      
(define-generic (sal-module/flat-modules-core ast env info))

(define-method (sal-module/flat-modules-core (ast <sal-base-module>) (env <primitive>) (info <module-flattening-info>))
  (let* ((ast (sal-base-module/preprocess ast env))
         (synch-decl-list (slot-value info :synch-decl-list))
         (aux-env (slot-value info :aux-env))
         (component-info (sal-base-module/component-info ast env aux-env))
         (place-provider ast)
         (initialization-expr (expand-def-and-cmds ast env :initialization-definitions :initialization-command-section synch-decl-list #f)))
    (multiple-value-bind
        (transition-expr transition-trace-info choice-vars)
        (expand-def-and-cmds ast env :transition-definitions :transition-command-section synch-decl-list #t)
      (make-ast-instance <sal-flat-module> ast
                         :state-vars (sal-module/state-variables ast)
                         :definition (apply make-sal-sequence place-provider
                                            (map (cut sal-ast/flat-modules-core <> env) (slot-value ast :definitions)))
                         :initialization initialization-expr
                         :transition transition-expr
                         :skip (sal-base-module/skip ast env)
                         :transition-trace-info transition-trace-info
                         :choice-vars choice-vars
                         :component-info component-info
                         :valid-input-expr (make-sal-true ast)
                         :valid-state-expr (make-sal-true ast)
                         :valid-constant-expr (make-sal-true ast)))))

(define-method (sal-module/flat-modules-core (ast <sal-module-instance>) (env <primitive>) (info <module-flattening-info>))
  (let* ((instance (sal-module-instance/expand ast))
         (env (accumulate-env* env (sal-module/state-variables instance) (sal-module/state-variables ast)))
         (result (sal-module/flat-modules-core instance env info)))
    (update-state-vars! ast result)
    (sal/set-module-instance-trace-info! result ast)
    result))

(define-method (sal-module/flat-modules-core (ast <sal-org-module>) (env <primitive>) (info <module-flattening-info>))
  (let* ((module (slot-value ast :module))
         (env (accumulate-env* env (sal-module/state-variables module) (sal-module/state-variables ast))))
    (update-state-vars! ast (sal-module/flat-modules-core module env info))))

(define-method (sal-module/flat-modules-core (ast <sal-new-output>) (env <primitive>) (info <module-flattening-info>))
  (let* ((result-module (call-next-method))
         ;; I have to update the skip section with the global variables that became output variables...
         (old-globals (slot-value ast :identifiers))
         (skip (slot-value result-module :skip))
         (place-provider ast)
         (new-skip (apply make-sal-sequence place-provider
                          skip
                          (map (lambda (global-id)
                                 (let* ((global-name (sal-identifier/name global-id))
                                        (global-decl (sal-module/lookup-var result-module global-name)))
                                   (sal-var-decl/skip global-decl env place-provider)))
                               old-globals))))
    (update-ast-slots result-module
                      :skip new-skip)))

(define-method (sal-module/flat-modules-core (ast <sal-with-module>) (env <primitive>) (info <module-flattening-info>))
  (update-state-vars! ast (sal-module/flat-modules-core (slot-value ast :module) env info)))

(define-method (sal-module/flat-modules-core (ast <sal-renaming>) (env <primitive>) (info <module-flattening-info>))
  (let* ((module (slot-value ast :module))
         (renames (slot-value ast :renames))
         (to-expr-list (map (lambda (rename) (sal-ast/flat-modules-core (slot-value rename :to-expr) env)) renames))
         (from-decl-list (map (lambda (rename) 
                                (sal-module/lookup-var module (sal-identifier/name (slot-value rename :from-name))))
                              renames))
         (env (accumulate-env* env from-decl-list to-expr-list))
         (result (sal-module/flat-modules-core module env info)))
    (update-state-vars! ast result)
    (trace 'flat-modules "expanding rename ~a --- ~a" (sal-ast->list ast) (sal-ast->list result))
    result))

(define (flat-children ast env info asynch?)
  (sal/flat-children ast env asynch? (lambda (ast env)
                                       (sal-module/flat-modules-core ast env info))))

(define (combine-synch-section flat1 flat2 slot-id)
  (let ((place-provider flat1))
    (make-sal-sequence place-provider
                       (slot-value flat1 slot-id) 
                       (slot-value flat2 slot-id))))

(define (combine-synch-section* flat-modules slot-id)
  (let ((place-provider (car flat-modules)))
    (apply make-sal-sequence place-provider
           (map (cut slot-value <> slot-id) flat-modules))))

;; return an expression that performs a "skip" of the global variable
;; of the other modules. 
(define (skip-other-globals flat-module all-state-variables env)
  (let ((place-provider flat-module)
        (non-used-global-skip-queue (make-queue)))
    (for-each (lambda (state-var-decl)
                (when (instance-of? state-var-decl <sal-global-state-var-decl>)
                  (let ((other-var (sal-module/lookup-var flat-module (sal-decl/name state-var-decl))))
                    (when (or (not other-var) ;; I do not use the variable
                              (not (instance-of? other-var <sal-global-state-var-decl>))) ;; It is not a global...
                      (queue/insert! non-used-global-skip-queue
                                     (sal-var-decl/skip state-var-decl env place-provider))))))
              all-state-variables)
    (apply make-sal-sequence place-provider
           (queue->list non-used-global-skip-queue))))

(define (skip-other-modules flat-module all-flat-modules)
  (let ((place-provider flat-module))
    (apply make-sal-sequence place-provider
           (map-and-filter (lambda (other-flat-module)
                             (and (not (eq? other-flat-module flat-module))
                                  (slot-value other-flat-module :skip)))
                           all-flat-modules))))

(define-method (sal-module/flat-modules-core (ast <sal-asynch-composition>) (env <primitive>) (info <module-flattening-info>))
  (let* ((place-provider ast)
         (flat-modules (flat-children ast env info #t))
         (new-definition (combine-synch-section* flat-modules :definition))
         (new-initialization (combine-synch-section* flat-modules :initialization))
         (new-skip (combine-synch-section* flat-modules :skip))
         (transitions (map (lambda (curr-flat-module)
                             (make-sal-sequence place-provider
                                                (slot-value curr-flat-module :transition)
                                                (skip-other-modules curr-flat-module flat-modules)
                                                (skip-other-globals curr-flat-module (sal-module/state-variables ast) env)))
                           flat-modules))
         (composite-component (make-ast-instance <sal-composite-component-info> place-provider
                                                 :components (map (cut slot-value <> :component-info) flat-modules)))
         (result (copy-ast (car flat-modules)
                           :component-info composite-component
                           :initialization new-initialization
                           :definition new-definition
                           :transition (apply make-sal-choice place-provider transitions)
                           :skip new-skip)))
      (when (sal/trace-info-enabled?)
        (multiple-value-bind
            (new-trace-info new-choice-var-decl new-choice-vars)
            (make-sal-asynch-trace-info flat-modules (slot-value info :synch-decl-list) place-provider)
          (let ((new-transitions (let loop ((transitions transitions)
                                            (i 0))
                                   (if (null? transitions)
                                     '()
                                     (let* ((curr-transition (car transitions))
                                            (rest (loop (cdr transitions) (+ i 1)))
                                            (trace-condition (make-sal-choice-test new-choice-var-decl
                                                                                   (slot-value info :synch-decl-list)
                                                                                   (make-sal-numeral i place-provider)
                                                                                   place-provider))
                                            (new-transition (make-sal-sequence place-provider
                                                                               trace-condition
                                                                               curr-transition)))
                                       (cons new-transition rest))))))
            (set-slot-value! result :transition-trace-info new-trace-info)
            (set-slot-value! result :choice-vars new-choice-vars)
            (set-slot-value! result :transition (apply make-sal-choice place-provider new-transitions)))))
      (update-state-vars! ast result)
      result))

(define-method (sal-module/flat-modules-core (ast <sal-synch-composition>) (env <primitive>) (info <module-flattening-info>))
  (let* ((place-provider ast)
         (children-list (flat-children ast env info #f))
         (_ [assert (children-list) (= (length children-list) 2)])
         (flat1 (car children-list))
         (flat2 (cadr children-list))
         (composite-component (make-ast-instance <sal-composite-component-info> place-provider
                                                 :components (list (slot-value flat1 :component-info)
                                                                   (slot-value flat2 :component-info))))
         (result (copy-ast flat1
                           :component-info composite-component
                           :definition (combine-synch-section flat1 flat2 :definition)
                           :transition (combine-synch-section flat1 flat2 :transition)
                           :initialization (combine-synch-section flat1 flat2 :initialization)
                           :skip (combine-synch-section flat1 flat2 :skip))))
    (sal/set-seq-trace-info-based-on-children! result flat1 flat2 place-provider)
    (update-state-vars! ast result)
    result))

(define (flat-multi-child ast env info)
  (sal/flat-multi-child ast env (lambda (child new-env)
                                  (sal-module/flat-modules-core child new-env info))))

(define (combine-multi-synch-section flat local-decl slot-id)
  (let ((expr (slot-value flat slot-id)))
    (make-ast-instance <sal-for-all-expr> expr
                       :local-decls (list local-decl)
                       :expr expr)))

(define-method (sal-module/flat-modules-core (ast <sal-multi-asynch-composition>) (env <primitive>) (info <module-flattening-info>))
  (let* ((place-provider ast)
         (local-decl (car (slot-value ast :local-decls)))
         (component-local-decl (copy-ast local-decl))
         (aux-env (slot-value info :aux-env))
         (new-aux-env (update-env aux-env local-decl component-local-decl))
         (new-info (copy-instance info :aux-env new-aux-env))
         (flat-child (flat-multi-child ast env new-info))
         (component (slot-value flat-child :component-info))
         (multi-component (make-ast-instance <sal-multi-component-info> place-provider
                                             :local-decls (list component-local-decl)
                                             :component component))
         (id (slot-value local-decl :id))
         (new-id (copy-ast id :name (gen-unique-name (slot-value id :name))))
         (local-decl2 (copy-ast local-decl 
                                :id new-id))
         (ref1 (make-sal-name-expr local-decl place-provider))
         (ref2 (make-sal-name-expr local-decl2 place-provider))
         (other-skip (sal-ast/substitute (slot-value flat-child :skip) (update-env (make-empty-env) local-decl ref2)))
         (synch-decl-list (slot-value info :synch-decl-list))
         (choice-var-decl (make-sal-choice-var-decl (slot-value local-decl :type) synch-decl-list place-provider))
         (choice-test (make-sal-choice-test choice-var-decl synch-decl-list 
                                            (make-sal-name-expr local-decl place-provider)
                                            place-provider)))
    (update-state-vars!
     ast
     (copy-ast flat-child
               :component-info multi-component
               :definition (combine-multi-synch-section flat-child local-decl :definition)
               :initialization (combine-multi-synch-section flat-child local-decl :initialization)
               :skip (combine-multi-synch-section flat-child local-decl :skip)
               ;; The non-shared global variables bug do not affect multi-asynch composition
               ;; since all global variables are shared in a multi-asynch composition
               :transition (make-ast-instance <sal-multi-choice-expr> place-provider
                                              :local-decls (list local-decl)
                                              :expr (make-sal-sequence place-provider
                                                                       choice-test
                                                                       (slot-value flat-child :transition)
                                                                       (make-ast-instance <sal-for-all-expr> place-provider
                                                                                          :local-decls (list local-decl2)
                                                                                          :expr (make-sal-or
                                                                                                 (make-sal-equality ref1 ref2)
                                                                                                 other-skip))))
               :choice-vars (if choice-var-decl
                              (cons choice-var-decl (slot-value flat-child :choice-vars))
                              (slot-value flat-child :choice-vars))
               :transition-trace-info (make-sal-multi-choice-trace-info choice-var-decl 
                                                                        local-decl 
                                                                        (slot-value flat-child :transition-trace-info)
                                                                        place-provider)))))

(define-method (sal-module/flat-modules-core (ast <sal-multi-synch-composition>) (env <primitive>) (info <module-flattening-info>))
  (let* ((place-provider ast)
         (local-decl (car (slot-value ast :local-decls)))
         (component-local-decl (copy-ast local-decl))
         (synch-decl-list (slot-value info :synch-decl-list))
         (aux-env (slot-value info :aux-env))
         (new-aux-env (update-env aux-env local-decl component-local-decl))
         (new-info (copy-instance info 
                                  :synch-decl-list (cons local-decl synch-decl-list)
                                  :aux-env new-aux-env))
         (flat-child (flat-multi-child ast env new-info))
         (component (slot-value flat-child :component-info))
         (multi-component (make-ast-instance <sal-multi-component-info> place-provider
                                             :local-decls (list component-local-decl)
                                             :component component)))
    (update-state-vars!
     ast
     (copy-ast flat-child
               :component-info multi-component
               :definition (combine-multi-synch-section flat-child local-decl :definition)
               :initialization (combine-multi-synch-section flat-child local-decl :initialization)
               :transition (combine-multi-synch-section flat-child local-decl :transition)
               :skip (combine-multi-synch-section flat-child local-decl :skip)
               :transition-trace-info (make-sal-multi-sequence-trace-info local-decl
                                                                          (slot-value flat-child :transition-trace-info)
                                                                          place-provider)))))
