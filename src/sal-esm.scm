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

(module sal-esm
        (include "sal.sch")
        (import sal-module sal-ast-copy sal-ast-env polarity sal-expression
                queue sal-trace-info sal-flat-modules-core sal-ast-simplify
                sal-type-membership sal-ast-eq sal-type sal-decls sal-expr-evaluator
                unique-names gmp-scheme runtime sal-assertion)
        (export <sal-to-esm-context>
                (sal-esm/make-esm-choice c1 c2 place-provider)
                (sal-esm/make-esm-choice* clist place-provider)
                (sal-esm/make-esm-seq c1 c2 place-provider)
                (sal-esm/make-esm-seq* clist place-provider)
                (sal->esm ast)
                (sal->esm-core ast env ctx))
        )

(define-class <sal-to-esm-context> () (:synch-decl-list
                                       :choice-var-decl
                                       :choice-idx
                                       :choice-vars-queue
                                       :transition?))

(define (mk-sal-to-esm-context)
  (make-instance <sal-to-esm-context> :synch-decl-list '()))

(define-generic (sal->esm ast))

(define (append-choice-vars! module)
  (when (slot-value module :choice-vars)
    (set-slot-value! module :state-vars (append (slot-value module :state-vars) (slot-value module :choice-vars)))))
  
(define-method (sal->esm (ast <sal-module>))
  (status-message :esm-flat-module)
  (verbose-message 1 "flattening module at ~a" (format-with-location ast ""))
  (display-runtime 2 "  module flattening time: ~a secs"
    (lambda ()
      (let ((result (sal->esm-core ast (make-empty-env) (mk-sal-to-esm-context))))
        (append-choice-vars! result)
        result))
    :esm-flat-module-time))

(define-method (sal->esm (ast <sal-assertion-expr>))
  (status-message :esm-flat-assertion)
  (verbose-message 1 "flattening modules in the assertion located at ~a" (format-with-location ast ""))
  (display-runtime 2 "  assertion flattening time: ~a secs"
    (lambda ()
      (sal->esm-core ast (make-empty-env) (mk-sal-to-esm-context)))
    :esm-flat-assertion-time))

(define-macro (sal-esm/make-esm-core* class add-to-queue-proc)
  `(bind-exit (exit)
     (let ((result (make-queue)))
       (for-each (lambda (cmd)
                   (unless (,add-to-queue-proc cmd result)
                     (exit #f)))
                 clist)
       (cond
        ((queue/empty? result)
         #f)
        ((= (queue/length result) 1)
         (queue/front result))
        (else
         (make-ast-instance ,class place-provider
                            :statements (queue->list result)))))))

(define-generic (dummy-seq-cmd? esm))
;; (define-method (dummy-seq-cmd? (esm <sal-gu

(define (sal-esm/make-esm-choice* clist place-provider)
  (sal-esm/make-esm-core* <sal-esm-choice> sal-esm/add-choice-to-queue))

(define (sal-esm/make-esm-seq* clist place-provider)
  (sal-esm/make-esm-core* <sal-esm-seq> sal-esm/add-seq-to-queue))

(define (sal-esm/make-esm-choice c1 c2 place-provider)
  (sal-esm/make-esm-choice* (list c1 c2) place-provider))

(define (sal-esm/make-esm-seq c1 c2 place-provider)
  (sal-esm/make-esm-seq* (list c1 c2) place-provider))
      
(define-generic (sal-esm/add-choice-to-queue cmd result))

(define-method (sal-esm/add-choice-to-queue (cmd <primitive>) (result <primitive>))
  [assert (cmd) (not cmd)]
  #t)

(define-method (sal-esm/add-choice-to-queue (cmd <sal-ast>) (result <primitive>))
  (queue/insert! result cmd)
  #t)

(define-method (sal-esm/add-choice-to-queue (cmd <sal-esm-guard>) (result <primitive>))
  (cond
   ((sal-expr/false? (slot-value cmd :expr))
    ;; ignore it
    #t)
   ((sal-expr/true? (slot-value cmd :expr))
    ;; stop... the whole stuff is true
    #f)
   (else
    (queue/insert! result cmd))))

(define-method (sal-esm/add-choice-to-queue (cmd <sal-esm-choice>) (result <primitive>))
  (queue/append! result (slot-value cmd :statements)))

(define-method (sal-esm/add-choice-to-queue (cmd <sal-false>) (result <primitive>))
  ;; do nothing
  #unspecified)

(define-generic (sal-esm/add-seq-to-queue cmd result))

(define-method (sal-esm/add-seq-to-queue (cmd <primitive>) (result <primitive>))
  [assert (cmd) (not cmd)]
  #unspecified)

(define-method (sal-esm/add-seq-to-queue (cmd <sal-ast>) (result <primitive>))
  (queue/insert! result cmd))

(define-method (sal-esm/add-seq-to-queue (cmd <sal-esm-guard>) (result <primitive>))
  (cond
   ((sal-expr/false? (slot-value cmd :expr))
    ;; stop... the whole stuff is false
    #f)
   ((sal-expr/true? (slot-value cmd :expr))
    ;; ignore it
    #t)
   (else
    (queue/insert! result cmd))))

(define-method (sal-esm/add-seq-to-queue (cmd <sal-esm-seq>) (result <primitive>))
  (queue/append! result (slot-value cmd :statements)))

(define-method (sal-esm/add-seq-to-queue (cmd <sal-true>) (result <primitive>))
  ;; do nothing
  #unspecified)

(define (update-state-vars! ast esm-module)
  (sal-module/set-state-variables! esm-module (sal-module/state-variables ast))
  esm-module)

(define-generic (sal->esm-core ast env ctx))

(define-method (sal->esm-core (ast <sal-ast>) (env <primitive>) (ctx <sal-to-esm-context>))
  (sal-ast/map ast env (lambda (child new-env) (sal->esm-core child new-env ctx))))

(define-method (sal->esm-core (ast <sal-next-operator>) (env <primitive>) (ctx <sal-to-esm-context>))
  (let ((lhs (sal->esm-core (slot-value ast :name-expr) env ctx)))
    (sal-lhs->next-lhs lhs)))

(define-method (sal->esm-core (ast <sal-base-module>) (env <primitive>) (ctx <sal-to-esm-context>))
  (let ((place-provider ast)
        (initialization (sal-def-and-cmds->esm ast env ctx :initialization-definitions :initialization-command-section #f)))
    (multiple-value-bind
        (transition transition-trace-info choice-vars)
        (sal-def-and-cmds->esm ast env ctx :transition-definitions :transition-command-section #t)
      (make-ast-instance <sal-esm-module> place-provider
                         :state-vars (sal-module/state-variables ast)
                         :definition (sal-esm/make-esm-seq* (map (cut sal-definition->esm <> env ctx) 
                                                                 (slot-value ast :definitions))
                                                            place-provider)
                         :initialization initialization
                         :transition transition
                         :transition-trace-info transition-trace-info
                         :choice-vars choice-vars))))

(define (sal-def-and-cmds->esm base-module env ctx def-slot cmd-sec-slot transition?)
  (let* ((place-provider base-module)
         (definitions (slot-value base-module def-slot))
         (cmd-sec (slot-value base-module cmd-sec-slot))
         (definition-exprs (map (cut sal-definition->esm <> env ctx) definitions)))
    (set-slot-value! ctx :choice-vars-queue (make-queue))
    (set-slot-value! ctx :choice-idx 0)
    (set-slot-value! ctx :transition? transition?)
    (multiple-value-bind
        (cmd-sec-expr trace-info)
        (if cmd-sec
          (sal-command-section->esm cmd-sec env ctx transition?)
          (values #f (make-ast-instance <sal-trace-info> place-provider)))
      (let ((result-expr (sal-esm/make-esm-seq* (append definition-exprs (list cmd-sec-expr)) place-provider)))
        ;; (breakpoint "sal-def-and-cmds->esm" (ctx) #t)
        (values result-expr trace-info (queue->list (slot-value ctx :choice-vars-queue)))))))

(define (sal-command-section->esm cmd-sec env ctx transition?)
  (let ((choice-var-decl (make-sal-command-section-choice-var-decl cmd-sec (slot-value ctx :synch-decl-list))))
    (set-slot-value! ctx :choice-var-decl choice-var-decl)
    (when choice-var-decl
      (queue/insert! (slot-value ctx :choice-vars-queue) choice-var-decl))
    (sal/flat-command-section-core 
     cmd-sec env choice-var-decl transition?
     (cut sal-cmd->esm <> env ctx)
     (lambda () (sal-else-cmd->esm (slot-value cmd-sec :else-command) (slot-value cmd-sec :commands) env ctx))
     sal-esm/make-esm-choice*
     (lambda ()
       (set-slot-value! ctx :choice-idx (+ (slot-value ctx :choice-idx) 1))))))
  
(define-generic (sal-cmd->esm cmd env ctx))

(define-method (sal-cmd->esm (command <sal-guarded-command>) (env <primitive>) (ctx <sal-to-esm-context>))
  (let* ((place-provider command)
         (trace-aux-expr (if (and (slot-value ctx :transition?) (slot-value ctx :choice-var-decl))
                           (make-sal-choice-esm-assignment (slot-value ctx :choice-var-decl) 
                                                           (slot-value ctx :synch-decl-list)
                                                           (make-sal-numeral (slot-value ctx :choice-idx) place-provider)
                                                           place-provider)
                           #f))
         (trace-info (if (slot-value ctx :transition?) (make-sal-transition-trace-info place-provider) #f)))
    (values (sal-esm/make-esm-seq* 
             (cons* trace-aux-expr
                    (sal-guard->esm (slot-value command :guard) env ctx *pos*)
                    (map (cut sal-definition->esm <> env ctx) (slot-value command :assignments)))
             place-provider)
            trace-info)))

(define-method (sal-cmd->esm (command <sal-labeled-command>) (env <primitive>) (ctx <sal-to-esm-context>))
  (multiple-value-bind
      (expr trace-info)
      (sal-cmd->esm (slot-value command :command) env ctx)
    (values expr
            (if (slot-value ctx :transition?) (make-sal-labeled-trace-info command trace-info) #f))))

(define-method (sal-cmd->esm (command <sal-multi-command>) (env <primitive>) (ctx <sal-to-esm-context>))
  (multiple-value-bind
      (command-expr trace-info)
      (sal-cmd->esm (slot-value command :command) env ctx)
    (if (and (sal/trace-info-enabled?) (slot-value ctx :transition?))
      (multiple-value-bind
          (new-trace-info choice-decls)
          (make-sal-multi-command-choice-trace-info command (slot-value ctx :synch-decl-list) trace-info)
        (let* ((place-provider command)
               (local-decls (slot-value command :local-decls)) 
               (condition (sal-esm/make-esm-seq*
                           (map (lambda (choice-decl local-decl)
                                  (make-sal-choice-esm-assignment
                                   choice-decl (slot-value ctx :synch-decl-list)
                                   (make-sal-name-expr local-decl) 
                                   place-provider))
                                choice-decls
                                local-decls)
                           place-provider))
               (new-command-expr (make-ast-instance <sal-esm-multi-choice> command
                                                    :local-decls local-decls
                                                    :statement (sal-esm/make-esm-seq condition command-expr place-provider))))
          (queue/append! (slot-value ctx :choice-vars-queue) choice-decls)
          (values new-command-expr new-trace-info)))
      (values (make-ast-instance <sal-esm-multi-choice> command
                                 :local-decls (slot-value command :local-decls)
                                 :statement command-expr)
              #f))))

(define-generic (sal-guard->esm ast env ctx pol))

(define-method (sal-guard->esm (ast <sal-ast>) (env <primitive>) (ctx <sal-to-esm-context>) (pol <pos>))
  (make-ast-instance <sal-esm-guard> ast
                     :expr (sal->esm-core ast env ctx)))

(define-method (sal-guard->esm (ast <sal-ast>) (env <primitive>) (ctx <sal-to-esm-context>) (pol <neg>))
  (make-ast-instance <sal-esm-guard> ast
                     :expr (make-sal-not (sal->esm-core ast env ctx))))

(define-method (sal-guard->esm (ast <sal-not>) (env <primitive>) (ctx <sal-to-esm-context>) (pol <polarity>))
  (sal-guard->esm (slot-value ast :arg) env ctx (polarity/invert pol)))

(define-inline (make-guard-seq ast env ctx pol)
  (sal-esm/make-esm-seq* (map (cut sal-guard->esm <> env ctx pol) (sal-application/argument-list ast))
                         ast))

(define-method (sal-guard->esm (ast <sal-and>) (env <primitive>) (ctx <sal-to-esm-context>) (pol <pos>))
  (make-guard-seq ast env ctx pol))

(define-method (sal-guard->esm (ast <sal-or>) (env <primitive>) (ctx <sal-to-esm-context>) (pol <neg>))
  (make-guard-seq ast env ctx pol))

(define-inline (make-guard-multi-seq ast env ctx pol)
  (make-ast-instance <sal-esm-multi-seq> ast
                     :local-decls (slot-value ast :local-decls)
                     :statement (sal-guard->esm (slot-value ast :expr) env ctx pol)))
  
(define-method (sal-guard->esm (ast <sal-for-all-expr>) (env <primitive>) (ctx <sal-to-esm-context>) (pol <pos>))
  (make-guard-multi-seq ast env ctx pol))

(define-method (sal-guard->esm (ast <sal-exists-expr>) (env <primitive>) (ctx <sal-to-esm-context>) (pol <neg>))
  (make-guard-multi-seq ast env ctx pol))

(define-generic (sal-definition->esm ast env ctx))

(define-method (sal-definition->esm (ast <sal-simple-definition>) (env <primitive>) (ctx <sal-to-esm-context>))
  (make-ast-instance <sal-esm-assignment> ast
                     :lhs (sal->esm-core (slot-value ast :lhs) env ctx)
                     :rhs (sal->esm-core (slot-value ast :rhs) env ctx)))

(define-method (sal-definition->esm (ast <sal-simple-selection-definition>) (env <primitive>) (ctx <sal-to-esm-context>))
  (let ((place-provider ast)
        (lhs (sal->esm-core (slot-value ast :lhs) env ctx))
        (rhs (sal->esm-core (slot-value ast :rhs) env ctx)))
    (cond
     ((instance-of? rhs <sal-set-list-expr>)
      (let ((basic-expr (sal-expr/apply rhs lhs)))
        (cond 
         ((instance-of? basic-expr <sal-eq>)
          (make-ast-instance <sal-esm-assignment> place-provider
                             :lhs lhs
                             :rhs (sal-binary-application/arg2 basic-expr)))
         ((instance-of? basic-expr <sal-or>)
          (sal-esm/make-esm-choice* (map (lambda (assignment)
                                           [assert (assignment) (instance-of? assignment <sal-eq>)]
                                           (make-ast-instance <sal-esm-assignment> place-provider
                                                              :lhs lhs
                                                              :rhs (sal-binary-application/arg2 assignment)))
                                         (sal-application/argument-list basic-expr))
                                    place-provider))
         (else
          (internal-error)))))
     (else
      (let* ((fun-type (sal-expr/type rhs))
             (domain1 (sal-function-type/domain fun-type))
             (domain2 (sal-expr/type lhs))
             (domain1-size (sal-type/number-of-elements domain1))
             (domain2-size (sal-type/number-of-elements domain2)))
        ;; (breakpoint "xxx" (domain1 domain2 domain1-size domain2-size) #t)
        (cond
         ((or (and (mpq? domain1-size)
                   (mpq? domain2-size)
                   (<=mpq domain1-size domain2-size))
              (not (mpq? domain2-size)))
          (selection->multi-choice lhs rhs env ctx domain1 #f place-provider))
         (else
          (selection->multi-choice lhs rhs env ctx domain2 domain1 place-provider))))))))

(define (selection->multi-choice lhs rhs env ctx domain1 domain2 place-provider)
  (let* ((membership-expr (if domain2 
                            (sal-type/membership-expr domain2 lhs)
                            (make-sal-true place-provider)))
         (idx (gen-unique-name 'idx))
         (idx-id (make-sal-identifier place-provider idx))
         (idx-decl (make-ast-instance <sal-var-decl> place-provider
                                      :id idx-id
                                      :type domain1))
         (idx-name (make-sal-name-expr idx-decl place-provider))
         (basic-expr (sal-expr/apply rhs idx-name))
         (lhs-eq-idx (make-ast-instance <sal-esm-assignment> place-provider
                                        :lhs lhs 
                                        :rhs idx-name)))
    (make-ast-instance <sal-esm-multi-choice> place-provider
                       :local-decls (list idx-decl)
                       :statement (sal-esm/make-esm-seq (sal-guard->esm (make-sal-and+ membership-expr basic-expr) env ctx *pos*)
                                                        lhs-eq-idx
                                                        place-provider))))

(define-method (sal-definition->esm (ast <sal-for-all-definition>) (env <primitive>) (ctx <sal-to-esm-context>))
  (make-ast-instance <sal-esm-multi-seq> ast
                     :local-decls (slot-value ast :local-decls)
                     :statement (sal-esm/make-esm-seq* (map (cut sal-definition->esm <> env ctx)
                                                            (slot-value ast :definitions))
                                                       ast)))


(define-generic (sal-else-cmd->esm ast other-cmds env ctx))

(define-method (sal-else-cmd->esm (ast <sal-else-command>) (other-cmds <primitive>) (env <primitive>) 
                                  (ctx <sal-to-esm-context>))
  (let* ((place-provider ast)
         (guards (map sal-command/guard other-cmds))
         (pre-implicit-guard (make-sal-not (make-sal-or* guards place-provider)))
         (implicit-guard (sal-guard->esm pre-implicit-guard env ctx *pos*))
         (trace-aux-expr (if (and (sal/trace-info-enabled?)
                                  (slot-value ctx :transition?)
                                  (slot-value ctx :choice-var-decl))
                           (make-sal-choice-esm-assignment (slot-value ctx :choice-var-decl)
                                                           (slot-value ctx :synch-decl-list)
                                                           (make-sal-numeral (slot-value ctx :choice-idx) place-provider)
                                                           place-provider)
                           #f))
         (trace-info (if (slot-value ctx :transition?) (sal-else-command/trace-info ast) #f)))
    (values (sal-esm/make-esm-seq* (cons*
                                    trace-aux-expr
                                    implicit-guard
                                    (map (cut sal-definition->esm <> env ctx) 
                                         (sal-else-command/assignments ast)))
                                   place-provider)
            trace-info)))

(define-method (sal->esm-core (ast <sal-module-instance>) (env <primitive>) (ctx <sal-to-esm-context>))
  (let* ((instance (sal-module-instance/expand ast))
         (env (accumulate-env* env (sal-module/state-variables instance) (sal-module/state-variables ast)))
         (result (sal->esm-core instance env ctx)))
    (update-state-vars! ast result)
    (sal/set-module-instance-trace-info! result ast)
    result))

(define-method (sal->esm-core (ast <sal-renaming>) (env <primitive>) (ctx <sal-to-esm-context>))
  (let* ((module (slot-value ast :module))
         (renames (slot-value ast :renames))
         (to-expr-list (map (lambda (rename) (sal->esm-core (slot-value rename :to-expr) env ctx)) renames))
         (from-decl-list (map (lambda (rename) 
                                (sal-module/lookup-var module (sal-identifier/name (slot-value rename :from-name))))
                              renames))
         (new-env (accumulate-env* env from-decl-list to-expr-list))
         (result (sal->esm-core module new-env ctx)))
    (update-state-vars! ast result)))

(define-method (sal->esm-core (ast <sal-with-module>) (env <primitive>) (ctx <sal-to-esm-context>))
  (update-state-vars! ast (sal->esm-core (slot-value ast :module) env ctx)))
  
(define-method (sal->esm-core (ast <sal-org-module>) (env <primitive>) (ctx <sal-to-esm-context>))
  (let* ((module (slot-value ast :module))
         (new-env (accumulate-env* env (sal-module/state-variables module) (sal-module/state-variables ast))))
    (update-state-vars! ast (sal->esm-core module new-env ctx))))

(define (flat-children ast env ctx asynch?)
  (sal/flat-children ast env asynch? (lambda (ast env)
                                       (sal->esm-core ast env ctx))))

(define (combine-seq-section flat1 flat2 slot place-provider)
  (sal-esm/make-esm-seq (slot-value flat1 slot) 
                        (slot-value flat2 slot)
                        place-provider))

(define-method (sal->esm-core (ast <sal-synch-composition>) (env <primitive>) (ctx <sal-to-esm-context>))
  (let* ((place-provider ast)
         (flat-modules (flat-children ast env ctx #f))
         (_ [assert (flat-modules) (= (length flat-modules) 2)])
         (flat1 (car flat-modules))
         (flat2 (cadr flat-modules))
         (result (make-ast-instance <sal-esm-module> place-provider
                                    :definition (combine-seq-section flat1 flat2 :definition place-provider)
                                    :initialization (combine-seq-section flat1 flat2 :initialization place-provider)
                                    :transition (combine-seq-section flat1 flat2 :transition place-provider))))
    (sal/set-seq-trace-info-based-on-children! result flat1 flat2 place-provider)
    (update-state-vars! ast result)
    result))

(define (combine-seq-section* flat-modules slot place-provider)
  (sal-esm/make-esm-seq* (map (cut slot-value <> slot) flat-modules)
                         place-provider))

(define-method (sal->esm-core (ast <sal-asynch-composition>) (env <primitive>) (ctx <sal-to-esm-context>))
  (let* ((place-provider ast)
         (flat-modules (flat-children ast env ctx #t))
         (new-definition (combine-seq-section* flat-modules :definition place-provider))
         (new-initialization (combine-seq-section* flat-modules :initialization place-provider))
         (transitions (map (cut slot-value <> :transition) flat-modules))
         (result (make-ast-instance <sal-esm-module> place-provider
                                    :initialization new-initialization
                                    :definition new-definition
                                    :transition #f)))
    (cond
     ((sal/trace-info-enabled?)
      (multiple-value-bind
          (new-trace-info new-choice-var-decl new-choice-vars)
          (make-sal-asynch-trace-info flat-modules (slot-value ctx :synch-decl-list) place-provider)
        (let ((new-transitions (let loop ((transitions transitions)
                                          (i 0))
                                 (if (null? transitions)
                                   '()
                                   (let* ((curr-transition (car transitions))
                                          (rest (loop (cdr transitions) (+ i 1)))
                                          (trace-condition 
                                           (make-sal-choice-esm-assignment new-choice-var-decl
                                                                           (slot-value ctx :synch-decl-list)
                                                                           (make-sal-numeral i place-provider)
                                                                           place-provider))
                                          (new-transition (sal-esm/make-esm-seq trace-condition
                                                                                curr-transition
                                                                                curr-transition)))
                                     (cons new-transition rest))))))
          (set-slot-value! result :transition-trace-info new-trace-info)
          (set-slot-value! result :choice-vars new-choice-vars)
          (set-slot-value! result :transition (sal-esm/make-esm-choice* new-transitions place-provider)))))
     (else
      (set-slot-value! result :transition (sal-esm/make-esm-choice* transitions place-provider))))
    (update-state-vars! ast result)
    result))

(define (flat-multi-child ast env ctx)
  (sal/flat-multi-child ast env (lambda (child new-env)
                                  (sal->esm-core child new-env ctx))))

(define (combine-multi-synch-section flat local-decl slot-id)
  (combine-multi-synch-core (slot-value flat slot-id) local-decl))

(define-generic (combine-multi-synch-core esm local-decl))

(define-method (combine-multi-synch-core (esm <primitive>) (local-decl <sal-decl>))
  #f)

(define-method (combine-multi-synch-core (esm <sal-esm-statement>) (local-decl <sal-decl>))
  (if (sal-ast/contains-reference? esm local-decl)
    (make-ast-instance <sal-esm-multi-seq> esm
                            :local-decls (list local-decl)
                            :statement esm)
    esm))

(define-method (combine-multi-synch-core (esm <sal-esm-seq>) (local-decl <sal-decl>))
  (copy-ast esm
            :statements (map (cut combine-multi-synch-core <> local-decl) (slot-value esm :statements))))
      
(define-method (sal->esm-core (ast <sal-multi-synch-composition>) (env <primitive>) (ctx <sal-to-esm-context>))
  (let* ((place-provider ast)
         (local-decl (car (slot-value ast :local-decls)))
         (synch-decl-list (slot-value ctx :synch-decl-list))
         (new-ctx (copy-instance ctx
                                 :synch-decl-list (cons local-decl synch-decl-list)))
         (flat-child (flat-multi-child ast env new-ctx)))
    (update-state-vars!
     ast
     (make-ast-instance <sal-esm-module> place-provider
                        :definition (combine-multi-synch-section flat-child local-decl :definition)
                        :initialization (combine-multi-synch-section flat-child local-decl :initialization)
                        :transition (combine-multi-synch-section flat-child local-decl :transition)
                        :choice-vars (slot-value flat-child :choice-vars)
                        :transition-trace-info (make-sal-multi-sequence-trace-info local-decl
                                                                                   (slot-value flat-child :transition-trace-info)
                                                                                   place-provider)))))

(define-method (sal->esm-core (ast <sal-multi-asynch-composition>) (env <primitive>) (ctx <sal-to-esm-context>))
  (let* ((place-provider ast)
         (local-decl (car (slot-value ast :local-decls)))
         (flat-child (flat-multi-child ast env ctx))
         (id (slot-value local-decl :id))
         (synch-decl-list (slot-value ctx :synch-decl-list))
         (choice-var-decl (make-sal-choice-var-decl (slot-value local-decl :type) synch-decl-list place-provider))
         (choice-test (make-sal-choice-esm-assignment choice-var-decl 
                                                      synch-decl-list 
                                                      (make-sal-name-expr local-decl place-provider)
                                                      place-provider)))
    (update-state-vars!
     ast
     (make-ast-instance <sal-esm-module> place-provider
                        :definition (combine-multi-synch-section flat-child local-decl :definition)
                        :initialization (combine-multi-synch-section flat-child local-decl :initialization)
                        :transition (make-ast-instance <sal-esm-multi-choice> place-provider
                                                       :local-decls (list local-decl)
                                                       :statement (sal-esm/make-esm-seq choice-test
                                                                                        (slot-value flat-child :transition)
                                                                                        place-provider))
                        :choice-vars (if choice-var-decl
                                       (cons choice-var-decl (slot-value flat-child :choice-vars))
                                       (slot-value flat-child :choice-vars))
                        :transition-trace-info (make-sal-multi-choice-trace-info choice-var-decl 
                                                                                 local-decl 
                                                                                 (slot-value flat-child :transition-trace-info)
                                                                                 place-provider)))))


(define-method (sal->esm-core (ast <sal-module-models>) (env <primitive>) (ctx <sal-to-esm-context>))
  (let* ((module (slot-value ast :module))
         (state-vars (sal-module/state-variables module))
         (new-module (sal->esm-core module env ctx))
         (new-state-vars (sal-module/state-variables new-module)))
    [assert (state-vars new-state-vars) (= (length state-vars) (length new-state-vars))]
    (append-choice-vars! new-module)
    (let ((env (update-env* env state-vars new-state-vars)))
      (update-ast-slots ast
                        :module new-module
                        :expr (sal->esm-core (slot-value ast :expr) env ctx)))))

(define-method (sal->esm-core (ast <sal-qualified-assertion-name>) (env <primitive>) (ctx <sal-to-esm-context>))
  (sal->esm-core (sal-assertion-name/definition ast) env ctx))

(define-method (sal->esm-core (ast <sal-module-implements>) (env <primitive>) (ctx <sal-to-esm-context>))
  (let ((new-module1 (sal->esm-core (slot-value ast :module1) env ctx))
        (new-module2 (sal->esm-core (slot-value ast :module2) env ctx)))
    (append-choice-vars! new-module1)
    (append-choice-vars! new-module2)
    (update-ast-slots ast
                      :module1 new-module1
                      :module2 new-module2)))
