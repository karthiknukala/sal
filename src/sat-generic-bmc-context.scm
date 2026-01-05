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

(module sat-generic-bmc-context
        (include "sal.sch")
        (import sat-bmc-context sat-generic-context polarity fast-hash-table 
                queue sal-type sal-ast-env sal-ast-copy sal-expression
                sat-generic-context-result sat-context sal-path sal-ast-for-each
                sal-pp sal-ast-simplify gmp-scheme)
        (export <sat-generic-bmc-context>
                <sat-step-decl>
                (sat-generic-bmc-context/init! ctx flat-module cont-proc)
                (make-sat-generic-bmc-context flat-module cont-proc))
        )

(define-class <sat-generic-bmc-context> (<sat-generic-context> <sat-bmc-context>) ())
(define-class <sat-step-decl> (<sat-decl>) ())

(define (sat-generic-bmc-context/init! ctx flat-module cont-proc)
  (sat-bmc-context/init! ctx flat-module)
  (sat-generic-context/init! ctx flat-module)
  (cont-proc ctx)
  ctx)
  
(define (make-sat-generic-bmc-context flat-module cont-proc)
  (let ((ctx (make-instance <sat-generic-bmc-context>)))
    (sat-generic-bmc-context/init! ctx flat-module cont-proc)))

(define-method (sat-bmc-context/decl-at (ctx <sat-generic-bmc-context>) (decl <sal-decl>) (step <primitive>))
  (let ((table (sat-bmc-context/decl-table-at ctx step)))
    (cond 
     ((eq-hash-table/get table decl) =>
      cdr)
     (else
      (let* ((name (sal-decl/name decl))
             (new-name (symbol-append name '_@_ (object->symbol step)))
             (new-identifier (copy-ast (slot-value decl :id)
                                       :name new-name))
             (type (slot-value decl :type))
             (new-type (if (instance-of? decl <sal-choice-input-state-var-decl>)
                         ;; I use this trick to avoid the generation of type constraints for choice-vars...
                         (sat-context/super-type type)
                         (sal-ast->sat type ctx (make-empty-env) step *pos*))) ;; fix a problem with subtypes identified by Bruno... 
             ;; Bruno's bug
             ;; Description:
             ;;  In a previous version I was not converting the type. However this may
             ;;  generate a bug, since when subtypes are used they may contain references
             ;;  to variables wich were converted to <sat-step-decl> and <sat-global-decl>
             (new-decl (make-ast-instance <sat-step-decl> decl
                                          :id new-identifier
                                          :type new-type))
             (inv-table (slot-value ctx :inv-step-decls)))
        (queue/insert! (slot-value ctx :declaration-queue) new-decl)
        (eq-hash-table/put! table decl new-decl)
        (eq-hash-table/put! inv-table new-decl (cons decl step))
        new-decl)))))

(define (sat-bmc-context/global-decl ctx decl)
  [assert (decl) (and ;; uninterpreted constant
                  (instance-of? decl <sal-constant-decl>)
                  (not (slot-value decl :value)))]
  (let ((table (slot-value ctx :global-decls)))
    (cond
     ((eq-hash-table/get table decl) =>
      cdr)
     (else
      (let ((new-decl (make-ast-instance <sat-global-decl> decl
                                         :id (slot-value decl :id)
                                         :type (sal-ast->sat (slot-value decl :type) ctx (make-empty-env) 0 *pos*))) ;; fix a problem with subtypes identified by bruno... check comment above...
            (inv-table (slot-value ctx :inv-global-decls)))
        (queue/insert! (slot-value ctx :declaration-queue) new-decl)
        (eq-hash-table/put! table decl new-decl)
        (eq-hash-table/put! inv-table new-decl decl)
        new-decl)))))

(define-method (sal-ast->sat (ast <sal-ast>) (ctx <sat-generic-bmc-context>) (env <primitive>) (step <primitive>) (polarity <polarity>))
  (trace 'sat-context "sal-ast->sat ~a" (sal-ast->list ast))
  (sal-ast/map ast env (lambda (child-ast new-env) (sal-ast->sat child-ast ctx new-env step polarity))))

(define-method (sal-ast->sat (ast <sal-scalar>) (ctx <sat-generic-bmc-context>) (env <primitive>) (step <primitive>) (polarity <polarity>))
  ast)

(define-method (sal-ast->sat (ast <sal-name-expr>) (ctx <sat-generic-bmc-context>) (env <primitive>) (step <primitive>) (polarity <polarity>))
  (cond
   ((instance-of? (slot-value ast :decl) <sal-state-var-decl>)
    (copy-ast ast 
              :decl (sat-bmc-context/decl-at ctx (slot-value ast :decl) step)))
   ((lookup-env (slot-value ast :decl) env) =>
    (lambda (new-decl)
      (update-ast-slots ast :decl new-decl)))
   (else 
    ;; the declaration was not modified
    ast)))

(define-method (sal-ast->sat (ast <sal-qualified-name-expr>) (ctx <sat-generic-bmc-context>) (env <primitive>) (step <primitive>) (polarity <polarity>))
  (cond
   ((sal-name-expr/definition ast) =>
    (lambda (definition)
      (sal-ast->sat definition ctx env step polarity)))
   ((sal-name-ref/builtin? ast)
    ast)
   (else
    ;; (breakpoint "qualified to sat-context" (ast env ctx step) #t)
    ;; uninterpreted constant...
    ;; remark: we must flat name expressions before calling this function...
    [assert (ast) (null? (slot-value ast :actuals))]
    (make-sal-name-expr (sat-bmc-context/global-decl ctx (slot-value ast :decl)) ast))))

(define (keep-app ast ctx env step polarity)
  (let* ((arg-list (sal-application/argument-list ast))
         (new-arg-list (map (cut sal-ast->sat <> ctx env step polarity)
                            arg-list)))
    (copy-ast ast
              :arg (apply make-application-argument new-arg-list))))

(define-method (sal-ast->sat (ast <sal-arith-application>) (ctx <sat-generic-bmc-context>) (env <primitive>) (step <primitive>) (polarity <polarity>))
  (keep-app ast ctx env step polarity))

(define-method (sal-ast->sat (ast <sal-temporal-application>) (ctx <sat-generic-bmc-context>) 
                             (env <primitive>) (step <primitive>) (polarity <polarity>))
  (keep-app ast ctx env step polarity))

(define-method (sal-ast->sat (ast <sal-type-name>) (ctx <sat-generic-bmc-context>) (env <primitive>) (step <primitive>) (polarity <polarity>))
  (let ((def (sal-type-name/definition ast)))
    (cond
     ((not def)
      ast)
     ((or (instance-of? def <sal-scalar-type>) (instance-of? def <sal-data-type>))
      ast)
     (else
      (sal-ast->sat def ctx env step polarity)))))

(define (generic-skolemize ast ctx env step polarity)
  (sat-generic-context/skolemize ast env
                                 (lambda (expr new-env new-local-decls)
                                   (queue/append! (slot-value ctx :declaration-queue) new-local-decls)
                                   (sal-ast->sat expr ctx new-env step polarity))))
  
(define-method (sal-ast->sat (ast <sal-exists-expr>) (ctx <sat-generic-bmc-context>) (env <primitive>) (step <primitive>) (polarity <pos>))
  (generic-skolemize ast ctx env step polarity))

(define-method (sal-ast->sat (ast <sal-for-all-expr>) (ctx <sat-generic-bmc-context>) (env <primitive>) (step <primitive>) (polarity <pos>))
  (generic-skolemize ast ctx env step polarity))

(define-method (sal-ast->sat (ast <sal-let-expr>) (ctx <sat-generic-bmc-context>) (env <primitive>) (step <primitive>) (polarity <polarity>))
  (sat-generic-context/remove-let ast env 
                                  (lambda (let-value new-decl new-name-expr)
                                    (let ((new-value (sal-ast->sat let-value ctx env step *pos-neg*)))
                                      (queue/insert! (slot-value ctx :declaration-queue) new-decl)
                                      (queue/insert! (slot-value ctx :constraint-queue) (make-sal-equality new-name-expr new-value))))
                                  (lambda (expr new-env)
                                    (sal-ast->sat expr ctx new-env step polarity))))

;;-------------------------------------------------------
;;
;; Counter example construction
;;
;;-------------------------------------------------------
(define-method (sat-bmc-context/make-path (ctx <sat-generic-bmc-context>))
  (let ((ctx-result (sat-context/solve ctx)))
    (and ctx-result
         (sat-generic-context-result/make-path ctx-result))))
    
(define-generic (sat-generic-context-result/make-path ctx-result))
(define-method (sat-generic-context-result/make-path (ctx-result <primitive>))
  #unspecified)

(define-method (sat-generic-context-result/make-path (ctx-result <sat-generic-context-result>))
  (verbose-message 3 "  building path...") 
  (multiple-value-bind
      (assignment-table constraint-list)
      (sat-generic-context-result/cleanup! ctx-result)
    (let* ((sat-context (slot-value ctx-result :sat-context))
           (global-constraints (make-queue))
           (step-vector-info (make-step-vector (+ (slot-value sat-context :max-step) 1)))
           (inv-step-decls (slot-value sat-context :inv-step-decls))
           (inv-global-decls (slot-value sat-context :inv-global-decls)))
      (process-assignment-table! assignment-table step-vector-info global-constraints inv-step-decls inv-global-decls)
      (process-constraint-list! constraint-list step-vector-info global-constraints inv-step-decls inv-global-decls)
      ;; (display-info global-constraints step-vector-info)
      ;; (breakpoint "make-path-core" (sat-context global-constraints step-vector-info) #t)
      (make-instance <sal-path>
                     :flat-module (slot-value sat-context :flat-module)
                     :step-info-list (vector->list step-vector-info)
                     :auxiliary-decls (collect-sat-aux-decls step-vector-info global-constraints)
                     :global-constraint-list (queue->list global-constraints)))))

(define (sal-ast/collect-sat-aux-decls! ast aux-decl-queue already-found-table)
  (sal-ast/for-each (lambda (ast)
                      (when (and (instance-of? ast <sal-name-expr>)
                                 (instance-of? (slot-value ast :decl) <sat-aux-decl>)
                                 (not (eq-hash-table/get already-found-table (slot-value ast :decl))))
                        (queue/insert! aux-decl-queue (slot-value ast :decl))
                        (eq-hash-table/put! already-found-table (slot-value ast :decl) #unspecified)))
                    ast))

(define (collect-sat-aux-decls step-vector-info global-constraint-queue)
  (let ((aux-decl-queue (make-queue))
        (already-found-table (make-eq-hash-table)))
    (for-each (cut sal-ast/collect-sat-aux-decls! <> aux-decl-queue already-found-table) (queue->list global-constraint-queue))
    (vector/for-each (lambda (step)
                       (for-each (cut sal-ast/collect-sat-aux-decls! <> aux-decl-queue already-found-table) 
                                 (slot-value step :constraint-list)))
                     step-vector-info)
    (queue->list aux-decl-queue)))

(define (display-info global-constraints step-vector-info)
  (unless (queue/empty? global-constraints)
    (print "Global constraints: ")
    (for-each (lambda (expr)
                (sal/pp expr)
                (print ";"))
              (queue->list global-constraints)))
  (let ((step-id 0))
    (vector/for-each (lambda (step)
                       (print "Step " step-id ":")
                       (print "Assignments: ")
                       (eq-hash-table/for-each (lambda (decl expr)
                                                 (sal/pp (make-sal-equality (make-sal-name-expr decl)
                                                                            expr))
                                                 (print ";"))
                                               (slot-value step :assignment-table))
                       (print "Constraints: ")
                       (for-each (lambda (expr)
                                   (sal/pp expr)
                                   (print ";"))
                                 (slot-value step :constraint-list))
                       (print "----------------------------")
                       (set! step-id (+ step-id 1)))
                     step-vector-info)))
 
(define *empty-env* (make-empty-env))

(define (make-step-vector n)
  (let ((result (make-vector n)))
    (let loop ((i 0))
      (when (< i n)
        (vector-set! result i (make-instance <sal-step> 
                                             :assignment-table (make-eq-hash-table)
                                             :constraint-list '()))
        (loop (+ i 1))))
    result))

(define (process-assignment-table! assignment-table step-vector-info global-constraints inv-step-decls inv-global-decls)
  (eq-hash-table/for-each (lambda (decl expr)
                            (let ((lhs (make-sal-name-expr decl)))
                              (multiple-value-bind
                                  (state-lhs lhs-owner-step)
                                  (build-state-constraint lhs inv-step-decls inv-global-decls)
                                (multiple-value-bind 
                                    (state-rhs rhs-owner-step)
                                    (build-state-constraint expr inv-step-decls inv-global-decls)
                                  (cond
                                   ((or (>= rhs-owner-step 0) ;; if rhs is not really ground...
                                        (< lhs-owner-step 0)) ;; if it is a global constraint
                                    (add-constraint! (make-sal-equality state-lhs state-rhs) (max lhs-owner-step rhs-owner-step)
                                                     step-vector-info global-constraints))
                                   (else
                                    [assert (lhs-owner-step rhs-owner-step) (and (>= lhs-owner-step 0) (< rhs-owner-step 0))]
                                    (eq-hash-table/put! (slot-value (vector-ref step-vector-info lhs-owner-step) :assignment-table)
                                                        (slot-value state-lhs :decl)
                                                        state-rhs)))))))
                          assignment-table))

(define (process-constraint-list! constraint-list step-vector-info global-constraints inv-step-decls inv-global-decls)
  (for-each (lambda (constraint)
              (multiple-value-bind 
                  (state-constraint owner-step)
                  (build-state-constraint constraint inv-step-decls inv-global-decls)
                (add-constraint! state-constraint owner-step step-vector-info global-constraints)))
            constraint-list))

(define (add-constraint! constraint owner-step step-vector-info global-constraints)
  (let ((constraint (sal-constraint/normalize constraint)))
    (if (< owner-step 0)
      (queue/insert! global-constraints constraint)
      (let* ((step (vector-ref step-vector-info owner-step))
             (constraint-list (slot-value step :constraint-list)))
        (set-slot-value! step :constraint-list (cons constraint constraint-list))))))

;;-------------------------------------------------------
;; 
;; Compute maximum step in constraint
;;
;;-------------------------------------------------------
(define (constraint-maximum-step ast inv-step-decls)
  (let ((max-step -1))
    (sal-ast/for-each (lambda (n)
                        (cond
                         ((and (instance-of? n <sal-name-expr>)
                               (eq-hash-table/get inv-step-decls (slot-value n :decl))) 
                          => (lambda (entry)
                               (let ((step (cddr entry)))
                                 (when (> step max-step)
                                   (set! max-step step)
                                   )))))
                        #t) ;; continue the search
                      ast)
    max-step))
                          
;;-------------------------------------------------------
;; 
;; Obtain ast which uses the state-variables used to 
;; construct the BMC problem
;;
;;-------------------------------------------------------
(define-generic (build-state-ast ast env max-step inv-step-decls inv-global-decls))
(define-method (build-state-ast (ast <sal-ast>) (env <primitive>) (max-step <primitive>) (inv-step-decls <primitive>) (inv-global-decls <primitive>))
  (sal-ast/map ast env (lambda (child-ast new-env)
                         (build-state-ast child-ast new-env max-step inv-step-decls inv-global-decls))))
(define-method (build-state-ast (ast <sal-name-expr>) (env <primitive>) (max-step <primitive>) 
                                (inv-step-decls <primitive>) (inv-global-decls <primitive>))
  (let ((decl (slot-value ast :decl)))
    (cond
     ((eq-hash-table/get inv-global-decls decl) =>
      (lambda (entry)
        (copy-ast ast :decl (cdr entry))))
     ((eq-hash-table/get inv-step-decls decl) =>
      (lambda (entry)
        (let* ((original-decl-step-pair (cdr entry))
               (original-decl (car original-decl-step-pair))
               (step (cdr original-decl-step-pair)))
          [assert (max-step step ast) (<= step max-step)]
          (let loop ((result-expr (copy-ast ast :decl original-decl))
                     (s max-step))
            (if (= s step)
              result-expr
              (loop (make-ast-instance <sal-pre-operator> ast
                                       :expr result-expr)
                    (- s 1)))))))
     (else
      (sal-name-ref/map ast env)))))

;;-------------------------------------------------------
;; 
;; Build an state constraint
;;
;;-------------------------------------------------------
(define (build-state-constraint constraint inv-step-decls inv-global-decls)
  (let* ((max-step (constraint-maximum-step constraint inv-step-decls))
         ;; (_ (begin (sal/pp constraint) (print "\nmax-step: " max-step "\n-----------")))
         (state-constraint (build-state-ast constraint *empty-env* max-step inv-step-decls inv-global-decls)))
    (values state-constraint max-step)))


                               
;;-------------------------------------------------------
;; 
;; Constraint normalizer... 
;; Try to put a constraint in a more "readable" format
;;
;;-------------------------------------------------------
(define-generic (sal-constraint/normalize ast))

(define-method (sal-constraint/normalize (ast <sal-ast>))
  ast)

(define-method (sal-constraint/normalize (ast <sal-eq>))                         
  ;; (print "normalizing...")
  ;; (sal/pp ast)
  ;; (print "")
  (let ((result (multiple-value-bind
                    (lhs rhs)
                    (sal-binary-application/arguments ast)
                  (if (and (not (instance-of? lhs <sal-name-expr>))
                           (instance-of? rhs <sal-name-expr>))
                    (copy-ast ast
                              :arg (make-application-argument rhs lhs))
                    ast))))
    ;; (sal/pp result)
    ;; (print "\n----------------")
    result))

(define-generic (sal-constraint/invert ast))
(define-method (sal-constraint/invert (ast <sal-ast>))
  #f) ;; don't know how to invert
(define-method (sal-constraint/invert (ast <sal-eq>))
  (apply make-sal-builtin-application <sal-diseq> ast (sal-application/argument-list ast)))
(define-method (sal-constraint/invert (ast <sal-diseq>))
  (apply make-sal-builtin-application <sal-eq> ast (sal-application/argument-list ast)))
(define-method (sal-constraint/invert (ast <sal-le>))
  (apply make-sal-builtin-application <sal-gt> ast (sal-application/argument-list ast)))
(define-method (sal-constraint/invert (ast <sal-lt>))
  (apply make-sal-builtin-application <sal-ge> ast (sal-application/argument-list ast)))
(define-method (sal-constraint/invert (ast <sal-ge>))
  (apply make-sal-builtin-application <sal-lt> ast (sal-application/argument-list ast)))
(define-method (sal-constraint/invert (ast <sal-gt>))
  (apply make-sal-builtin-application <sal-le> ast (sal-application/argument-list ast)))


(define-method (sal-constraint/normalize (ast <sal-not>))
  (let ((arg (slot-value ast :arg)))
    (cond 
     ((sal-constraint/invert arg) =>
      sal-constraint/normalize)
     (else
      ast))))

(define-method (sal-constraint/normalize (ast <sal-inequality>))
  (multiple-value-bind
      (lhs rhs)
      (sal-binary-application/arguments ast)
    (let ((lhs-queue (make-queue))
          (rhs-queue (make-queue)))
      (sal-ast/side-insert! lhs #t lhs-queue rhs-queue)
      (sal-ast/side-insert! rhs #f lhs-queue rhs-queue)
      (let* ((new-lhs (make-side lhs-queue ast))
             (new-rhs (make-side rhs-queue ast))
             (new-ast (copy-ast ast
                                :arg (make-application-argument new-lhs new-rhs))))
        (if (and (instance-of? new-rhs <sal-name-expr>)
                 (not (instance-of? new-lhs <sal-name-expr>)))
          (sal-inequality/swap new-ast)
          new-ast)))))

(define (make-side side-queue place-provider)
  (if (queue/empty? side-queue)
    (make-sal-numeral 0 place-provider)
    (apply make-simplified-sal-builtin-application <sal-add> place-provider (queue->list side-queue))))

(define-generic (sal-ast/side-insert! ast lhs? lhs-queue rhs-queue))
(define-method (sal-ast/side-insert! (ast <sal-ast>) (lhs? <primitive>) (lhs-queue <primitive>) (rhs-queue <primitive>))
  (if lhs?
    (queue/insert! lhs-queue ast)
    (queue/insert! rhs-queue ast)))
(define-method (sal-ast/side-insert! (ast <sal-add>) (lhs? <primitive>) (lhs-queue <primitive>) (rhs-queue <primitive>))
  (for-each (cut sal-ast/side-insert! <> lhs? lhs-queue rhs-queue)
            (sal-application/argument-list ast)))
(define-method (sal-ast/side-insert! (ast <sal-sub>) (lhs? <primitive>) (lhs-queue <primitive>) (rhs-queue <primitive>))
  (let ((args (sal-application/argument-list ast)))
    (sal-ast/side-insert! (car args) lhs? lhs-queue rhs-queue)
    (for-each (cut sal-ast/side-insert! <> (not lhs?) lhs-queue rhs-queue)
              (cdr args))))
(define-method (sal-ast/side-insert! (ast <sal-numeral>) (lhs? <primitive>) (lhs-queue <primitive>) (rhs-queue <primitive>))
  (if (<mpq (slot-value ast :num) *mpq-zero*)
    (let ((new-ast (copy-ast ast
                             :num (mpq/absolute (slot-value ast :num)))))
      (sal-ast/side-insert! new-ast (not lhs?) lhs-queue rhs-queue))
    (call-next-method)))
(define-method (sal-ast/side-insert! (ast <sal-mul>) (lhs? <primitive>) (lhs-queue <primitive>) (rhs-queue <primitive>))
  (let ((args (sal-application/argument-list ast)))
    (if (= (length args) 2)
      (let ((arg1 (car args))
            (arg2 (cadr args)))
        (multiple-value-bind
            (arg1 arg2)
            (if (instance-of? arg1 <sal-numeral>)
              (values arg1 arg2)
              (values arg2 arg1))
          (if (instance-of? arg1 <sal-numeral>)
            (cond
             ((=mpq (slot-value arg1 :num) *mpq-minus-one*)
              (sal-ast/side-insert! arg2 (not lhs?) lhs-queue rhs-queue))
             ((<mpq (slot-value arg1 :num) *mpq-zero*)
              (let* ((new-arg1 (copy-ast arg1 
                                         :num (mpq/absolute (slot-value arg1 :num))))
                     (new-ast (make-sal-builtin-application <sal-mul> ast new-arg1 arg2)))
                (sal-ast/side-insert! new-ast (not lhs?) lhs-queue rhs-queue)))
             (else
              (call-next-method)))
            (call-next-method))))
      (call-next-method))))

(define (sal-inequality/swap ast)
  (multiple-value-bind 
      (lhs rhs)
      (sal-binary-application/arguments ast)
    (make-sal-builtin-application (sal-inequality/swap-class ast) ast rhs lhs)))

(define-generic (sal-inequality/swap-class ast))
(define-method (sal-inequality/swap-class (ast <sal-le>)) <sal-ge>)
(define-method (sal-inequality/swap-class (ast <sal-lt>)) <sal-gt>)
(define-method (sal-inequality/swap-class (ast <sal-ge>)) <sal-le>)
(define-method (sal-inequality/swap-class (ast <sal-gt>)) <sal-lt>)




