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

(module sal-assertion
        (include "sal.sch")
        (import sal-ast-instantiate sal-ast-copy sal-ast-env sal-module sal-ast-for-each 
                sal-expression sal-ltl sal-ast-simplify runtime sal-decls sal-component-info sal-module
                sal-esm ltl-ctl)
        (export (sal-assertion-name/definition assertion-name)
                (sal-assertion/transformation-core ast env proc-module-modules-flat-module)
                (sal-expr/contains-temporal-operators? expr)
                (sal-module-models/ltl-property? ast)
                (sal-module-models/ctl-property? ast)
                (sal-module-models/invariant? assertion)
                (sal-module-models/invariant-body assertion)
                (sal-module-models/trivially-true? ast)
                (sal-module-models/trivially-false? ast)
                (sal-module-models/ltl->ba assertion . bool-ba?)
                (sal-module-models/ltl->dot assertion output-port pos)
                (sal-module-models/accepting? ast)
                (sal-module-models/weak-accepting? ast)
                (sal-module-models/accepting-body ast)
                (sal-module-models/display-ltl-monitor assertion)
                (sal-module-models/convert-to-ltl assertion))
        )

(define-generic (sal-assertion-name/definition assertion-name))

(define-method (sal-assertion-name/definition (assertion-name <sal-qualified-assertion-name>))
  (let ((def (slot-value (slot-value assertion-name :decl) :assertion-expr)))
    [assert (def) def]
    (sal-ast/instantiate def (slot-value assertion-name :actuals))))
  
(define-generic (sal-assertion/transformation-core ast env proc-module-modules-flat-module))

(define-method (sal-assertion/transformation-core (ast <sal-ast>) (env <primitive>) (proc-module-modules-flat-module <primitive>))
  (sal-ast/map ast env (lambda (n e) 
                         (sal-assertion/transformation-core n e proc-module-modules-flat-module))))

(define-method (sal-assertion/transformation-core (ast <sal-module-models>) (env <primitive>) (proc-module-modules-flat-module <primitive>))
  (let ((module (slot-value ast :module)))
    (unless (instance-of? module <sal-flat-module>)
      (sign-error "Assertion transformation wrapper can only be applied over assertions that only contain flat modules. Please use sal-assertion/flat before calling this procedure."))
    (let* ((new-module (proc-module-modules-flat-module module env))
           (new-expr (sal-module/rebind-expr module (slot-value ast :expr) new-module env)))
      (update-ast-slots ast
                        :module new-module
                        :expr new-expr))))

(define-api (sal-expr/contains-temporal-operators? (expr <sal-expr>))
  (sal-ast/find (lambda (ast)
                  (instance-of? ast <sal-temporal-application>))
                expr))

(define-api (sal-module-models/invariant? (ast <sal-module-models>))
  (let ((property (slot-value ast :expr)))
    (and (or (instance-of? property <sal-ctl-ag>)
             (instance-of? property <sal-ltl-g>))
         (not (sal-expr/contains-temporal-operators? (slot-value property :arg))))))

(define-api (sal-module-models/invariant-body (ast <sal-module-models>))
  [sal-assert "sal-module-models/invariant-body" (ast) (sal-module-models/invariant? ast)]
  (car (sal-application/argument-list (slot-value ast :expr))))

(define-api (sal-expr/only-ltl-operators? expr)
  (not (sal-ast/find (lambda (ast)
                       (and (instance-of? ast <sal-temporal-application>)
                            (not (instance-of? ast <sal-ltl-application>))))
                      expr)))

(define-api (sal-expr/only-ctl-operators? expr)
  (not (sal-ast/find (lambda (ast)
                       (and (instance-of? ast <sal-temporal-application>)
                            (not (instance-of? ast <sal-ctl-application>))))
                      expr)))

(define (sal-module-models/ltl-property? ast)
  (and (instance-of? ast <sal-module-models>)
       (sal-expr/only-ltl-operators? (slot-value ast :expr))))

(define (sal-module-models/ctl-property? ast)
  (and (instance-of? ast <sal-module-models>)
       (sal-expr/only-ctl-operators? (slot-value ast :expr))))

(define (make-component-info-for-buchi-automata property ba-pc-decl-list)
  (let* ((used-var-decls (sal-ast/collect-used-state-var-decls property))
         (owned-data (map make-sal-name-expr ba-pc-decl-list))
         (input-data (map make-sal-name-expr used-var-decls)))
    (make-ast-instance <sal-base-component-info> property
                       :input-data input-data
                       :output-data owned-data
                       :owned-data owned-data)))

;; creates a new module-models expression, where the LTL property is converted to a buchi automata (monitor).
;; the result module-models will contain a simple property:
;;   - (G (not error)) if the LTL property is a safety property
;;   - (F (G (not error))) if the LTL property is a liveness property
(define-api (sal-module-models/ltl->ba (assertion <sal-module-models>) . bool-ba?)
  (let ((bool-ba? (optional-arg bool-ba? #f))
        (module (slot-value assertion :module))
        (property (slot-value assertion :expr)))
    (unless (sal-expr/only-ltl-operators? property)
      (sign-error "sal-module-models/ltl->ba can only be applied over LTL assertions."))
    (cond
     ((sal-module-models/invariant? assertion)
      assertion) ;; do not convert invariants
     (else
      (status-message :creating-ba)
      (verbose-message 1 "creating monitor (buchi-automata) for LTL property...")
      (display-runtime 2 "  monitor generation time: ~a secs"
        (lambda ()
          (make-module-models-with-monitor module assertion bool-ba?))
        :ba-time)))))

(define-generic (make-module-models-with-monitor module assertion bool-ba?))

(define-method (make-module-models-with-monitor (module <sal-module>) (assertion <sal-assertion-expr>)
                                                (bool-ba? <primitive>))
  (sign-error "sal-module-models/ltl->ba can only be applied over assertions about flat-modules or esm-modules. Use sal-ast/flat-modules or sal->esm before calling this function."))

(define (trivial? new-property)
  (and
   (instance-of? new-property <sal-ltl-g>)
   (or (instance-of? (slot-value new-property :arg) <sal-true>)
       (instance-of? (slot-value new-property :arg) <sal-false>))))
  
(define-method (make-module-models-with-monitor (module <sal-flat-module>) (assertion <sal-assertion-expr>)
                                                (bool-ba? <primitive>))
  (let ((property (slot-value assertion :expr)))
    (multiple-value-bind
        (ba-pc-decl-list initial-ba trans-ba new-property)
        (ltl->monitor (make-sal-not property) :bool? bool-ba?)
      (cond
       ((not (null? ba-pc-decl-list))
        (let* ((component-info (slot-value module :component-info))
               (ba-component-info (make-component-info-for-buchi-automata property ba-pc-decl-list))
               (new-component-info (make-ast-instance <sal-composite-component-info> module
                                                      :components (list ba-component-info component-info)))
               (new-module (copy-ast module
                                     :component-info new-component-info
                                     :state-vars (append ba-pc-decl-list (slot-value module :state-vars))
                                     :initialization (make-sal-and+ (slot-value module :initialization)
                                                                    initial-ba)
                                     :transition (make-sal-and+ (slot-value module :transition)
                                                                trans-ba))))
          (copy-ast assertion
                    :module new-module
                    :expr new-property)))
       (else
        [assert (new-property) (trivial? new-property)]
        (copy-ast assertion :module module :expr new-property))))))

(define-method (make-module-models-with-monitor (module <sal-esm-module>) (assertion <sal-assertion-expr>)
                                                (bool-ba? <primitive>))
  [assert (bool-ba?) (not bool-ba?)]
  (let ((property (slot-value assertion :expr)))
    (multiple-value-bind
        (ba-pc-decl-list initial-ba trans-ba new-property)
        (ltl->monitor (make-sal-not property) :esm? #t)
      (cond
       ((not (null? ba-pc-decl-list))
        (let* ((initialization (slot-value module :initialization))
               (transition (slot-value module :transition))
               (new-initialization (sal-esm/make-esm-seq initialization
                                                         initial-ba
                                                         initialization))
               (new-transition (change-class (sal-esm/make-esm-seq transition
                                                                   trans-ba
                                                                   transition)
                                             <sal-esm-monitor-seq>))
               (new-module (copy-ast module
                                     :state-vars (append ba-pc-decl-list (slot-value module :state-vars))
                                     :initialization new-initialization
                                     :transition new-transition)))
          (copy-ast assertion
                    :module new-module
                    :expr new-property)))
       (else
        [assert (new-property) (trivial? new-property)]
        (copy-ast assertion :module module :expr new-property))))))

(define-api (sal-module-models/ltl->dot (assertion <sal-module-models>) (output-port <primitive>) (pos <primitive>))
  (let ((module (slot-value assertion :module))
        (property (if pos
                    (slot-value assertion :expr)
                    (make-sal-not (slot-value assertion :expr)))))
    (unless (sal-expr/only-ltl-operators? property)
      (sign-error "sal-module-models/ltl->dot can only be applied over LTL assertions."))
    (verbose-message 1 "creating Buchi-Automata...")
    (with-output-to-port output-port
      (lambda ()
        (ltl->dot property)))))

(define-api (sal-module-models/trivially-true? (ast <sal-module-models>))
  (and (sal-module-models/invariant? ast)
       (sal-expr/true? (sal-module-models/invariant-body ast))))

(define-api (sal-module-models/trivially-false? (ast <sal-module-models>))
  (and (sal-module-models/invariant? ast)
       (sal-expr/false? (sal-module-models/invariant-body ast))))

(define-api (sal-module-models/accepting? (ast <sal-module-models>))
  (let ((property (slot-value ast :expr)))
    (instance-of? property <sal-accepting>)))

(define-api (sal-module-models/weak-accepting? (ast <sal-module-models>))
  (let ((property (slot-value ast :expr)))
    (instance-of? property <sal-weak-accepting>)))
  
(define-api (sal-module-models/accepting-body (ast <sal-module-models>))
  (unless (sal-module-models/accepting? ast)
    (sign-source-error ast "The property is not an accepting condition property (low level encoding of LTL properties), or it was not processed by sal-module-models/ltl->ba"))
  (let ((property (slot-value ast :expr)))
    (slot-value property :arg)))

;; Display the buchi automata (monitor) for a LTL property
(define-api (sal-module-models/display-ltl-monitor (assertion <sal-module-models>))
  (let ((property (slot-value assertion :expr)))
    (unless (sal-expr/only-ltl-operators? property)
      (sign-error "sal-module-models/display-ltl-monitor can only be applied over LTL assertions."))
    (let ((ba (ltl->ba (make-sal-not property))))
      (ba/display ba))))

(define (sign-unsupported-property property)
  (sign-unsupported-feature property "Failed to convert to a LTL property."))

(define-api (sal-module-models/convert-to-ltl assertion)
  (cond
   ((sal-module-models/ctl-property? assertion)
    (try 
     (let ((new-property (ctl->ltl (slot-value assertion :expr))))
       (copy-ast assertion :expr new-property))
     (catch 'ctl->ltl-failure
            (lambda (_)
              (sign-unsupported-property assertion)))))
   ((or (sal-module-models/ltl-property? assertion)
        (sal-module-models/accepting? assertion)
        (sal-module-models/weak-accepting? assertion))
    assertion)
   (else
    (sign-unsupported-property assertion))))
