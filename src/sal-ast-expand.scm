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

(module sal-ast-expand
        (include "sal.sch")
        (include "sal-ast-table.sch")
        (import sal-ast-copy sal-expression sal-expr-evaluator queue sal-ast-env iterators sal-ast-simplify
                sal-type sal-ast-eq gmp-scheme sal-ast-for-each sal-ast-attributes sal-decls runtime sal-ast-list sal-pp
                sal-ast-env front-end polarity unique-names)
        (export (sal-expander/set-max-expansion-depth! max)
                (sal-expander/set-max-function-domain-size-to-memoize! max)
                (sal-ast/expand-core ast env depth)
                (sal-ast/expand ast)
                (sal-expr/lift fun arg env depth)
                (sal-quantified-expr/expand-core ast env proc)
                (sal-ast/expand-quantifiers-core ast env polarity expand-pred?)
                (sal-ast/expand-quantifiers ast)
                (sal-ast/conditionally-expand-quantifiers ast expand-pred?)
                (skolem-expand? ast polarity var-decl)
                (skolem-expand-finite? ast polarity var-decl)
                (sal-expander/maintain-app ast env depth)
                (sal-ast/expand-names ast env)
                (sal-ast/expand-applications ast env pred?)
                (sign-expander-error ast msg . args))
        )

(define *max-expansion-depth* 128)

(define *max-function-domain-size-to-memoize* (integer->mpq 0))

(define (sal-expander/set-max-expansion-depth! max)
  (set! *max-expansion-depth* max))

(define (sal-expander/set-max-function-domain-size-to-memoize! max)
  (set! *max-function-domain-size-to-memoize* (object->mpq max)))

(front-end/add-simple-option! 
 "Code Transformations" 
 "--inf-loop-threshold=<num>"
 "Set the threshold for the infinite loop detector in the SAL partial evaluator. The threshold is the maximum number of recursive code unfoldings (default: 1024)."
 (front-end-adapter/nat-arg 
  (lambda (arg)
    (sal-expander/set-max-expansion-depth! arg))))

(define-generic (sal-ast/expand-core ast env depth))

(define (sal-ast/expand ast)
  (status-message :expanding-function-applications)
  (verbose-message 1 "expanding function applications...")
  (display-runtime 2 "  expansion time: ~a secs"
    (lambda ()
      (sal-ast/expand-core ast (make-empty-env) 0))
    :function-expansion-time))

(register-app-error! 'expander)

(define (sign-expander-error ast msg . args)
  (error 'expander (apply format-with-location ast msg args) #unspecified))

(define (sign-cannot-be-expanded ast)
  (sign-expander-error ast "node cannot be expanded."))

(define-inline (default-expand ast env depth)
  (let ((proc (lambda (ast new-env) (sal-ast/expand-core ast new-env depth))))
    (sal-ast/local-simplify-core ast env proc)))

(define-method (sal-ast/expand-core (ast <sal-ast>) (env <primitive>) (depth <primitive>))
  (default-expand ast env depth))

;;
;; BD: 2011/09/21
;;
;; Attempt to fix a variable-capture bug (reported by Anthony Simons)
;;
;; The bug was triggered when applying 'expand-core' to 
;;
;;   (EXISTS (tuple: Pair): r(tuple) AND tuple.1 = pair.1
;;
;; when the environment contains the mapping
;;
;;   (pair: Pair) --> (pair!1: Pair) = tuple
;; 
;; the 'tuple' in the substitution clashes with the bound variable in
;; the EXISTS. To fix this, we rename all the bound variables before
;; applying 'sal-ast/expand-core' to the body.
;;
;; NOTES
;; -----
;; 1) it's not clear that building local-decls-proc first is
;; necessary (since expand-core does not touch the types)
;;
;; 2) we may have to do something similar in 'sal-ast-copy' (and any
;; other place where substitutions are performed)
;;
;; 3) we could try to be clever and apply the renaming only when
;; there's a clash.
;;
(define-method (sal-ast/expand-core (ast <sal-local-binds-expr>) (env <primitive>) (depth <primitive>))
  (let* ((local-decls (slot-value ast :local-decls))
	 (proc (lambda (a e) (sal-ast/expand-core a e depth)))
	 (local-decls-proc (sal-ast/map-list local-decls env proc))
	 (new-local-decls (map (lambda (decl) (copy-ast decl :id (make-sal-identifier decl (gen-unique-name (slot-value (slot-value decl :id) :name)))))
			       local-decls-proc))
	 (new-env (update-env* env local-decls new-local-decls)))
    (update-ast-slots ast
		      :local-decls new-local-decls 
		      :expr (proc (slot-value ast :expr) new-env))))

(define (sal-expander/maintain-app ast env depth)
  (update-ast-slots ast
                    :arg (apply make-application-argument
                                (map (cut sal-ast/expand-core <> env depth) (sal-application/argument-list ast)))))

(define-method (sal-ast/expand-core (ast <sal-builtin-application>) (env <primitive>) (depth <primitive>))
  (sal-expander/maintain-app ast env depth))

(define (expand-builtin-app app env depth)
  (ensure ((app <sal-application>))
    (let* ((fun (slot-value app :fun))
           (_  [assert (fun) (instance-of? fun <sal-name-expr>)])
           (definition (sal-name-expr/definition fun)))
      (if definition
        (sal-expr/lift definition (slot-value app :arg) env depth)
        (sal-expander/maintain-app app env depth)))))

(define-method (sal-ast/expand-core (ast <sal-conditional>) (env <primitive>) (depth <primitive>))
  (let ((new-cond (try-to-evaluate (slot-value ast :cond-expr) env)))
    (cond
     ((instance-of? new-cond <sal-true>)
      (sal-ast/expand-core (slot-value ast :then-expr) env depth))
     ((instance-of? new-cond <sal-false>)
      (sal-ast/expand-core (slot-value ast :else-expr) env depth))
     (else
      (call-next-method)))))

(define-method (sal-ast/expand-core (ast <sal-min>) (env <primitive>) (depth <primitive>))
  (expand-builtin-app ast env depth))

(define-method (sal-ast/expand-core (ast <sal-max>) (env <primitive>) (depth <primitive>))
  (expand-builtin-app ast env depth))

(define-method (sal-ast/expand-core (ast <sal-type>) (env <primitive>) (depth <primitive>))
  ;; types are not expanded
  (sal-ast/simplify-core ast env))

(define-method (sal-ast/expand-core (ast <sal-module>) (env <primitive>) (depth <primitive>))
  (sign-unsupported-feature ast "Only the expressions in flat modules can be expanded, please convert the module to a flat module and try again."))

(define-method (sal-ast/expand-core (ast <sal-flat-module>) (env <primitive>) (depth <primitive>))
  (default-expand ast env depth))

(define-method (sal-ast/expand-core (ast <sal-qualified-name-ref>) (env <primitive>) (depth <primitive>))
  (sal-ast/substitute ast env))
  
(define (try-to-evaluate value env)
  (try-until-success (sal-expr/evaluate-core value env 0) value))

(define (evaluate-or-error value env place-provider)
  (try-until-success (sal-expr/evaluate-core value env 0) (sign-expander-error place-provider "Expression cannot be expanded, reason: ~a" previous-error-message)))

(define-generic (sal-expr/lift fun arg env depth))

(define (check-depth ast depth)
  (when (>= depth *max-expansion-depth*)
    (sign-expander-error ast "Infinite loop detector threshold was reached while expanding expression. Please increase the threshold or rewrite your specification. The threshold can be increased using the option `--inf-loop-threshold=<num>' or adding `(sal-expander/set-max-expansion-depth! <num>)' in the `.salrc' file in your home directory.")))

(define-method (sal-expr/lift (fun <sal-name-expr>) (arg <sal-expr>) (env <primitive>) (depth <primitive>))
  (let ((new-fun (sal-ast/substitute fun env))  ;; apply substitutions
        (place-provider fun))
    (if (instance-of? new-fun <sal-name-expr>)
      (cond 
       ((instance-of? (slot-value new-fun :decl) <sal-auxiliary-decl>)
        ;; name-expr is referencing an auxiliary declaration, so we don't need to continue the lifting...
        (sal-expander/maintain-app (make-sal-application new-fun arg place-provider) env depth))
       ((sal-name-expr/definition new-fun) =>
        (lambda (definition)
          (check-depth new-fun depth)
          (sal-expr/lift definition arg env (+ depth 1))))
       (else
        (sal-expander/maintain-app (make-sal-application new-fun arg place-provider) env depth)))
      (sal-expr/lift new-fun arg env depth))))

(define-method (sal-expr/lift (fun <sal-qualified-name-expr>) (arg <sal-expr>) (env <primitive>) (depth <primitive>))
  (let ((place-provider fun)
        (fun (sal-ast/substitute fun env))) ;; apply substitutions
    [assert (fun) (instance-of? fun <sal-qualified-name-expr>)]
    (let ((app (make-sal-application fun arg place-provider)))
      (cond
       ((instance-of? app <sal-builtin-application>)
        ;; I should check if there is an applicable sal-ast/expand-core method.
        (sal-ast/expand-core app env (+ depth 1))) ;; I incremented the depth just to be safe...
       ((sal-name-expr/inline? fun) 
        (check-depth fun depth)
        (sal-ast/expand-core (sal-expr/apply (sal-name-expr/definition fun) arg) env depth))
       ((sal-name-expr/definition fun) =>
        (lambda (definition)
;;	  [breakpoint "sal-expr/lift 1" (fun arg env depth definition) #t]
          (try-until-success
           (begin
             (let ((arg (sal-ast/expand-core arg env depth)))
               (unless (sal-expr/first-order-value? arg)
                 (fail))
               (let* ((app (sal-expr/apply definition arg))
                      (result (sal-expr/evaluate-core app env 0)))
                 (sal-ast/expand-core result env depth))))
           (let* ((fun-type (sal-name-expr/type fun))
                  (domain-type (sal-function-type/domain fun-type))
                  (rannge-type (sal-function-type/range fun-type))
                  (num-elems (if (sal-type/finite? domain-type)
                               (sal-type/number-of-elements-core domain-type (cut sal-expr/evaluate-core <> env 0))
                               'infinite)))
             (if (and (mpq? num-elems)
                      (<=mpq num-elems *max-function-domain-size-to-memoize*)
                      (sal-type/first-order? domain-type)
                      (sal-type/first-order? rannge-type))
               (let* ((new-fun (sal-ast/expand-core fun env depth))
                      (new-app (make-sal-application new-fun arg place-provider)))
;;		 [breakpoint "sal-expr/lift 2" (fun arg env depth definition new-fun new-app) #t]
                 (sal-expander/maintain-app new-app env depth))
               (begin
                 (check-depth fun depth)
;;		 [breakpoint "sal-expr/lift 3" (fun arg env depth definition) #t]
                 (sal-expr/lift definition arg env (+ depth 1))))))))
       (else
        (sal-expander/maintain-app app env depth))))))
  
(define-method (sal-expr/lift (fun <sal-lambda>) (arg <sal-expr>) (env <primitive>) (depth <primitive>))
  (make-sal-let+ (slot-value fun :local-decls)
                 (slot-value fun :expr)
                 (sal-argument->argument-list arg (length (slot-value fun :local-decls)))
                 env
                 (lambda (n e) (sal-ast/expand-core n e depth))))

(define-method (sal-expr/lift (fun <sal-conditional>) (arg <sal-expr>) (env <primitive>) (depth <primitive>))
  (let ((new-cond (try-to-evaluate (slot-value fun :cond-expr) env)))
    (cond
     ((sal-expr/true? new-cond)
      (sal-expr/lift (slot-value fun :then-expr) arg env depth))
     ((sal-expr/false? new-cond)
      (sal-expr/lift (slot-value fun :else-expr) arg env depth))
     (else
      (update-ast-slots fun
                        :cond-expr (sal-ast/expand-core (slot-value fun :cond-expr) env depth)
                        :then-expr (sal-expr/lift (slot-value fun :then-expr) arg env depth)
                        :else-expr (sal-expr/lift (slot-value fun :else-expr) arg env depth))))))

(define-method (sal-expr/lift (fun <sal-let-expr>) (arg <sal-expr>) (env <primitive>) (depth <primitive>))
  (let* ((local-decls (slot-value fun :local-decls))
         (new-local-decls (conservative-map-1 (lambda (decl)
                                                (ensure ((decl <sal-let-decl>))
                                                  (update-ast-slots decl :value (sal-ast/expand-core (slot-value decl :value) env depth))))
                                              local-decls))
         (env (update-env* env local-decls new-local-decls)))
    (update-ast-slots fun
                      :local-decls new-local-decls
                      :expr (sal-expr/lift (slot-value fun :expr) arg env depth))))

(define-method (sal-expr/lift (fun <sal-application>) (arg <sal-expr>) (env <primitive>) (depth <primitive>))
  (let ((new-fun (sal-ast/expand-core fun env depth))
        (place-provider fun))
    (if (sal-ast/equivalent? new-fun fun)
      (sal-expander/maintain-app (make-sal-application new-fun arg place-provider) env depth)
      (sal-expr/lift new-fun arg env depth))))

(define (evaluate-or-simplify value env)
  (try-until-success
   (sal-expr/evaluate-core value env 0)
   (sal-ast/eager-simplify-core value env)))

(define-method (sal-expr/lift (fun <sal-expr>) (arg <sal-expr>) (env <primitive>) (depth <primitive>))
   ;; Check if it is possible to transform fun using the evaluator...
  (let ((new-fun (evaluate-or-simplify fun env))
        (place-provider fun))
    (if (sal-ast/equivalent? new-fun fun)
      (sal-expander/maintain-app (make-sal-application new-fun arg place-provider) env depth)
      (sal-expr/lift new-fun arg env depth))))

(define-method (sal-ast/expand-core (ast <sal-application>) (env <primitive>) (depth <primitive>))
  (let ((result (sal-expr/lift (slot-value ast :fun) (slot-value ast :arg) env depth)))
;; BD:    [breakpoint "sal-ast/expand-core" (ast env depth result) #t]
    result))

(define-generic (sal-expr/auxiliary-decl-for ast source-decl env depth))

(define-method (sal-expr/auxiliary-decl-for (ast <sal-expr>) (source-decl <sal-decl>) (env <primitive>) (depth <primitive>))
  (let ((type (sal-expr/type ast)))
    (make-ast-instance <sal-auxiliary-decl> source-decl
                       :id (slot-value source-decl :id)
                       :type type
                       :value ast)))

(define (check-param param-list arg-list)
  [assert (param-list arg-list) (= (length param-list) (length arg-list))]
  (apply make-sal-and (map make-sal-equality param-list arg-list)))

(define (compute-application-result function domain-value env depth)
  (let ((tmp (sal-expr/apply function domain-value)))
    ;; (breakpoint "foo" (function domain-value tmp env) #t)
    (sal-ast/expand-core (evaluate-or-simplify tmp env) env depth)))

(define (create-function-body param-nameexpr-list domain-values function env depth)
  [assert (domain-values) (not (null? domain-values))]
  (let loop ((domain-values (cdr domain-values))
             (body (compute-application-result function (car domain-values) env depth)))
    (if (null? domain-values)
      body
      (let* ((curr-arg (car domain-values))
             (curr-arg-list (sal-argument->argument-list curr-arg (length param-nameexpr-list)))
             (curr-condition (check-param param-nameexpr-list curr-arg-list))
             (curr-value (compute-application-result function curr-arg env depth))
             (curr-body (make-ast-instance <sal-conditional> function
                                           :cond-expr curr-condition
                                           :then-expr curr-value
                                           :else-expr body)))
        (loop (cdr domain-values) curr-body)))))

(define-generic (make-function-base-on ast param-decls body))

(define-method (make-function-base-on (ast <sal-lambda>) (param-decls <primitive>) (body <sal-expr>))
  (make-ast-instance <sal-lambda> ast
                     :local-decls param-decls
                     :expr body))

(define-method (make-function-base-on (ast <sal-array-literal>) (param-decls <primitive>) (body <sal-expr>))
  [assert (param-decls) (= (length param-decls) 1)]
  (make-ast-instance <sal-array-literal> ast
                     :local-decls param-decls
                     :expr body))

(define-method (sal-expr/auxiliary-decl-for (ast <sal-lambda>) (source-decl <sal-decl>) (env <primitive>) (depth <primitive>))
  (try
   (let* ((type (sal-expr/type ast))
          (domain-type (sal-function-type/domain type))
          (type-list (sal-type->type-list domain-type))
          (param-decls (sal-type-list->sal-decl-list type-list))
          (param-nameexpr-list (sal-decl-list->sal-name-expr-list param-decls))
          (proc (lambda (expr) (sal-expr/evaluate-core expr env 0)))
          (it (sal-type/make-iterator-core domain-type proc))
          (domain-values (iterator->list it))
          (body (create-function-body param-nameexpr-list domain-values ast env depth)))
     (make-ast-instance <sal-auxiliary-decl> ast
                        :id (slot-value source-decl :id)
                        :type type
                        :value (make-function-base-on ast param-decls body)))
   (catch 'type-iterator
          (lambda (msg)
            (sign-expander-error ast "Higher order expression cannot be expanded, reason: ~a" msg)))))

(define-method (sal-ast/expand-core (ast <sal-qualified-name-expr>) (env <primitive>) (depth <primitive>))
  (cond 
   ((sal-name-expr/definition ast) =>
    (lambda (definition)
      (let* ((def-value (try-to-evaluate definition env))) ;; evaluate-or-error definition env ast)))
        (if (instance-of? def-value <sal-simple-expr>)
          (sal-ast/expand-core def-value env depth)
          (begin
            [assert (def-value) (not (sal-ast/contains-open-references? def-value))]
            (let ((aux-decl (sal-expr/auxiliary-decl-for def-value (slot-value ast :decl) env depth)))
              (make-sal-name-expr aux-decl ast)))))))
     (else
      ast)))

(memoize-sal-ast-method-for-closed-asts (sal-ast/expand-core (ast <sal-qualified-name-expr>) (env <primitive>) (depth <primitive>)))

(define-method (sal-ast/expand-core (ast <sal-mod-trans>) (env <primitive>) (depth <primitive>))
  (sign-cannot-be-expanded ast))

(define-method (sal-ast/expand-core (ast <sal-mod-init>) (env <primitive>) (depth <primitive>))
  (sign-cannot-be-expanded ast))

(define-generic (sal-quantified-expr/expand-core ast env proc))

(define *cc* 0)
(define (display-cc)
  (print *cc*)
  (set! *cc* (+ *cc* 1)))

(define (expand-quantifier-core ast env body-slot-id proc make-op-proc)
  (try
   (let* ((place-provider ast)
          (local-decls (slot-value ast :local-decls))
          (body (slot-value ast body-slot-id))
          (types (map (lambda (var-decl)
                        (ensure ((var-decl <sal-var-decl>))
                          (slot-value var-decl :type)))
                      local-decls))
          (tuple-type (make-ast-instance <sal-tuple-type> ast :types types))
          (proc-child (lambda (child)
                        (sal-expr/evaluate-core child env 0)))
          (iterator (sal-type/make-iterator-core tuple-type proc-child))
          (elems (make-queue)))
     (iterator/for-each 
      (lambda (tuple-literal)
        (ensure ((tuple-literal <sal-tuple-literal>))
          (let* ((env (update-env* env local-decls (slot-value tuple-literal :exprs)))
                 (expanded-body (proc body env)))
            (queue/insert! elems expanded-body))))
      iterator)
     (let ((result (apply make-op-proc (queue->list elems))))
       (sal-ast/copy-place-information! result place-provider)
       result))
   (catch* '(type-iterator expr-evaluator)
           (lambda (_ msg)
             (sign-expander-error ast "Failed to expand quantifier, reason: ~a" msg)))))

(define (expand-quantifier ast env proc make-op-proc)
  (let ((ast (sal-quantified-expr/local-simplify ast)))
    (if (not (instance-of? ast <sal-quantified-expr>))
      (proc ast env)
      (expand-quantifier-core ast env :expr proc make-op-proc))))

(define-method (sal-quantified-expr/expand-core (ast <sal-exists-expr>) (env <primitive>) (proc <primitive>))
  (expand-quantifier ast env proc make-sal-or+))

(define-method (sal-quantified-expr/expand-core (ast <sal-multi-choice-expr>) (env <primitive>) (proc <primitive>))
  (expand-quantifier ast env proc make-sal-choice+))

(define-method (sal-quantified-expr/expand-core (ast <sal-for-all-expr>) (env <primitive>) (proc <primitive>))
  (expand-quantifier ast env proc make-sal-and+))

(define-method (sal-quantified-expr/expand-core (ast <sal-multi-component-info>) (env <primitive>) (proc <primitive>))
  (expand-quantifier-core ast env :component proc (lambda args
                                                    (make-ast-instance <sal-composite-component-info> ast
                                                                       :components args))))

(define-generic (sal-ast/expand-quantifiers-core ast env polarity expand-pred?))

(define (expand-quantifiers-default ast env polarity expand-pred?)
  (let ((proc (lambda (a e) 
                (sal-ast/expand-quantifiers-core a e polarity expand-pred?))))
    ;; quantifier expansion will create several opportunities for simplification
    (sal-ast/local-simplify-core ast env proc)))

(define (expand-quantifiers-switching-to-pos-neg ast env expand-pred?)
  (let ((proc (lambda (a e) 
                (sal-ast/expand-quantifiers-core a e *pos-neg* expand-pred?))))
    ;; quantifier expansion will create several opportunities for simplification
    (sal-ast/local-simplify-core ast env proc)))
  
(define-method (sal-ast/expand-quantifiers-core (ast <sal-ast>) (env <primitive>) (polarity <polarity>) (expand-pred? <primitive>))
  (expand-quantifiers-default ast env polarity expand-pred?))

;; ************* IMPORTANT!!!! ****************
;;
;; TODO: I've to implement the effect of assertion-proposition over polarity...
;;
;;

(define-method (sal-ast/expand-quantifiers-core (ast <sal-module-models>) (env <primitive>) (polarity <polarity>) (expand-pred? <primitive>))
  (let ((new-module (sal-ast/expand-quantifiers-core (slot-value ast :module) env (polarity/invert polarity) expand-pred?))
        (new-expr (sal-ast/expand-quantifiers-core (slot-value ast :expr) env polarity expand-pred?)))
    (update-ast-slots ast
                      :module new-module
                      :expr new-expr)))

(define-method (sal-ast/expand-quantifiers-core (ast <sal-definition-expression>) (env <primitive>) (polarity <polarity>) (expand-pred? <primitive>))
  (expand-quantifiers-default ast env polarity expand-pred?))

(define-method (sal-ast/expand-quantifiers-core (ast <sal-ltl-g>) (env <primitive>) (polarity <polarity>) (expand-pred? <primitive>))
  (expand-quantifiers-default ast env polarity expand-pred?))

(define-method (sal-ast/expand-quantifiers-core (ast <sal-ltl-f>) (env <primitive>) (polarity <polarity>) (expand-pred? <primitive>))
  (expand-quantifiers-default ast env polarity expand-pred?))

(define-method (sal-ast/expand-quantifiers-core (ast <sal-ltl-x>) (env <primitive>) (polarity <polarity>) (expand-pred? <primitive>))
  (expand-quantifiers-default ast env polarity expand-pred?))

(define-method (sal-ast/expand-quantifiers-core (ast <sal-expr>) (env <primitive>) (polarity <polarity>) (expand-pred? <primitive>))
  ;; the default behavior for expressions is to switch to full expansion...
  (expand-quantifiers-switching-to-pos-neg ast env expand-pred?))

(define-method (sal-ast/expand-quantifiers-core (ast <sal-propositional-application>) (env <primitive>) (polarity <polarity>) (expand-pred? <primitive>))
  (expand-quantifiers-default ast env polarity expand-pred?))

(define-method (sal-ast/expand-quantifiers-core (ast <sal-iff>) (env <primitive>) (polarity <polarity>) (expand-pred? <primitive>))
  (expand-quantifiers-switching-to-pos-neg ast env expand-pred?))

(define-method (sal-ast/expand-quantifiers-core (ast <sal-xor>) (env <primitive>) (polarity <polarity>) (expand-pred? <primitive>))
  (expand-quantifiers-switching-to-pos-neg ast env expand-pred?))

(define-method (sal-ast/expand-quantifiers-core (ast <sal-not>) (env <primitive>) (polarity <polarity>) (expand-pred? <primitive>))
  (expand-quantifiers-default ast env (polarity/invert polarity) expand-pred?))

(define-method (sal-ast/expand-quantifiers-core (ast <sal-implies>) (env <primitive>) (polarity <polarity>) (expand-pred? <primitive>))
  (let* ((arg1 (sal-binary-application/arg1 ast))
         (proc (lambda (expr env)
                 (cond
                  ((eq? expr arg1)
                   (sal-ast/expand-quantifiers-core expr env (polarity/invert polarity) expand-pred?))
                  (else
                   (sal-ast/expand-quantifiers-core expr env polarity expand-pred?))))))
    (sal-ast/local-simplify-core ast env proc)))

(define-method (sal-ast/expand-quantifiers-core (ast <sal-conditional>) (env <primitive>) (polarity <polarity>) (expand-pred? <primitive>))
  (let* ((cond-expr (slot-value ast :cond-expr))
         (proc (lambda (expr env)
                 (cond 
                  ((eq? expr cond-expr)
                   (sal-ast/expand-quantifiers-core expr env *pos-neg* expand-pred?))
                  (else
                   (sal-ast/expand-quantifiers-core expr env polarity expand-pred?))))))
    (sal-ast/local-simplify-core ast env proc)))

(define-method (sal-ast/expand-quantifiers-core (ast <sal-let-decl>) (env <primitive>) (polarity <polarity>) (expand-pred? <primitive>))
  ;; This is an overapproximation... I'm *always* expanding the quantifiers in let-decls...
  (expand-quantifiers-switching-to-pos-neg ast env expand-pred?))

(define-method (sal-ast/expand-quantifiers-core (ast <sal-let-expr>) (env <primitive>) (polarity <polarity>) (expand-pred? <primitive>))
  ;; the previous method will force the desired behavior... that is, all quantifiers in let-delcs are expanded...
  ;; this is an overapproximation...
  (expand-quantifiers-default ast env polarity expand-pred?))

(define-method (sal-ast/expand-quantifiers-core (ast <sal-quantified-expr>) (env <primitive>) (polarity <polarity>) (expand-pred? <primitive>))
  (let ((local-decls (slot-value ast :local-decls))
        (vars-to-expand-queue (make-queue))
        (vars-to-keep-queue (make-queue)))
    (for-each (lambda (var-decl)
                (if (expand-pred? ast polarity var-decl)
                  (queue/insert! vars-to-expand-queue var-decl)
                  (queue/insert! vars-to-keep-queue var-decl)))
              local-decls)
    (let* ((vars-to-expand (queue->list vars-to-expand-queue))
           (vars-to-keep (queue->list vars-to-keep-queue))
           (expanded-body (if (null? vars-to-expand)
                            (sal-ast/expand-quantifiers-core (slot-value ast :expr) env polarity expand-pred?)
                            (let ((temp-quant-expr (copy-ast ast
                                                             :local-decls vars-to-expand
                                                             :expr (slot-value ast :expr))))
                              (sal-quantified-expr/expand-core temp-quant-expr env (lambda (a e) (sal-ast/expand-quantifiers-core a e polarity expand-pred?))))))
           (result-expr (if (null? vars-to-keep)
                          expanded-body
                          (copy-ast ast
                                    :local-decls vars-to-keep
                                    :expr expanded-body))))
      (sal-ast/local-simplify result-expr))))

(define-method (sal-ast/expand-quantifiers-core (ast <sal-multi-component-info>) (env <primitive>) (polarity <polarity>) (expand-pred? <primitive>))
  (sal-quantified-expr/expand-core ast env (lambda (a e) (sal-ast/expand-quantifiers-core a e polarity expand-pred?))))

; (define-method (sal-ast/expand-quantifiers-core (ast <sal-exists-expr>) (env <primitive>) (polarity <pos>))
;   ;; the quantifiers doesn't need to be expanded, since it can be skolemized
;   (keep-quantifier ast env polarity))
; (define-method (sal-ast/expand-quantifiers-core (ast <sal-exists-expr>) (env <primitive>) (polarity <neg>))
;   (sal-quantified-expr/expand-core ast env (lambda (a e) (sal-ast/expand-quantifiers-core a e polarity))))
; (define-method (sal-ast/expand-quantifiers-core (ast <sal-exists-expr>) (env <primitive>) (polarity <pos-neg>))
;   (sal-quantified-expr/expand-core ast env (lambda (a e) (sal-ast/expand-quantifiers-core a e polarity))))
; (define-method (sal-ast/expand-quantifiers-core (ast <sal-for-all-expr>) (env <primitive>) (polarity <neg>))
;   ;; the quantifiers doesn't need to be expanded, since it can be skolemized
;   (keep-quantifier ast env polarity))
; (define-method (sal-ast/expand-quantifiers-core (ast <sal-for-all-expr>) (env <primitive>) (polarity <pos>))
;   (sal-quantified-expr/expand-core ast env (lambda (a e) (sal-ast/expand-quantifiers-core a e polarity))))
; (define-method (sal-ast/expand-quantifiers-core (ast <sal-for-all-expr>) (env <primitive>) (polarity <pos-neg>))
;   (sal-quantified-expr/expand-core ast env (lambda (a e) (sal-ast/expand-quantifiers-core a e polarity))))
; (define-method (sal-ast/expand-quantifiers-core (ast <sal-multi-component-info>) (env <primitive>) (polarity <polarity>))
;   (sal-quantified-expr/expand-core ast env (lambda (a e) (sal-ast/expand-quantifiers-core a e polarity))))
  
(define (sal-ast/contains-quantifiers? ast)
  (sal-ast/find (lambda (n)
                  (or (instance-of? n <sal-quantified-expr>)
                      (instance-of? n <sal-multi-component-info>)))
                ast))

(define (sal-ast/expand-quantifiers ast)
  (cond
   ((sal-ast/contains-quantifiers? ast)
    (status-message :expanding-quantifiers)
    (verbose-message 1 "unfolding quantifiers...")
    (display-runtime 2 "  unfolding quantifiers time: ~a secs"
      (lambda ()
        (sal-ast/expand-quantifiers-core ast 
                                         (make-empty-env) 
                                         *pos-neg*
                                         (lambda (_ __ ___) #t)))
      :quantifier-expansion-time))
   (else
    ast)))

(define (sal-ast/conditionally-expand-quantifiers ast expand-pred?)
  (cond
   ((sal-ast/contains-quantifiers? ast)
    (status-message :expanding-quantifiers)
    (verbose-message 1 "unfolding quantifiers...")
    (display-runtime 2 "  unfolding quantifiers time: ~a secs"
      (lambda ()
        (sal-ast/expand-quantifiers-core ast (make-empty-env) *pos* expand-pred?))
      :quantifier-expansion-time))
   (else
    ast)))

(define-generic (skolem-expand? ast polarity var-decl))
(define-method (skolem-expand? (ast <sal-quantified-expr>) (polarity <polarity>) (var-decl <sal-var-decl>)) #t)
(define-method (skolem-expand? (ast <sal-exists-expr>) (polarity <neg>) (var-decl <sal-var-decl>)) #f)
(define-method (skolem-expand? (ast <sal-for-all-expr>) (polarity <pos>) (var-decl <sal-var-decl>)) #f)

(define-generic (skolem-expand-finite? ast polarity var-decl))
(define-method (skolem-expand-finite? (ast <sal-quantified-expr>) (polarity <polarity>) (var-decl <sal-var-decl>)) #t)
(define-method (skolem-expand-finite? (ast <sal-exists-expr>) (polarity <neg>) (var-decl <sal-var-decl>)) (sal-type/finite? (slot-value var-decl :type)))
(define-method (skolem-expand-finite? (ast <sal-for-all-expr>) (polarity <pos>) (var-decl <sal-var-decl>)) (sal-type/finite? (slot-value var-decl :type)))

   
;; The following function will expand all name-exprs whose declaration is
;; mapped in env. The function will also remove any let-expression found in the given ast.
;; So, this function should be used with care, since the user can get an exponential
;; blow-up.
(define-generic (sal-ast/expand-names ast env))
(define-method (sal-ast/expand-names (ast <sal-ast>) (env <primitive>))
  (sal-ast/map ast env sal-ast/expand-names))
(define-method (sal-ast/expand-names (ast <sal-name-expr>) (env <primitive>))
  (cond
   ((lookup-env (slot-value ast :decl) env) =>
    (lambda (new-ref)
      (cond
       ((and (instance-of? new-ref <sal-var-decl>)
             (sal-var-decl/definition new-ref))
        =>
        identity)
       ((instance-of? new-ref <sal-decl>)
        (update-ast-slots ast :decl new-ref))
       (else
        new-ref))))
   (else
    ast)))
(define-method (sal-ast/expand-names (ast <sal-let-expr>) (env <primitive>))
  ;; update the environment
  (let ((new-env (update-env* env 
                              (slot-value ast :local-decls) 
                              (map (lambda (let-decl)
                                     (sal-ast/expand-names (slot-value let-decl :value) env))
                                   (slot-value ast :local-decls))))) 
    (sal-ast/expand-names (slot-value ast :expr) new-env)))

;; expand the applications that satisfy the predicate pred?
(define-generic (sal-ast/expand-applications ast env pred?))
(define-method (sal-ast/expand-applications (ast <sal-ast>) (env <primitive>) (pred? <primitive>))
  (sal-ast/map ast env (lambda (child env) (sal-ast/expand-applications child env pred?))))
(define-method (sal-ast/expand-applications (expr <sal-application>) (env <primitive>) (pred? <primitive>))
  (if (pred? expr)
    (let ((new-fun (sal-ast/substitute (slot-value expr :fun) env))) ;; apply substitutions
      (cond
       ((and (instance-of? new-fun <sal-name-expr>)
             (sal-name-expr/definition new-fun))
        =>
        (lambda (def)
          (sal-ast/expand-applications (sal-expr/apply def (slot-value expr :arg)) env pred?)))
       (else
        (call-next-method))))
    (call-next-method)))
