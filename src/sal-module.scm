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

(module sal-module
        (include "sal.sch")
        (include "sal-ast-table.sch")
        (include "fast-hash-table.sch")
        (import symbol-table sal-ast-env sal-ast-copy sal-ast-instantiate sal-ast-list
                queue symbol-set sal-ast-for-each sal-decls sal-expression sal-string-reader
                sal-type-checker sal-environment sal-decls gmp-scheme sal-type)
        (export (sal/basemodule-running-var-name)
                (sal-module/state-variables module)
                (sal-module/set-state-variables! module state-vars)
                (sal-module/state-variables-table module)
                (sal-module/lookup-var module var-name)
                (sal-module/type module)
                (sal-module/record-literal module)
                (sal-module/next-record-literal module)
                (sal-module/update-interface! module)
                (sal-module-name/definition module)
                (sal-module-instance/expand module)
                (sal-module/reset!)
                (sal-module/defined-variables flat-module)
                (sal-module/choice-vars module)
                (sal-module/display-var-info module)
                (sal-module/rebind-expr source-module source-expr target-module . env)
                (sal-module/state-expr-string->ast module str)
                (sal-derived-flat-module/original-flat-module flat-module)
                (sal-derived-flat-module/original-boolean-flat-module flat-module)
                (sal-derived-flat-module/original-simple-data-flat-module flat-module)
                (sal-flat-module/slice-of? flat-module-1 flat-module-2)
                (sal-module/num-input-values module)
                (sal-module/finite-state? module)
                (sal-module/potential-state-space-size module))
        )

;; In SAL, every module has a default variable called 'running'
(define (sal/basemodule-running-var-name)
  'running)

(define (sal-module/state-variables module)
  [assert (module) (instance-of? module <sal-module>)]
  (unless (slot-value module :state-vars)
    (sal-module/update-interface! module))
  (slot-value module :state-vars))

(define (sal-module/set-state-variables! module state-vars)
  (set-slot-value! module :state-vars state-vars)
  (set-slot-value! module :state-vars-table #f))

(define-generic (sal-module/update-state-variable-table! module))
(define-method (sal-module/update-state-variable-table! (module <sal-module>))
  (let ((state-vars (sal-module/state-variables module)))
    (set-slot-value! module :state-vars-table (sal-decl-list->symbol-table state-vars))))
(define-method (sal-module/update-state-variable-table! (module <sal-module-instance>))
  (let ((expanded-module (sal-module-instance/expand module)))
    (set-slot-value! module :state-vars-table (sal-module/state-variables-table expanded-module))))

(define (sal-module/state-variables-table module)
  (unless (slot-value module :state-vars-table)
    (sal-module/update-state-variable-table! module))
  (slot-value module :state-vars-table))

(define (sal-module/lookup-var module var-name)
  (symbol-table/lookup (sal-module/state-variables-table module) var-name))

(define (sal-module/type module)
  (make-ast-instance <sal-record-type> module
                     :fields (map-and-filter (lambda (var-decl)
                                               (and (not (instance-of? var-decl <sal-choice-input-state-var-decl>))
                                                    (change-ast-class var-decl <sal-field>)))
                                             (sal-module/state-variables module))))

(define (sal-module/record-literal-core module mk-state-ref-proc)
  (make-ast-instance <sal-state-record-literal> module
                     :entries (map-and-filter (lambda (var-decl)
                                                (and (not (instance-of? var-decl <sal-choice-input-state-var-decl>))
                                                     (make-ast-instance <sal-record-entry> var-decl
                                                                        :id (slot-value var-decl :id)
                                                                        :expr (mk-state-ref-proc var-decl))))
                                              (sal-module/state-variables module))))

(define (sal-module/record-literal module)
  (sal-module/record-literal-core module make-sal-name-expr))

(define (sal-module/next-record-literal module)
  (sal-module/record-literal-core module (lambda (var-decl)
                                           (make-ast-instance <sal-next-operator> var-decl
                                                              :name-expr (make-sal-name-expr var-decl)))))

(define-generic (sal-module/update-interface! module))

(define-method (sal-module/update-interface! (module <sal-base-module>))
  ;; do nothing
  module)

(define-method (sal-module/update-interface! (module <sal-flat-module>))
  ;; do nothing
  module)

(define (merge-local-decls local1 local2)
  (copy-ast local1
            :type (make-ast-instance <sal-tuple-type> local1
                                     :types (list (slot-value local1 :type)
                                                  (slot-value local2 :type)))))

(define (throw-local-var-exception module var)
  (sign-source-error module "Invalid module composition, the set of local variables must be disjoint from the sets of input, output and global variables in the resultant module. The local variable \"~a\" does not satisfy this rule." var))

(define (throw-shared-output-exception module var)
  (sign-source-error module "Invalid module composition, the output variable \"~a\" is an output/global variable in both modules." var))

(define (move-vars-to-result-table module state-vars defined-table)
  (for-each 
   (lambda (decl)
     (let* ((var (sal-decl/name decl))
            (already-decl (symbol-table/lookup defined-table var))
            (add! (lambda (decl)
                    (symbol-table/add! defined-table var decl)))
            (simple-add! (lambda ()
                           (add! decl))))
       (if (not already-decl)
         (simple-add!)
         (cond
          ((instance-of? decl <sal-input-state-var-decl>)
           (when (instance-of? already-decl <sal-local-state-var-decl>)
             (throw-local-var-exception module var)))
          ((instance-of? decl <sal-output-state-var-decl>)
           (cond
            ((instance-of? already-decl <sal-input-state-var-decl>)
             (simple-add!))
            ((instance-of? already-decl <sal-local-state-var-decl>)
             (throw-local-var-exception module var))
            (else
             (throw-shared-output-exception module var))))
          ((instance-of? decl <sal-global-state-var-decl>)
           (cond 
            ((instance-of? already-decl <sal-local-state-var-decl>)
             (throw-local-var-exception module var))
            ((instance-of? already-decl <sal-output-state-var-decl>)
             (throw-shared-output-exception module var))
            (else
             (simple-add!))))
          ((instance-of? decl <sal-local-state-var-decl>)
           (cond 
            ((instance-of? already-decl <sal-local-state-var-decl>)
             (add! (merge-local-decls already-decl decl)))
            (else
             (throw-local-var-exception module var))))))))
   state-vars))

(define (decl-table->decl-list table)
  (symbol-table/fold
   (lambda (var-name var-decl lst)
     (cons var-decl lst))
   '()
   table))

(define-method (sal-module/update-interface! (module <sal-module-composition>))
  (let ((mod1 (slot-value module :module1))
        (mod2 (slot-value module :module2))
        (defined-table (make-symbol-table)))
    (move-vars-to-result-table module (sal-module/state-variables mod1) defined-table)
    (move-vars-to-result-table module (sal-module/state-variables mod2) defined-table)
    (sal-module/set-state-variables! module (decl-table->decl-list defined-table))
    module))

(define (check-synch-composition-constraint module)
  (let* ((mod1 (slot-value module :module1))
         (mod2 (slot-value module :module2))
         (state-vars1 (sal-module/state-variables mod1)))
    (for-each
     (lambda (decl)
       (when (and (instance-of? decl <sal-global-state-var-decl>)
                  (let ((other-decl (sal-module/lookup-var mod2 (sal-decl/name decl))))
                    (and other-decl
                         (instance-of? other-decl <sal-global-state-var-decl>))))
         (sign-source-error module "Invalid synchronous composition, modules cannot share the global variable \"~a\"." (sal-decl/name decl))))
     state-vars1)))

(define-method (sal-module/update-interface! (module <sal-synch-composition>))
  (check-synch-composition-constraint module)
  (call-next-method))

(define (convert-to-array-var module array-idx-type local-decl)
  (let* ((type (slot-value local-decl :type))
         (place-provider module)
         (new-type (make-ast-instance <sal-array-type> place-provider
                                  :domain array-idx-type
                                  :range type)))
    (make-ast-instance <sal-local-state-var-decl> place-provider
                   :id (slot-value local-decl :id)
                   :type new-type)))
                     
(define (check-if-contain-output-variables module)
  (for-each
   (lambda (decl)
     (when (instance-of? decl <sal-output-state-var-decl>)
       (sign-source-error module "Invalid multicomposition, the output variable ~a is clashing. This problem can be solved using renaming. For instance, an output variable \"x\" can be renamed to \"X[i]\" where \"X\" is an array, and \"i\" is the index variable used in the multicomposition." 
                          (sal-decl/name decl))))
   (sal-module/state-variables module)))

(define-method (sal-module/update-interface! (module <sal-multi-composition>))
  (let ((nested-mod (slot-value module :module))
        (array-idx-type (slot-value (car (slot-value module :local-decls)) :type))
        (new-state-vars-queue (make-queue)))
    (check-if-contain-output-variables nested-mod)
    (for-each
     (lambda (decl)
       (if (instance-of? decl <sal-local-state-var-decl>)
         (queue/insert! new-state-vars-queue (convert-to-array-var module array-idx-type decl))
         (queue/insert! new-state-vars-queue decl)))
     (sal-module/state-variables nested-mod))
    (sal-module/set-state-variables! module (queue->list new-state-vars-queue))
    module))

(define (check-if-contain-global-variables module)
  (for-each
   (lambda (decl)
     (when (instance-of? decl <sal-global-state-var-decl>)
       (sign-source-error module "Invalid multi synchronous composition, the modules cannot share global variables. The global variable ~a is the source of the problem." (sal-decl/name decl))))
   (sal-module/state-variables module)))

(define-method (sal-module/update-interface! (module <sal-multi-synch-composition>))
  (call-next-method)
  (check-if-contain-global-variables module)
  module)


(define-method (sal-module/update-interface! (module <sal-with-module>))
  (let* ((nested-mod (slot-value module :module))
         (nested-mod-state-vars (sal-module/state-variables nested-mod)))
    (for-each
     (lambda (new-decl)
       (when (sal-module/lookup-var nested-mod (sal-decl/name new-decl))
         (sign-source-error module "A `with' module cannot hide state variables, that is, it cannot redeclare state variables. The `~a' was redeclared."
                            (sal-decl/name new-decl))))
     (slot-value module :new-state-vars))
    (sal-module/set-state-variables! module (append (slot-value module :new-state-vars) nested-mod-state-vars))))

(define (sal-renaming/renamed-variables-set renaming)
  (let* ((nested-mod (slot-value renaming :module))
         (renames (slot-value renaming :renames)))
    (fold-left
     (lambda (set rename)
       (let* ((from-name (sal-identifier/name (slot-value rename :from-name)))
              (from-name-decl (sal-module/lookup-var nested-mod from-name))
              (to-expr (slot-value rename :to-expr)))
         ;; I had to add this constraint to simplify how flat-modules are computed.
         (when (and (instance-of? from-name-decl <sal-global-state-var-decl>)
                    (not (instance-of? to-expr <sal-name-expr>)))
           (sign-unsupported-feature rename "Invalid global variable renaming of `~a'. In SALenv, global variables can only be renamed to new names, that is, you cannot rename a global variable to a tuple/record/array selection." from-name))
         (symbol-set/add! set from-name)))
     (make-symbol-set)
     renames)))

(define-method (sal-module/update-interface! (module <sal-renaming>))
  (let* ((nested-mod (slot-value module :module))
         (nested-mod-state-vars (sal-module/state-variables nested-mod))
         (renamed-variables (sal-renaming/renamed-variables-set module))
         (new-state-vars-queue (make-queue)))
    (for-each 
     (lambda (decl)
       (let ((var-name (sal-decl/name decl)))
         (if (symbol-set/member? var-name renamed-variables)
           (symbol-set/delete! renamed-variables var-name)
           (queue/insert! new-state-vars-queue decl))))
     nested-mod-state-vars)
    (unless (symbol-set/empty? renamed-variables)
      (sign-source-error module "Invalid rename, the following `from' variables are not state variables ~a." (symbol-set->list renamed-variables)))
    (sal-module/set-state-variables! module (queue->list new-state-vars-queue))
    module))

(define (sal-identifier-list->set identifiers)
  (fold-left
   (lambda (set id)
     (symbol-set/add! set (sal-identifier/name id)))
   (make-symbol-set)
   identifiers))

(define-method (sal-module/update-interface! (module <sal-hiding>))
  (let* ((ids (slot-value module :identifiers))
         (id-set (sal-identifier-list->set ids))
         (nested-module (slot-value module :module))
         (new-state-vars-queue (make-queue)))
    (for-each
     (lambda (decl)
       (let ((var (sal-decl/name decl)))
         (if (symbol-set/member? var id-set)
           (if (or (instance-of? decl <sal-global-state-var-decl>)
                   (instance-of? decl <sal-output-state-var-decl>))
             (begin
              (queue/insert! new-state-vars-queue (change-ast-class (copy-ast decl) <sal-local-state-var-decl>))
              (symbol-set/delete! id-set var))
             (sign-source-error module "Invalid module expression, the variable ~a is not an output/global variable, so it cannot be hidden." var))
           (queue/insert! new-state-vars-queue decl))))
     (sal-module/state-variables nested-module))
    (unless (symbol-set/empty? id-set)
      (sign-source-error module "Invalid module expression, the following variable(s) ~a is(are) unknown, so they cannot be hidden." (symbol-set->list id-set)))
    (sal-module/set-state-variables! module (queue->list new-state-vars-queue))
    module))

(define-method (sal-module/update-interface! (module <sal-new-output>))
  (let* ((ids (slot-value module :identifiers))
         (id-set (sal-identifier-list->set ids))
         (nested-module (slot-value module :module))
         (new-state-vars-queue (make-queue)))
    (for-each
     (lambda (decl)
       (let ((var (sal-decl/name decl)))
         (if (symbol-set/member? var id-set)
           (if (instance-of? decl <sal-global-state-var-decl>)
             (begin
               (queue/insert! new-state-vars-queue (change-ast-class decl <sal-output-state-var-decl>))
               (symbol-set/delete! id-set var))
             (sign-source-error module "Invalid module expression, the variable ~a is not a global variable, so it cannot be converted to an output variable." var))
           (queue/insert! new-state-vars-queue decl))))
     (sal-module/state-variables nested-module))
    (unless (symbol-set/empty? id-set)
      (sign-source-error module "Invalid module expression, the following variable(s) ~a is(are) unknown, so they cannot be converted to output variable(s)." (symbol-set->list id-set)))
    (sal-module/set-state-variables! module (queue->list new-state-vars-queue))
    module))

(define-method (sal-module/update-interface! (module <sal-module-instance>))
  (let ((expanded-module (sal-module-instance/expand module)))
    (sal-module/set-state-variables! module (sal-module/state-variables expanded-module))))

(define-generic (sal-module-name/definition module))
(define-method (sal-module-name/definition (module <sal-module-name>))
  (slot-value (slot-value module :decl) :parametric-module))
(define-method (sal-module-name/definition (module <sal-qualified-module-name>))
  (sal-ast/instantiate (slot-value (slot-value module :decl) :parametric-module)
                       (slot-value module :actuals)))

(memoize-sal-ast-method (sal-module-name/definition (module <sal-qualified-module-name>)))
      
(define-generic (sal-module-instance/expand module))

(define-method (sal-module-instance/expand (module <sal-module-instance>))
  (trace 'module "expanding module instance ~a" (sal-ast->list module))
  (let* ((module-name (slot-value module :module-name))
         (actuals (slot-value module :actuals))
         (parametric-module (sal-module-name/definition module-name))
         (params (slot-value parametric-module :local-decls))
         (body (slot-value parametric-module :module))
         (env (update-env* (make-empty-env) params actuals))
         (result (sal-ast/substitute body env)))
    (trace 'module "  result: ~a" (sal-ast->list result))
    result))

(memoize-sal-ast-method (sal-module-instance/expand (module <sal-module-instance>)))

(define (sal-flat-module/defined-variables-core flat-module proc-add)
  (sal-ast/for-each (lambda (ast)
                      (when (instance-of? ast <sal-definition-expression>)
                        (for-each (lambda (lhs)
                                    (proc-add (slot-value (sal-lhs/name-expr lhs) :decl)))
                                  (slot-value ast :lhs-list)))
                      (when (instance-of? ast <sal-assignment>)
                        (let ((lhs (sal-binary-application/arg1 ast)))
                          (proc-add (slot-value (sal-lhs/name-expr lhs) :decl)))))
                    (slot-value flat-module :definition)))

(define *defined-variables-cache* (make-eq-hash-table))
(define (sal-module/reset!)
  (set! *defined-variables-cache* (make-eq-hash-table)))

(define-generic (sal-module/defined-variables module))

(memoize-method (sal-module/defined-variables (module <sal-module>)))

(define-method (sal-module/defined-variables (module <sal-flat-module>))
  (sal-flat-module/defined-variables module))

(define (sal-flat-module/defined-variables flat-module)
  (let* ((result (make-eq-hash-table))
         (proc-add (lambda (decl)
                     (eq-hash-table/put! result decl #unspecified))))
      (sal-flat-module/defined-variables-core flat-module proc-add)
      result))

(define (sal-module/choice-vars module)
  (let ((choice-vars (make-queue)))
    (for-each (lambda (state-var-decl)
                (when (instance-of? state-var-decl <sal-choice-input-state-var-decl>)
                  (queue/insert! choice-vars state-var-decl)))
              (sal-module/state-variables module))
    (queue->list choice-vars)))

(define (sal-module/display-var-info module)
  (let* ((total-num-vars (length (sal-module/state-variables module)))
         (num-aux-vars (length (sal-module/choice-vars module)))
         (num-sys-vars (- total-num-vars num-aux-vars)))
    (verbose-message 1 "number of system variables: ~a, number of auxiliary variables: ~a" 
                     num-sys-vars num-aux-vars)))

;; The source-expr has binds to the state variables of source-module.
;; This function returns a new expression, where these binds are redirected to target-module.
(define (sal-module/rebind-expr source-module source-expr target-module . env)
  (let* ((env (optional-arg env (make-empty-env)))
         (source-state-vars (sal-module/state-variables source-module))
         (new-env (let loop ((source-state-vars source-state-vars)
                             (env env))
                    (if (null? source-state-vars)
                      env
                      (let* ((curr-source-var (car source-state-vars))
                             (curr-source-var-name (sal-decl/name curr-source-var))
                             (curr-target-var (sal-module/lookup-var target-module curr-source-var-name))
                             (new-env (if curr-target-var (update-env env curr-source-var curr-target-var) env)))
                        (loop (cdr source-state-vars)
                              new-env))))))
    (sal-ast/substitute source-expr new-env)))

;; cached value for the function sal-module/state-expr-string->ast
(define *cached-sym-tab* #unspecified)
(define *cached-sym-tab-module* #unspecified)

(define-api (sal-module/state-expr-string->ast (module <sal-module>) (str string?))
  :doc "Convert a string representing a SAL state expression in an AST."
  (let* ((state-vars (sal-module/state-variables module))
         (sym-tab (cond 
                   ((eq? module *cached-sym-tab-module*) 
                    *cached-sym-tab*)
                   (else 
                    (set! *cached-sym-tab-module* module)
                    (set! *cached-sym-tab* (sal-decl-list->symbol-table state-vars))
                    *cached-sym-tab*)))
         (sal-env (sal-ast/sal-env module))
         (result (sal-string-reader/read-expr-using (sal-env/string-reader sal-env) str sym-tab)))
    result))

(define-generic (sal-derived-flat-module/original-flat-module flat-module))

(define-method (sal-derived-flat-module/original-flat-module (flat-module <sal-flat-module>))
  flat-module)

(define-method (sal-derived-flat-module/original-flat-module (flat-module <sal-derived-flat-module>))
  (sal-derived-flat-module/original-flat-module (slot-value flat-module :original-module)))

(define-generic (sal-derived-flat-module/original-boolean-flat-module flat-module))

(define-method (sal-derived-flat-module/original-boolean-flat-module (flat-module <sal-boolean-flat-module>))
  flat-module)

(define-method (sal-derived-flat-module/original-boolean-flat-module (flat-module <sal-sliced-boolean-flat-module>))
  (sal-derived-flat-module/original-boolean-flat-module (slot-value flat-module :original-module)))

(define-generic (sal-derived-flat-module/original-simple-data-flat-module flat-module))

(define-method (sal-derived-flat-module/original-simple-data-flat-module (flat-module <sal-simple-data-flat-module>))
  flat-module)

(define-method (sal-derived-flat-module/original-simple-data-flat-module (flat-module <sal-sliced-simple-data-flat-module>))
  (sal-derived-flat-module/original-simple-data-flat-module (slot-value flat-module :original-module)))

;; returns true if flat-module-1 is a slice of flat-module-2
(define-generic (sal-flat-module/slice-of? flat-module-1 flat-module-2))

(define-method (sal-flat-module/slice-of? (flat-module-1 <sal-flat-module>) (flat-module-2 <sal-flat-module>))
  #f)

(define-method (sal-flat-module/slice-of? (flat-module-1 <sal-sliced-flat-module>) (flat-module-2 <sal-flat-module>))
  (let ((original-flat-module-1 (slot-value flat-module-1 :original-module)))
    (if (eq? original-flat-module-1 flat-module-2)
      #t
      (sal-flat-module/slice-of? original-flat-module-1 flat-module-2))))
  
(define (sal-module/num-input-values module)
  (cond
   ((fold-left (lambda (curr var-decl)
                 (if (and (instance-of? var-decl <sal-input-state-var-decl>)
                          (not (instance-of? var-decl <sal-choice-input-state-var-decl>)))
                   (let ((num-vals (sal-type/number-of-elements (slot-value var-decl :type))))
                     (if curr
                       (*mpq curr num-vals)
                       num-vals))
                   curr))
               #f
               (sal-module/state-variables module))
    => identity)
   (else *mpq-zero*)))

(define (sal-module/finite-state? module)
  (for-all (lambda (var-decl)
             (sal-type/finite? (slot-value var-decl :type)))
           (sal-module/state-variables module)))

(define (sal-module/potential-state-space-size module)
  (let ((defined-vars (sal-module/defined-variables module)))
    (cond 
     ((fold-left (lambda (curr var-decl)
                   (if (or (instance-of? var-decl <sal-input-state-var-decl>)
                           (eq-hash-table/get defined-vars var-decl))
                     curr
                     (let ((num-vals (sal-type/number-of-elements (slot-value var-decl :type))))
                       (if curr
                         (*mpq curr num-vals)
                         num-vals))))
                 #f
                 (sal-module/state-variables module))
      => identity)
     (else
      *mpq-zero*))))

