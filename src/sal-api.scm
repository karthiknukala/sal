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

(module sal-api
        (include "sal.sch")
        (import front-end sal-cse sal-ast-simplify sal-expression sal-type sal-context 
                sal-module sal-environment sal-ast-table unique-names runtime sal-slicer
                sal-finite-expressions sal-ast-expand sal-assertion sal-flat-modules
                sal-smc-core sal-pseudo-lets sal-ast-env sal-flat-data-structures
                simple-abstraction ltl-ctl fast-hash-table sal-pp sal-module-simplifications) 
        (export *sal-env*
                (sal/init-main-sal-env!)
                (sal/context ctx-name)
                (sal/import str)
                (sal/expr str)
                (sal/state-expr str module)
                (sal/type str)
                (sal/assertion-name str)
                (sal/expr-type str)
                (sal/module str)
                (sal/reset!)                
                (sal/set-make-sal-string-reader-proc! proc)
                (sal/enable-cse! flag)      
                (sal/enable-simplifier! flag)
                (sal/enable-slicer! flag)
                (sal/simplify ast)
                (sal/slice ast)
                (sal/cse ast)
                (sal-module-models/create-ltl-monitor module-models . bool-ba?)
                (make-boolean-assertion assertion . svsv)
                (make-flat-assertion assertion . svsv)
                (make-boolean-flat-module module . svsv)
                (make-flat-module module . svsv)
                (make-boolean-state-expression-core expr bool-flat-module)
                (make-simple-data-state-expression-core expr simple-data-flat-module)
                (make-boolean-state-expression expr-str bool-flat-module)
                (make-simple-data-state-expression expr-str flat-module))
        )

(define *sal-env* #unspecified)

(define (sal/init-main-sal-env!)
  (set! *sal-env* (make-sal-env)))

(define-api (sal/context (ctx-name string-or-symbol?))
  :doc "Import a SAL context and create a SAL context object. @code{ctx-name} is the name of the context. SAL will use the environment variable @code{SALCONTEXTPATH} to search for the context file." 
  (sal-env/context *sal-env* ctx-name))

(define-api (sal/import (str string?))
  (sal-env/read-import-string *sal-env* str)
  #unspecified)

(define-api (sal/expr (str string?))
  :doc "Convert a string into a SAL expression"
  :examples '((sal/expr "lambda (x:natural): x+1"))
  (sal-env/expr-string->ast *sal-env* str))

(define-api (sal/state-expr (str string?) (module <sal-module>))
  :doc "Convert a string into a SAL expression which may reference the variables of the given module."
  :examples '((sal/state-expr "x1 and x2" (sal/module "peterson!system")))
  (sal-module/state-expr-string->ast module str))

(define-api (sal/type (str string?))
  :doc "Convert a string into a SAL type."
  :examples '((sal/type "[ [0..10] -> bool ]")
              (sal/type "[# idx: [0..3], flag : bool #]"))
  (sal-env/type-string->ast *sal-env* str))

(define-api (sal/assertion-name (str string?))
  :doc "Convert a string into a SAL assertion qualified name."
  :examples '((sal/assertion-name "peterson!mutex"))
  (sal-env/assertion-name-string->ast *sal-env* str))

(define-api (sal/expr-type (str string?))
  (sal-expr/type (sal/expr str)))

(define-api (sal/module (str string?))
  :doc "Convert a string into a SAL module."
  :examples '((sal/module "peterson!mutex"))
  (sal-env/module-string->ast *sal-env* str))

(define-api (sal/reset!)
  :doc "Reset the SAL environment instance referenced by *sal-env*"
  (sal/reset-memoized-ast-method-cache-tables!)
  (sal/reset-memoized-method-cache-tables!)
  (unique-name/reset!)
  (sal-module/reset!)
  (sal-env/reset! *sal-env*)
  (force-gc!))

(define-api (sal/set-make-sal-string-reader-proc! (proc procedure?))
  :doc "Set the SAL string reader generator. The current string reader of *sal-env* is reinitialized, using the new generator."
  :examples '((sal/set-make-sal-string-reader-proc! make-sal-string-reader))
  (set! *make-sal-string-reader-proc* proc)
  (sal-env/initialize-string-reader! *sal-env*))

(define *cse-enabled?* #t)
(define *simplifier-enabled?* #t)
(define *slice?* #f)

(define-api (sal/enable-cse! (flag boolean?))
  :doc "Enable/Disable common subexpression elimination."
  (set! *cse-enabled?* flag))

(define-api (sal/enable-simplifier! (flag boolean?))
  :doc "Enable/Disable abstract syntax tree simplifications."
  (set! *simplifier-enabled?* flag))

(define-api (sal/enable-slicer! (flag boolean?))
  :doc "Enable/Disable slicer. A slicer removes state variables that are irrelevant to prove a property."
  (set! *slice?* flag))

(front-end/add-toggle-option! "Code Transformations""cse" "common subexpression elimination (default: enabled)."
                              (lambda (flag)
                                (set! *cse-enabled?* flag)))
(front-end/add-toggle-option! "Code Transformations" "simp" "code simplifications (default: enabled)."
                              (lambda (flag)
                                (set! *simplifier-enabled?* flag)))
(front-end/add-toggle-option! "Code Transformations" "slicer" "slicer (i.e., cone of influence) (default: disabled)."
                              (lambda (flag) 
                                (set! *slice?* flag)))

(define *vars-to-abstract* '())

(front-end/add-simple-option! 
 "Abstraction"
 "--abs-vars=<file-name>"
 "Name of the file that contains the variables to be abstracted. For each variable appearing in the file, a new input variable is created to drive all the variables that were previously driven by the variable. Abstracting a variable effectively allows it to take any value in its range, at every step. The syntax of the input file is: '(' <var-name>* ')'"
 (lambda (arg)
   (try
    (with-input-from-file arg 
      (lambda ()
        (set! *vars-to-abstract* (read))
        (unless (and (list? *vars-to-abstract*)
                     (for-all symbol? *vars-to-abstract*))
          (set! *vars-to-abstract* '())
          (sign-error "")))) ;; error
    (lambda (escape proc msg obj)
      (sign-error "Failed to open/read file `~a', the file which contains variables to abstract should use the following syntax:\n        '(' <var-name>* ')'" arg)))))

(define-api (sal/cse (ast <sal-ast>))
  :doc "Return a new abstract syntax tree node, where the common subexpressions were eliminated. It is the identity function if common subexpression elimination is disabled."
  :examples '((sal/cse (sal/expr "lambda (x:natural): (x + x) * (x + x)")))
  (if *cse-enabled?*
    (sal-ast/cse ast)
    ast))

(define-api (sal/simplify (ast <sal-ast>))
  :doc "Return a new simplified abstract syntax tree node. It is the identity function if abstract syntax tree simplification is disabled."
  :examples '((sal/simplify (sal/expr "if true then 2 else 3 endif")))
  (status-message :simplifying-ast)
  (verbose-message 1 "simplifying abstract syntax tree...")
  (display-runtime 2 "  simplification time: ~a secs"
    (lambda ()
      (if *simplifier-enabled?*
        (sal-ast/simplify ast)
        ast))
    :simplification-time))

(define-api (sal/slice (ast <sal-module-models>))
  :doc "Slice a module models assertion. The state variables which are irrelevant to prove the assicated property are eliminated. It is the identity function if slicing is disabled." 
  (if *slice?*
    (sal-module-models/slice ast)
    ast))

(define (sal/simple-abstraction ast)
  (if (null? *vars-to-abstract*)
    ast
    (sal-ast/simple-abstraction ast *vars-to-abstract*)))

(define-api (sal-module-models/create-ltl-monitor (ast <sal-module-models>) . bool-ba?)
  :doc "Convert a LTL property into a monitor (Buchi Automata). If @code{bool-ba?} is provided and is true, then the Buchi Automata will only introduce boolean variables."
  (let ((bool-ba? (optional-arg bool-ba? #f)))
    (cond
     ((sal-module-models/ctl-property? ast)
      (let* ((property (slot-value ast :expr))
             (ltl-property (ctl->ltl property))
             (new-ast (copy-instance ast :expr ltl-property)))
        (sal-module-models/ltl->ba new-ast bool-ba?)))
     (else
      (sal-module-models/ltl->ba ast bool-ba?)))))

(define-generic (obj->flat-assertion obj))

(define-method (obj->flat-assertion (obj <primitive>))
  (sal-ast/flat-modules (sal/assertion-name obj)))

(define-method (obj->flat-assertion (obj <sal-qualified-assertion-name>))
  (sal-ast/flat-modules obj))

(define-method (obj->flat-assertion (obj <sal-assertion-expr>))
  (sal-ast/flat-modules obj))

(define-api (make-boolean-assertion assertion . svsv)
  :doc "Create a boolean assertion. @code{assertion} may be a string representing a qualified assertion name, a qualified assertion name object, or an assertion object. This function accepts the optional keyword arguments: @code{:skolemize?} and @code{:ltl?}. This function applies a sequence of transformation to produce an assertion which only uses boolean variables. The following transformations are applied: module flattening, simplification, partial evaluation, quantifier expansion and skolemization (if @code{:skolemize?} is provided and is true), common subexpression elimination, LTL monitor generation (if @code{:ltl?} is provided and is true), convertion to an equivalent boolean formula. "
  :examples '((make-boolean-assertion "peterson!mutex")
              (make-boolean-assertion "peterson!liveness1" :ltl? #t)
              (begin
                (define a (sal/assertion-name "peterson!mutex"))
                (make-boolean-assertion a)))
  (let* ((ltl? (svsv/value svsv :ltl? #f))
         (skolemize? (svsv/value svsv :skolemize? #f))
         (a1 (obj->flat-assertion assertion))
         (a2 (sal/simplify a1))
         (a3 (sal-ast/expand a2))
         (a4 (cond
              (skolemize? 
               (sal-ast/conditionally-expand-quantifiers a3 skolem-expand?))
              (else
               ;; (sal-ast/conditionally-expand-quantifiers a3 (lambda (ast polarity var-decl)
               ;; (instance-of? ast <sal-multi-component-info>))))))
               (sal-ast/expand-quantifiers a3))))
         (a5 (sal-ast/remove-pseudo-lets a4))
         (a6 (sal/cse a5))
         (a7 (if ltl? (sal-module-models/create-ltl-monitor a6) a6))
         ;; (_ [breakpoint "make-boolean-assertion" (a7) #t])
         (a8 (sal/simple-abstraction a7))
         ;; (_ (breakpoint "bool-assertion" (a8) #t))
         (a9 (sal-ast->boolean-ast a8)))
    (sal/slice a9)))

(define-api (make-flat-assertion assertion . svsv)
  (let* ((ltl? (svsv/value svsv :ltl? #f))
         (skolemize? (svsv/value svsv :skolemize? #f))
         (a1 (obj->flat-assertion assertion))
         (a2 (sal/simplify a1))
         (a3 (sal-ast/expand a2))
         (a4 (sal-ast/conditionally-expand-quantifiers a3 (if skolemize? skolem-expand? skolem-expand-finite?)))
         (a5 (sal/cse a4))
         (a6 (if ltl? (sal-module-models/create-ltl-monitor a5) a5))
         (a7 (sal/simple-abstraction a6))
         ;; (a8 (sal/simplify (sal-ast/flat-data a7)))
         (a8 (sal/simplify (sal-ast/substitute-state-var-definitions (sal/simplify (sal-ast/flat-data a7)))))
         (a9 (sal/slice a8)))
    a9))

(define-generic (obj->flat-module obj))

(define-method (obj->flat-module (obj <primitive>))
  (sal-ast/flat-modules (sal/module obj)))

(define-method (obj->flat-module (obj <sal-module>))
  (sal-ast/flat-modules obj))

(define-method (obj->flat-module (obj <sal-flat-module>))
  obj)

(define (make-boolean-flat-module module . svsv)
  ;; (breakpoint "make-boolean-flat-module" (module svsv) #t)
  (let* ((skolemize? (svsv/value svsv :skolemize? #f))
         (m1 (obj->flat-module module))
         (m2 (sal/simplify m1))
         (m3 (sal-ast/expand m2))
         (m4 (if skolemize? (sal-ast/conditionally-expand-quantifiers m3 skolem-expand?) (sal-ast/expand-quantifiers m3)))
         (m5 (sal-ast/remove-pseudo-lets m4))
         (m6 (sal/cse m5))
         (m7 (sal/simple-abstraction m6)))
    (sal-ast->boolean-ast m7)))
  
(define-api (make-flat-module module . svsv)
  (let* ((skolemize? (svsv/value svsv :skolemize? #f))
         (m1 (obj->flat-module module))
         (m2 (sal/simplify m1))
         (m3 (sal-ast/expand m2))
         (m4 (sal-ast/conditionally-expand-quantifiers m3 (if skolemize? skolem-expand? skolem-expand-finite?)))
         (m5 (sal/cse m4))
         (m6 (sal/simple-abstraction m5)))
    (sal/simplify (sal-ast/substitute-state-var-definitions (sal/simplify (sal-ast/flat-data m6))))))

(define (make-boolean-state-expression-core expr bool-flat-module)
  (dlet ((*verbosity-level* 0))
    (sal/simplify (sal-state-expr->boolean-state-expr
                   (sal/cse (sal-ast/expand-quantifiers (sal-ast/expand (sal/simplify expr))))
                   (make-empty-env)
                   bool-flat-module))))

(define (make-simple-data-state-expression-core expr simple-data-flat-module)
  (dlet ((*verbosity-level* 0))
    (let* ((df-info (make-sal-df-info-using simple-data-flat-module))
           (result (sal/simplify (sal-state-expr/flat-data
                                  (sal/cse (sal-ast/conditionally-expand-quantifiers (sal-ast/expand (sal/simplify expr)) skolem-expand-finite?))
                                  (make-empty-env)
                                  simple-data-flat-module
                                  df-info))))
      result)))

(define-api (make-boolean-state-expression (expr-str <primitive>) (bool-flat-module <sal-boolean-flat-module>))
  (let* ((original-module (sal-derived-flat-module/original-flat-module bool-flat-module))
         (expr (sal/state-expr expr-str original-module)))
    (make-boolean-state-expression-core expr bool-flat-module)))

(define-api (make-simple-data-state-expression (expr-str <primitive>) (flat-module <sal-simple-data-flat-module>))
  (let* ((original-module (sal-derived-flat-module/original-flat-module flat-module))
         (expr (sal/state-expr expr-str original-module)))
    (make-simple-data-state-expression-core expr flat-module)))

