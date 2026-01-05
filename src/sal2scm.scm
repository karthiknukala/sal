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

(module sal2scm
        (include "sal.sch")
        (include "iterators.sch")
        (import sal2scm-core gmp-scheme sal-ast-table queue sal-ast-env unique-names sal-type 
                sal-expression xformat sal-context sal-parser-utility sal-decls 
                sal-pp sal-ast-simplify sal2scm-type)
        (use sal2scm-runtime)
        (export (sal-type->scm-iterator type ctx env)
                (sal-tuple-index->scm-index idx)
                (sal-record-index->scm-index record idx)
                (sal-type/gen-check-membership-pred type ctx env))
        )

;; ENCODING OF SAL LAMBDA EXPRESSIONS
;; 
;; The following SAL lambda expressions are mapped to Scheme lambda expressions:
;; - Globally defined functions (i.e., a context constant declaration)
;; - Beta-reducible lambdas. That is, the SAL lambda is immediately used in
;;   a function application
;; 
;; All other SAL lambda expressions are mapped to vectors.
;;
;; I decided to use this approach because:
;;
;; - Allows the user to have efficient function state variables.
;;   The other model checkers allow the user to define state
;;   variables such as:
;;     OUTPUT mapping : [ Idx1, Idx2 --> BOOLEAN ]
;;   The symbolic model checkers use a vector encoding to handle
;;   this examples. So, I used the same approach with sal-esmc.
;;   The idea is to be able to use sal-esmc (random simulation)
;;   with any example that can be handled with the other model
;;   checkers.
;;
;; - Usually specifications written in SAL do not use higher
;;   order features. So, we are not losing a lot with this
;;   decision.
;;
;; - The confusion between n-ary functions and unary n-tuple
;;   functions does not exist. 
;;
;; - Several common uses of higher-order functions can be handled
;;   by partial evaluation.
;;

;; Check whether the function of an application or function-update
;; was encoded as a Scheme lambda or vector.
;; Return #f when `fun' is not encoded as a function
;; Return a number when it is encoded as a function, the number
;; indicates the number of expected arguments.
(define-generic (sal-scm/encoded-as-lambda? fun))
(define-method (sal-scm/encoded-as-lambda? (fun <sal-expr>)) #f)
(define-method (sal-scm/encoded-as-lambda? (fun <sal-lambda>)) (length (slot-value fun :local-decls)))
(define-method (sal-scm/encoded-as-lambda? (fun <sal-qualified-name-expr>))
  (let ((definition (sal-name-expr/definition fun)))
    (and definition
         (sal-scm/encoded-as-lambda? definition))))

;;-------------------------------------------------------
;; SAL VALUES Representation
;;
;; booleans        -> scheme booleans {#f, #t}
;; numerals        -> fixnums OR gmp big numbers (depends on the ctx slot :gmp?)
;; strings         -> scheme strings
;; scalars         -> fixnums
;; tuples          -> scheme vectors (size of the vector = size of the tuple)
;; records         -> scheme vectors (remark: fields are sorted)
;; lambdas/arrays  -> scheme vectors (see comment above)
;;                      * the functions idx->val and val->idx are used to map
;;                        indices (values) to vector positions and vice versa.
;; datatype        -> scheme vectors 
;;                      * similar to the representation used for C unions
;;                      * size of the vector = 1 + max-constructor-num-args
;;                      * pos 0 -> tag (fixnum that identifies the datatype constructor)
;;-------------------------------------------------------

(define-method (sal->scm (ast <sal-lambda>) (ctx <sal-scm-context>) (env <primitive>))
  ;; All non global lambdas are implemented as vectors 
  (let* ((local-decls (slot-value ast :local-decls))
         (num-combinations (sal-decl-list/num-elements local-decls))
         (_ (unless (and (<mpq num-combinations *mpq-max-int*)
                         (<= (mpq->integer num-combinations) *sal-scm-max-vector-size*))
              (sign-sal-to-scm-error ast "Lambda expression cannot be converted to a vector, there are ~a elements in the domain of the function, and the maximum vector size is ~a. The function '(sal-scm/set-max-vector-size! <num>)' or the option --max-vector-size=<num> can be used to increase this threshold. "
                                     (mpq->string num-combinations)
                                     *sal-scm-max-vector-size*)))
         (int-num-combinations (mpq->integer num-combinations))
         (local-sizes (map (lambda (var-decl)
                             (sal-type/number-of-elements-as-integer (slot-value var-decl :type)))
                           local-decls))
         (idx (gen-unique-name 'idx))
         (fun (gen-unique-name 'fun))
         (new-vars (map (lambda (decl) (gen-unique-name (sal-decl/name decl))) local-decls))
         (new-env (update-env* env local-decls new-vars)))
    (multiple-value-bind
        (local-idxs local-idxs-assignments)
        (sal-scm/product-idx local-sizes idx)
      (let ((new-var-eq-value-list (map (lambda (new-var local-decl idx)
                                       `(,new-var
                                         ,(sal-type/idx->val (slot-value local-decl :type) ctx env idx)))
                                        new-vars
                                        local-decls
                                        local-idxs))
            (lambda-loop (gen-unique-name 'lambda-loop)))
        `(let ((,fun (make-vector ,int-num-combinations)))
           (let ,lambda-loop ((,idx 0))
                (if (<fx ,idx ,int-num-combinations)
                  (let* (,@local-idxs-assignments
                         ,@new-var-eq-value-list)
                    (vector-set! ,fun ,idx ,(sal->scm (slot-value ast :expr) ctx new-env))
                    (,lambda-loop (+fx ,idx 1)))
                  ,fun)))))))

(define-method (sal->scm :around (ast <sal-application>) (ctx <sal-scm-context>) (env <primitive>))
  (if (slot-value ctx :loop-detection?)
    (let ((fun (slot-value ast :fun)))
      (if (and (instance-of? fun <sal-name-expr>)
               (sal-name-ref/builtin? fun))
        (call-next-method)
        `(sal-scm/loop-detector (lambda () ,(call-next-method)))))
    (call-next-method)))

(define-method (sal->scm (ast <sal-string-expr>) (ctx <sal-scm-context>) (env <primitive>))
  (slot-value ast :string))

(define-method (sal->scm (ast <sal-numeral>) (ctx <sal-scm-context>) (env <primitive>))
  (if (slot-value ctx :gmp?)
    `(make-mpq ,(mpq->string (slot-value ast :num)))
    (mpq->integer (slot-value ast :num))))

(define-method (sal->scm (ast <sal-true>) (ctx <sal-scm-context>) (env <primitive>))
  #t)

(define-method (sal->scm (ast <sal-false>) (ctx <sal-scm-context>) (env <primitive>))
  #f)

(define-method (sal->scm (ast <sal-conditional>) (ctx <sal-scm-context>) (env <primitive>))
  `(if ,(sal->scm (slot-value ast :cond-expr) ctx env)
     ,(sal->scm (slot-value ast :then-expr) ctx env)
     ,(sal->scm (slot-value ast :else-expr) ctx env)))

(define (sal-qualified-name-expr->scm ast ctx env decl-table fun->scm-proc)
  (let ((global-decl-table decl-table))
    (cond
     ((sal-ast-table/get global-decl-table ast) =>
      cdr)
     (else
      (let ((new-name (gen-unique-name (sal-name-ref/name ast)))
            (definition (sal-name-expr/definition ast)))
        (unless definition
          (sign-unsupported-feature ast "Uninterpreted constants are not supported by this tool."))
        (sal-ast-table/put! global-decl-table ast new-name) ;; create definition
        (let* ((body (fun->scm-proc definition ctx (make-empty-env)))
               (new-decl `(define ,new-name ,body)))
          ;;(pp new-decl)
          (sal-scm-context/add-decl! ctx new-decl)
          new-name))))))
  
(define-method (sal->scm (ast <sal-qualified-name-expr>) (ctx <sal-scm-context>) (env <primitive>))
  (sal-qualified-name-expr->scm ast ctx env (slot-value ctx :global-constant-decl-table) sal->scm))

(define-method (sal->scm (ast <sal-name-expr>) (ctx <sal-scm-context>) (env <primitive>))
  (cond
   ((lookup-env (slot-value ast :decl) env) =>
    identity)
   (else
    (sign-sal-to-scm-error ast "Undefined name `~a'" (sal-name-ref/name ast)))))

(define-method (sal->scm (ast <sal-scalar>) (ctx <sal-scm-context>) (env <primitive>))
  (sal-scalar-element/idx ast))

(define-generic (sal-scm/create-lambda ast ctx env))

(define-method (sal-scm/create-lambda (ast <sal-expr>) (ctx <sal-scm-context>) (env <primitive>))
  (sal->scm ast ctx env))

(define-method (sal-scm/create-lambda (ast <sal-qualified-name-expr>) (ctx <sal-scm-context>) (env <primitive>))
  (sal-qualified-name-expr->scm ast ctx env (slot-value ctx :global-function-decl-table) sal-scm/create-lambda))

(define-method (sal-scm/create-lambda (ast <sal-lambda>) (ctx <sal-scm-context>) (env <primitive>))
  (let* ((local-decls (slot-value ast :local-decls))
         (new-local-names (map (lambda (decl)
                                 (gen-unique-name (sal-decl/name decl)))
                               local-decls))
         (new-env (update-env* env local-decls new-local-names))
         (new-body (sal->scm (slot-value ast :expr) ctx new-env)))
    (if (slot-value ctx :runtime-type-check?)
      ;; instrumented version
      (let* ((arg-idx 1)
             (checks `(begin ,@(map (lambda (decl name)
                                      (let* ((type (slot-value decl :type))
                                             (info-idx (sal-scm/register-runtime-info! (cons type ctx)))
                                             (pred (sal-type/gen-check-membership-pred-core type ctx env name))
                                             (result `(unless ,pred
                                                        (sign-sal-scm-runtime-invalid-arg-error 
                                                         (quote ,(sal-context/name (sal-ast/context ast)))
                                                         ,(sal-place/initial-line (sal-ast/place ast))
                                                         ,(sal-place/initial-column (sal-ast/place ast))
                                                         ,arg-idx ,info-idx ,name))))
                                        (set! arg-idx (+ arg-idx 1))
                                        result))
                                    local-decls
                                    new-local-names))))
        `(lambda ,new-local-names ,checks ,new-body))
      ;; non instrumented version
      `(lambda ,new-local-names ,new-body))))

(define (gen-nary-op ast ctx env scm-op)
  (let ((arg-list (sal-application/argument-list ast)))
    (let loop ((arg-list (cdr arg-list))
               (result (sal->scm (car arg-list) ctx env)))
      (if (null? arg-list)
        result
        (loop (cdr arg-list)
              `(,scm-op ,result ,(sal->scm (car arg-list) ctx env)))))))

(define-method (sal->scm (ast <sal-add>) (ctx <sal-scm-context>) (env <primitive>))
  (gen-nary-op ast ctx env (if (slot-value ctx :gmp?) '+mpq '+fx)))

(define-method (sal->scm (ast <sal-mul>) (ctx <sal-scm-context>) (env <primitive>))
  (gen-nary-op ast ctx env (if (slot-value ctx :gmp?) '*mpq '*fx)))

(define-method (sal->scm (ast <sal-and>) (ctx <sal-scm-context>) (env <primitive>))
  (gen-nary-op ast ctx env 'and))

(define-method (sal->scm (ast <sal-or>) (ctx <sal-scm-context>) (env <primitive>))
  (gen-nary-op ast ctx env 'or))

(define-method (sal->scm (ast <sal-not>) (ctx <sal-scm-context>) (env <primitive>))
  `(not ,(sal->scm (slot-value ast :arg) ctx env)))

(define-method (sal->scm (ast <sal-int-pred>) (ctx <sal-scm-context>) (env <primitive>))
  (if (slot-value ctx :gmp?)
    `(mpq/integer? ,(sal->scm (slot-value ast :arg) ctx env))
    #t))

(define-method (sal->scm (ast <sal-real-pred>) (ctx <sal-scm-context>) (env <primitive>))
  #t)

(define-method (sal->scm (app <sal-ring-pre>) (ctx <sal-scm-context>) (env <primitive>))
  (sal->scm (sal-ast/local-simplify app) ctx env))

(define-method (sal->scm (app <sal-ring-succ>) (ctx <sal-scm-context>) (env <primitive>))
  (sal->scm (sal-ast/local-simplify app) ctx env))

(define (gen-binary-op ast ctx env scm-op)
  (multiple-value-bind
      (arg1 arg2)
      (sal-binary-application/arguments ast)
    `(,scm-op ,(sal->scm arg1 ctx env) ,(sal->scm arg2 ctx env))))

(define (gen-instrumented-binary-op ast ctx env scm-op)
  (multiple-value-bind
      (arg1 arg2)
      (sal-binary-application/arguments ast)
    `(,scm-op ,(sal->scm arg1 ctx env) ,(sal->scm arg2 ctx env)
              (quote ,(sal-context/name (sal-ast/context ast)))
              ,(sal-place/initial-line (sal-ast/place ast))
              ,(sal-place/initial-column (sal-ast/place ast)))))


(define-method (sal->scm (ast <sal-sub>) (ctx <sal-scm-context>) (env <primitive>))
  (gen-binary-op ast ctx env (if (slot-value ctx :gmp?) '-mpq '-fx)))

(define-method (sal->scm (ast <sal-div>) (ctx <sal-scm-context>) (env <primitive>))
  (if (slot-value ctx :runtime-type-check?)
    (gen-instrumented-binary-op ast ctx env (if (slot-value ctx :gmp?) 'sal-scm/instrumented-mpq-div 'sal-scm/instrumented-div))
    (gen-binary-op ast ctx env (if (slot-value ctx :gmp?) '/mpq '/fx))))


(define-method (sal->scm (ast <sal-mod>) (ctx <sal-scm-context>) (env <primitive>))
  (if (slot-value ctx :runtime-type-check?)
    (gen-instrumented-binary-op ast ctx env (if (slot-value ctx :gmp?) 'sal-scm/instrumented-mpq-modulo 'sal-scm/instrumented-modulo))
    (gen-binary-op ast ctx env (if (slot-value ctx :gmp?) '%mpq 'modulo))))

(define-method (sal->scm (ast <sal-idiv>) (ctx <sal-scm-context>) (env <primitive>))
  (if (slot-value ctx :runtime-type-check?)
    (gen-instrumented-binary-op ast ctx env (if (slot-value ctx :gmp?) 'sal-scm/instrumented-mpq-idiv 'sal-scm/instrumented-idiv))
    (gen-binary-op ast ctx env (if (slot-value ctx :gmp?) 'div-mpq '/fx))))

(define-method (sal->scm (ast <sal-implies>) (ctx <sal-scm-context>) (env <primitive>))
  (multiple-value-bind
      (arg1 arg2)
      (sal-binary-application/arguments ast)
    `(or (not ,(sal->scm arg1 ctx env)) ,(sal->scm arg2 ctx env))))

(define-method (sal->scm (ast <sal-iff>) (ctx <sal-scm-context>) (env <primitive>))
  (gen-binary-op ast ctx env 'eq?))

(define-method (sal->scm (ast <sal-xor>) (ctx <sal-scm-context>) (env <primitive>))
  (gen-binary-op ast ctx env 'xor))

(define-method (sal->scm (ast <sal-ge>) (ctx <sal-scm-context>) (env <primitive>))
  (gen-binary-op ast ctx env (if (slot-value ctx :gmp?) '>=mpq '>=fx)))

(define-method (sal->scm (ast <sal-le>) (ctx <sal-scm-context>) (env <primitive>))
  (gen-binary-op ast ctx env (if (slot-value ctx :gmp?) '<=mpq '<=fx)))

(define-method (sal->scm (ast <sal-gt>) (ctx <sal-scm-context>) (env <primitive>))
  (gen-binary-op ast ctx env (if (slot-value ctx :gmp?) '>mpq '>fx)))

(define-method (sal->scm (ast <sal-lt>) (ctx <sal-scm-context>) (env <primitive>))
  (gen-binary-op ast ctx env (if (slot-value ctx :gmp?) '<mpq '<fx)))

(define (gen-eq ast ctx env)
  (multiple-value-bind
      (arg1 arg2)
      (sal-binary-application/arguments ast)
    (let ((type (sal-expr/type arg1)))
      (sal-type/gen-eq-pred type ctx (sal->scm arg1 ctx env) (sal->scm arg2 ctx env)))))

(define-method (sal->scm (ast <sal-eq>) (ctx <sal-scm-context>) (env <primitive>))
  (gen-eq ast ctx env))

(define-method (sal->scm (ast <sal-diseq>) (ctx <sal-scm-context>) (env <primitive>))
  `(not ,(gen-eq ast ctx env)))

(define-method (sal->scm (ast <sal-let-expr>) (ctx <sal-scm-context>) (env <primitive>))
  (let* ((local-decls (slot-value ast :local-decls))
         (new-local-decls (map (lambda (decl)
                                 (let* ((new-name (gen-unique-name (sal-decl/name decl)))
                                        (new-value (sal->scm (slot-value decl :value) ctx env)))
                                   `(,new-name ,new-value)))
                               local-decls))
         (new-local-names (map car new-local-decls))
         (new-env (update-env* env local-decls new-local-names))
         (new-body (sal->scm (slot-value ast :expr) ctx new-env)))
    `(let ,new-local-decls ,new-body)))

(define-method (sal->scm (ast <sal-tuple-literal>) (ctx <sal-scm-context>) (env <primitive>))
  `(vector ,@(map (cut sal->scm <> ctx env)
                  (slot-value ast :exprs))))

(define (sal-tuple-index->scm-index idx)
  (-fx (sal-tuple-position->integer idx) 1))

(define-method (sal->scm (ast <sal-tuple-selection>) (ctx <sal-scm-context>) (env <primitive>))
  `(vector-ref ,(sal->scm (slot-value ast :target) ctx env) ,(sal-tuple-index->scm-index (slot-value ast :idx))))

(define-method (sal->scm (ast <sal-tuple-update>) (ctx <sal-scm-context>) (env <primitive>))
  `(sal-scm/update-vector ,(sal->scm (slot-value ast :target) ctx env)
                          ,(- (sal-tuple-position->integer (slot-value ast :idx)) 1)
                          ,(sal->scm (slot-value ast :new-value) ctx env)))

(define-method (sal->scm (ast <sal-record-literal>) (ctx <sal-scm-context>) (env <primitive>))
  `(vector ,@(map (lambda (entry)
                    (sal->scm (slot-value entry :expr) ctx env))
                  (slot-value ast :entries))))

(define (sal-record-index->scm-index record idx)
  [assert (record idx) (instance-of? idx <sal-identifier>)]
  (let* ((field-name (sal-identifier/name idx))
         (record-type (sal-expr/type record))
         (sorted-fields (sal-record-type/sorted-fields record-type)))
    (let loop ((idx 0)
               (fields sorted-fields))
      (if (null? fields)
        (sign-sal-to-scm-error record "Unknown field `~a'." field-name)
        (let ((curr-field (car fields)))
          [assert (curr-field record idx) (instance-of? (slot-value curr-field :id) <sal-identifier>)]
          (if (eq? (sal-identifier/name (slot-value curr-field :id)) field-name)
            idx
            (loop (+ idx 1)
                  (cdr fields))))))))

(define-method (sal->scm (ast <sal-record-selection>) (ctx <sal-scm-context>) (env <primitive>))
  `(vector-ref ,(sal->scm (slot-value ast :target) ctx env)
               ,(sal-record-index->scm-index (slot-value ast :target) (slot-value ast :idx))))

(define-method (sal->scm (ast <sal-record-update>) (ctx <sal-scm-context>) (env <primitive>))
  `(sal-scm/update-vector ,(sal->scm (slot-value ast :target) ctx env)
                          ,(sal-record-index->scm-index (slot-value ast :target) (slot-value ast :idx))
                          ,(sal->scm (slot-value ast :new-value) ctx env)))

(define-method (sal->scm (ast <sal-function-update>) (ctx <sal-scm-context>) (env <primitive>))
  (let* ((target (slot-value ast :target))
         (type (sal-expr/type target))
         (domain (sal-function-type/domain type))
         (scm-target (sal->scm target ctx env))
         (scm-new-value (sal->scm (slot-value ast :new-value) ctx env))
         (idx (slot-value ast :idx)))
    (cond
     ((sal-scm/encoded-as-lambda? target) =>
      (lambda (num-expected-args)
        (let* ((idx-list (sal-argument->argument-list idx num-expected-args))
               (args (generate-list (lambda (_) (gen-unique-name 'arg)) num-expected-args))
               (domain-types (if (= num-expected-args 1)
                               (list domain)
                               (sal-tuple-type/types domain))))
          [assert (idx-list domain-types) (= (length idx-list) (length domain-types))]
          `(lambda ,args
             (if (and ,@(map (lambda (arg idx type)
                               (sal-type/gen-eq-pred type ctx arg (sal->scm idx ctx env)))
                             args
                             idx-list
                             domain-types))
               ,scm-new-value
               (,scm-target ,@args))))))
     (else
      `(sal-scm/update-vector ,scm-target
                              ,(sal-type/val->idx domain ctx env (sal->scm idx ctx env))
                              ,scm-new-value)))))

(define-method (sal->scm (ast <sal-debug-print>) (ctx <sal-scm-context>) (env <primitive>))
  (if (slot-value ctx :debug?)
   `(sal-scm/dbg-print ,@(map (lambda (arg)
                                (if (instance-of? arg <sal-string-expr>)
                                  (slot-value arg :string)
                                  (let* ((type (sal-expr/type arg))
                                         (info-idx (sal-scm/register-runtime-info! (cons type ctx)))
                                         (arg-scm (sal->scm arg ctx env)))
                                    `(cons ,info-idx ,arg-scm))))
                              (sal-application/argument-list ast)))
   (sal->scm (list-last-element (sal-application/argument-list ast)) ctx env)))

(define-method (sal->scm (ast <sal-debug-expr>) (ctx <sal-scm-context>) (env <primitive>))
  (if (slot-value ctx :debug?)
    (let* ((arg (slot-value ast :arg))
           (type (sal-expr/type arg))
           (info-idx (sal-scm/register-runtime-info! (cons* arg type ctx))))
      `(sal-scm/dbg-expr ,info-idx ,(sal->scm arg ctx env)))
    (sal->scm (list-last-element (sal-application/argument-list ast)) ctx env)))

;; Encoding Datatypes
;;
;; Datatype values are encoded as vectors. The size of the 
;; vector is equals to max-constructor-size + 1 (tag).
;; This representation allows us to preallocated datatype values
;; without knowing the tag.

(define-method (sal->scm (ast <sal-constructor-application>) (ctx <sal-scm-context>) (env <primitive>))
  (let* ((constructor (slot-value ast :fun))
         (num-expected-args (sal-constructor/num-expected-args constructor))
         (arg-list (sal-argument->argument-list (slot-value ast :arg) num-expected-args))
         (max-constructor-size (sal-constructor/max-constructor-size constructor))
         (garbage (generate-list (lambda (_) #f) (- max-constructor-size num-expected-args))))
    `(vector ,(sal-constructor/idx constructor)
             ,@(map (cut sal->scm <> ctx env) arg-list)
             ,@garbage)))

(define-method (sal->scm (ast <sal-recognizer-application>) (ctx <sal-scm-context>) (env <primitive>))
  (let ((recognizer (slot-value ast :fun)))
    `(=fx (vector-ref ,(sal->scm (slot-value ast :arg) ctx env) 0)
          ,(sal-recognizer/idx recognizer))))

(define-method (sal->scm (ast <sal-accessor-application>) (ctx <sal-scm-context>) (env <primitive>))
  (let* ((accessor (slot-value ast :fun))
         (accessor-decl (slot-value accessor :decl))
         (constructor-decl (slot-value accessor-decl :constructor-decl))
         (accessor-list (slot-value constructor-decl :accessors))
         (accessor-idx (let loop ((idx 0)
                                  (accessor-list accessor-list))
                         (if (null? accessor-list)
                           (sign-sal-to-scm-error ast "Invalid accessor.")
                           (let ((curr-accessor (car accessor-list)))
                             ;; (breakpoint "sal->scm" (ast curr-accessor) #t)
                             (if (eq? (sal-name-ref/name curr-accessor) (sal-decl/name accessor-decl))
                               idx
                               (loop (+ idx 1) (cdr accessor-list)))))))
         (arg (sal->scm (slot-value ast :arg) ctx env))
         (accessor-code `(vector-ref ,arg ,(+fx accessor-idx 1))))
    (if (slot-value ctx :debug?)
      `(begin
         (sal-scm/check-accessor arg ,(sal-constructor-decl/idx constructor-decl)
                                   (quote ,(sal-context/name (sal-ast/context ast)))
                                   ,(sal-place/initial-line (sal-ast/place ast))
                                   ,(sal-place/initial-column (sal-ast/place ast)))
         ,accessor-code)
      accessor-code)))

(define-method (sal->scm (ast <sal-application>) (ctx <sal-scm-context>) (env <primitive>))
  (let* ((fun (slot-value ast :fun))
         (type (sal-expr/type fun))
         (domain (sal-function-type/domain type))
         (arg (slot-value ast :arg)))
    (cond
     ((sal-scm/encoded-as-lambda? fun) =>
      (lambda (num-expected-args)
        (let ((arg-list (sal-argument->argument-list arg num-expected-args)))
          `(,(sal-scm/create-lambda fun ctx env) ,@(map (cut sal->scm <> ctx env) arg-list)))))
     ((slot-value ctx :runtime-type-check?)
      `(sal-scm/safe-vector-ref ,(sal->scm fun ctx env)
                                ,(sal-type/val->idx domain ctx env (sal->scm arg ctx env))
                                (quote ,(sal-context/name (sal-ast/context ast)))
                                ,(sal-place/initial-line (sal-ast/place ast))
                                ,(sal-place/initial-column (sal-ast/place ast))))
     (else
      `(vector-ref ,(sal->scm fun ctx env)
                   ,(sal-type/val->idx domain ctx env (sal->scm arg ctx env)))))))

(define-method (sal->scm (ast <sal-constructor>) (ctx <sal-scm-context>) (env <primitive>))
  (if (sal-name-expr/constant-constructor? ast)
    (let* ((max-constructor-size (sal-constructor/max-constructor-size ast))
           (garbage (generate-list (lambda (_) #f) max-constructor-size)))
      `(vector ,(sal-constructor/idx ast) ,@garbage))
    (sign-sal-to-scm-error ast "Invalid high-order use of datatype constructor.")))

(define-method (sal->scm (ast <sal-accessor>) (ctx <sal-scm-context>) (env <primitive>))
  (sign-sal-to-scm-error ast "Invalid high-order use of datatype accessor."))

(define-method (sal->scm (ast <sal-recognizer>) (ctx <sal-scm-context>) (env <primitive>))
  (sign-sal-to-scm-error ast "Invalid high-order use of datatype recognizer."))

(define (quantified-expr->scm ast ctx env forall?)
  (let* ((body (slot-value ast :expr))
         (local-decls (slot-value ast :local-decls))
         (local-sizes (map (lambda (var-decl)
                             (sal-type/number-of-elements-as-integer (slot-value var-decl :type)))
                           local-decls))
         (idx (gen-unique-name 'idx)))
    (multiple-value-bind
        (local-idxs local-idxs-assignments)
        (sal-scm/product-idx local-sizes idx)
      (let* ((new-vars (map (lambda (decl) (gen-unique-name (sal-decl/name decl))) local-decls))
             (new-env (update-env* env local-decls new-vars))
             (num-combinations (sal-decl-list/num-elements local-decls))
             (int-num-combinations (if (<mpq num-combinations *mpq-max-int*)
                                     (mpq->integer num-combinations)
                                     (sign-unsupported-feature ast "The types in the quantified expressions contain too many elements.")))
             (new-var-eq-value-list (map (lambda (new-var local-decl idx)
                                           `(,new-var
                                             ,(sal-type/idx->val (slot-value local-decl :type) ctx env idx)))
                                         new-vars
                                         local-decls
                                         local-idxs))
             (and-proc (if forall? 'and 'or))
             (def-result (if forall? #t #f))
             (quant (gen-unique-name 'quant)))
        `(let ,quant ((,idx 0))
              (if (<fx ,idx ,int-num-combinations)
                (let* (,@local-idxs-assignments
                       ,@new-var-eq-value-list)
                  (,and-proc ,(sal->scm (slot-value ast :expr) ctx new-env)
                             (,quant (+fx ,idx 1))))
                ,def-result))))))
                   
(define-method (sal->scm (ast <sal-for-all-expr>) (ctx <sal-scm-context>) (env <primitive>))
  (quantified-expr->scm ast ctx env #t))

(define-method (sal->scm (ast <sal-exists-expr>) (ctx <sal-scm-context>) (env <primitive>))
  (quantified-expr->scm ast ctx env #f))

(define-generic (sal-type->scm-iterator type ctx env))

(define-method (sal-type->scm-iterator (type <sal-type>) (ctx <sal-scm-context>) (env <primitive>))
  ;;(breakpoint "sal-type->scm-iterator" (type) #t)
  (sign-sal-to-scm-error type "Type elements iteration is not supported for this type."))

(define-generic (subrange-iterator type ctx env))

(define-method (subrange-iterator (type <sal-subrange>) (ctx <sal-scm-context>) (env <primitive>))
  (if (slot-value ctx :gmp?)
    `(sal-scm/gmp-subrange-iterator ,(sal->scm (slot-value type :lower) ctx env)
                                    ,(sal->scm (slot-value type :upper) ctx env)) 
    `(sal-scm/subrange-iterator ,(sal->scm (slot-value type :lower) ctx env)
                                ,(sal->scm (slot-value type :upper) ctx env))))

(define-method (sal-type->scm-iterator (type <sal-subrange>) (ctx <sal-scm-context>) (env <primitive>))
  (subrange-iterator type ctx env))

(define (gen-check-type-predicate-code pred arg ctx env)
  (let* ((pred-type (sal-expr/type pred))
         (domain (sal-function-type/domain pred-type))
         (pred-code (sal->scm pred ctx env)))
    `(vector-ref ,pred-code ,arg)))

(define (mk-bounded-subtype-iterator type ctx env)
  (let* ((aux-var (gen-unique-name 'aux))
         (check-pred-code (gen-check-type-predicate-code (slot-value type :expr) aux-var ctx env)))
    `(iterator/filter 
      (lambda (,aux-var)
        ,check-pred-code)
      ,(subrange-iterator type ctx env))))

(define-method (sal-type->scm-iterator (type <sal-bounded-subtype>) (ctx <sal-scm-context>) (env <primitive>))
  (mk-bounded-subtype-iterator type ctx env))

(define-method (sal-type->scm-iterator (type <sal-subtype>) (ctx <sal-scm-context>) (env <primitive>))
  (cond
   ((and (sal-type/integer? type) 
         (sal-type->bounded-subtype type)) 
    =>
    (lambda (bounded-subtype)
      (mk-bounded-subtype-iterator bounded-subtype ctx env)))
   (else
    (let* ((aux-var (gen-unique-name 'aux))
           (check-pred-code (gen-check-type-predicate-code (slot-value type :expr) aux-var ctx env)))
      `(iterator/filter
        (lambda (,aux-var)
          ,check-pred-code)
        ,(sal-type->scm-iterator (sal-subtype/immediate-super-type type) ctx env))))))

(define-method (sal-type->scm-iterator (type <sal-scalar-type>) (ctx <sal-scm-context>) (env <primitive>))
  (let ((num-elems (length (slot-value type :scalar-elements))))
    `(make-interval-iterator 0 ,(- num-elems 1))))

(define-method (sal-type->scm-iterator (type <sal-bool-type>) (ctx <sal-scm-context>) (env <primitive>))
  `(make-list-iterator (list #f #t)))

(define-method (sal-type->scm-iterator (type <sal-tuple-type>) (ctx <sal-scm-context>) (env <primitive>))
  (let ((args (map (lambda (n)
                     (gen-unique-name 'a))
                   (slot-value type :types))))
    `(iterator/product 
      (lambda ,args
        (vector ,@args))
      ,@(map (cut sal-type->scm-iterator <> ctx env) 
             (slot-value type :types)))))

(define-method (sal-type->scm-iterator (type <sal-type-name>) (ctx <sal-scm-context>) (env <primitive>))
  (unless (sal-type/finite? type)
    (sign-sal-to-scm-error type "Type cannot be iterated, the type is infinite or it was not possible to verify its finiteness."))
  (sal-type->scm-iterator (sal-type-name/definition type) ctx env))

(define-method (sal-type->scm-iterator (type <sal-record-type>) (ctx <sal-scm-context>) (env <primitive>))
  (let ((args (map (lambda (n)
                     (gen-unique-name 'a))
                   (slot-value type :fields)))
        (sorted-fields (sal-record-type/sorted-fields type)))
    `(iterator/product
      (lambda ,args
        (vector ,@args))
      ,@(map (lambda (field)
               (sal-type->scm-iterator (slot-value field :type) ctx env))
             sorted-fields))))

(define (make-constructor-iterator constructor-name ctx env)
  (let* ((accessor-list (sal-name-expr/constructor-accessors constructor-name))
         (accessor-type-list (map sal-name-expr/accessor-type accessor-list))
         (args (map (lambda (n)
                     (gen-unique-name 'a))
                    accessor-list)))
    (if (null? accessor-list)
      `(make-singleton-iterator (cons ,(sal-constructor/idx constructor-name) '()))
      `(iterator/product
        (lambda ,args
          (list ,(sal-constructor/idx constructor-name) ,@args))
        ,@(map (cut sal-type->scm-iterator <> ctx env) accessor-type-list)))))

(define-method (sal-type->scm-iterator (type <sal-data-type>) (ctx <sal-scm-context>) (env <primitive>))
  `(iterator/append* (list ,@(map (cut make-constructor-iterator <> ctx env) (slot-value type :constructors)))))

(define-method (sal-type->scm-iterator (type <sal-function-type>) (ctx <sal-scm-context>) (env <primitive>))
  (let ((domain (slot-value type :domain))
        (range (slot-value type :range)))
    ;; (breakpoint "sal-type->scm-iterator" (type domain range) #t)
    (let* ((domain-size (sal-type/number-of-elements-as-integer domain))
           (range-it (sal-type->scm-iterator range ctx env))
           (iterators `(generate-list (lambda (_)
                                        ,range-it)
                                      ,domain-size))
           (values (gen-unique-name 'values)))
      `(apply iterator/product 
              (lambda ,values
                (list->vector ,values))
              ,iterators))))

(define-generic (sal-type/gen-eq-pred type ctx lhs rhs))

(define-method (sal-type/gen-eq-pred (type <sal-scalar-type>) (ctx <sal-scm-context>) (lhs <primitive>) (rhs <primitive>))
  `(= ,lhs ,rhs))

(define-method (sal-type/gen-eq-pred (type <sal-number-type>) (ctx <sal-scm-context>) (lhs <primitive>) (rhs <primitive>))
  (if (slot-value ctx :gmp?)
    `(=mpq ,lhs ,rhs)
    `(= ,lhs ,rhs)))

(define-method (sal-type/gen-eq-pred (type <sal-subrange>) (ctx <sal-scm-context>) (lhs <primitive>) (rhs <primitive>))
  (if (slot-value ctx :gmp?)
    `(=mpq ,lhs ,rhs)
    `(= ,lhs ,rhs)))

(define-method (sal-type/gen-eq-pred (type <sal-bool-type>) (ctx <sal-scm-context>) (lhs <primitive>) (rhs <primitive>))
  `(eq? ,lhs ,rhs))

(define-method (sal-type/gen-eq-pred (type <sal-type-name>) (ctx <sal-scm-context>) (lhs <primitive>) (rhs <primitive>))
  (let ((definition (sal-type-name/definition type)))
    (if definition
      (sal-type/gen-eq-pred definition ctx lhs rhs)
      (sign-sal-to-scm-error type "Failed to generate equality predicated for type."))))

(define-method (sal-type/gen-eq-pred (type <sal-type>) (ctx <sal-scm-context>) (lhs <primitive>) (rhs <primitive>))
  `(sal-scm/eq? ,lhs ,rhs))


(define (sal-type/gen-check-membership-pred type ctx env)
  `(lambda (arg)
     ,(sal-type/gen-check-membership-pred-core type ctx env 'arg)))

(define-generic (sal-type/gen-check-membership-pred-core type ctx env arg))

(define-method (sal-type/gen-check-membership-pred-core (type <sal-type>) (ctx <sal-scm-context>) (env <primitive>) 
                                                        (arg <primitive>))
  #t)

(define (gen-cached-type-mem-pred type ctx env arg proc)
  (let* ((type-check-table (slot-value ctx :type-check-table))
         (pred (cond
                ((and (not (sal-ast/contains-open-references? type))
                      (sal-ast-table/get type-check-table type)) =>
                      cdr)
                (else
                 ;; (print "gen type pred for:")
                 ;; (sal/pp type) (print "")
                 (let* ((name (gen-unique-name 'type-check))
                        (new-arg (gen-unique-name 'arg)))
                   (sal-ast-table/put! type-check-table type name)
                   (let ((new-scm-decl `(define (,name ,new-arg) ,(proc new-arg))))
                     (sal-scm-context/add-decl! ctx new-scm-decl)
                     name))))))
    `(,pred ,arg)))

(define-method (sal-type/gen-check-membership-pred-core (type <sal-subtype>) (ctx <sal-scm-context>)
                                                        (env <primitive>) (arg <primitive>))
  (gen-cached-type-mem-pred 
   type ctx env arg
   (lambda (arg)
     (let ((super-type (sal-subtype/immediate-super-type type))
           (check-pred-code (gen-check-type-predicate-code (slot-value type :expr) arg ctx env)))
       `(and ,check-pred-code
             ,(sal-type/gen-check-membership-pred-core super-type ctx env arg))))))

(define-method (sal-type/gen-check-membership-pred-core (type <sal-real-type>) (ctx <sal-scm-context>) (env <primitive>) 
                                                        (arg <primitive>))
  #t)

(define-method (sal-type/gen-check-membership-pred-core (type <sal-number-type>) (ctx <sal-scm-context>) (env <primitive>) 
                                                        (arg <primitive>))
  #t)

(define-method (sal-type/gen-check-membership-pred-core (type <sal-tuple-type>) (ctx <sal-scm-context>)
                                                        (env <primitive>) (arg <primitive>))
  (gen-cached-type-mem-pred
   type ctx env arg
   (lambda (arg)
     (let ((idx 0))
       `(and ,@(map (lambda (child-type)
                      (let ((result (sal-type/gen-check-membership-pred-core child-type ctx env 
                                                                             `(vector-ref ,arg ,idx))))
                        (set! idx (+ idx 1))
                        result))
                    (slot-value type :types)))))))

(define-method (sal-type/gen-check-membership-pred-core (type <sal-record-type>) (ctx <sal-scm-context>)
                                                        (env <primitive>) (arg <primitive>))
  (gen-cached-type-mem-pred
   type ctx env arg
   (lambda (arg)
     (let ((idx 0))
       `(and ,@(map (lambda (field)
                      (let ((result (sal-type/gen-check-membership-pred-core (slot-value field :type) ctx env 
                                                                             `(vector-ref ,arg ,idx))))
                        (set! idx (+ idx 1))
                        result))
                    (sal-record-type/sorted-fields type)))))))
   
(define-method (sal-type/gen-check-membership-pred-core (type <sal-type-name>) (ctx <sal-scm-context>)
                                                        (env <primitive>) (arg <primitive>))
  (let ((definition (sal-type-name/definition type)))
    (if definition
      (sal-type/gen-check-membership-pred-core definition ctx env arg)
      #t)))

(define-method (sal-type/gen-check-membership-pred-core (type <sal-function-type>) (ctx <sal-scm-context>)
                                                        (env <primitive>) (arg <primitive>))
  (gen-cached-type-mem-pred
   type ctx env arg
   (lambda (arg)
     (let ((domain (slot-value type :domain))
           (range (slot-value type :range)))
       (let ((range-pred `(lambda (arg)
                            ,(sal-type/gen-check-membership-pred-core range ctx env 'arg))))
         `(sal-scm/vector-for-all ,range-pred ,arg))))))
    
(define-method (sal-type/gen-check-membership-pred-core (type <sal-data-type>) (ctx <sal-scm-context>)
                                                        (env <primitive>) (arg <primitive>))
  (gen-cached-type-mem-pred
   type ctx env arg
   (lambda (arg)
     (let ((tag-idx 0))
       `(or ,@(map (lambda (constructor)
                     (let* ((accessor-list (sal-name-expr/constructor-accessors constructor))
                            (accessor-type-list (map sal-name-expr/accessor-type accessor-list))
                            (check-tag `(= (car ,arg) ,tag-idx))
                            (result   (if (null? accessor-list)
                                        check-tag
                                        `(and ,check-tag
                                              (let ((args (cdr ,arg)))
                                                ,@(map (lambda (acc-type)
                                                         `(begin
                                                            ,(sal-type/gen-check-membership-pred-core 
                                                              acc-type ctx env '(car args))
                                                            (set! args (cdr args))))
                                                       accessor-type-list))))))
                       (set! tag-idx (+ tag-idx 1))
                       result))
                   (slot-value type :constructors)))))))


    






  
  
