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

(module sal-esm-bitstream
        (include "sal.sch")
        (import sal-ast-table unique-names sal-type 
                sal-esm-runtime sal2scm-core sal-expression sal2scm-type
                sal-esm-engine-scm-context sal-esm-support)
        (export (sal-type/val->bitstream type ctx arg)
                (sal-type/val->bitstream-proc type ctx)
                (sal-type/bitstream->val type ctx)
                (sal-type/bitstream->state-var type ctx var)
                (sal-type/bitstream->vector-proc type ctx))
        )


;; Auxiliary function which returns a Scheme global variable which will
;; contain the runtime constraint for a symmetric type.
(define (sal-symmetric-type/constraint-variable type ctx)
  (cond 
   ((sal-ast-table/get (slot-value ctx :symmetry-constraint-table) type) =>
    cdr)
   (else
    (let ((name (gen-unique-name 'symm-constraint)))
      (sal-ast-table/put! (slot-value ctx :symmetry-constraint-table) type name)
      name))))

;; --------------------------------------------------------
;; Values are copied from and to the bitstream.
;; The bitstream encodes the value of the state variables.
;; 
;; --------------------------------------------------------
(define-generic (sal-type/val->bitstream type ctx arg))

(define-method (sal-type/val->bitstream (type <sal-type>) (ctx <sal-esm-engine-scm-context>) (arg <primitive>))
  `(,(sal-type/val->bitstream-proc type ctx) ,arg))

(define-method (sal-type/val->bitstream (type <sal-scalar-type>) (ctx <sal-esm-engine-scm-context>) (arg <primitive>))
  `(sec/add-num! *esm-out-bitstream* ,arg ,(sal-scalar-type/num-bits type)))

(define-method (sal-type/val->bitstream (type <sal-bool-type>) (ctx <sal-esm-engine-scm-context>) (arg <primitive>))
  `(sec/add-bit! *esm-out-bitstream* ,arg))

(define-method (sal-type/val->bitstream (type <sal-type-name>) (ctx <sal-esm-engine-scm-context>) (arg <primitive>))
  (if (sal-type-name/recursive? type)
    `(sec/add-obj! *esm-out-bitstream* ,(sal-type/gen-normalize type ctx arg))
    (sal-type/val->bitstream (sal-type-name/definition type) ctx arg)))

(define-method (sal-type/val->bitstream (type <sal-subtype>) (ctx <sal-esm-engine-scm-context>) (arg <primitive>))
  (sal-type/val->bitstream (sal-subtype/immediate-super-type type) ctx arg))

(define-method (sal-type/val->bitstream (type <sal-bounded-subtype>) (ctx <sal-esm-engine-scm-context>) (arg <primitive>))
  (multiple-value-bind
      (num-bits lower)
      (sal-bounded-subtype/num-bits-and-lower type ctx)
    (cond
     ((eq? num-bits 'infinite)
      `(sec/add-obj! *esm-out-bitstream* ,arg))
     ((slot-value ctx :gmp?)
      ;; bounded gmp number
      (if (= lower 0)
        `(sec/add-num! *esm-out-bitstream* (mpq->integer ,arg) ,num-bits)
        `(sec/add-num! *esm-out-bitstream* (mpq->integer (-mpq ,arg (integer->mpq ,lower))) ,num-bits)))
     (else
      (if (= lower 0)
        `(sec/add-num! *esm-out-bitstream* ,arg ,num-bits)
        `(sec/add-num! *esm-out-bitstream* (-fx ,arg ,lower) ,num-bits))))))

(define-method (sal-type/val->bitstream (type <sal-number-type>) (ctx <sal-esm-engine-scm-context>) (arg <primitive>))
  `(sec/add-obj! *esm-out-bitstream* ,arg))

(define (symmetric-type/val->bitstream type ctx arg normalize-proc)
  (let ((num-bits (sal-bounded-subtype/num-bits-and-lower type ctx))
        (constraint (sal-symmetric-type/constraint-variable type ctx))
        (arg (if (slot-value ctx :gmp?)
               `(mpq->integer ,arg)
               arg)))
    `(sec/add-num! *esm-out-bitstream* (,normalize-proc ,constraint ,arg) ,num-bits)))
  
(define-method (sal-type/val->bitstream (type <sal-scalar-set-type>) (ctx <sal-esm-engine-scm-context>) (arg <primitive>))
  (cond
   ((slot-value ctx :symmetry?)
    (symmetric-type/val->bitstream type ctx arg 'scalar-set-constraint/normalize!))
   (else
    (call-next-method))))

(define-method (sal-type/val->bitstream (type <sal-ring-set-type>) (ctx <sal-esm-engine-scm-context>) (arg <primitive>))
  (cond
   ((slot-value ctx :symmetry?)
    (symmetric-type/val->bitstream type ctx arg 'ring-set-constraint/normalize!))
   (else
    (call-next-method))))

(define (gen-cached-proc-core type ctx proc table-slot id-prefix)
  (let* ((table (slot-value ctx table-slot))
         (proc (cond
                ((sal-ast-table/get table type) =>
                 cdr)
                (else
                 (let ((name (gen-unique-name id-prefix)))
                   (sal-ast-table/put! table type name)
                   (let ((new-scm-decl `(define ,name ,(proc))))
                     (sal-scm-context/add-decl! ctx new-scm-decl)
                     name))))))
    proc))

(define (gen-cached-val->bitstream-proc type ctx proc)
  (gen-cached-proc-core type ctx proc :to-bitstream-proc-table 'val->bitstream))

(define-generic (sal-type/val->bitstream-proc type ctx))

(define-method (sal-type/val->bitstream-proc (type <sal-type-name>) (ctx <sal-esm-engine-scm-context>))
  (if (sal-type-name/recursive? type)
    `(lambda (val)
       (sec/add-obj! *esm-out-bitstream* ,(sal-type/gen-normalize type ctx 'val)))
    (sal-type/val->bitstream-proc (sal-type-name/definition type) ctx)))

(define-method (sal-type/val->bitstream-proc (type <sal-subtype>) (ctx <sal-esm-engine-scm-context>))
  (sal-type/val->bitstream-proc (sal-subtype/immediate-super-type type) ctx))

(define (type-list/val->bitstream-core type-list ctx val first-idx)
  (let ((idx first-idx))
    (map (lambda (child-type)
           (let ((result (sal-type/val->bitstream child-type ctx `(vector-ref ,val ,idx))))
             (set! idx (+ idx 1))
             result))
         type-list)))
  
(define (type-list/val->bitstream type-list ctx)
  `(lambda (val)
     ,@(type-list/val->bitstream-core type-list ctx 'val 0)))

(define-method (sal-type/val->bitstream-proc (type <sal-tuple-type>) (ctx <sal-esm-engine-scm-context>))
  (gen-cached-val->bitstream-proc
   type ctx
   (lambda ()
     (type-list/val->bitstream (slot-value type :types) ctx))))

(define-method (sal-type/val->bitstream-proc (type <sal-record-type>) (ctx <sal-esm-engine-scm-context>))
  (gen-cached-val->bitstream-proc
   type ctx
   (lambda ()
     (type-list/val->bitstream (map (lambda (field) (slot-value field :type)) (sal-record-type/sorted-fields type)) 
                               ctx))))

(define-method (sal-type/val->bitstream-proc (type <sal-scalar-type>) (ctx <sal-esm-engine-scm-context>))
  `(lambda (val)
     ,(sal-type/val->bitstream type ctx 'val)))

(define-method (sal-type/val->bitstream-proc (type <sal-bounded-subtype>) (ctx <sal-esm-engine-scm-context>))
  `(lambda (val)
     ,(sal-type/val->bitstream type ctx 'val)))

(define-method (sal-type/val->bitstream-proc (type <sal-number-type>) (ctx <sal-esm-engine-scm-context>))
  `(lambda (val)
     ,(sal-type/val->bitstream type ctx 'val)))
  
(define-method (sal-type/val->bitstream-proc (type <sal-symmetric-type>) (ctx <sal-esm-engine-scm-context>))
  `(lambda (val)
     ,(sal-type/val->bitstream type ctx 'val)))

(define-method (sal-type/val->bitstream-proc (type <sal-data-type>) (ctx <sal-esm-engine-scm-context>))
  (gen-cached-val->bitstream-proc 
   type ctx 
   (lambda ()
     (let ((constructors (slot-value type :constructors))
           (tag-idx 0))
       `(lambda (val)
          (let ((tag (vector-ref val 0)))
            (sec/add-num! *esm-out-bitstream* tag ,(sal-data-type/tag-num-bits type))
            (cond
             ,@(map (lambda (constructor)
                      (let* ((accessor-list (sal-name-expr/constructor-accessors constructor))
                             (accessor-type-list (map sal-name-expr/accessor-type accessor-list))
                             (result `((=fx tag ,tag-idx)
                                       ,@(type-list/val->bitstream-core accessor-type-list ctx 'val 1))))
                        (set! tag-idx (+ tag-idx 1))
                        result))
                    constructors))))))))

(define-method (sal-type/val->bitstream-proc (type <sal-function-type>) (ctx <sal-esm-engine-scm-context>))
  (gen-cached-val->bitstream-proc
   type ctx
   (lambda ()
     (let ((domain (sal-type/expand-if-type-name (slot-value type :domain)))
           (range (slot-value type :range)))
       (sal-function-type/val->bitstream-proc domain range ctx)))))

(define-generic (sal-function-type/val->bitstream-proc domain range ctx))

(define-method (sal-function-type/val->bitstream-proc (domain <sal-type>) (range <sal-type>) (ctx <sal-esm-engine-scm-context>))
  (let ((n (sal-type/number-of-elements-as-integer domain)))
    `(lambda (val)
       (let loop ((idx 0))
         (when (<fx idx ,n)
           ,(sal-type/val->bitstream range ctx '(vector-ref val idx))
           (loop (+fx idx 1)))))))
  
(define-method (sal-function-type/val->bitstream-proc (domain <sal-type-name>) (range <sal-type>) (ctx <sal-esm-engine-scm-context>))
  (sal-function-type/val->bitstream-proc (sal-type-name/definition domain) range ctx))

(define (gen-next-scalar-set-idx-code range ctx constraint n)
  ;; The generated code assumes:
  ;; 1) the variable i contains the position in a scalar set normalized array.
  ;; 2) the variable inv-permutation and permutation are defined
  ;; 3) the variable val is defined and contains the scalar set to be normalized.
  ;; 4) the argument constraint contains the global variable name which will store the constraint at runtime
  ;; The generated code returns the position in the original scalar set array.
  ;;
  ;; This procedure is used to implement sal-function-type/val->bitstream-proc and sal-function-type/gen-normalize-proc <sal-scalar-set-type>
  ;; methods.
  `(let ((j (vector-ref inv-permutation i)))
     (if (eq? j 'undef)
       (let ((min (let find-min ((i 0)
                                 (min #f))
                    (if (<fx i ,n)
                      (find-min (+fx i 1)
                                (if (and (eq? (vector-ref permutation i) 'undef)
                                         (or (not min)
                                             (<fx ,(sal-type/gen-cmp range ctx 
                                                                     '(vector-ref val i) 
                                                                     '(vector-ref val min)) 
                                                  0)))
                                  i
                                  min))
                      min))))
         (scalar-set-constraint/normalize! ,constraint min)
         min)
       j)))

(define-method (sal-function-type/val->bitstream-proc (domain <sal-scalar-set-type>) (range <sal-type>) (ctx <sal-esm-engine-scm-context>))
  (if (not (slot-value ctx :symmetry?))
    (call-next-method)
    (let ((constraint (sal-symmetric-type/constraint-variable domain ctx))
          (n (sal-type/number-of-elements-as-integer domain)))
      ;; The following code can be optimized.
      ;; - if range does not contain symmetric types, the slow sorting algorithm can be replaced by an efficient one (e.g.,
      ;;   quicksort) without loss of precision.
      ;; - the comparasion function can be more precise. For instance, it can take into account the position being compared.
      ;;   Example: suppose we are comparing the values at positions 'i' and 'j', then if the values contains values of the
      ;;   symetric type domain, then we my assume that the 'i' == 'j' and 'i' and 'j' are less than any other undef element.
      `(lambda (val)
         (let ((inv-permutation (scalar-set-constraint/inv-permutation ,constraint))
               (permutation (scalar-set-constraint/permutation ,constraint)))
           (let loop ((i 0))
             (when (<fx i ,n)
               (let ((j ,(gen-next-scalar-set-idx-code range ctx constraint n)))
                 ,(sal-type/val->bitstream range ctx '(vector-ref val j))
                 (loop (+fx i 1))))))))))

(define (gen-delta-ring-set-code range ctx constraint n)
  ;; The generated code assumes:
  ;; 1) the variable val is defined and contains the ring set array to be normalized
  ;; 2) the argument constraint contains the global variable name which will store the constraint at runtime
  ;; The generated code returns the position in the original ring set array.
  ;;
  ;; This procedure is used to implement sal-function-type/val->bitstream-proc and sal-function-type/gen-normalize-proc <sal-ring-set-type>
  ;; methods.
  `(begin 
     (when (eq? (ring-set-constraint/delta ,constraint) 'undef)
       (let ((min (let find-min ((i 1)
                                 (min 0))
                    (if (<fx i ,n)
                      (case ,(sal-type/gen-cmp range ctx '(vector-ref val i) '(vector-ref val min))
                        ((-1) (find-min (+fx i 1) i))
                        ((0) 
                         ;; compare the succesors of min and i
                         (let compare-succs ((j 1)
                                             (i1 (esm-runtime/ring-succ-idx i ,n))
                                             (i2 (esm-runtime/ring-succ-idx min ,n)))
                           (if (<fx j ,n)
                             (case ,(sal-type/gen-cmp range ctx '(vector-ref val i1) '(vector-ref val i2))
                               ((-1) (find-min (+fx i 1) i))
                               ((0) (compare-succs (+fx j 1) 
                                                   (esm-runtime/ring-succ-idx i1 ,n)
                                                   (esm-runtime/ring-succ-idx i2 ,n)))
                               ((1) (find-min (+fx i 1) min)))
                             (find-min (+fx i 1) min))))
                        ((1) (find-min (+fx i 1) min)))
                      min))))
         (ring-set-constraint/normalize! ,constraint min)))))

(define-method (sal-function-type/val->bitstream-proc (domain <sal-ring-set-type>) (range <sal-type>) (ctx <sal-esm-engine-scm-context>))
  (if (not (slot-value ctx :symmetry?))
    (call-next-method)
    (let ((constraint (sal-symmetric-type/constraint-variable domain ctx))
          (n (sal-type/number-of-elements-as-integer domain)))
      `(lambda (val)
         (let ((delta ,(gen-delta-ring-set-code range ctx constraint n)))
           (let loop ((i 0)
                      (idx delta))
             (when (<fx i ,n)
               ,(sal-type/val->bitstream range ctx '(vector-ref val idx))
               (loop (+fx i 1) (esm-runtime/ring-succ-idx idx ,n)))))))))
               
;; ----------------------------------------------------------------------
;; sal-type/simple?
;; Returns true when the given type is stored/retrived in/from a bitstream
;; with a single command (sec/read-bit!, sec/read-num!, or sec/read-obj!).
;;
;; ----------------------------------------------------------------------
(define-generic (sal-type/simple? type))
(define-method (sal-type/simple? (type <sal-type>)) #f)
(define-method (sal-type/simple? (type <sal-type-name>)) 
  (or (sal-type-name/recursive? type)
      (sal-type/simple? (sal-type-name/definition type))))
(define-method (sal-type/simple? (type <sal-subtype>)) (sal-type/simple? (sal-subtype/immediate-super-type type)))
(define-method (sal-type/simple? (type <sal-scalar-type>)) #t)
(define-method (sal-type/simple? (type <sal-bounded-subtype>)) #t)
(define-method (sal-type/simple? (type <sal-number-type>)) #t)

;; ----------------------------------------------------------------------
;; sal-type/bitstream->val
;; Generate code for converting data in the bitstream into a simple value.
;; See sal-type/simple?
;; ----------------------------------------------------------------------
(define-generic (sal-type/bitstream->val type ctx))

(define-method (sal-type/bitstream->val (type <sal-type-name>) (ctx <sal-esm-engine-scm-context>))
  (if (sal-type-name/recursive? type)
    '(sec/read-obj! *esm-in-bitstream*)
    (sal-type/bitstream->val (sal-type-name/definition type) ctx)))

(define-method (sal-type/bitstream->val (type <sal-subtype>) (ctx <sal-esm-engine-scm-context>))
  (sal-type/bitstream->val (sal-subtype/immediate-super-type type) ctx))

(define-method (sal-type/bitstream->val (type <sal-bool-type>) (ctx <sal-esm-engine-scm-context>))
  '(sec/read-bit! *esm-in-bitstream*))

(define-method (sal-type/bitstream->val (type <sal-scalar-type>) (ctx <sal-esm-engine-scm-context>))
  `(sec/read-num! *esm-in-bitstream* ,(sal-scalar-type/num-bits type)))

(define-method (sal-type/bitstream->val (type <sal-number-type>) (ctx <sal-esm-engine-scm-context>))
  '(sec/read-obj! *esm-in-bitstream*))

(define-method (sal-type/bitstream->val (type <sal-subrange>) (ctx <sal-esm-engine-scm-context>))
  (multiple-value-bind
      (num-bits lower)
      (sal-bounded-subtype/num-bits-and-lower type ctx)
    (cond
     ((eq? num-bits 'infinite)
      '(sec/read-obj! *esm-in-bitstream*))
     ((slot-value ctx :gmp?)
      (if (= lower 0)
        `(integer->mpq (sec/read-num! *esm-in-bitstream* ,num-bits))
        `(+mpq (integer->mpq (sec/read-num! *esm-in-bitstream* ,num-bits)) (integer->mpq ,lower))))
     (else
      (if (= lower 0)
        `(sec/read-num! *esm-in-bitstream* ,num-bits)
        `(+fx (sec/read-num! *esm-in-bitstream* ,num-bits) ,lower))))))

;; ----------------------------------------------------------------------
;; sal-type/bitstream->state-var
;; Generate code for converting data stored in a bitstream in a value used to set
;; a state variable
;; ----------------------------------------------------------------------
(define (sal-type/bitstream->state-var type ctx var)
  (if (sal-type/simple? type)
    `(set! ,var ,(sal-type/bitstream->val type ctx))
    `(,(sal-type/bitstream->vector-proc type ctx) ,var)))

(define (gen-cached-bitstream->vector-proc type ctx proc)
  (gen-cached-proc-core type ctx proc :from-bitstream-proc-table 'bitstream->vector))

;; ----------------------------------------------------------------------
;; sal-type/bitstream->vector-proc
;; Generate code for setting a pre-allocated data structure with data
;; extracted from a bitstream.
;; ----------------------------------------------------------------------
(define-generic (sal-type/bitstream->vector-proc type ctx))

(define-method (sal-type/bitstream->vector-proc (type <sal-type-name>) (ctx <sal-esm-engine-scm-context>))
  (sal-type/bitstream->vector-proc (sal-type-name/definition type) ctx))

(define-method (sal-type/bitstream->vector-proc (type <sal-subtype>) (ctx <sal-esm-engine-scm-context>))
  (sal-type/bitstream->vector-proc (sal-subtype/immediate-super-type type) ctx))

(define (sal-type/set-vector-code type ctx vect idx)
  (if (sal-type/simple? type)
    `(vector-set! ,vect ,idx ,(sal-type/bitstream->val type ctx))
    `(,(sal-type/bitstream->vector-proc type ctx) (vector-ref ,vect ,idx))))

(define (type-list/bitstream->vector type-list ctx)
  `(lambda (vect)
     ,@(let ((idx 0))
         (map (lambda (child-type)
                (let ((result (sal-type/set-vector-code child-type ctx 'vect idx)))
                  (set! idx (+ idx 1))
                  result))
              type-list))))

(define-method (sal-type/bitstream->vector-proc (type <sal-tuple-type>) (ctx <sal-esm-engine-scm-context>))
  (gen-cached-bitstream->vector-proc
   type ctx
   (lambda ()
     (type-list/bitstream->vector (slot-value type :types) ctx))))

(define-method (sal-type/bitstream->vector-proc (type <sal-record-type>) (ctx <sal-esm-engine-scm-context>))
  (gen-cached-bitstream->vector-proc
   type ctx
   (lambda ()
     (type-list/bitstream->vector (map (lambda (field) (slot-value field :type)) (sal-record-type/sorted-fields type)) 
                                  ctx))))

(define-method (sal-type/bitstream->vector-proc (type <sal-data-type>) (ctx <sal-esm-engine-scm-context>))
  (gen-cached-bitstream->vector-proc
   type ctx
   (lambda ()
     (let ((constructors (slot-value type :constructors))
           (pre-alloc-args (sal-data-type/pre-allocated-arguments type))
           (tag-idx 0))
       `(lambda (vect)
          (vector-set! vect 0 (sec/read-num! *esm-in-bitstream* ,(sal-data-type/tag-num-bits type)))
          (case (vector-ref vect 0)
            ,@(map (lambda (constructor)
                     (let* ((accessor-list (sal-name-expr/constructor-accessors constructor))
                            (arg-idx 0)
                            (result `((,tag-idx)
                                      ,@(let gen-accessor-code ((accessor-list accessor-list)
                                                                (pre-alloc-args pre-alloc-args)
                                                                (arg-idx 0))
                                          (if (null? accessor-list)
                                            '()
                                            (let ((accessor-type (sal-name-expr/accessor-type (car accessor-list)))
                                                  (pre-alloc-arg (car pre-alloc-args)))
                                              (cons (cond
                                                     ((sal-type/simple? accessor-type)
                                                      `(vector-set! vect ,arg-idx ,(sal-type/bitstream->val accessor-type ctx)))
                                                     (pre-alloc-arg
                                                      ;; the memory allocated to store this argument can be reused.
                                                      ;; See comment in sal-data-type/pre-allocated-arguments
                                                      `(,(sal-type/bitstream->vector-proc pre-alloc-arg ctx) (vector-ref vect ,arg-idx)))
                                                     (else
                                                      `(begin 
                                                         (vector-set! vect ,arg-idx (quote ,(sal-type/setup-code accessor-type)))
                                                         (,(sal-type/bitstream->vector-proc accessor-type ctx) (vector-ref vect ,arg-idx)))))
                                                    (gen-accessor-code (cdr accessor-list)
                                                                       (cdr pre-alloc-args)
                                                                       (+ arg-idx 1))))))
                                      #unspecified)))
                       (set! tag-idx (+ tag-idx 1))
                       result))
                   constructors)))))))

(define-method (sal-type/bitstream->vector-proc (type <sal-function-type>) (ctx <sal-esm-engine-scm-context>))
  (gen-cached-bitstream->vector-proc
   type ctx
   (lambda ()
     (let ((domain (slot-value type :domain))
           (range (slot-value type :range)))
       `(lambda (vect)
          (let loop ((idx 0))
            (when (<fx idx ,(sal-type/number-of-elements-as-integer domain))
              ,(if (sal-type/simple? range)
                 `(vector-set! vect idx ,(sal-type/bitstream->val range ctx))
                 `(,(sal-type/bitstream->vector-proc range ctx) (vector-ref vect idx)))
              (loop (+fx idx 1)))))))))

;; --------------------------------------------------------
;; Copy/Normalize object code generation 
;; Why do we need this method?
;; Recursive datastructures cannot be directly inserted into
;; the bitstream, since they cannot be stored in a fixed and
;; predefined number of bits. So, these values are stored
;; in a hashtable and are associated with an unique ID.
;; An object must be copied (and normalized if symmetry
;; reduction is enabled) before being inserted into the 
;; hashtable. This deep copy process avoids aliasing problems
;; due to the use of preallocated datastructures. 
;; --------------------------------------------------------

(define-generic (sal-type/gen-normalize type ctx arg))

(define-method (sal-type/gen-normalize (type <sal-type-name>) (ctx <sal-esm-engine-scm-context>) (arg <primitive>))
  (sal-type/gen-normalize (sal-type-name/definition type) ctx arg))

(define-method (sal-type/gen-normalize (type <sal-subtype>) (ctx <sal-esm-engine-scm-context>) (arg <primitive>))
  (sal-type/gen-normalize (sal-subtype/immediate-super-type type) ctx arg))

(define-method (sal-type/gen-normalize (type <sal-scalar-type>) (ctx <sal-esm-engine-scm-context>) (arg <primitive>))
  arg)

(define-method (sal-type/gen-normalize (type <sal-bounded-subtype>) (ctx <sal-esm-engine-scm-context>) (arg <primitive>))
  arg)

(define-method (sal-type/gen-normalize (type <sal-number-type>) (ctx <sal-esm-engine-scm-context>) (arg <primitive>))
  arg)

(define (symmetric-type/gen-normalize type ctx arg normalize-proc)
  (let ((constraint (sal-symmetric-type/constraint-variable type ctx)))
    (if (slot-value ctx :gmp?)
      `(integer->mpq (,normalize-proc ,constraint (mpq->integer ,arg)))
      `(,normalize-proc ,constraint ,arg))))

(define-method (sal-type/gen-normalize (type <sal-scalar-set-type>) (ctx <sal-esm-engine-scm-context>) (arg <primitive>))
  (cond 
   ((slot-value ctx :symmetry?)
    (symmetric-type/gen-normalize type ctx arg 'scalar-set-constraint/normalize!))
   (else
    (call-next-method))))

(define-method (sal-type/gen-normalize (type <sal-ring-set-type>) (ctx <sal-esm-engine-scm-context>) (arg <primitive>))
  (cond 
   ((slot-value ctx :symmetry?)
    (symmetric-type/gen-normalize type ctx arg 'ring-set-constraint/normalize!))
   (else
    (call-next-method))))

(define-method (sal-type/gen-normalize (type <sal-type>) (ctx <sal-esm-engine-scm-context>) (arg <primitive>))
  `(,(sal-type/gen-normalize-proc type ctx) ,arg))

(define (gen-cached-normalize-proc type ctx proc)
  (gen-cached-proc-core type ctx proc :normalized-proc-table 'normalize))

(define-generic (sal-type/gen-normalize-proc type ctx))

(define-method (sal-type/gen-normalize-proc (type <sal-type-name>) (ctx <sal-esm-engine-scm-context>))
  (if (sal-type-name/recursive? type)
    (gen-cached-normalize-proc
     type ctx
     (lambda ()
       `(lambda (arg)
          ,(sal-type/gen-normalize (sal-type-name/definition type) ctx 'arg))))
    (sal-type/gen-normalize-proc (sal-type-name/definition type) ctx)))

(define-method (sal-type/gen-normalize-proc (type <sal-subtype>) (ctx <sal-esm-engine-scm-context>))
  (sal-type/gen-normalize-proc (sal-subtype/immediate-super-type type) ctx))

(define-method (sal-type/gen-normalize-proc (type <sal-scalar-type>) (ctx <sal-esm-engine-scm-context>))
  '(lambda (arg) arg))

(define-method (sal-type/gen-normalize-proc (type <sal-bounded-subtype>) (ctx <sal-esm-engine-scm-context>))
  '(lambda (arg) arg))

(define-method (sal-type/gen-normalize-proc (type <sal-number-type>) (ctx <sal-esm-engine-scm-context>))
  '(lambda (arg) arg))

(define-method (sal-type/gen-normalize-proc (type <sal-symmetric-type>) (ctx <sal-esm-engine-scm-context>))
  '(lambda (arg) ,(sal-type/gen-normalize type ctx 'arg)))

(define (type-list/gen-normalize-core type-list ctx source-vect target-vect first-idx)
  (let ((idx first-idx))
    (map (lambda (child-type)
           (let ((result `(vector-set! ,target-vect ,idx ,(sal-type/gen-normalize child-type ctx `(vector-ref ,source-vect ,idx)))))
             (set! idx (+ idx 1))
             result))
         type-list)))

(define (type-list/gen-normalize type-list ctx)
  (let ((n (length type-list)))
    `(lambda (arg)
       (let ((result (make-vector ,n)))
         ,@(type-list/gen-normalize-core type-list ctx 'arg 'result 0)
         result))))

(define-method (sal-type/gen-normalize-proc (type <sal-tuple-type>) (ctx <sal-esm-engine-scm-context>))
  (gen-cached-normalize-proc
   type ctx
   (lambda ()
     (type-list/gen-normalize (slot-value type :types) ctx))))

(define-method (sal-type/gen-normalize-proc (type <sal-record-type>) (ctx <sal-esm-engine-scm-context>))
  (gen-cached-normalize-proc
   type ctx
   (lambda ()
     (type-list/gen-normalize (map (lambda (field) (slot-value field :type)) (sal-record-type/sorted-fields type)) 
                              ctx))))

(define-method (sal-type/gen-normalize-proc (type <sal-data-type>) (ctx <sal-esm-engine-scm-context>))
  (gen-cached-normalize-proc
   type ctx
   (lambda ()
     (let ((constructors (slot-value type :constructors))
           (tag-idx 0)
           (n (sal-data-type/max-constructor-size type)))
       `(lambda (arg)
          (let ((result (make-vector ,(+ n 1))))
            (vector-set! result 0 (vector-ref arg 0))
            (case (vector-ref arg 0)
              ,@(map (lambda (constructor)
                       (let* ((accessor-list (sal-name-expr/constructor-accessors constructor))
                              (accessor-type-list (map sal-name-expr/accessor-type accessor-list))
                              (result `((,tag-idx)
                                        ,@(type-list/gen-normalize-core accessor-type-list ctx 'arg 'result 1)
                                        #unspecified)))
                         (set! tag-idx (+ tag-idx 1))
                         result))
                     constructors))
            result))))))

(define-method (sal-type/gen-normalize-proc (type <sal-function-type>) (ctx <sal-esm-engine-scm-context>))
  (gen-cached-normalize-proc
   type ctx
   (lambda ()
     (let ((domain (sal-type/expand-if-type-name (slot-value type :domain)))
           (range (slot-value type :range)))
       (sal-function-type/gen-normalize-proc domain range ctx)))))

(define-generic (sal-function-type/gen-normalize-proc domain range ctx))

(define-method (sal-function-type/gen-normalize-proc (domain <sal-type>) (range <sal-type>) (ctx <sal-esm-engine-scm-context>))
  (let ((n (sal-type/number-of-elements-as-integer domain)))
    `(lambda (val)
       (let ((result (make-vector ,n)))
         (let loop ((idx 0))
           (when (<fx idx ,n)
             (vector-set! result idx ,(sal-type/gen-normalize range ctx '(vector-ref val idx)))
             (loop (+fx idx 1))))
         result))))

(define-method (sal-function-type/gen-normalize-proc (domain <sal-scalar-set-type>) (range <sal-type>) (ctx <sal-esm-engine-scm-context>))
  (if (not (slot-value ctx :symmetry?))
    (call-next-method)
    (let ((constraint (sal-symmetric-type/constraint-variable domain ctx))
          (n (sal-type/number-of-elements-as-integer domain)))
      ;; See comments in sal-function-type/val->bitstream-proc
      `(lambda (val)
         (let ((inv-permutation (scalar-set-constraint/inv-permutation ,constraint))
               (permutation (scalar-set-constraint/permutation ,constraint))
               (result (make-vector ,n)))
           (let loop ((i 0))
             (when (<fx i ,n)
               (let ((j ,(gen-next-scalar-set-idx-code range ctx constraint n)))
                 (vector-set! result i ,(sal-type/gen-normalize range ctx '(vector-ref val j)))
                 (loop (+fx i 1)))))
           result)))))
         
(define-method (sal-function-type/gen-normalize-proc (domain <sal-ring-set-type>) (range <sal-type>) (ctx <sal-esm-engine-scm-context>))
  (if (not (slot-value ctx :symmetry?))
    (call-next-method)
    (let ((constraint (sal-symmetric-type/constraint-variable domain ctx))
          (n (sal-type/number-of-elements-as-integer domain)))
      ;; See comments in sal-function-type/val->bitstream-proc
      `(lambda (val)
         (let ((delta ,(gen-delta-ring-set-code range ctx constraint n))
               (result (make-vector ,n)))
           (let loop ((i 0)
                      (idx delta))
             (when (<fx i ,n)
               (vector-set! result i ,(sal-type/gen-normalize range ctx '(vector-ref val idx)))
               (loop (+fx i 1) (esm-runtime/ring-succ-idx idx ,n))))
           result)))))

;; --------------------------------------------------------
;; Generate a comparison code for values.
;; The code returns: -1 (lhs < rhs), 0 (lhs = rhs), 1 (lhs > rhs)
;; The comparison is used to implement symmetry reduction.
;; 
;; --------------------------------------------------------
(define-generic (sal-type/gen-cmp type ctx lhs rhs))

(define-method (sal-type/gen-cmp (type <sal-type>) (ctx <sal-esm-engine-scm-context>) (lhs <primitive>) (rhs <primitive>))
  `(,(sal-type/gen-cmp-proc type ctx) ,lhs ,rhs))

(define-method (sal-type/gen-cmp (type <sal-scalar-type>) (ctx <sal-esm-engine-scm-context>) (lhs <primitive>) (rhs <primitive>))
  `(esm-runtime/int-cmp?  ,lhs ,rhs))

(define-method (sal-type/gen-cmp (type <sal-bool-type>) (ctx <sal-esm-engine-scm-context>) (lhs <primitive>) (rhs <primitive>))
  `(esm-runtime/bool-cmp? ,lhs ,rhs))

(define-method (sal-type/gen-cmp (type <sal-type-name>) (ctx <sal-esm-engine-scm-context>) (lhs <primitive>) (rhs <primitive>))
  (sal-type/gen-cmp (sal-type-name/definition type) ctx lhs rhs))

(define (gen-number-cmp ctx lhs rhs)
  (if (slot-value ctx :gmp?)
    `(esm-runtime/mpq-cmp? ,lhs ,rhs)
    `(esm-runtime/int-cmp? ,lhs ,rhs)))
  
(define-method (sal-type/gen-cmp (type <sal-bounded-subtype>) (ctx <sal-esm-engine-scm-context>) (lhs <primitive>) (rhs <primitive>))
  (gen-number-cmp ctx lhs rhs))

(define-method (sal-type/gen-cmp (type <sal-number-type>) (ctx <sal-esm-engine-scm-context>) (lhs <primitive>) (rhs <primitive>))
  (gen-number-cmp ctx lhs rhs))

(define-method (sal-type/gen-cmp (type <sal-scalar-set-type>) (ctx <sal-esm-engine-scm-context>) (lhs <primitive>) (rhs <primitive>))
  (cond
   ((slot-value ctx :symmetry?)
    `(esm-runtime/scalar-set-cmp? ,(sal-symmetric-type/constraint-variable type ctx) ,lhs ,rhs))
   (else
    (call-next-method))))

(define-method (sal-type/gen-cmp (type <sal-ring-set-type>) (ctx <sal-esm-engine-scm-context>) (lhs <primitive>) (rhs <primitive>))
  (cond
   ((slot-value ctx :symmetry?)
    `(esm-runtime/ring-set-cmp? ,(sal-symmetric-type/constraint-variable type ctx) ,lhs ,rhs))
   (else
    (call-next-method))))

(define (gen-cached-cmp-proc type ctx proc)
  (gen-cached-proc-core type ctx proc :cmp-proc-table 'cmp))

(define-generic (sal-type/gen-cmp-proc type ctx))

(define-method (sal-type/gen-cmp-proc (type <sal-type-name>) (ctx <sal-esm-engine-scm-context>))
  (if (sal-type-name/recursive? type)
    (gen-cached-cmp-proc
     type ctx
     (lambda ()
       `(lambda (val1 val2)
          ,(sal-type/gen-cmp (sal-type-name/definition type) ctx 'val1 'val2))))
    (sal-type/gen-cmp-proc (sal-type-name/definition type) ctx)))

(define-method (sal-type/gen-cmp-proc (type <sal-subtype>) (ctx <sal-esm-engine-scm-context>))
  (sal-type/gen-cmp-proc (sal-subtype/immediate-super-type type) ctx))

(define (type-list/gen-cmp-core type-list ctx val1 val2 first-idx)
  (let loop ((type-list type-list)
             (idx first-idx))
    (if (null? type-list)
      0
      `(case ,(sal-type/gen-cmp (car type-list) ctx `(vector-ref ,val1 ,idx) `(vector-ref ,val2 ,idx))
         ((1) 1)
         ((-1) -1)
         ((0) ,(loop (cdr type-list) (+ idx 1)))))))


(define (type-list/gen-cmp type-list ctx)
  `(lambda (val1 val2)
     ,(type-list/gen-cmp-core type-list ctx 'val1 'val2 0)))

(define-method (sal-type/gen-cmp-proc (type <sal-tuple-type>) (ctx <sal-esm-engine-scm-context>))
  (gen-cached-cmp-proc
   type ctx
   (lambda ()
     (type-list/gen-cmp (slot-value type :types) ctx))))

(define-method (sal-type/gen-cmp-proc (type <sal-record-type>) (ctx <sal-esm-engine-scm-context>))
  (gen-cached-cmp-proc
   type ctx
   (lambda ()
     (type-list/gen-cmp (map (lambda (field) (slot-value field :type)) (sal-record-type/sorted-fields type)) ctx))))

(define-method (sal-type/gen-cmp-proc (type <sal-scalar-type>) (ctx <sal-esm-engine-scm-context>))
  `(lambda (val1 val2)
     ,(sal-type/gen-cmp type ctx 'val1 'val2)))

(define-method (sal-type/gen-cmp-proc (type <sal-bounded-subtype>) (ctx <sal-esm-engine-scm-context>))
  `(lambda (val1 val2)
     ,(sal-type/gen-cmp type ctx 'val1 'val2)))

(define-method (sal-type/gen-cmp-proc (type <sal-number-type>) (ctx <sal-esm-engine-scm-context>))
  `(lambda (val1 val2)
     ,(sal-type/gen-cmp type ctx 'val1 'val2)))

(define-method (sal-type/gen-cmp-proc (type <sal-symmetric-type>) (ctx <sal-esm-engine-scm-context>))
  `(lambda (val1 val2)
     ,(sal-type/gen-cmp type ctx 'val1 'val2)))

(define-method (sal-type/gen-cmp-proc (type <sal-data-type>) (ctx <sal-esm-engine-scm-context>))
  (gen-cached-cmp-proc
   type ctx
   (lambda ()
     (let* ((constructors (slot-value type :constructors))
            (tag-idx 0))
       `(lambda (val1 val2)
          (case (esm-runtime/int-cmp? (vector-ref val1 0) (vector-ref val2 0))
            ((1) 1)
            ((-1) -1)
            ((0)
             (case (vector-ref val1 0)
               ,@(map (lambda (constructor)
                        (let* ((accessor-list (sal-name-expr/constructor-accessors constructor))
                               (accessor-type-list (map sal-name-expr/accessor-type accessor-list))
                               (result `((,tag-idx)
                                         ,@(type-list/gen-cmp-core accessor-type-list ctx 'val1 'val2 1))))
                          (set! tag-idx (+ tag-idx 1))
                          result))
                      constructors)))))))))

(define-method (sal-type/gen-cmp-proc (type <sal-function-type>) (ctx <sal-esm-engine-scm-context>))
  (gen-cached-cmp-proc
   type ctx
   (lambda ()
     (let ((domain (sal-type/expand-if-type-name (slot-value type :domain)))
           (range (slot-value type :range)))
       (sal-function-type/gen-cmp-proc domain range ctx)))))

(define-generic (sal-function-type/gen-cmp-proc type range ctx))

(define-method (sal-function-type/gen-cmp-proc (domain <sal-type>) (range <sal-type>) (ctx <sal-esm-engine-scm-context>))
  (let ((n (sal-type/number-of-elements-as-integer domain)))
    `(lambda (val1 val2)
       (let loop ((idx 0))
         (if (<fx idx ,n)
           (case ,(sal-type/gen-cmp range ctx '(vector-ref val1 idx) '(vector-ref val2 idx))
             ((1) 1)
             ((-1) -1)
             ((0) (loop (+fx idx 1))))
           0)))))

(define-method (sal-function-type/gen-cmp-proc (domain <sal-scalar-set-type>) (range <sal-type>) (ctx <sal-esm-engine-scm-context>))
  (if (not (slot-value ctx :symmetry?))
    (call-next-method)
    (let ((constraint (sal-symmetric-type/constraint-variable domain ctx))
          (n (sal-type/number-of-elements-as-integer domain)))
      `(lambda (val1 val2)
         (let ((inv-permutation (scalar-set-constraint/inv-permutation ,constraint)))
           (let loop ((i 0))
             (if (>= i ,n)
               0 ;; precise result...
               (let ((j (vector-ref inv-permutation i)))
                 (cond
                  ((eq? j 'undef) 
                   ;; USING APPROXIMATION
                   ;; Possible improvement: 
                   ;;   * If all elements of val1 < all elements of val2, then return -1
                   ;;   * If all elements of val1 > all elements of val2, then return 1
                   0)
                  (else
                   ;; precise
                   (case ,(sal-type/gen-cmp range ctx '(vector-ref val1 j) '(vector-ref val2 j))
                     ((1) 1)
                     ((-1) -1)
                     ((0) (loop (+fx i 1))))))))))))))

(define-method (sal-function-type/gen-cmp-proc (domain <sal-ring-set-type>) (range <sal-type>) (ctx <sal-esm-engine-scm-context>))
  (if (not (slot-value ctx :symmetry?))
    (call-next-method)
    (let ((constraint (sal-symmetric-type/constraint-variable domain ctx))
          (n (sal-type/number-of-elements-as-integer domain)))
      `(lambda (val1 val2)
         (let ((delta (ring-set-constraint/delta ,constraint)))
           (if (eq? delta 'undef)
             ;; USING APPROXIMATION
             ;; possible improvement:
             ;; If for all rotations val1 < val2, then return -1 
             ;; if for all rotations val2 < val1, then return 1
             0 
             (let loop ((i 0)
                        (idx delta))
               (if (>=fx i ,n)
                 0
                 (case ,(sal-type/gen-cmp range ctx '(vector-ref val1 idx) '(vector-ref val2 idx))
                   ((1) 1)
                   ((-1) -1)
                   ((0) (loop (+fx i 1)
                              (if (=fx idx (-fx ,n 1)) 0 (+fx idx 1)))))))))))))

             
                        




             
    
