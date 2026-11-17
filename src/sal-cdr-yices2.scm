;; Private adapters for the generated Yices2 FFI, included by sal-cdr.
;; Keep storage and field access for opaque FFI arguments here.
(define-macro (yices-enum name)
  `(pragma::int ,(symbol->string name)))

(define-macro (yices-allocate kind c-type count)
  (let ((size (gensym 'size)))
    `(let ((,(symbol-append size '::long) ,count))
       (,(symbol-append 'pragma:: kind)
        ,(string-append "(" c-type " *) GC_MALLOC(sizeof(" c-type ") * $1)") ,size))))

(define-macro (yices-pointer-present? pointer)
  `(pragma::bool "$1 != NULL" ,pointer))

(define-macro (yices-default-config) '(pragma::yices-ctx-config-pointer "NULL"))
(define-macro (yices-default-parameters) '(pragma::yices-param-pointer "NULL"))

(define-inline (yices-config-present? config::yices-ctx-config-pointer)
  (yices-pointer-present? config))

(define-inline (yices-context-unsat? context::yices-context-pointer)
  (= (yices-context-status context) (yices-enum YICES_STATUS_UNSAT)))

(define-inline (yices-arithmetic-constant? term::int32)
  (= (yices-term-constructor term) (yices-enum YICES_ARITH_CONSTANT)))

(define-inline (make-yices-term-array::yices-term-pointer size::long)
  (yices-allocate yices-term-pointer "term_t" size))

(define-inline (make-yices-int32-array::yices-int32-pointer size::long)
  (yices-allocate yices-int32-pointer "int32_t" size))

(define-inline (make-yices-uint32-cell::yices-uint32-pointer)
  (yices-allocate yices-uint32-pointer "uint32_t" 1))

(define-inline (yices-term-array-ref::int32 array::yices-term-pointer i::long)
  (pragma::int32 "$1[$2]" array i))

(define-inline (yices-term-array-set! array::yices-term-pointer i::long term::int32)
  (pragma::void "$1[$2] = $3" array i term))

(define-inline (yices-int32-array-ref::int32 array::yices-int32-pointer i::long)
  (pragma::int32 "$1[$2]" array i))

(define-inline (yices-uint32-cell-ref::uint32 cell::yices-uint32-pointer)
  (pragma::uint32 "$1[0]" cell))

(define (list->yices-term-array values)
  (let ((array::yices-term-pointer (make-yices-term-array (max 1 (length values)))))
    (let loop ((rest values) (i::long 0))
      (unless (null? rest)
        (yices-term-array-set! array i (car rest))
        (loop (cdr rest) (+ i 1))))
    array))

(define-inline (make-yices-term-vector::yices-term-vector-pointer)
  (let ((vector::yices-term-vector-pointer (yices-allocate yices-term-vector-pointer "term_vector_t" 1)))
    (yices-init-term-vector vector)
    vector))

(define (yices-term-vector-conjunction vector::yices-term-vector-pointer)
  (cdr-ok (yices-and (pragma::uint32 "$1->size" vector)
                    (pragma::yices-term-pointer "$1->data" vector))))

(define-inline (make-yices-interpolation-record::yices-interpolation-context-pointer)
  (yices-allocate yices-interpolation-context-pointer "interpolation_context_t" 1))

(define-inline (yices-set-interpolation-partitions! record::yices-interpolation-context-pointer
                                                   a::yices-context-pointer b::yices-context-pointer)
  (pragma::void "$1->ctx_A = $2; $1->ctx_B = $3" record a b))

(define-inline (yices-interpolant::int32 record::yices-interpolation-context-pointer)
  (pragma::int32 "$1->interpolant" record))

;; A native model can have different Scheme boxes at different call sites.
(define-inline (yices-same-model? a::yices-model-pointer b::yices-model-pointer)
  (pragma::bool "$1 == $2" a b))

(define (yices-take-string pointer::yices-string)
  (let ((result (yices-string-copy pointer)))
    (yices-free-string pointer)
    result))

(define (yices-term-text term #!optional (width 4294967295))
  (yices-take-string (yices-term-to-string term width 4294967295 0)))

(define (cdr-fail operation)
  (sign-error "sal-cdr: ~a: ~a" operation (yices-take-string (yices-error-string))))

(define (cdr-ok value)
  (when (< value 0) (cdr-fail "Yices2 operation failed"))
  value)

;; Keep native int32 boxing out of the hash keys. SAL's eq table hashes the
;; numeric fixnum IDs directly even when a context owns many native terms.
(define-inline (cdr-term-table-ref table term)
  (cond ((eq-hash-table/get table (int32->fixnum term)) => cdr)
        (else #f)))

(define-inline (cdr-term-table-set! table term value)
  (eq-hash-table/put! table (int32->fixnum term) value))

(define (cdr-and values)
  (cond ((null? values) (yices-true))
        ((null? (cdr values)) (car values))
        ((null? (cddr values)) (cdr-ok (yices-and2 (car values) (cadr values))))
        (else (cdr-ok (yices-and (length values) (list->yices-term-array values))))))

(define (cdr-or values)
  (cond ((null? values) (yices-false))
        ((null? (cdr values)) (car values))
        ((null? (cddr values)) (cdr-ok (yices-or2 (car values) (cadr values))))
        (else (cdr-ok (yices-or (length values) (list->yices-term-array values))))))

(define (cdr-not t) (cdr-ok (yices-not t)))

(define-inline (yices-new-configured-context mcsat? logic tags interpolation?)
  (let ((ctx
          (if (or mcsat? logic tags)
            (let ((config (yices-new-config)))
              (unless (yices-config-present? config) (cdr-fail "creating solver configuration"))
              (unwind-protect
                (begin
                  (when logic
                    (when (< (yices-default-config-for-logic config logic) 0)
                      (cdr-fail (string-append "configuring solver logic " logic))))
                  (when tags (cdr-ok (yices-set-config config "trace" tags)))
                  (when mcsat?
                    (cdr-ok (yices-set-config config "solver-type" "mcsat"))
                    (when interpolation?
                      (cdr-ok (yices-set-config config "model-interpolation" "true"))))
                  (yices-new-context config))
                (yices-free-config config)))
            ;; Yices' default configuration already supports push/pop.
            (yices-new-context (yices-default-config)))))
    (unless (yices-pointer-present? ctx) (cdr-fail "creating solver"))
    ctx))
