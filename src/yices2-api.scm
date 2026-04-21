;;
;; SAL 3.3, Copyright (C) 2026, SRI International.  All Rights Reserved.
;;
;; SAL is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;

(module yices2-api
        (include "sal.sch")
        (import utility)
        (extern
         (type yices-config (opaque) "void *")
         (type yices-context (opaque) "void *")
         (type yices-model (opaque) "void *")
         (type yices-term-array (opaque) "void *")
         (type yices-term-vector (opaque) "void *")
         (yices-config-null?::bool (::yices-config) "bgl_yices_pointer_null")
         (yices-context-null?::bool (::yices-context) "bgl_yices_pointer_null")
         (yices-model-null?::bool (::yices-model) "bgl_yices_pointer_null")
         (yices-term-array-null?::bool (::yices-term-array) "bgl_yices_pointer_null")
         (yices-term-vector-null?::bool (::yices-term-vector) "bgl_yices_pointer_null")
         (yices-init::void () "bgl_yices_init")
         (yices-exit::void () "bgl_yices_exit")
         (yices-has-mcsat::int () "bgl_yices_has_mcsat")
         (yices-version::obj () "bgl_yices_version_string")
         (yices-new-config::yices-config () "bgl_yices_new_config")
         (yices-free-config::void (::yices-config) "bgl_yices_free_config")
         (yices-set-config::int (::yices-config ::string ::string) "bgl_yices_set_config")
         (yices-new-context::yices-context (::yices-config) "bgl_yices_new_context")
         (yices-free-context::void (::yices-context) "bgl_yices_free_context")
         (yices-reset-context::void (::yices-context) "bgl_yices_reset_context")
         (yices-parse-type::int (::string) "bgl_yices_parse_type")
         (yices-parse-term-raw::int (::string) "bgl_yices_parse_term")
         (yices-parse-rational-raw::int (::string) "bgl_yices_parse_rational")
         (yices-true-raw::int () "bgl_yices_true")
         (yices-false-raw::int () "bgl_yices_false")
         (yices-new-uninterpreted-type::int () "bgl_yices_new_uninterpreted_type")
         (yices-set-type-name::int (::int ::string) "bgl_yices_set_type_name")
         (yices-new-uninterpreted-term::int (::int) "bgl_yices_new_uninterpreted_term")
         (yices-set-term-name::int (::int ::string) "bgl_yices_set_term_name")
         (yices-not-raw::int (::int) "bgl_yices_not")
         (yices-neg-raw::int (::int) "bgl_yices_neg")
         (yices-eq-raw::int (::int ::int) "bgl_yices_eq")
         (yices-neq-raw::int (::int ::int) "bgl_yices_neq")
         (yices-iff-raw::int (::int ::int) "bgl_yices_iff")
         (yices-implies-raw::int (::int ::int) "bgl_yices_implies")
         (yices-ite-raw::int (::int ::int ::int) "bgl_yices_ite")
         (yices-or-raw::int (::int ::yices-term-array) "bgl_yices_or_terms")
         (yices-and-raw::int (::int ::yices-term-array) "bgl_yices_and_terms")
         (yices-or2-raw::int (::int ::int) "bgl_yices_or2")
         (yices-and2-raw::int (::int ::int) "bgl_yices_and2")
         (yices-or3-raw::int (::int ::int ::int) "bgl_yices_or3")
         (yices-and3-raw::int (::int ::int ::int) "bgl_yices_and3")
         (yices-application-raw::int (::int ::int ::yices-term-array) "bgl_yices_application")
         (yices-update-raw::int (::int ::int ::yices-term-array ::int) "bgl_yices_update")
         (yices-add-raw::int (::int ::int) "bgl_yices_add")
         (yices-sub-raw::int (::int ::int) "bgl_yices_sub")
         (yices-mul-raw::int (::int ::int) "bgl_yices_mul")
         (yices-sum-raw::int (::int ::yices-term-array) "bgl_yices_sum_terms")
         (yices-product-raw::int (::int ::yices-term-array) "bgl_yices_product_terms")
         (yices-division-raw::int (::int ::int) "bgl_yices_division")
         (yices-idiv-raw::int (::int ::int) "bgl_yices_idiv")
         (yices-arith-lt-raw::int (::int ::int) "bgl_yices_arith_lt_atom")
         (yices-arith-le-raw::int (::int ::int) "bgl_yices_arith_leq_atom")
         (yices-arith-gt-raw::int (::int ::int) "bgl_yices_arith_gt_atom")
         (yices-arith-ge-raw::int (::int ::int) "bgl_yices_arith_geq_atom")
         (yices-arith-lt0-raw::int (::int) "bgl_yices_arith_lt0_atom")
         (yices-arith-le0-raw::int (::int) "bgl_yices_arith_leq0_atom")
         (yices-arith-gt0-raw::int (::int) "bgl_yices_arith_gt0_atom")
         (yices-arith-ge0-raw::int (::int) "bgl_yices_arith_geq0_atom")
         (yices-assert-formula-raw::int (::yices-context ::int) "bgl_yices_assert_formula")
         (yices-push-raw::int (::yices-context) "bgl_yices_push")
         (yices-pop-raw::int (::yices-context) "bgl_yices_pop")
         (yices-check-context-raw::int (::yices-context) "bgl_yices_check_context")
         (yices-check-context-with-model-raw::int (::yices-context ::yices-model ::int ::yices-term-array)
                                                  "bgl_yices_check_context_with_model")
         (yices-get-model::yices-model (::yices-context ::int) "bgl_yices_get_model")
         (yices-free-model::void (::yices-model) "bgl_yices_free_model")
         (yices-model-from-map-raw::yices-model (::int ::yices-term-array ::yices-term-array)
                                                "bgl_yices_model_from_map")
         (yices-get-model-interpolant-raw::int (::yices-context) "bgl_yices_get_model_interpolant")
         (yices-get-value-as-term-raw::int (::yices-model ::int) "bgl_yices_get_value_as_term")
         (yices-get-value-formula-string-raw::obj (::yices-model ::int) "bgl_yices_get_value_formula_string")
         (yices-get-value-string-raw::obj (::yices-model ::int) "bgl_yices_get_value_string")
         (yices-generalize-model-array::int (::yices-model ::int ::yices-term-array ::int ::yices-term-array ::int ::yices-term-vector)
                                            "bgl_yices_generalize_model_array")
         (yices-error-string::obj () "bgl_yices_error_string")
         (yices-term->string-raw::obj (::int) "bgl_yices_term_to_string")
         (yices-term-constructor-raw::int (::int) "bgl_yices_term_constructor")
         (yices-term-num-children-raw::int (::int) "bgl_yices_term_num_children")
         (yices-term-child-raw::int (::int ::int) "bgl_yices_term_child")
         (yices-term-name-raw::obj (::int) "bgl_yices_get_term_name")
         (yices-term-is-bool-raw::int (::int) "bgl_yices_term_is_bool")
         (yices-sum-component-term-raw::int (::int ::int) "bgl_yices_sum_component_term")
         (yices-product-component-term-raw::int (::int ::int) "bgl_yices_product_component_term")
         (yices-term-array-alloc::yices-term-array (::int) "bgl_yices_term_array_alloc")
         (yices-term-array-set!::void (::yices-term-array ::int ::int) "bgl_yices_term_array_set")
         (yices-term-array-ref::int (::yices-term-array ::int) "bgl_yices_term_array_ref")
         (yices-term-array-free::void (::yices-term-array) "bgl_yices_term_array_free")
         (yices-term-array-value-raw::int (::yices-model ::int ::yices-term-array ::yices-term-array)
                                          "bgl_yices_term_array_value")
         (yices-term-vector-new::yices-term-vector () "bgl_yices_term_vector_new")
         (yices-term-vector-delete::void (::yices-term-vector) "bgl_yices_term_vector_delete")
         (yices-term-vector-reset!::void (::yices-term-vector) "bgl_yices_term_vector_reset")
         (yices-term-vector-size::int (::yices-term-vector) "bgl_yices_term_vector_size")
         (yices-term-vector-ref::int (::yices-term-vector ::int) "bgl_yices_term_vector_ref"))
        (export (yices2-api/acquire!)
                (yices2-api/release!)
                (yices2-api/has-mcsat?)
                (yices2-api/version-string)
                (yices2-api/status->string status)
                (yices2-api/declare-sort! sort-name)
                (yices2-api/declare-uninterpreted-term! name type-string)
                (yices2-api/parse-term term-string)
                (yices2-api/rational-term rational-string)
                (yices2-api/true-term)
                (yices2-api/false-term)
                (yices2-api/not-term term)
                (yices2-api/neg-term term)
                (yices2-api/eq-term left right)
                (yices2-api/neq-term left right)
                (yices2-api/iff-term left right)
                (yices2-api/implies-term left right)
                (yices2-api/ite-term cond then-term else-term)
                (yices2-api/or-terms terms)
                (yices2-api/and-terms terms)
                (yices2-api/application-term fun args)
                (yices2-api/update-term fun args new-value)
                (yices2-api/add-terms terms)
                (yices2-api/sub-term left right)
                (yices2-api/mul-terms terms)
                (yices2-api/division-term left right)
                (yices2-api/idiv-term left right)
                (yices2-api/lt-term left right)
                (yices2-api/le-term left right)
                (yices2-api/gt-term left right)
                (yices2-api/ge-term left right)
                (yices2-api/lt0-term term)
                (yices2-api/le0-term term)
                (yices2-api/gt0-term term)
                (yices2-api/ge0-term term)
                (yices2-api/term->string term)
                (yices2-api/term-constructor term)
                (yices2-api/term-num-children term)
                (yices2-api/term-child term idx)
                (yices2-api/term-name term)
                (yices2-api/term-bool? term)
                (yices2-api/sum-component-term term idx)
                (yices2-api/product-component-term term idx)
                (yices2-api/new-context interpolants?)
                (yices2-api/free-context! ctx)
                (yices2-api/context-reset! ctx)
                (yices2-api/context-push! ctx)
                (yices2-api/context-pop! ctx)
                (yices2-api/assert-formula! ctx term)
                (yices2-api/check-context ctx)
                (yices2-api/check-context-with-model ctx model terms)
                (yices2-api/get-model ctx)
                (yices2-api/free-model! model)
                (yices2-api/model-from-map vars values)
                (yices2-api/get-model-interpolant ctx)
                (yices2-api/try-value-as-term model term)
                (yices2-api/try-value-formula-string model term)
                (yices2-api/try-value-string model term)
                (yices2-api/term-array-values model terms)
                (yices2-api/generalize-model-terms model formula-terms elim-terms)
                (yices2-api/generalize-terms formula-terms elim-terms)
                (yices2-api/generalize-formulas logic sort-names decl-specs keep-names formulas)))

;; yices_types.h:
;;   YICES_STATUS_SAT = 3
;;   YICES_GEN_DEFAULT = 0
;;   YICES_GEN_BY_SUBST = 1
(define *yices-status-sat* 3)
(define *yices-gen-default* 0)
(define *yices-gen-by-subst* 1)

(define *yices2-api-refcount* 0)

(define (yices2-api/error-string)
  (let ((msg (yices-error-string)))
    (if (and (string? msg)
             (> (string-length msg) 0))
      msg
      "unknown Yices error")))

(define (yices2-api/check-code code where)
  (when (< code 0)
    (sign-error "~a failed: ~a" where (yices2-api/error-string))))

(define (yices2-api/check-pointer ptr null? where)
  (when (or (eq? ptr #f)
            (null? ptr))
    (sign-error "~a failed: ~a" where (yices2-api/error-string))))

(define (yices2-api/pointer-live? ptr null?)
  (and (not (eq? ptr #f))
       (not (null? ptr))))

(define (yices2-api/check-string-list values label)
  (let loop ((remaining values)
             (idx 0))
    (unless (null? remaining)
      (unless (string? (car remaining))
        (sign-error "sal-cdr expected ~a[~a] to be a string, received ~a."
                    label
                    idx
                    (car remaining)))
      (loop (cdr remaining) (+ idx 1)))))

(define (yices2-api/check-term-list values label)
  (let loop ((remaining values)
             (idx 0))
    (unless (null? remaining)
      (let ((term (car remaining)))
        (unless (integer? term)
          (sign-error "sal-cdr expected ~a[~a] to be a Yices term id, received ~a."
                      label
                      idx
                      term)))
      (loop (cdr remaining) (+ idx 1)))))

(define (yices2-api/check-decl-specs decl-specs)
  (let loop ((remaining decl-specs)
             (idx 0))
    (unless (null? remaining)
      (let ((spec (car remaining)))
        (unless (and (pair? spec)
                     (string? (car spec))
                     (string? (cdr spec)))
          (sign-error "sal-cdr expected declaration spec ~a to be a pair of strings, received ~a."
                      idx
                      spec)))
      (loop (cdr remaining) (+ idx 1)))))

(define (yices2-api/status->string status)
  (case status
    ((0) "idle")
    ((1) "searching")
    ((2) "unknown")
    ((3) "sat")
    ((4) "unsat")
    ((5) "interrupted")
    ((6) "error")
    (else
     (object->string status))))

(define (yices2-api/acquire!)
  (when (= *yices2-api-refcount* 0)
    (yices-init))
  (set! *yices2-api-refcount* (+ *yices2-api-refcount* 1))
  *yices2-api-refcount*)

(define (yices2-api/release!)
  (when (<= *yices2-api-refcount* 0)
    (sign-error "sal-cdr internal error: Yices2 API release underflow."))
  (set! *yices2-api-refcount* (- *yices2-api-refcount* 1))
  (when (= *yices2-api-refcount* 0)
    (yices-exit))
  *yices2-api-refcount*)

(define (with-yices2-api thunk)
  (yices2-api/acquire!)
  (unwind-protect
   (thunk)
   (yices2-api/release!)))

(define (yices2-api/version-string)
  (with-yices2-api
   (lambda ()
     (let ((version (yices-version)))
       (if (string? version)
         version
         "unknown")))))

(define (yices2-api/has-mcsat?)
  (with-yices2-api
   (lambda ()
     (= (yices-has-mcsat) 1))))

(define (yices2-api/list->term-array terms label)
  (let* ((len (length terms))
         (array (yices-term-array-alloc len)))
    (when (and (> len 0) (yices-term-array-null? array))
      (sign-error "sal-cdr failed to allocate a Yices term array for ~a." label))
    (let loop ((remaining terms)
               (idx 0))
      (unless (null? remaining)
        (let ((term (car remaining)))
          (unless (integer? term)
            (sign-error "sal-cdr expected ~a[~a] to be a Yices term id, received ~a."
                        label
                        idx
                        term))
          (yices-term-array-set! array idx term)
          (loop (cdr remaining) (+ idx 1)))))
    array))

(define (yices2-api/term-array->list array len label)
  (let loop ((idx 0)
             (result '()))
    (if (= idx len)
      (reverse! result)
      (let ((term (yices-term-array-ref array idx)))
        (when (< term 0)
          (sign-error "sal-cdr failed to read ~a[~a] from a Yices term array." label idx))
        (loop (+ idx 1) (cons term result))))))

(define (yices2-api/collect-generalized-term-ids generalized)
  (let ((size (yices-term-vector-size generalized)))
    (let loop ((idx 0)
               (result '()))
      (if (= idx size)
        (reverse! result)
        (loop (+ idx 1)
              (cons (yices-term-vector-ref generalized idx)
                    result))))))

(define (yices2-api/collect-generalized-terms generalized)
  (map yices2-api/term->string
       (yices2-api/collect-generalized-term-ids generalized)))

(define (yices2-api/declare-sort! sort-name)
  (unless (string? sort-name)
    (sign-error "sal-cdr expected the sort name to be a string, received ~a." sort-name))
  (let ((tau (yices-new-uninterpreted-type)))
    (yices2-api/check-code tau
                           (string-append "yices_new_uninterpreted_type(" sort-name ")"))
    (yices2-api/check-code (yices-set-type-name tau sort-name)
                           (string-append "yices_set_type_name(" sort-name ")"))
    tau))

(define (yices2-api/declare-uninterpreted-term! name type-string)
  (unless (string? name)
    (sign-error "sal-cdr expected the declaration name to be a string, received ~a." name))
  (unless (string? type-string)
    (sign-error "sal-cdr expected the declaration type to be a string, received ~a." type-string))
  (let ((tau (yices-parse-type type-string)))
    (yices2-api/check-code tau
                           (string-append "yices_parse_type(" name ")"))
    (let ((term (yices-new-uninterpreted-term tau)))
      (yices2-api/check-code term
                             (string-append "yices_new_uninterpreted_term(" name ")"))
      (yices2-api/check-code (yices-set-term-name term name)
                             (string-append "yices_set_term_name(" name ")"))
      term)))

(define (yices2-api/parse-term term-string)
  (unless (string? term-string)
    (sign-error "sal-cdr expected a Yices term string, received ~a." term-string))
  (let ((term (yices-parse-term-raw term-string)))
    (when (< term 0)
      (sign-error "yices_parse_term failed: ~a\nFormula: ~a"
                  (yices2-api/error-string)
                  term-string))
    term))

(define (yices2-api/rational-term rational-string)
  (unless (string? rational-string)
    (sign-error "sal-cdr expected a rational string, received ~a." rational-string))
  (let ((term (yices-parse-rational-raw rational-string)))
    (yices2-api/check-code term "yices_parse_rational")
    term))

(define (yices2-api/true-term)
  (let ((term (yices-true-raw)))
    (yices2-api/check-code term "yices_true")
    term))

(define (yices2-api/false-term)
  (let ((term (yices-false-raw)))
    (yices2-api/check-code term "yices_false")
    term))

(define (yices2-api/not-term term)
  (unless (integer? term)
    (sign-error "sal-cdr expected a Yices term id, received ~a." term))
  (let ((negated (yices-not-raw term)))
    (yices2-api/check-code negated "yices_not")
    negated))

(define (yices2-api/neg-term term)
  (unless (integer? term)
    (sign-error "sal-cdr expected a Yices term id, received ~a." term))
  (let ((negated (yices-neg-raw term)))
    (yices2-api/check-code negated "yices_neg")
    negated))

(define (yices2-api/check-binary-terms left right label)
  (unless (integer? left)
    (sign-error "sal-cdr expected the left operand for ~a to be a Yices term id, received ~a."
                label
                left))
  (unless (integer? right)
    (sign-error "sal-cdr expected the right operand for ~a to be a Yices term id, received ~a."
                label
                right)))

(define (yices2-api/eq-term left right)
  (yices2-api/check-binary-terms left right "eq")
  (let ((term (yices-eq-raw left right)))
    (yices2-api/check-code term "yices_eq")
    term))

(define (yices2-api/neq-term left right)
  (yices2-api/check-binary-terms left right "neq")
  (let ((term (yices-neq-raw left right)))
    (yices2-api/check-code term "yices_neq")
    term))

(define (yices2-api/iff-term left right)
  (yices2-api/check-binary-terms left right "iff")
  (let ((term (yices-iff-raw left right)))
    (yices2-api/check-code term "yices_iff")
    term))

(define (yices2-api/implies-term left right)
  (yices2-api/check-binary-terms left right "implies")
  (let ((term (yices-implies-raw left right)))
    (yices2-api/check-code term "yices_implies")
    term))

(define (yices2-api/ite-term cond then-term else-term)
  (unless (integer? cond)
    (sign-error "sal-cdr expected an ite condition term id, received ~a." cond))
  (unless (integer? then-term)
    (sign-error "sal-cdr expected an ite then-branch term id, received ~a." then-term))
  (unless (integer? else-term)
    (sign-error "sal-cdr expected an ite else-branch term id, received ~a." else-term))
  (let ((term (yices-ite-raw cond then-term else-term)))
    (yices2-api/check-code term "yices_ite")
    term))

(define (split-list-halves terms)
  (let* ((len (length terms))
         (left-size (/fx len 2)))
    (let loop ((remaining terms)
               (count left-size)
               (left '()))
      (if (= count 0)
        (values (reverse! left) remaining)
        (loop (cdr remaining)
              (-fx count 1)
              (cons (car remaining) left))))))

(define (build-nary-boolean-term terms zero-term two-raw three-raw label)
  (yices2-api/check-term-list terms label)
  (cond
   ((null? terms)
    zero-term)
   ((null? (cdr terms))
    (car terms))
   ((null? (cddr terms))
    (let ((result (two-raw (car terms) (cadr terms))))
      (yices2-api/check-code result label)
      result))
   ((null? (cdddr terms))
    (let ((result (three-raw (car terms) (cadr terms) (caddr terms))))
      (yices2-api/check-code result label)
      result))
   (else
    (multiple-value-bind
        (left right)
        (split-list-halves terms)
      (let ((result (two-raw (build-nary-boolean-term left
                                                      zero-term
                                                      two-raw
                                                      three-raw
                                                      label)
                             (build-nary-boolean-term right
                                                      zero-term
                                                      two-raw
                                                      three-raw
                                                      label))))
        (yices2-api/check-code result label)
        result)))))

(define (build-nary-binary-term terms zero-term two-raw label)
  (yices2-api/check-term-list terms label)
  (cond
   ((null? terms)
    zero-term)
   ((null? (cdr terms))
    (car terms))
   ((null? (cddr terms))
    (let ((result (two-raw (car terms) (cadr terms))))
      (yices2-api/check-code result label)
      result))
   (else
    (multiple-value-bind
        (left right)
        (split-list-halves terms)
      (let ((result (two-raw (build-nary-binary-term left
                                                     zero-term
                                                     two-raw
                                                     label)
                             (build-nary-binary-term right
                                                     zero-term
                                                     two-raw
                                                     label))))
        (yices2-api/check-code result label)
        result)))))

(define (yices2-api/or-terms terms)
  (build-nary-boolean-term terms
                           (yices2-api/false-term)
                           yices-or2-raw
                           yices-or3-raw
                           "yices_or"))

(define (yices2-api/and-terms terms)
  (build-nary-boolean-term terms
                           (yices2-api/true-term)
                           yices-and2-raw
                           yices-and3-raw
                           "yices_and"))

(define (yices2-api/application-term fun args)
  (unless (integer? fun)
    (sign-error "sal-cdr expected a Yices function term id, received ~a." fun))
  (yices2-api/check-term-list args "application arguments")
  (cond
   ((null? args)
    fun)
   (else
    (let ((term-array #f))
      (unwind-protect
       (begin
         (set! term-array (yices2-api/list->term-array args "application arguments"))
         (let ((term (yices-application-raw fun (length args) term-array)))
           (yices2-api/check-code term "yices_application")
           term))
       (when (yices2-api/pointer-live? term-array yices-term-array-null?)
         (yices-term-array-free term-array)))))))

(define (yices2-api/update-term fun args new-value)
  (unless (integer? fun)
    (sign-error "sal-cdr expected a Yices function update target term id, received ~a." fun))
  (yices2-api/check-term-list args "update arguments")
  (unless (integer? new-value)
    (sign-error "sal-cdr expected a Yices update value term id, received ~a." new-value))
  (when (null? args)
    (sign-error "sal-cdr expected at least one update argument."))
  (let ((term-array #f))
    (unwind-protect
     (begin
       (set! term-array (yices2-api/list->term-array args "update arguments"))
       (let ((term (yices-update-raw fun (length args) term-array new-value)))
         (yices2-api/check-code term "yices_update")
         term))
     (when (yices2-api/pointer-live? term-array yices-term-array-null?)
       (yices-term-array-free term-array)))))

(define (yices2-api/add-terms terms)
  (build-nary-binary-term terms
                          (yices2-api/rational-term "0")
                          yices-add-raw
                          "yices_add"))

(define (yices2-api/sub-term left right)
  (yices2-api/check-binary-terms left right "sub")
  (let ((term (yices-sub-raw left right)))
    (yices2-api/check-code term "yices_sub")
    term))

(define (yices2-api/mul-terms terms)
  (build-nary-binary-term terms
                          (yices2-api/rational-term "1")
                          yices-mul-raw
                          "yices_mul"))

(define (yices2-api/division-term left right)
  (yices2-api/check-binary-terms left right "division")
  (let ((term (yices-division-raw left right)))
    (yices2-api/check-code term "yices_division")
    term))

(define (yices2-api/idiv-term left right)
  (yices2-api/check-binary-terms left right "idiv")
  (let ((term (yices-idiv-raw left right)))
    (yices2-api/check-code term "yices_idiv")
    term))

(define (yices2-api/lt-term left right)
  (yices2-api/check-binary-terms left right "lt")
  (let ((term (yices-arith-lt-raw left right)))
    (yices2-api/check-code term "yices_arith_lt_atom")
    term))

(define (yices2-api/le-term left right)
  (yices2-api/check-binary-terms left right "le")
  (let ((term (yices-arith-le-raw left right)))
    (yices2-api/check-code term "yices_arith_leq_atom")
    term))

(define (yices2-api/gt-term left right)
  (yices2-api/check-binary-terms left right "gt")
  (let ((term (yices-arith-gt-raw left right)))
    (yices2-api/check-code term "yices_arith_gt_atom")
    term))

(define (yices2-api/ge-term left right)
  (yices2-api/check-binary-terms left right "ge")
  (let ((term (yices-arith-ge-raw left right)))
    (yices2-api/check-code term "yices_arith_geq_atom")
    term))

(define (yices2-api/lt0-term term)
  (unless (integer? term)
    (sign-error "sal-cdr expected a Yices term id, received ~a." term))
  (let ((atom (yices-arith-lt0-raw term)))
    (yices2-api/check-code atom "yices_arith_lt0_atom")
    atom))

(define (yices2-api/le0-term term)
  (unless (integer? term)
    (sign-error "sal-cdr expected a Yices term id, received ~a." term))
  (let ((atom (yices-arith-le0-raw term)))
    (yices2-api/check-code atom "yices_arith_leq0_atom")
    atom))

(define (yices2-api/gt0-term term)
  (unless (integer? term)
    (sign-error "sal-cdr expected a Yices term id, received ~a." term))
  (let ((atom (yices-arith-gt0-raw term)))
    (yices2-api/check-code atom "yices_arith_gt0_atom")
    atom))

(define (yices2-api/ge0-term term)
  (unless (integer? term)
    (sign-error "sal-cdr expected a Yices term id, received ~a." term))
  (let ((atom (yices-arith-ge0-raw term)))
    (yices2-api/check-code atom "yices_arith_geq0_atom")
    atom))

(define (yices2-api/term->string term)
  (unless (integer? term)
    (sign-error "sal-cdr expected a Yices term id, received ~a." term))
  (let ((term-str (yices-term->string-raw term)))
    (unless (string? term-str)
      (sign-error "sal-cdr failed to pretty-print a Yices term."))
    term-str))

(define (yices2-api/term-constructor term)
  (unless (integer? term)
    (sign-error "sal-cdr expected a Yices term id, received ~a." term))
  (let ((ctor (yices-term-constructor-raw term)))
    (yices2-api/check-code ctor "yices_term_constructor")
    ctor))

(define (yices2-api/term-num-children term)
  (unless (integer? term)
    (sign-error "sal-cdr expected a Yices term id, received ~a." term))
  (let ((n (yices-term-num-children-raw term)))
    (yices2-api/check-code n "yices_term_num_children")
    n))

(define (yices2-api/term-child term idx)
  (unless (integer? term)
    (sign-error "sal-cdr expected a Yices term id, received ~a." term))
  (unless (integer? idx)
    (sign-error "sal-cdr expected a Yices child index, received ~a." idx))
  (let ((child (yices-term-child-raw term idx)))
    (yices2-api/check-code child "yices_term_child")
    child))

(define (yices2-api/term-name term)
  (unless (integer? term)
    (sign-error "sal-cdr expected a Yices term id, received ~a." term))
  (let ((name (yices-term-name-raw term)))
    (and (string? name)
         name)))

(define (yices2-api/term-bool? term)
  (unless (integer? term)
    (sign-error "sal-cdr expected a Yices term id, received ~a." term))
  (= (yices-term-is-bool-raw term) 1))

(define (yices2-api/sum-component-term term idx)
  (unless (integer? term)
    (sign-error "sal-cdr expected a Yices term id, received ~a." term))
  (unless (integer? idx)
    (sign-error "sal-cdr expected a Yices summand index, received ~a." idx))
  (yices-sum-component-term-raw term idx))

(define (yices2-api/product-component-term term idx)
  (unless (integer? term)
    (sign-error "sal-cdr expected a Yices term id, received ~a." term))
  (unless (integer? idx)
    (sign-error "sal-cdr expected a Yices factor index, received ~a." idx))
  (yices-product-component-term-raw term idx))

(define (yices2-api/new-context interpolants?)
  (let ((config #f)
        (ctx #f))
    (unwind-protect
     (begin
       (set! config (yices-new-config))
       (yices2-api/check-pointer config yices-config-null? "yices_new_config")
       (yices2-api/check-code (yices-set-config config "solver-type" "mcsat")
                              "yices_set_config(solver-type=mcsat)")
       (when interpolants?
         (yices2-api/check-code (yices-set-config config "model-interpolation" "true")
                                "yices_set_config(model-interpolation=true)"))
       (set! ctx (yices-new-context config))
       (yices2-api/check-pointer ctx yices-context-null? "yices_new_context")
       ctx)
     (when (yices2-api/pointer-live? config yices-config-null?)
       (yices-free-config config)))))

(define (yices2-api/free-context! ctx)
  (when (yices2-api/pointer-live? ctx yices-context-null?)
    (yices-free-context ctx)))

(define (yices2-api/context-reset! ctx)
  (when (yices2-api/pointer-live? ctx yices-context-null?)
    (yices-reset-context ctx)))

(define (yices2-api/context-push! ctx)
  (yices2-api/check-code (yices-push-raw ctx) "yices_push"))

(define (yices2-api/context-pop! ctx)
  (yices2-api/check-code (yices-pop-raw ctx) "yices_pop"))

(define (yices2-api/assert-formula! ctx term)
  (unless (integer? term)
    (sign-error "sal-cdr expected a Yices term id to assert, received ~a." term))
  (yices2-api/check-code (yices-assert-formula-raw ctx term) "yices_assert_formula")
  term)

(define (yices2-api/check-context ctx)
  (yices-check-context-raw ctx))

(define (yices2-api/check-context-with-model ctx model terms)
  (yices2-api/check-term-list terms "model assumption terms")
  (let ((term-array #f))
    (unwind-protect
     (begin
       (set! term-array (yices2-api/list->term-array terms "model assumption terms"))
       (yices-check-context-with-model-raw ctx model (length terms) term-array))
     (when (yices2-api/pointer-live? term-array yices-term-array-null?)
       (yices-term-array-free term-array)))))

(define (yices2-api/get-model ctx)
  (let ((model (yices-get-model ctx 1)))
    (yices2-api/check-pointer model yices-model-null? "yices_get_model")
    model))

(define (yices2-api/free-model! model)
  (when (yices2-api/pointer-live? model yices-model-null?)
    (yices-free-model model)))

(define (yices2-api/model-from-map vars values)
  (yices2-api/check-term-list vars "model vars")
  (yices2-api/check-term-list values "model values")
  (unless (= (length vars) (length values))
    (sign-error "sal-cdr expected model vars and values to have the same length, received ~a and ~a."
                (length vars)
                (length values)))
  (let ((var-array #f)
        (value-array #f)
        (model #f))
    (unwind-protect
     (begin
       (set! var-array (yices2-api/list->term-array vars "model vars"))
       (set! value-array (yices2-api/list->term-array values "model values"))
       (set! model (yices-model-from-map-raw (length vars) var-array value-array))
       (yices2-api/check-pointer model yices-model-null? "yices_model_from_map")
       model)
     (when (yices2-api/pointer-live? var-array yices-term-array-null?)
       (yices-term-array-free var-array))
     (when (yices2-api/pointer-live? value-array yices-term-array-null?)
       (yices-term-array-free value-array)))))

(define (yices2-api/get-model-interpolant ctx)
  (let ((term (yices-get-model-interpolant-raw ctx)))
    (yices2-api/check-code term "yices_get_model_interpolant")
    term))

(define (yices2-api/try-value-as-term model term)
  (unless (integer? term)
    (sign-error "sal-cdr expected a Yices term id for value extraction, received ~a." term))
  (let ((value-term (yices-get-value-as-term-raw model term)))
    (and (>= value-term 0)
         value-term)))

(define (yices2-api/try-value-formula-string model term)
  (unless (integer? term)
    (sign-error "sal-cdr expected a Yices term id for formula extraction, received ~a." term))
  (let ((value-string (yices-get-value-formula-string-raw model term)))
    (and (string? value-string)
         value-string)))

(define (yices2-api/try-value-string model term)
  (unless (integer? term)
    (sign-error "sal-cdr expected a Yices term id for value extraction, received ~a." term))
  (let ((value-string (yices-get-value-string-raw model term)))
    (and (string? value-string)
         (> (string-length value-string) 0)
         value-string)))

(define (yices2-api/term-array-values model terms)
  (yices2-api/check-term-list terms "model value query terms")
  (let ((input-array #f)
        (output-array #f))
    (unwind-protect
     (begin
       (set! input-array (yices2-api/list->term-array terms "model value query terms"))
       (set! output-array (yices-term-array-alloc (length terms)))
       (when (and (> (length terms) 0)
                  (yices-term-array-null? output-array))
         (sign-error "sal-cdr failed to allocate a Yices output term array for model values."))
       (yices2-api/check-code (yices-term-array-value-raw model
                                                          (length terms)
                                                          input-array
                                                          output-array)
                              "yices_term_array_value")
       (yices2-api/term-array->list output-array (length terms) "model values"))
     (when (yices2-api/pointer-live? input-array yices-term-array-null?)
       (yices-term-array-free input-array))
     (when (yices2-api/pointer-live? output-array yices-term-array-null?)
       (yices-term-array-free output-array)))))

(define (yices2-api/generalize-model-terms model formula-terms elim-terms)
  (yices2-api/check-term-list formula-terms "formula terms")
  (yices2-api/check-term-list elim-terms "elimination terms")
  (let ((target-formula-terms (if (null? formula-terms)
                                (list (yices2-api/true-term))
                                formula-terms))
        (formula-array #f)
        (elim-array #f)
        (generalized #f))
    (unwind-protect
     (begin
       (set! formula-array (yices2-api/list->term-array target-formula-terms "formula terms"))
       (set! elim-array (yices2-api/list->term-array elim-terms "elimination terms"))
       (set! generalized (yices-term-vector-new))
       (yices2-api/check-pointer generalized yices-term-vector-null? "yices_term_vector_new")
       (let ((result (yices-generalize-model-array model
                                                   (length target-formula-terms)
                                                   formula-array
                                                   (length elim-terms)
                                                   elim-array
                                                   *yices-gen-default*
                                                   generalized)))
         (when (< result 0)
           (yices-term-vector-reset! generalized)
           (set! result (yices-generalize-model-array model
                                                      (length target-formula-terms)
                                                      formula-array
                                                      (length elim-terms)
                                                      elim-array
                                                      *yices-gen-by-subst*
                                                      generalized)))
         (yices2-api/check-code result "yices_generalize_model_array"))
       (yices2-api/collect-generalized-term-ids generalized))
     (when (yices2-api/pointer-live? generalized yices-term-vector-null?)
       (yices-term-vector-delete generalized))
     (when (yices2-api/pointer-live? formula-array yices-term-array-null?)
       (yices-term-array-free formula-array))
     (when (yices2-api/pointer-live? elim-array yices-term-array-null?)
       (yices-term-array-free elim-array)))))

(define (yices2-api/generalize-terms formula-terms elim-terms)
  (yices2-api/check-term-list formula-terms "formula terms")
  (yices2-api/check-term-list elim-terms "elimination terms")
  (let ((target-formula-terms (if (null? formula-terms)
                                (list (yices2-api/parse-term "true"))
                                formula-terms))
        (ctx #f)
        (model #f))
    (unwind-protect
     (begin
       (unless (= (yices-has-mcsat) 1)
         (sign-error "sal-cdr requires a Yices2 build with MCSAT support for in-process generalization."))
       (set! ctx (yices2-api/new-context #f))
       (for-each (lambda (term)
                   (yices2-api/assert-formula! ctx term))
                 target-formula-terms)
       (let ((status (yices2-api/check-context ctx)))
         (unless (= status *yices-status-sat*)
           (sign-error "sal-cdr expected the Yices generalization query to be satisfiable, but the in-process solver returned ~a."
                       (yices2-api/status->string status))))
       (set! model (yices2-api/get-model ctx))
       (yices2-api/generalize-model-terms model target-formula-terms elim-terms))
     (when (yices2-api/pointer-live? model yices-model-null?)
       (yices-free-model model))
     (when (yices2-api/pointer-live? ctx yices-context-null?)
       (yices-free-context ctx)))))

(define (yices2-api/generalize-formulas logic sort-names decl-specs keep-names formulas)
  (unless (string? logic)
    (sign-error "sal-cdr expected the Yices logic to be a string, received ~a." logic))
  (yices2-api/check-string-list sort-names "sort-names")
  (yices2-api/check-decl-specs decl-specs)
  (yices2-api/check-string-list keep-names "keep-names")
  (yices2-api/check-string-list formulas "formulas")
  (with-yices2-api
   (lambda ()
     (for-each yices2-api/declare-sort! sort-names)
     (let* ((decl-term-pairs
             (map (lambda (spec)
                    (cons (car spec)
                          (yices2-api/declare-uninterpreted-term! (car spec) (cdr spec))))
                  decl-specs))
            (formula-terms
             (map yices2-api/parse-term
                  (if (null? formulas)
                    (list "true")
                    formulas)))
            (elim-terms
             (map-and-filter
              (lambda (binding)
                (and (not (member (car binding) keep-names))
                     (cdr binding)))
              decl-term-pairs)))
       (map yices2-api/term->string
            (yices2-api/generalize-terms formula-terms elim-terms))))))
