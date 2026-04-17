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
         (yices-new-config::yices-config () "bgl_yices_new_config")
         (yices-free-config::void (::yices-config) "bgl_yices_free_config")
         (yices-set-config::int (::yices-config ::string ::string) "bgl_yices_set_config")
         (yices-new-context::yices-context (::yices-config) "bgl_yices_new_context")
         (yices-free-context::void (::yices-context) "bgl_yices_free_context")
         (yices-parse-type::int (::string) "bgl_yices_parse_type")
         (yices-parse-term::int (::string) "bgl_yices_parse_term")
         (yices-new-uninterpreted-type::int () "bgl_yices_new_uninterpreted_type")
         (yices-set-type-name::int (::int ::string) "bgl_yices_set_type_name")
         (yices-new-uninterpreted-term::int (::int) "bgl_yices_new_uninterpreted_term")
         (yices-set-term-name::int (::int ::string) "bgl_yices_set_term_name")
         (yices-assert-formula::int (::yices-context ::int) "bgl_yices_assert_formula")
         (yices-get-model::yices-model (::yices-context ::int) "bgl_yices_get_model")
         (yices-free-model::void (::yices-model) "bgl_yices_free_model")
         (yices-generalize-model-array::int (::yices-model ::int ::yices-term-array ::int ::yices-term-array ::int ::yices-term-vector)
                                            "bgl_yices_generalize_model_array")
         (yices-check-context-default::int (::yices-context) "bgl_yices_check_context_default")
         (yices-error-string::obj () "bgl_yices_error_string")
         (yices-term->string::obj (::int) "bgl_yices_term_to_string")
         (yices-term-array-alloc::yices-term-array (::int) "bgl_yices_term_array_alloc")
         (yices-term-array-set!::void (::yices-term-array ::int ::int) "bgl_yices_term_array_set")
         (yices-term-array-free::void (::yices-term-array) "bgl_yices_term_array_free")
         (yices-term-vector-new::yices-term-vector () "bgl_yices_term_vector_new")
         (yices-term-vector-delete::void (::yices-term-vector) "bgl_yices_term_vector_delete")
         (yices-term-vector-reset!::void (::yices-term-vector) "bgl_yices_term_vector_reset")
         (yices-term-vector-size::int (::yices-term-vector) "bgl_yices_term_vector_size")
         (yices-term-vector-ref::int (::yices-term-vector ::int) "bgl_yices_term_vector_ref"))
        (export (yices2-api/has-mcsat?)
                (yices2-api/generalize-formulas logic sort-names decl-specs keep-names formulas)))

;; yices_types.h:
;;   YICES_STATUS_SAT = 3
;;   YICES_GEN_DEFAULT = 0
;;   YICES_GEN_BY_SUBST = 1
(define *yices-status-sat* 3)
(define *yices-gen-default* 0)
(define *yices-gen-by-subst* 1)

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
  (when (null? ptr)
    (sign-error "~a failed: ~a" where (yices2-api/error-string))))

(define (yices2-api/pointer-live? ptr null?)
  (and ptr
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

(define (yices2-api/collect-generalized-terms generalized)
  (let ((size (yices-term-vector-size generalized)))
    (if (= size 0)
      (list "true")
      (let loop ((idx 0)
                 (result '()))
        (if (= idx size)
          (reverse! result)
          (let ((term-str (yices-term->string (yices-term-vector-ref generalized idx))))
            (unless (string? term-str)
              (sign-error "sal-cdr failed to pretty-print a Yices generalized term."))
            (loop (+ idx 1) (cons term-str result))))))))

(define (yices2-api/has-mcsat?)
  (yices-init)
  (unwind-protect
   (= (yices-has-mcsat) 1)
   (yices-exit)))

(define (yices2-api/generalize-formulas logic sort-names decl-specs keep-names formulas)
  (unless (string? logic)
    (sign-error "sal-cdr expected the Yices logic to be a string, received ~a." logic))
  (yices2-api/check-string-list sort-names "sort-names")
  (yices2-api/check-decl-specs decl-specs)
  (yices2-api/check-string-list keep-names "keep-names")
  (yices2-api/check-string-list formulas "formulas")
  (let ((config #f)
        (ctx #f)
        (model #f)
        (generalized #f)
        (formula-array #f)
        (elim-array #f))
    (unwind-protect
     (begin
       (yices-init)
       (unless (= (yices-has-mcsat) 1)
         (sign-error "sal-cdr requires a Yices2 build with MCSAT support for in-process generalization."))
       (for-each
        (lambda (sort-name)
          (let ((tau (yices-new-uninterpreted-type)))
            (yices2-api/check-code tau
                                   (string-append "yices_new_uninterpreted_type(" sort-name ")"))
            (yices2-api/check-code (yices-set-type-name tau sort-name)
                                   (string-append "yices_set_type_name(" sort-name ")"))))
        sort-names)
       (let ((decl-term-pairs
              (map (lambda (spec)
                     (let* ((name (car spec))
                            (tau (yices-parse-type (cdr spec))))
                       (yices2-api/check-code tau
                                              (string-append "yices_parse_type(" name ")"))
                       (let ((term (yices-new-uninterpreted-term tau)))
                         (yices2-api/check-code term
                                                (string-append "yices_new_uninterpreted_term(" name ")"))
                         (yices2-api/check-code (yices-set-term-name term name)
                                                (string-append "yices_set_term_name(" name ")"))
                         (cons name term))))
                   decl-specs)))
         (set! config (yices-new-config))
         (yices2-api/check-pointer config yices-config-null? "yices_new_config")
         (yices2-api/check-code (yices-set-config config "solver-type" "mcsat")
                                "yices_set_config(solver-type=mcsat)")
         (set! ctx (yices-new-context config))
         (yices2-api/check-pointer ctx yices-context-null? "yices_new_context")
         (yices-free-config config)
         (set! config #f)
         (let ((formula-terms
                (map (lambda (formula)
                       (let ((term (yices-parse-term formula)))
                         (yices2-api/check-code term
                                                "yices_parse_term")
                         (yices2-api/check-code (yices-assert-formula ctx term)
                                                "yices_assert_formula")
                         term))
                     (if (null? formulas)
                       (list "true")
                       formulas))))
           (let ((status (yices-check-context-default ctx)))
             (unless (= status *yices-status-sat*)
               (sign-error "sal-cdr expected the Yices generalization query to be satisfiable, but the in-process solver returned ~a."
                           (yices2-api/status->string status))))
           (set! model (yices-get-model ctx 1))
           (yices2-api/check-pointer model yices-model-null? "yices_get_model")
           (set! formula-array (yices2-api/list->term-array formula-terms "formula terms"))
           (let* ((elim-terms
                   (map-and-filter
                    (lambda (binding)
                      (and (not (member (car binding) keep-names))
                           (cdr binding)))
                    decl-term-pairs))
                  (_ (set! elim-array
                           (yices2-api/list->term-array elim-terms "elimination terms")))
                  (_ (set! generalized (yices-term-vector-new))))
             (yices2-api/check-pointer generalized yices-term-vector-null? "yices_term_vector_new")
             (let ((result (yices-generalize-model-array model
                                                         (length formula-terms)
                                                         formula-array
                                                         (length elim-terms)
                                                         elim-array
                                                         *yices-gen-default*
                                                         generalized)))
               (when (< result 0)
                 (yices-term-vector-reset! generalized)
                 (set! result (yices-generalize-model-array model
                                                            (length formula-terms)
                                                            formula-array
                                                            (length elim-terms)
                                                            elim-array
                                                            *yices-gen-by-subst*
                                                            generalized)))
             (yices2-api/check-code result "yices_generalize_model_array"))
             (yices2-api/collect-generalized-terms generalized)))))
     (begin
       (when (yices2-api/pointer-live? generalized yices-term-vector-null?)
         (yices-term-vector-delete generalized))
       (when (yices2-api/pointer-live? model yices-model-null?)
         (yices-free-model model))
       (when (yices2-api/pointer-live? ctx yices-context-null?)
         (yices-free-context ctx))
       (when (yices2-api/pointer-live? config yices-config-null?)
         (yices-free-config config))
       (when (yices2-api/pointer-live? formula-array yices-term-array-null?)
         (yices-term-array-free formula-array))
       (when (yices2-api/pointer-live? elim-array yices-term-array-null?)
         (yices-term-array-free elim-array))
       (yices-exit)))))
