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

(module sal-esm-lhs
        (include "sal.sch")
        (import fast-hash-table sal-module sal-lhs-set sal-collect-state-lhs
                sal-expression)
        (export <sal-esm-used-provided-lhs-ctx>
                (make-sal-esm-used-provided-lhs-ctx module . conservative?)
                (sal-esm-used-provided-lhs-ctx/init-section! ctx section-id)
                (sal-esm/used-and-provided-lhs esm ctx))
        )

;; Collect LHS used and provided by sal-esm-statements

;; --------------------------------------------------
;; sal-esm/used-and-provided-lhs CONTEXT
;;
(define-class <sal-esm-used-provided-lhs-ctx> () 
  (;; Caches the result of previous calls to sal-esm/used-and-provided-lhs
   :cached-used-and-provided-lhs
   ;; Stores the table of defined variables.
   :defined-variables
   ;; Specifies which transition relation section is being analyzed
   ;; The possible values for this slot are:
   ;;
   ;; 'definition     : definition section, input and current state variables
   ;;                   are always available, so they are not included
   ;;                   in the used-variables result set.
   ;;
   ;; 'pre-definition : similar to 'definition, but includes input variables
   ;;                   in the used-variables result set.
   ;;
   ;; 'initialization : initialization section
   ;; 'transition     : transition section; input, currrent state and defined
   ;;                   variables are always available, so they are not included
   ;;                   in the used-variables result set.
   :section-id
   ;; Specifies how non-ground lhs is handled.
   ;; #t -> non-ground LHS in assignments are ignored.
   :conservative?))
;; --------------------------------------------------

(define (make-sal-esm-used-provided-lhs-ctx module . conservative?)
  (let ((conservative? (optional-arg conservative? #f)))
    (make-instance <sal-esm-used-provided-lhs-ctx>
                   :defined-variables (sal-module/defined-variables module)
                   :conservative? conservative?)))

(define (sal-esm-used-provided-lhs-ctx/init-section! ctx section-id)
  [assert (ctx section-id) (memq section-id '(pre-definition definition initialization transition))]
  (set-slot-value! ctx :cached-used-and-provided-lhs (make-eq-hash-table))
  (set-slot-value! ctx :section-id section-id))

(define-generic (sal-esm/used-and-provided-lhs esm ctx))

(define-method (sal-esm/used-and-provided-lhs :around (esm <sal-esm-statement>) (ctx <sal-esm-used-provided-lhs-ctx>))
  (let ((cached-table (slot-value ctx :cached-used-and-provided-lhs)))
    (cond
     ((eq-hash-table/get cached-table esm) =>
      (lambda (entry)
        (let ((result-pair (cdr entry)))
          (values (car result-pair) (cdr result-pair)))))
     (else
      (multiple-value-bind
          (used-lhs provided-lhs)
          (call-next-method)
        (eq-hash-table/put! cached-table esm (cons used-lhs provided-lhs))
        (values used-lhs provided-lhs))))))

(define (add-used-lhs-core set lhs defined-variables section-id)
  (let ((lhs-decl   (slot-value (sal-lhs/name-expr lhs) :decl)))
    (cond
     ((eq? section-id 'pre-definition)
      ;; in the first pass in the definition section... we consider
      ;; that input variables are not available...  so we record the
      ;; use of input variables and other defined variables.
      ;;
      ;; in this way, the defined variables that do not depend on
      ;; state variables are going to be move to the beginning of
      ;; the list.
      (if (or (eq-hash-table/get defined-variables lhs-decl)
              (instance-of? lhs-decl <sal-input-state-var-decl>))
        (sal-lhs-set/add set lhs)
        set))
     ((instance-of? lhs-decl <sal-input-state-var-decl>)
      (when (sal-expr/next-lhs? lhs)
        (sign-unsupported-feature lhs "Next value of input variables is not supported by this tool."))
      (when (eq? section-id 'initialization)
        (sign-unsupported-feature lhs "Input variables cannot be used in the initialization section."))
      set)
     ((eq? section-id 'definition)
      [assert (lhs) (not (sal-expr/next-lhs? lhs))]
      ;; state variables are also available in the definition section.
      ;; So, we should consider only used defined variables.
      ;; (breakpoint "add-used-lhs-core!" (set lhs defined-variables lhs-decl) #t)
      (if (eq-hash-table/get defined-variables lhs-decl)
        (sal-lhs-set/add set lhs)
        set))
     ((eq? section-id 'transition)
      (let ((next? (sal-expr/next-lhs? lhs)))
        (when (and next? (eq-hash-table/get defined-variables lhs-decl))
          (sign-unsupported-feature lhs "Next value of defined variables is not supported by this tool."))
        (if next?
          ;; only current variables are tracked in the transition section
          (sal-lhs-set/add set lhs)
          set)))
     (else
      [assert (section-id) (eq? section-id 'initialization)]
      (sal-lhs-set/add set lhs)))))

(define (used-lhs-in-expr expr ctx)
  (let* ((lhs-set (make-sal-lhs-set))
         (section-id (slot-value ctx :section-id))
         (defined-variables (slot-value ctx :defined-variables))
         (add-proc! (lambda (lhs)
                      (set! lhs-set (add-used-lhs-core lhs-set lhs defined-variables section-id)))))
    (sal-ast/collect-state-lhs! expr add-proc!)
    lhs-set))

(define-method (sal-esm/used-and-provided-lhs (esm <sal-esm-guard>) (ctx <sal-esm-used-provided-lhs-ctx>))
  (values (used-lhs-in-expr (slot-value esm :expr) ctx) (make-sal-lhs-set)))

(define-method (sal-esm/used-and-provided-lhs (esm <sal-esm-choice-assignment>) (ctx <sal-esm-used-provided-lhs-ctx>))
  (values (make-sal-lhs-set) (make-sal-lhs-set)))

(define-method (sal-esm/used-and-provided-lhs (esm <sal-esm-assignment>) (ctx <sal-esm-used-provided-lhs-ctx>))
  (let* ((provided-lhs #f)
         (add-provided-lhs! (lambda (lhs)
                              [assert (provided-lhs) (not provided-lhs)]
                              (set! provided-lhs lhs)))
         (used-lhs-set (used-lhs-in-expr (slot-value esm :rhs) ctx)))
    (when (or (not (slot-value ctx :conservative?))
              (sal-lhs/ground? (slot-value esm :lhs)))
      (sal-ast/collect-state-lhs! (slot-value esm :lhs) add-provided-lhs!))
    (values used-lhs-set (if provided-lhs
                           (sal-lhs-set/add (make-sal-lhs-set) provided-lhs)
                           (make-sal-lhs-set)))))

(define-method (sal-esm/used-and-provided-lhs (esm <sal-esm-choice>) (ctx <sal-esm-used-provided-lhs-ctx>))
  (let loop ((used-lhs-set (make-sal-lhs-set))
             (provided-lhs-set #f)
             (statements (slot-value esm :statements)))
    (if (null? statements)
      (values used-lhs-set (if provided-lhs-set
                             provided-lhs-set
                             (make-sal-lhs-set)))
      (multiple-value-bind
          (child-used-lhs child-provided-lhs)
          (sal-esm/used-and-provided-lhs (car statements) ctx)
        (loop (sal-lhs-set/union used-lhs-set child-used-lhs)
              (if provided-lhs-set
                (sal-lhs-set/intersection provided-lhs-set child-provided-lhs)
                child-provided-lhs)
              (cdr statements))))))

(define-method (sal-esm/used-and-provided-lhs (esm <sal-esm-case>) (ctx <sal-esm-used-provided-lhs-ctx>))
  (let loop ((used-lhs-set (used-lhs-in-expr (slot-value esm :expr) ctx))
             (provided-lhs-set #f)
             (case-entries (slot-value esm :case-entries)))
    (if (null? case-entries)
      (values used-lhs-set provided-lhs-set)
      (multiple-value-bind
          (child-used-lhs child-provided-lhs)
          (sal-esm/used-and-provided-lhs (slot-value (car case-entries) :statement) ctx)
        (loop (sal-lhs-set/union used-lhs-set child-used-lhs)
              (if provided-lhs-set
                (sal-lhs-set/intersection provided-lhs-set child-provided-lhs)
                child-provided-lhs)
              (cdr case-entries))))))

(define-method (sal-esm/used-and-provided-lhs (esm <sal-esm-when-undefined>) (ctx <sal-esm-used-provided-lhs-ctx>))
  (sal-esm/used-and-provided-lhs (slot-value esm :statement) ctx))

(define-method (sal-esm/used-and-provided-lhs (esm <sal-esm-new-binds-statement>) (ctx <sal-esm-used-provided-lhs-ctx>))
  (sal-esm/used-and-provided-lhs (slot-value esm :statement) ctx))

(define-method (sal-esm/used-and-provided-lhs (esm <sal-esm-seq>) (ctx <sal-esm-used-provided-lhs-ctx>))
  (let loop ((used-lhs-set (make-sal-lhs-set))
             (provided-lhs-set (make-sal-lhs-set))
             (statements (slot-value esm :statements)))
    (if (null? statements)
      (values (sal-lhs-set/difference used-lhs-set provided-lhs-set) provided-lhs-set)
      (multiple-value-bind
          (child-used-lhs child-provided-lhs)
          (sal-esm/used-and-provided-lhs (car statements) ctx)
        ;; (breakpoint "sal-esm/used-and-provided-lhs" (child-provided-lhs child-used-lhs esm ctx used-lhs-set provided-lhs-set statements)
        ;;  #t)
        (loop (sal-lhs-set/union used-lhs-set child-used-lhs)
              (sal-lhs-set/union provided-lhs-set child-provided-lhs)
              (cdr statements))))))
