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

(module sal-esm-support
        (include "sal.sch")
        (import fast-hash-table sal-ast-for-each queue sal-esm sal-esm-options 
                state-cache state-to-do-list sal-module gmp-scheme sal-type sal-decls 
                sal2scm-core sal-ast-env)
        (export (sal-esm/collect-lhs-name-expr esm table)
                (sal-esm/uses-next-operator? esm)
                (sal-esm-module/defined-vars-input-dependent esm-module)
                (sal-esm-module/break-definition-section esm-module)
                (sal-esm/remove-non-det-parts esm non-det-def-table)
                (sal-esm/mk-tick-counter-core-proc report-proc)
                (sal-esm/mk-tick-counter-proc cache to-do)
                (sal-esm/num-alternatives esm)
                (sal-esm/has-guards? esm)
                (sal-bounded-subtype/num-bits-and-lower type ctx)
                (sal-data-type/tag-num-bits type)
                (sal-scalar-type/num-bits type))
        )

(define-generic (sal-esm/collect-lhs-name-expr esm table))

(define-method (sal-esm/collect-lhs-name-expr (esm <primitive>) (table <primitive>))
  #unspecified)

(define-method (sal-esm/collect-lhs-name-expr (esm <sal-esm-statement>) (table <primitive>))
  #unspecified)

(define-method (sal-esm/collect-lhs-name-expr (esm <sal-esm-composition-statement>) (table <primitive>))
  (for-each (cut sal-esm/collect-lhs-name-expr <> table) (slot-value esm :statements)))

(define-method (sal-esm/collect-lhs-name-expr (esm <sal-esm-case>) (table <primitive>))
  (for-each (lambda (entry)
              (sal-esm/collect-lhs-name-expr (slot-value entry :statement) table))
            (slot-value esm :case-entries)))

(define-method (sal-esm/collect-lhs-name-expr (esm <sal-esm-when-undefined>) (table <primitive>))
  (sal-esm/collect-lhs-name-expr (slot-value esm :statement) table))

(define-method (sal-esm/collect-lhs-name-expr (esm <sal-esm-new-binds-statement>) (table <primitive>))
  (sal-esm/collect-lhs-name-expr (slot-value esm :statement) table))

(define-method (sal-esm/collect-lhs-name-expr (esm <sal-esm-assignment>) (table <primitive>))
  (eq-hash-table/put! table (slot-value (sal-lhs/name-expr (slot-value esm :lhs)) :decl) #t))
  
(define-method (sal-module/defined-variables (esm-module <sal-esm-module>))
  (let ((result (make-eq-hash-table)))
    (sal-esm/collect-lhs-name-expr (slot-value esm-module :definition) result)
    result))

(define-generic (sal-esm/uses-next-operator? esm))

(define-method (sal-esm/uses-next-operator? (esm <primitive>))
  #f)

(define-method (sal-esm/uses-next-operator? (esm <sal-esm-guard>))
  (sal-ast/uses-next-operator? (slot-value esm :expr)))

(define-method (sal-esm/uses-next-operator? (esm <sal-esm-assignment>))
  (sal-ast/uses-next-operator? (slot-value esm :rhs)))

(define-method (sal-esm/uses-next-operator? (esm <sal-esm-composition-statement>))
  (exists sal-esm/uses-next-operator? (slot-value esm :statements)))

(define-method (sal-esm/uses-next-operator? (esm <sal-esm-composition-statement>))
  (exists (lambda (entry) 
            (sal-esm/uses-next-operator? (slot-value entry :statement)))
          (slot-value esm :case-entries)))

(define-method (sal-esm/uses-next-operator? (esm <sal-esm-new-binds-statement>))
  (sal-esm/uses-next-operator? (slot-value esm :statement)))


(define-generic (collect-defined-vars-input-dependent! esm input-dep-vars))

(define-method (collect-defined-vars-input-dependent! (esm <primitive>) (input-dep-vars <primitive>))
  ;; do nothing
  #unspecified)
(define-method (collect-defined-vars-input-dependent! (esm <sal-esm-composition-statement>) (input-dep-vars <primitive>))
  (for-each (cut collect-defined-vars-input-dependent! <> input-dep-vars) (slot-value esm :statements)))
(define-method (collect-defined-vars-input-dependent! (esm <sal-esm-case>) (input-dep-vars <primitive>))
  (for-each (lambda (entry)
              (collect-defined-vars-input-dependent! (slot-value entry :statement) input-dep-vars))
            (slot-value esm :case-entries)))
(define-method (collect-defined-vars-input-dependent! (esm <sal-esm-when-undefined>) (input-dep-vars <primitive>))
  (collect-defined-vars-input-dependent! (slot-value esm :statement) input-dep-vars))

(define-method (collect-defined-vars-input-dependent! (esm <sal-esm-new-binds-statement>) (input-dep-vars <primitive>))
  (collect-defined-vars-input-dependent! (slot-value esm :statement) input-dep-vars))
(define-method (collect-defined-vars-input-dependent! (esm <sal-esm-assignment>) (input-dep-vars <primitive>))
  (let* ((lhs (slot-value esm :lhs))
         (name-expr (sal-lhs/name-expr lhs))
         (decl (slot-value name-expr :decl)))
    (when (and (not (eq-hash-table/get input-dep-vars decl))
               (sal-ast/find (lambda (ast)
                               (and (instance-of? ast <sal-name-expr>)
                                    ;; (breakpoint "collect-defined-vars-input-dependent!" (esm input-dep-vars ast) #t)
                                    (or (instance-of? (slot-value ast :decl) <sal-input-state-var-decl>)
                                        (eq-hash-table/get input-dep-vars (slot-value ast :decl)))))
                             (slot-value esm :rhs)))
      (eq-hash-table/put! input-dep-vars decl #t))))

(define (sal-esm-module/defined-vars-input-dependent esm-module)
  (let ((result (make-eq-hash-table))
        (definition (slot-value esm-module :definition)))
    (let loop ((prev-size 0))
      (collect-defined-vars-input-dependent! definition result)
      (let ((new-size (eq-hash-table/size result)))
        (unless (= new-size prev-size)
          (loop new-size))))
    result))
        
;; Break the definition section in two parts:
;;  1- independent of input vars
;;  2- dependent of input vars
(define (sal-esm-module/break-definition-section esm-module)
  (let ((input-dep-vars (sal-esm-module/defined-vars-input-dependent esm-module))
        (input-independent-queue (make-queue))
        (input-dependent-queue (make-queue))
        (definition (slot-value esm-module :definition)))
    ;; (eq-hash-table/for-each (lambda (k v)
    ;;                          (print "input dep: " (sal-decl/name k)))
    ;;                        input-dep-vars)
    (break-definitions-in-two-lists definition
                                    input-independent-queue
                                    input-dependent-queue
                                    input-dep-vars)
    (values (queue->esm-seq input-independent-queue definition)
            (queue->esm-seq input-dependent-queue definition))))

(define (queue->esm-seq queue place-provider)
  (if (= (queue/length queue) 0)
    #f
    (sal-esm/make-esm-seq* (queue->list queue) place-provider))) 

(define-generic (break-definitions-in-two-lists esm input-independent-queue input-dependent-queue input-dep-vars))
(define-method (break-definitions-in-two-lists (esm <primitive>) (input-independent-queue <primitive>)
                                               (input-dependent-queue <primitive>) (input-dep-vars <primitive>))
  #f)
(define-method (break-definitions-in-two-lists (esm <sal-esm-seq>) (input-independent-queue <primitive>)
                                               (input-dependent-queue <primitive>) (input-dep-vars <primitive>))
  (for-each (cut break-definitions-in-two-lists <> input-independent-queue input-dependent-queue input-dep-vars)
            (slot-value esm :statements)))
(define-method (break-definitions-in-two-lists (esm <sal-esm-statement>) (input-independent-queue <primitive>)
                                               (input-dependent-queue <primitive>) (input-dep-vars <primitive>))
  (let ((info (cons #unspecified #unspecified)))
    (decide-definition-kind esm info input-dep-vars)
    (unless (eq? (car info) #unspecified)
      (if (car info)
        (queue/insert! input-dependent-queue esm)
        (queue/insert! input-independent-queue esm)))))

(define-generic (decide-definition-kind esm info input-dep-vars))
(define-method (decide-definition-kind (esm <primitive>) (info <primitive>) (input-dep-vars <primitive>))
  #unspecified)
(define-method (decide-definition-kind (esm <sal-esm-composition-statement>) (info <primitive>) (input-dep-vars <primitive>))
  (for-each (cut decide-definition-kind <> info input-dep-vars) (slot-value esm :statements)))
(define-method (decide-definition-kind (esm <sal-esm-case>) (info <primitive>) (input-dep-vars <primitive>))
  (for-each (lambda (entry)
              (decide-definition-kind (slot-value entry :statement) info input-dep-vars))
            (slot-value esm :case-entries)))
(define-method (decide-definition-kind (esm <sal-esm-when-undefined>) (info <primitive>) (input-dep-vars <primitive>))
  (decide-definition-kind (slot-value esm :statement) info input-dep-vars))
(define-method (decide-definition-kind (esm <sal-esm-new-binds-statement>) (info <primitive>) (input-dep-vars <primitive>))
  (decide-definition-kind (slot-value esm :statement) info input-dep-vars))
(define-method (decide-definition-kind (esm <sal-esm-assignment>) (info <primitive>) (input-dep-vars <primitive>))
  (let* ((lhs (slot-value esm :lhs))
         (name-expr (sal-lhs/name-expr lhs))
         (decl (slot-value name-expr :decl))
         (input-dep? (eq-hash-table/get input-dep-vars decl)))
    (cond
     ((eq? (car info) #unspecified)
      (set-car! info input-dep?)
      (set-cdr! info esm))
     ((not (eq? (car info) input-dep?))
      (sign-unsupported-feature esm "This tools can not have in the same definition block (e.g., body of a FORALL definition) a variable that dependes on a input variable, and one that doesn't. The given assignment conflicts with the one defined at ~a" (format-with-location (cdr info) "")))))) 


(define-generic (sal-esm/remove-non-det-parts esm non-det-def-table))

(define-method (sal-esm/remove-non-det-parts (esm <primitive>) (non-det-def-table <primitive>))
  esm)

(define-method (sal-esm/remove-non-det-parts (esm <sal-esm-leaf>) (non-det-def-table <primitive>))
  esm)

(define-method (sal-esm/remove-non-det-parts (esm <sal-esm-multi-seq>) (non-det-def-table <primitive>))
  (let ((new-child (sal-esm/remove-non-det-parts (slot-value esm :statement) non-det-def-table)))
    (and new-child
         (copy-ast esm :statement new-child))))

(define-method (sal-esm/remove-non-det-parts (esm <sal-esm-choice>) (non-det-def-table <primitive>))
  (sal-esm/collect-lhs-name-expr esm non-det-def-table)
  #f)

(define-method (sal-esm/remove-non-det-parts (esm <sal-esm-case>) (non-det-def-table <primitive>))
  (sal-esm/collect-lhs-name-expr esm non-det-def-table)
  #f)

(define-method (sal-esm/remove-non-det-parts (esm <sal-esm-when-undefined>) (non-det-def-table <primitive>))
  ;; this kind of AST is not used in the definition section.
  (unreachable-code))

(define-method (sal-esm/remove-non-det-parts (esm <sal-esm-seq>) (non-det-def-table <primitive>))
  (let ((new-statements (map-and-filter (cut sal-esm/remove-non-det-parts <> non-det-def-table)
                                        (slot-value esm :statements))))
    (and (not (null? new-statements))
         (copy-ast esm
                   :statements new-statements))))

(define-method (sal-esm/remove-non-det-parts (esm <sal-esm-multi-choice>) (non-det-def-table <primitive>))
  (sal-esm/collect-lhs-name-expr esm non-det-def-table)
  #f)

(define (sal-esm/mk-tick-counter-core-proc report-proc)
  (let ((tick 0))
    (lambda (num-new-states depth)
      (set! tick (+ tick num-new-states))
      (when (>fx (/fx tick *esm-report-frequency*) 0)
        (report-proc depth)
        [assert (tick) (>= tick 0)]
        (set! tick (remainder tick *esm-report-frequency*))))))

(define (sal-esm/mk-tick-counter-proc cache to-do)
  (sal-esm/mk-tick-counter-core-proc (lambda (depth)
                                       (status-message :esm-tick (sc/size cache) (stdl/size to-do) depth)
                                       (verbose-message 3 
                                                        "  number of visited states: ~a, states to process: ~a, depth: ~a" 
                                                        (sc/size cache) 
                                                        (stdl/size to-do)
                                                        depth))))

(define-generic (sal-esm/num-alternatives esm))

(define-method (sal-esm/num-alternatives :around (esm <sal-esm-component>))
  (cond
   ((slot-value esm :num-alternatives) 
    => identity)
   (else 
    (let ((num-rules (call-next-method)))
      (set-slot-value! esm :num-alternatives num-rules)
      num-rules))))

(define-method (sal-esm/num-alternatives (esm <primitive>)) *mpq-zero*)
(define-method (sal-esm/num-alternatives (esm <sal-esm-guard>)) *mpq-one*)
(define-method (sal-esm/num-alternatives (esm <sal-esm-assignment>)) *mpq-one*)
(define-method (sal-esm/num-alternatives (esm <sal-esm-seq>))
  (fold-left (lambda (curr child-esm)
               (*mpq curr (sal-esm/num-alternatives child-esm)))
             *mpq-one*
             (slot-value esm :statements)))
(define-method (sal-esm/num-alternatives (esm <sal-esm-choice>))
  (fold-left (lambda (curr child-esm)
               (+mpq curr (sal-esm/num-alternatives child-esm)))
             *mpq-zero*
             (slot-value esm :statements)))
(define-method (sal-esm/num-alternatives (esm <sal-esm-case>))
  (fold-left (lambda (curr entry)
               (mpq/max curr (sal-esm/num-alternatives (slot-value entry :statement))))
             *mpq-zero*
             (slot-value esm :case-entries)))
(define-method (sal-esm/num-alternatives (esm <sal-esm-when-undefined>))
  (sal-esm/num-alternatives (slot-value esm :statement)))
(define-method (sal-esm/num-alternatives (esm <sal-esm-multi-seq>))
  (mpq/exp (sal-esm/num-alternatives (slot-value esm :statement))
           (sal-decl-list/num-elements (slot-value esm :local-decls))))
(define-method (sal-esm/num-alternatives (esm <sal-esm-multi-choice>))
  (*mpq (sal-esm/num-alternatives (slot-value esm :statement))
        (sal-decl-list/num-elements (slot-value esm :local-decls))))

(define-generic (sal-esm/has-guards? esm))
(define-method (sal-esm/has-guards? (esm <primitive>))  #f)
(define-method (sal-esm/has-guards? (esm <sal-esm-guard>))  #t)
(define-method (sal-esm/has-guards? (esm <sal-esm-assignment>)) #f)
(define-method (sal-esm/has-guards? (esm <sal-esm-composition-statement>))
  (exists sal-esm/has-guards? (slot-value esm :statements)))
(define-method (sal-esm/has-guards? (esm <sal-esm-case>))
  (exists (lambda (entry)
            (sal-esm/has-guards? (slot-value entry :statement)))
          (slot-value esm :case-entries)))
(define-method (sal-esm/has-guards? (esm <sal-esm-when-undefined>))
  [assert (esm) (not (sal-esm/has-guards? (slot-value esm :statement)))]
  #f)
(define-method (sal-esm/has-guards? (esm <sal-esm-new-binds-statement>))
  (sal-esm/has-guards? (slot-value esm :statement)))

(define *esm-num-lower-limit* (make-mpq "-524288"))
(define *esm-num-upper-limit* (make-mpq "524287"))

(define (calculate-bound bound sal-scm-ctx)
  (try
   (object->mpq (eval (sal->scm bound sal-scm-ctx (make-empty-env))))
   (lambda (escape p m o)
     (escape #f))))

(define (sal-esm/map-to-bits? lower upper)
  (or (not lower) (not upper) (<mpq lower *esm-num-lower-limit*) (>mpq upper *esm-num-upper-limit*)))

(define (sal-bounded-subtype/num-bits-and-lower type ctx)
  (let ((lower (calculate-bound (slot-value type :lower) ctx))
        (upper (calculate-bound (slot-value type :upper) ctx)))
    (if (sal-esm/map-to-bits? lower upper)
      (values 'infinite #unspecified)
      (values (mpq/num-bits (+mpq (-mpq upper lower) *mpq-one*)) 
              (mpq->integer lower)))))

(define (sal-data-type/tag-num-bits type)
  (mpq/num-bits (length (slot-value type :constructors))))

(define (sal-scalar-type/num-bits type)
  (mpq/num-bits (integer->mpq (length (slot-value type :scalar-elements)))))
