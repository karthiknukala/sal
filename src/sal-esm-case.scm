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

(module sal-esm-case
        (include "sal.sch")
        (import sal-ast-table fast-hash-table sal-decls sal-expression 
                sal-esm-support sal-ast-eq sal-esm)
        (export (sal-esm-module/promote-choice-to-case esm-module))
        )

;; Collect the guard expressions that can be transformed in case-stmts.
;; A guard expression is considered when it is an equality between
;; a simple value (numeral or scalar), and a lhs which does not use
;; next value of a state variable.
(define (collect-case-target esm)
  (let ((result (make-sal-ast-table)))
    (collect-case-target-core! esm '() result)
    result))

(define-generic (collect-case-target-core! esm decl-list target-table))
(define-method (collect-case-target-core! (esm <sal-esm-assignment>) (decl-list <primitive>) (target-table <primitive>))
  #unspecified)
(define (simple-value? expr)
  (or (instance-of? expr <sal-numeral>)
      (instance-of? expr <sal-scalar>)))
(define (target-lhs? expr decl-list)
  (and (sal-expr/lhs? expr)
       (not (sal-ast/contains-reference*? expr decl-list))))
(define-method (collect-case-target-core! (esm <sal-esm-guard>) (decl-list <primitive>) (target-table <primitive>))
  (let ((expr (slot-value esm :expr)))
    (and (instance-of? expr <sal-eq>)
         (not (sal-esm/uses-next-operator? esm))
         (let ((arg1 (sal-binary-application/arg1 expr))
               (arg2 (sal-binary-application/arg2 expr)))
           (cond
            ((and (simple-value? arg1) (target-lhs? arg2 decl-list))
             (sal-ast-table/put! target-table arg2 arg1))
            ((and (simple-value? arg2) (target-lhs? arg1 decl-list))
             (sal-ast-table/put! target-table arg1 arg2)))))))
(define-method (collect-case-target-core! (esm <sal-esm-choice>) (decl-list <primitive>) (target-table <primitive>))
  #unspecified)
(define-method (collect-case-target-core! (esm <sal-esm-case>) (decl-list <primitive>) (target-table <primitive>))
  #unspecified)
(define-method (collect-case-target-core! (esm <sal-esm-when-undefined>) (decl-list <primitive>) (target-table <primitive>))
  #unspecified)
(define-method (collect-case-target-core! (esm <sal-esm-seq>) (decl-list <primitive>) (target-table <primitive>))
  (for-each (cut collect-case-target-core! <> decl-list target-table) (slot-value esm :statements)))
(define-method (collect-case-target-core! (esm <sal-esm-new-binds-statement>) (decl-list <primitive>) 
                                          (target-table <primitive>))
  (collect-case-target-core! (slot-value esm :statement)
                        (append (slot-value esm :local-decls) decl-list)
                        target-table))

(define-generic (erase-guard-exprs-subsumed-by-case-stmt esm lhs value))
(define-method (erase-guard-exprs-subsumed-by-case-stmt (esm <sal-esm-assignment>) (lhs <primitive>) (value <primitive>))
  esm)
(define-method (erase-guard-exprs-subsumed-by-case-stmt (esm <sal-esm-guard>) (lhs <primitive>) (value <primitive>))
  (let ((expr (slot-value esm :expr)))
    (if (instance-of? expr <sal-eq>)
      (let ((arg1 (sal-binary-application/arg1 expr))
            (arg2 (sal-binary-application/arg2 expr)))
        (if (or (and (simple-value? arg1)
                     (sal-ast/equivalent? arg1 value)
                     (sal-ast/equivalent? arg2 lhs))
                (and (simple-value? arg2)
                     (sal-ast/equivalent? arg2 value)
                     (sal-ast/equivalent? arg1 lhs)))
          #f
          esm))
      esm)))
(define-method (erase-guard-exprs-subsumed-by-case-stmt (esm <sal-esm-choice>) (lhs <primitive>) (value <primitive>))
  esm)
(define-method (erase-guard-exprs-subsumed-by-case-stmt (esm <sal-esm-case>) (lhs <primitive>) (value <primitive>))
  esm)
(define-method (erase-guard-exprs-subsumed-by-case-stmt (esm <sal-esm-when-undefined>) (lhs <primitive>) (value <primitive>))
  esm)
(define-method (erase-guard-exprs-subsumed-by-case-stmt (esm <sal-esm-new-binds-statement>) (lhs <primitive>) (value <primitive>))
  (let ((new-stmt (erase-guard-exprs-subsumed-by-case-stmt (slot-value esm :statement) lhs value)))
    (and new-stmt
         (update-ast-slots esm
                           :statement new-stmt))))
(define-method (erase-guard-exprs-subsumed-by-case-stmt (esm <sal-esm-seq>) (lhs <primitive>) (value <primitive>))
  (let ((stmts (conservative-map-filter (cut erase-guard-exprs-subsumed-by-case-stmt <> lhs value)
                                        (slot-value esm :statements))))
    (sal-esm/make-esm-seq* stmts esm)))
(define-method (erase-guard-exprs-subsumed-by-case-stmt (esm <sal-esm-monitor-seq>) (lhs <primitive>) (value <primitive>))
  (change-class (call-next-method)
                <sal-esm-monitor-seq>))


(define-generic (sal-esm/promote-choice-to-case esm))
(define-method (sal-esm/promote-choice-to-case (esm <primitive>))
  esm)
(define-method (sal-esm/promote-choice-to-case (esm <sal-esm-leaf>))
  esm)
(define-method (sal-esm/promote-choice-to-case (esm <sal-esm-new-binds-statement>))
  (update-ast-slots esm
                    :statement (sal-esm/promote-choice-to-case (slot-value esm :statement))))
(define-method (sal-esm/promote-choice-to-case (esm <sal-esm-seq>))
  (update-ast-slots esm
                    :statements (conservative-map-1 sal-esm/promote-choice-to-case (slot-value esm :statements))))
(define-method (sal-esm/promote-choice-to-case (esm <sal-esm-case>))
  (update-ast-slots esm
                    :case-entries 
                    (conservative-map-1 
                     (lambda (entry)
                       (update-ast-slots entry
                                         :statement (sal-esm/promote-choice-to-case (slot-value entry :statement))))
                     (slot-value esm :case-entries))))
(define-method (sal-esm/promote-choice-to-case (esm <sal-esm-when-undefined>))
  esm)

(define-method (sal-esm/promote-choice-to-case (esm <sal-esm-choice>))
  (let* ((statements (slot-value esm :statements))
         (target-tables (map collect-case-target statements))
         (found-lhs (make-sal-ast-table)))
    ;; collect all found lhs.
    ;; Use target-tables and :statements to create the table
    ;; lhs -> list of pairs (val . list of stmt)
    ;; This table is used to decide which lhs is going to be
    ;; use in the new case statement.
    (for-each (lambda (statement target-table)
                (sal-ast-table/for-each (lambda (lhs val)
                                          (cond
                                           ((sal-ast-table/get found-lhs lhs) =>
                                            (lambda (entry)
                                              (let ((pair-list (cdr entry)))
                                                (cond
                                                 ((find (lambda (pair)
                                                          (sal-ast/equivalent? (car pair) val))
                                                        pair-list) =>
                                                        (lambda (pair)
                                                          ;; include the current statement in this pair
                                                          (set-cdr! pair (cons statement (cdr pair)))))
                                                 (else
                                                  ;; add a new pair (val . (statement))
                                                  (set-cdr! entry (cons (cons val (list statement)) pair-list)))))))
                                           (else
                                            ;; add a fresh entry in the table.
                                            (sal-ast-table/put! found-lhs lhs (list (cons val (list statement)))))))
                                        target-table))
              statements
              target-tables)
    ;; select the lhs with the longest list of pairs (i.e., with more alternatives)
    (let ((selected-lhs #f)
          (num-max-alternatives 0))
      (sal-ast-table/for-each (lambda (lhs pair-list)
                                (when (or (not selected-lhs)
                                          (> (length pair-list) num-max-alternatives))
                                  (set! selected-lhs lhs)
                                  (set! num-max-alternatives (length pair-list))))
                              found-lhs)
      (cond
       ((<= num-max-alternatives 1)
        ;; the optimization cannot be applied...
        (update-ast-slots esm
                          :statements (conservative-map-1 sal-esm/promote-choice-to-case (slot-value esm :statements))))
       (else
        (let* ((pair-list (cdr (sal-ast-table/get found-lhs selected-lhs)))
               (num-used-stmts 0)
               (place-provider esm)
               (case-entries (map (lambda (pair)
                                    (let* ((val (car pair))
                                           (stmts (map (cut erase-guard-exprs-subsumed-by-case-stmt <> selected-lhs val)
                                                       (cdr pair))))
                                      (if (null? (cdr stmts))
                                        ;; only one stmt for this value
                                        (make-ast-instance <sal-esm-case-entry> place-provider
                                                           :value val
                                                           :statement (sal-esm/promote-choice-to-case (car stmts)))
                                        ;; more than one stmt for this value, so I create a choice stmt for them
                                        (let ((aux-choice (sal-esm/promote-choice-to-case
                                                           (make-ast-instance <sal-esm-choice> place-provider
                                                                              :statements stmts))))
                                          (make-ast-instance <sal-esm-case-entry> place-provider
                                                             :value val
                                                             :statement aux-choice)))))
                                  pair-list))
               (case-stmt (make-ast-instance <sal-esm-case> place-provider
                                             :expr selected-lhs
                                             :case-entries case-entries))
               (used-stmts (make-eq-hash-table)))
          (for-each (lambda (pair)
                      (let ((stmt-list (cdr pair)))
                        (set! num-used-stmts (+ num-used-stmts (length stmt-list)))
                        (for-each (cut eq-hash-table/put! used-stmts <> #t) stmt-list)))
                    pair-list)
          (cond
           ((< num-used-stmts (length statements))
            (let ((non-used-stmts (filter (lambda (stmt)
                                            (not (eq-hash-table/get used-stmts stmt)))
                                          statements)))
              ;; combine the case-stmt and the non-used-stmts with a choice stmt.
              (make-ast-instance <sal-esm-choice> esm
                                 :statements (cons case-stmt non-used-stmts))))
           (else
            case-stmt))))))))

(define (sal-esm-module/promote-choice-to-case esm-module)
  (update-ast-slots esm-module
                    :transition (sal-esm/promote-choice-to-case (slot-value esm-module :transition))))
  
