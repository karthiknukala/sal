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

(module sal-promote-inputs
        (include "sal.sch")
        (import sal-esm sal-ast-copy sal-ast-env fast-hash-table
                unique-names sal-ast-for-each)
        (export (sal-esm-module/promote-inputs esm-module))
        )

;; The explicit state model checker cannot handle specifications which
;; use the next value of an input variable. So, this procedure
;; promotes input variables to local variables, and the non-deterministic
;; assignments are also included.
;;
;; Remark: an WARNING is produced to the user, since this transformation
;; may hide possible deadlocks.
(define (sal-esm-module/promote-inputs esm-module)
  (let ((transition (slot-value esm-module :transition))
        (input-vars-to-promote-table (make-eq-hash-table)))
    (sal-esm/collect-next-input-vars! transition input-vars-to-promote-table)
    (if (= (eq-hash-table/size input-vars-to-promote-table) 0)
      esm-module
      (let ((input-vars-to-promote (eq-hash-table/fold-keys (flip cons) '() input-vars-to-promote-table)))
        (warning-message 
         "The specification uses the next value of the input variables: ~a. They will be converted to local variables, and non-deterministic assignments for them will be include in the initialization/transition section." 
         (map sal-decl/name input-vars-to-promote))
        (let* ((new-locals (map (lambda (var-decl)
                                  (change-class var-decl <sal-local-state-var-decl>))
                                input-vars-to-promote))
               (env (update-env* (make-empty-env) input-vars-to-promote new-locals))
               (new-state-vars (map (lambda (var-decl)
                                      (cond
                                       ((lookup-env var-decl env) =>
                                        identity)
                                       (else
                                        var-decl)))
                                    (slot-value esm-module :state-vars)))
               (new-definition (sal-esm/substitute (slot-value esm-module :definition) env))
               (tmp-initialization (slot-value esm-module :initialization))
               (tmp-transition (sal-esm/substitute transition env))
               (init-place-provider (slot-value esm-module :initialization))
               (trans-place-provider (slot-value esm-module :transition))
               (new-non-det-assignments (gen-non-det-assignments-for new-locals #f init-place-provider))
               (new-next-non-det-assignments (gen-non-det-assignments-for new-locals #t trans-place-provider))
               (new-initialization (sal-esm/make-esm-seq new-non-det-assignments
                                                         tmp-initialization
                                                         init-place-provider))
               (new-transition (sal-esm/make-esm-seq new-next-non-det-assignments
                                                     tmp-transition
                                                     trans-place-provider))) ;; <<< place provider
          (copy-ast esm-module
                    :state-vars new-state-vars
                    :initialization new-initialization
                    :definition new-definition
                    :transition new-transition))))))

(define (gen-non-det-assignments-for local-var-decls next? place-provider)
  (sal-esm/make-esm-seq* (map (cut gen-non-det-assignment-for <> next?)
                              local-var-decls)
                         place-provider))
  
(define (gen-non-det-assignment-for local-var-decl next?)
  (let* ((place-provider local-var-decl)
         (aux-name (gen-unique-name 'aux))
         (aux-id (make-sal-identifier place-provider aux-name))
         (aux-var-decl (make-ast-instance <sal-var-decl> local-var-decl
                                          :id aux-id 
                                          :type (slot-value local-var-decl :type)))
         (aux-name-expr (make-sal-name-expr aux-var-decl))
         (local-name-expr (make-sal-name-expr local-var-decl)))
    (make-ast-instance <sal-esm-multi-choice> place-provider
                       :local-decls (list aux-var-decl)
                       :statement (make-ast-instance <sal-esm-assignment> place-provider
                                                     :lhs (if next?
                                                            (make-ast-instance <sal-next-operator> place-provider
                                                                               :name-expr local-name-expr)
                                                            local-name-expr)
                                                     :rhs aux-name-expr))))

                              
(define (sal-esm/collect-next-input-vars! esm var-decl-table)
  (when esm
    (sal-ast/for-each (lambda (ast)
                        (when (and (instance-of? ast <sal-next-operator>)
                                   (instance-of? (slot-value (slot-value ast :name-expr) :decl) <sal-input-state-var-decl>))
                          (eq-hash-table/put! var-decl-table (slot-value (slot-value ast :name-expr) :decl) #t)))
                      esm)))

(define (sal-esm/substitute esm env)
  (and esm
       (sal-ast/substitute esm env)))
