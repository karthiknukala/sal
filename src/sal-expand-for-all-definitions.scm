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

(module sal-expand-for-all-definitions
        (include "sal.sch")
        (import sal-type queue sal-ast-copy sal-expr-evaluator iterators 
                sal-ast-env sal-ast-for-each)
        (export (sal-base-module/expand-for-all-definitions ast env))
        )


(define (sal-base-module/expand-for-all-definitions ast env)
  (cond
   ((or (find (lambda (def) (sal-ast/find (cut instance-of? <> <sal-for-all-definition>) def))
              (slot-value ast :transition-definitions))
        (and (slot-value ast :transition-command-section)
             (sal-ast/find (cut instance-of? <> <sal-for-all-definition>) (slot-value ast :transition-command-section))))
    (status-message :expanding-forall-definitions)
    (verbose-message 2 "expanding FORALL definitions of base module at ~a..." (format-with-location ast ""))
    (update-ast-slots ast
                      :transition-definitions (sal-definition-list/expand (slot-value ast :transition-definitions) env)
                      :transition-command-section (if (slot-value ast :transition-command-section)
                                                    (sal-ast/expand-for-all-definitions (slot-value ast :transition-command-section) env)
                                                    #f)))
   (else
    ast)))

(define (sal-definition-list/expand def-list env)
  (let ((result (make-queue)))
    (for-each (lambda (def)
                (queue/append! result (sal-definition/expand def env)))
              def-list)
    (queue->list result)))

(define-generic (sal-definition/expand ast env))

(define-method (sal-definition/expand (ast <sal-simple-definition>) (env <primitive>))
  (list (sal-ast/substitute ast env)))

(define-method (sal-definition/expand (ast <sal-for-all-definition>) (env <primitive>))
  ;; TODO: check simple cases that can be converted to array literals
  (try
   (let* ((local-decls (slot-value ast :local-decls))
          (definitions (slot-value ast :definitions))
          (result (make-queue))
          (types (map (cut slot-value <> :type) local-decls))
          (tuple-type (make-ast-instance <sal-tuple-type> ast :types types))
          (proc-child (lambda (child)
                        (sal-expr/evaluate-core child env 0)))
          (iterator (sal-type/make-iterator-core tuple-type proc-child)))
     (iterator/for-each
      (lambda (tuple-literal)
        (let* ((env (update-env* env local-decls (slot-value tuple-literal :exprs))))
          (queue/append! result (sal-definition-list/expand definitions env))))
      iterator)
     (queue->list result))
   (catch* '(type-iterator expr-evaluator)
           (lambda (_ msg)
             (sign-source-error ast "Failed to expand FORALL definition, reason: ~a" msg)))))

(define-generic (sal-ast/expand-for-all-definitions ast env))

(define-method (sal-ast/expand-for-all-definitions (ast <sal-ast>) (env <primitive>))
  (sal-ast/map ast env sal-ast/expand-for-all-definitions))

(define-method (sal-ast/expand-for-all-definitions (ast <sal-guarded-command>) (env <primitive>))
  (update-ast-slots ast
                    :guard (sal-ast/substitute (slot-value ast :guard) env)
                    :assignments (sal-definition-list/expand (slot-value ast :assignments) env)))

(define-method (sal-ast/expand-for-all-definitions (ast <sal-else-command>) (env <primitive>))
  (update-ast-slots ast
                    :assignments (sal-definition-list/expand (slot-value ast :assignments) env)))

