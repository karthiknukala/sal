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

(module sal-esm-expand
        (include "sal.sch")
        (import sal-esm sal-ast-env sal-ast-for-each
                sal-expr-evaluator sal-type sal-ast-copy
                iterators queue runtime sal-ast-simplify sal-ast-expand)
        (export <sal-esm-expansion-context>
                (sal-esm-expansion-context/expand ctx esm)
                (sal-esm/expand-quantified-commands esm)
                (sal-esm/expand esm)
                (sal-esm/expand-core esm env ctx))
        )


(define-class <sal-esm-expansion-context> () (:expand-everything?))

;; Return a list o local-decls that should be expanded for a <sal-esm-new-binds-statement>
(define-generic (sal-esm-expansion-context/expand ctx esm))

(define-method (sal-esm-expansion-context/expand (ctx <sal-esm-expansion-context>) (esm <sal-esm-new-binds-statement>))
  (let ((local-decls (slot-value esm :local-decls)))
    (if (slot-value ctx :expand-everything?)
      local-decls
      (let ((result '()))
        (sal-ast/for-each (lambda (child)
                            (when (and (instance-of? child <sal-array-selection>)
                                       (instance-of? (slot-value (sal-lhs/name-expr child) :decl) <sal-state-var-decl>))
                              (sal-ast/for-each (lambda (ast)
                                                  (when (and (instance-of? ast <sal-name-expr>)
                                                             (memq (slot-value ast :decl) local-decls))
                                                    (pushnew! (slot-value ast :decl) result)))
                                                (slot-value child :arg))))
                          esm)
        result))))

(define (sal-esm/expand-quantified-commands esm)
  (status-message :esm-expanding-commands)
  (verbose-message 1 "expanding some quantified commands...")
  (display-runtime 2 "  expansion time: ~a secs"
    (lambda ()
      (sal-esm/expand-core esm (make-empty-env) (make-instance <sal-esm-expansion-context> :expand-everything? #f)))
    :esm-command-expansion-time))

(define (sal-esm/expand esm)
  (status-message :esm-expanding-commands)
  (verbose-message 1 "expanding some quantified commands, function applications, and quantifiers...")
  (display-runtime 2 "  expansion time: ~a secs"
    (lambda ()
      (sal-esm/expand-core esm (make-empty-env) (make-instance <sal-esm-expansion-context> :expand-everything? #t)))
    :esm-command-expansion-time))
        
(define-generic (sal-esm/expand-core esm env ctx))

(define-method (sal-esm/expand-core (esm <sal-ast>) (env <primitive>) (ctx <sal-esm-expansion-context>))
  (sal-ast/local-simplify-core esm env (lambda (child new-env) (sal-esm/expand-core child new-env ctx))))

(define-method (sal-esm/expand-core (esm <sal-esm-guard>) (env <primitive>) (ctx <sal-esm-expansion-context>))
  (if (slot-value ctx :expand-everything?)
    (update-ast-slots esm :expr (sal-ast/simplify (sal-ast/expand-core (slot-value esm :expr) env 0)))
    (call-next-method)))

(define-method (sal-esm/expand-core (esm <sal-esm-assignment>) (env <primitive>) (ctx <sal-esm-expansion-context>))
  (if (slot-value ctx :expand-everything?)
    (update-ast-slots esm 
                      :lhs (sal-ast/expand-core (slot-value esm :lhs) env 0)
                      :rhs (sal-ast/expand-core (slot-value esm :rhs) env 0))
    (call-next-method)))

(define-method (sal-esm/expand-core (esm <sal-esm-seq>) (env <primitive>) (ctx <sal-esm-expansion-context>))
  (let ((result (call-next-method)))
    (if (eq? result esm)
      esm
      (sal-esm/make-esm-seq* (slot-value result :statements) esm))))

(define-method (sal-esm/expand-core (esm <sal-esm-choice>) (env <primitive>) (ctx <sal-esm-expansion-context>))
  (let ((result (call-next-method)))
    (if (eq? result esm)
      esm
      (sal-esm/make-esm-choice* (slot-value result :statements) esm))))

(define-method (sal-esm/expand-core (esm <sal-esm-component>) (env <primitive>) (ctx <sal-esm-expansion-context>))
  (sal-ast/map esm env (lambda (child new-env) (sal-esm/expand-core child new-env ctx))))

(define (expand-esm-multi esm env ctx mk-proc)
  (let ((decls-to-expand (sal-esm-expansion-context/expand ctx esm)))
    (if (null? decls-to-expand)
      (sal-ast/map esm env (lambda (child new-env) (sal-esm/expand-core child new-env ctx)))
      (try
       (let* ((place-provider esm)
              (local-decls (slot-value esm :local-decls))
              (tmp-local-decls (difference local-decls decls-to-expand))
              (new-local-decls (map (cut sal-ast/substitute <> env) tmp-local-decls))
              (tmp-env (update-env* env tmp-local-decls new-local-decls))
              (body (slot-value esm :statement))
              (types (map (cut slot-value <> :type) decls-to-expand))
              (tuple-type (make-ast-instance <sal-tuple-type> place-provider
                                             :types types))
              (proc-child (lambda (child)
                            (sal-expr/evaluate-core child env 0)))
              (iterator (sal-type/make-iterator-core tuple-type proc-child))
              (result-queue (make-queue)))
         (iterator/for-each
          (lambda (tuple-literal)
            (let* ((new-env (update-env* tmp-env decls-to-expand (slot-value tuple-literal :exprs))))
              (queue/insert! result-queue (sal-esm/expand-core body new-env ctx))))
          iterator)
         (let ((new-body (mk-proc (queue->list result-queue) place-provider)))
           (if (null? new-local-decls)
             new-body
             (copy-ast esm
                       :local-decls new-local-decls
                       :statement new-body))))
       (catch* '(type-iterator expr-evaluator)
               (lambda (_ msg)
                 (sign-unsupported-feature esm "Failed to expand quantified command. Reason: ~a" msg)))))))

(define-method (sal-esm/expand-core (esm <sal-esm-multi-choice>) (env <primitive>) (ctx <sal-esm-expansion-context>))
  (expand-esm-multi esm env ctx sal-esm/make-esm-choice*))

(define-method (sal-esm/expand-core (esm <sal-esm-multi-seq>) (env <primitive>) (ctx <sal-esm-expansion-context>))
  (expand-esm-multi esm env ctx sal-esm/make-esm-seq*))

