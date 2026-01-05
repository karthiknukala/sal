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

(module sal-flat-modules-core
        (include "sal.sch")
        (import sal-ast-env sal-module sal-ast-copy sal-expression queue
                sal-trace-info)
        (export (sal/flat-children ast env asynch? mk-flat-proc)
                (sal/flat-command-section-core cmd-sec env choice-var-decl transition? 
                                               flat-cmd-proc flat-else-cmd-proc mk-choice-proc
                                               inc-choice-idx-proc!)
                (sal/flat-multi-child ast env mk-flat-proc))
        )

;;--------------------------------------------------------------------
;;
;; Contains code common to sal-flat-modules and sal-esm
;;
;;--------------------------------------------------------------------

(define (expand-child-module ast child1 child2 pos env asynch? mk-flat-proc)
  (let* ((numeral (make-sal-numeral pos ast))
         (child1-state-vars (sal-module/state-variables child1))
         (make-shared-local-ref
          (lambda (local-var-name)
            (let* ((decl (sal-module/lookup-var ast local-var-name))
                   (name-expr (make-sal-name-expr decl ast)))
              (make-ast-instance <sal-tuple-selection> ast
                                 :target (sal-ast/substitute name-expr env) ;; apply substitution to name-expr
                                 :idx numeral))))
         (bind-list (map
                     (lambda (decl1)
                       (let ((var-name1 (sal-decl/name decl1)))
                         (if (and (instance-of? decl1 <sal-local-state-var-decl>)
                                  (sal-module/lookup-var child2 var-name1))
                           (make-shared-local-ref var-name1)
                           (sal-module/lookup-var ast var-name1))))
                     child1-state-vars))
         (env (accumulate-env* env child1-state-vars bind-list)))
    (if (and asynch? (instance-of? child1 <sal-asynch-composition>))
      (sal/flat-children child1 env asynch? mk-flat-proc)
      (list (mk-flat-proc child1 env)))))

(define (sal/flat-children ast env asynch? mk-flat-proc)
  (let* ((mod1 (slot-value ast :module1))
         (mod2 (slot-value ast :module2))
         (flat-list1 (expand-child-module ast mod1 mod2 1 env asynch? mk-flat-proc))
         (flat-list2 (expand-child-module ast mod2 mod1 2 env asynch? mk-flat-proc)))
    (append flat-list1 flat-list2)))


(define (sal/flat-command-section-core cmd-sec env choice-var-decl transition? 
                                       flat-cmd-proc flat-else-cmd-proc mk-choice-proc
                                       inc-choice-idx-proc!)
  (if (and (sal/trace-info-enabled?) transition?)
    (let* ((place-provider cmd-sec)
           (expr-queue (make-queue))
           (trace-info-queue (make-queue)))
      (for-each (lambda (command)
                  (multiple-value-bind
                      (command-expr command-trace-info)
                      (flat-cmd-proc command)
                    (queue/insert! expr-queue command-expr)
                    (queue/insert! trace-info-queue command-trace-info)
                    (inc-choice-idx-proc!)))
                (slot-value cmd-sec :commands))
      (let ((command-expr-list (queue->list expr-queue))
            (command-trace-info-list (queue->list trace-info-queue)))
        (multiple-value-bind
            (else-expr else-trace-info)
            (if (slot-value cmd-sec :else-command)
              (flat-else-cmd-proc)
              (values #f #f))
          (let ((result-expr (mk-choice-proc (if else-expr 
                                               (append command-expr-list (list else-expr))
                                               command-expr-list)
                                             place-provider))
                (result-info (make-sal-choice-trace-info cmd-sec choice-var-decl 
                                                         command-trace-info-list else-trace-info)))
            (values result-expr result-info)))))
    (let* ((place-provider cmd-sec)
           (command-expr-list (map flat-cmd-proc (slot-value cmd-sec :commands)))
           (else-expr (if (slot-value cmd-sec :else-command)
                        (flat-else-cmd-proc)
                        #f))
           (result-expr (mk-choice-proc (if else-expr
                                          (append command-expr-list (list else-expr))
                                          command-expr-list)
                                        place-provider)))
      (values result-expr #f))))

(define (sal/flat-multi-child ast env mk-flat-proc)
  (let* ((child (slot-value ast :module))
         (child-state-vars (sal-module/state-variables child))
         (local-decl (car (slot-value ast :local-decls)))
         (idx (make-sal-name-expr local-decl ast))
         (make-shared-local-ref
          (lambda (local-var-name)
            (let* ((decl (sal-module/lookup-var ast local-var-name))
                   (name-expr (make-sal-name-expr decl ast)))
              (make-ast-instance <sal-array-selection> ast
                                 :fun (sal-ast/substitute name-expr env) ;; apply substitutions to name-expr
                                 :arg idx))))
         (bind-list (map 
                     (lambda (child-var-decl)
                       (let ((child-var-name (sal-decl/name child-var-decl)))
                         (if (instance-of? child-var-decl <sal-local-state-var-decl>)
                           (make-shared-local-ref child-var-name)
                           (sal-module/lookup-var ast child-var-name))))
                     child-state-vars))
         (new-env (accumulate-env* env child-state-vars bind-list)))
    (mk-flat-proc child new-env)))

  
